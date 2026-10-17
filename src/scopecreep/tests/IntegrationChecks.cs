using System.Buffers.Binary;
using System.Security.Cryptography;
using System.Text;
using System.Text.Json;
using Microsoft.Extensions.Logging.Abstractions;

/// <summary>Hardware-free checks for framing, coordinated receipts, MTP transactions, and file lifetimes.</summary>
internal static class IntegrationChecks
{
    /// <summary>Exercises real parsers with fragmented/coalesced streams and a deterministic MTP device transcript.</summary>
    public static async Task RunAsync()
    {
        Check(VideoProtocol.PayloadLength(Convert.FromHexString("035855AA10000000")) == 16, "video length");
        Throws<InvalidDataException>(() => VideoProtocol.PayloadLength(Convert.FromHexString("035855AAFFFFFFFF")));
        Throws<InvalidDataException>(() => VideoProtocol.PayloadLength(new byte[8]));
        Check(VideoProtocol.KeyframeOffset(Convert.FromHexString("0000014112000000016556")) == 5, "IDR boundary");
        Check(VideoProtocol.KeyframeOffset(VideoProtocol.Bootstrap) == -1 && VideoProtocol.HasParameters(VideoProtocol.Bootstrap), "SPS/PPS bootstrap");
        var png = File.ReadAllBytes(Path.Combine(AppContext.BaseDirectory, "reference.png"));
        using (var stream = new FragmentedStream(png.Concat(png).ToArray()))
        {
            Check((await VideoProtocol.ReadPngAsync(stream, default)).SequenceEqual(png), "fragmented PNG");
            Check((await VideoProtocol.ReadPngAsync(stream, default)).SequenceEqual(png), "coalesced PNG boundary");
        }
        await ThrowsAsync<EndOfStreamException>(() => VideoProtocol.ReadPngAsync(new MemoryStream(png[..^1]), default));
        Throws<ArgumentException>(() => ScopeController.ValidatePoint(800, 0));
        Throws<ArgumentException>(() => ScopeController.ValidateDuration(10001));
        Throws<ArgumentException>(() => ScopeController.ValidateSettings(new ScopeSettings { Channels = [new() { Channel = "CH0", Enabled = true }] }));
        Throws<ArgumentException>(() => ScopeController.ValidateSettings(new ScopeSettings { Timebase = new() { SecondsPerDiv = double.NaN } }));
        Throws<ArgumentException>(() => CaptureFiles.ValidateName("../capture.bin"));
        Throws<ArgumentException>(() => CaptureFiles.ValidateName("..\\capture.bin"));
        using var remote = new RemoteDisplay(new ScopeConfig { RemoteEnabled = false }, NullLogger<RemoteDisplay>.Instance);
        using var scpi = new ScopeDevice(new ScopeConfig(), new NoScpi());
        var control = new ScopeController(scpi, remote);
        var configuration = await control.ConfigureAsync(new() { Timebase = new() { PositionSeconds = .001 } }, default);
        Check(configuration.Action.Outcome == "needsGui" && configuration.Result is ConfigurationReport { AnyApplied: false }, "unsupported configuration cannot claim transmission");
        var failed = await control.PressAsync(400, 300, 100, null, default);
        Check(failed.Action.Outcome == "unknown" && control.GetAction(failed.Action.Id) == failed.Action, "failed action receipt retained");
        var usbTranscript = new Transcript();
        usbTranscript.Replies.Enqueue(() => Encoding.ASCII.GetBytes("Micsig,STO1104C,16298,5\r\n"));
        using (var verifiedDevice = new ScopeDevice(new ScopeConfig(), usbTranscript))
        {
            var verifiedControl = new ScopeController(verifiedDevice, remote);
            var timebase = await verifiedControl.ConfigureAsync(new() { Timebase = new() { SecondsPerDiv = .001 } }, default);
            Check(timebase.Action.Outcome == "transmitted" && timebase.Result is ConfigurationReport { VisualReviewRequired: true, AnyApplied: false },
                "broken timebase getter cannot certify application");
            Check(usbTranscript.Writes.SequenceEqual(new[] { "*IDN?", ":TIMebase:EXTent 0.001000000000" }), "verified decimal timebase command without broken query");
        }
        var rejectedTranscript = new Transcript();
        using (var rejectedDevice = new ScopeDevice(new ScopeConfig(), rejectedTranscript))
        {
            var rejectedControl = new ScopeController(rejectedDevice, remote);
            await ThrowsAsync<ArgumentException>(() => rejectedControl.ConfigureAsync(new()
            {
                Channels = [new() { Channel = "CH1", Enabled = false }], RunState = "invalid"
            }, default));
            Check(rejectedTranscript.Writes.Count == 0, "all configuration validated before first mutation");
        }
        var formatPreset = ScopeController.Presets.Single(p => p.Name == "save.format");
        Check(ScopeController.ResolvePreset(formatPreset, "CSV").X == 348, "verified preset argument");
        Throws<ArgumentException>(() => ScopeController.ResolvePreset(formatPreset, "unknown"));

        var folder = Path.Combine(Path.GetTempPath(), "scopecreep-checks-" + Guid.NewGuid().ToString("N"));
        var config = new ScopeConfig { CaptureDirectory = folder, MaxFileBytes = 1024, MaxTemporaryBytes = 2048 };
        var transcript = new MtpTranscript();
        string temporary;
        try
        {
            using var mtp = new MtpDevice(config, transcript);
            using (var captures = new CaptureFiles(config, mtp))
            {
                var roots = await mtp.ListAsync(null, 0, 100, default);
                var storageRef = JsonSerializer.SerializeToElement(roots.Entries[0]).GetProperty("reference").GetString()!;
                var listing = await mtp.ListAsync(storageRef, 0, 100, default);
                var info = (MtpObject)listing.Entries.Single();
                Check(info.Name == "test.csv" && info.Size == MtpTranscript.FileBytes.Length, "MTP metadata");
                var downloaded = await captures.FetchAsync(info.Reference, default);
                temporary = downloaded.LocalPath;
                Check(File.ReadAllBytes(temporary).SequenceEqual(MtpTranscript.FileBytes), "streamed MTP download");
                Check(downloaded.Sha256 == Convert.ToHexString(SHA256.HashData(MtpTranscript.FileBytes)).ToLowerInvariant(), "download hash");
                var chunk = JsonSerializer.SerializeToElement(await captures.ReadAsync(downloaded.Id, 2, 4, "base64", default));
                Check(Convert.FromBase64String(chunk.GetProperty("base64").GetString()!).SequenceEqual(MtpTranscript.FileBytes[2..6]), "bounded chunk access");
                await captures.SaveAsync(downloaded.Id, "kept.csv", default);
                await ThrowsAsync<IOException>(() => captures.SaveAsync(downloaded.Id, "kept.csv", default));
                Check(File.ReadAllBytes(Path.Combine(folder, "kept.csv")).SequenceEqual(MtpTranscript.FileBytes), "explicit save");
                await ThrowsAsync<ArgumentException>(() => mtp.ListAsync("expired:storage:65537", 0, 100, default));
                transcript.WrongTransaction = true;
                await ThrowsAsync<InvalidDataException>(() => mtp.ListAsync(storageRef, 0, 100, default));
                var writes = transcript.Operations.Count;
                await ThrowsAsync<IOException>(() => mtp.ListAsync(null, 0, 100, default));
                Check(transcript.Operations.Count == writes, "unsynchronized MTP never blindly retries");
                transcript.DeviceEpoch = "test-device-2";
                transcript.WrongTransaction = false;
                var recovered = await mtp.ListAsync(null, 0, 100, default);
                Check(recovered.Generation != roots.Generation, "physical reconnect invalidates old MTP references");
                await ThrowsAsync<ArgumentException>(() => mtp.ListAsync(storageRef, 0, 100, default));
            }
            Check(!File.Exists(temporary) && File.Exists(Path.Combine(folder, "kept.csv")), "temporary cleanup preserves saved file");
            var healthy = new MtpTranscript();
            using (var second = new MtpDevice(config, healthy)) await second.ListAsync(null, 0, 100, default);
            Check(healthy.Operations.Last() == 0x1003, "healthy session closed without USB reset");
            var initiallyMissing = new MtpTranscript { Unavailable = true };
            using (var lateDevice = new MtpDevice(config, initiallyMissing))
            {
                await ThrowsAsync<IOException>(() => lateDevice.ListAsync(null, 0, 100, default));
                Check(initiallyMissing.Operations.Count == 0, "missing device did not start a transaction");
                initiallyMissing.Unavailable = false;
                await lateDevice.ListAsync(null, 0, 100, default);
            }
            var quota = new ScopeConfig { MaxFileBytes = 1, MaxTemporaryBytes = 1 };
            using var limitedMtp = new MtpDevice(quota, new MtpTranscript());
            using var limitedFiles = new CaptureFiles(quota, limitedMtp);
            var limitedRoot = await limitedMtp.ListAsync(null, 0, 100, default);
            var limitedRef = JsonSerializer.SerializeToElement(limitedRoot.Entries[0]).GetProperty("reference").GetString()!;
            var limitedListing = await limitedMtp.ListAsync(limitedRef, 0, 100, default);
            await ThrowsAsync<IOException>(() => limitedFiles.FetchAsync(((MtpObject)limitedListing.Entries[0]).Reference, default));
            await limitedMtp.ListAsync(null, 0, 100, default);
        }
        finally { if (Directory.Exists(folder)) Directory.Delete(folder, true); }
        Console.WriteLine("PASS: video/PNG fragmentation, action receipts, configuration validation, MTP phases/IDs, streaming, quotas, file save/cleanup");
    }

    /// <summary>Fails with a readable check name.</summary>
    private static void Check(bool condition, string name) { if (!condition) throw new Exception(name); }
    /// <summary>Requires a synchronous parser/validation failure.</summary>
    private static void Throws<T>(Action action) where T : Exception
    {
        try { action(); } catch (T) { return; }
        throw new Exception("Expected " + typeof(T).Name);
    }
    /// <summary>Requires an asynchronous service failure.</summary>
    private static async Task ThrowsAsync<T>(Func<Task> action) where T : Exception
    {
        try { await action(); } catch (T) { return; }
        throw new Exception("Expected " + typeof(T).Name);
    }

    /// <summary>Splits every parser read inside protocol fields.</summary>
    private sealed class FragmentedStream(byte[] bytes) : MemoryStream(bytes)
    {
        /// <summary>Returns at most three bytes, regardless of requested buffer length.</summary>
        public override ValueTask<int> ReadAsync(Memory<byte> buffer, CancellationToken token = default)
            => base.ReadAsync(buffer[..Math.Min(3, buffer.Length)], token);
    }

    /// <summary>Fails if an unsupported configuration unexpectedly reaches USB.</summary>
    private sealed class NoScpi : IScpiTransport
    {
        /// <summary>Rejects accidental hardware access.</summary>
        public void Open() => throw new Exception("Unexpected USB access");
        /// <summary>Rejects writes.</summary>
        public void Write(byte[] bytes, System.Diagnostics.Stopwatch clock, int timeout, CancellationToken token) => throw new Exception("Unexpected write");
        /// <summary>Rejects queries.</summary>
        public byte[] Read(System.Diagnostics.Stopwatch clock, int timeout, int maxBytes, CancellationToken token) => throw new Exception("Unexpected read");
        /// <summary>Releases no resources.</summary>
        public void Dispose() { }
    }

    /// <summary>Emulates data/response phases and three-byte reads with real MTP dataset layouts.</summary>
    private sealed class MtpTranscript : IMtpTransport
    {
        public static readonly byte[] FileBytes = Encoding.UTF8.GetBytes("Time,CH1\n0,1\n1,2\n");
        public readonly List<ushort> Operations = [];
        public bool WrongTransaction;
        private readonly Queue<byte> incoming = new();
        private string epoch = "test-device-1";
        public bool Unavailable;
        public string DeviceEpoch
        {
            get => Unavailable ? throw new IOException("Device not present") : epoch;
            set => epoch = value;
        }
        /// <summary>Opens an emulated device.</summary>
        public void Open() { incoming.Clear(); }
        /// <summary>Releases an emulated device.</summary>
        public void Dispose() { }
        /// <summary>Produces the appropriate dataset and completion response for one command.</summary>
        public void Write(byte[] data, CancellationToken token)
        {
            var op = BinaryPrimitives.ReadUInt16LittleEndian(data.AsSpan(6));
            var tx = BinaryPrimitives.ReadUInt32LittleEndian(data.AsSpan(8));
            Operations.Add(op);
            byte[]? payload = op switch
            {
                0x1001 => [0, 0],
                0x1004 => [1, 0, 0, 0, 1, 0, 1, 0],
                0x1005 => new byte[26].Concat(PtpString("Internal")).Concat(PtpString("")).ToArray(),
                0x1007 => [1, 0, 0, 0, 7, 0, 0, 0],
                0x1008 => ObjectInfo(),
                0x1009 => FileBytes,
                0x1002 or 0x1003 => null,
                _ => throw new Exception("Unexpected MTP operation")
            };
            if (payload is not null) Enqueue(2, op, WrongTransaction ? tx + 1 : tx, payload);
            Enqueue(3, 0x2001, tx, []);
        }
        /// <summary>Returns fragments that are unrelated to container boundaries.</summary>
        public byte[] Read(CancellationToken token)
        {
            var result = new byte[Math.Min(3, incoming.Count)];
            for (var i = 0; i < result.Length; i++) result[i] = incoming.Dequeue();
            return result;
        }
        /// <summary>Queues one complete PTP container.</summary>
        private void Enqueue(ushort kind, ushort code, uint tx, byte[] payload)
        {
            var header = new byte[12];
            BinaryPrimitives.WriteUInt32LittleEndian(header, (uint)(12 + payload.Length));
            BinaryPrimitives.WriteUInt16LittleEndian(header.AsSpan(4), kind);
            BinaryPrimitives.WriteUInt16LittleEndian(header.AsSpan(6), code);
            BinaryPrimitives.WriteUInt32LittleEndian(header.AsSpan(8), tx);
            foreach (var b in header.Concat(payload)) incoming.Enqueue(b);
        }
        /// <summary>Builds fixed object fields followed by variable UTF-16 metadata.</summary>
        private static byte[] ObjectInfo()
        {
            var data = new byte[52];
            BinaryPrimitives.WriteUInt32LittleEndian(data, 65537);
            BinaryPrimitives.WriteUInt16LittleEndian(data.AsSpan(4), 0x3000);
            BinaryPrimitives.WriteUInt32LittleEndian(data.AsSpan(8), (uint)FileBytes.Length);
            return data.Concat(PtpString("test.csv")).Concat(PtpString("")).Concat(PtpString("20261017T070000")).ToArray();
        }
        /// <summary>Encodes a counted, terminated UTF-16LE string.</summary>
        private static byte[] PtpString(string value) => new[] { (byte)(value.Length + 1) }.Concat(Encoding.Unicode.GetBytes(value + '\0')).ToArray();
    }
}
