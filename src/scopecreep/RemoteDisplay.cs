using System.Buffers.Binary;
using System.Diagnostics;
using System.Net.Sockets;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;

/// <summary>Parses the observed scope framing and decoder output without relying on read boundaries.</summary>
internal static class VideoProtocol
{
    public static readonly byte[] Bootstrap = Convert.FromHexString("000000016742401FA680C8137E540000000168CE3880");
    public const int MaxPayload = 16 * 1024 * 1024;
    private static readonly byte[] PngSignature = [137, 80, 78, 71, 13, 10, 26, 10];

    /// <summary>Checks a complete eight-byte scope header and bounds its payload.</summary>
    public static int PayloadLength(ReadOnlySpan<byte> header)
    {
        if (header.Length != 8 || header[2] != 0x55 || header[3] != 0xaa)
            throw new InvalidDataException("Invalid scope video header");
        var length = BinaryPrimitives.ReadUInt32LittleEndian(header[4..]);
        if (length > MaxPayload) throw new InvalidDataException("Scope video payload exceeds 16 MiB");
        return (int)length;
    }

    /// <summary>Finds an Annex B IDR boundary, retaining preceding encoder parameters when present.</summary>
    public static int KeyframeOffset(ReadOnlySpan<byte> payload)
    {
        var parameters = false;
        for (var i = 0; i + 3 < payload.Length; i++)
        {
            if (payload[i] != 0 || payload[i + 1] != 0) continue;
            var header = payload[i + 2] == 1 ? i + 3 :
                i + 4 < payload.Length && payload[i + 2] == 0 && payload[i + 3] == 1 ? i + 4 : -1;
            if (header < 0) continue;
            var kind = payload[header] & 31;
            if (kind is 7 or 8) parameters = true;
            if (kind == 5) return parameters ? 0 : i;
        }
        return -1;
    }

    /// <summary>Detects fresh SPS/PPS so they can replace the bootstrap inside FFmpeg.</summary>
    public static bool HasParameters(ReadOnlySpan<byte> payload)
    {
        for (var i = 0; i + 3 < payload.Length; i++)
        {
            if (payload[i] != 0 || payload[i + 1] != 0) continue;
            var header = payload[i + 2] == 1 ? i + 3 :
                i + 4 < payload.Length && payload[i + 2] == 0 && payload[i + 3] == 1 ? i + 4 : -1;
            if (header >= 0 && (payload[header] & 31) is 7 or 8) return true;
        }
        return false;
    }

    /// <summary>Reads exactly one PNG by chunk lengths; marker bytes inside compressed data are not boundaries.</summary>
    public static async Task<byte[]> ReadPngAsync(Stream stream, CancellationToken token)
    {
        var signature = new byte[8];
        await stream.ReadExactlyAsync(signature, token);
        if (!signature.AsSpan().SequenceEqual(PngSignature)) throw new InvalidDataException("Invalid decoder PNG signature");
        using var png = new MemoryStream();
        png.Write(signature);
        var header = new byte[8];
        var first = true;
        while (true)
        {
            await stream.ReadExactlyAsync(header, token);
            var length = BinaryPrimitives.ReadUInt32BigEndian(header);
            if (length > 4 * 1024 * 1024 || png.Length + length + 12 > 4 * 1024 * 1024)
                throw new InvalidDataException("Decoder image exceeds 4 MiB");
            var body = new byte[(int)length + 4];
            await stream.ReadExactlyAsync(body, token);
            if (first && (!header.AsSpan(4).SequenceEqual("IHDR"u8) || length != 13 ||
                BinaryPrimitives.ReadUInt32BigEndian(body) != 800 || BinaryPrimitives.ReadUInt32BigEndian(body.AsSpan(4)) != 600))
                throw new InvalidDataException("Decoder must produce 800x600 PNGs");
            first = false;
            png.Write(header);
            png.Write(body);
            if (header.AsSpan(4).SequenceEqual("IEND"u8))
            {
                if (length != 0) throw new InvalidDataException("Invalid PNG end chunk");
                return png.ToArray();
            }
        }
    }
}

/// <summary>One immutable host-observed frame; its timestamp is not an instrument capture timestamp.</summary>
internal sealed record ScreenFrame(string Id, long Generation, DateTimeOffset ReceivedAtUtc, long Timestamp, byte[] Png);

/// <summary>Owns one bidirectional relay client and a supervised optional FFmpeg decoder.</summary>
internal sealed class RemoteDisplay(ScopeConfig config, ILogger<RemoteDisplay> logger) : BackgroundService
{
    private readonly object state = new();
    private readonly string instance = Guid.NewGuid().ToString("N");
    private readonly Dictionary<string, (long Generation, long Timestamp)> recentFrames = new();
    private TaskCompletionSource changed = new(TaskCreationOptions.RunContinuationsAsynchronously);
    private TcpClient? connection;
    private ScreenFrame? latest;
    private long generation;
    private long sequence;
    private string? error;
    private bool decoderRunning;
    // A previous MCP process may have exited while holding the button.
    private bool releaseRequired = true;
    private int releaseX = 400, releaseY = 300;

    /// <summary>Returns independently available remote health without attempting USB communication.</summary>
    public object Health
    {
        get
        {
            lock (state) return new
            {
                enabled = config.RemoteEnabled, connected = connection is not null, decoderRunning,
                endpoint = $"{config.RemoteHost}:{config.RemotePort}", generation, error, releaseRequired,
                frameId = latest?.Id, receivedAtUtc = latest?.ReceivedAtUtc,
                frameAgeMs = latest is null ? (double?)null : Stopwatch.GetElapsedTime(latest.Timestamp).TotalMilliseconds
            };
        }
    }

    /// <summary>Waits for a current frame after a monotonic deadline, respecting disconnect invalidation.</summary>
    public async Task<ScreenFrame> CaptureAsync(long notBefore, int timeoutMs, CancellationToken token)
    {
        if (!config.RemoteEnabled) throw new IOException("Remote display is disabled");
        using var timeout = CancellationTokenSource.CreateLinkedTokenSource(token);
        timeout.CancelAfter(timeoutMs);
        try
        {
            while (true)
            {
                Task signal;
                lock (state)
                {
                    if (latest is not null && latest.Timestamp >= notBefore &&
                        Stopwatch.GetElapsedTime(latest.Timestamp).TotalSeconds <= 2) return latest;
                    signal = changed.Task;
                }
                await signal.WaitAsync(timeout.Token);
            }
        }
        catch (OperationCanceledException) when (!token.IsCancellationRequested)
        {
            lock (state) throw new TimeoutException($"No fresh screen within {timeoutMs} ms. {error}");
        }
    }

    /// <summary>Pins a gesture to a live generation and optionally checks the caller's recent screen reference.</summary>
    public long BeginInput(string? frameId)
    {
        lock (state)
        {
            if (connection is null || latest is null || Stopwatch.GetElapsedTime(latest.Timestamp).TotalSeconds > 2)
                throw new IOException("No live decoded screen; inspect scope_status before controlling the GUI");
            if (frameId is not null && (!recentFrames.TryGetValue(frameId, out var frame) ||
                frame.Generation != generation || Stopwatch.GetElapsedTime(frame.Timestamp).TotalSeconds > 30))
                throw new ArgumentException("Screen reference is expired or belongs to an old connection; capture another screen");
            return generation;
        }
    }

    /// <summary>Records pending button cleanup before a press can reach the network.</summary>
    public void RequireRelease(int x, int y, long expectedGeneration)
    {
        lock (state)
        {
            if (generation != expectedGeneration) throw new IOException("Remote session changed; gesture was not replayed");
            releaseRequired = true; releaseX = x; releaseY = y;
        }
    }

    /// <summary>Sends one whole event on the pinned session, invalidating it on an uncertain write.</summary>
    public async Task SendAsync(uint eventCode, int x, int y, long expectedGeneration, CancellationToken token)
    {
        TcpClient socket;
        lock (state)
        {
            if (connection is null || generation != expectedGeneration)
                throw new IOException("Remote session changed; input was not replayed");
            socket = connection;
        }
        using var timeout = CancellationTokenSource.CreateLinkedTokenSource(token);
        timeout.CancelAfter(2000);
        try
        {
            await WriteEventAsync(socket, eventCode, x, y, timeout.Token);
            if (eventCode == 1)
            {
                lock (state) if (generation == expectedGeneration) releaseRequired = false;
            }
        }
        catch
        {
            socket.Dispose();
            throw;
        }
    }

    /// <summary>Reconnects the local client without restarting the separate scope relay or replaying actions.</summary>
    protected override async Task ExecuteAsync(CancellationToken stoppingToken)
    {
        if (!config.RemoteEnabled) return;
        while (!stoppingToken.IsCancellationRequested)
        {
            try { await RunSessionAsync(stoppingToken); }
            catch (Exception failure) when (!stoppingToken.IsCancellationRequested)
            {
                lock (state) error = failure.Message;
                logger.LogWarning("Remote display unavailable: {Message}", failure.Message);
            }
            if (!stoppingToken.IsCancellationRequested) await Task.Delay(2000, stoppingToken);
        }
    }

    /// <summary>Supervises video input, PNG output, and diagnostics as one decoder generation.</summary>
    private async Task RunSessionAsync(CancellationToken stoppingToken)
    {
        using var lifetime = CancellationTokenSource.CreateLinkedTokenSource(stoppingToken);
        using var socket = new TcpClient { NoDelay = true };
        using var connecting = CancellationTokenSource.CreateLinkedTokenSource(stoppingToken);
        connecting.CancelAfter(5000);
        await socket.ConnectAsync(config.RemoteHost, config.RemotePort, connecting.Token);
        using var process = new Process();
        process.StartInfo = new ProcessStartInfo(config.FfmpegPath)
        {
            RedirectStandardInput = true, RedirectStandardOutput = true, RedirectStandardError = true,
            UseShellExecute = false, CreateNoWindow = true
        };
        foreach (var arg in new[] { "-nostdin", "-hide_banner", "-loglevel", "warning", "-probesize", "32768",
            "-analyzeduration", "0", "-flags", "low_delay", "-f", "h264", "-i", "pipe:0", "-vf", "fps=2",
            "-c:v", "png", "-f", "image2pipe", "pipe:1" }) process.StartInfo.ArgumentList.Add(arg);
        process.Start();
        Task[] pumps = [];
        try
        {
            bool cleanup;
            int x, y;
            long session;
            lock (state) { cleanup = releaseRequired; x = releaseX; y = releaseY; session = ++generation; }
            if (cleanup)
            {
                using var releasing = CancellationTokenSource.CreateLinkedTokenSource(stoppingToken);
                releasing.CancelAfter(2000);
                await WriteEventAsync(socket, 1, x, y, releasing.Token);
            }
            lock (state)
            {
                connection = socket; latest = null; recentFrames.Clear(); error = null;
                decoderRunning = true; releaseRequired = false;
                Signal();
            }
            pumps = [FeedDecoderAsync(socket.GetStream(), process.StandardInput.BaseStream, lifetime.Token),
                ReadImagesAsync(process.StandardOutput.BaseStream, session, lifetime.Token),
                DrainErrorsAsync(process, lifetime.Token)];
            var completed = await Task.WhenAny(pumps);
            await completed;
            throw new IOException("Remote video or FFmpeg output closed; relay may be busy or disconnected");
        }
        finally
        {
            lifetime.Cancel();
            lock (state)
            {
                if (ReferenceEquals(connection, socket)) connection = null;
                latest = null; recentFrames.Clear(); decoderRunning = false;
                Signal();
            }
            socket.Dispose();
            // Closing/killing first releases any blocked decoder pipe writes before awaiting the pumps.
            try { if (!process.HasExited) process.Kill(entireProcessTree: true); }
            catch (InvalidOperationException) { /* The decoder may have exited between the check and kill. */ }
            try { await Task.WhenAll(pumps); } catch (Exception failure) { logger.LogDebug("Decoder stopped: {Message}", failure.Message); }
            await process.WaitForExitAsync(CancellationToken.None);
        }
    }

    /// <summary>Strips complete scope packets, restores parameters, and skips undecodable leading interframes.</summary>
    private static async Task FeedDecoderAsync(Stream input, Stream output, CancellationToken token)
    {
        await output.WriteAsync(VideoProtocol.Bootstrap, token);
        await output.FlushAsync(token);
        var waiting = true;
        var header = new byte[8];
        while (true)
        {
            using var receiving = CancellationTokenSource.CreateLinkedTokenSource(token);
            receiving.CancelAfter(15000);
            await input.ReadExactlyAsync(header, receiving.Token);
            var payload = new byte[VideoProtocol.PayloadLength(header)];
            await input.ReadExactlyAsync(payload, receiving.Token);
            var start = 0;
            if (waiting)
            {
                start = VideoProtocol.KeyframeOffset(payload);
                if (start < 0)
                {
                    if (VideoProtocol.HasParameters(payload)) await output.WriteAsync(payload, token);
                    continue;
                }
                waiting = false;
            }
            using var writing = CancellationTokenSource.CreateLinkedTokenSource(token);
            writing.CancelAfter(2000);
            await output.WriteAsync(payload.AsMemory(start), writing.Token);
            await output.FlushAsync(writing.Token);
        }
    }

    /// <summary>Publishes latest PNGs and a bounded metadata-only history for screen references.</summary>
    private async Task ReadImagesAsync(Stream input, long session, CancellationToken token)
    {
        while (true)
        {
            using var receiving = CancellationTokenSource.CreateLinkedTokenSource(token);
            receiving.CancelAfter(15000);
            var png = await VideoProtocol.ReadPngAsync(input, receiving.Token);
            lock (state)
            {
                if (connection is null || session != generation || token.IsCancellationRequested) return;
                var frame = new ScreenFrame($"{instance}:{session}:{++sequence}", session, DateTimeOffset.UtcNow,
                    Stopwatch.GetTimestamp(), png);
                latest = frame;
                recentFrames.Add(frame.Id, (session, frame.Timestamp));
                if (recentFrames.Count > 64) recentFrames.Remove(recentFrames.Keys.First());
                Signal();
            }
        }
    }

    /// <summary>Drains decoder diagnostics independently so neither stdio MCP nor video can block on them.</summary>
    private async Task DrainErrorsAsync(Process process, CancellationToken token)
    {
        while (await process.StandardError.ReadLineAsync(token) is { } line)
            logger.LogDebug("FFmpeg: {Message}", line);
    }

    /// <summary>Encodes one little-endian event; callers serialize complete gestures.</summary>
    private static async Task WriteEventAsync(TcpClient socket, uint code, int x, int y, CancellationToken token)
    {
        var packet = new byte[12];
        BinaryPrimitives.WriteUInt32LittleEndian(packet, code);
        BinaryPrimitives.WriteUInt32LittleEndian(packet.AsSpan(4), (uint)x);
        BinaryPrimitives.WriteUInt32LittleEndian(packet.AsSpan(8), (uint)y);
        await socket.GetStream().WriteAsync(packet, token);
    }

    /// <summary>Wakes frame waiters while holding the state lock.</summary>
    private void Signal()
    {
        var previous = changed;
        changed = new(TaskCreationOptions.RunContinuationsAsynchronously);
        previous.TrySetResult();
    }
}
