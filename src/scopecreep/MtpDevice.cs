using System.Buffers.Binary;
using System.ComponentModel;
using System.Diagnostics;
using System.Globalization;
using System.Runtime.InteropServices;
using System.Text;
using Microsoft.Win32.SafeHandles;

/// <summary>One claimed MTP interface; a transcript can replace it in hardware-free tests.</summary>
internal interface IMtpTransport : IDisposable
{
    /// <summary>Identifies a physical USB enumeration without initiating a protocol transaction.</summary>
    string DeviceEpoch { get; }
    /// <summary>Claims the MTP interface without resetting the composite device.</summary>
    void Open();
    /// <summary>Writes one container without replaying a transmitted prefix.</summary>
    void Write(byte[] data, CancellationToken token);
    /// <summary>Receives a bounded transfer; container boundaries belong to the protocol layer.</summary>
    byte[] Read(CancellationToken token);
}

/// <summary>Claims only the MTP interface of the configured USBTMC composite device; never resets USB.</summary>
internal sealed class UsbMtpTransport(ScopeConfig config) : IMtpTransport
{
    private SafeFileHandle? handle;
    private uint interfaceNumber;
    private byte inputEndpoint, outputEndpoint;
    public string? DeviceNode { get; private set; }

    /// <summary>Detects a physical USB reconnection without sending device requests.</summary>
    public string DeviceEpoch
    {
        get
        {
            var node = RealPath(config.DevicePath);
            var device = Directory.GetParent(RealPath($"/sys/class/usbmisc/{Path.GetFileName(node)}/device"))!.FullName;
            return device + ":" + File.ReadAllText(Path.Combine(device, "busnum")).Trim() + ":" +
                File.ReadAllText(Path.Combine(device, "devnum")).Trim();
        }
    }

    /// <summary>Discovers bus/device numbers and bulk endpoints via the USBTMC node's sysfs parent.</summary>
    public void Open()
    {
        if (handle is { IsClosed: false, IsInvalid: false }) return;
        if (!OperatingSystem.IsLinux()) throw new PlatformNotSupportedException("MTP requires Linux USBFS");
        var devicePath = RealPath(config.DevicePath);
        // sysfs has symlinked ancestors: resolving just the final link normalizes ../.. incorrectly.
        var interfacePath = RealPath($"/sys/class/usbmisc/{Path.GetFileName(devicePath)}/device");
        var usb = Directory.GetParent(interfacePath)!.FullName;
        if (File.ReadAllText(Path.Combine(usb, "idVendor")).Trim() != "18d1" ||
            File.ReadAllText(Path.Combine(usb, "idProduct")).Trim() != "4ee9" ||
            File.ReadAllText(Path.Combine(usb, "product")).Trim() != "STO1104C")
            throw new IOException("Configured USBTMC node is not the supported STO1104C composite device");
        var mtp = Directory.EnumerateDirectories(usb).FirstOrDefault(p =>
            File.Exists(Path.Combine(p, "interface")) && File.ReadAllText(Path.Combine(p, "interface")).Trim() == "MTP")
            ?? throw new IOException("MTP interface not found beside USBTMC");
        interfaceNumber = uint.Parse(File.ReadAllText(Path.Combine(mtp, "bInterfaceNumber")).Trim(), NumberStyles.HexNumber);
        if (interfaceNumber != 0) throw new IOException("Unexpected MTP interface; refusing to claim USBTMC");
        foreach (var endpoint in Directory.EnumerateDirectories(mtp, "ep_*"))
        {
            var attributes = byte.Parse(File.ReadAllText(Path.Combine(endpoint, "bmAttributes")).Trim(), NumberStyles.HexNumber);
            if ((attributes & 3) != 2) continue;
            var address = byte.Parse(File.ReadAllText(Path.Combine(endpoint, "bEndpointAddress")).Trim(), NumberStyles.HexNumber);
            if ((address & 0x80) != 0) inputEndpoint = address;
            else outputEndpoint = address;
        }
        if (inputEndpoint == 0 || outputEndpoint == 0) throw new IOException("MTP bulk endpoints not found");
        var bus = int.Parse(File.ReadAllText(Path.Combine(usb, "busnum")));
        var dev = int.Parse(File.ReadAllText(Path.Combine(usb, "devnum")));
        DeviceNode = $"/dev/bus/usb/{bus:000}/{dev:000}";
        var fd = NativeOpen(DeviceNode, 2 | 0x80000);
        if (fd < 0) throw Error("Cannot open MTP USB node; check USB permissions");
        handle = new SafeFileHandle(fd, true);
        if (NativeIoctlUInt(handle, 0x8004550f, ref interfaceNumber) < 0)
        {
            var error = Error("Cannot claim MTP interface; close other MTP clients");
            handle.Dispose(); handle = null;
            throw error;
        }
    }

    /// <summary>Writes one MTP container, completing short bulk writes without repeating an already-sent prefix.</summary>
    public void Write(byte[] data, CancellationToken token)
    {
        for (var offset = 0; offset < data.Length;)
        {
            var chunk = data.AsSpan(offset, Math.Min(16384, data.Length - offset)).ToArray();
            var count = Bulk(outputEndpoint, chunk, token);
            if (count == 0) throw new IOException("Empty MTP bulk write");
            offset += count;
        }
    }

    /// <summary>Reads one bounded USB bulk transfer; protocol parsing handles fragmentation and coalescing.</summary>
    public byte[] Read(CancellationToken token)
    {
        // One high-speed packet avoids legacy gadget short-transfer ambiguity; the protocol layer reassembles containers.
        var buffer = new byte[512];
        for (var terminators = 0; terminators < 4; terminators++)
        {
            var count = Bulk(inputEndpoint, buffer, token);
            if (count != 0) return buffer.AsSpan(0, count).ToArray();
        }
        throw new IOException("Repeated empty MTP USB transfers");
    }

    /// <summary>Bounds each blocking ioctl and retains ownership until it completes, including after cancellation.</summary>
    private int Bulk(byte endpoint, byte[] buffer, CancellationToken token)
    {
        token.ThrowIfCancellationRequested();
        var pin = GCHandle.Alloc(buffer, GCHandleType.Pinned);
        try
        {
            var transfer = new BulkTransfer(endpoint, (uint)buffer.Length, (uint)config.TimeoutMs, pin.AddrOfPinnedObject());
            var count = NativeIoctlBulk(handle!, 0xc0185502, ref transfer);
            if (count < 0) throw Error("MTP bulk transfer failed; no USB reset attempted");
            token.ThrowIfCancellationRequested();
            return count;
        }
        finally { pin.Free(); }
    }

    /// <summary>Releases interface zero only; this does not detach, clear, or reset the USBTMC interface.</summary>
    public void Dispose()
    {
        if (handle is null) return;
        if (!handle.IsInvalid && !handle.IsClosed) NativeIoctlUInt(handle, 0x80045510, ref interfaceNumber);
        handle.Dispose(); handle = null;
    }

    /// <summary>Captures errno before another native operation can overwrite it.</summary>
    private static IOException Error(string message)
    {
        var number = Marshal.GetLastPInvokeError();
        return new IOException($"{message}: {new Win32Exception(number).Message} (errno {number})");
    }

    /// <summary>Canonicalizes all symlinked ancestors, including sysfs class directories.</summary>
    private static string RealPath(string path)
    {
        var pointer = NativeRealPath(path, IntPtr.Zero);
        if (pointer == IntPtr.Zero) throw Error($"Cannot resolve {path}");
        try { return Marshal.PtrToStringUTF8(pointer)!; }
        finally { NativeFree(pointer); }
    }

    /// <summary>Resolves a Linux path into a newly allocated canonical pathname.</summary>
    [DllImport("libc", EntryPoint = "realpath", SetLastError = true)]
    private static extern IntPtr NativeRealPath([MarshalAs(UnmanagedType.LPUTF8Str)] string path, IntPtr resolved);
    /// <summary>Releases the canonical pathname allocated by libc.</summary>
    [DllImport("libc", EntryPoint = "free")]
    private static extern void NativeFree(IntPtr pointer);

    /// <summary>Linux USBFS bulk-transfer descriptor on the supported 64-bit architectures.</summary>
    [StructLayout(LayoutKind.Sequential)]
    private struct BulkTransfer(uint endpoint, uint length, uint timeout, IntPtr data)
    {
        public uint Endpoint = endpoint, Length = length, Timeout = timeout;
        public IntPtr Data = data;
    }
    /// <summary>Opens a USB device node without inheriting it into FFmpeg.</summary>
    [DllImport("libc", EntryPoint = "open", SetLastError = true)]
    private static extern int NativeOpen([MarshalAs(UnmanagedType.LPUTF8Str)] string path, int flags);
    /// <summary>Claims or releases a specific interface.</summary>
    [DllImport("libc", EntryPoint = "ioctl", SetLastError = true)]
    private static extern int NativeIoctlUInt(SafeFileHandle fd, nuint request, ref uint number);
    /// <summary>Transfers bounded bulk data without native recovery side effects.</summary>
    [DllImport("libc", EntryPoint = "ioctl", SetLastError = true)]
    private static extern int NativeIoctlBulk(SafeFileHandle fd, nuint request, ref BulkTransfer transfer);
}

/// <summary>Read-only MTP object metadata; references belong to one open device session.</summary>
internal sealed record MtpObject(string Reference, string Name, bool Folder, long Size, string? Modified,
    uint Storage, uint Handle);
/// <summary>A shallow, bounded listing of storages or object metadata.</summary>
internal sealed record MtpListing(string Generation, string? ParentRef, object[] Entries, int? NextOffset);

/// <summary>Implements a narrow persistent MTP session with streamed file transfer and no USB reset recovery.</summary>
internal sealed class MtpDevice : IDisposable
{
    private readonly IMtpTransport transport;
    private readonly SemaphoreSlim gate = new(1);
    private readonly ScopeConfig config;
    private byte[] pending = [];
    private int pendingOffset;
    private uint transaction;
    private bool opened, faulted, disposed;
    private bool pendingResponse;
    private string generation = Guid.NewGuid().ToString("N");
    private string? error;
    private string? deviceEpoch;
    private string? lastOperation;
    private readonly object healthLock = new();

    /// <summary>Constructs the USBFS production transport.</summary>
    public MtpDevice(ScopeConfig config) : this(config, new UsbMtpTransport(config)) { }
    /// <summary>Accepts a byte transcript for protocol and cancellation checks.</summary>
    internal MtpDevice(ScopeConfig config, IMtpTransport transport) { this.config = config; this.transport = transport; }

    /// <summary>Reports MTP independently of USBTMC and without opening a session as a status side effect.</summary>
    public object Health
    {
        get { lock (healthLock) return new { connected = opened && !faulted, faulted, generation, error, lastOperation, resetsAttempted = false }; }
    }

    /// <summary>Lists storages or one folder, requesting metadata only for the requested page.</summary>
    public Task<MtpListing> ListAsync(string? parentRef, int offset, int count, CancellationToken token)
    {
        if (offset < 0 || count is < 1 or > 500) throw new ArgumentException("offset >= 0; count must be 1..500");
        if (parentRef is not null) ArgumentException.ThrowIfNullOrWhiteSpace(parentRef);
        return ExecuteAsync(() =>
        {
            if (parentRef is null)
            {
                var ids = DecodeIds(Transfer(0x1004, [], token));
                var entries = ids.Skip(offset).Take(count).Select(id =>
                {
                    var info = Transfer(0x1005, [id], token);
                    if (info.Length < 27) throw new InvalidDataException("Truncated MTP storage info");
                    var cursor = 26;
                    var name = ReadString(info, ref cursor);
                    return (object)new { reference = $"{generation}:storage:{id}", name,
                        capacity = BinaryPrimitives.ReadUInt64LittleEndian(info.AsSpan(6)),
                        freeBytes = BinaryPrimitives.ReadUInt64LittleEndian(info.AsSpan(14)) };
                }).ToArray();
                return new MtpListing(generation, null, entries, offset + entries.Length < ids.Length ? offset + entries.Length : null);
            }
            var (storage, parent, storageRoot) = ParseReference(parentRef);
            if (!storageRoot && !GetInfo(storage, parent, token).Folder) throw new ArgumentException("parentRef must name a folder or storage");
            var handles = DecodeIds(Transfer(0x1007, [storage, 0, storageRoot ? uint.MaxValue : parent], token));
            var objects = handles.Skip(offset).Take(count).Select(id => (object)GetInfo(storage, id, token)).ToArray();
            return new MtpListing(generation, parentRef, objects, offset + objects.Length < handles.Length ? offset + objects.Length : null);
        }, token);
    }

    /// <summary>Downloads one object without buffering it in memory and validates metadata again after transfer.</summary>
    public Task<MtpObject> DownloadAsync(string reference, Stream output, long limit, CancellationToken token)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(reference);
        if (limit < 0) throw new ArgumentException("Download limit must be nonnegative");
        return ExecuteAsync(() =>
        {
        var (storage, handle, root) = ParseReference(reference);
        if (root) throw new ArgumentException("A storage is not a file");
        var before = GetInfo(storage, handle, token);
        if (before.Folder) throw new ArgumentException("A folder cannot be downloaded");
        if (before.Size > limit) throw new MtpRequestException($"File size {before.Size} exceeds remaining download limit {limit}");
        long written = 0;
        Transfer(0x1009, [handle], token, length =>
        {
            if (length != before.Size || length > limit) throw new InvalidDataException("MTP data length differs from object metadata or exceeds quota");
            while (written < length)
            {
                var bytes = ReadBytes((int)Math.Min(65536, length - written), token);
                output.Write(bytes);
                written += bytes.Length;
            }
        });
        var after = GetInfo(storage, handle, token);
        if (written != before.Size || after.Size != before.Size || after.Name != before.Name || after.Modified != before.Modified)
            throw new MtpRequestException("Scope file changed during retrieval; wait for save completion and retry");
        return after;
        }, token);
    }

    /// <summary>Serializes and awaits native I/O; an uncertain protocol phase disables MTP until the server restarts.</summary>
    private async Task<T> ExecuteAsync<T>(Func<T> operation, CancellationToken token)
    {
        await gate.WaitAsync(token);
        try
        {
            ObjectDisposedException.ThrowIf(disposed, this);
            return await Task.Run(() =>
            {
                string epoch;
                try { epoch = transport.DeviceEpoch; }
                catch (Exception failure)
                {
                    lock (healthLock) { opened = false; error = failure.Message; }
                    transport.Dispose();
                    throw;
                }
                if (deviceEpoch is not null && epoch != deviceEpoch)
                {
                    transport.Dispose();
                    pending = []; pendingOffset = 0; pendingResponse = false;
                    lock (healthLock) { faulted = false; opened = false; error = null; generation = Guid.NewGuid().ToString("N"); }
                }
                deviceEpoch = epoch;
                if (faulted) throw new IOException($"MTP is unsynchronized: {error}. Reconnect USB or restart after resolving device access; no automatic USB reset");
                try
                {
                    EnsureSession(token);
                    var result = operation();
                    lock (healthLock) error = null;
                    return result;
                }
                catch (Exception failure)
                {
                    lock (healthLock) error = failure.Message;
                    if (failure is not (MtpResponseException or MtpRequestException or ArgumentException))
                    {
                        if (pendingResponse)
                        {
                            lock (healthLock) { faulted = true; opened = false; }
                        }
                        if (!opened) transport.Dispose();
                    }
                    throw;
                }
            }, CancellationToken.None);
        }
        finally { gate.Release(); }
    }

    /// <summary>Opens one session and uses protocol close/open only for an explicit already-open response.</summary>
    private void EnsureSession(CancellationToken token)
    {
        if (opened) return;
        transport.Open(); pending = []; pendingOffset = 0; transaction = 0;
        Transfer(0x1001, [], token);
        transaction = 0;
        try { Transfer(0x1002, [0x53435001], token, expectData: false); }
        catch (MtpResponseException response) when (response.Code == 0x201e)
        {
            Transfer(0x1003, [], token, expectData: false);
            transaction = 0;
            Transfer(0x1002, [0x53435001], token, expectData: false);
        }
        lock (healthLock) { opened = true; error = null; generation = Guid.NewGuid().ToString("N"); }
    }

    /// <summary>Runs one PTP command/data/response transaction with exact IDs and bounded metadata.</summary>
    private byte[] Transfer(ushort operation, uint[] parameters, CancellationToken token,
        Action<long>? receiveFile = null, bool expectData = true)
    {
        var id = transaction++;
        lock (healthLock) lastOperation = $"0x{operation:x4} transaction {id}";
        var command = new byte[12 + parameters.Length * 4];
        BinaryPrimitives.WriteUInt32LittleEndian(command, (uint)command.Length);
        BinaryPrimitives.WriteUInt16LittleEndian(command.AsSpan(4), 1);
        BinaryPrimitives.WriteUInt16LittleEndian(command.AsSpan(6), operation);
        BinaryPrimitives.WriteUInt32LittleEndian(command.AsSpan(8), id);
        for (var i = 0; i < parameters.Length; i++) BinaryPrimitives.WriteUInt32LittleEndian(command.AsSpan(12 + i * 4), parameters[i]);
        pendingResponse = true;
        transport.Write(command, token);
        var header = ReadHeader(id, token);
        byte[] data = [];
        if (header.Kind == 2)
        {
            if (!expectData || header.Code != operation) throw new InvalidDataException("Unexpected MTP data phase");
            if (receiveFile is not null) receiveFile(header.Length - 12L);
            else
            {
                if (header.Length > 16 * 1024 * 1024) throw new InvalidDataException("MTP metadata exceeds 16 MiB");
                data = ReadBytes((int)header.Length - 12, token);
            }
            header = ReadHeader(id, token);
        }
        else if (expectData && header.Kind == 3 && header.Code == 0x2001)
            throw new InvalidDataException("MTP returned success without required data");
        if (header.Kind != 3 || header.Length > 32) throw new InvalidDataException("Invalid MTP completion container");
        ReadBytes((int)header.Length - 12, token);
        pendingResponse = false;
        if (header.Code != 0x2001) throw new MtpResponseException(header.Code, operation);
        return data;
    }

    /// <summary>Reads one container header without treating USB transfers as container boundaries.</summary>
    private (uint Length, ushort Kind, ushort Code) ReadHeader(uint expectedId, CancellationToken token)
    {
        var bytes = ReadBytes(12, token);
        var length = BinaryPrimitives.ReadUInt32LittleEndian(bytes);
        var id = BinaryPrimitives.ReadUInt32LittleEndian(bytes.AsSpan(8));
        if (length < 12 || id != expectedId) throw new InvalidDataException("Invalid MTP length or transaction ID");
        return (length, BinaryPrimitives.ReadUInt16LittleEndian(bytes.AsSpan(4)), BinaryPrimitives.ReadUInt16LittleEndian(bytes.AsSpan(6)));
    }

    /// <summary>Consumes exactly the requested number of bytes and retains coalesced trailing containers.</summary>
    private byte[] ReadBytes(int length, CancellationToken token)
    {
        var result = new byte[length];
        for (var offset = 0; offset < length;)
        {
            token.ThrowIfCancellationRequested();
            if (pendingOffset == pending.Length)
            {
                pending = transport.Read(token); pendingOffset = 0;
                if (pending.Length == 0) throw new EndOfStreamException("Empty MTP bulk transfer");
            }
            var count = Math.Min(length - offset, pending.Length - pendingOffset);
            pending.AsSpan(pendingOffset, count).CopyTo(result.AsSpan(offset));
            offset += count; pendingOffset += count;
        }
        return result;
    }

    /// <summary>Reads a bounded MTP uint32 array of storage or object IDs.</summary>
    internal static uint[] DecodeIds(byte[] bytes)
    {
        if (bytes.Length < 4) throw new InvalidDataException("Truncated MTP ID array");
        var count = BinaryPrimitives.ReadUInt32LittleEndian(bytes);
        if (count > 100000 || bytes.Length != 4L + count * 4L) throw new InvalidDataException("Invalid MTP ID array length");
        var ids = new uint[(int)count];
        for (var i = 0; i < ids.Length; i++) ids[i] = BinaryPrimitives.ReadUInt32LittleEndian(bytes.AsSpan(4 + i * 4));
        return ids;
    }

    /// <summary>Decodes a length-prefixed UTF-16 string, including its required terminator.</summary>
    internal static string ReadString(byte[] bytes, ref int offset)
    {
        if (offset >= bytes.Length) throw new InvalidDataException("Missing MTP string");
        var chars = bytes[offset++];
        if (chars == 0) return "";
        if (bytes.Length - offset < chars * 2 || bytes[offset + chars * 2 - 2] != 0 || bytes[offset + chars * 2 - 1] != 0)
            throw new InvalidDataException("Truncated or unterminated MTP string");
        var text = new UnicodeEncoding(false, false, true).GetString(bytes, offset, (chars - 1) * 2);
        offset += chars * 2;
        return text;
    }

    /// <summary>Parses object metadata and verifies that it belongs to the requested storage.</summary>
    private MtpObject GetInfo(uint storage, uint handle, CancellationToken token)
    {
        var bytes = Transfer(0x1008, [handle], token);
        if (bytes.Length < 53 || BinaryPrimitives.ReadUInt32LittleEndian(bytes) != storage)
            throw new InvalidDataException("Invalid MTP object metadata or storage mismatch");
        var cursor = 52;
        var name = ReadString(bytes, ref cursor);
        ReadString(bytes, ref cursor); // Capture time.
        var modified = ReadString(bytes, ref cursor);
        return new($"{generation}:object:{storage}:{handle}", name,
            BinaryPrimitives.ReadUInt16LittleEndian(bytes.AsSpan(4)) == 0x3001,
            BinaryPrimitives.ReadUInt32LittleEndian(bytes.AsSpan(8)), modified, storage, handle);
    }

    /// <summary>Rejects stale references instead of reusing storage/object IDs from another device session.</summary>
    private (uint Storage, uint Handle, bool Root) ParseReference(string reference)
    {
        var parts = reference.Split(':');
        if (parts.Length is not (3 or 4) || parts[0] != generation ||
            !uint.TryParse(parts[2], NumberStyles.None, CultureInfo.InvariantCulture, out var storage))
            throw new ArgumentException("Expired or invalid MTP reference; list files again");
        if (parts.Length == 3 && parts[1] == "storage") return (storage, 0, true);
        if (parts.Length == 4 && parts[1] == "object" && uint.TryParse(parts[3], NumberStyles.None, CultureInfo.InvariantCulture, out var handle))
            return (storage, handle, false);
        throw new ArgumentException("Invalid MTP reference");
    }

    /// <summary>Closes a healthy protocol session without reset; unsynchronized sessions are only released.</summary>
    public void Dispose()
    {
        gate.Wait();
        try
        {
            if (disposed) return;
            disposed = true;
            if (opened && !faulted)
            {
                try { Transfer(0x1003, [], CancellationToken.None, expectData: false); }
                catch { /* Shutdown must not replace protocol failure with a USB reset. */ }
            }
            transport.Dispose();
            lock (healthLock) opened = false;
        }
        finally { gate.Release(); }
    }
}

/// <summary>An explicit protocol rejection is synchronized and does not trigger transport recovery.</summary>
internal sealed class MtpResponseException(ushort code, ushort operation)
    : IOException($"MTP operation 0x{operation:x4} returned 0x{code:x4}")
{
    public ushort Code { get; } = code;
}

/// <summary>A rejected local request after a complete response leaves the MTP session synchronized.</summary>
internal sealed class MtpRequestException(string message) : IOException(message);
