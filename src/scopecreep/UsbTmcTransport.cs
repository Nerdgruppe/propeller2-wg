using System.ComponentModel;
using System.Diagnostics;
using System.Runtime.InteropServices;
using Microsoft.Win32.SafeHandles;

/// <summary>Owns a Linux USBTMC descriptor; the caller serializes complete SCPI transactions.</summary>
internal sealed class UsbTmcTransport(string path) : IScpiTransport
{
    // Linux asm-generic ioctl encoding shared by the supported x64/arm64 targets.
    private const nuint SetTimeout = 0x40045b0a;
    private const nuint MessageInAttributes = 0x80015b18;
    private const byte EndOfMessage = 1;
    private const int OpenReadWriteCloseOnExec = 2 | 0x80000;
    private SafeFileHandle? handle;

    /// <summary>Opens lazily so a missing scope does not prevent MCP startup.</summary>
    public void Open()
    {
        if (!OperatingSystem.IsLinux())
            throw new PlatformNotSupportedException("Scopecreep requires Linux's usbtmc driver");
        if (handle is { IsInvalid: false, IsClosed: false }) return;
        var fd = NativeOpen(path, OpenReadWriteCloseOnExec);
        if (fd < 0) throw Error($"Cannot open {path}; check the device path and read/write permissions");
        handle = new SafeFileHandle(fd, ownsHandle: true);
    }

    /// <summary>Writes once; a short write fails instead of replaying an uncertain command.</summary>
    public void Write(byte[] data, Stopwatch clock, int timeoutMs, CancellationToken cancellationToken)
    {
        SetRemainingTimeout(clock, timeoutMs, cancellationToken);
        var written = NativeWrite(handle!, data, (nuint)data.Length);
        if (written < 0) throw Error("USBTMC write failed; command was not retried");
        if (written != data.Length) throw new IOException($"Short USBTMC write: {written}/{data.Length} bytes");
    }

    /// <summary>Reads to USBTMC EOM, preserving binary data and enforcing a total response limit.</summary>
    public byte[] Read(Stopwatch clock, int timeoutMs, int maxBytes, CancellationToken cancellationToken)
    {
        using var result = new MemoryStream();
        var buffer = new byte[4096];
        while (true)
        {
            SetRemainingTimeout(clock, timeoutMs, cancellationToken);
            var count = NativeRead(handle!, buffer, (nuint)Math.Min(buffer.Length, maxBytes + 1 - result.Length));
            if (count < 0)
            {
                // An interrupted read may have consumed part of a USB message; recover before another transaction.
                throw Error("USBTMC read failed");
            }
            if (count == 0) throw new IOException("USBTMC returned an empty transfer before EOM");
            result.Write(buffer, 0, (int)count);
            if (result.Length > maxBytes) throw new IOException($"SCPI response exceeds {maxBytes} bytes");
            byte attributes = 0;
            if (NativeIoctlByte(handle!, MessageInAttributes, ref attributes) < 0)
                throw Error("Cannot obtain USBTMC EOM attributes");
            if ((attributes & EndOfMessage) != 0) return result.ToArray();
        }
    }

    /// <summary>Bounds each native transfer by the remaining transaction time and checks cancellation.</summary>
    private void SetRemainingTimeout(Stopwatch clock, int timeoutMs, CancellationToken cancellationToken)
    {
        cancellationToken.ThrowIfCancellationRequested();
        var remaining = timeoutMs - clock.ElapsedMilliseconds;
        if (remaining <= 0) throw new TimeoutException("SCPI transaction deadline expired");
        // The kernel rejects smaller timeouts; the final transfer can overshoot by at most this floor.
        var nativeTimeout = (uint)Math.Max(remaining, 100);
        if (NativeIoctlUInt(handle!, SetTimeout, ref nativeTimeout) < 0)
            throw Error("Cannot set USBTMC timeout");
    }

    /// <summary>Closes after the caller has finished native I/O.</summary>
    public void Dispose()
    {
        handle?.Dispose();
        handle = null;
    }

    /// <summary>Captures errno before any other native calls can overwrite it.</summary>
    private static IOException Error(string message)
    {
        var code = Marshal.GetLastPInvokeError();
        return new IOException($"{message}: {new Win32Exception(code).Message} (errno {code})");
    }

    /// <summary>Opens a Linux descriptor.</summary>
    [DllImport("libc", EntryPoint = "open", SetLastError = true)]
    private static extern int NativeOpen([MarshalAs(UnmanagedType.LPUTF8Str)] string path, int flags);
    /// <summary>Reads bytes from a Linux descriptor.</summary>
    [DllImport("libc", EntryPoint = "read", SetLastError = true)]
    private static extern nint NativeRead(SafeFileHandle fd, [Out] byte[] buffer, nuint count);
    /// <summary>Writes bytes to a Linux descriptor.</summary>
    [DllImport("libc", EntryPoint = "write", SetLastError = true)]
    private static extern nint NativeWrite(SafeFileHandle fd, byte[] buffer, nuint count);
    /// <summary>Invokes a USBTMC ioctl taking a 32-bit value.</summary>
    [DllImport("libc", EntryPoint = "ioctl", SetLastError = true)]
    private static extern int NativeIoctlUInt(SafeFileHandle fd, nuint request, ref uint value);
    /// <summary>Invokes a USBTMC ioctl taking a byte value.</summary>
    [DllImport("libc", EntryPoint = "ioctl", SetLastError = true)]
    private static extern int NativeIoctlByte(SafeFileHandle fd, nuint request, ref byte value);
}
