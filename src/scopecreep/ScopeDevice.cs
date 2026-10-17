using System.Diagnostics;

/// <summary>Serializes SCPI traffic and verifies communication after an incomplete transaction.</summary>
internal sealed class ScopeDevice : IDisposable
{
    private readonly ScopeConfig config;
    private readonly SemaphoreSlim gate = new(1);
    private readonly IScpiTransport transport;
    private bool unsynchronized;
    private bool disposed;
    public string? RecoveryDiagnostic { get; private set; }

    /// <summary>Constructs the production USBTMC device service.</summary>
    public ScopeDevice(ScopeConfig config) : this(config, new UsbTmcTransport(config.DevicePath)) { }

    /// <summary>Accepts a transcript transport for hardware-independent transaction tests.</summary>
    internal ScopeDevice(ScopeConfig config, IScpiTransport transport)
    {
        this.config = config;
        this.transport = transport;
    }

    /// <summary>Executes a transaction without abandoning the native worker on cancellation.</summary>
    public async Task<byte[]> ExecuteAsync(string command, bool query, int maxBytes, CancellationToken cancellationToken)
    {
        var bytes = ScpiProtocol.Encode(command, query);
        if (maxBytes is < 1 or > 16 * 1024 * 1024)
            throw new ArgumentException("maxBytes must be between 1 and 16777216");
        await gate.WaitAsync(cancellationToken);
        try
        {
            ObjectDisposedException.ThrowIf(disposed, this);
            return await Task.Run(() =>
            {
                try
                {
                    transport.Open();
                    if (unsynchronized)
                    {
                        try { Recover(cancellationToken); }
                        catch (IOException error)
                        {
                            throw new IOException("Communication recovery failed; the requested command was not sent. " +
                                "Reconnect the scope if identity checks keep failing. " + error.Message, error);
                        }
                    }
                    var clock = Stopwatch.StartNew();
                    transport.Write(bytes, clock, config.TimeoutMs, cancellationToken);
                    var result = query ? transport.Read(clock, config.TimeoutMs, maxBytes, cancellationToken) : [];
                    cancellationToken.ThrowIfCancellationRequested();
                    return result;
                }
                catch
                {
                    unsynchronized = true;
                    transport.Dispose();
                    throw;
                }
            }, CancellationToken.None);
        }
        finally { gate.Release(); }
    }

    /// <summary>Checks identity twice after recovery, never replaying the failed user command.</summary>
    private void Recover(CancellationToken cancellationToken)
    {
        // Firmware 5 rejects USBTMC clear; reopen and require matching identity replies instead.
        RecoveryDiagnostic = "Reopened device; USBTMC clear omitted because this firmware rejects it";
        for (var attempt = 0; attempt < 2; attempt++)
        {
            var clock = Stopwatch.StartNew();
            transport.Write(ScpiProtocol.Encode("*IDN?", true), clock, config.TimeoutMs, cancellationToken);
            var identity = ScpiProtocol.Text(transport.Read(clock, config.TimeoutMs, 4096, cancellationToken));
            if (!identity.StartsWith("Micsig,STO1104C,", StringComparison.Ordinal))
                throw new IOException($"Communication is unsynchronized; reconnect the scope. Unexpected identity: {identity}");
        }
        unsynchronized = false;
    }

    /// <summary>Waits for any native transaction before closing during host shutdown.</summary>
    public void Dispose()
    {
        gate.Wait();
        try
        {
            disposed = true;
            transport.Dispose();
        }
        finally { gate.Release(); }
    }
}
