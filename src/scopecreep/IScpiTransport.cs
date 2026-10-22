using System.Diagnostics;

/// <summary>Device I/O boundary, implemented by USBTMC and deterministic test transcripts.</summary>
internal interface IScpiTransport : IDisposable
{
    /// <summary>Opens the underlying device when needed.</summary>
    void Open();
    /// <summary>Transmits a complete SCPI message once.</summary>
    void Write(byte[] data, Stopwatch clock, int timeoutMs, CancellationToken cancellationToken);
    /// <summary>Receives a complete response within the configured byte limit.</summary>
    byte[] Read(Stopwatch clock, int timeoutMs, int maxBytes, CancellationToken cancellationToken);
}
