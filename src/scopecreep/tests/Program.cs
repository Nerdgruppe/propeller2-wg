using System.Diagnostics;
using System.Text;

foreach (var input in new[] { "", " ", "*IDN?\n", "*IDN?\r", "*IDN?;*IDN?", "*IDN??", "*IDN?\t", "*IDN?µ" })
    Expect<ArgumentException>(() => ScpiProtocol.Encode(input, true));
Expect<ArgumentException>(() => ScpiProtocol.Encode("*IDN?", false));
Expect<ArgumentException>(() => ScpiProtocol.Encode(":MENU:STOP", true));
Check(ScpiProtocol.Text(ScpiProtocol.Encode(" :MEAS:FREQ? CH1 ", true)) == ":MEAS:FREQ? CH1", "ASCII encoding");
Check(ScpiProtocol.IsError(Encoding.ASCII.GetBytes("ErrorEnum:SCPI Command!")), "unterminated firmware error");
Check(!ScpiProtocol.IsError(Encoding.ASCII.GetBytes("0.0\r\n")), "zero is not a transport error");

var transcript = new Transcript();
using (var device = new ScopeDevice(new ScopeConfig(), transcript))
{
    transcript.Replies.Enqueue(() => throw new IOException("simulated timeout"));
    await ExpectAsync<IOException>(() => device.ExecuteAsync(":WAV:DATA?", true, 1024, default));
    transcript.Replies.Enqueue(Identity);
    transcript.Replies.Enqueue(Identity);
    transcript.Replies.Enqueue(() => Encoding.ASCII.GetBytes("1000.0\r\n"));
    var reply = await device.ExecuteAsync(":MEAS:FREQ? CH1", true, 1024, default);
    Check(ScpiProtocol.Text(reply) == "1000.0", "recovery reply");
    Check(transcript.Writes.SequenceEqual(new[] { ":WAV:DATA?", "*IDN?", "*IDN?", ":MEAS:FREQ? CH1" }),
        "failed user query must never be replayed");
}

var wrongScope = new Transcript();
using (var device = new ScopeDevice(new ScopeConfig(), wrongScope))
{
    wrongScope.Replies.Enqueue(() => throw new IOException("simulated disconnect"));
    await ExpectAsync<IOException>(() => device.ExecuteAsync(":WAV:DATA?", true, 1024, default));
    wrongScope.Replies.Enqueue(() => Encoding.ASCII.GetBytes("Other,Scope,1,2\r\n"));
    await ExpectAsync<IOException>(() => device.ExecuteAsync(":MENU:RUN", false, 1, default));
    Check(!wrongScope.Writes.Contains(":MENU:RUN"), "failed recovery blocks commands");
}

var blocking = new Transcript();
using (var device = new ScopeDevice(new ScopeConfig(), blocking))
using (var cancel = new CancellationTokenSource())
using (var entered = new ManualResetEventSlim())
using (var release = new ManualResetEventSlim())
{
    blocking.Replies.Enqueue(() =>
    {
        entered.Set();
        Check(release.Wait(TimeSpan.FromSeconds(5)), "release native read");
        return Identity();
    });
    var pending = device.ExecuteAsync("*IDN?", true, 1024, cancel.Token);
    Check(entered.Wait(TimeSpan.FromSeconds(5)), "native read entered");
    cancel.Cancel();
    var queued = device.ExecuteAsync(":MEAS:FREQ? CH1", true, 1024, default);
    await Task.Delay(50);
    Check(!pending.IsCompleted && !queued.IsCompleted && blocking.Writes.Count == 1,
        "cancellation must retain ownership until native I/O finishes");
    blocking.Replies.Enqueue(Identity);
    blocking.Replies.Enqueue(Identity);
    blocking.Replies.Enqueue(() => Encoding.ASCII.GetBytes("1000.0\r\n"));
    release.Set();
    await ExpectAsync<OperationCanceledException>(() => pending);
    Check(ScpiProtocol.Text(await queued) == "1000.0", "queued request after cancellation recovery");
}

var validation = new Transcript();
using (var device = new ScopeDevice(new ScopeConfig(), validation))
{
    await ExpectAsync<ArgumentException>(() => device.ExecuteAsync("*IDN?", true, 0, default));
    await ExpectAsync<ArgumentException>(() => device.ExecuteAsync("*IDN?", true, int.MaxValue, default));
    Check(validation.Writes.Count == 0, "invalid inputs perform no I/O");
    await device.ExecuteAsync(":MENU:STOP", false, 1, default);
    Check(validation.Writes.Count == 1, "command sent once");
}
var disposed = new ScopeDevice(new ScopeConfig(), new Transcript());
disposed.Dispose();
await ExpectAsync<ObjectDisposedException>(() => disposed.ExecuteAsync("*IDN?", true, 1024, default));
Console.WriteLine("PASS: SCPI validation, firmware errors, recovery/no replay, cancellation ownership, bounds, disposal");
await IntegrationChecks.RunAsync();

/// <summary>Returns the instrument identity used by recorded recovery fixtures.</summary>
static byte[] Identity() => Encoding.ASCII.GetBytes("Micsig,STO1104C,16298,5\r\n");

/// <summary>Fails a check with a readable diagnostic.</summary>
static void Check(bool condition, string description)
{
    if (!condition) throw new Exception(description);
}

/// <summary>Checks synchronous validation failures.</summary>
static void Expect<T>(Action action) where T : Exception
{
    try { action(); }
    catch (T) { return; }
    throw new Exception($"Expected {typeof(T).Name}");
}

/// <summary>Checks asynchronous I/O and cancellation failures.</summary>
static async Task ExpectAsync<T>(Func<Task> action) where T : Exception
{
    try { await action(); }
    catch (T) { return; }
    throw new Exception($"Expected {typeof(T).Name}");
}

/// <summary>Supplies deterministic native-call outcomes without connecting to hardware.</summary>
sealed class Transcript : IScpiTransport
{
    public List<string> Writes { get; } = [];
    public Queue<Func<byte[]>> Replies { get; } = new();

    /// <summary>Represents a successful device open.</summary>
    public void Open() { }
    /// <summary>Records the exact command sent by the production transaction service.</summary>
    public void Write(byte[] data, Stopwatch clock, int timeoutMs, CancellationToken cancellationToken)
    {
        Writes.Add(ScpiProtocol.Text(data));
    }
    /// <summary>Completes or fails the next native read according to the test transcript.</summary>
    public byte[] Read(Stopwatch clock, int timeoutMs, int maxBytes, CancellationToken cancellationToken)
    {
        return Replies.Dequeue()();
    }
    /// <summary>Represents closing the descriptor between recovery attempts.</summary>
    public void Dispose() { }
}
