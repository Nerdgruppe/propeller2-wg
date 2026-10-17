using System.Runtime.InteropServices;

namespace Scopecreep.Relay;

/// <summary>Runs the remote-display relay independently of the MCP server.</summary>
internal static class Program
{
    /// <summary>Starts the relay and closes both listeners and connections on Ctrl+C or SIGTERM.</summary>
    public static async Task<int> Main(string[] args)
    {
        if (args.Length != 0)
        {
            Console.Error.WriteLine("This relay takes no arguments; endpoints are hardcoded in RemoteRelay.cs");
            return 1;
        }
        using var shutdown = new CancellationTokenSource();
        Console.CancelKeyPress += (_, e) =>
        {
            e.Cancel = true;
            shutdown.Cancel();
        };
        using var terminate = OperatingSystem.IsLinux()
            ? PosixSignalRegistration.Create(PosixSignal.SIGTERM, context =>
            {
                context.Cancel = true;
                shutdown.Cancel();
            })
            : null;

        try
        {
            await new RemoteRelay().RunAsync(shutdown.Token);
            return 0;
        }
        catch (OperationCanceledException) when (shutdown.IsCancellationRequested)
        {
            return 0;
        }
        catch (Exception error)
        {
            Console.Error.WriteLine($"Relay stopped: {error.Message}");
            return 1;
        }
    }
}
