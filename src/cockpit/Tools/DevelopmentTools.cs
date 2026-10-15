using System.ComponentModel;
using Microsoft.Extensions.Hosting;
using ModelContextProtocol.Server;

internal sealed class DevelopmentTools(IHostApplicationLifetime lifetime)
{
    [McpServerTool(Name = "mcp-reboot"), Description("Stop Cockpit after acknowledging this call so a development control loop can restart it.")]
    public string Reboot()
    {
        _ = Task.Run(async () =>
        {
            await Task.Delay(250);
            lifetime.StopApplication();
        });
        return "Cockpit is stopping for restart.";
    }
}
