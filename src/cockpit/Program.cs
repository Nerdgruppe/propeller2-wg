using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;

var http = false;
var development = false;
string? configPath = null;
for (var i = 0; i < args.Length; i++)
{
    if (args[i] == "--http") http = true;
    else if (args[i] == "--development") development = true;
    else if (args[i] == "--config" && i + 1 < args.Length) configPath = args[++i];
    else throw new ArgumentException($"Unknown or incomplete argument: {args[i]}");
}

var config = CockpitConfig.Load(configPath);
if (http)
{
    var builder = WebApplication.CreateBuilder();
    builder.WebHost.UseUrls(config.HttpUrl);
    builder.Services.AddSingleton(config);
    var mcp = builder.Services.AddMcpServer()
        .WithHttpTransport(options => options.Stateless = true)
        .WithTools<AssemblyTools>()
        .WithTools<InstructionTools>();
    if (development) mcp.WithTools<DevelopmentTools>();
    var app = builder.Build();
    app.MapMcp("/mcp");
    await app.RunAsync();
}
else
{
    var builder = Host.CreateApplicationBuilder();
    builder.Logging.AddConsole(o => o.LogToStandardErrorThreshold = LogLevel.Trace);
    builder.Services.AddSingleton(config);
    var mcp = builder.Services.AddMcpServer()
        .WithStdioServerTransport()
        .WithTools<AssemblyTools>()
        .WithTools<InstructionTools>();
    if (development) mcp.WithTools<DevelopmentTools>();
    await builder.Build().RunAsync();
}
