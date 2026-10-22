using System.Diagnostics;
using System.Text.Json;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;

var http = false;
string? configPath = null;
string? diagnosticsPath = null;
for (var i = 0; i < args.Length; i++)
{
    switch (args[i])
    {
        case "--http": http = true; break;
        case "--config" when i + 1 < args.Length: configPath = args[++i]; break;
        case "--diagnose" when i + 1 < args.Length: diagnosticsPath = args[++i]; break;
        default: throw new ArgumentException($"Unknown or incomplete argument: {args[i]}");
    }
}
var config = ScopeConfig.Load(configPath);
if (diagnosticsPath is not null)
{
    if (http) throw new ArgumentException("--diagnose cannot be combined with --http");
    using var device = new ScopeDevice(config);
    var steps = JsonSerializer.Deserialize<DiagnosticStep[]>(File.ReadAllText(diagnosticsPath),
        new JsonSerializerOptions(JsonSerializerDefaults.Web)) ?? throw new InvalidDataException("Empty diagnostics script");
    foreach (var step in steps)
    {
        ScpiProtocol.Encode(step.Command, step.Query);
        if (step.DelayMs is < 0 or > 5000 || step.MaxBytes is < 1 or > 16777216)
            throw new ArgumentException("Diagnostic delayMs must be 0..5000 and maxBytes 1..16777216");
    }
    foreach (var step in steps)
    {
        await Task.Delay(step.DelayMs);
        var clock = Stopwatch.StartNew();
        try
        {
            var reply = await device.ExecuteAsync(step.Command, step.Query, step.MaxBytes, CancellationToken.None);
            if (ScpiProtocol.IsError(reply)) Environment.ExitCode = 1;
            Console.WriteLine(JsonSerializer.Serialize(new
            {
                step.Command,
                step.Query,
                elapsedMs = clock.ElapsedMilliseconds,
                firmwareError = ScpiProtocol.IsError(reply),
                text = ScpiProtocol.Text(reply),
                base64 = Convert.ToBase64String(reply),
                recovery = device.RecoveryDiagnostic
            }));
            if (step.ExpectedText is not null && ScpiProtocol.Text(reply) != step.ExpectedText)
            {
                Console.Error.WriteLine($"Expected reply {step.ExpectedText}; stopping diagnostic script");
                Environment.ExitCode = 1;
                break;
            }
        }
        catch (Exception error)
        {
            Environment.ExitCode = 1;
            Console.WriteLine(JsonSerializer.Serialize(new
            {
                step.Command,
                step.Query,
                elapsedMs = clock.ElapsedMilliseconds,
                error = error.Message,
                recovery = device.RecoveryDiagnostic
            }));
            if (step.ExpectedText is not null) break;
        }
    }
}
else if (http)
{
    var builder = WebApplication.CreateBuilder();
    builder.WebHost.UseUrls(config.HttpUrl);
    builder.Services.AddSingleton(config);
    builder.Services.AddSingleton<ScopeDevice>();
    builder.Services.AddSingleton<RemoteDisplay>();
    builder.Services.AddHostedService(sp => sp.GetRequiredService<RemoteDisplay>());
    builder.Services.AddSingleton<ScopeController>();
    builder.Services.AddSingleton<MtpDevice>();
    builder.Services.AddSingleton<CaptureFiles>();
    builder.Services.AddMcpServer().WithHttpTransport(o => o.Stateless = true).WithTools<ScopeTools>();
    var app = builder.Build();
    app.MapMcp("/mcp");
    await app.RunAsync();
}
else
{
    var builder = Host.CreateApplicationBuilder();
    builder.Logging.AddConsole(o => o.LogToStandardErrorThreshold = LogLevel.Trace);
    builder.Services.AddSingleton(config);
    builder.Services.AddSingleton<ScopeDevice>();
    builder.Services.AddSingleton<RemoteDisplay>();
    builder.Services.AddHostedService(sp => sp.GetRequiredService<RemoteDisplay>());
    builder.Services.AddSingleton<ScopeController>();
    builder.Services.AddSingleton<MtpDevice>();
    builder.Services.AddSingleton<CaptureFiles>();
    builder.Services.AddMcpServer().WithStdioServerTransport().WithTools<ScopeTools>();
    await builder.Build().RunAsync();
}

/// <summary>One explicit diagnostic operation; query replies retain their exact bytes in base64.</summary>
internal sealed record DiagnosticStep(string Command, bool Query, int MaxBytes = 65536, int DelayMs = 0,
    string? ExpectedText = null);
