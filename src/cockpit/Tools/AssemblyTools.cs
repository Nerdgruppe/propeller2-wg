using System.Buffers.Binary;
using System.ComponentModel;
using System.Diagnostics;
using System.Globalization;
using System.Net.WebSockets;
using System.Text;
using ModelContextProtocol.Server;

internal sealed record AssemblyResult(bool Success, string Diagnostics, int? ImageSize, string? Output);

internal sealed class AssemblyTools(CockpitConfig config)
{
    [McpServerTool, Description("Assemble Propan or FlexSpin PASM2 DAT source. FlexSpin uses temporary source and binary files that are deleted after assembly.")]
    public async Task<AssemblyResult> Assemble(
        [Description("Source language: propan or pasm2 (FlexSpin DAT mode)")] string language,
        [Description("Complete source code")] string code,
        [Description("Output format: hex, listing, or none")] string format = "hex")
    {
        if (format is not ("hex" or "listing" or "none"))
            throw new ArgumentException("format must be hex, listing, or none");
        if (format == "listing" && language != "propan")
            throw new ArgumentException("listing is available only for Propan");
        var result = await Compile(language, code, format == "listing");
        return new(result.Success, result.Diagnostics, format == "listing" ? null : result.Image.Length,
            !result.Success || format == "none" ? null : format == "listing" ? result.Listing : HexDump(result.Image));
    }

    [McpServerTool, Description("Assemble source, upload it to P2AAS, send stdin bytes, and return captured Propeller output. Numeric formats use little-endian words.")]
    public async Task<object> Run(
        [Description("Source language: propan or pasm2 (FlexSpin DAT mode)")] string language,
        [Description("Complete source code")] string code,
        [Description("UTF-8 data to send to the board after upload")] string stdin = "",
        [Description("Output format: hex, text, u8, i8, u16, i16, u32, or i32")] string format = "text",
        [Description("Optional base64 stdin bytes; cannot be combined with stdin text")] string? stdinBase64 = null,
        [Description("Serial baud rate (default 115200)")] int baudrate = 115200,
        [Description("Board execution timeout in milliseconds (default 5000)")] int timeout_ms = 5000,
        [Description("Wrap a Propan or PASM2 snippet in the 200 MHz UART startup and read/write helpers")] bool scaffold = false)
    {
        if (format is not ("hex" or "text" or "u8" or "i8" or "u16" or "i16" or "u32" or "i32"))
            throw new ArgumentException("Invalid output format");
        if (baudrate <= 0) throw new ArgumentException("baudrate must be positive");
        if (timeout_ms <= 0) throw new ArgumentException("timeout_ms must be positive");
        if (stdin.Length > 0 && stdinBase64 is not null)
            throw new ArgumentException("Supply either stdin or stdinBase64");
        var input = stdinBase64 is null ? Encoding.UTF8.GetBytes(stdin) : Convert.FromBase64String(stdinBase64);
        if (input.Length > 1_048_576) throw new ArgumentException("stdin is limited to 1 MiB");
        if (scaffold) code = ExpandScaffold(language, code, baudrate);
        var assembly = await Compile(language, code, false);
        if (!assembly.Success)
            return new { success = false, diagnostics = assembly.Diagnostics, output = (object?)null };
        if (assembly.Image.Length == 0 || assembly.Image.Length > 512 * 1024)
            return new { success = false, diagnostics = "P2AAS requires a nonempty image of at most 512 KiB", output = (object?)null };

        using var timeout = new CancellationTokenSource(TimeSpan.FromMilliseconds((long)timeout_ms + 15_000));
        using var socket = new ClientWebSocket();
        socket.Options.CollectHttpResponseDetails = true;
        using var output = new MemoryStream();
        var uploaded = false;
        try
        {
            var endpoint = new UriBuilder(config.P2aasEndpoint)
            {
                Query = $"baudrate={baudrate}&timeout_ms={timeout_ms}",
            }.Uri;
            if (endpoint.Scheme is not ("ws" or "wss")) throw new ArgumentException("P2AAS endpoint must be ws:// or wss://");
            await socket.ConnectAsync(endpoint, timeout.Token);
            var paddedLength = (assembly.Image.Length + 3) & ~3;
            if (paddedLength > 512 * 1024)
                return new { success = false, diagnostics = "P2AAS image exceeds 512 KiB after long alignment", output = (object?)null };
            var packet = new byte[paddedLength + 4];
            BinaryPrimitives.WriteInt32LittleEndian(packet, paddedLength);
            assembly.Image.CopyTo(packet.AsSpan(4));
            await socket.SendAsync(packet, WebSocketMessageType.Binary, true, timeout.Token);
            if (input.Length > 0)
            {
                if (scaffold) await Task.Delay(Math.Min(120, timeout_ms / 2), timeout.Token);
                await socket.SendAsync(input, WebSocketMessageType.Binary, true, timeout.Token);
            }
            uploaded = true;

            var buffer = new byte[8192];
            WebSocketCloseStatus? closeStatus = null;
            while (true)
            {
                var received = await socket.ReceiveAsync(buffer, timeout.Token);
                if (received.MessageType == WebSocketMessageType.Close)
                {
                    closeStatus = received.CloseStatus;
                    break;
                }
                var remaining = 1_048_576 - (int)output.Length;
                output.Write(buffer, 0, Math.Min(remaining, received.Count));
                if (received.Count > remaining)
                    return new { success = false, diagnostics = assembly.Diagnostics, complete = false, truncated = true,
                        output = FormatOutput(output.ToArray(), format) };
            }
            return new { success = closeStatus == WebSocketCloseStatus.NormalClosure,
                diagnostics = assembly.Diagnostics, complete = true, truncated = false, closeStatus = closeStatus?.ToString(),
                closeReason = socket.CloseStatusDescription,
                output = FormatOutput(output.ToArray(), format) };
        }
        catch (OperationCanceledException)
        {
            if (uploaded)
                return new { success = true, diagnostics = assembly.Diagnostics, complete = true, truncated = false,
                    output = FormatOutput(output.ToArray(), format), closeStatus = "Timeout" };
            return new { success = false, diagnostics = assembly.Diagnostics, complete = false, truncated = false,
                output = FormatOutput(output.ToArray(), format), error = "P2AAS operation timed out" };
        }
        catch (WebSocketException error)
        {
            if (uploaded)
                return new { success = true, diagnostics = assembly.Diagnostics, complete = true, truncated = false,
                    output = FormatOutput(output.ToArray(), format), closeStatus = "Disconnected" };
            string? p2aasError = null;
            if (socket.HttpResponseHeaders is { } headers && headers.TryGetValue("x-p2aas-error", out var values))
                p2aasError = values.FirstOrDefault();
            return new { success = false, diagnostics = assembly.Diagnostics, complete = false,
                output = FormatOutput(output.ToArray(), format), httpStatus = (int)socket.HttpStatusCode,
                error = p2aasError ?? error.Message };
        }
    }

    private static string ExpandScaffold(string language, string code, int baudrate)
    {
        var template = language switch
        {
            "propan" => "Cockpit.RunScaffold.propan",
            "pasm2" => "Cockpit.RunScaffold.spin2",
            _ => throw new ArgumentException("language must be propan or pasm2"),
        };
        using var reader = new StreamReader(typeof(AssemblyTools).Assembly.GetManifestResourceStream(template)!);
        var uartConfig = ((200_000_000L << 16) / baudrate & ~1023L) | 7;
        if (uartConfig > uint.MaxValue) throw new ArgumentException("baudrate is too low for the 200 MHz UART");
        return reader.ReadToEnd()
            .Replace("{{BAUDRATE}}", baudrate.ToString(CultureInfo.InvariantCulture))
            .Replace("{{UART_CFG}}", uartConfig.ToString("X8", CultureInfo.InvariantCulture))
            .Replace(language == "propan" ? "        // {{SNIPPET}}" : "        ' {{SNIPPET}}", code);
    }

    private async Task<(bool Success, string Diagnostics, byte[] Image, string? Listing)> Compile(string language, string code, bool listing)
    {
        if (language is not ("propan" or "pasm2"))
            throw new ArgumentException("language must be propan or pasm2");
        if (Encoding.UTF8.GetByteCount(code) > 1_048_576) throw new ArgumentException("code is limited to 1 MiB");
        if (language == "propan")
        {
            var args = listing ? new[] { "-f", "none", "--list-file", "-", "-" } : new[] { "-o", "-", "-" };
            var result = await Execute(config.PropanPath, args, Encoding.UTF8.GetBytes(code));
            return (result.ExitCode == 0, result.Stderr, listing ? [] : result.Stdout,
                listing ? Encoding.UTF8.GetString(result.Stdout) : null);
        }

        var sourcePath = Path.GetTempFileName();
        string? binaryPath = null;
        try
        {
            binaryPath = Path.GetTempFileName();
            await File.WriteAllTextAsync(sourcePath, code);
            var result = await Execute(config.FlexspinPath,
                ["-q", "-2", "-c", "-o", binaryPath, sourcePath], null);
            var image = await File.ReadAllBytesAsync(binaryPath);
            var diagnostics = (Encoding.UTF8.GetString(result.Stdout) + result.Stderr).Trim();
            return (result.ExitCode == 0, diagnostics, image, null);
        }
        finally
        {
            File.Delete(sourcePath);
            if (binaryPath is not null) File.Delete(binaryPath);
        }
    }

    private static async Task<(int ExitCode, byte[] Stdout, string Stderr)> Execute(string executable, string[] args, byte[]? stdin)
    {
        var info = new ProcessStartInfo(executable)
        {
            WorkingDirectory = Path.GetTempPath(),
            RedirectStandardInput = stdin is not null,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
        };
        foreach (var arg in args) info.ArgumentList.Add(arg);
        using var process = Process.Start(info) ?? throw new InvalidOperationException($"Could not start {executable}");
        var stdout = new MemoryStream();
        var stdoutTask = process.StandardOutput.BaseStream.CopyToAsync(stdout);
        var stderrTask = process.StandardError.ReadToEndAsync();
        using var timeout = new CancellationTokenSource(TimeSpan.FromSeconds(20));
        try
        {
            if (stdin is not null)
            {
                try { await process.StandardInput.BaseStream.WriteAsync(stdin, timeout.Token); }
                catch (IOException) { /* The compiler exited before consuming the whole source. */ }
                process.StandardInput.Close();
            }
            await process.WaitForExitAsync(timeout.Token);
        }
        catch (OperationCanceledException)
        {
            process.Kill(entireProcessTree: true);
            await process.WaitForExitAsync();
            throw new TimeoutException($"{executable} took more than 20 seconds");
        }
        await Task.WhenAll(stdoutTask, stderrTask);
        return (process.ExitCode, stdout.ToArray(), await stderrTask);
    }

    private static object FormatOutput(byte[] data, string format)
    {
        if (format == "hex") return HexDump(data);
        if (format == "text") return Encoding.UTF8.GetString(data);
        var width = int.Parse(format.AsSpan(1));
        var bytes = width / 8;
        var values = new long[data.Length / bytes];
        for (var i = 0; i < values.Length; i++)
        {
            var span = data.AsSpan(i * bytes, bytes);
            values[i] = format switch
            {
                "u8" => span[0], "i8" => (sbyte)span[0],
                "u16" => BinaryPrimitives.ReadUInt16LittleEndian(span),
                "i16" => BinaryPrimitives.ReadInt16LittleEndian(span),
                "u32" => BinaryPrimitives.ReadUInt32LittleEndian(span),
                "i32" => BinaryPrimitives.ReadInt32LittleEndian(span),
                _ => throw new ArgumentException("Invalid numeric format"),
            };
        }
        return new { values, trailingBytes = data.Length % bytes };
    }

    private static string HexDump(byte[] data)
    {
        var result = new StringBuilder();
        for (var offset = 0; offset < data.Length; offset += 16)
        {
            var row = data.AsSpan(offset, Math.Min(16, data.Length - offset));
            result.Append(offset.ToString("x8")).Append("  ");
            for (var i = 0; i < 16; i++)
            {
                result.Append(i < row.Length ? row[i].ToString("x2") : "  ").Append(' ');
                if (i == 7) result.Append(' ');
            }
            result.Append(" |");
            foreach (var b in row) result.Append(b is >= 32 and <= 126 ? (char)b : '.');
            result.AppendLine("|");
        }
        return result.Append(data.Length.ToString("x8")).ToString();
    }

}
