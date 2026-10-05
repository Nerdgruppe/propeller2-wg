using System.ComponentModel;
using System.Text.Json;
using System.Text.RegularExpressions;
using ModelContextProtocol.Server;

internal sealed record TsvInstruction(string Syntax, string Category, string Encoding, string Description, string[] Columns);
internal sealed record InstructionInfo(string Mnemonic, JsonElement[] Options, TsvInstruction[] ReferenceRows);

internal sealed class InstructionTools(CockpitConfig config)
{
    [McpServerTool, Description("Return every encoding and operand option for an exact instruction mnemonic, with descriptions from the instruction reference.")]
    public InstructionInfo LookupInstruction([Description("Instruction mnemonic, for example CALLD")] string mnemonic)
    {
        var key = mnemonic.Trim().ToUpperInvariant();
        if (!Regex.IsMatch(key, "^[A-Z][A-Z0-9_]*$")) throw new ArgumentException("Invalid mnemonic");
        return Load().FirstOrDefault(x => x.Mnemonic == key) ?? new(key, [], []);
    }

    [McpServerTool, Description("Search instruction descriptions and mnemonics. Each result includes all variants of that mnemonic.")]
    public InstructionInfo[] SearchInstructions(
        [Description("Text to find in instruction descriptions or mnemonics")] string query,
        [Description("Maximum mnemonic groups to return, 1 through 100")] int limit = 20)
    {
        if (string.IsNullOrWhiteSpace(query)) throw new ArgumentException("query must not be empty");
        if (limit is < 1 or > 100) throw new ArgumentException("limit must be between 1 and 100");
        return Load().Where(x => x.Mnemonic.Contains(query, StringComparison.OrdinalIgnoreCase)
            || x.ReferenceRows.Any(r => r.Description.Contains(query, StringComparison.OrdinalIgnoreCase))
            || x.Options.Any(o => o.TryGetProperty("shortdesc", out var description)
                && description.ValueKind == JsonValueKind.String
                && description.GetString()!.Contains(query, StringComparison.OrdinalIgnoreCase)))
            .Take(limit).ToArray();
    }

    private InstructionInfo[] Load()
    {
        using var jsonStream = OpenData(config.P2instructionsJsonPath, "Cockpit.p2instructions.json");
        using var json = JsonDocument.Parse(jsonStream);
        var options = json.RootElement.EnumerateArray()
            .GroupBy(x => x.GetProperty("name").GetString()!, StringComparer.OrdinalIgnoreCase)
            .ToDictionary(x => x.Key.ToUpperInvariant(), x => x.Select(y => y.Clone()).ToArray());
        using var tsv = new StreamReader(OpenData(config.InstructionsTsvPath, "Cockpit.instructions.tsv"));
        var rows = tsv.ReadToEnd().Split('\n')
            .Select(line => line.Split('\t'))
            .Where(columns => columns.Length >= 6)
            .Select(columns => (columns, match: Regex.Match(columns[1], "^\\s*([A-Za-z][A-Za-z0-9_]*)")))
            .Where(x => x.match.Success)
            .GroupBy(x => x.match.Groups[1].Value.ToUpperInvariant())
            .ToDictionary(x => x.Key,
                x => x.Select(y => new TsvInstruction(y.columns[1].Trim(), y.columns[2].Trim(),
                    y.columns[3].Trim(), y.columns[5].Trim(), y.columns)).ToArray());
        return options.Keys.Union(rows.Keys).Order(StringComparer.Ordinal)
            .Select(key => new InstructionInfo(key,
                options.GetValueOrDefault(key) ?? [], rows.GetValueOrDefault(key) ?? []))
            .ToArray();
    }

    private static Stream OpenData(string? path, string resource) => path is null
        ? typeof(InstructionTools).Assembly.GetManifestResourceStream(resource)!
        : File.OpenRead(path);
}
