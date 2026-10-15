using System.Text.Json;

internal sealed class CockpitConfig
{
    public string P2aasEndpoint { get; set; } = "ws://127.0.0.1:12880/";
    public string FlexspinPath { get; set; } = "flexspin";
    public string PropanPath { get; set; } = "propan";
    public string? InstructionsTsvPath { get; set; }
    public string? P2instructionsJsonPath { get; set; }
    public string HttpUrl { get; set; } = "http://127.0.0.1:3000";

    public static CockpitConfig Load(string? path)
    {
        var config = path is null
            ? new CockpitConfig()
            : JsonSerializer.Deserialize<CockpitConfig>(File.ReadAllText(path), new JsonSerializerOptions(JsonSerializerDefaults.Web))
                ?? throw new InvalidDataException("Empty Cockpit configuration");
        var root = path is null ? FindRepository() : Path.GetDirectoryName(Path.GetFullPath(path))!;
        if (path is null && root is not null)
        {
            config.InstructionsTsvPath = "data/encoding/instructions.tsv";
            config.P2instructionsJsonPath = "data/encoding/p2instructions.json";
        }
        if (config.InstructionsTsvPath is not null)
            config.InstructionsTsvPath = Path.GetFullPath(config.InstructionsTsvPath, root!);
        if (config.P2instructionsJsonPath is not null)
            config.P2instructionsJsonPath = Path.GetFullPath(config.P2instructionsJsonPath, root!);
        if (config.FlexspinPath.IndexOfAny([Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar]) >= 0)
            config.FlexspinPath = Path.GetFullPath(config.FlexspinPath, root ?? Directory.GetCurrentDirectory());
        if (config.PropanPath.IndexOfAny([Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar]) >= 0)
            config.PropanPath = Path.GetFullPath(config.PropanPath, root ?? Directory.GetCurrentDirectory());
        return config;
    }

    private static string? FindRepository()
    {
        foreach (var start in new[] { Directory.GetCurrentDirectory(), AppContext.BaseDirectory })
            for (var dir = new DirectoryInfo(start); dir is not null; dir = dir.Parent)
                if (File.Exists(Path.Combine(dir.FullName, "data/encoding/p2instructions.json")))
                    return dir.FullName;
        return null;
    }
}
