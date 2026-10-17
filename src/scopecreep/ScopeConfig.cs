using System.Text.Json;

/// <summary>Configuration shared by MCP hosts and the standalone diagnostic client.</summary>
internal sealed class ScopeConfig
{
    public string DevicePath { get; set; } = "/dev/usbtmc0";
    public string HttpUrl { get; set; } = "http://127.0.0.1:3001";
    public int TimeoutMs { get; set; } = 5000;
    public bool RemoteEnabled { get; set; } = true;
    public string RemoteHost { get; set; } = "127.0.0.1";
    public int RemotePort { get; set; } = 8888;
    public string FfmpegPath { get; set; } = "ffmpeg";
    public int ScreenSettleMs { get; set; } = 5000;
    public string CaptureDirectory { get; set; } = "captures/scopecreep";
    public long MaxFileBytes { get; set; } = 512L * 1024 * 1024;
    public long MaxTemporaryBytes { get; set; } = 1024L * 1024 * 1024;

    /// <summary>Loads configuration and resolves relative device paths beside the configuration file.</summary>
    public static ScopeConfig Load(string? path)
    {
        var config = path is null
            ? new ScopeConfig()
            : JsonSerializer.Deserialize<ScopeConfig>(File.ReadAllText(path), new JsonSerializerOptions(JsonSerializerDefaults.Web))
                ?? throw new InvalidDataException("Empty Scopecreep configuration");
        if (config.TimeoutMs is < 100 or > 60000)
            throw new ArgumentException("timeoutMs must be between 100 and 60000");
        if (string.IsNullOrWhiteSpace(config.DevicePath))
            throw new ArgumentException("devicePath must not be empty");
        config.DevicePath = Path.GetFullPath(config.DevicePath,
            path is null ? Directory.GetCurrentDirectory() : Path.GetDirectoryName(Path.GetFullPath(path))!);
        var root = path is null ? Directory.GetCurrentDirectory() : Path.GetDirectoryName(Path.GetFullPath(path))!;
        config.CaptureDirectory = Path.GetFullPath(config.CaptureDirectory, root);
        if (config.FfmpegPath.Contains(Path.DirectorySeparatorChar))
            config.FfmpegPath = Path.GetFullPath(config.FfmpegPath, root);
        if (string.IsNullOrWhiteSpace(config.RemoteHost) || config.RemotePort is < 1 or > 65535 ||
            string.IsNullOrWhiteSpace(config.FfmpegPath) || config.ScreenSettleMs is < 0 or > 60000)
            throw new ArgumentException("Invalid remote endpoint, FFmpeg path, or screenSettleMs (0..60000)");
        if (config.MaxFileBytes < 1 || config.MaxTemporaryBytes < config.MaxFileBytes)
            throw new ArgumentException("File limits must be positive and maxTemporaryBytes >= maxFileBytes");
        if (!Uri.TryCreate(config.HttpUrl, UriKind.Absolute, out var url) || url.Scheme != "http")
            throw new ArgumentException("httpUrl must be an absolute http:// URL");
        return config;
    }
}
