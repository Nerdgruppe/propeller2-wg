using System.ComponentModel;
using System.Diagnostics;
using System.Text.Json;
using ModelContextProtocol;
using ModelContextProtocol.Protocol;
using ModelContextProtocol.Server;

/// <summary>Agent-directed USB, visual control, and read-only MTP tools with explicit observation boundaries.</summary>
[McpServerToolType]
internal sealed class ScopeTools(ScopeDevice device, ScopeController controller, RemoteDisplay remote,
    MtpDevice mtp, CaptureFiles files, ScopeConfig config)
{
    private static readonly JsonSerializerOptions Json = new(JsonSerializerDefaults.Web);

    /// <summary>Reports independent connection health; unavailable USB does not hide available video or MTP diagnostics.</summary>
    [McpServerTool(Name = "scope_status"), Description("Read identity and acquisition status, plus independent video/decoder/MTP health and verified capabilities. Waveform sample scaling remains unverified.")]
    public async Task<object> Status(CancellationToken cancellationToken)
    {
        string? identity = null, acquisitionStatus = null, error = null;
        try
        {
            var id = await device.ExecuteAsync("*IDN?", true, 4096, cancellationToken);
            var status = await device.ExecuteAsync(":TRIGGER:STATUS?", true, 4096, cancellationToken);
            identity = ScpiProtocol.Text(id); acquisitionStatus = ScpiProtocol.Text(status);
            if (ScpiProtocol.IsError(id) || ScpiProtocol.IsError(status)) error = "Firmware rejected a status query";
        }
        catch (Exception failure) when (failure is not OperationCanceledException) { error = failure.Message; }
        return new { usb = new { connected = error is null, identity, acquisitionStatus, error }, remote = remote.Health,
            mtp = mtp.Health, lastAction = controller.LastAction, waveformsVerified = false,
            capabilities = new { screen = true, leftPress = true, leftDrag = true, hover = false,
                verifiedUsbSettings = ScopeController.VerifiedUsbSettings, mtpReadOnly = true },
            warning = "Video timestamps are host observations, not instrument capture times. File/sample scaling is not verified." };
    }

    /// <summary>Runs one raw mutation under the same coordinator as gestures and configuration.</summary>
    [McpServerTool(Name = "scpi_command"), Description("Send one SCPI command once. Returns an action receipt; uncertain writes are not replayed or replaced by GUI actions. Inspect scope_screen(afterActionId) afterward.")]
    public async Task<CallToolResult> Command(string command, CancellationToken cancellationToken)
        => await GuardAsync(async () => ActionResult(await controller.CommandAsync(command, cancellationToken)));

    /// <summary>Preserves exact raw replies without inferring physical units or freshness.</summary>
    [McpServerTool(Name = "scpi_query"), Description("Send one SCPI query. format text or base64, maxBytes 1..16777216. Numeric replies may be cached or have unverified units.")]
    public async Task<object> Query(string command, int maxBytes = 65536, string format = "text", CancellationToken cancellationToken = default)
    {
        if (format is not ("text" or "base64")) throw new McpException("format must be text or base64");
        var reply = await GuardAsync(() => device.ExecuteAsync(command, true, maxBytes, cancellationToken));
        return new { success = !ScpiProtocol.IsError(reply), text = format == "text" ? ScpiProtocol.Text(reply) : null,
            base64 = format == "base64" ? Convert.ToBase64String(reply) : null, byteCount = reply.Length,
            recovery = device.RecoveryDiagnostic };
    }

    /// <summary>Returns a native PNG content block and explicit host-side freshness metadata.</summary>
    [McpServerTool(Name = "scope_screen"), Description("Capture the live 800x600 screen. For action verification, pass afterActionId; waits settleMs (default configured 5000) then a newer frame. Video has several seconds of latency: visually confirm the intended result, and request another image if needed. Superseded means other actions intervened.")]
    public async Task<CallToolResult> Screen(string? afterActionId = null, int? settleMs = null, int timeoutMs = 15000,
        CancellationToken cancellationToken = default)
        => await GuardAsync(() => ScreenCore(afterActionId, settleMs, timeoutMs, cancellationToken));

    /// <summary>Applies host-side settling and returns PNG plus observation metadata.</summary>
    private async Task<CallToolResult> ScreenCore(string? afterActionId, int? settleMs, int timeoutMs, CancellationToken cancellationToken)
    {
        var settle = settleMs ?? config.ScreenSettleMs;
        if (settle is < 0 or > 60000 || timeoutMs is < 100 or > 120000)
            throw new ArgumentException("settleMs 0..60000; timeoutMs 100..120000");
        var action = afterActionId is null ? null : controller.GetAction(afterActionId);
        var deadline = action is null ? Stopwatch.GetTimestamp() - Stopwatch.Frequency * 2 :
            action.Timestamp!.Value + (long)(settle * (double)Stopwatch.Frequency / 1000);
        var frame = await remote.CaptureAsync(deadline, timeoutMs, cancellationToken);
        var last = controller.LastAction;
        var result = Result(new { frameId = frame.Id, frame.Generation, frame.ReceivedAtUtc,
            frameAgeMs = Stopwatch.GetElapsedTime(frame.Timestamp).TotalMilliseconds, afterActionId,
            lastActionId = last?.Id, superseded = action is not null && last?.Id != action.Id,
            width = 800, height = 600, freshness = "host-observed; instrument capture time unknown",
            visualReviewRequired = true });
        result.Content.Add(ImageContentBlock.FromBytes(frame.Png, "image/png"));
        return result;
    }

    /// <summary>Provides short and long presses without persistent held state across MCP calls.</summary>
    [McpServerTool(Name = "scope_press"), Description("Left press at x=0..799,y=0..599. durationMs defaults to 100 for a short press; use 1000 for long press (10..10000). Returns after release, without waiting for video. Capture scope_screen(afterActionId) to verify.")]
    public async Task<CallToolResult> Press(int x, int y, int durationMs = 100, string? frameId = null,
        CancellationToken cancellationToken = default)
        => await GuardAsync(async () => ActionResult(await controller.PressAsync(x, y, durationMs, frameId, cancellationToken)));

    /// <summary>Moves a held left button between display coordinates; this is not hover movement.</summary>
    [McpServerTool(Name = "scope_drag"), Description("Drag with the left button held across the 800x600 display, then release. durationMs 10..10000, default 500. Hover is unsupported. Returns an action receipt immediately after the gesture; verify with a delayed screen.")]
    public async Task<CallToolResult> Drag(int fromX, int fromY, int toX, int toY, int durationMs = 500,
        string? frameId = null, CancellationToken cancellationToken = default)
        => await GuardAsync(async () => ActionResult(await controller.DragAsync(fromX, fromY, toX, toY, durationMs, frameId, cancellationToken)));

    /// <summary>Lists verified presets separately from unavailable candidates, including required screen context.</summary>
    [McpServerTool(Name = "scope_actions"), Description("Discover firmware-specific named actions. Only verified entries are executable. Check requiredView against a current screen; choose GUI fallback explicitly when USB configuration reports needsGui.")]
    public object Actions() => new
    {
        available = ScopeController.Presets.Where(p => p.Verified).Select(p => new
        {
            p.Name, p.Description, p.RequiredView, p.Transport, p.Arguments
        }),
        unavailable = ScopeController.Presets.Where(p => !p.Verified).Select(p => new { p.Name, reason = p.Description }),
        automaticGuiFallback = false
    };

    /// <summary>Runs a verified named operation through its preferred transport.</summary>
    [McpServerTool(Name = "scope_action"), Description("Execute one verified preset from scope_actions. USB is preferred where verified. GUI presets require the listed screen context; the agent checks it visually. No automatic replay/fallback. Returns action ID for delayed observation.")]
    public async Task<CallToolResult> Action(string name, string? argument = null, string? frameId = null,
        CancellationToken cancellationToken = default)
        => await GuardAsync(async () => ActionResult(await controller.PresetAsync(name, argument, frameId, cancellationToken)));

    /// <summary>Applies verified USB settings and exposes per-field GUI requirements without silent fallback.</summary>
    [McpServerTool(Name = "scope_configure"), Description("Configure requested settings using verified USB mappings only. Unverified fields return needsGui and suggestedAction; the agent inspects the screen and chooses GUI controls. runState run/stop uses this instrument's verified reversed commands. An uncertain write is never retried.")]
    public async Task<CallToolResult> Configure(ScopeSettings settings, CancellationToken cancellationToken)
        => await GuardAsync(async () => ActionResult(await controller.ConfigureAsync(settings, cancellationToken)));

    /// <summary>Lists MTP storage roots or one folder without modifying files or resetting USB.</summary>
    [McpServerTool(Name = "scope_files"), Description("Read-only MTP listing. Omit parentRef to list storages, then use a returned storage/folder reference. References expire when this MTP device session changes. Paged offset/count; no USB reset recovery.")]
    public async Task<object> Files(string? parentRef = null, int offset = 0, int count = 100,
        CancellationToken cancellationToken = default) => await GuardAsync(() => mtp.ListAsync(parentRef, offset, count, cancellationToken));

    /// <summary>Downloads one completed file into quota-limited temporary storage.</summary>
    [McpServerTool(Name = "scope_file_get"), Description("Retrieve one MTP file by objectRef. Stream to temporary storage, validate byte count, and return download ID, SHA256 and server-local path. Wait for scope save completion first. Data scaling is unverified. Temporary files are removed at server shutdown.")]
    public Task<DownloadedFile> FileGet(string objectRef, CancellationToken cancellationToken) => GuardAsync(() => files.FetchAsync(objectRef, cancellationToken));

    /// <summary>Reads a bounded chunk so HTTP clients can access downloads without filesystem access.</summary>
    [McpServerTool(Name = "scope_file_read"), Description("Read a temporary download by byte offset in bounded text/base64 chunks. Default 65536 bytes, maximum 1048576. Base64 is lossless for binary files or split UTF-8 characters.")]
    public Task<object> FileRead(string downloadId, long offset = 0, int maxBytes = 65536, string format = "base64",
        CancellationToken cancellationToken = default) => GuardAsync(() => files.ReadAsync(downloadId, offset, maxBytes, format, cancellationToken));

    /// <summary>Explicitly retains a selected file beyond this MCP process.</summary>
    [McpServerTool(Name = "scope_file_save"), Description("Persist a selected temporary download to captureDirectory under a plain filename. Existing files are never overwritten. This saves locally, not onto the scope.")]
    public Task<object> FileSave(string downloadId, string name, CancellationToken cancellationToken) => GuardAsync(() => files.SaveAsync(downloadId, name, cancellationToken));

    /// <summary>Preserves actionable device/validation errors while leaving programming failures sanitized by MCP.</summary>
    private static async Task<T> GuardAsync<T>(Func<Task<T>> operation)
    {
        try { return await operation(); }
        catch (Exception error) when (error is IOException or ArgumentException or TimeoutException or UnauthorizedAccessException)
        {
            throw new McpException(error.Message);
        }
    }

    /// <summary>Returns receipts even for uncertain writes, retaining their action IDs for observation.</summary>
    private static CallToolResult ActionResult(ActionExecution result) => Result(new
    {
        actionId = result.Action.Id, result.Action.Sequence, result.Action.StartedAtUtc, result.Action.CompletedAtUtc,
        result.Action.Operation, result.Action.Transport, result.Action.Outcome, result.Action.Error, result.Result,
        nextObservation = "scope_screen(afterActionId: actionId); allow video latency and inspect the intended state"
    }, result.Action.Error is not null);

    /// <summary>Formats structured tool metadata as JSON text alongside optional native image content.</summary>
    private static CallToolResult Result(object value, bool error = false) => new()
    {
        IsError = error, Content = [new TextContentBlock { Text = JsonSerializer.Serialize(value, Json) }]
    };
}
