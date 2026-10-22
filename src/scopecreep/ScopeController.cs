using System.Diagnostics;
using System.ComponentModel;
using System.Globalization;
using System.Text.Json.Serialization;

/// <summary>Tracks an attempted mutation even when its effect is uncertain.</summary>
internal sealed record ScopeAction(string Id, long Sequence, DateTimeOffset StartedAtUtc, DateTimeOffset? CompletedAtUtc,
    [property: JsonIgnore] long? Timestamp,
    string Operation, string Transport, string Outcome, string? Error);

/// <summary>Returns a mutation receipt and any per-setting results without claiming transmission proves application.</summary>
internal sealed record ActionExecution(ScopeAction Action, object? Result);

/// <summary>Distinguishes applied settings from guided GUI requirements and uncertain writes.</summary>
internal sealed record ConfigurationReport(List<object> Settings, bool Uncertain, bool AnyApplied, bool NeedsGui,
    bool VisualReviewRequired);

/// <summary>Defines one firmware-specific preset and the screen context the agent must check.</summary>
internal sealed record ScopePreset(string Name, string Description, string RequiredView, string Transport,
    bool Verified, string? Command = null, int X = 0, int Y = 0, int? ToX = null, int? ToY = null, int DurationMs = 100,
    string[]? Arguments = null);

/// <summary>Optional settings form one validated configuration request; unsupported fields report GUI requirements.</summary>
internal sealed class ScopeSettings
{
    [Description("run or stop; uses this instrument's verified reversed USB mapping")]
    public string? RunState { get; set; }
    public bool? Single { get; set; }
    public ChannelSettings[]? Channels { get; set; }
    public TimebaseSettings? Timebase { get; set; }
    public TriggerSettings? Trigger { get; set; }
    public AcquisitionSettings? Acquisition { get; set; }
}

/// <summary>Physical channel settings use CH1..CH4 and explicit volts/probe factors.</summary>
internal sealed class ChannelSettings
{
    [Description("CH1, CH2, CH3, or CH4; only verified channel settings are sent over USB")]
    public string Channel { get; set; } = "";
    public bool? Enabled { get; set; }
    public double? ProbeFactor { get; set; }
    public string? Coupling { get; set; }
    public double? VoltsPerDiv { get; set; }
    public double? OffsetVolts { get; set; }
}

/// <summary>Requested horizontal settings use seconds.</summary>
internal sealed class TimebaseSettings
{
    [Description("Seconds per division, 50e-9..1000. Verified setter; its getter is broken, so visually check a delayed screen.")]
    public double? SecondsPerDiv { get; set; }
    public double? PositionSeconds { get; set; }
}

/// <summary>Requested edge-trigger settings; effective units require firmware verification.</summary>
internal sealed class TriggerSettings
{
    public string? Source { get; set; }
    public string? Slope { get; set; }
    public double? LevelVolts { get; set; }
    public string? Mode { get; set; }
}

/// <summary>Requested acquisition mode and record length.</summary>
internal sealed class AcquisitionSettings
{
    public string? Mode { get; set; }
    public int? Depth { get; set; }
}

/// <summary>Serializes USB mutations and complete GUI gestures; observations never reserve the instrument.</summary>
internal sealed class ScopeController(ScopeDevice device, RemoteDisplay remote)
{
    private readonly SemaphoreSlim mutation = new(1);
    private readonly object journalLock = new();
    private readonly Dictionary<string, ScopeAction> journal = new();
    private long sequence;
    private ScopeAction? lastAction;
    // Enable only after screen-assisted verification on the attached firmware.
    private static readonly HashSet<string> VerifiedSettings = ["runState", "CH1.enabled", "secondsPerDiv"];
    public static string[] VerifiedUsbSettings => VerifiedSettings.Order().ToArray();
    public static readonly ScopePreset[] Presets =
    [
        new("acquisition.run", "Start acquisition using the verified reversed command mapping", "any scope view", "usb", true, ":MENU:STOP"),
        new("acquisition.stop", "Stop acquisition using the verified reversed command mapping", "any scope view", "usb", true, ":MENU:RUN"),
        new("main_menu.open", "Pull down the scope main menu", "scope display; no modal dialog", "gui", true, X: 400, Y: 5, ToX: 400, ToY: 240, DurationMs: 500),
        new("main_menu.close", "Dismiss the Sample/Save/Trigger main menu", "main menu open", "gui", true, X: 650, Y: 90, ToX: 650, ToY: 5, DurationMs: 500),
        new("capture_menu.open", "Select acquisition sampling settings", "main menu open", "gui", true, X: 223, Y: 58),
        new("capture_menu.close", "Dismiss acquisition sampling settings", "Sample main menu open", "gui", true, X: 650, Y: 90, ToX: 650, ToY: 5, DurationMs: 500),
        new("capture.mode", "Select a verified sampling mode", "Sample main menu open", "gui", true, Arguments: ["normal", "peak"]),
        new("capture.depth", "Record depth selection is not mapped on this firmware", "inspect the scope UI", "gui", false),
        new("channel.select", "Tap a channel button; may also open its settings", "scope display; no modal dialog", "gui", true, Arguments: ["CH1", "CH2"]),
        new("channel_menu.open", "Long-press a channel button to open its settings", "scope display; no modal dialog", "gui", true, DurationMs: 1000, Arguments: ["CH1", "CH2"]),
        new("channel_menu.close", "Dismiss the channel settings panel", "channel settings panel open", "gui", true, X: 400, Y: 450),
        new("timebase_menu.open", "Open the horizontal scale grid", "scope display; no modal dialog", "gui", true, X: 381, Y: 568),
        new("timebase_menu.close", "Dismiss the horizontal scale grid", "horizontal scale grid open", "gui", true, X: 400, Y: 100),
        new("timebase.value", "Horizontal grid selection still needs reliable hardware verification; prefer USB secondsPerDiv", "horizontal scale grid open", "gui", false, Arguments: ["1ms", "500us"]),
        new("trigger_menu.open", "Select trigger settings", "main menu open", "gui", true, X: 400, Y: 58),
        new("trigger_menu.close", "Dismiss trigger settings", "Trigger main menu open", "gui", true, X: 650, Y: 90, ToX: 650, ToY: 5, DurationMs: 500),
        new("save_menu.open", "Select waveform saving", "main menu open", "gui", true, X: 133, Y: 58),
        new("save_menu.close", "Dismiss waveform saving", "Save main menu open", "gui", true, X: 351, Y: 252, ToX: 351, ToY: 35, DurationMs: 500),
        new("save.source", "Choose an enabled waveform source", "Save main menu open with CH1 and CH2 enabled", "gui", true, Arguments: ["CH1", "CH2"]),
        new("save.format", "Choose waveform file format", "Save main menu open", "gui", true, Arguments: ["CSV", "BIN", "WAV"]),
        new("save.location", "Choose local scope storage", "Save main menu open", "gui", true, X: 100, Y: 160, Arguments: ["local"]),
        new("save.execute", "Save with the scope-generated filename; never confirm an overwrite", "Save menu configured; inspect filename first", "gui", true, X: 654, Y: 160)
    ];

    /// <summary>Returns the most recent receipt without issuing device commands.</summary>
    public ScopeAction? LastAction { get { lock (journalLock) return lastAction; } }

    /// <summary>Looks up a recent action for delayed screenshots; expired and cross-process IDs fail explicitly.</summary>
    public ScopeAction GetAction(string id)
    {
        lock (journalLock)
        {
            if (!journal.TryGetValue(id, out var action))
                throw new ArgumentException("Unknown or expired action ID; action IDs last for this MCP process");
            if (action.Timestamp is null) throw new ArgumentException("Action is still in progress; wait for its receipt");
            return action;
        }
    }

    /// <summary>Validates the display coordinate space before any input reaches the socket.</summary>
    internal static void ValidatePoint(int x, int y)
    {
        if (x is < 0 or >= 800 || y is < 0 or >= 600)
            throw new ArgumentException("Coordinates must be within the 800x600 display");
    }

    /// <summary>Bounds deliberate holds so cancellation and cleanup remain predictable.</summary>
    internal static void ValidateDuration(int durationMs)
    {
        if (durationMs is < 10 or > 10000) throw new ArgumentException("durationMs must be 10..10000");
    }

    /// <summary>Executes a short/long left press, returning immediately after its release.</summary>
    public Task<ActionExecution> PressAsync(int x, int y, int durationMs, string? frameId, CancellationToken token)
    {
        ValidatePoint(x, y); ValidateDuration(durationMs);
        return MutateAsync("press", "gui", async ct =>
        {
            await GestureAsync(x, y, x, y, durationMs, false, frameId, ct);
            return new { x, y, durationMs, visualReviewRequired = true };
        }, token);
    }

    /// <summary>Executes one complete interpolated left drag under the shared mutation gate.</summary>
    public Task<ActionExecution> DragAsync(int fromX, int fromY, int toX, int toY, int durationMs,
        string? frameId, CancellationToken token)
    {
        ValidatePoint(fromX, fromY); ValidatePoint(toX, toY); ValidateDuration(durationMs);
        return MutateAsync("drag", "gui", async ct =>
        {
            await GestureAsync(fromX, fromY, toX, toY, durationMs, true, frameId, ct);
            return new { fromX, fromY, toX, toY, durationMs, visualReviewRequired = true };
        }, token);
    }

    /// <summary>Forwards one raw command once, recording uncertainty instead of hiding a possibly applied write.</summary>
    public Task<ActionExecution> CommandAsync(string command, CancellationToken token)
    {
        ScpiProtocol.Encode(command, false);
        return MutateAsync("scpi_command", "usb", async ct =>
        {
            await device.ExecuteAsync(command, false, 1, ct);
            return new { transmitted = true, applied = (bool?)null, visualReviewRequired = true };
        }, token);
    }

    /// <summary>Runs only hardware-verified presets; unavailable catalog entries never send guessed clicks.</summary>
    public Task<ActionExecution> PresetAsync(string name, string? argument, string? frameId, CancellationToken token)
    {
        var preset = Presets.SingleOrDefault(p => p.Name == name) ?? throw new ArgumentException("Unknown preset");
        if (!preset.Verified) throw new ArgumentException("Preset is not hardware-verified; use screen plus explicit press/drag");
        preset = ResolvePreset(preset, argument);
        return MutateAsync(name, preset.Transport, async ct =>
        {
            if (preset.Command is not null)
            {
                await RequireProfileAsync(ct);
                await device.ExecuteAsync(preset.Command, false, 1, ct);
                var status = ScpiProtocol.Text(await device.ExecuteAsync(":TRIGGER:STATUS?", true, 4096, ct));
                var expected = name == "acquisition.run" ? "RUN" : "STOP";
                if (status != expected) throw new IOException($"Acquisition readback {status}; expected {expected}");
                return new { verifiedBy = "usb-readback", acquisitionStatus = status };
            }
            await GestureAsync(preset.X, preset.Y, preset.ToX ?? preset.X, preset.ToY ?? preset.Y,
                preset.DurationMs, preset.ToX is not null, frameId, ct);
            return (object)new { visualReviewRequired = true, preset.RequiredView };
        }, token);
    }

    /// <summary>Resolves only verified argument choices into fixed screen coordinates before taking the mutation gate.</summary>
    internal static ScopePreset ResolvePreset(ScopePreset preset, string? argument)
    {
        if (preset.Arguments is null)
        {
            if (argument is not null) throw new ArgumentException("This preset takes no argument");
            return preset;
        }
        if (argument is null || !preset.Arguments.Contains(argument, StringComparer.Ordinal))
            throw new ArgumentException("Preset argument must be one of: " + string.Join(", ", preset.Arguments));
        return preset.Name switch
        {
            "channel.select" or "channel_menu.open" => preset with { X = 730, Y = argument == "CH1" ? 96 : 190 },
            "capture.mode" => preset with { X = argument == "normal" ? 136 : 347, Y = 109 },
            "save.source" => preset with { X = argument == "CH1" ? 23 : 98, Y = 105 },
            "save.format" => preset with { X = argument switch { "CSV" => 348, "BIN" => 409, _ => 288 }, Y = 160 },
            "save.location" => preset,
            "timebase.value" => preset with { X = argument == "1ms" ? 39 : 116, Y = 523 },
            _ => throw new InvalidOperationException("Preset argument mapping missing")
        };
    }

    /// <summary>Validates all requested fields, then applies verified USB mappings without GUI fallback.</summary>
    public Task<ActionExecution> ConfigureAsync(ScopeSettings settings, CancellationToken token)
    {
        var requests = ValidateSettings(settings);
        return MutateAsync("configure", "usb", async ct =>
        {
            var results = new List<object>();
            var stopped = false;
            var anyApplied = false;
            var needsGui = false;
            var visualReview = false;
            foreach (var request in requests)
            {
                if (!VerifiedSettings.Contains(request.Key))
                {
                    needsGui = true;
                    results.Add(new { setting = request.Key, status = "needsGui", requested = request.Value,
                        suggestedAction = request.Preset, reason = "No verified USB mapping; agent must inspect the screen and choose GUI control" });
                    continue;
                }
                if (stopped)
                {
                    results.Add(new { setting = request.Key, status = "notAttempted", requested = request.Value });
                    continue;
                }
                try
                {
                    await RequireProfileAsync(ct);
                    var applied = await ApplySettingAsync(request, ct);
                    results.Add(new { setting = request.Key, status = applied.Matches switch { true => "verified", false => "coerced", null => "transmitted" },
                        requested = request.Value, effective = applied.Effective, readback = applied.Readback });
                    anyApplied |= applied.Matches is not null;
                    visualReview |= applied.Matches is null;
                }
                catch (Exception error) when (error is not OperationCanceledException)
                {
                    results.Add(new { setting = request.Key, status = "unknown", requested = request.Value, error = error.Message });
                    stopped = true;
                }
            }
            return new ConfigurationReport(results, stopped, anyApplied, needsGui, visualReview);
        }, token);
    }

    /// <summary>Flattens and validates configuration before any mutation; units are never inferred from broken queries.</summary>
    internal static List<SettingRequest> ValidateSettings(ScopeSettings settings)
    {
        ArgumentNullException.ThrowIfNull(settings);
        var requests = new List<SettingRequest>();
        foreach (var channel in settings.Channels ?? [])
        {
            if (channel is null) throw new ArgumentException("Channel entries cannot be null");
            ValidateChannel(channel.Channel);
            var prefix = channel.Channel + ".";
            if (channel.ProbeFactor is { } probe) { Positive(probe); requests.Add(new(prefix + "probeFactor", probe, "channel_menu.open")); }
            if (channel.Coupling is { } coupling) { Choice(coupling, "DC", "AC", "GND"); requests.Add(new(prefix + "coupling", coupling, "channel_menu.open")); }
            if (channel.VoltsPerDiv is { } scale) { Positive(scale); requests.Add(new(prefix + "voltsPerDiv", scale, "channel_menu.open")); }
            if (channel.OffsetVolts is { } offset) { Finite(offset); requests.Add(new(prefix + "offsetVolts", offset, "channel_menu.open")); }
            if (channel.Enabled is { } enabled) requests.Add(new(prefix + "enabled", enabled, "channel.select"));
        }
        if (settings.Channels is { } channels && channels.Select(c => c.Channel).Distinct().Count() != channels.Length)
            throw new ArgumentException("Channel entries must be unique");
        if (settings.Timebase?.SecondsPerDiv is { } timebase)
        {
            Positive(timebase);
            if (timebase is < 50e-9 or > 1000) throw new ArgumentException("secondsPerDiv must be 50 ns..1000 s on this profile");
            requests.Add(new("secondsPerDiv", timebase, "timebase_menu.open"));
        }
        if (settings.Timebase?.PositionSeconds is { } position) { Finite(position); requests.Add(new("positionSeconds", position, "timebase_menu.open")); }
        if (settings.Trigger is { } trigger)
        {
            if (trigger.Source is { } source) { ValidateChannel(source); requests.Add(new("trigger.source", source, "trigger_menu.open")); }
            if (trigger.Slope is { } slope) { Choice(slope, "rising", "falling"); requests.Add(new("trigger.slope", slope, "trigger_menu.open")); }
            if (trigger.Mode is { } mode) { Choice(mode, "auto", "normal", "single"); requests.Add(new("trigger.mode", mode, "trigger_menu.open")); }
            if (trigger.LevelVolts is { } level) { Finite(level); requests.Add(new("trigger.levelVolts", level, "trigger_menu.open")); }
        }
        if (settings.Acquisition?.Mode is { } acquisition)
        {
            Choice(acquisition, "normal", "peak", "average", "envelope");
            requests.Add(new("acquisition.mode", acquisition, "capture_menu.open"));
        }
        if (settings.Acquisition?.Depth is { } depth)
        {
            if (depth < 1) throw new ArgumentException("Acquisition depth must be positive");
            requests.Add(new("acquisition.depth", depth, "capture.depth"));
        }
        if (settings.Single == true) requests.Add(new("single", true, "trigger_menu.open"));
        if (settings.RunState is { } run) { Choice(run, "run", "stop"); requests.Add(new("runState", run, "acquisition." + run)); }
        if (requests.Count == 0) throw new ArgumentException("No settings supplied");
        return requests;
    }

    /// <summary>Restricts channel identifiers to the instrument's one-based public names.</summary>
    private static void ValidateChannel(string channel) => Choice(channel, "CH1", "CH2", "CH3", "CH4");
    /// <summary>Rejects values outside a closed set.</summary>
    private static void Choice(string value, params string[] options)
    {
        if (!options.Contains(value, StringComparer.Ordinal)) throw new ArgumentException($"Expected one of: {string.Join(", ", options)}");
    }
    /// <summary>Rejects non-finite physical requests.</summary>
    private static void Finite(double value)
    {
        if (!double.IsFinite(value)) throw new ArgumentException("Numeric settings must be finite");
    }
    /// <summary>Rejects non-positive scales and factors.</summary>
    private static void Positive(double value)
    {
        Finite(value);
        if (value <= 0) throw new ArgumentException("Scales and probe factors must be positive");
    }

    /// <summary>Protects the reversed firmware mapping from accidental use on another scope.</summary>
    private async Task RequireProfileAsync(CancellationToken token)
    {
        var id = ScpiProtocol.Text(await device.ExecuteAsync("*IDN?", true, 4096, token));
        if (id != "Micsig,STO1104C,16298,5")
            throw new IOException($"No verified control profile for {id}; use explicit diagnostics");
    }

    /// <summary>Applies only mappings admitted by the verified profile, preserving effective values and coercion.</summary>
    private async Task<SettingApplication> ApplySettingAsync(SettingRequest request, CancellationToken token)
    {
        if (request.Key == "runState")
        {
            var run = (string)request.Value;
            await device.ExecuteAsync(run == "run" ? ":MENU:STOP" : ":MENU:RUN", false, 1, token);
            var status = ScpiProtocol.Text(await device.ExecuteAsync(":TRIGGER:STATUS?", true, 4096, token));
            if (status != run.ToUpperInvariant()) throw new IOException($"Readback {status}; expected {run}");
            return new(run, status, true);
        }
        if (request.Key == "secondsPerDiv")
        {
            // The setter was verified against the screen; its getter always reports 0.0 on this firmware.
            var seconds = (double)request.Value;
            await device.ExecuteAsync(":TIMebase:EXTent " + seconds.ToString("F12", CultureInfo.InvariantCulture), false, 1, token);
            return new(null, null, null);
        }
        var parts = request.Key.Split('.');
        if (parts.Length != 2 || parts[0] is not ("CH1" or "CH2" or "CH3" or "CH4"))
            throw new InvalidOperationException("Verified profile has no setting implementation");
        var prefix = ":CHANNEL" + parts[0][2];
        if (parts[1] == "enabled")
        {
            var enabled = (bool)request.Value;
            await device.ExecuteAsync(prefix + ":DISPLAY " + (enabled ? "1" : "0"), false, 1, token);
            var readback = ScpiProtocol.Text(await device.ExecuteAsync(prefix + ":DISPLAY?", true, 4096, token));
            var actual = readback.Split('|')[0] switch
            {
                "1" => true, "0" => false,
                _ => throw new IOException("Unrecognized channel display readback: " + readback)
            };
            return new(actual, readback, enabled == actual);
        }
        throw new InvalidOperationException("Verified profile has no setting implementation");
    }

    /// <summary>Releases a held button even after cancellation; failed cleanup is retried only as release on reconnect.</summary>
    private async Task GestureAsync(int x, int y, int toX, int toY, int durationMs, bool drag,
        string? frameId, CancellationToken token)
    {
        var session = remote.BeginInput(frameId);
        var startX = x;
        var startY = y;
        remote.RequireRelease(x, y, session);
        try
        {
            await remote.SendAsync(0x101, x, y, session, token);
            var clock = Stopwatch.StartNew();
            if (drag)
            {
                var steps = Math.Max(1, (durationMs + 99) / 100);
                for (var step = 1; step <= steps; step++)
                {
                    var delay = (int)((long)durationMs * step / steps - clock.ElapsedMilliseconds);
                    if (delay > 0) await Task.Delay(delay, token);
                    var fraction = (double)step / steps;
                    var pointX = (int)Math.Round(startX + (toX - startX) * fraction);
                    var pointY = (int)Math.Round(startY + (toY - startY) * fraction);
                    remote.RequireRelease(pointX, pointY, session);
                    await remote.SendAsync(0x101, pointX, pointY, session, token);
                    x = pointX; y = pointY;
                }
            }
            else await Task.Delay(durationMs, token);
        }
        finally
        {
            await remote.SendAsync(1, x, y, session, CancellationToken.None);
        }
    }

    /// <summary>Runs one serialized mutation and retains a bounded journal of completed receipts.</summary>
    private async Task<ActionExecution> MutateAsync(string operation, string transport,
        Func<CancellationToken, Task<object>> execute, CancellationToken token)
    {
        await mutation.WaitAsync(token);
        try
        {
            ScopeAction started;
            lock (journalLock)
            {
                started = new(Guid.NewGuid().ToString("N"), ++sequence, DateTimeOffset.UtcNow, null, null,
                    operation, transport, "inProgress", null);
                journal.Add(started.Id, started);
                if (journal.Count > 256) journal.Remove(journal.Keys.First());
                lastAction = started;
            }
            object? result = null;
            string? error = null;
            try { result = await execute(token); }
            catch (Exception failure) { error = failure.Message; }
            if (result is ConfigurationReport { Uncertain: true }) error = "A configuration write or readback is uncertain; inspect the per-setting results";
            var outcome = error is null ? "transmitted" : "unknown";
            if (error is null && result is ConfigurationReport report)
            {
                if (report.NeedsGui) outcome = report.AnyApplied || report.VisualReviewRequired ? "partial" : "needsGui";
                else if (!report.VisualReviewRequired) outcome = "verified";
            }
            lock (journalLock)
            {
                var action = started with { CompletedAtUtc = DateTimeOffset.UtcNow,
                    Timestamp = Stopwatch.GetTimestamp(), Outcome = outcome, Error = error };
                journal[action.Id] = action;
                lastAction = action;
                return new(action, result);
            }
        }
        finally { mutation.Release(); }
    }
}

/// <summary>One validated configuration field and its guided GUI alternative.</summary>
internal sealed record SettingRequest(string Key, object Value, string Preset);

/// <summary>Effective state from a trustworthy USB readback, distinct from the requested state.</summary>
internal sealed record SettingApplication(object? Effective, string? Readback, bool? Matches);
