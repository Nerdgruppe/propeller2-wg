//! Minimal desktop shell for Windtunnel, using zig-gamedev's GLFW/OpenGL ImGui backend.
const std = @import("std");
const builtin = @import("builtin");
const glfw = @import("zglfw");
const zopengl = @import("zopengl");
const zgui = @import("zgui");
const gl = zopengl.bindings;
const windtunnel = @import("windtunnel");
const args_parser = @import("args");
const Oscillator = windtunnel.ClockConfiguration.Oscillator;

const CliArgs = struct {
    help: bool = false,
    image: ?[]const u8 = null,
    sim: windtunnel.SimulationProfile = .ideal,
    rcfast: ?Oscillator = null,
    rcslow: ?Oscillator = null,
    crystal: ?Oscillator = null,
    seed: ?u64 = null,

    pub const shorthands = .{ .h = "help" };

    pub const meta = .{
        .usage_summary = "[--help] [--image <file>] [--sim ideal|realistic] [--rcfast CLOCK] [--rcslow CLOCK] [--crystal CLOCK] [--seed N]",
        .full_text = "Windtunnel's graphical interface for the Parallax Propeller 2 simulator.",
        .option_docs = .{
            .help = "Prints this help text",
            .image = "Loads the initial hub image from a file",
            .sim = "Typical ideal values or realistic bounded clocks and empirical PLL profiles",
            .rcfast = "RCFAST in MHz: 24-4+6 or 25.23+3.2ppm (default follows --sim)",
            .rcslow = "RCSLOW in MHz or with Hz/kHz/MHz; ranges accept ppm, ppb, % and absolute units",
            .crystal = "XI clock with the same syntax (default 20 MHz, unspecified tolerance is fixed)",
            .seed = "Clock drift RNG seed; omitted uses entropy, selected seed is printed",
        },
    };
};

/// Open the desktop window and render interactive controls until the user closes it.
pub fn main(init: std.process.Init) !u8 {
    var cli = args_parser.parseForCurrentProcess(CliArgs, init, .print) catch return 1;
    defer cli.deinit();

    if (cli.options.help or cli.positionals.len != 0) {
        var buffer: [256]u8 = undefined;
        const file = if (cli.options.help) std.Io.File.stdout() else std.Io.File.stderr();
        var output = file.writer(init.io, &buffer);
        try args_parser.printHelp(CliArgs, cli.executable_name orelse "windtunnel-gui", &output.interface);
        try output.interface.flush();
        return if (cli.options.help) 0 else 1;
    }

    var gui: GUI = .{ .hub = undefined, .debugger = .{ .allocator = init.gpa } };
    defer gui.debugger.deinit();
    defer {
        if (gui.initial_image) |image| {
            init.gpa.free(image);
        }
    }

    var host_io = init.io;
    gui.configuration_draft = switch (cli.options.sim) {
        .ideal => .ideal,
        .realistic => .realistic,
    };
    windtunnel.configureClocks(init, &gui.configuration_draft, .{
        .rcfast = cli.options.rcfast,
        .rcslow = cli.options.rcslow,
        .crystal = cli.options.crystal,
        .seed = cli.options.seed,
    });
    try gui.hub.init(gui.configuration_draft);
    gui.hub.debugger = &gui.debugger;

    if (cli.options.image) |path| {
        const file = try std.Io.Dir.cwd().openFile(init.io, path, .{});
        defer file.close(init.io);

        const stat = try file.stat(init.io);
        const count = try file.readPositionalAll(init.io, &gui.hub.memory, 0);
        if (stat.size > count) {
            std.log.warn("hub image exceeds hub size. expected {Bi:.3} or less bytes, but got {Bi:.3}", .{
                gui.hub.memory.len,
                stat.size,
            });
        } else {
            std.log.info("loaded {Bi:.2} into hub memory", .{count});
        }

        gui.initial_image = try init.gpa.dupe(u8, gui.hub.memory[0..count]);
        try gui.hub.start_cog(0, .{});
        gui.cogs_visible[0] = true;
    }

    try glfw.init();
    defer glfw.terminate();

    glfw.windowHint(.context_version_major, 3);
    glfw.windowHint(.context_version_minor, 3);
    glfw.windowHint(.opengl_profile, .opengl_core_profile);
    glfw.windowHint(.opengl_forward_compat, true);

    const window = try glfw.createWindow(960, 640, "Windtunnel", null, null);
    defer window.destroy();

    glfw.makeContextCurrent(window);
    glfw.swapInterval(1);
    try zopengl.loadCoreProfile(glfw.getProcAddress, 3, 3);

    zgui.init(init.gpa);
    defer zgui.deinit();

    zgui.io.setIniFilename(null);
    zgui.backend.initWithGlSlVersion(window, "#version 330 core");
    defer zgui.backend.deinit();

    while (!window.shouldClose()) {
        glfw.pollEvents();
        if (window.shouldClose())
            break;

        const size = window.getFramebufferSize();
        if (size[0] == 0 or size[1] == 0) {
            glfw.waitEvents();
            continue;
        }

        zgui.backend.newFrame(@intCast(size[0]), @intCast(size[1]));

        const window_size = window.getSize();
        gui.logWindowSize("Windtunnel", .{ @floatFromInt(window_size[0]), @floatFromInt(window_size[1]) });
        gui.controls();
        if (gui.exit_requested) {
            window.setShouldClose(true);
            break;
        }
        try gui.toolbar();
        gui.advance(&host_io);
        try gui.inspectors();

        gl.viewport(0, 0, size[0], size[1]);
        gl.clearColor(0.08, 0.09, 0.12, 1.0);
        gl.clear(gl.COLOR_BUFFER_BIT);
        zgui.backend.draw();
        window.swapBuffers();
    }
    return 0;
}

const Debugger = windtunnel.Debugger;
const inspect = windtunnel.inspect;

const WindowSize = struct {
    title_hash: u64,
    size: [2]f32,
};

const PinDrive = struct {
    driver: windtunnel.Pad.Driver,
};

const LocalViewOptions = struct {
    show_tail_ram: bool = false,
    show_disassembly: bool = true,
    show_execute_word: bool = false,
};

const GUI = struct {
    hub: windtunnel.Hub,
    debugger: Debugger,
    initial_image: ?[]const u8 = null,
    configuration_draft: windtunnel.SystemConfiguration = .ideal,
    configuration_error: ?anyerror = null,
    is_running: bool = false,
    step_requested: bool = false,
    frame: windtunnel.Frame = .{},
    // Main window, six shared inspectors, eight cogs and sixty-four pins.
    window_sizes: [1 + 6 + 8 + 64]?WindowSize = @splat(null),
    hide_automatic_ui: bool = false,
    exit_requested: bool = false,
    breakpoints_visible: bool = false,
    configuration_visible: bool = false,
    hub_visible: bool = true,
    io_visible: bool = true,
    memory_visible: bool = false,
    cordic_visible: bool = false,
    cogs_visible: [8]bool = @splat(false),
    pins_visible: [64]bool = @splat(false),
    jump: u32 = 0,
    jump_pending: bool = false,
    start_address: [8]u32 = @splat(0),
    start_ptra: [8]u32 = @splat(0),
    local_view_options: [8]LocalViewOptions = @splat(.{}),
    hubset: u32 = 0,
    seed: u32 = 0,
    sensor_config: [8][4]u32 = @splat(@splat(0)),
    pattern_mask: [8]u32 = @splat(0),
    pattern_value: [8]u32 = @splat(0),
    selected_cell: usize = 0,
    selected_pin: u6 = 0,
    watch: Debugger.Watchpoint = .{
        .space = .hub,
        .cog = null,
        .start = 0,
        .end = 16,
        .access = .both,
    },
    watch_cog: u32 = 0,
    watch_issuer: u32 = 0,
    filter_issuer: bool = false,
    execution: Debugger.Execution = .{ .cog = 0, .domain = .hub, .pc = 0 },
    execution_cog: u32 = 0,
    execution_pc: u32 = 0,

    /// Mark the entire generic inspector block with a magenta background.
    fn show(gui: *GUI, label: [:0]const u8, value: anytype) void {
        if (gui.hide_automatic_ui)
            return;

        zgui.pushStrId(label);
        defer zgui.popId();

        const magenta = [4]f32{ 1, 0, 1, 1 };
        zgui.pushStyleColor4f(.{ .idx = .header, .c = magenta });
        zgui.pushStyleColor4f(.{ .idx = .header_hovered, .c = magenta });
        zgui.pushStyleColor4f(.{ .idx = .header_active, .c = magenta });
        defer zgui.popStyleColor(.{ .count = 3 });

        if (!zgui.beginTable("##generic", .{ .column = 1, .flags = .{ .sizing = .stretch_same } }))
            return;
        defer zgui.endTable();

        zgui.tableNextRow(.{});
        _ = zgui.tableSetColumnIndex(0);
        zgui.tableSetBgColor(.{ .target = .row_bg0, .color = 0xffff00ff });
        showValue(label, value);
    }

    /// Open a closable inspector and report changes to its size in the host terminal.
    fn newWindow(gui: *GUI, title: [:0]const u8, visible: ?*bool) bool {
        zgui.setNextWindowSize(.{ .w = 700, .h = 540, .cond = .first_use_ever });
        if (!zgui.begin(title, .{ .popen = visible })) {
            zgui.end();
            return false;
        }
        gui.logWindowSize(title, zgui.getWindowSize());
        return true;
    }

    /// Remember initial dimensions silently, then log each change without retaining temporary title buffers.
    fn logWindowSize(gui: *GUI, title: []const u8, size: [2]f32) void {
        const title_hash = std.hash.Wyhash.hash(0, title);
        for (&gui.window_sizes) |*entry| {
            if (entry.*) |*previous| {
                if (previous.title_hash != title_hash)
                    continue;
                if (!std.meta.eql(previous.size, size)) {
                    std.log.info("Window \"{s}\": {d:.0} x {d:.0}", .{ title, size[0], size[1] });
                    previous.size = size;
                }
                return;
            }
            entry.* = .{ .title_hash = title_hash, .size = size };
            return;
        }
    }

    /// Collect application actions and window toggles in the main menu before advancing simulation.
    fn controls(gui: *GUI) void {
        if (!zgui.beginMainMenuBar())
            return;
        defer zgui.endMainMenuBar();

        if (zgui.beginMenu("File", true)) {
            defer zgui.endMenu();

            _ = zgui.menuItem("Load image (coming soon)", .{ .enabled = false });
            _ = zgui.menuItem("Export VCD (coming soon)", .{ .enabled = false });
            zgui.separator();
            if (zgui.menuItem("Exit", .{})) {
                gui.exit_requested = true;
            }
        }
        if (zgui.beginMenu("Windows", true)) {
            defer zgui.endMenu();

            _ = zgui.menuItemPtr("Hub", .{ .selected = &gui.hub_visible });
            _ = zgui.menuItemPtr("I/O", .{ .selected = &gui.io_visible });
            _ = zgui.menuItemPtr("Memory", .{ .selected = &gui.memory_visible });
            _ = zgui.menuItemPtr("CORDIC", .{ .selected = &gui.cordic_visible });
            _ = zgui.menuItemPtr("Breakpoints", .{ .selected = &gui.breakpoints_visible });
            if (zgui.beginMenu("Cogs", true)) {
                defer zgui.endMenu();

                for (&gui.cogs_visible, 0..) |*visible, i| {
                    var buffer: [32]u8 = undefined;
                    const title = std.fmt.bufPrintSentinel(&buffer, "Cog {}", .{i}, 0) catch unreachable;
                    _ = zgui.menuItemPtr(title, .{ .selected = visible });
                }
            }
            if (zgui.beginMenu("Pins", true)) {
                defer zgui.endMenu();

                // Eight groups of eight minimize the largest menu: max(64 / 8, 8) = 8.
                for (0..8) |group| {
                    const first = group * 8;
                    var buffer: [32]u8 = undefined;
                    const title = std.fmt.bufPrintSentinel(&buffer, "Pins {}-{}", .{ first, first + 7 }, 0) catch unreachable;
                    if (zgui.beginMenu(title, true)) {
                        defer zgui.endMenu();

                        for (first..first + 8) |i| {
                            var pin_buffer: [32]u8 = undefined;
                            const pin_title = std.fmt.bufPrintSentinel(&pin_buffer, "Pin {}", .{i}, 0) catch unreachable;
                            _ = zgui.menuItemPtr(pin_title, .{ .selected = &gui.pins_visible[i] });
                        }
                    }
                }
            }
        }
        if (zgui.beginMenu("Options", true)) {
            defer zgui.endMenu();

            _ = zgui.menuItemPtr("System Configuration", .{ .selected = &gui.configuration_visible });
            _ = zgui.menuItemPtr("Hide automatic UI", .{ .selected = &gui.hide_automatic_ui });
        }
    }

    /// Keep execution actions in a fixed toolbar below the application menu.
    fn toolbar(gui: *GUI) !void {
        const viewport = zgui.getMainViewport();
        const pos = viewport.getPos();
        const size = viewport.getSize();
        const height = zgui.getFrameHeight() + 2 * zgui.getStyle().window_padding[1];
        zgui.setNextWindowPos(.{ .x = pos[0], .y = pos[1] + zgui.getFrameHeight() });
        zgui.setNextWindowSize(.{ .w = size[0], .h = height });
        const visible = zgui.begin("##Execution toolbar", .{ .flags = .{
            .no_title_bar = true,
            .no_resize = true,
            .no_move = true,
            .no_collapse = true,
            .no_scrollbar = true,
            .no_saved_settings = true,
            .no_focus_on_appearing = true,
        } });
        defer zgui.end();

        if (!visible)
            return;

        if (zgui.button("Reboot", .{})) {
            try gui.initializeChip(gui.hub.system_configuration);
        }
        zgui.sameLine(.{});
        if (zgui.button("Reset", .{})) {
            gui.configuration_draft = gui.hub.system_configuration;
            try gui.reinitialize();
        }
        zgui.sameLine(.{});
        if (zgui.button(if (gui.is_running) "Stop" else "Run", .{})) {
            gui.is_running = !gui.is_running;
            if (gui.is_running) {
                gui.debugger.acknowledge(&gui.hub);
            }
        }
        zgui.sameLine(.{});
        if (zgui.button("Step", .{})) {
            gui.debugger.acknowledge(&gui.hub);
            gui.is_running = false;
            gui.step_requested = true;
        }
    }

    /// Edit host breakpoints/watchpoints and inspect the matches responsible for the current stop.
    fn breakpointsWindow(gui: *GUI) !void {
        if (!gui.newWindow("Breakpoints", &gui.breakpoints_visible))
            return;
        defer zgui.end();

        if (header("Execution breakpoints", .{ .default_open = true })) {
            _ = zgui.inputScalar("Cog##execution", u32, .{ .v = &gui.execution_cog });
            gui.execution.cog = @intCast(@min(gui.execution_cog, 7));
            if (zgui.button(@tagName(gui.execution.domain), .{})) {
                gui.execution.domain = switch (gui.execution.domain) {
                    .cog => .lut,
                    .lut => .hub,
                    .hub => .cog,
                };
            }
            _ = hexInput("PC (local includes 0x200 LUT base)", &gui.execution_pc, false);
            gui.execution.pc = @truncate(gui.execution_pc);
            if (zgui.button("Toggle breakpoint", .{})) {
                try gui.debugger.toggleExecution(gui.execution);
            }
            var i: usize = 0;
            while (i < gui.debugger.executions.items.len) {
                const item = gui.debugger.executions.items[i];
                zgui.pushIntId(@intCast(i));
                zgui.text("Cog {} {t} 0x{X:0>5}", .{ item.cog, item.domain, item.pc });
                zgui.sameLine(.{});
                const remove = zgui.button("Remove", .{});
                zgui.popId();
                if (remove) {
                    _ = gui.debugger.executions.orderedRemove(i);
                } else {
                    i += 1;
                }
            }
        }
        if (header("Data watchpoints", .{ .default_open = true })) {
            try gui.watchEditor();
        }
        zgui.separator();
        zgui.text("Stop: {t}", .{gui.debugger.stop});
        for (gui.debugger.encounters.items) |encounter| {
            zgui.text("Cog {} {t} PC 0x{X:0>5} before clock {}", .{
                encounter.cog, encounter.domain, encounter.pc, gui.hub.counter,
            });
        }
        if (gui.hub.fault) |fault| {
            gui.show("Fault", fault);
        }
        if (header("Data stop matches", .{})) {
            for (gui.debugger.hits.items, 0..) |hit, i| {
                zgui.pushIntId(@intCast(i));
                defer zgui.popId();
                zgui.text("Clock {} {t}/{t} {t} 0x{X:0>8} = 0x{X:0>8} ({t})", .{
                    hit.clock, hit.space, hit.storage, hit.access, hit.address, hit.value, hit.class,
                });
                gui.show("Provenance", hit.provenance);
            }
        }
    }

    /// Show completed-frame execution state in a fixed, single-row bottom status bar.
    fn status(gui: *GUI) void {
        const viewport = zgui.getMainViewport();
        const pos = viewport.getPos();
        const size = viewport.getSize();
        const height = zgui.getFrameHeight();
        zgui.setNextWindowPos(.{ .x = pos[0], .y = pos[1] + size[1] - height });
        zgui.setNextWindowSize(.{ .w = size[0], .h = height });
        const visible = zgui.begin("##Execution status", .{ .flags = .{
            .no_title_bar = true,
            .no_resize = true,
            .no_move = true,
            .no_scrollbar = true,
            .no_saved_settings = true,
            .no_focus_on_appearing = true,
            .menu_bar = true,
        } });
        defer zgui.end();

        if (visible and zgui.beginMenuBar()) {
            defer zgui.endMenuBar();

            zgui.text("{s} | stop: {t} | {} clocks | {d:.3} ms simulation/frame", .{
                if (gui.is_running) "Running" else "Paused",
                gui.debugger.stop,
                gui.frame.clocks,
                @as(f64, @floatFromInt(gui.frame.nanoseconds)) / 1_000_000.0,
            });
            if (gui.hub.fault != null) {
                zgui.textColored(.{ 1, 0.3, 0.25, 1 }, "Fault", .{});
            }
            if (zgui.menuItem("Stop details", .{})) {
                gui.breakpoints_visible = true;
            }
        }
    }

    /// Edit a half-open local-long or backing-RAM-byte watch range and its read scope.
    fn watchEditor(gui: *GUI) !void {
        if (zgui.button(@tagName(gui.watch.space), .{})) {
            gui.watch.space = switch (gui.watch.space) {
                .hub => .registers,
                .registers => .lut,
                .lut => .hub,
            };
        }
        _ = zgui.inputScalar("Local cog", u32, .{ .v = &gui.watch_cog });
        gui.watch.cog = if (gui.watch.space == .hub) null else @intCast(@min(gui.watch_cog, 7));
        _ = hexInput("Start (inclusive)", &gui.watch.start, false);
        _ = hexInput("End (exclusive)", &gui.watch.end, false);
        if (zgui.button(@tagName(gui.watch.access), .{})) {
            gui.watch.access = switch (gui.watch.access) {
                .read => .write,
                .write => .both,
                .both => .read,
            };
        }
        var physical = gui.watch.scope == .physical;
        _ = zgui.checkbox("All physical accesses (otherwise consumed data only)", .{ .v = &physical });
        gui.watch.scope = if (physical) .physical else .data;
        _ = zgui.checkbox("Filter issuing cog", .{ .v = &gui.filter_issuer });
        if (gui.filter_issuer) {
            _ = zgui.inputScalar("Issuer", u32, .{ .v = &gui.watch_issuer });
        }
        gui.watch.issuer = if (gui.filter_issuer) @intCast(@min(gui.watch_issuer, 7)) else null;
        const limit: u32 = if (gui.watch.space == .hub) 0x80000 else 512;
        zgui.beginDisabled(.{ .disabled = gui.watch.start >= gui.watch.end or gui.watch.end > limit });
        if (zgui.button("Add watchpoint", .{})) {
            try gui.debugger.watches.append(gui.debugger.allocator, gui.watch);
        }
        zgui.endDisabled();
        var i: usize = 0;
        while (i < gui.debugger.watches.items.len) {
            const item = gui.debugger.watches.items[i];
            zgui.pushIntId(@intCast(i));
            zgui.text("{t} 0x{X:0>8}..0x{X:0>8} {t} {t}", .{ item.space, item.start, item.end, item.access, item.scope });
            gui.show("Filters", .{ .cog = item.cog, .issuer = item.issuer });
            const remove = zgui.button("Remove watch", .{});
            zgui.popId();
            if (remove) {
                _ = gui.debugger.watches.orderedRemove(i);
            } else {
                i += 1;
            }
        }
    }

    /// Advance on this thread using the monotonic host clock, with at most fourteen milliseconds per frame.
    fn advance(gui: *GUI, io: *std.Io) void {
        gui.frame = windtunnel.Frame.advance(&gui.hub, if (gui.step_requested) .step else if (gui.is_running) .run else .paused, .{
            .context = io,
            .now = monotonic,
        });
        if (gui.frame.notification == .reset) {
            gui.debugger.acknowledge(&gui.hub);
            gui.hub.debugger = &gui.debugger;
        }
        gui.is_running = gui.frame.running;
        gui.step_requested = false;
    }

    /// Render independent inspectors without advancing simulator state.
    fn inspectors(gui: *GUI) !void {
        if (gui.hub_visible) {
            gui.hubWindow();
        }
        if (gui.io_visible) {
            gui.ioWindow();
        }
        if (gui.memory_visible) {
            try gui.memoryWindow();
        }
        if (gui.cordic_visible) {
            gui.cordicWindow();
        }
        for (gui.cogs_visible, 0..) |visible, id| {
            if (visible) {
                try gui.cogWindow(@intCast(id));
            }
        }
        for (gui.pins_visible, 0..) |visible, id| {
            if (visible) {
                gui.pinWindow(@intCast(id));
            }
        }
        if (gui.breakpoints_visible) {
            try gui.breakpointsWindow();
        }
        if (gui.configuration_visible) {
            gui.systemConfiguration();
        }
        gui.status();
    }

    /// Inspect shared resources and edit architectural hub settings only while paused.
    fn hubWindow(gui: *GUI) void {
        zgui.setNextWindowSize(.{ .w = 320, .h = 340, .cond = .first_use_ever });
        const visible = zgui.begin("Hub", .{ .popen = &gui.hub_visible });
        defer zgui.end();

        if (!visible)
            return;

        gui.logWindowSize("Hub", zgui.getWindowSize());
        const hub = &gui.hub;
        zgui.text("CT {} | clock 0x{X:0>8} | {d:.3} Hz", .{
            hub.counter,
            hub.io.clock_mode,
            hub.io.clock.frequency(),
        });
        zgui.text("Clock seed {} | {t} | acquiring {} | unsafe P-up {}", .{ hub.io.clock.configuration.seed, hub.io.clock.coverage, hub.io.clock.acquiring(), hub.io.clock.unsafe_postdivider_up });
        if (header("Random generator", .{})) {
            if (zgui.beginTable("Random generator", .{
                .column = 2,
                .flags = .{ .borders = .all, .row_bg = true, .sizing = .fixed_fit },
            })) {
                defer zgui.endTable();

                zgui.tableSetupColumn("Register", .{});
                zgui.tableSetupColumn("Value", .{});
                zgui.tableHeadersRow();
                const labels = [_][:0]const u8{ "S0", "S1", "Multiply5", "Output" };
                const values = [_]u64{ hub.random.s0, hub.random.s1, hub.random.multiply5, hub.random.output };
                for (labels, values) |label, value| {
                    zgui.tableNextRow(.{});
                    _ = zgui.tableSetColumnIndex(0);
                    zgui.textUnformatted(label);
                    _ = zgui.tableSetColumnIndex(1);
                    zgui.text("0x{X:0>16}", .{value});
                }
            }
        }
        gui.show("Debug configuration", hub.debug);
        gui.show("Pending starts", hub.pending_starts);
        gui.show("Fault", hub.fault);
        zgui.beginDisabled(.{ .disabled = gui.is_running });
        defer zgui.endDisabled();
        _ = hexInput("HUBSET clock/debug", &gui.hubset, false);
        if (zgui.button("Apply HUBSET", .{})) {
            hub.configure(gui.hubset);
            if (hub.reset_requested) {
                gui.step_requested = true;
            }
        }
        zgui.text("Crystal {:.3} Hz; edit clocks in System Configuration and reinitialize", .{hub.io.clock.crystal.specification.frequency});
        _ = hexInput("Random seed", &gui.seed, false);
        if (zgui.button("Seed random", .{})) {
            hub.configure(gui.seed | 0x80000000);
        }
        if (header("Locks", .{ .default_open = true })) {
            if (zgui.beginTable("Locks", .{ .column = 4, .flags = .{ .borders = .all, .row_bg = true } })) {
                defer zgui.endTable();

                for ([_][:0]const u8{ "ID", "Allocated", "Taken", "Owner" }) |label| {
                    zgui.tableSetupColumn(label, .{});
                }
                zgui.tableHeadersRow();
                for (&hub.locks) |*lock| {
                    zgui.pushIntId(lock.id);
                    defer zgui.popId();

                    zgui.tableNextRow(.{});
                    _ = zgui.tableSetColumnIndex(0);
                    zgui.text("{}", .{lock.id});
                    _ = zgui.tableSetColumnIndex(1);
                    _ = zgui.checkbox("##allocated", .{ .v = &lock.allocated });
                    _ = zgui.tableSetColumnIndex(2);
                    var taken = lock.taken;
                    if (zgui.checkbox("##taken", .{ .v = &taken })) {
                        lock.taken = taken;
                        hub.signal_lock(lock.id, taken);
                    }
                    _ = zgui.tableSetColumnIndex(3);
                    var owner: i32 = lock.owner;
                    zgui.setNextItemWidth(-1);
                    if (zgui.dragInt("##owner", .{
                        .v = &owner,
                        .min = 0,
                        .max = 7,
                        .flags = .{ .always_clamp = true },
                    })) {
                        lock.owner = @intCast(std.math.clamp(owner, 0, 7));
                    }
                }
            }
        }
    }

    /// Reset the chip with validated settings and pause while retaining host debugger definitions.
    fn initializeChip(gui: *GUI, configuration: windtunnel.SystemConfiguration) !void {
        try gui.hub.init(configuration);
        gui.debugger.acknowledge(&gui.hub);
        gui.hub.debugger = &gui.debugger;
        gui.is_running = false;
        gui.step_requested = false;
        gui.jump_pending = false;
        gui.frame = .{};
        gui.configuration_error = null;
    }

    /// Apply draft settings and restore the original image, starting cog zero when an image was loaded.
    fn reinitialize(gui: *GUI) !void {
        try gui.initializeChip(gui.configuration_draft);
        if (gui.initial_image) |image| {
            @memcpy(gui.hub.memory[0..image.len], image);
            try gui.hub.start_cog(0, .{});
            gui.cogs_visible[0] = true;
        }
    }

    /// Edit a host-only draft; the active chip changes only when explicitly reinitialized.
    fn systemConfiguration(gui: *GUI) void {
        if (!gui.newWindow("System Configuration", &gui.configuration_visible))
            return;
        defer zgui.end();

        gui.show("Active system configuration (SI units)", gui.hub.system_configuration);
        zgui.beginDisabled(.{ .disabled = gui.is_running });
        defer zgui.endDisabled();

        const draft = &gui.configuration_draft;
        zgui.text("Active: {s} | Draft: {s}", .{ gui.hub.system_configuration.name(), draft.name() });
        if (zgui.button("Ideal", .{})) {
            draft.* = .ideal;
            gui.configuration_error = null;
        }
        zgui.sameLine(.{});
        if (zgui.button("Realistic", .{})) {
            draft.* = .realistic;
            gui.configuration_error = null;
        }
        zgui.text("Active analog oversampling: {d}x", .{gui.hub.system_configuration.oversample});
        _ = zgui.inputScalar("Analog samples per clock (>= 1)", u32, .{ .v = &draft.oversample });
        zgui.textUnformatted("Analog substeps only; cogs and smart pins keep the system clock.");
        if (header("Clock sources", .{})) {
            _ = zgui.inputScalar("Drift RNG seed", u64, .{ .v = &draft.clocks.seed });
            const clock_behavior = if (draft.clocks.isIdeal())
                "ideal"
            else
                "measured profiles";
            zgui.text("PLL behavior: {s}", .{clock_behavior});
            inline for (.{ "rcfast", "rcslow", "crystal" }) |name| {
                const spec = &@field(draft.clocks, name);
                quantityInput(name ++ " nominal (MHz)", &spec.frequency, 1e-6);
                quantityInput(name ++ " minus (MHz)", &spec.below, 1e-6);
                quantityInput(name ++ " plus (MHz)", &spec.above, 1e-6);
            }
        }
        quantityInput("VIO (V)", &draft.vio_voltage, 1);
        quantityInput("Digital threshold / VIO", &draft.threshold_ratio, 1);
        quantityInput("Schmitt low / VIO", &draft.schmitt_low_ratio, 1);
        quantityInput("Schmitt high / VIO", &draft.schmitt_high_ratio, 1);
        quantityInput("Receiver delay (ns)", &draft.receiver_delay, 1e9);
        quantityInput("Comparator delay (ns)", &draft.comparator_delay, 1e9);
        quantityInput("Comparator reference settling (ns)", &draft.reference_settle_time, 1e9);
        quantityInput("DAC 990R range (ohm)", &draft.dac_resistance[0], 1);
        quantityInput("DAC 600R range (ohm)", &draft.dac_resistance[1], 1);
        quantityInput("DAC 124R range (ohm)", &draft.dac_resistance[2], 1);
        quantityInput("DAC 75R range (ohm)", &draft.dac_resistance[3], 1);
        quantityInput("ADC GIO density", &draft.adc_gio_density, 1);
        quantityInput("ADC VIO density", &draft.adc_vio_density, 1);
        quantityInput("ADC floating density", &draft.adc_float_density, 1);
        quantityInput("ADC gain center / VIO", &draft.adc_center_ratio, 1);
        quantityInput("Internal pad capacitance (pF)", &draft.pad_capacitance, 1e12);
        if (zgui.button(if (draft.fast_model == .linear) "Fast drive: linear" else "Fast drive: piecewise", .{})) {
            draft.fast_model = switch (draft.fast_model) {
                .linear => .piecewise,
                .piecewise => .linear,
            };
        }
        quantityInput("Linear fast low (ohm)", &draft.fast_low_resistance, 1);
        quantityInput("Linear fast high (ohm)", &draft.fast_high_resistance, 1);
        if (zgui.beginTable("Fast drive curve", .{ .column = 4, .flags = .{ .borders = .all } })) {
            zgui.tableNextRow(.{});
            for ([_][:0]const u8{ "Point", "Current (mA)", "Low drop (mV)", "High drop (mV)" }, 0..) |label, i| {
                _ = zgui.tableSetColumnIndex(@intCast(i));
                zgui.textUnformatted(label);
            }
            for (&draft.curve, 0..) |*point, i| {
                zgui.pushIntId(@intCast(i));
                zgui.tableNextRow(.{});
                _ = zgui.tableSetColumnIndex(0);
                zgui.text("{}", .{i + 1});
                _ = zgui.tableSetColumnIndex(1);
                quantityInput("##current", &point.current, 1e3);
                _ = zgui.tableSetColumnIndex(2);
                quantityInput("##low", &point.low_drop, 1e3);
                _ = zgui.tableSetColumnIndex(3);
                quantityInput("##high", &point.high_drop, 1e3);
                zgui.popId();
            }
            zgui.endTable();
        }
        quantityInput("1k5 drive (kohm)", &draft.resistor_1k5, 1e-3);
        quantityInput("15k drive (kohm)", &draft.resistor_15k, 1e-3);
        quantityInput("150k drive (kohm)", &draft.resistor_150k, 1e-3);
        quantityInput("Current-drive resistance (ohm)", &draft.current_drive_resistance, 1);
        quantityInput("1mA drive limit (mA)", &draft.current_1ma, 1e3);
        quantityInput("100uA drive limit (uA)", &draft.current_100ua, 1e6);
        quantityInput("10uA drive limit (uA)", &draft.current_10ua, 1e6);
        _ = zgui.checkbox("Leakage enabled", .{ .v = &draft.leakage_enabled });
        quantityInput("Leakage to ground (Mohm)", &draft.leakage_resistance, 1e-6);
        quantityInput("Minimum pad voltage (V)", &draft.min_pad_voltage, 1);
        quantityInput("Maximum pad voltage above VIO (V)", &draft.max_pad_voltage_margin, 1);
        quantityInput("Maximum pin current (mA)", &draft.max_pin_current, 1e3);
        zgui.textUnformatted("Reinitialize restores the original image and pauses.");
        zgui.textUnformatted("Runtime RAM edits and external stimuli are discarded.");
        if (zgui.button("Reinitialize with settings", .{})) {
            gui.reinitialize() catch |err| {
                gui.configuration_error = err;
            };
        }
        if (gui.configuration_error) |err| {
            zgui.textColored(.{ 1, 0.3, 0.25, 1 }, "Cannot initialize: {t}", .{err});
        }
    }

    /// Show physical pad levels in a compact matrix, with controls for the selected pin below it.
    fn ioWindow(gui: *GUI) void {
        zgui.setNextWindowSize(.{ .w = 260, .h = 290, .cond = .first_use_ever });
        const visible = zgui.begin("I/O", .{ .popen = &gui.io_visible });
        defer zgui.end();

        if (!visible)
            return;

        gui.logWindowSize("I/O", zgui.getWindowSize());
        const io = &gui.hub.io;
        if (zgui.beginTable("Pads", .{ .column = 8, .flags = .{ .borders = .all } })) {
            defer zgui.endTable();

            for (0..64) |i| {
                if (i % 8 == 0) {
                    zgui.tableNextRow(.{});
                }
                _ = zgui.tableSetColumnIndex(@intCast(i % 8));
                zgui.pushIntId(@intCast(i));
                defer zgui.popId();

                const color: [4]f32 = switch (io.pads[i]) {
                    .low => .{ 0.65, 0.08, 0.08, 1 },
                    .high => .{ 0.05, 0.55, 0.15, 1 },
                };
                const highlight: [4]f32 = switch (io.pads[i]) {
                    .low => .{ 0.95, 0.25, 0.25, 1 },
                    .high => .{ 0.1, 0.85, 0.25, 1 },
                };
                zgui.pushStyleColor4f(.{ .idx = .text, .c = .{ 1, 1, 1, 1 } });
                zgui.pushStyleColor4f(.{ .idx = .header, .c = highlight });
                zgui.pushStyleColor4f(.{ .idx = .header_hovered, .c = highlight });
                zgui.pushStyleColor4f(.{ .idx = .header_active, .c = highlight });
                defer zgui.popStyleColor(.{ .count = 4 });

                var buffer: [8]u8 = undefined;
                const label = std.fmt.bufPrintSentinel(&buffer, "{}", .{i}, 0) catch unreachable;
                const selected = gui.selected_pin == i;
                if (zgui.selectable(label, .{ .selected = selected, .h = zgui.getFrameHeight() })) {
                    gui.selected_pin = @intCast(i);
                }
                zgui.tableSetBgColor(.{ .target = .cell_bg, .color = zgui.colorConvertFloat4ToU32(color) });
            }
        }
        zgui.separator();
        zgui.text("Pin {}", .{gui.selected_pin});
        const level: []const u8 = switch (io.pads[gui.selected_pin]) {
            .low => "low",
            .high => "high",
        };
        if (io.getExternal(@enumFromInt(gui.selected_pin))) |state| {
            zgui.text("Voltage: {d:.2} V ({s})", .{ state.voltage, level });
        } else {
            zgui.text("Voltage: pending first clock ({s})", .{level});
        }
        zgui.textUnformatted("Drive:");
        const sources = [_]windtunnel.Pad.Source{ .low, .high, .float };
        const labels = [_][:0]const u8{ "L", "H", "Z" };
        for (sources, labels) |preset, label| {
            zgui.sameLine(.{});
            if (zgui.button(label, .{})) {
                io.setExternal(@enumFromInt(gui.selected_pin), preset) catch unreachable; // Validated presets.
            }
        }
        zgui.sameLine(.{});
        if (zgui.button("Inspect", .{})) {
            gui.pins_visible[gui.selected_pin] = true;
        }
    }

    /// Inspect backing RAM directly, clipping rows and never calling watched access paths.
    fn memoryWindow(gui: *GUI) !void {
        zgui.setNextWindowSize(.{ .w = 565, .h = 435, .cond = .first_use_ever });
        const visible = zgui.begin("Memory", .{ .popen = &gui.memory_visible, .flags = .{ .menu_bar = true } });
        defer zgui.end();

        if (!visible)
            return;

        gui.logWindowSize("Memory", zgui.getWindowSize());
        if (zgui.beginMenuBar()) {
            defer zgui.endMenuBar();

            if (zgui.beginMenu("Jump", true)) {
                defer zgui.endMenu();

                _ = hexInput("Byte address", &gui.jump, false);
                if (zgui.menuItem("Go", .{})) {
                    gui.jump = @min(gui.jump, 0x7ffff);
                    gui.jump_pending = true;
                }
            }
        }
        if (!zgui.beginTable("RAM", .{
            .column = 3,
            .outer_size = zgui.getContentRegionAvail(),
            .flags = .{
                .scroll_y = true,
                .scroll_x = true,
                .resizable = true,
                .sizing = .fixed_fit,
                .row_bg = true,
                .borders = .all,
            },
        }))
            return;
        defer zgui.endTable();
        zgui.tableSetupScrollFreeze(0, 1);
        zgui.tableSetupColumn("Address", .{
            .flags = .{ .width_fixed = true },
            .init_width_or_height = zgui.calcTextSize("0x7FFFF", .{})[0],
        });
        zgui.tableSetupColumn("Bytes", .{
            .flags = .{ .width_fixed = true },
            .init_width_or_height = zgui.calcTextSize("FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF", .{})[0],
        });
        zgui.tableSetupColumn("ASCII", .{
            .flags = .{ .width_fixed = true },
            .init_width_or_height = zgui.calcTextSize("WWWWWWWWWWWWWWWW", .{})[0],
        });
        zgui.tableHeadersRow();
        var clipper = zgui.ListClipper.init();
        clipper.begin(32768, null);
        defer clipper.end();
        if (gui.jump_pending) {
            const row: i32 = @intCast(gui.jump / 16);
            clipper.includeItemsByIndex(row, row + 1);
        }
        while (clipper.step()) {
            var row = clipper.DisplayStart;
            while (row < clipper.DisplayEnd) : (row += 1) {
                const address: usize = @as(usize, @intCast(row)) * 16;
                zgui.pushIntId(row);
                defer zgui.popId();
                zgui.tableNextRow(.{});
                _ = zgui.tableSetColumnIndex(0);
                var label: [32]u8 = undefined;
                const name = std.fmt.bufPrintSentinel(&label, "0x{X:0>5}", .{address}, 0) catch unreachable;
                if (zgui.selectable(name, .{ .selected = address >= gui.watch.start and address < gui.watch.end })) {
                    gui.watch.space = .hub;
                    gui.watch.start = @intCast(address);
                    gui.watch.end = @intCast(address + 16);
                }
                if (gui.jump_pending and address == gui.jump / 16 * 16) {
                    zgui.setScrollHereY(.{ .center_y_ratio = 0 });
                    gui.jump_pending = false;
                }
                _ = zgui.tableSetColumnIndex(1);
                var bytes: [47]u8 = undefined;
                var writer = std.Io.Writer.fixed(&bytes);
                var ascii: [16]u8 = undefined;
                for (gui.hub.memory[address..][0..16], 0..) |byte, i| {
                    if (i != 0) {
                        writer.writeByte(' ') catch unreachable;
                    }
                    writer.print("{X:0>2}", .{byte}) catch unreachable;
                    ascii[i] = if (byte >= 32 and byte < 127) byte else '.';
                }
                zgui.textUnformatted(writer.buffered());
                _ = zgui.tableSetColumnIndex(2);
                zgui.textUnformatted(&ascii);
            }
        }
    }

    /// Display the inferred arithmetic allocation and select any input, arithmetic or return cell.
    fn cordicWindow(gui: *GUI) void {
        if (!gui.newWindow("CORDIC", &gui.cordic_visible))
            return;
        defer zgui.end();
        zgui.textUnformatted("54 arithmetic cells: internal allocation is inferred, not published RTL.");
        const cordic = &gui.hub.cordic;
        const pending_returns = cordic.returns.pending();
        if (zgui.beginTable("Cells", .{
            .column = 6,
            .flags = .{ .borders = .all, .scroll_x = true },
        })) {
            defer zgui.endTable();

            for (0..58) |i| {
                zgui.pushIntId(@intCast(i));
                defer zgui.popId();

                if (i % 6 == 0) {
                    zgui.tableNextRow(.{});
                }
                _ = zgui.tableSetColumnIndex(@intCast(i % 6));
                var label: [96]u8 = undefined;
                const entry = if (i == 0) cordic.input else if (i <= 54) cordic.pipeline[i - 1] else null;
                const title = if (i == 0) "Input latch" else std.fmt.bufPrintSentinel(&label, "{s}{}", .{
                    if (i <= 54) "Arithmetic " else "Return ",
                    if (i <= 54) i - 1 else i - 55,
                }, 0) catch unreachable;
                if (zgui.selectable(title, .{ .selected = gui.selected_cell == i })) {
                    gui.selected_cell = i;
                }
                if (entry) |item| {
                    zgui.textColored(.{ 0.4, 1, 0.6, 1 }, "Cog {} {t}", .{ @intFromEnum(item.cog), item.operation });
                    zgui.textUnformatted(if (item.tracked) "Tracked" else "Abandoned");
                } else if (i >= 55 and pending_returns[i - 55] != null) {
                    const item = pending_returns[i - 55].?;
                    zgui.textColored(.{ 0.4, 1, 0.6, 1 }, "Cog {} {t}", .{ @intFromEnum(item.cog), item.operation });
                    zgui.textUnformatted(if (item.tracked) "Tracked" else "Abandoned");
                } else {
                    zgui.textUnformatted("Empty");
                }
            }
        }
        const selected = gui.selected_cell;
        if (selected == 0) {
            gui.show("Input latch", cordic.input);
        } else if (selected <= 54) {
            gui.show("Stage allocation", windtunnel.Cordic.stage_layout[selected - 1]);
            gui.show("Arithmetic payload", cordic.pipeline[selected - 1]);
        } else {
            gui.show("Return payload", pending_returns[selected - 55]);
        }
    }

    /// Summarize execution, primed augmentations and interrupt state without hiding editable flags.
    fn cogStatus(gui: *GUI, cog: *windtunnel.Cog) void {
        if (!zgui.beginTable("Status", .{ .column = 9, .flags = .{ .borders = .all, .sizing = .fixed_fit } }))
            return;
        defer zgui.endTable();

        for ([_][:0]const u8{ "Mode", "PC", "C", "Z", "AUGD", "AUGS", "INT1", "INT2", "INT3" }) |label| {
            zgui.tableSetupColumn(label, .{});
        }
        zgui.tableHeadersRow();
        zgui.tableNextRow(.{});
        _ = zgui.tableSetColumnIndex(0);
        const mode = switch (cog.exec_mode) {
            .stopped => "Halt",
            .hub => "hub exec",
            .cog => "cog exec",
        };
        zgui.textUnformatted(mode);
        _ = zgui.tableSetColumnIndex(1);
        zgui.text("0x{X:0>5}", .{cog.pc});
        zgui.beginDisabled(.{ .disabled = gui.is_running });
        _ = zgui.tableSetColumnIndex(2);
        _ = zgui.checkbox("##carry", .{ .v = &cog.c });
        _ = zgui.tableSetColumnIndex(3);
        _ = zgui.checkbox("##zero", .{ .v = &cog.z });
        zgui.endDisabled();
        _ = zgui.tableSetColumnIndex(4);
        if (cog.augd_pending) {
            zgui.text("0x{X:0>8}", .{cog.augd});
        } else {
            zgui.textUnformatted("-");
        }
        _ = zgui.tableSetColumnIndex(5);
        if (cog.augs_pending) {
            zgui.text("0x{X:0>8}", .{cog.augs});
        } else {
            zgui.textUnformatted("-");
        }
        for (cog.interrupts.levels.values, 0..) |level, i| {
            _ = zgui.tableSetColumnIndex(@intCast(6 + i));
            const state = if (level.state == .active)
                "ACT"
            else if (cog.interrupts.stalled or (level.source == null and level.state != .pending))
                "-"
            else
                "ENA";
            zgui.textUnformatted(state);
        }
    }

    /// Arrange latched events in architectural order, with decoded selectable-event sources on hover.
    fn cogEvents(gui: *GUI, cog: *windtunnel.Cog) void {
        if (!zgui.beginTable("Events", .{ .column = 4, .flags = .{ .borders = .all } }))
            return;
        defer zgui.endTable();

        for (std.enums.values(windtunnel.types.EventId), 0..) |event, i| {
            if (i % 4 == 0) {
                zgui.tableNextRow(.{});
            }
            _ = zgui.tableSetColumnIndex(@intCast(i % 4));
            var active = cog.events.contains(event);
            zgui.beginDisabled(.{ .disabled = gui.is_running });
            if (zgui.checkbox(@tagName(event), .{ .v = &active })) {
                if (active) {
                    cog.signal_event(event);
                } else {
                    cog.events.remove(event);
                }
            }
            zgui.endDisabled();
            switch (event) {
                .SE1, .SE2, .SE3, .SE4 => {
                    if (zgui.isItemHovered(.{ .for_tooltip = true, .allow_when_disabled = true }) and zgui.beginTooltip()) {
                        defer zgui.endTooltip();

                        const index = @intFromEnum(event) - @intFromEnum(windtunnel.types.EventId.SE1);
                        if (cog.selectable_events[index]) |selection| {
                            switch (selection) {
                                .pin => |pin| {
                                    const condition = switch (pin.mode) {
                                        .rising => "rising edge",
                                        .falling => "falling edge",
                                        .change => "either edge",
                                        .low => "low level",
                                        .high => "high level",
                                    };
                                    zgui.text("Pin {}: {s}", .{ @intFromEnum(pin.index), condition });
                                },
                                .lut_lock => |source| {
                                    if (source < 0x10) {
                                        const owner = if (source & 8 != 0)
                                            cog.id ^ 1
                                        else
                                            cog.id;
                                        const access = if (source & 4 != 0)
                                            "writes"
                                        else
                                            "reads";
                                        zgui.text("Cog {} {s} LUT address 0x{X:0>3}", .{
                                            owner, access, @as(u9, 0x1fc) | (source & 3),
                                        });
                                    } else {
                                        const condition = switch (@as(u2, @truncate(source >> 4))) {
                                            1 => "rising (taken)",
                                            2 => "falling (released)",
                                            3 => "any change",
                                            0 => unreachable,
                                        };
                                        zgui.text("Hub lock {}: {s}", .{ source & 0xf, condition });
                                    }
                                },
                            }
                        } else {
                            zgui.textUnformatted("No event selected");
                        }
                    }
                },
                else => {},
            }
        }
    }

    /// Group architectural editors and read-only pipeline/controller state for one cog.
    fn cogWindow(gui: *GUI, id: u3) !void {
        var buffer: [32]u8 = undefined;
        const title = std.fmt.bufPrintSentinel(&buffer, "Cog {}", .{id}, 0) catch unreachable;
        zgui.setNextWindowSize(.{ .w = 400, .h = 500, .cond = .first_use_ever });
        const visible = zgui.begin(title, .{ .popen = &gui.cogs_visible[id], .flags = .{ .menu_bar = true } });
        defer zgui.end();

        if (!visible)
            return;

        gui.logWindowSize(title, zgui.getWindowSize());
        const cog = &gui.hub.cogs[id];
        if (zgui.beginMenuBar()) {
            defer zgui.endMenuBar();

            if (zgui.beginMenu("Start", true)) {
                defer zgui.endMenu();

                _ = hexInput("Start address", &gui.start_address[id], false);
                _ = hexInput("PTRA", &gui.start_ptra[id], false);
                zgui.separator();
                for ([_]bool{ true, false }, [_][:0]const u8{ "Start", "Start without load" }) |load, label| {
                    if (zgui.menuItem(label, .{})) {
                        try gui.hub.start_cog(id, .{
                            .hub_address = gui.start_address[id],
                            .ptra = gui.start_ptra[id],
                            .load_image = load,
                        });
                    }
                }
            }
            const stoppable = cog.exec_mode != .stopped or gui.hub.pending_starts[id] != null;
            if (zgui.menuItem("Stop", .{ .enabled = stoppable })) {
                cog.reset();
            }
            if (zgui.beginMenu("Options", true)) {
                defer zgui.endMenu();

                const options = &gui.local_view_options[id];
                _ = zgui.menuItemPtr("Show Tail Ram", .{ .selected = &options.show_tail_ram });
                _ = zgui.menuItemPtr("Show disassembly", .{ .selected = &options.show_disassembly });
                _ = zgui.menuItemPtr("Show execute word", .{ .selected = &options.show_execute_word });
            }
        }
        gui.cogStatus(cog);
        zgui.textUnformatted("Raw edits retain captured operands and pending writes.");
        if (header("Registers", .{ .default_open = true })) {
            try gui.localTable(cog, .cog);
        }
        if (header("LUT", .{})) {
            try gui.localTable(cog, .lut);
        }
        if (header("Stack", .{})) {
            for (&cog.stack, 0..) |*value, i| {
                zgui.pushIntId(@intCast(i));
                defer zgui.popId();

                var label_buffer: [32]u8 = undefined;
                const label = switch (i) {
                    0 => "0: Top (PUSH / POP)",
                    1 => "1: Next after POP",
                    7 => "7: Bottom (discarded by PUSH)",
                    else => std.fmt.bufPrintSentinel(&label_buffer, "Slot {}", .{i}, 0) catch unreachable,
                };
                _ = hexInput(label, value, gui.is_running);
            }
        }
        if (header("Events", .{})) {
            gui.cogEvents(cog);
        }
        zgui.beginDisabled(.{ .disabled = gui.is_running });
        if (header("Architectural configuration / prefixes", .{})) {
            var xbyte_mode: u32 = @intFromEnum(cog.xbyte_mode);
            if (hexInput("XBYTE mode", &xbyte_mode, false)) {
                cog.xbyte_mode = @enumFromInt(@as(u9, @truncate(xbyte_mode)));
            }
            var xbyte_default: u32 = @intFromEnum(cog.xbyte_default_mode);
            if (hexInput("XBYTE default mode", &xbyte_default, false)) {
                cog.xbyte_default_mode = @enumFromInt(@as(u9, @truncate(xbyte_default)));
            }
            _ = hexInput("Q", &cog.q, false);
            _ = hexInput("AUGS", &cog.augs, false);
            _ = zgui.checkbox("AUGS pending", .{ .v = &cog.augs_pending });
            _ = hexInput("AUGD", &cog.augd, false);
            _ = zgui.checkbox("AUGD pending", .{ .v = &cog.augd_pending });
            _ = zgui.checkbox("SETQ pending", .{ .v = &cog.setq_pending });
            _ = zgui.checkbox("Q2", .{ .v = &cog.q2 });
            _ = zgui.checkbox("LUT sharing", .{ .v = &cog.lut_sharing });
            if (header("Sensors / interrupt sources", .{})) {
                for (0..4) |i| {
                    zgui.pushIntId(@intCast(i));
                    defer zgui.popId();
                    _ = hexInput("Sensor configuration", &gui.sensor_config[id][i], false);
                    if (zgui.button("Configure selectable", .{})) {
                        cog.configure_selectable(@enumFromInt(@as(u4, @intCast(i + 4))), gui.sensor_config[id][i]);
                    }
                    if (i < 3) {
                        if (zgui.button("Configure interrupt source", .{})) {
                            cog.configure_interrupt(@enumFromInt(i), gui.sensor_config[id][i]);
                        }
                        if (zgui.button("Configure CT target", .{})) {
                            cog.set_counter_target(@enumFromInt(@as(u4, @intCast(i + 1))), gui.sensor_config[id][i]);
                        }
                    }
                }
                _ = hexInput("Pattern mask", &gui.pattern_mask[id], false);
                _ = hexInput("Pattern value", &gui.pattern_value[id], false);
                if (zgui.button("Configure pattern (C selects INB, Z selects equal)", .{})) {
                    cog.configure_pattern(gui.pattern_mask[id], gui.pattern_value[id]);
                }
            }
            _ = hexInput("Debug control", &cog.debug.control, false);
            _ = hexInput("Debug IJMP0", &cog.debug.ijmp, false);
            _ = hexInput("Debug IRET0", &cog.debug.iret, false);
            var pivot: u32 = cog.pixel_pivot;
            if (hexInput("Pixel pivot", &pivot, false)) {
                cog.pixel_pivot = @truncate(pivot);
            }
            var mode: u32 = cog.pixel_mode;
            if (hexInput("Pixel mode", &mode, false)) {
                cog.pixel_mode = @truncate(mode);
            }
        }
        zgui.endDisabled();
        gui.show("Counter sensors", cog.ct_targets);
        gui.show("Pending events", cog.pending_events);
        gui.show("Selectable sensors", cog.selectable_events);
        gui.show("Pattern sensor", cog.pattern);
        gui.show("Interrupt controller", cog.interrupts);
        gui.show("Debug controller", cog.debug);
        gui.show("Repeat", cog.repeat);
        gui.show("Skip", cog.skip);
        gui.show("XBYTE mode", cog.xbyte_mode);
        gui.show("XBYTE default mode", cog.xbyte_default_mode);
        gui.show("XBYTE phase", cog.xbyte);
        gui.show("FIFO", cog.fifo_state);
        if (header("DAC outputs", .{ .default_open = true })) {
            for (cog.dacs, 0..) |value, index| {
                zgui.text("DAC{d}: {d} (0x{X:0>2})", .{ index, value, value });
            }
        }
        gui.show("Raw DAC inputs", cog.raw_dacs);
        gui.show("Color converter", cog.converter);
        gui.show("Streamer", cog.streamer);
        gui.show("Memory transfer", cog.memory_transfer);
        gui.show("Startup", cog.startup);
        gui.show("Read bus", cog.read_bus);
        gui.show("CORDIC result ports", cog.cordic_result);
        gui.show("RAM collision", cog.ram_collision);
        gui.show("LUT collision", cog.lut_collision);
        gui.show("Pending writeback", cog.writeback);
        gui.show("Pending hub write", cog.hub_writeback);
        gui.show("Wait deadline", cog.wait_until);
        gui.show("Retired instruction", cog.retired);
        if (header("Pipeline (fetch/read/latch/execute/store)", .{ .default_open = true })) {
            for (cog.pipeline, 0..) |slot, i| {
                zgui.pushIntId(@intCast(i));
                defer zgui.popId();
                if (slot) |state| {
                    zgui.text("Slot {}: PC 0x{X:0>5}, instruction 0x{X:0>8}", .{ i, state.pc, state.instr });
                    disassemblyText(state.instr);
                    gui.show("Captured state / timing", state);
                } else {
                    zgui.text("Slot {}: empty", .{i});
                }
            }
        }
    }

    /// Clip local rows, show references on hover, and retain direct data and breakpoint editing.
    fn localTable(gui: *GUI, cog: *windtunnel.Cog, domain: Debugger.Domain) !void {
        zgui.pushIntId(@intFromEnum(domain));
        defer zgui.popId();
        if (!zgui.beginTable("Local RAM", .{
            .column = 7,
            .outer_size = .{ 0, 330 },
            .flags = .{
                .scroll_y = true,
                .scroll_x = true,
                .sizing = .fixed_fit,
                .row_bg = true,
                .borders = .all,
                .resizable = true,
                .hideable = true,
            },
        }))
            return;
        defer zgui.endTable();
        zgui.tableSetupScrollFreeze(0, 1);
        zgui.tableSetupColumn("BP", .{
            .flags = .{ .width_fixed = true, .no_resize = true },
            .init_width_or_height = zgui.getFrameHeight(),
        });
        zgui.tableSetupColumn("Address", .{
            .flags = .{ .width_fixed = true },
            .init_width_or_height = zgui.calcTextSize("Address", .{})[0],
        });
        const role_sample = if (domain == .lut)
            "LUT RAM"
        else
            "IJMP1";
        zgui.tableSetupColumn("Role", .{
            .flags = .{ .width_fixed = true },
            .init_width_or_height = zgui.calcTextSize(role_sample, .{})[0],
        });
        const word_width = zgui.calcTextSize("0x00000000", .{})[0] + 2 * zgui.getStyle().frame_padding[0];
        zgui.tableSetupColumn("Hex data", .{ .flags = .{ .width_fixed = true }, .init_width_or_height = word_width });
        const options = gui.local_view_options[cog.id];
        zgui.tableSetupColumn("Tail RAM", .{
            .flags = .{ .width_fixed = true, .default_hide = !options.show_tail_ram },
            .init_width_or_height = word_width,
        });
        zgui.tableSetupColumn("Execute word", .{
            .flags = .{ .width_fixed = true, .default_hide = !options.show_execute_word },
            .init_width_or_height = zgui.calcTextSize("Execute word", .{})[0],
        });
        zgui.tableSetupColumn("Disassembly", .{
            .flags = .{ .width_stretch = true, .default_hide = !options.show_disassembly },
        });
        zgui.tableSetColumnEnabled(4, options.show_tail_ram);
        zgui.tableSetColumnEnabled(5, options.show_execute_word);
        zgui.tableSetColumnEnabled(6, options.show_disassembly);
        zgui.tableHeadersRow();
        var clipper = zgui.ListClipper.init();
        clipper.begin(512, null);
        defer clipper.end();
        while (clipper.step()) {
            var row = clipper.DisplayStart;
            while (row < clipper.DisplayEnd) : (row += 1) {
                const index: u9 = @intCast(row);
                const reg: windtunnel.types.Register = @enumFromInt(index);
                const address: inspect.Address = .{ .domain = domain, .address = index };
                const badges = inspect.annotations(cog, address);
                zgui.pushIntId(row);
                defer zgui.popId();
                zgui.tableNextRow(.{});
                if (badges.any()) {
                    zgui.tableSetBgColor(.{ .target = .row_bg0, .color = 0x553aa7e8 });
                }
                _ = zgui.tableSetColumnIndex(0);
                const execution: Debugger.Execution = .{
                    .cog = cog.id,
                    .domain = domain,
                    .pc = @as(u20, index) + @as(u20, if (domain == .lut) 0x200 else 0),
                };
                var breakpoint = gui.debugger.hasExecution(execution);
                if (zgui.checkbox("##execution-breakpoint", .{ .v = &breakpoint })) {
                    try gui.debugger.toggleExecution(execution);
                }
                _ = zgui.tableSetColumnIndex(1);
                var label_buffer: [16]u8 = undefined;
                const label = std.fmt.bufPrintSentinel(&label_buffer, "0x{X:0>3}", .{index}, 0) catch unreachable;
                _ = zgui.selectable(label, .{
                    .h = zgui.getFrameHeight(),
                    .flags = .{ .span_all_columns = true, .allow_overlap = true },
                });
                if (badges.any() and zgui.isItemHovered(.{
                    .for_tooltip = true,
                    .allow_when_overlapped_by_item = true,
                }) and zgui.beginTooltip()) {
                    defer zgui.endTooltip();

                    var buffer: [512]u8 = undefined;
                    var writer = std.Io.Writer.fixed(&buffer);
                    try badges.write(&writer);
                    zgui.pushTextWrapPos(zgui.getFontSize() * 40);
                    defer zgui.popTextWrapPos();

                    zgui.textUnformatted(writer.buffered());
                }
                _ = zgui.tableSetColumnIndex(2);
                if (domain == .cog and index >= 0x1f0) {
                    zgui.text("{t}", .{reg});
                } else {
                    zgui.textUnformatted(if (domain == .lut) "LUT RAM" else "GP");
                }
                _ = zgui.tableSetColumnIndex(3);
                var value = if (domain == .cog) inspect.register(cog, index) else cog.lut[index];
                zgui.setNextItemWidth(zgui.getContentRegionAvail()[0]);
                if (hexInput("##data", &value, gui.is_running or (domain == .cog and index >= 0x1fe and !badges.debug_overlay))) {
                    if (domain == .cog) {
                        cog.write_reg(reg, value);
                    } else {
                        cog.lut[index] = value;
                    }
                }
                if (options.show_tail_ram and zgui.tableSetColumnIndex(4) and domain == .cog and index >= 504) {
                    zgui.setNextItemWidth(zgui.getContentRegionAvail()[0]);
                    _ = hexInput("##tail", &cog.ram_tail[index - 504], gui.is_running);
                }
                const word = if (domain == .cog and index >= 504) windtunnel.Cog.debug_rom[index - 504] else value;
                if (options.show_execute_word and zgui.tableSetColumnIndex(5)) {
                    zgui.text("0x{X:0>8}", .{word});
                }
                if (options.show_disassembly and zgui.tableSetColumnIndex(6)) {
                    disassemblyText(word);
                }
            }
        }
    }

    /// Decode the cached driver that produced the last electrical snapshot, including delayed output selection.
    fn pinDrive(gui: *const GUI, id: u6) PinDrive {
        return .{ .driver = gui.hub.io.resolvedDriver(
            @enumFromInt(id),
            &gui.hub.system_configuration,
        ) };
    }

    /// Summarize the physical pad, then compare external and internal electrical sources side by side.
    fn pinPad(gui: *GUI, id: u6) void {
        const io = &gui.hub.io;
        const snapshot = io.getExternal(@enumFromInt(id));
        const drive = gui.pinDrive(id);
        if (zgui.beginTable("Pad status", .{ .column = 7, .flags = .{ .borders = .all, .sizing = .fixed_fit } })) {
            defer zgui.endTable();

            for ([_][:0]const u8{ "Voltage", "Current", "Level", "Drive", "UV", "OV", "OC" }) |label| {
                zgui.tableSetupColumn(label, .{});
            }
            zgui.tableHeadersRow();
            zgui.tableNextRow(.{});
            _ = zgui.tableSetColumnIndex(0);
            if (snapshot) |state| {
                zgui.text("{d:.2} V", .{state.voltage});
            } else {
                zgui.textUnformatted("-");
            }
            _ = zgui.tableSetColumnIndex(1);
            if (snapshot) |state| {
                zgui.text("{d:.3} mA", .{state.pin_current * 1e3});
            } else {
                zgui.textUnformatted("-");
            }
            _ = zgui.tableSetColumnIndex(2);
            const level = switch (io.pads[id]) {
                .low => "low",
                .high => "high",
            };
            zgui.textUnformatted(level);
            _ = zgui.tableSetColumnIndex(3);
            pinDriveText(drive);
            if (snapshot) |state| {
                const violations = state.violations;
                for ([_]bool{ violations.undervoltage, violations.overvoltage, violations.overcurrent }, 0..) |flag, i| {
                    _ = zgui.tableSetColumnIndex(@intCast(4 + i));
                    const color: [4]f32 = if (flag)
                        .{ 1, 0.2, 0.2, 1 }
                    else
                        .{ 0.7, 0.7, 0.7, 1 };
                    const text = if (flag)
                        "YES"
                    else
                        "-";
                    zgui.textColored(color, "{s}", .{text});
                }
            } else {
                for (4..7) |i| {
                    _ = zgui.tableSetColumnIndex(@intCast(i));
                    zgui.textUnformatted("-");
                }
            }
        }
        if (zgui.beginTable("Sources", .{ .column = 2, .flags = .{ .borders = .all } })) {
            defer zgui.endTable();

            zgui.tableSetupColumn("External source", .{});
            zgui.tableSetupColumn("Pad source", .{});
            zgui.tableHeadersRow();
            zgui.tableNextRow(.{});
            _ = zgui.tableSetColumnIndex(0);
            const external_current = if (snapshot) |state|
                state.current
            else
                null;
            pinSourceTable("External", io.external[id], external_current, false);
            _ = zgui.tableSetColumnIndex(1);
            const system = &gui.hub.system_configuration;
            const source: windtunnel.Pad.Source = switch (drive.driver) {
                .linear => |linear| blk: {
                    var source = linear;
                    source.capacitance = system.pad_capacitance;
                    break :blk source;
                },
                .fast => |level| .{
                    .voltage = if (level == .high)
                        system.vio_voltage
                    else
                        0,
                    .resistance = if (level == .high)
                        system.fast_high_resistance
                    else
                        system.fast_low_resistance,
                    .capacitance = system.pad_capacitance,
                },
            };
            const internal_current = if (snapshot) |state|
                state.internal_current
            else
                null;
            pinSourceTable("Internal", source, internal_current, drive.driver == .fast);
        }
    }

    /// Summarize routed logic inputs, output paths and physical drive gating before the engine details.
    fn pinLogic(gui: *GUI, id: u6) void {
        const io = &gui.hub.io;
        const pin = &io.pins[id];
        zgui.text("Receiver {t}", .{io.receiver_inputs[id]});
        if (pin.configuration.isAdc()) {
            zgui.text("ADC density {d:.4}; bit {t}", .{ io.adc_inputs[id].density, io.adc_bits[id] });
        }
        if (zgui.beginTable("Logic status", .{ .column = 7, .flags = .{ .borders = .all, .sizing = .fixed_fit } })) {
            defer zgui.endTable();

            for ([_][:0]const u8{ "A", "B", "IN", "OUT", "DIR", "Smart out", "Drive enable" }) |label| {
                zgui.tableSetupColumn(label, .{});
            }
            zgui.tableHeadersRow();
            zgui.tableNextRow(.{});
            const levels = [_]windtunnel.IO.Signal{
                io.smart_a[id],
                io.smart_b[id],
                windtunnel.IO.Signal.from(pin.outputs.in != 0),
                windtunnel.IO.Signal.from((io.outputs >> id) & 1 != 0),
                windtunnel.IO.Signal.from((io.directions >> id) & 1 != 0),
                pin.outputs.smart_out,
                windtunnel.IO.Signal.from(io.outputEnabled(@enumFromInt(id))),
            };
            for (levels, 0..) |level, i| {
                _ = zgui.tableSetColumnIndex(@intCast(i));
                zgui.textUnformatted(signalName(level));
            }
        }
        const mode = std.meta.activeTag(pin.smart.logic);
        zgui.text("Smart mode: {t}", .{mode});
        switch (mode) {
            .gpio, .dac_noise, .uart_tx, .uart_rx, .sync_tx, .sync_rx => {},
            else => zgui.textColored(.{ 1, 0.4, 0.2, 1 }, "Unsupported smart engine", .{}),
        }
        var x = pin.smart.registers.x;
        if (hexInput("X", &x, gui.is_running)) {
            io.editSmartRegister(@enumFromInt(id), .x, x);
        }
        var y = pin.smart.registers.y;
        if (hexInput("Y", &y, gui.is_running)) {
            io.editSmartRegister(@enumFromInt(id), .y, y);
        }
        gui.show("Host edit delivery", pin.edits);
        gui.show("Result / enable / ready / endpoints", pin.smart.registers);
        gui.show("Active mode payload", pin.smart.logic);
        gui.show("Outputs", pin.outputs);
        gui.show("Command owner", io.owners[id]);
        if (header("Delayed commands", .{})) {
            for (io.commands.slots, 0..) |slot, delay| {
                for (slot) |command| {
                    if (command) |item| {
                        if (item.mask & (@as(u64, 1) << id) != 0) {
                            zgui.pushIntId(@intCast(delay));
                            defer zgui.popId();
                            zgui.pushIntId(item.cog);
                            defer zgui.popId();
                            gui.show("Command", item);
                        }
                    }
                }
            }
        }
        if (header("Delayed samples", .{})) {
            for (0..4) |i| {
                zgui.text("Sample {} pad {s} IN {} result 0x{X:0>8} flag {}", .{
                    i,                                signalName(io.samples.pads[i][id]),
                    (io.samples.inputs[i] >> id) & 1, io.samples.results[i][id].value,
                    io.samples.results[i][id].flag,
                });
            }
            gui.show("DIR delivery", io.direction_pipeline);
            signalText("Synchronous OUT", io.sync_out[id].level);
            signalText("Synchronous IN", io.sync_in[id]);
        }
        if (io.failure) |failure| {
            if (@intFromEnum(failure.pin) == id) {
                gui.show("Failure", failure);
            }
        }
        if (io.collision) |collision| {
            if (@intFromEnum(collision.pin) == id) {
                gui.show("Command collision", collision);
            }
        }
    }

    /// Separate electrical pad state from smart logic, with WRPIN configuration in the local menu.
    fn pinWindow(gui: *GUI, id: u6) void {
        var buffer: [32]u8 = undefined;
        const title = std.fmt.bufPrintSentinel(&buffer, "Pin {}", .{id}, 0) catch unreachable;
        zgui.setNextWindowSize(.{ .w = 700, .h = 540, .cond = .first_use_ever });
        const visible = zgui.begin(title, .{ .popen = &gui.pins_visible[id], .flags = .{ .menu_bar = true } });
        defer zgui.end();

        if (!visible)
            return;

        gui.logWindowSize(title, zgui.getWindowSize());
        const pin = &gui.hub.io.pins[id];
        if (zgui.beginMenuBar()) {
            defer zgui.endMenuBar();

            if (zgui.beginMenu("Configure", true)) {
                defer zgui.endMenu();

                var configuration: u32 = @bitCast(pin.configuration);
                if (hexInput("WRPIN", &configuration, gui.is_running)) {
                    gui.hub.io.editConfiguration(@enumFromInt(id), configuration);
                }
                gui.show("Decoded routing / pad fields", pin.configuration);
            }
        }
        if (header("Pad", .{ .default_open = true })) {
            gui.pinPad(id);
        }
        if (header("Logic", .{ .default_open = true })) {
            gui.pinLogic(id);
        }
    }
};

/// Name the resolved internal drive and its rail without confusing it with the pad's sampled level.
fn pinDriveText(drive: PinDrive) void {
    switch (drive.driver) {
        .fast => |level| zgui.text("fast {t}", .{level}),
        .linear => |source| {
            if (!source.drive_enabled) {
                zgui.textUnformatted("float");
            } else {
                zgui.text("{d:.2}V / {d:.0}ohm", .{ source.voltage, source.resistance });
            }
        },
    }
}

/// Display source parameters in engineering units and signed current into the pad, without generic reflection.
fn pinSourceTable(label: [:0]const u8, source: windtunnel.Pad.Source, current: ?f64, piecewise: bool) void {
    if (!zgui.beginTable(label, .{ .column = 2, .flags = .{ .borders = .all, .sizing = .fixed_fit } }))
        return;
    defer zgui.endTable();

    const labels = [_][:0]const u8{ "Drive", "Voltage", "Resistance", "Current limit", "Capacitance", "Current" };
    for (labels, 0..) |name, row| {
        zgui.tableNextRow(.{});
        _ = zgui.tableSetColumnIndex(0);
        zgui.textUnformatted(name);
        _ = zgui.tableSetColumnIndex(1);
        switch (row) {
            0 => {
                const state = if (source.drive_enabled)
                    "enabled"
                else
                    "float";
                zgui.textUnformatted(state);
            },
            1 => zgui.text("{d:.2} V", .{source.voltage}),
            2 => {
                if (!source.drive_enabled) {
                    zgui.textUnformatted("-");
                } else if (piecewise) {
                    zgui.textUnformatted("piecewise");
                } else {
                    zgui.text("{d:.3} ohm", .{source.resistance});
                }
            },
            3 => {
                if (std.math.isFinite(source.max_current)) {
                    zgui.text("{d:.3} mA", .{source.max_current * 1e3});
                } else {
                    zgui.textUnformatted("Unlimited");
                }
            },
            4 => zgui.text("{d:.3} pF", .{source.capacitance * 1e12}),
            5 => {
                if (current) |value| {
                    zgui.text("{d:.3} mA", .{value * 1e3});
                } else {
                    zgui.textUnformatted("-");
                }
            },
            else => unreachable,
        }
    }
}

/// Read the host's monotonic awake clock in nanoseconds.
fn monotonic(context: *anyopaque) i96 {
    const io: *std.Io = @ptrCast(@alignCast(context));
    return std.Io.Clock.awake.now(io.*).toNanoseconds();
}

/// Open all nested inspector groups during headless rendering tests, using normal saved state in the application.
fn header(label: [:0]const u8, flags: zgui.TreeNodeFlags) bool {
    var options = flags;
    if (builtin.is_test) {
        options.default_open = true;
    }
    return zgui.collapsingHeader(label, options);
}

/// Edit a prefixed hexadecimal word, parsing digits separately from the display prefix.
fn hexInput(label: [:0]const u8, value: *u32, read_only: bool) bool {
    zgui.beginDisabled(.{ .disabled = read_only });
    defer zgui.endDisabled();

    var buffer: [16:0]u8 = @splat(0);
    _ = std.fmt.bufPrintSentinel(&buffer, "0x{X:0>8}", .{value.*}, 0) catch unreachable;
    if (!zgui.inputText(label, .{
        .buf = &buffer,
        .flags = .{ .chars_hexadecimal = true, .auto_select_all = true, .read_only = read_only },
    }))
        return false;

    const text = std.mem.sliceTo(&buffer, 0);
    const digits = if (std.mem.startsWith(u8, text, "0x") or std.mem.startsWith(u8, text, "0X"))
        text[2..]
    else
        text;
    const parsed = std.fmt.parseInt(u32, digits, 16) catch return false;
    if (parsed == value.*)
        return false;

    value.* = parsed;
    return true;
}

/// Format disassembly only when the corresponding visible row/slot is rendered.
fn disassemblyText(word: u32) void {
    var buffer: [256]u8 = undefined;
    var writer = std.Io.Writer.fixed(&buffer);
    windtunnel.disassemble(word, &writer) catch {
        zgui.textUnformatted("Disassembly buffer exhausted");
        return;
    };
    zgui.textUnformatted(writer.buffered());
}

/// Edit an electrical parameter in engineering units without changing its SI representation.
fn quantityInput(label: [:0]const u8, value: *f64, scale: f64) void {
    var displayed = value.* * scale;
    if (zgui.inputScalar(label, f64, .{ .v = &displayed, .cfmt = "%.6g" })) {
        value.* = displayed / scale;
    }
}

/// Give both digital levels explicit labels.
fn signalName(signal: windtunnel.IO.Signal) [:0]const u8 {
    return switch (signal) {
        .low => "0",
        .high => "1",
    };
}

/// Color digital levels consistently across pad and pin inspectors.
fn signalText(label: []const u8, signal: windtunnel.IO.Signal) void {
    const color: [4]f32 = switch (signal) {
        .low => .{ 0.4, 0.65, 1, 1 },
        .high => .{ 0.4, 1, 0.5, 1 },
    };
    zgui.textColored(color, "{s}: {s}", .{ label, signalName(signal) });
}

/// Render nested latches without executing semantics or dereferencing attached endpoints.
fn showValue(label: [:0]const u8, value: anytype) void {
    const T = @TypeOf(value);
    zgui.pushStrId(label);
    defer zgui.popId();
    switch (@typeInfo(T)) {
        .bool => zgui.text("{s}: {}", .{ label, value }),
        .int => {
            const Word = std.meta.Int(.unsigned, @bitSizeOf(T));
            const format = comptime std.fmt.comptimePrint("{{s}}: {{}} (0x{{X:0>{d}}})", .{(@bitSizeOf(T) + 3) / 4});
            zgui.text(format, .{ label, value, @as(Word, @bitCast(value)) });
        },
        .comptime_int => showValue(label, @as(i64, value)),
        .float, .comptime_float => zgui.text("{s}: {d}", .{ label, value }),
        .@"enum" => |info| {
            if (comptime info.fields.len == 0) {
                showValue(label, @intFromEnum(value));
            } else if (std.enums.tagName(T, value)) |name| {
                zgui.text("{s}: {s}", .{ label, name });
            } else {
                showValue(label, @intFromEnum(value));
            }
        },
        .error_set => zgui.text("{s}: {t}", .{ label, value }),
        .optional => {
            if (value) |present| {
                showValue(label, present);
            } else {
                zgui.text("{s}: none", .{label});
            }
        },
        .@"struct" => |info| {
            if (header(label, .{})) {
                inline for (info.fields) |field| {
                    // Uninitialized inactive write/FIFO entries are never inspected.
                    if (comptime std.mem.eql(u8, field.name, "writes")) {
                        for (value.writes[0..value.count], 0..) |item, i| {
                            zgui.pushIntId(@intCast(i));
                            showValue("write", item);
                            zgui.popId();
                        }
                    } else if (comptime std.mem.eql(u8, field.name, "write_bytes") or std.mem.eql(u8, field.name, "words")) {
                        if (header(field.name, .{})) {
                            const count = if (comptime std.mem.eql(u8, field.name, "words")) value.count else value.write_count;
                            const head = if (comptime std.mem.eql(u8, field.name, "words")) value.head else value.write_head;
                            const storage = @field(value, field.name);
                            for (0..count) |i| {
                                const slot = (head + i) % storage.len;
                                var buffer: [32]u8 = undefined;
                                const title = std.fmt.bufPrintSentinel(&buffer, "[{}]", .{slot}, 0) catch unreachable;
                                showValue(title, storage[slot]);
                            }
                        }
                    } else {
                        showValue(field.name, @field(value, field.name));
                    }
                }
            }
        },
        .@"union" => {
            if (header(label, .{})) {
                switch (value) {
                    inline else => |payload, tag| showValue(@tagName(tag), payload),
                }
            }
        },
        .array => {
            if (header(label, .{})) {
                for (value, 0..) |item, i| {
                    zgui.pushIntId(@intCast(i));
                    defer zgui.popId();
                    var buffer: [32]u8 = undefined;
                    const title = std.fmt.bufPrintSentinel(&buffer, "[{}]", .{i}, 0) catch unreachable;
                    showValue(title, item);
                }
            }
        },
        .pointer => zgui.text("{s}: attached", .{label}),
        else => zgui.text("{s}: (opaque)", .{label}),
    }
}

test "all pin and cog inspectors reopen and clip without modifying simulation or triggering watches" {
    zgui.init(std.testing.allocator);
    defer zgui.deinit();

    zgui.io.setIniFilename(null);
    zgui.io.setBackendFlags(.{ .renderer_has_textures = true });
    zgui.io.setDisplaySize(1920, 1080);
    const gui = try std.testing.allocator.create(GUI);
    defer std.testing.allocator.destroy(gui);

    gui.* = .{ .hub = undefined, .debugger = .{ .allocator = std.testing.allocator } };
    try gui.hub.init(.ideal);
    defer gui.debugger.deinit();

    gui.hub.debugger = &gui.debugger;
    gui.configuration_draft = .realistic;
    gui.cogs_visible = @splat(true);
    gui.pins_visible = @splat(true);
    gui.breakpoints_visible = true;
    gui.configuration_visible = true;
    gui.memory_visible = true;
    gui.cordic_visible = true;
    gui.jump = 0x7fff0;
    gui.jump_pending = true;
    for ([_]Debugger.Space{ .registers, .lut, .hub }) |space| {
        try gui.debugger.watches.append(std.testing.allocator, .{
            .space = space,
            .cog = null,
            .start = 0,
            .end = if (space == .hub) 0x80000 else 512,
            .access = .both,
            .scope = .physical,
        });
    }
    const execution: Debugger.Execution = .{ .cog = 0, .domain = .cog, .pc = 0 };
    try gui.debugger.toggleExecution(execution);
    for (&gui.hub.cogs) |*cog| {
        cog.streamer.apply(cog, .{ .command = try windtunnel.Streamer.Command.init(
            0x7f01ffff,
            0x78563412,
            null,
            .immediate,
            .{},
        ) });
        cog.streamer.apply(cog, .{ .command = try windtunnel.Streamer.Command.init(
            0x7f010020,
            0xfedcba98,
            null,
            .continuous,
            .{},
        ) });
        cog.converter.apply(.{ .cy = 0x8000_0000 });
        cog.converter.apply(.{ .control = 0x120 });
        _ = cog.converter.step(cog.raw_dacs);
        for (&cog.pipeline, 0..) |*slot, i| {
            slot.* = .{
                .pc = @intCast(i),
                .instr = 0xf6042809,
                .local_address = @intCast(i),
                .operands = .{ .d = 0, .s = 9, .ptra = 0, .ptrb = 0, .pa = 0, .pb = 0 },
                .ready_at = 99,
                .command_at = 88,
            };
        }
    }
    gui.hub.cordic.submit(@enumFromInt(0), .mul, 3, 7, 0);
    _ = gui.hub.cordic.step();
    gui.selected_cell = 1;
    gui.selected_pin = 63;
    const cordic = gui.hub.cordic;
    const streamer = gui.hub.cogs[0].streamer;
    const dacs = gui.hub.cogs[0].dacs;
    const raw_dacs = gui.hub.cogs[0].raw_dacs;
    const converter = gui.hub.cogs[0].converter;
    const registers = gui.hub.cogs[0].registers;
    const pipeline = gui.hub.cogs[0].pipeline;
    const random = gui.hub.random;
    for (0..3) |frame| {
        if (frame == 1) {
            gui.cogs_visible = @splat(false);
            gui.pins_visible = @splat(false);
            gui.breakpoints_visible = false;
            gui.configuration_visible = false;
        } else {
            gui.cogs_visible = @splat(true);
            gui.pins_visible = @splat(true);
            gui.breakpoints_visible = true;
            gui.configuration_visible = true;
        }
        zgui.newFrame();
        gui.controls();
        try gui.inspectors();
        zgui.render();
    }
    try std.testing.expectEqual(@as(u64, 0), gui.hub.counter);
    try std.testing.expectEqualDeep(registers, gui.hub.cogs[0].registers);
    try std.testing.expectEqualDeep(pipeline, gui.hub.cogs[0].pipeline);
    try std.testing.expectEqualDeep(random, gui.hub.random);
    try std.testing.expectEqualDeep(cordic, gui.hub.cordic);
    try std.testing.expectEqualDeep(streamer, gui.hub.cogs[0].streamer);
    try std.testing.expectEqualDeep(dacs, gui.hub.cogs[0].dacs);
    try std.testing.expectEqualDeep(raw_dacs, gui.hub.cogs[0].raw_dacs);
    try std.testing.expectEqualDeep(converter, gui.hub.cogs[0].converter);
    try std.testing.expectEqual(@as(usize, 0), gui.debugger.hits.items.len);
    try std.testing.expect(gui.debugger.hasExecution(execution));
    try std.testing.expectEqual(@as(u32, 0x7fff0), gui.jump);
    try std.testing.expectEqual(@as(u6, 63), gui.selected_pin);
    try std.testing.expectEqualDeep(windtunnel.SystemConfiguration.ideal, gui.hub.system_configuration);
    try std.testing.expectEqualDeep(windtunnel.SystemConfiguration.realistic, gui.configuration_draft);
}

test "GUI configuration drafts are isolated and reinitialization restores the image and debugger definitions" {
    const gui = try std.testing.allocator.create(GUI);
    defer std.testing.allocator.destroy(gui);

    gui.* = .{ .hub = undefined, .debugger = .{ .allocator = std.testing.allocator } };
    defer gui.debugger.deinit();

    try gui.hub.init(.ideal);
    gui.hub.debugger = &gui.debugger;
    const image = [_]u8{ 0x11, 0x22, 0x33, 0x44 };
    gui.initial_image = &image;
    const execution: Debugger.Execution = .{ .cog = 0, .domain = .cog, .pc = 0 };
    try gui.debugger.toggleExecution(execution);
    try gui.debugger.watches.append(std.testing.allocator, .{
        .space = .hub,
        .cog = null,
        .start = 0,
        .end = 4,
        .access = .both,
    });
    gui.hub.memory[0] = 0xaa;
    gui.hub.memory[4096] = 0xbb;
    try gui.hub.io.setExternal(@enumFromInt(10), .high);
    try std.testing.expectEqual(windtunnel.Hub.StepResult.normal, gui.hub.step());
    gui.debugger.stop = .data;
    gui.is_running = true;
    gui.step_requested = true;
    gui.jump_pending = true;
    gui.configuration_draft = .realistic;
    gui.configuration_draft.pad_capacitance = 0;
    try std.testing.expectError(error.InvalidConfiguration, gui.reinitialize());
    try std.testing.expectEqualDeep(windtunnel.SystemConfiguration.ideal, gui.hub.system_configuration);
    try std.testing.expectEqual(@as(u8, 0xaa), gui.hub.memory[0]);
    try std.testing.expectEqual(@as(u64, 1), gui.hub.counter);
    try std.testing.expect(gui.is_running and gui.step_requested);
    gui.configuration_draft = .realistic;
    gui.configuration_draft.oversample = 0;
    try std.testing.expectError(error.InvalidConfiguration, gui.reinitialize());
    try std.testing.expectEqual(@as(u32, 1), gui.hub.system_configuration.oversample);
    gui.configuration_draft.oversample = 8;
    try std.testing.expectEqual(@as(u32, 1), gui.hub.system_configuration.oversample);
    try gui.reinitialize();
    try std.testing.expectEqualDeep(gui.configuration_draft, gui.hub.system_configuration);
    try std.testing.expectEqual(@as(u32, 8), gui.hub.system_configuration.oversample);
    try std.testing.expectEqualSlices(u8, &image, gui.hub.memory[0..image.len]);
    try std.testing.expectEqual(@as(u8, 0), gui.hub.memory[4096]);
    try std.testing.expectEqual(@as(u64, 0), gui.hub.counter);
    try std.testing.expect(gui.hub.io.getExternal(@enumFromInt(10)) == null);
    try std.testing.expectEqualDeep(windtunnel.Pad.Source.float, gui.hub.io.external[10]);
    try std.testing.expect(gui.hub.is_any_cog_active());
    try std.testing.expect(gui.hub.debugger == &gui.debugger);
    try std.testing.expect(gui.debugger.hasExecution(execution));
    try std.testing.expectEqual(@as(usize, 1), gui.debugger.watches.items.len);
    try std.testing.expectEqual(Debugger.Stop.none, gui.debugger.stop);
    try std.testing.expectEqual(@as(usize, 0), gui.debugger.hits.items.len);
    try std.testing.expectEqual(@as(usize, 0), gui.debugger.encounters.items.len);
    try std.testing.expect(!gui.is_running and !gui.step_requested and !gui.jump_pending);
    try std.testing.expectEqualDeep(windtunnel.Frame{}, gui.frame);
    gui.initial_image = null;
    gui.configuration_draft.vio_voltage = 3.2;
    try std.testing.expectEqualStrings("Custom", gui.configuration_draft.name());
    try std.testing.expectEqual(@as(f64, 3.3), gui.hub.system_configuration.vio_voltage);
    try gui.reinitialize();
    try std.testing.expect(!gui.hub.is_any_cog_active());
    try std.testing.expectEqual(@as(u8, 0), gui.hub.memory[0]);
    try std.testing.expectEqual(@as(f64, 3.2), gui.hub.system_configuration.vio_voltage);
}
