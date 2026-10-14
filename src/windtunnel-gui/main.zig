//! Minimal desktop shell for Windtunnel, using zig-gamedev's GLFW/OpenGL ImGui backend.
const std = @import("std");
const builtin = @import("builtin");
const glfw = @import("zglfw");
const zopengl = @import("zopengl");
const zgui = @import("zgui");
const gl = zopengl.bindings;
const windtunnel = @import("windtunnel");
const args_parser = @import("args");

const CliArgs = struct {
    help: bool = false,
    image: ?[]const u8 = null,

    pub const shorthands = .{ .h = "help" };

    pub const meta = .{
        .usage_summary = "[--help] [--image <file>]",
        .full_text = "Windtunnel's graphical interface for the Parallax Propeller 2 simulator.",
        .option_docs = .{
            .help = "Prints this help text",
            .image = "Loads the initial hub image from a file",
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
    var host_io = init.io;
    gui.hub.init();
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

        // draw imgui here
        try gui.controls();
        gui.advance(&host_io);
        try gui.inspectors();

        {
            const visible = zgui.begin("GLFW", .{});
            defer zgui.end();

            if (visible) {
                if (zgui.button("Quit", .{})) {
                    window.setShouldClose(true);
                }
            }
        }

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

const GUI = struct {
    hub: windtunnel.Hub,
    debugger: Debugger,
    is_running: bool = false,
    step_requested: bool = false,
    frame: windtunnel.Frame = .{},
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
    start_load: [8]bool = @splat(true),
    hubset: u32 = 0,
    seed: u32 = 0,
    sensor_config: [8][4]u32 = @splat(@splat(0)),
    pattern_mask: [8]u32 = @splat(0),
    pattern_value: [8]u32 = @splat(0),
    selected_cell: usize = 0,
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

    /// Collect host controls before simulation; inspectors always render the resulting complete edge.
    fn controls(gui: *GUI) !void {
        if (!new_window("Control", null))
            return;
        defer zgui.end();

        if (zgui.button("Step", .{})) {
            gui.debugger.acknowledge(&gui.hub);
            gui.is_running = false;
            gui.step_requested = true;
        }
        zgui.sameLine(.{});
        if (zgui.button(if (gui.is_running) "Pause" else "Continue", .{})) {
            gui.is_running = !gui.is_running;
            if (gui.is_running) {
                gui.debugger.acknowledge(&gui.hub);
            }
        }
        if (header("Windows", .{ .default_open = true })) {
            _ = zgui.checkbox("Hub", .{ .v = &gui.hub_visible });
            zgui.sameLine(.{});
            _ = zgui.checkbox("I/O", .{ .v = &gui.io_visible });
            zgui.sameLine(.{});
            _ = zgui.checkbox("Memory", .{ .v = &gui.memory_visible });
            zgui.sameLine(.{});
            _ = zgui.checkbox("CORDIC", .{ .v = &gui.cordic_visible });
            for (&gui.cogs_visible, 0..) |*visible, i| {
                var buffer: [32]u8 = undefined;
                const title = std.fmt.bufPrintSentinel(&buffer, "Cog {}", .{i}, 0) catch unreachable;
                if (i != 0) {
                    zgui.sameLine(.{});
                }
                _ = zgui.checkbox(title, .{ .v = visible });
            }
            if (header("Pins", .{})) {
                for (&gui.pins_visible, 0..) |*visible, i| {
                    var buffer: [32]u8 = undefined;
                    const title = std.fmt.bufPrintSentinel(&buffer, "Pin {}", .{i}, 0) catch unreachable;
                    if (i % 8 != 0) {
                        zgui.sameLine(.{});
                    }
                    _ = zgui.checkbox(title, .{ .v = visible });
                }
            }
        }
        if (header("Execution breakpoints", .{})) {
            _ = zgui.inputScalar("Cog##execution", u32, .{ .v = &gui.execution_cog });
            gui.execution.cog = @intCast(@min(gui.execution_cog, 7));
            if (zgui.button(@tagName(gui.execution.domain), .{})) {
                gui.execution.domain = switch (gui.execution.domain) {
                    .cog => .lut,
                    .lut => .hub,
                    .hub => .cog,
                };
            }
            _ = hexInput("PC (local includes $200 LUT base)", &gui.execution_pc, false);
            gui.execution.pc = @truncate(gui.execution_pc);
            if (zgui.button("Toggle breakpoint", .{})) {
                try gui.debugger.toggleExecution(gui.execution);
            }
            var i: usize = 0;
            while (i < gui.debugger.executions.items.len) {
                const item = gui.debugger.executions.items[i];
                zgui.pushIntId(@intCast(i));
                zgui.text("Cog {} {t} ${X:0>5}", .{ item.cog, item.domain, item.pc });
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
        if (header("Data watchpoints", .{})) {
            try gui.watchEditor();
        }
    }

    /// Render the latest completed frame and structured stop after advancement.
    fn status(gui: *GUI) void {
        if (!new_window("Execution", null))
            return;
        defer zgui.end();

        zgui.text("{s} | stop: {t} | {} clocks | {d:.3} ms simulation/frame", .{
            if (gui.is_running) "Running" else "Paused",
            gui.debugger.stop,
            gui.frame.clocks,
            @as(f64, @floatFromInt(gui.frame.nanoseconds)) / 1_000_000.0,
        });
        for (gui.debugger.encounters.items) |encounter| {
            zgui.text("Cog {} {t} PC ${X:0>5} before clock {}", .{
                encounter.cog, encounter.domain, encounter.pc, gui.hub.counter,
            });
        }
        if (gui.hub.fault) |fault| {
            show("Fault", fault);
        }
        if (header("Data stop matches", .{})) {
            for (gui.debugger.hits.items, 0..) |hit, i| {
                zgui.pushIntId(@intCast(i));
                defer zgui.popId();
                zgui.text("Clock {} {t}/{t} {t} ${X} = ${X} ({t})", .{
                    hit.clock, hit.space, hit.storage, hit.access, hit.address, hit.value, hit.class,
                });
                show("Provenance", hit.provenance);
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
            zgui.text("{t} ${X}..${X} {t} {t}", .{ item.space, item.start, item.end, item.access, item.scope });
            show("Filters", .{ .cog = item.cog, .issuer = item.issuer });
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
        gui.status();
        if (gui.hub_visible) {
            try gui.hubWindow();
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
    }

    /// Inspect shared resources and edit architectural hub settings only while paused.
    fn hubWindow(gui: *GUI) !void {
        if (!new_window("Hub", &gui.hub_visible))
            return;
        defer zgui.end();
        const hub = &gui.hub;
        zgui.text("CT {} | clock ${X:0>8} | {} Hz", .{ hub.counter, hub.io.clock_mode, hub.io.clock_frequency });
        show("Random generator", hub.random);
        show("Debug configuration", hub.debug);
        show("Pending starts", hub.pending_starts);
        show("Pending events", hub.pending_events);
        show("Fault", hub.fault);
        zgui.beginDisabled(.{ .disabled = gui.is_running });
        defer zgui.endDisabled();
        _ = hexInput("HUBSET clock/debug", &gui.hubset, false);
        if (zgui.button("Apply HUBSET", .{})) {
            hub.configure(gui.hubset);
            if (hub.reset_requested) {
                gui.step_requested = true;
            }
        }
        if (zgui.inputScalar("Crystal frequency Hz", u64, .{ .v = &hub.io.crystal_frequency })) {
            _ = hub.io.setClock(hub.io.clock_mode);
        }
        _ = hexInput("Random seed", &gui.seed, false);
        if (zgui.button("Seed random", .{})) {
            hub.configure(gui.seed | 0x80000000);
        }
        if (header("Locks", .{ .default_open = true })) {
            for (&hub.locks, 0..) |*lock, i| {
                zgui.pushIntId(@intCast(i));
                defer zgui.popId();
                zgui.text("Lock {}", .{i});
                zgui.sameLine(.{});
                _ = zgui.checkbox("Allocated", .{ .v = &lock.allocated });
                zgui.sameLine(.{});
                var taken = lock.taken;
                if (zgui.checkbox("Taken", .{ .v = &taken })) {
                    lock.taken = taken;
                    hub.signal_lock(lock.id, taken);
                }
                var owner: u32 = lock.owner;
                if (zgui.inputScalar("Owner", u32, .{ .v = &owner })) {
                    lock.owner = @intCast(@min(owner, 7));
                }
            }
        }
        if (header("Start/stop cogs", .{})) {
            for (&hub.cogs, 0..) |*cog, i| {
                zgui.pushIntId(@intCast(i));
                defer zgui.popId();
                zgui.text("Cog {}: {t}", .{ i, cog.exec_mode });
                _ = hexInput("Address", &gui.start_address[i], false);
                _ = hexInput("PTRA", &gui.start_ptra[i], false);
                _ = zgui.checkbox("Load image", .{ .v = &gui.start_load[i] });
                if (zgui.button("Start cog", .{})) {
                    try hub.start_cog(@intCast(i), .{
                        .hub_address = gui.start_address[i],
                        .ptra = gui.start_ptra[i],
                        .load_image = gui.start_load[i],
                    });
                }
                zgui.sameLine(.{});
                if (zgui.button("Stop cog", .{})) {
                    cog.reset();
                }
            }
        }
    }

    /// Show every physical pad, with distinct four-state signals and independently addressable pin windows.
    fn ioWindow(gui: *GUI) void {
        if (!new_window("I/O", &gui.io_visible))
            return;
        defer zgui.end();
        if (!zgui.beginTable("Pads", .{ .column = 8, .flags = .{ .borders = .all } }))
            return;
        defer zgui.endTable();
        for (0..64) |i| {
            if (i % 8 == 0) {
                zgui.tableNextRow(.{});
            }
            _ = zgui.tableSetColumnIndex(@intCast(i % 8));
            zgui.pushIntId(@intCast(i));
            defer zgui.popId();
            const io = &gui.hub.io;
            const enabled = io.outputEnabled(@enumFromInt(i));
            zgui.text("P{}", .{i});
            signalText("Pad", io.pads[i]);
            zgui.text("OE {}", .{enabled});
            signalText("Ext", io.external[i]);
            signalText("Sample", io.pad_reads[i]);
            if (zgui.button("Inspect", .{})) {
                gui.pins_visible[i] = true;
            }
            zgui.beginDisabled(.{ .disabled = enabled });
            for ([_]windtunnel.IO.Signal{ .zero, .one, .z }) |state| {
                if (zgui.button(signalName(state), .{})) {
                    io.setExternal(@enumFromInt(i), if (io.external[i] == state) .z else state);
                }
                if (state != .z) {
                    zgui.sameLine(.{});
                }
            }
            zgui.endDisabled();
        }
    }

    /// Inspect backing RAM directly, clipping rows and never calling watched access paths.
    fn memoryWindow(gui: *GUI) !void {
        if (!new_window("Memory", &gui.memory_visible))
            return;
        defer zgui.end();
        zgui.text("512 KiB backing RAM | debug protection: {}", .{gui.hub.debug.protected});
        zgui.textUnformatted("$FC000..$FFFFF mirrors $7C000..$7FFFF; $80000..$FBFFF is a hole.");
        _ = hexInput("Jump to byte", &gui.jump, false);
        if (zgui.button("Jump", .{})) {
            gui.jump = @min(gui.jump, 0x7ffff);
            gui.jump_pending = true;
        }
        if (header("Watch selected byte range", .{})) {
            try gui.watchEditor();
        }
        if (!zgui.beginTable("RAM", .{
            .column = 3,
            .outer_size = .{ 0, 400 },
            .flags = .{ .scroll_y = true, .row_bg = true, .borders = .all },
        }))
            return;
        defer zgui.endTable();
        zgui.tableSetupScrollFreeze(0, 1);
        zgui.tableSetupColumn("Address (select range start)", .{});
        zgui.tableSetupColumn("Bytes", .{});
        zgui.tableSetupColumn("ASCII", .{});
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
                const name = std.fmt.bufPrintSentinel(&label, "${X:0>5}", .{address}, 0) catch unreachable;
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
                var bytes: [49]u8 = undefined;
                var writer = std.Io.Writer.fixed(&bytes);
                var ascii: [16]u8 = undefined;
                for (gui.hub.memory[address..][0..16], 0..) |byte, i| {
                    writer.print("{X:0>2} ", .{byte}) catch unreachable;
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
        if (!new_window("CORDIC", &gui.cordic_visible))
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
            show("Input latch", cordic.input);
        } else if (selected <= 54) {
            show("Stage allocation", windtunnel.Cordic.stage_layout[selected - 1]);
            show("Arithmetic payload", cordic.pipeline[selected - 1]);
        } else {
            show("Return payload", pending_returns[selected - 55]);
        }
    }

    /// Group architectural editors and read-only pipeline/controller state for one cog.
    fn cogWindow(gui: *GUI, id: u3) !void {
        var buffer: [32]u8 = undefined;
        const title = std.fmt.bufPrintSentinel(&buffer, "Cog {}", .{id}, 0) catch unreachable;
        if (!new_window(title, &gui.cogs_visible[id]))
            return;
        defer zgui.end();
        const cog = &gui.hub.cogs[id];
        zgui.text("Execution {t} | PC ${X:0>5} (read only)", .{ cog.exec_mode, cog.pc });
        zgui.beginDisabled(.{ .disabled = gui.is_running });
        _ = zgui.checkbox("C", .{ .v = &cog.c });
        zgui.sameLine(.{});
        _ = zgui.checkbox("Z", .{ .v = &cog.z });
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
        zgui.textUnformatted("Raw edits retain captured operands and pending writes. Stack entries may be sticky; depth is unknown.");
        if (header("Registers", .{ .default_open = true })) {
            try gui.localTable(cog, .cog);
        }
        if (header("LUT", .{})) {
            try gui.localTable(cog, .lut);
        }
        if (header("Stack (all physical slots)", .{})) {
            for (&cog.stack, 0..) |*value, i| {
                zgui.pushIntId(@intCast(i));
                defer zgui.popId();
                _ = hexInput("Slot", value, gui.is_running);
            }
        }
        if (header("Events", .{})) {
            for (std.enums.values(windtunnel.types.EventId)) |event| {
                var active = cog.events.contains(event);
                zgui.beginDisabled(.{ .disabled = gui.is_running });
                if (zgui.checkbox(@tagName(event), .{ .v = &active })) {
                    if (active) {
                        cog.raise_event(event);
                    } else {
                        cog.events.remove(event);
                    }
                }
                zgui.endDisabled();
            }
        }
        show("Counter sensors", cog.ct_targets);
        show("Selectable sensors", cog.selectable_events);
        show("Pattern sensor", cog.pattern);
        show("Interrupt controller", cog.interrupts);
        show("Debug controller", cog.debug);
        show("Repeat", cog.repeat);
        show("Skip", cog.skip);
        show("XBYTE mode", cog.xbyte_mode);
        show("XBYTE default mode", cog.xbyte_default_mode);
        show("XBYTE phase", cog.xbyte);
        show("FIFO", cog.fifo_state);
        show("Memory transfer", cog.memory_transfer);
        show("Startup", cog.startup);
        show("Read bus", cog.read_bus);
        show("CORDIC result ports", cog.cordic_result);
        show("RAM collision", cog.ram_collision);
        show("LUT collision", cog.lut_collision);
        show("Pending writeback", cog.writeback);
        show("Pending hub write", cog.hub_writeback);
        show("Wait deadline", cog.wait_until);
        show("Retired instruction", cog.retired);
        if (header("Pipeline (fetch/read/latch/execute/store)", .{ .default_open = true })) {
            for (cog.pipeline, 0..) |slot, i| {
                zgui.pushIntId(@intCast(i));
                defer zgui.popId();
                if (slot) |state| {
                    zgui.text("Slot {}: PC ${X:0>5}, instruction ${X:0>8}", .{ i, state.pc, state.instr });
                    disassemblyText(state.instr);
                    show("Captured state / timing", state);
                } else {
                    zgui.text("Slot {}: empty", .{i});
                }
            }
        }
    }

    /// Clip local rows and show data, independent disassembly, all references and persistent breakpoints together.
    fn localTable(gui: *GUI, cog: *windtunnel.Cog, domain: Debugger.Domain) !void {
        zgui.pushIntId(@intFromEnum(domain));
        defer zgui.popId();
        if (!zgui.beginTable("Local RAM", .{
            .column = 8,
            .outer_size = .{ 0, 330 },
            .flags = .{ .scroll_y = true, .scroll_x = true, .row_bg = true, .borders = .all, .resizable = true },
        }))
            return;
        defer zgui.endTable();
        zgui.tableSetupScrollFreeze(0, 1);
        for ([_][:0]const u8{ "Address", "Role", "Hex data", "Tail RAM", "Execute word", "Disassembly", "Annotations", "Break / watch" }) |label| {
            zgui.tableSetupColumn(label, .{});
        }
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
                zgui.text("${X:0>3}", .{index});
                _ = zgui.tableSetColumnIndex(1);
                if (domain == .cog and index >= 0x1f0) {
                    zgui.text("{t}", .{reg});
                } else {
                    zgui.textUnformatted(if (domain == .lut) "LUT RAM" else "Cog RAM");
                }
                _ = zgui.tableSetColumnIndex(2);
                var value = if (domain == .cog) inspect.register(cog, index) else cog.lut[index];
                if (hexInput("##data", &value, gui.is_running or (domain == .cog and index >= 0x1fe and !badges.debug_overlay))) {
                    if (domain == .cog) {
                        cog.write_reg(reg, value);
                    } else {
                        cog.lut[index] = value;
                    }
                }
                _ = zgui.tableSetColumnIndex(3);
                if (domain == .cog and index >= 504) {
                    _ = hexInput("##tail", &cog.ram_tail[index - 504], gui.is_running);
                }
                const word = if (domain == .cog and index >= 504) windtunnel.Cog.debug_rom[index - 504] else value;
                _ = zgui.tableSetColumnIndex(4);
                zgui.text("${X:0>8}", .{word});
                _ = zgui.tableSetColumnIndex(5);
                disassemblyText(word);
                _ = zgui.tableSetColumnIndex(6);
                var buffer: [512]u8 = undefined;
                var writer = std.Io.Writer.fixed(&buffer);
                try badges.write(&writer);
                zgui.textUnformatted(writer.buffered());
                _ = zgui.tableSetColumnIndex(7);
                const execution: Debugger.Execution = .{
                    .cog = cog.id,
                    .domain = domain,
                    .pc = @as(u20, index) + @as(u20, if (domain == .lut) 0x200 else 0),
                };
                var breakpoint = gui.debugger.hasExecution(execution);
                if (zgui.checkbox("Exec", .{ .v = &breakpoint })) {
                    try gui.debugger.toggleExecution(execution);
                }
                zgui.sameLine(.{});
                if (zgui.button("Watch", .{})) {
                    gui.watch.space = if (domain == .lut) .lut else .registers;
                    gui.watch.cog = cog.id;
                    gui.watch_cog = cog.id;
                    gui.watch.start = index;
                    gui.watch.end = @as(u32, index) + 1;
                }
            }
        }
    }

    /// Inspect full routing first, then the active smart-engine payload and all delayed work.
    fn pinWindow(gui: *GUI, id: u6) void {
        var buffer: [32]u8 = undefined;
        const title = std.fmt.bufPrintSentinel(&buffer, "Pin {}", .{id}, 0) catch unreachable;
        if (!new_window(title, &gui.pins_visible[id]))
            return;
        defer zgui.end();
        const io = &gui.hub.io;
        const pin = &io.pins[id];
        var configuration: u32 = @bitCast(pin.configuration);
        if (hexInput("WRPIN", &configuration, gui.is_running)) {
            io.editConfiguration(@enumFromInt(id), configuration);
        }
        show("Decoded routing / pad fields", pin.configuration);
        if (!windtunnel.IO.supported(configuration & ~@as(u32, 1))) {
            zgui.textColored(.{ 1, 0.4, 0.2, 1 }, "Unsupported pad configuration", .{});
        }
        const mode = std.meta.activeTag(pin.smart.logic);
        zgui.text("Smart mode {t} | physical output enable {}", .{ mode, io.outputEnabled(@enumFromInt(id)) });
        switch (mode) {
            .gpio, .repository, .uart_tx, .uart_rx, .sync_tx, .sync_rx => {},
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
        show("Host edit delivery", pin.edits);
        show("Result / enable / ready / endpoints", pin.smart.registers);
        show("Active mode payload", pin.smart.logic);
        show("Outputs", pin.outputs);
        signalText("Physical pad", io.pads[id]);
        signalText("Sampled input", io.pad_reads[id]);
        signalText("SmartA", io.smart_a[id]);
        signalText("SmartB", io.smart_b[id]);
        show("Command owner", io.owners[id]);
        if (header("Delayed commands", .{})) {
            for (io.commands.slots, 0..) |slot, delay| {
                for (slot) |command| {
                    if (command) |item| {
                        if (item.mask & (@as(u64, 1) << id) != 0) {
                            zgui.pushIntId(@intCast(delay));
                            defer zgui.popId();
                            zgui.pushIntId(item.cog);
                            defer zgui.popId();
                            show("Command", item);
                        }
                    }
                }
            }
        }
        if (header("Delayed samples", .{})) {
            for (0..4) |i| {
                zgui.text("Sample {} pad {s} IN {} result ${X:0>8} flag {}", .{
                    i,                                signalName(io.samples.pads[i][id]),
                    (io.samples.inputs[i] >> id) & 1, io.samples.results[i][id].value,
                    io.samples.results[i][id].flag,
                });
            }
            show("DIR delivery", io.direction_pipeline);
            signalText("Synchronous OUT", io.sync_out[id]);
            signalText("Synchronous IN", io.sync_in[id]);
        }
        if (io.failure) |failure| {
            if (@intFromEnum(failure.pin) == id) {
                show("Failure", failure);
            }
        }
        if (io.collision) |collision| {
            if (@intFromEnum(collision.pin) == id) {
                show("Command collision", collision);
            }
        }
    }
};

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

/// Open a closable independent inspector, balancing Begin/End even when collapsed.
fn new_window(title: [:0]const u8, visible: ?*bool) bool {
    zgui.setNextWindowSize(.{ .w = 700, .h = 540, .cond = .first_use_ever });
    if (!zgui.begin(title, .{ .popen = visible })) {
        zgui.end();
        return false;
    }
    return true;
}

/// Edit a hexadecimal architectural word; read-only inputs cannot mutate the source.
fn hexInput(label: [:0]const u8, value: *u32, read_only: bool) bool {
    zgui.beginDisabled(.{ .disabled = read_only });
    defer zgui.endDisabled();
    return zgui.inputScalar(label, u32, .{
        .v = value,
        .cfmt = "%08X",
        .flags = .{ .chars_hexadecimal = true, .read_only = read_only },
    });
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

/// Give all four digital states explicit, distinct labels.
fn signalName(signal: windtunnel.IO.Signal) [:0]const u8 {
    return switch (signal) {
        .zero => "0",
        .one => "1",
        .x => "X",
        .z => "Z",
    };
}

/// Color four-state signals consistently across pad and pin inspectors.
fn signalText(label: []const u8, signal: windtunnel.IO.Signal) void {
    const color: [4]f32 = switch (signal) {
        .zero => .{ 0.4, 0.65, 1, 1 },
        .one => .{ 0.4, 1, 0.5, 1 },
        .x => .{ 1, 0.3, 0.25, 1 },
        .z => .{ 0.8, 0.7, 0.9, 1 },
    };
    zgui.textColored(color, "{s}: {s}", .{ label, signalName(signal) });
}

/// Render nested simulator latches without invoking semantic helpers or dereferencing attached endpoints.
fn show(label: [:0]const u8, value: anytype) void {
    const T = @TypeOf(value);
    zgui.pushStrId(label);
    defer zgui.popId();
    switch (@typeInfo(T)) {
        .bool => zgui.text("{s}: {}", .{ label, value }),
        .int, .comptime_int => zgui.text("{s}: {} (${X})", .{ label, value, value }),
        .@"enum" => |info| {
            if (comptime info.fields.len == 0) {
                zgui.text("{s}: {} (${X})", .{ label, @intFromEnum(value), @intFromEnum(value) });
            } else if (std.enums.tagName(T, value)) |name| {
                zgui.text("{s}: {s}", .{ label, name });
            } else {
                zgui.text("{s}: {} (${X})", .{ label, @intFromEnum(value), @intFromEnum(value) });
            }
        },
        .error_set => zgui.text("{s}: {t}", .{ label, value }),
        .optional => {
            if (value) |present| {
                show(label, present);
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
                            show("write", item);
                            zgui.popId();
                        }
                    } else if (comptime std.mem.eql(u8, field.name, "write_bytes") or std.mem.eql(u8, field.name, "words")) {
                        if (header(field.name, .{})) {
                            const count = if (comptime std.mem.eql(u8, field.name, "words")) value.count else value.write_count;
                            const head = if (comptime std.mem.eql(u8, field.name, "words")) value.head else value.write_head;
                            const storage = @field(value, field.name);
                            for (0..count) |i| {
                                const slot = (head + i) % storage.len;
                                zgui.text("[{}]: ${X}", .{ slot, storage[slot] });
                            }
                        }
                    } else {
                        show(field.name, @field(value, field.name));
                    }
                }
            }
        },
        .@"union" => {
            if (header(label, .{})) {
                switch (value) {
                    inline else => |payload, tag| show(@tagName(tag), payload),
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
                    show(title, item);
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
    gui.hub.init();
    defer gui.debugger.deinit();

    gui.hub.debugger = &gui.debugger;
    gui.cogs_visible = @splat(true);
    gui.pins_visible = @splat(true);
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
    const cordic = gui.hub.cordic;
    const registers = gui.hub.cogs[0].registers;
    const pipeline = gui.hub.cogs[0].pipeline;
    const random = gui.hub.random;
    for (0..3) |frame| {
        if (frame == 1) {
            gui.cogs_visible = @splat(false);
            gui.pins_visible = @splat(false);
        } else {
            gui.cogs_visible = @splat(true);
            gui.pins_visible = @splat(true);
        }
        zgui.newFrame();
        try gui.controls();
        try gui.inspectors();
        zgui.render();
    }
    try std.testing.expectEqual(@as(u64, 0), gui.hub.counter);
    try std.testing.expectEqualDeep(registers, gui.hub.cogs[0].registers);
    try std.testing.expectEqualDeep(pipeline, gui.hub.cogs[0].pipeline);
    try std.testing.expectEqualDeep(random, gui.hub.random);
    try std.testing.expectEqualDeep(cordic, gui.hub.cordic);
    try std.testing.expectEqual(@as(usize, 0), gui.debugger.hits.items.len);
    try std.testing.expect(gui.debugger.hasExecution(execution));
    try std.testing.expectEqual(@as(u32, 0x7fff0), gui.jump);
}
