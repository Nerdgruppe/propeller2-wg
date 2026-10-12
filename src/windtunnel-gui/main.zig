//! Minimal desktop shell for Windtunnel, using zig-gamedev's GLFW/OpenGL ImGui backend.
const std = @import("std");
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

    var gui: GUI = .{ .hub = undefined };
    gui.hub.init();

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
        try gui.update_controls();

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

const GUI = struct {
    hub: windtunnel.Hub,
    is_running: bool = false,
    cogs_visible: [8]bool = @splat(false),

    fn update_controls(gui: *GUI) !void {
        const hub = &gui.hub;

        var step_one: bool = gui.is_running;

        var temp_buf: [128]u8 = undefined;

        if (new_window("Control", .{})) {
            defer zgui.end();

            if (zgui.button("Step", .{})) {
                step_one = true;
            }

            zgui.sameLine(.{});
            zgui.textUnformatted("|");
            zgui.sameLine(.{});

            zgui.beginDisabled(.{ .disabled = gui.is_running });
            if (zgui.button("Start", .{})) {
                gui.is_running = true;
                step_one = true;
            }
            zgui.endDisabled();

            zgui.sameLine(.{});
            zgui.textUnformatted("|");
            zgui.sameLine(.{});

            zgui.beginDisabled(.{ .disabled = !gui.is_running });
            if (zgui.button("Stop", .{})) {
                gui.is_running = false;
                step_one = false;
            }
            zgui.endDisabled();
        }

        if (new_window("Hub", .{})) {
            defer zgui.end();

            var ct: u64 = hub.counter;
            _ = zgui.inputScalar("Counter", u64, .{
                .v = &ct,
                .flags = .{ .read_only = true },
            });

            zgui.separator();
            zgui.text("Locks", .{});

            for (0..16) |lock_id| {
                const name = std.fmt.bufPrintSentinel(&temp_buf, "{[0]}##lock.{[0]}", .{lock_id}, 0) catch unreachable;

                if (lock_id % 4 != 0) {
                    zgui.sameLine(.{});
                }

                _ = zgui.selectable(name, .{
                    .selected = hub.locks[lock_id].taken,
                    .flags = .{ .disabled = !hub.locks[lock_id].allocated },
                    .w = 12,
                });
            }

            zgui.separator();
            zgui.text("Debug", .{});

            _ = zgui.checkbox("Protected##DebugEnable", .{ .v = &hub.debug.protected });
            _ = zgui.checkbox("Memory Locked##DebugEnable", .{ .v = &hub.debug.locked });
        }

        for (0..8) |cog_id| {
            if (!gui.cogs_visible[cog_id])
                continue;

            zgui.pushIntId(@intCast(cog_id));
            defer zgui.popId();

            const cog = &hub.cogs[cog_id];

            const title = std.fmt.bufPrintSentinel(&temp_buf, "Cog {}", .{cog_id}, 0) catch unreachable;

            if (new_window(title, .{})) {
                defer zgui.end();

                zgui.text("Execution: {t}", .{cog.exec_mode});

                var pc: u32 = cog.pc;
                _ = zgui.inputScalar("PC", u32, .{
                    .v = &pc,
                    .flags = .{ .chars_hexadecimal = true },
                });

                _ = zgui.checkbox("C##cflag", .{ .v = &cog.c });
                zgui.sameLine(.{});
                _ = zgui.checkbox("Z##zflag", .{ .v = &cog.z });

                zgui.separator();

                _ = zgui.checkbox("LUT Sharing", .{ .v = &cog.lut_sharing });

                if (zgui.collapsingHeader("Registers", .{})) {
                    if (zgui.beginTable("Registers", .{
                        .column = 4,
                        .outer_size = .{ 0, 300 },
                        .flags = .{
                            .scroll_y = true,
                            .row_bg = true,
                            .borders = .all,
                            .resizable = true,
                        },
                    })) {
                        defer zgui.endTable();

                        // Keep header visible while scrolling
                        zgui.tableSetupScrollFreeze(0, 1);

                        zgui.tableSetupColumn("ID", .{});
                        zgui.tableSetupColumn("Name", .{});
                        zgui.tableSetupColumn("Value", .{ .flags = .{ .width_stretch = true } });
                        zgui.tableSetupColumn("Hint", .{});
                        zgui.tableHeadersRow();
                        for (0..512) |reg_id| {
                            zgui.pushIntId(@intCast(cog_id));
                            defer zgui.popId();

                            const reg: windtunnel.types.Register = @enumFromInt(@as(u9, @intCast(reg_id)));

                            zgui.tableNextRow(.{});

                            _ = zgui.tableSetColumnIndex(0);
                            zgui.text("{d}", .{reg_id});

                            if (reg_id >= 0x1F0) {
                                _ = zgui.tableSetColumnIndex(1);
                                zgui.text("{t}", .{reg});
                            }

                            _ = zgui.tableSetColumnIndex(2);
                            zgui.text("0x{X:0>8}", .{cog.registers.get(reg)});

                            _ = zgui.tableSetColumnIndex(3);
                            zgui.text("{s}", .{
                                if (cog.exec_mode == .cog and cog.pc == @intFromEnum(reg))
                                    "PC"
                                else
                                    "",
                            });
                            zgui.sameLine(.{});

                            var breakpoint: bool = false;
                            _ = zgui.checkbox("BRK", .{ .v = &breakpoint });
                        }
                    }
                }

                if (zgui.collapsingHeader("Stack", .{})) {
                    for (0..8) |level| {
                        zgui.text("[{}] = 0x{X:0>8}", .{
                            level,
                            cog.stack[level],
                        });
                    }
                }

                if (zgui.collapsingHeader("Events", .{})) {
                    for (0..16) |event_index| {
                        const event: windtunnel.types.EventId = @enumFromInt(@as(u4, @intCast(event_index)));
                        const name = std.fmt.bufPrintSentinel(&temp_buf, "{t}##event.{}", .{ event, event_index }, 0) catch unreachable;

                        if (event_index % 4 != 0) {
                            zgui.sameLine(.{});
                        }

                        const clicked = zgui.selectable(name, .{
                            .selected = cog.events.contains(event),
                            .w = 20,
                        });
                        if (clicked) {
                            cog.events.toggle(event);
                        }
                    }
                }
            }
        }

        if (step_one) {
            gui.hub.step();
        }
    }
};

const BeginArgs = @typeInfo(@TypeOf(zgui.begin)).@"fn".params[1].type.?;
fn new_window(title: [:0]const u8, args: BeginArgs) bool {
    const visible = zgui.begin(title, args);
    if (!visible) {
        zgui.end();
    }
    return visible;
}
