//! Interactive terminal and image runner for the Windtunnel simulator.
const std = @import("std");
const args_parser = @import("args");
const windtunnel = @import("windtunnel");
const Hub = windtunnel.Hub;
const Capture = windtunnel.Capture;
const SystemConfiguration = windtunnel.SystemConfiguration;
const ClockConfiguration = windtunnel.ClockConfiguration;
const smart_pin = windtunnel.smart_pin;
const serial_boot = windtunnel.serial_boot;
const Terminal = @import("Terminal.zig");

comptime {
    _ = Terminal;
}

pub const std_options: std.Options = .{
    .log_scope_levels = &.{},
    .log_level = .debug,
    .logFn = writeLog,
};

const SimulationProfile = enum { ideal, realistic };
const Oscillator = ClockConfiguration.Oscillator;

const CliArgs = struct {
    help: bool = false,
    verbose: bool = false,
    boot: bool = false,
    baudrate: u32 = 115_200,
    sim: SimulationProfile = .ideal,
    rcfast: ?Oscillator = null,
    rcslow: ?Oscillator = null,
    crystal: ?Oscillator = null,
    seed: ?u64 = null,
    oversample: u32 = 1,
    @"fast-io": bool = false,
    image: []const u8 = "",
    scope: []const u8 = "",
    @"scope-memory": usize = Capture.default_memory / (1024 * 1024),
    @"scope-overflow": Capture.Overflow = .stop,

    pub const shorthands = .{
        .h = "help",
        .v = "verbose",
        .i = "image",
    };

    pub const meta = .{
        .usage_summary = "[-h] [-v] [--sim ideal|realistic] [--rcfast CLOCK] [--rcslow CLOCK] [--crystal CLOCK] [--seed N] [--oversample N] [--fast-io] [--scope PATH] [--boot] [--baudrate N] [-i IMAGE]",

        .full_text =
        \\Windtunnel is a cycle-exact simulator for the Parallax Propeller 2.
        ,

        .option_docs = .{
            .help = "Prints this help text",
            .verbose = "Enables debug logging",
            .boot = "Start the ROM; with --image, download the image through its serial loader",
            .baudrate = "Serial terminal/upload baud rate, 9600..2000000 (default 115200)",
            .sim = "Electrical and clock model: ideal typical values (default), or realistic bounded drift/measured PLL profiles",
            .rcfast = "RCFAST: MHz[-LOW+HIGH|+RANGE]; ppm/ppb/%/Hz/kHz/MHz accepted (typical 24 MHz, realistic 20..30)",
            .rcslow = "RCSLOW: same syntax, bare values in MHz (typical 0.020 MHz, realistic 12..30 kHz)",
            .crystal = "XI frequency: same syntax (default fixed 20 MHz; specify its actual tolerance explicitly)",
            .seed = "Drift RNG seed; omitted generates entropy, selected seed is printed to stderr",
            .oversample = "Analog samples per system clock (positive integer, default 1); Scope records intermediate pads",
            .@"fast-io" = "Use virtual image-run TX; interactive input and ROM sessions retain UART timing",
            .image = "The image file which contains the hub data.",
            .scope = "Save all pin streams to a native Scope block image",
            .@"scope-memory" = "Scope memory in MiB, minimum 2 (default 256)",
            .@"scope-overflow" = "Scope retention: rolling or stop (default stop)",
        },
    };
};

/// Save a retained Scope recording without exposing host file operations to simulator stepping.
fn saveCapture(capture: *const Capture, io: std.Io, path: []const u8) !void {
    const file = try std.Io.Dir.cwd().createFile(io, path, .{});
    defer file.close(io);

    var buffer: [4096]u8 = undefined;
    var output = file.writer(io, &buffer);
    try capture.writeNative(&output.interface);
}

/// Parse terminal options, load an image and run the interactive simulation.
pub fn main(init: std.process.Init) !u8 {
    var cli = args_parser.parseForCurrentProcess(CliArgs, init, .print) catch return 1;
    defer cli.deinit();

    if (cli.options.verbose) {
        global_log_level = .debug;
    }

    if (cli.options.help) {
        var buffer: [256]u8 = undefined;
        var stdout = std.Io.File.stdout().writer(init.io, &buffer);
        try args_parser.printHelp(
            CliArgs,
            cli.executable_name orelse "windtunnel",
            &stdout.interface,
        );
        try stdout.interface.flush();
        return 0;
    }
    if (cli.positionals.len != 0) {
        var buffer: [256]u8 = undefined;
        var stderr = std.Io.File.stderr().writer(init.io, &buffer);
        try args_parser.printHelp(
            CliArgs,
            cli.executable_name orelse "windtunnel",
            &stderr.interface,
        );
        try stderr.interface.flush();
        return 1;
    }

    const scope_requested = cli.options.scope.len != 0;
    if (!scope_requested and (cli.options.@"scope-memory" != 256 or cli.options.@"scope-overflow" != .stop)) {
        std.log.err("Scope options require --scope", .{});
        return 1;
    }
    const scope_memory = std.math.mul(usize, cli.options.@"scope-memory", 1024 * 1024) catch {
        std.log.err("Scope memory size overflows the host address space", .{});
        return 1;
    };
    if (scope_requested and scope_memory < Capture.minimum_memory) {
        std.log.err("Scope memory must be at least 2 MiB", .{});
        return 1;
    }
    if (cli.options.oversample == 0) {
        std.log.err("--oversample must be a positive integer", .{});
        return 1;
    }
    var system_configuration: SystemConfiguration = switch (cli.options.sim) {
        .ideal => .ideal,
        .realistic => .realistic,
    };
    system_configuration.oversample = cli.options.oversample;
    inline for (.{ "rcfast", "rcslow", "crystal" }) |name| {
        if (@field(cli.options, name)) |value| {
            @field(system_configuration.clocks, name) = value;
        }
    }
    system_configuration.clocks.seed = cli.options.seed orelse blk: {
        var seed: u64 = undefined;
        std.Io.random(init.io, std.mem.asBytes(&seed));
        break :blk seed;
    };
    std.log.info("clock seed: {d} (--seed {d})", .{
        system_configuration.clocks.seed,
        system_configuration.clocks.seed,
    });

    if (cli.options.baudrate < 9_600 or cli.options.baudrate > 2_000_000) {
        std.log.err("--baudrate must be between 9600 and 2000000", .{});
        return 1;
    }
    const boot = cli.options.boot or cli.options.image.len == 0;

    var stdout_buffer: [4096]u8 = undefined;
    var stdout = std.Io.File.stdout().writer(init.io, &stdout_buffer);
    defer stdout.interface.flush() catch {};

    var hub: Hub = undefined;
    try hub.init(system_configuration);

    if (!boot) {
        serial_boot.prepare(&hub, cli.options.baudrate);
    }

    var terminal_sink: smart_pin.DataSink = .{ .endpoint = .{ .live_writer = &stdout.interface } };
    hub.io.pins[62].smart.tx_mode = if (cli.options.@"fast-io" and cli.options.image.len != 0)
        .{ .virtual = &terminal_sink }
    else
        .{ .emulated = &terminal_sink };

    // Serial boot must retain the image outside RAM while the ROM builds its lookup table there.
    const image_buffer = if (boot and cli.options.image.len != 0)
        try init.gpa.alloc(u8, hub.ram.bytes().len)
    else
        hub.ram.bytes();
    defer {
        if (boot and cli.options.image.len != 0) {
            init.gpa.free(image_buffer);
        }
    }

    var image_length: usize = 0;
    if (cli.options.image.len != 0) {
        var file = try std.Io.Dir.cwd().openFile(init.io, cli.options.image, .{});
        defer file.close(init.io);

        const stat = try file.stat(init.io);

        const count = try file.readPositionalAll(init.io, image_buffer, 0);
        std.debug.assert(count <= hub.ram.bytes().len);
        image_length = count;

        if (stat.size > count) {
            if (boot) {
                std.log.err("serial boot image exceeds hub RAM", .{});
                return 1;
            }
            std.log.warn("hub image exceeds hub size. expected {Bi:.3} or less bytes, but got {Bi:.3}", .{
                hub.ram.bytes().len,
                stat.size,
            });
        } else {
            std.log.info("read {Bi:.2} image", .{
                count,
            });
        }
    }

    var capture: ?Capture = if (scope_requested)
        try Capture.init(init.gpa, scope_memory, cli.options.@"scope-overflow", hub.io.captureState(hub.io.pads))
    else
        null;
    defer {
        if (capture) |*recording| {
            hub.capture = null;
            recording.deinit();
        }
    }
    if (capture) |*recording| {
        hub.attachCapture(recording);
    }
    var terminal: Terminal = .{
        .io = init.io,
        .allocator = init.gpa,
        .output = &stdout.interface,
        .sink = &terminal_sink,
        .capture = if (capture) |*recording|
            recording
        else
            null,
    };
    defer terminal.deinit();

    const result: anyerror!u8 = session: {
        const raw = try Terminal.Raw.init();
        defer raw.deinit();
        defer {
            stdout.interface.writeAll("\r\n") catch {};
            stdout.interface.flush() catch {};
        }

        const Select = std.Io.Select(union(enum) {
            input: anyerror!Terminal.InputEnd,
            simulation: anyerror!u8,
        });
        var events: [2]Select.Union = undefined;
        var select = Select.init(init.io, &events);
        defer select.cancelDiscard();

        try select.concurrent(.input, Terminal.read, .{&terminal});
        try select.concurrent(.simulation, Terminal.simulate, .{ &terminal, &hub, Terminal.Session{
            .boot = boot,
            .image = if (cli.options.image.len != 0)
                image_buffer[0..image_length]
            else
                null,
            .rate = cli.options.baudrate,
        } });
        const completed: anyerror!u8 = switch (try select.await()) {
            .simulation => |result| result,
            .input => |ended| blk: {
                const end = ended catch |err| break :blk err;
                if (end == .quit)
                    break :blk @as(u8, 0);
                // EOF disconnects host input; let queued bytes and the application finish.
                break :blk switch (try select.await()) {
                    .simulation => |result| result,
                    .input => unreachable,
                };
            },
        };
        break :session completed;
    };
    var export_failed = false;
    if (capture) |*recording| {
        if (recording.metadata().status == .recording) {
            recording.metadata().status = .frozen;
        }
        saveCapture(recording, init.io, cli.options.scope) catch |err| {
            std.log.err("Scope export failed: {t}", .{err});
            export_failed = true;
        };
        std.log.info("Scope {t}: retained {d}..{d} fs, {Bi:.2} / {Bi:.2}", .{
            recording.metadataConst().status, recording.metadataConst().start, recording.metadataConst().end, recording.memoryUsed(), recording.memoryCapacity(),
        });
    }
    const exit_code = result catch |err| switch (err) {
        error.CaptureFull => 0,
        else => blk: {
            std.log.err("simulation stopped: {t}", .{err});
            break :blk @as(u8, 1);
        },
    };
    if (export_failed)
        return 1;

    stdout.interface.flush() catch |err| {
        std.log.err("writing stdout failed: {t}", .{err});
        return 1;
    };

    std.log.info("simulation stopped after {} clocks", .{
        hub.counter,
    });

    return exit_code;
}

var global_log_level: std.log.Level = .info;

/// Send simulator log messages to the configured output stream.
fn writeLog(
    comptime message_level: std.log.Level,
    comptime scope: @TypeOf(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    if (@intFromEnum(message_level) > @intFromEnum(global_log_level)) {
        return;
    }
    std.log.defaultLog(message_level, scope, format, args);
}
