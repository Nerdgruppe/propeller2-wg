//!
//! This file is the entry point for Windtunnel, a Propeller 2 simulator.
//!

const std = @import("std");

const args_parser = @import("args");

pub const types = @import("p2").types;

const disassembly = @import("sim/disassemble.zig");
pub const disassemble = disassembly.disassemble;
pub const Frame = @import("sim/Frame.zig");
pub const inspect = @import("sim/inspect.zig");
pub const Debugger = @import("sim/Debugger.zig");
pub const Cordic = @import("sim/Cordic.zig");
pub const Streamer = @import("sim/Streamer.zig");
pub const ColorConverter = @import("sim/ColorConverter.zig");

pub const Hub = @import("sim/Hub.zig");
pub const Cog = @import("sim/Cog.zig");
pub const IO = @import("sim/IO.zig");
pub const Pad = @import("sim/Pad.zig");
pub const Pads = @import("sim/Pads.zig");
const Vcd = @import("sim/Vcd.zig");
pub const SystemConfiguration = @import("sim/SystemConfiguration.zig");
pub const ClockConfiguration = @import("sim/ClockConfiguration.zig");

pub const SmartPin = smart_pin.SmartPin;

pub const smart_pin = @import("sim/smart_pin.zig");

const p2aas_server = @import("p2aas_server.zig");
pub const serial_boot = @import("serial_boot.zig");
const Terminal = @import("Terminal.zig");

pub const std_options: std.Options = .{
    .log_scope_levels = &.{},
    .log_level = .debug,
    .logFn = writeLog,
};

pub const SimulationProfile = enum { ideal, realistic };
const Oscillator = ClockConfiguration.Oscillator;

const CliArgs = struct {
    help: bool = false,
    verbose: bool = false,
    serve: bool = false,
    boot: bool = false,
    baudrate: u32 = 115_200,
    sim: SimulationProfile = .ideal,
    rcfast: ?Oscillator = null,
    rcslow: ?Oscillator = null,
    crystal: ?Oscillator = null,
    seed: ?u64 = null,
    oversample: u32 = 1,
    url: []const u8 = "ws://127.0.0.1:21591/",
    @"trace-pipeline": bool = false,
    @"fast-io": bool = false,
    image: []const u8 = "",
    vcd: []const u8 = "",

    pub const shorthands = .{
        .h = "help",
        .v = "verbose",
        .i = "image",
    };

    pub const meta = .{
        .usage_summary = "[-h] [-v] [--sim ideal|realistic] [--rcfast CLOCK] [--rcslow CLOCK] [--crystal CLOCK] [--seed N] [--oversample N] [--fast-io] [--trace-pipeline] [--vcd PATH] [--boot] [--baudrate N] [-i IMAGE | --serve [--url URL]]",

        .full_text =
        \\Windtunnel is a cycle-exact simulator for the Parallax Propeller 2.
        ,

        .option_docs = .{
            .help = "Prints this help text",
            .verbose = "Enables debug logging",
            .serve = "Serve the P2AAS upload and terminal protocol",
            .boot = "Start the ROM; with --image, download the image through its serial loader",
            .baudrate = "Serial terminal/upload baud rate, 9600..2000000 (default 115200)",
            .sim = "Electrical and clock model: ideal typical values (default), or realistic bounded drift/measured PLL profiles",
            .rcfast = "RCFAST: MHz[-LOW+HIGH|+RANGE]; ppm/ppb/%/Hz/kHz/MHz accepted (typical 24 MHz, realistic 20..30)",
            .rcslow = "RCSLOW: same syntax, bare values in MHz (typical 0.020 MHz, realistic 12..30 kHz)",
            .crystal = "XI frequency: same syntax (default fixed 20 MHz; specify its actual tolerance explicitly)",
            .seed = "Drift RNG seed; omitted generates entropy, selected seed is printed to stderr",
            .oversample = "Analog samples per system clock (positive integer, default 1); VCD records intermediate pads",
            .url = "Listen URL for --serve (default ws://127.0.0.1:21591/)",
            .@"trace-pipeline" = "Write up to 10000 pipeline stage events to stderr",
            .@"fast-io" = "Use virtual image-run TX (server: RX/TX); interactive input and ROM sessions retain UART timing",
            .image = "The image file which contains the hub data.",
            .vcd = "Export digital and analog signals with physical simulation timestamps",
        },
    };
};

pub const ClockOptions = struct {
    rcfast: ?Oscillator,
    rcslow: ?Oscillator,
    crystal: ?Oscillator,
    seed: ?u64,
};

/// Resolve source overrides and entropy once at process startup; always disclose the replay seed.
pub fn configureClocks(init: std.process.Init, configuration: *SystemConfiguration, options: ClockOptions) void {
    inline for (.{ "rcfast", "rcslow", "crystal" }) |name| {
        if (@field(options, name)) |value| {
            @field(configuration.clocks, name) = value;
        }
    }
    configuration.clocks.seed = options.seed orelse blk: {
        var seed: u64 = undefined;
        std.Io.random(init.io, std.mem.asBytes(&seed));
        break :blk seed;
    };
    std.log.info("clock seed: {d} (--seed {d})", .{ configuration.clocks.seed, configuration.clocks.seed });
}

/// Parse CLI options and run the simulator, test suite, or WebSocket server.
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

    if (cli.options.vcd.len != 0 and cli.options.serve) {
        std.log.err("--vcd cannot be combined with --serve", .{});
        return 1;
    }

    if (cli.options.boot and cli.options.serve) {
        std.log.err("--boot cannot be combined with --serve", .{});
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
    configureClocks(init, &system_configuration, .{
        .rcfast = cli.options.rcfast,
        .rcslow = cli.options.rcslow,
        .crystal = cli.options.crystal,
        .seed = cli.options.seed,
    });

    if (cli.options.serve) {
        if (cli.options.image.len != 0) {
            std.log.err("--serve cannot be combined with --image", .{});
            return 1;
        }
        p2aas_server.serve(init.gpa, init.io, cli.options.url, .{
            .trace = cli.options.@"trace-pipeline",
            .fast_io = cli.options.@"fast-io",
            .system_configuration = system_configuration,
        }) catch |err| {
            std.log.err("P2AAS server failed: {t}", .{err});
            return 1;
        };
        return 0;
    }

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

    var terminal_sink: smart_pin.DataSink = .{ .live_writer = &stdout.interface };
    hub.io.pins[62].smart.tx_mode = if (cli.options.@"fast-io" and cli.options.image.len != 0)
        .{ .virtual = &terminal_sink }
    else
        .{ .emulated = &terminal_sink };

    var trace_buffer: [4096]u8 = undefined;
    var trace = std.Io.File.stderr().writer(init.io, &trace_buffer);
    defer trace.interface.flush() catch {};

    if (cli.options.@"trace-pipeline") {
        hub.trace_writer = &trace.interface;
    }

    // Serial boot must retain the image outside RAM while the ROM builds its lookup table there.
    const image_buffer = if (boot and cli.options.image.len != 0)
        try init.gpa.alloc(u8, hub.memory.len)
    else
        &hub.memory;
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
        std.debug.assert(count <= hub.memory.len);
        image_length = count;

        if (stat.size > count) {
            if (boot) {
                std.log.err("serial boot image exceeds hub RAM", .{});
                return 1;
            }
            std.log.warn("hub image exceeds hub size. expected {Bi:.3} or less bytes, but got {Bi:.3}", .{
                hub.memory.len,
                stat.size,
            });
        } else {
            std.log.info("read {Bi:.2} image", .{
                count,
            });
        }
    }

    const vcd_file: ?std.Io.File = if (cli.options.vcd.len != 0)
        try std.Io.Dir.cwd().createFile(init.io, cli.options.vcd, .{})
    else
        null;
    defer {
        if (vcd_file) |file| {
            file.close(init.io);
        }
    }

    var vcd_buffer: [4096]u8 = undefined;
    var vcd_writer = if (vcd_file) |file|
        file.writer(init.io, &vcd_buffer)
    else
        undefined;
    var vcd: Vcd = .{ .writer = &vcd_writer.interface };
    defer {
        if (vcd_file != null) {
            vcd.finish() catch {};
        }
    }

    if (vcd_file != null) {
        try hub.attachVcd(&vcd);
    }
    const raw = try Terminal.Raw.init();
    defer raw.deinit();

    var terminal: Terminal = .{ .io = init.io, .allocator = init.gpa };
    defer terminal.deinit();

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
    const result = switch (try select.await()) {
        .simulation => |result| try result,
        .input => |ended| blk: {
            if (try ended == .quit)
                return 0;
            // EOF disconnects host input; let queued bytes and the application finish.
            break :blk switch (try select.await()) {
                .simulation => |result| try result,
                .input => unreachable,
            };
        },
    };
    select.cancelDiscard();

    if (vcd_file != null) {
        try vcd.finish();
    }

    stdout.interface.flush() catch |err| {
        std.log.err("writing stdout failed: {t}", .{err});
        return 1;
    };

    std.log.warn("all cogs stopped after {} clocks. halting...", .{
        hub.counter,
    });

    return result;
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
