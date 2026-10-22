//! Standalone P2AAS-compatible server backed by the Windtunnel simulator.
const std = @import("std");
const args_parser = @import("args");
const windtunnel = @import("windtunnel");
const server = @import("server.zig");

pub const std_options: std.Options = .{
    .log_scope_levels = &.{},
    .log_level = .debug,
    .logFn = writeLog,
};

const SimulationProfile = enum { ideal, realistic };
const Oscillator = windtunnel.ClockConfiguration.Oscillator;

const CliArgs = struct {
    help: bool = false,
    verbose: bool = false,
    sim: SimulationProfile = .ideal,
    rcfast: ?Oscillator = null,
    rcslow: ?Oscillator = null,
    crystal: ?Oscillator = null,
    seed: ?u64 = null,
    oversample: u32 = 1,
    url: []const u8 = "ws://127.0.0.1:21591/",
    @"fast-io": bool = false,

    pub const shorthands = .{
        .h = "help",
        .v = "verbose",
    };

    pub const meta = .{
        .usage_summary = "[-h] [-v] [--url URL] [--sim ideal|realistic] [--rcfast CLOCK] [--rcslow CLOCK] [--crystal CLOCK] [--seed N] [--oversample N] [--fast-io]",

        .full_text =
        \\Serve the P2AAS upload and terminal protocol using the Windtunnel simulator.
        ,

        .option_docs = .{
            .help = "Prints this help text",
            .verbose = "Enables debug logging",
            .sim = "Electrical and clock model: ideal typical values (default), or realistic bounded drift/measured PLL profiles",
            .rcfast = "RCFAST: MHz[-LOW+HIGH|+RANGE]; ppm/ppb/%/Hz/kHz/MHz accepted (typical 24 MHz, realistic 20..30)",
            .rcslow = "RCSLOW: same syntax, bare values in MHz (typical 0.020 MHz, realistic 12..30 kHz)",
            .crystal = "XI frequency: same syntax (default fixed 20 MHz; specify its actual tolerance explicitly)",
            .seed = "Drift RNG seed; omitted generates entropy, selected seed is printed to stderr",
            .oversample = "Analog samples per system clock (positive integer, default 1)",
            .url = "Listen URL (default ws://127.0.0.1:21591/)",
            .@"fast-io" = "Use virtual RX/TX endpoints with single-clock serial throughput",
        },
    };
};

/// Parse listener and simulation options, then serve sequential simulated-board sessions.
pub fn main(init: std.process.Init) !u8 {
    var cli = args_parser.parseForCurrentProcess(CliArgs, init, .print) catch return 1;
    defer cli.deinit();

    if (cli.options.verbose) {
        global_log_level = .debug;
    }
    if (cli.options.help or cli.positionals.len != 0) {
        var buffer: [256]u8 = undefined;
        const file = if (cli.options.help)
            std.Io.File.stdout()
        else
            std.Io.File.stderr();
        var output = file.writer(init.io, &buffer);
        try args_parser.printHelp(CliArgs, cli.executable_name orelse "windtunnel-p2aas", &output.interface);
        try output.interface.flush();
        return if (cli.options.help)
            0
        else
            1;
    }
    if (cli.options.oversample == 0) {
        std.log.err("--oversample must be a positive integer", .{});
        return 1;
    }
    var system_configuration: windtunnel.SystemConfiguration = switch (cli.options.sim) {
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

    server.serve(init.gpa, init.io, cli.options.url, .{
        .fast_io = cli.options.@"fast-io",
        .system_configuration = system_configuration,
    }) catch |err| {
        std.log.err("P2AAS server failed: {t}", .{err});
        return 1;
    };
    return 0;
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
