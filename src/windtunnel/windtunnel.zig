//!
//! This file is the entry point for Windtunnel, a Propeller 2 simulator.
//!

const std = @import("std");

const args_parser = @import("args");

const Hub = @import("sim/Hub.zig");

pub const std_options: std.Options = .{
    .log_scope_levels = &.{},
    .log_level = .debug,
    .logFn = writeLog,
};

const CliArgs = struct {
    help: bool = false,
    verbose: bool = false,
    @"trace-pipeline": bool = false,
    image: []const u8 = "",

    pub const shorthands = .{
        .h = "help",
        .v = "verbose",
        .i = "image",
    };

    pub const meta = .{
        .usage_summary = "[-h] [-v] [--trace-pipeline] [-i IMAGE]",

        .full_text =
        \\Windtunnel is a cycle-exact simulator for the Parallax Propeller 2.
        ,

        .option_docs = .{
            .help = "Prints this help text",
            .verbose = "Enables debug logging",
            .@"trace-pipeline" = "Write up to 10000 pipeline stage events to stderr",
            .image = "The image file which contains the hub data.",
        },
    };
};

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

    var stdout_buffer: [4096]u8 = undefined;
    var stdout = std.Io.File.stdout().writer(init.io, &stdout_buffer);
    defer stdout.interface.flush() catch {};

    var hub: Hub = undefined;
    hub.init();
    hub.output_writer = &stdout.interface;
    var trace_buffer: [4096]u8 = undefined;
    var trace = std.Io.File.stderr().writer(init.io, &trace_buffer);
    defer trace.interface.flush() catch {};
    if (cli.options.@"trace-pipeline") hub.trace_writer = &trace.interface;

    if (cli.options.image.len != 0) {
        var file = try std.Io.Dir.cwd().openFile(init.io, cli.options.image, .{});
        defer file.close(init.io);

        const stat = try file.stat(init.io);

        const count = try file.readPositionalAll(init.io, &hub.memory, 0);
        std.debug.assert(count <= hub.memory.len);

        if (stat.size > count) {
            std.log.warn("hub image exceeds hub size. expected {Bi:.3} or less bytes, but got {Bi:.3}", .{
                hub.memory.len,
                stat.size,
            });
        } else {
            std.log.info("loaded {Bi:.2} into hub memory", .{
                count,
            });
        }
    }

    try hub.start_cog(0, .{});

    while (hub.is_any_cog_active()) {
        hub.step();
        if (hub.fault) |fault| {
            std.log.err("cog {d}, pc 0x{x}, instruction 0x{x:0>8}: {s}", .{
                fault.cog, fault.pc, fault.instruction, fault.reason(),
            });
            return 1;
        }
    }

    stdout.interface.flush() catch |err| {
        std.log.err("writing stdout failed: {t}", .{err});
        return 1;
    };

    std.log.warn("all cogs stopped after {} clocks. halting...", .{
        hub.counter,
    });

    return 0;
}

var global_log_level: std.log.Level = .info;

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
