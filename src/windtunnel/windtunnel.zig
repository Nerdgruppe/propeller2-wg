//!
//! This file is the entry point for Windtunnel, a Propeller 2 simulator.
//!

const std = @import("std");
const builtin = @import("builtin");

const args_parser = @import("args");

const Hub = @import("sim/Hub.zig");
const Cog = @import("sim/Cog.zig");

pub const std_options: std.Options = .{
    .log_scope_levels = &.{},
    .log_level = .debug,
    .logFn = writeLog,
};

const TestMode = enum {
    none,
};

const CliArgs = struct {
    help: bool = false,
    @"test-mode": ?TestMode = null,
    verbose: bool = false,

    pub const shorthands = .{
        .h = "help",
        .v = "verbose",
    };

    pub const meta = .{
        .usage_summary = "[-h] [-v]",

        .full_text =
        \\Windtunnel is a cycle-exact simulator for the Parallax Propeller 2.
        ,

        .option_docs = .{
            .help = "Prints this help text",
            .verbose = "Enables debug logging",
            .@"test-mode" = "<internal use only>",
        },
    };
};

pub fn main() !u8 {
    // try std.io.getStdOut().writeAll("\x1B[2J\x1B[H");

    var debug_allocator: std.heap.DebugAllocator(.{}) = .init;

    const allocator = if (builtin.mode == .Debug)
        debug_allocator.allocator()
    else
        std.heap.smp_allocator;

    var arena: std.heap.ArenaAllocator = .init(allocator);
    defer arena.deinit();

    var cli = args_parser.parseForCurrentProcess(CliArgs, allocator, .print) catch return 1;
    defer cli.deinit();

    if (cli.options.@"test-mode" != null) {
        global_log_level = .err; // mute warnings in test mode
    }
    if (cli.options.verbose) {
        global_log_level = .debug;
    }

    if (cli.options.help) {
        try args_parser.printHelp(
            CliArgs,
            cli.executable_name orelse "windtunnel",
            std.io.getStdOut().writer(),
        );
        return 0;
    }

    var hub: Hub = undefined;
    hub.init();

    while (true) {
        hub.step();
    }

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
