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
    image: []const u8 = "",

    pub const shorthands = .{
        .h = "help",
        .v = "verbose",
        .i = "image",
    };

    pub const meta = .{
        .usage_summary = "[-h] [-v]",

        .full_text =
        \\Windtunnel is a cycle-exact simulator for the Parallax Propeller 2.
        ,

        .option_docs = .{
            .help = "Prints this help text",
            .verbose = "Enables debug logging",
            .image = "The image file which contains the hub data.",
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
    if (cli.positionals.len != 0) {
        try args_parser.printHelp(
            CliArgs,
            cli.executable_name orelse "windtunnel",
            std.io.getStdErr().writer(),
        );
        return 1;
    }

    var hub: Hub = undefined;
    hub.init();

    if (cli.options.image.len != 0) {
        var file = try std.fs.cwd().openFile(cli.options.image, .{});
        defer file.close();

        const stat = try file.stat();

        const count = try file.readAll(&hub.memory);
        std.debug.assert(count <= hub.memory.len);

        if (stat.size > count) {
            std.log.warn("hub image exceeds hub size. expected {:.3} or less bytes, but got {:.3}", .{
                std.fmt.fmtIntSizeBin(hub.memory.len),
                std.fmt.fmtIntSizeBin(stat.size),
            });
        } else {
            std.log.info("loaded {:.2} into hub memory", .{
                std.fmt.fmtIntSizeBin(count),
            });
        }
    }

    try hub.start_cog(0, .{});

    while (hub.is_any_cog_active()) {
        hub.step();
    }

    std.log.warn("all cogs stopped. halting...", .{});

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
