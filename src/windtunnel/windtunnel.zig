//!
//! This file is the entry point for Windtunnel, a Propeller 2 simulator.
//!

const std = @import("std");

const args_parser = @import("args");

const Hub = @import("sim/Hub.zig");
const smart_pin = @import("sim/smart_pin.zig");

pub const std_options: std.Options = .{
    .log_scope_levels = &.{},
    .log_level = .debug,
    .logFn = writeLog,
};

const CliArgs = struct {
    help: bool = false,
    verbose: bool = false,
    serve: bool = false,
    url: []const u8 = "ws://127.0.0.1:21591/",
    @"trace-pipeline": bool = false,
    image: []const u8 = "",
    vcd: []const u8 = "",

    pub const shorthands = .{
        .h = "help",
        .v = "verbose",
        .i = "image",
    };

    pub const meta = .{
        .usage_summary = "[-h] [-v] [--trace-pipeline] [--vcd PATH] [-i IMAGE | --serve [--url URL]]",

        .full_text =
        \\Windtunnel is a cycle-exact simulator for the Parallax Propeller 2.
        ,

        .option_docs = .{
            .help = "Prints this help text",
            .verbose = "Enables debug logging",
            .serve = "Serve the P2AAS upload and terminal protocol",
            .url = "Listen URL for --serve (default ws://127.0.0.1:21591/)",
            .@"trace-pipeline" = "Write up to 10000 pipeline stage events to stderr",
            .image = "The image file which contains the hub data.",
            .vcd = "Export digital signals for an image run (one VCD time unit = one system clock)",
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

    if (cli.options.vcd.len != 0 and (cli.options.serve or cli.options.image.len == 0)) {
        std.log.err("--vcd requires --image and cannot be combined with --serve", .{});
        return 1;
    }

    if (cli.options.serve) {
        if (cli.options.image.len != 0) {
            std.log.err("--serve cannot be combined with --image", .{});
            return 1;
        }
        @import("p2aas_server.zig").serve(init.gpa, init.io, cli.options.url, cli.options.@"trace-pipeline") catch |err| {
            std.log.err("P2AAS server failed: {t}", .{err});
            return 1;
        };
        return 0;
    }

    var stdout_buffer: [4096]u8 = undefined;
    var stdout = std.Io.File.stdout().writer(init.io, &stdout_buffer);
    defer stdout.interface.flush() catch {};

    var hub: Hub = undefined;
    hub.init();
    var terminal_sink: smart_pin.DataSink = .{ .writer = &stdout.interface };
    hub.io.pins[62].smart.registers.sink = &terminal_sink;
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

    const vcd_file: ?std.Io.File = if (cli.options.vcd.len != 0)
        try std.Io.Dir.cwd().createFile(init.io, cli.options.vcd, .{})
    else
        null;
    defer if (vcd_file) |file| file.close(init.io);
    var vcd_buffer: [4096]u8 = undefined;
    var vcd_writer = if (vcd_file) |file| file.writer(init.io, &vcd_buffer) else undefined;
    var vcd: @import("sim/Vcd.zig") = .{ .writer = &vcd_writer.interface };
    defer if (vcd_file != null) vcd.finish() catch {};
    if (vcd_file != null) try hub.attachVcd(&vcd);

    try hub.start_cog(0, .{});

    // One final edge aggregates stopped cogs' DIR/OUT and aborts their smart transfers.
    var final_edge = false;
    while (hub.is_any_cog_active() or !final_edge) {
        const inactive = !hub.is_any_cog_active();
        hub.step();
        final_edge = inactive;
        if (hub.fault) |fault| {
            std.log.err("cog {d}, pc 0x{x}, instruction 0x{x:0>8}: {s}", .{
                fault.cog, fault.pc, fault.instruction, fault.reason(),
            });
            return 1;
        }
        if (hub.next_idle_clock()) |next| hub.counter = next;
    }

    if (vcd_file != null) try vcd.finish();

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
