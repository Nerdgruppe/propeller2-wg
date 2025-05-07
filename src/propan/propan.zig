const std = @import("std");
const builtin = @import("builtin");

const frontend = @import("frontend.zig");
const sema = @import("sema.zig");

const args_parser = @import("args");

pub const std_options: std.Options = .{
    .log_scope_levels = &.{},
    .log_level = .debug,
    .logFn = writeLog,
};

const TestMode = enum {
    parser,
    sema,
    compare,
};

const CliArgs = struct {
    help: bool = false,
    output: []const u8 = "",
    @"test-mode": ?TestMode = null,
    @"compare-to": []const u8 = "",
    verbose: bool = false,

    pub const shorthands = .{
        .h = "help",
        .o = "output",
        .v = "verbose",
    };

    pub const meta = .{
        .usage_summary = "[-h] [-o <output>] <sources...>",

        .full_text =
        \\Propan is an assembler for the Propeller 2 architecture.
        ,

        .option_docs = .{
            .help = "Prints this help text",
            .output = "Sets the path of the output file.",
            .verbose = "Enables debug logging",
            .@"test-mode" = "<internal use only>",
            .@"compare-to" = "<internal use only>",
        },
    };
};

pub fn main() !u8 {
    try std.io.getStdOut().writeAll("\x1B[2J\x1B[H");

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
        global_log_level = .warn;
    }
    if (cli.options.verbose) {
        global_log_level = .debug;
    }

    if (cli.options.help) {
        try args_parser.printHelp(
            CliArgs,
            cli.executable_name orelse "propan",
            std.io.getStdOut().writer(),
        );
        return 0;
    }

    const source_files = try arena.allocator().alloc([]const u8, cli.positionals.len);
    for (source_files, cli.positionals) |*buffer, input_path| {
        std.log.debug("loading {s}...", .{input_path});
        buffer.* = try std.fs.cwd().readFileAlloc(arena.allocator(), input_path, 1 << 20);
    }

    const loaded_files = try arena.allocator().alloc(frontend.ParsedFile, cli.positionals.len);
    for (cli.positionals, loaded_files, source_files, 0..) |input_path, *parsed_file, source_code, i| {
        errdefer for (loaded_files[0..i]) |*file|
            file.deinit();
        std.log.debug("parsing {s}...", .{input_path});

        var parser: frontend.Parser = .init(source_code, input_path);

        parsed_file.* = try parser.parse(allocator);
    }
    defer for (loaded_files) |*file|
        file.deinit();

    // Stop after having each file parsed successfully:
    if (cli.options.@"test-mode" == .parser)
        return 0;

    // try frontend.render.pretty_print(
    //     std.io.getStdOut().writer(),
    //     parsed_file.file,
    // );
    // try frontend.dump_ast(
    //     std.io.getStdOut().writer(),
    //     parsed_file.file,
    // );

    var output: std.ArrayListUnmanaged(u8) = .empty;
    defer output.deinit(allocator);

    for (cli.positionals, loaded_files) |input_path, parsed_file| {
        std.log.debug("analyzing {s}...", .{input_path});

        var module = try sema.analyze(allocator, parsed_file.file);
        defer module.deinit();

        for (module.segments) |segment| {
            try output.resize(allocator, @max(output.items.len, segment.hub_offset + segment.data.len));
            std.debug.assert(output.items.len >= segment.hub_offset + segment.data.len);
            @memcpy(output.items[segment.hub_offset..][0..segment.data.len], segment.data);
        }

        std.log.info("sema yielded {} segments:", .{module.segments.len});

        for (module.segments, 0..) |seg, seg_i| {
            std.log.info("  [{}]: {} bytes", .{ seg_i, seg.data.len });

            var i: usize = 0;
            const chunk_size = 16;
            while (i < seg.data.len) : (i += chunk_size) {
                const rest = seg.data[i..];
                const segment = rest[0..@min(chunk_size, rest.len)];

                var chunk_buffer: [4 * chunk_size]u8 = undefined;

                var fbs = std.io.fixedBufferStream(&chunk_buffer);
                for (segment, 0..) |byte, off| {
                    if (off > 0) {
                        try fbs.writer().writeAll(" ");
                        if ((off % 4) == 0) {
                            try fbs.writer().writeAll(" ");
                        }
                    }
                    try fbs.writer().print("{X:0>2}", .{byte});
                }

                std.log.info("    0x{X:0>5}: {s}", .{ seg.hub_offset + i, fbs.getWritten() });
            }
        }
    }

    // Stop after having each file parsed successfully:
    if (cli.options.@"test-mode" == .sema)
        return 0;

    // Stop after having each file parsed successfully:
    if (cli.options.@"test-mode" == .compare) {
        const ref = try std.fs.cwd().readFileAlloc(arena.allocator(), cli.options.@"compare-to", 512 * 1024);

        if (std.mem.eql(u8, ref, output.items)) {
            // boring case: our files are identical
            return 0;
        }

        const out = std.io.getStdOut();

        try out.writer().print("mismatch detected. expected length: {}, actual length: {}\n", .{
            ref.len,
            output.items.len,
        });

        try out.writer().print("<diff>\n", .{});
        try render_bin_diff(
            out,
            ref,
            output.items,
        );
        try out.writer().print("</diff>\n", .{});

        return 1;
    }

    return 0;
}

fn usage_mistake(comptime fmt: []const u8, args: anytype) !noreturn {
    try std.io.getStdErr().print("usage error: " ++ fmt ++ "\n", args);
    std.process.exit(1);
}

test {
    _ = frontend;
    _ = sema;
}

fn render_bin_diff(stream: std.fs.File, expected_data: []const u8, actual_data: []const u8) !void {
    const writer = stream.writer();
    const common_len = @max(expected_data.len, actual_data.len);

    std.log.info("{}", .{common_len});

    var fbs_exp = std.io.fixedBufferStream(expected_data);
    var fbs_act = std.io.fixedBufferStream(actual_data);

    while (fbs_exp.pos < expected_data.len or fbs_act.pos < expected_data.len) {
        const offset = @max(fbs_exp.pos, fbs_act.pos);

        const expected = fbs_exp.reader().readInt(u32, .little) catch 0;
        const actual = fbs_act.reader().readInt(u32, .little) catch 0;

        // const expected: ?u8 = if (offset < expected_data.len) expected_data[offset] else null;
        // const actual: ?u8 = if (offset < actual_data.len) actual_data[offset] else null;

        if (actual == expected)
            continue;

        const instr_groups: []const u32 = &.{ 9, 9, 3, 7, 4 };
        try writer.print("@{X:0>5}: expected: 0x{X:0>8} ({s}), actual: 0x{X:0>8} ({s})\n", .{
            offset,
            expected,
            bitdiff(u32, expected, actual, instr_groups),
            actual,
            bitdiff(u32, actual, expected, instr_groups),
        });
    }
}

fn bitdiff(comptime T: type, comp: T, ref: T, comptime groups: []const u32) [@bitSizeOf(T) + groups.len]u8 {
    var out: [@bitSizeOf(T) + groups.len]u8 = @splat('-');

    comptime {
        var size = 0;
        for (groups) |grp| {
            size += grp;
        }
        std.debug.assert(size == @bitSizeOf(T));
    }

    var group_id: usize = 0;
    var group_end: usize = groups[0];
    var bit_index: usize = 0;

    inline for (0..out.len) |idx| {
        const chr = &out[out.len - idx - 1];
        if (idx == group_end) {
            chr.* = ' ';
            group_id += 1;
            group_end += 1;
            if (group_id < groups.len) {
                group_end += groups[group_id];
            } else {
                std.debug.assert(group_end == out.len);
            }
        } else {
            const mask = @as(T, 1) << @intCast(bit_index);
            const bit = (comp >> @intCast(bit_index)) & 1;
            const miss = (comp & mask) != (ref & mask);

            chr.* = if (miss)
                "01"[bit]
            else
                '-';

            bit_index += 1;
        }
    }

    return out;
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
