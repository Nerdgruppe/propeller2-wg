const std = @import("std");
const builtin = @import("builtin");

const frontend = @import("frontend.zig");
const sema = @import("sema.zig");
const emit = @import("emit.zig");
const stdlib = @import("stdlib/stdlib.zig");
const Module = @import("Module.zig");

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
    format: emit.BinaryFormat = .flat,
    @"fill-byte": u8 = 0x00,

    pub const shorthands = .{
        .h = "help",
        .o = "output",
        .v = "verbose",
        .f = "format",
        .F = "fill-byte",
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
            .format = "Selects the binary format to use",
            .@"fill-byte" = "The byte value which is used to fill empty/undefined space in the binary. Defaults to 0x00.",
            .@"test-mode" = "<internal use only>",
            .@"compare-to" = "<internal use only>",
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
        var buffer: [4096]u8 = undefined;
        var stdout = std.fs.File.stdout().writer(&buffer);
        try args_parser.printHelp(
            CliArgs,
            cli.executable_name orelse "propan",
            &stdout.interface,
        );
        try stdout.interface.flush();
        return 0;
    }

    const output_format = cli.options.format;
    if (output_format.is_binary() and cli.options.output.len == 0) {
        try usage_mistake("Cannot emit {s} to stdio. Use \"-o -\" to force emission to stdout.", .{@tagName(output_format)});
    }

    if (cli.positionals.len == 0) {
        try usage_mistake("missing input files.", .{});
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

        parsed_file.* = parser.parse(allocator) catch |err| switch (err) {
            error.Overflow,
            error.InvalidUtf8,
            error.Utf8CannotEncodeSurrogateHalf,
            error.CodepointTooLarge,
            error.UnexpectedToken,
            error.UnexpectedCharacter,
            error.UnexpectedEndOfFile,
            error.SyntaxError,
            error.InvalidCharacter,
            error.InvalidFlag,
            => return 1,

            error.OutOfMemory => |e| return e,
        };
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

    // this compile without exit code 1!
    // TODO: ADDCT1 tmp, ticks(CLK, us=15000)

    var mod: ?Module = null;
    for (cli.positionals, loaded_files) |input_path, parsed_file| {
        std.log.debug("analyzing {s}...", .{input_path});

        var module = try sema.analyze(allocator, parsed_file.file, .{
            .blank_pointer_expr = .as_ptr_epxr,
        });
        errdefer module.deinit();

        mod = module;

        for (module.segments) |segment| {
            const previous_end = output.items.len;
            try output.resize(allocator, @max(output.items.len, segment.hub_offset + segment.data.len));
            std.debug.assert(output.items.len >= segment.hub_offset + segment.data.len);

            // fill newly created data with the user-defined fill byte:
            @memset(output.items[previous_end..], cli.options.@"fill-byte");

            // then insert the segments data:
            @memcpy(output.items[segment.hub_offset..][0..segment.data.len], segment.data);
        }

        std.log.info("sema yielded {} segments:", .{module.segments.len});

        for (module.segments, 0..) |seg, seg_i| {
            std.log.info("  [{}]: offset 0x{X:0>6}, length {} bytes", .{ seg_i, seg.hub_offset, seg.data.len });

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

        var buffer: [4096]u8 = undefined;
        var stdout_writer = std.fs.File.stdout().writer(&buffer);

        const writer = &stdout_writer.interface;

        try writer.print("OUTPUT DOES NOT MATCH '{s}'.\n", .{
            cli.options.@"compare-to",
        });

        if (ref.len == output.items.len) {
            try writer.print("binary length: {}\n", .{ref.len});
        } else {
            try writer.print("expected binary length: {}\n", .{
                ref.len,
            });
            try writer.print("actual binary length:   {}\n", .{
                output.items.len,
            });
        }

        try writer.print("<diff>\n", .{});
        try render_bin_diff(
            writer,
            ref,
            output.items,
            mod.?,
        );
        try writer.print("</diff>\n", .{});

        try writer.flush();

        return 1;
    }

    if (output_format == .none)
        return 0;

    if (cli.options.output.len > 0 and !std.mem.eql(u8, cli.options.output, "-")) {
        var buffer: [4096]u8 = undefined;
        var file = try std.fs.cwd().atomicFile(cli.options.output, .{
            .write_buffer = &buffer,
        });
        defer file.deinit();

        try emit.emit(allocator, file.file_writer.file, mod.?, output_format);

        try file.finish();
    } else {
        try emit.emit(allocator, std.fs.File.stdout(), mod.?, output_format);
    }

    return 0;
}

fn usage_mistake(comptime fmt: []const u8, args: anytype) !noreturn {
    var buffer: [256]u8 = undefined;
    var writer = std.fs.File.stdout().writerStreaming(&buffer);

    try writer.interface.print("usage error: " ++ fmt ++ "\n", args);
    try writer.interface.flush();

    std.process.exit(1);
}

test {
    _ = frontend;
    _ = sema;
}

fn render_bin_diff(writer: *std.Io.Writer, expected_data: []const u8, actual_data: []const u8, mod: ?Module) !void {
    const common_len = @max(expected_data.len, actual_data.len);

    std.log.info("{}", .{common_len});

    var fbs_exp = std.io.fixedBufferStream(expected_data);
    var fbs_act = std.io.fixedBufferStream(actual_data);

    while (fbs_exp.pos < expected_data.len or fbs_act.pos < actual_data.len) {
        const offset: u32 = @intCast(@max(fbs_exp.pos, fbs_act.pos));

        const expected = fbs_exp.reader().readInt(u32, .little) catch 0;
        const actual = fbs_act.reader().readInt(u32, .little) catch 0;

        // const expected: ?u8 = if (offset < expected_data.len) expected_data[offset] else null;
        // const actual: ?u8 = if (offset < actual_data.len) actual_data[offset] else null;

        if (actual == expected)
            continue;

        const instr_groups: []const u32 = &.{ 9, 9, 3, 7, 4 };
        try writer.print("@{X:0>5}: expected: 0x{X:0>8} ({s}), actual: 0x{X:0>8} ({s}) | {?f}\n", .{
            offset,
            expected,
            bitdiff(u32, expected, actual, instr_groups),
            actual,
            bitdiff(u32, actual, expected, instr_groups),
            if (mod) |m| m.line_for_address(offset) else null,
        });
        var dis_buf: [256]u8 = undefined;
        try writer.print("  expected: {!s}\n", .{disasm(&dis_buf, expected)});
        try writer.print("  actual:   {!s}\n", .{disasm(&dis_buf, actual)});
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

fn disasm(buffer: []u8, encoded: u32) ![]const u8 {
    var fbs = std.io.fixedBufferStream(buffer);
    const writer = fbs.writer();

    for (stdlib.p2.instructions) |instr| {
        var ignore_mask: u32 = 0xF000_0000; // we mask the condition by default

        for (instr.operands) |op| {
            ignore_mask |= op.slot.mask();
            switch (op.type) {
                .address => |meta| ignore_mask |= meta.rel.mask(),
                .reg_or_imm => |meta| ignore_mask |= meta.imm.mask(),
                .pointer_expr => |meta| ignore_mask |= meta.imm.mask(),
                .register, .immediate, .pointer_reg, .enumeration => {},
            }
        }
        if (instr.c_effect_slot) |slot| ignore_mask |= slot.mask();
        if (instr.z_effect_slot) |slot| ignore_mask |= slot.mask();

        if ((encoded & ~ignore_mask) != instr.binary) {
            continue;
        }

        const cond: frontend.ast.Condition.Code = @enumFromInt(encoded >> 28);
        if (encoded != 0) {
            try writer.print("{s} {s}", .{ condition_str(cond), instr.mnemonic });
        } else {
            try writer.print("{s} {s}", .{ condition_str(.always), instr.mnemonic });
        }

        for (instr.operands, 0..) |op, i| {
            if (i > 0) {
                try writer.writeAll(",");
            }
            try writer.writeAll(" ");

            const Decorator = enum { none, imm, abs };

            const decorator: Decorator = switch (op.type) {
                .address => |meta| if (meta.rel.read(encoded) != 0) .imm else .abs,
                .reg_or_imm => |meta| if (meta.imm.read(encoded) != 0) .imm else .none,
                .pointer_expr => |meta| if (meta.imm.read(encoded) != 0) .imm else .none,
                .immediate => .imm,
                .register, .pointer_reg, .enumeration => .none,
            };

            switch (decorator) {
                .none => {},
                .imm => try writer.writeAll("#"),
                .abs => try writer.writeAll("#\\"),
            }

            const slotval = op.slot.read(encoded);

            try writer.print("0x{X}", .{slotval});
        }
        break;
    }

    return fbs.getWritten();
}

fn condition_str(cond: frontend.ast.Condition.Code) []const u8 {
    return switch (cond) {
        .@"return" => "return     ",
        .if_c => "if(C)      ",
        .if_nc => "if(!C)     ",
        .if_z => "if(Z)      ",
        .if_nz => "if(!Z)     ",
        .if_c_eq_z => "if(C == Z) ",
        .if_c_ne_z => "if(C != Z) ",
        .if_nc_and_nz => "if(!C & !Z)",
        .if_nc_and_z => "if(!C & Z) ",
        .if_c_and_z => "if(C & Z)  ",
        .if_c_and_nz => "if(C & !Z) ",
        .if_nc_or_nz => "if(!C | !Z)",
        .if_nc_or_z => "if(!C | Z) ",
        .if_c_or_nz => "if(C | !Z) ",
        .if_c_or_z => "if(C | Z)  ",
        .always => "           ",
    };
}
