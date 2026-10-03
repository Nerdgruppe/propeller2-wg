const std = @import("std");
const builtin = @import("builtin");

const frontend = @import("frontend.zig");
const sema = @import("sema.zig");
const emit = @import("emit.zig");
const listfile = @import("listfile.zig");
const check_list = @import("check_list.zig");
const diagnostics = @import("diagnostics.zig");
const stdlib = @import("stdlib/stdlib.zig");
const Module = @import("Module.zig");
const SourceFile = @import("SourceFile.zig");

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
    @"list-file": []const u8 = "",
    @"include-path": []const u8 = "",
    @"render-stdlib-docs": []const u8 = "",
    @"pretty-print": []const u8 = "",

    pub const shorthands = .{
        .h = "help",
        .o = "output",
        .v = "verbose",
        .f = "format",
        .F = "fill-byte",
        .I = "include-path",
    };

    pub const meta = .{
        .usage_summary = "[-h] [-I <path>] [-o <output>] <source> | --pretty-print <path>",

        .full_text =
        \\Propan is an assembler for the Propeller 2 architecture.
        ,

        .option_docs = .{
            .help = "Prints this help text",
            .output = "Sets the path of the output file.",
            .verbose = "Enables debug logging",
            .format = "Selects the binary format to use",
            .@"fill-byte" = "The byte value which is used to fill empty/undefined space in the binary. Defaults to 0x00.",
            .@"list-file" = "Writes a list file to the given path. Use '-' to write to stdout.",
            .@"include-path" = "Adds an import search path. May be specified more than once.",
            .@"render-stdlib-docs" = "Renders the standard library documentation as an HTML file",
            .@"pretty-print" = "Pretty-prints a Propan source file to stdout. Use '-' for stdin.",
            .@"test-mode" = "<internal use only>",
            .@"compare-to" = "<internal use only>",
        },
    };
};

pub fn main(init: std.process.Init) !u8 {
    const allocator = init.gpa;

    var diagnostics_collection: diagnostics.Collection = .init(allocator);
    defer diagnostics_collection.deinit();

    var diagnostic_render_options: diagnostics.RenderOptions = .{};
    defer {
        var buffer: [4096]u8 = undefined;
        var stderr_writer = std.Io.File.stderr().writer(init.io, &buffer);
        diagnostics_collection.render(&stderr_writer.interface, diagnostic_render_options) catch {};
        stderr_writer.interface.flush() catch {};
    }

    var cli = args_parser.parseForCurrentProcess(CliArgs, init, .print) catch return 1;
    defer cli.deinit();

    if (cli.options.@"render-stdlib-docs".len > 0) {
        var file = if (std.mem.eql(u8, cli.options.@"render-stdlib-docs", "-"))
            std.Io.File.stdout()
        else
            try std.Io.Dir.cwd().createFile(init.io, cli.options.@"render-stdlib-docs", .{});
        defer file.close(init.io);

        var buffer: [8192]u8 = undefined;
        var fileWriter = file.writer(init.io, &buffer);

        try stdlib.render.write_html(
            &fileWriter.interface,
            stdlib.p2.constants,
            stdlib.p2.functions,
        );

        try fileWriter.interface.flush();
        return 0;
    }

    if (cli.options.@"test-mode" != null) {
        global_log_level = .err; // mute warnings in test mode
        diagnostic_render_options.include_warnings = false;
        diagnostic_render_options.include_infos = false;
    }
    if (cli.options.verbose) {
        global_log_level = .debug;
    }

    if (cli.options.help) {
        var buffer: [4096]u8 = undefined;
        var stdout = std.Io.File.stdout().writer(init.io, &buffer);
        try args_parser.printHelp(
            CliArgs,
            cli.executable_name orelse "propan",
            &stdout.interface,
        );
        try stdout.interface.flush();
        return 0;
    }

    if (cli.options.@"pretty-print".len > 0) {
        if (cli.positionals.len != 0)
            return try usage_mistake(&diagnostics_collection, .err_multiple_input_files_are_not_supported);

        const path = cli.options.@"pretty-print";
        const source = if (std.mem.eql(u8, path, "-")) blk: {
            var buffer: [8192]u8 = undefined;
            var reader = std.Io.File.stdin().reader(init.io, &buffer);
            var contents: std.Io.Writer.Allocating = .init(init.arena.allocator());
            _ = try reader.interface.streamRemaining(&contents.writer);
            break :blk try contents.toOwnedSlice();
        } else try std.Io.Dir.cwd().readFileAlloc(init.io, path, init.arena.allocator(), .limited(1 << 20));

        var parser: frontend.Parser = .init(source, path, &diagnostics_collection);
        var parsed = parser.parse(init.arena.allocator()) catch |err| switch (err) {
            error.OutOfMemory => return err,
            else => return 1,
        };
        defer parsed.deinit();

        var rendered: std.Io.Writer.Allocating = .init(init.arena.allocator());
        try frontend.render.pretty_print_alloc(init.arena.allocator(), &rendered.writer, parsed.file);
        try std.Io.File.stdout().writeStreamingAll(init.io, rendered.written());
        return 0;
    }

    const output_format = cli.options.format;
    if (output_format.is_binary() and cli.options.output.len == 0) {
        return try usage_mistake(&diagnostics_collection, .{
            .err_usage_cannot_emit_to_stdio = .{
                .format = output_format,
            },
        });
    }

    if (cli.positionals.len == 0) {
        return try usage_mistake(&diagnostics_collection, .err_usage_missing_input_files);
    }
    if (cli.positionals.len > 1) {
        return try usage_mistake(&diagnostics_collection, .err_multiple_input_files_are_not_supported);
    }

    const include_paths = try collect_include_paths(init);
    const input_path = cli.positionals[0];
    // Index zero is the source directory; include paths follow in CLI order.
    const dirs = try init.arena.allocator().alloc(std.Io.Dir, include_paths.len + 1);
    dirs[0] = try std.Io.Dir.cwd().openDir(init.io, std.fs.path.dirname(input_path) orelse ".", .{});
    var opened_dirs: usize = 1;
    defer for (dirs[0..opened_dirs]) |dir| dir.close(init.io);
    for (include_paths, 1..) |path, index| {
        dirs[index] = std.Io.Dir.cwd().openDir(init.io, path, .{}) catch |err| {
            try diagnostics_collection.emit_diag(null, .{ .err_cannot_open_include_path = .{ .path = path, .reason = err } });
            return 1;
        };
        opened_dirs += 1;
    }

    const source_file = try init.arena.allocator().create(SourceFile);
    source_file.path = try init.arena.allocator().dupe(u8, input_path);
    source_file.dir_index = 0;
    source_file.relative_path = std.fs.path.basename(source_file.path);
    source_file.identity = try std.fmt.allocPrint(init.arena.allocator(), "0:{s}", .{source_file.relative_path});
    source_file.text = blk: {
        if (std.mem.eql(u8, input_path, "-")) {
            std.log.debug("loading stdin...", .{});
            var buf: [8192]u8 = undefined;

            var reader = std.Io.File.stdin().reader(init.io, &buf);

            var writer: std.Io.Writer.Allocating = .init(init.arena.allocator());
            defer writer.deinit();

            _ = try reader.interface.streamRemaining(&writer.writer);

            break :blk try writer.toOwnedSlice();
        } else {
            std.log.debug("loading {s}...", .{input_path});

            break :blk try dirs[0].readFileAlloc(init.io, source_file.relative_path, init.arena.allocator(), .limited(1 << 20));
        }
    };
    try diagnostics_collection.register_source_file(source_file);

    var check_list_value: ?check_list.List = if (cli.options.@"test-mode" != null)
        try check_list.parse(allocator, source_file.path, source_file.text, &diagnostics_collection)
    else
        null;
    defer if (check_list_value) |*list| list.deinit();
    if (diagnostics_collection.has_errors()) return 1;
    const diagnostic_start = diagnostics_collection.diagnostics.items.len;

    var expander = frontend.imports.Expander.init(init.arena.allocator(), init.io, &diagnostics_collection, dirs, include_paths);
    defer expander.deinit();
    const ast_file = expander.expand(source_file) catch |err| switch (err) {
        error.OutOfMemory => return err,
        else => return try diagnostic_status(&diagnostics_collection, check_list_value, diagnostic_start),
    };
    if (cli.options.@"test-mode" == .parser or diagnostics_collection.has_errors())
        return try diagnostic_status(&diagnostics_collection, check_list_value, diagnostic_start);

    var output: std.ArrayListUnmanaged(u8) = .empty;
    defer output.deinit(allocator);

    // this compile without exit code 1!
    // TODO: ADDCT1 tmp, ticks(CLK, us=15000)

    std.log.debug("analyzing {s}...", .{input_path});
    var module = sema.analyze(allocator, ast_file, .{
        .blank_pointer_expr = .as_ptr_epxr,
        .fill_byte = cli.options.@"fill-byte",
        .io = init.io,
        .rebind_scopes = expander.did_import,
    }, &diagnostics_collection) catch |err| switch (err) {
        error.SemanticErrors => return try diagnostic_status(&diagnostics_collection, check_list_value, diagnostic_start),
        else => |e| return e,
    };
    defer module.deinit();

    for (module.segments) |segment| {
        const previous_end = output.items.len;
        try output.resize(allocator, @max(output.items.len, segment.hub_offset + segment.data.len));
        std.debug.assert(output.items.len >= segment.hub_offset + segment.data.len);

        // fill newly created data with the user-defined fill byte:
        @memset(output.items[previous_end..], cli.options.@"fill-byte");

        // then insert the segments data:
        @memcpy(output.items[segment.hub_offset..][0..segment.data.len], segment.data);
    }

    std.log.debug("sema yielded {} segments:", .{module.segments.len});

    for (module.segments, 0..) |seg, seg_i| {
        std.log.debug("  [{}]: offset 0x{X:0>6}, length {} bytes", .{ seg_i, seg.hub_offset, seg.data.len });

        var i: usize = 0;
        const chunk_size = 16;
        while (i < seg.data.len) : (i += chunk_size) {
            const rest = seg.data[i..];
            const segment = rest[0..@min(chunk_size, rest.len)];

            var chunk_buffer: [4 * chunk_size]u8 = undefined;

            var fbs: std.Io.Writer = .fixed(&chunk_buffer);
            for (segment, 0..) |byte, off| {
                if (off > 0) {
                    try fbs.writeAll(" ");
                    if ((off % 4) == 0) {
                        try fbs.writeAll(" ");
                    }
                }
                try fbs.print("{X:0>2}", .{byte});
            }

            std.log.debug("    0x{X:0>5}: {s}", .{ seg.hub_offset + i, fbs.buffered() });
        }
    }
    if (diagnostics_collection.has_errors())
        return try diagnostic_status(&diagnostics_collection, check_list_value, diagnostic_start);

    if (check_list_value) |list| {
        try list.evaluate(module, output.items, &diagnostics_collection);
        const check_failed = diagnostics_collection.has_errors();
        const status = try diagnostic_status(&diagnostics_collection, list, diagnostic_start);
        if (check_failed or status != 0) return status;
    }

    if (cli.options.@"list-file".len > 0) {
        const list_inputs: [1]listfile.Input = .{.{
            .source_file = source_file,
            .sources = &diagnostics_collection,
            .ast_file = ast_file,
            .module = module,
        }};

        if (std.mem.eql(u8, cli.options.@"list-file", "-")) {
            var buffer: [4096]u8 = undefined;
            var stdout_writer = std.Io.File.stdout().writer(init.io, &buffer);

            try listfile.render(&stdout_writer.interface, &list_inputs);
            try stdout_writer.interface.flush();
        } else {
            var buffer: [4096]u8 = undefined;
            var file = try std.Io.Dir.cwd().createFileAtomic(init.io, cli.options.@"list-file", .{ .replace = true });
            defer file.deinit(init.io);
            var file_writer = file.file.writer(init.io, &buffer);

            try listfile.render(&file_writer.interface, &list_inputs);
            try file_writer.flush();

            try file.replace(init.io);
        }
    }

    // Stop after having each file parsed successfully:
    if (cli.options.@"test-mode" == .sema)
        return 0;

    // Stop after having each file parsed successfully:
    if (cli.options.@"test-mode" == .compare) {
        const ref = try std.Io.Dir.cwd().readFileAlloc(init.io, cli.options.@"compare-to", init.arena.allocator(), .limited(512 * 1024));

        if (std.mem.eql(u8, ref, output.items)) {
            // boring case: our files are identical
            return 0;
        }

        var buffer: [4096]u8 = undefined;
        var stdout_writer = std.Io.File.stdout().writer(init.io, &buffer);

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
            module,
        );
        try writer.print("</diff>\n", .{});

        try writer.flush();

        return 1;
    }

    if (output_format == .none)
        return 0;

    if (cli.options.output.len > 0 and !std.mem.eql(u8, cli.options.output, "-")) {
        var file = try std.Io.Dir.cwd().createFileAtomic(init.io, cli.options.output, .{ .replace = true });
        defer file.deinit(init.io);

        try emit.emit(init.io, allocator, file.file, &.{module}, output.items, output_format);

        try file.replace(init.io);
    } else {
        try emit.emit(init.io, allocator, std.Io.File.stdout(), &.{module}, output.items, output_format);
    }

    return 0;
}

fn collect_include_paths(init: std.process.Init) ![]const []const u8 {
    const allocator = init.arena.allocator();
    var args = try init.minimal.args.iterateAllocator(allocator);
    defer args.deinit();
    _ = args.next(); // executable name

    var paths: std.ArrayListUnmanaged([]const u8) = .empty;
    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "--")) break;
        if (std.mem.eql(u8, arg, "-I") or std.mem.eql(u8, arg, "--include-path") or
            (arg.len > 2 and arg[0] == '-' and arg[1] != '-' and arg[arg.len - 1] == 'I'))
        {
            try paths.append(allocator, try allocator.dupe(u8, args.next().?));
        } else if (std.mem.startsWith(u8, arg, "--include-path=")) {
            try paths.append(allocator, try allocator.dupe(u8, arg["--include-path=".len..]));
        } else if (cli_option_requires_value(arg)) {
            _ = args.next();
        }
    }
    return try paths.toOwnedSlice(allocator);
}

fn cli_option_requires_value(arg: []const u8) bool {
    if (std.mem.startsWith(u8, arg, "--") and std.mem.indexOfScalar(u8, arg, '=') == null) {
        inline for (std.meta.fields(CliArgs)) |field| {
            if (field.type != bool and std.mem.eql(u8, arg[2..], field.name)) return true;
        }
    } else if (arg.len > 1 and arg[0] == '-' and arg[1] != '-') {
        inline for (std.meta.fields(@TypeOf(CliArgs.shorthands))) |field| {
            if (arg[arg.len - 1] == field.name[0] and @FieldType(CliArgs, @field(CliArgs.shorthands, field.name)) != bool) return true;
        }
    }
    return false;
}

fn usage_mistake(diagnostics_collection: *diagnostics.Collection, diagnostic: diagnostics.Kind) !u8 {
    try diagnostics_collection.emit_diag(null, diagnostic);
    return 1;
}

fn diagnostic_status(diagnostics_collection: *diagnostics.Collection, list: ?check_list.List, start: usize) !u8 {
    if (list) |checks| {
        if (checks.hasDiagnosticChecks()) _ = try checks.evaluateDiagnostics(diagnostics_collection, start);
    }
    return if (diagnostics_collection.has_errors()) 1 else 0;
}

test {
    _ = frontend;
    _ = sema;
    _ = listfile;
    _ = check_list;
    _ = diagnostics;
}

fn render_bin_diff(writer: *std.Io.Writer, expected_data: []const u8, actual_data: []const u8, mod: ?Module) !void {
    const common_len = @max(expected_data.len, actual_data.len);

    std.log.info("{}", .{common_len});

    var offset: usize = 0;
    while (offset < common_len) : (offset += @sizeOf(u32)) {
        const expected = read_diff_word(expected_data, offset);
        const actual = read_diff_word(actual_data, offset);

        // const expected: ?u8 = if (offset < expected_data.len) expected_data[offset] else null;
        // const actual: ?u8 = if (offset < actual_data.len) actual_data[offset] else null;

        if (actual == expected)
            continue;

        const instr_groups: []const u32 = &.{ 9, 9, 3, 7, 4 };
        try writer.print("@{X:0>5}: expected: 0x{X:0>8} ({s}), actual: 0x{X:0>8} ({s}) | {?f}\n", .{
            @as(u32, @intCast(offset)),
            expected,
            bitdiff(u32, expected, actual, instr_groups),
            actual,
            bitdiff(u32, actual, expected, instr_groups),
            if (mod) |m| m.line_for_address(@intCast(offset)) else null,
        });
        var dis_buf: [256]u8 = undefined;
        try writer.print("  expected: {!s}\n", .{disasm(&dis_buf, expected)});
        try writer.print("  actual:   {!s}\n", .{disasm(&dis_buf, actual)});
    }
}

fn read_diff_word(data: []const u8, offset: usize) u32 {
    var bytes: [4]u8 = @splat(0);
    if (offset < data.len) {
        const length = @min(bytes.len, data.len - offset);
        @memcpy(bytes[0..length], data[offset..][0..length]);
    }
    return std.mem.readInt(u32, &bytes, .little);
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
    var writer: std.Io.Writer = .fixed(buffer);

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

    return writer.buffered();
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

test {
    _ = @import("metadata_tests.zig");
}
