const std = @import("std");

const frontend = @import("frontend.zig");
const Module = @import("Module.zig");
const eval = @import("stdlib/eval.zig");

pub const Input = struct {
    path: []const u8,
    source: []const u8,
    ast_file: frontend.ast.File,
    module: Module,
};

pub fn render(writer: *std.Io.Writer, inputs: []const Input) !void {
    var arena_allocator: std.heap.ArenaAllocator = .init(std.heap.page_allocator);
    defer arena_allocator.deinit();

    const allocator = arena_allocator.allocator();

    try render_symbols(allocator, writer, inputs);
    try writer.writeAll("\n");
    try render_segments(allocator, writer, inputs);

    var segment_index: usize = 0;
    for (inputs) |input| {
        for (input.module.segments) |segment| {
            try writer.writeAll("\n");
            try render_segment_body(allocator, writer, input, segment, segment_index);
            segment_index += 1;
        }
    }
}

const SymbolRow = struct {
    kind: []const u8,
    hub: []const u8,
    pc: []const u8,
    name: []const u8,
    value: []const u8,
    source: []const u8,
};

fn render_symbols(allocator: std.mem.Allocator, writer: *std.Io.Writer, inputs: []const Input) !void {
    var rows: std.ArrayList(SymbolRow) = .empty;
    defer rows.deinit(allocator);

    for (inputs) |input| {
        for (input.ast_file.sequence) |item| {
            switch (item) {
                .empty, .instruction => {},

                .label => |label| {
                    const symbol = find_symbol(input.module, label.identifier) orelse continue;
                    try rows.append(allocator, .{
                        .kind = @tagName(symbol.label.local),
                        .hub = try format_hub(allocator, symbol.label.hub_address),
                        .pc = try format_pc(allocator, symbol.label.local),
                        .name = symbol.name,
                        .value = "",
                        .source = source_line(input.source, label.location.line),
                    });
                },

                .constant => |constant| {
                    const value = find_constant(input.module, constant.identifier) orelse continue;
                    try rows.append(allocator, .{
                        .kind = "const",
                        .hub = "-----",
                        .pc = "---",
                        .name = value.name,
                        .value = try format_value(allocator, value.value),
                        .source = source_line(input.source, constant.location.line),
                    });
                },
            }
        }
    }

    const widths = widths_for_rows(SymbolRow, &.{
        "kind",
        "hub",
        "pc",
        "name",
        "value",
        "source",
    }, rows.items);

    try writer.writeAll("symbols\n");
    const header_alignments = [_]Alignment{ .left, .left, .left, .left, .left, .left };
    const row_alignments = [_]Alignment{ .left, .left, .left, .left, .right, .left };

    try write_row(writer, &widths, &.{ "kind", "hub", "pc", "name", "value", "source" }, &header_alignments);
    for (rows.items) |row| {
        try write_row(writer, &widths, &.{ row.kind, row.hub, row.pc, row.name, row.value, row.source }, &row_alignments);
    }
}

const SegmentRow = struct {
    id: []const u8,
    mode: []const u8,
    hub: []const u8,
    size: []const u8,
};

fn render_segments(allocator: std.mem.Allocator, writer: *std.Io.Writer, inputs: []const Input) !void {
    var rows: std.ArrayList(SegmentRow) = .empty;
    defer rows.deinit(allocator);

    var segment_index: usize = 0;
    for (inputs) |input| {
        for (input.module.segments) |segment| {
            try rows.append(allocator, .{
                .id = try std.fmt.allocPrint(allocator, "{d}", .{segment_index}),
                .mode = try format_exec_mode(allocator, segment.exec_mode),
                .hub = try format_hub(allocator, segment.hub_offset),
                .size = try std.fmt.allocPrint(allocator, "{d}", .{segment.data.len}),
            });
            segment_index += 1;
        }
    }

    const widths = widths_for_rows(SegmentRow, &.{ "id", "mode", "hub", "size" }, rows.items);
    const header_alignments = [_]Alignment{ .left, .left, .left, .left };
    const row_alignments = [_]Alignment{ .left, .left, .left, .right };

    try writer.writeAll("segments\n");
    try write_row(writer, &widths, &.{ "id", "mode", "hub", "size" }, &header_alignments);
    for (rows.items) |row| {
        try write_row(writer, &widths, &.{ row.id, row.mode, row.hub, row.size }, &row_alignments);
    }
}

const BodyRow = struct {
    hub: []const u8,
    pc: []const u8,
    bytes: []const u8,
    source: []const u8,
};

fn render_segment_body(
    allocator: std.mem.Allocator,
    writer: *std.Io.Writer,
    input: Input,
    segment: Module.Segment,
    segment_index: usize,
) !void {
    var rows: std.ArrayList(BodyRow) = .empty;
    defer rows.deinit(allocator);

    for (input.module.line_data) |line| {
        if (!line_belongs_to_segment(input, line, segment))
            continue;

        try rows.append(allocator, .{
            .hub = try format_hub(allocator, line.offset),
            .pc = try format_segment_pc(allocator, segment, line.offset),
            .bytes = try format_bytes(allocator, segment, line),
            .source = source_line(input.source, line.location.line),
        });
    }

    const widths = widths_for_rows(BodyRow, &.{ "hub", "pc", "bytes", "source" }, rows.items);
    const alignments = [_]Alignment{ .left, .left, .left, .left };

    try writer.print("{s} segment {}\n", .{ try format_exec_mode(allocator, segment.exec_mode), segment_index });
    try write_row(writer, &widths, &.{ "hub", "pc", "bytes", "source" }, &alignments);
    for (rows.items) |row| {
        try write_row(writer, &widths, &.{ row.hub, row.pc, row.bytes, row.source }, &alignments);
    }
}

const Alignment = enum {
    left,
    right,
};

fn write_row(
    writer: *std.Io.Writer,
    widths: []const usize,
    columns: []const []const u8,
    alignments: []const Alignment,
) !void {
    std.debug.assert(widths.len == columns.len);
    std.debug.assert(alignments.len == columns.len);

    for (columns, widths, alignments, 0..) |column, width, alignment, index| {
        if (index > 0)
            try writer.writeAll(" | ");

        if (alignment == .right and width > column.len)
            try writer.splatByteAll(' ', width - column.len);

        try writer.writeAll(column);
        if (alignment == .left and index + 1 < columns.len and width > column.len)
            try writer.splatByteAll(' ', width - column.len);
    }
    try writer.writeAll("\n");
}

fn widths_for_rows(comptime Row: type, comptime fields: []const []const u8, rows: []const Row) [fields.len]usize {
    var widths: [fields.len]usize = undefined;

    inline for (fields, 0..) |field, index| {
        widths[index] = field.len;
        for (rows) |row| {
            widths[index] = @max(widths[index], @field(row, field).len);
        }
    }

    return widths;
}

fn find_symbol(module: Module, name: []const u8) ?Module.Symbol {
    for (module.symbols) |symbol| {
        if (std.mem.eql(u8, symbol.name, name))
            return symbol;
    }
    return null;
}

fn find_constant(module: Module, name: []const u8) ?Module.Constant {
    for (module.constants) |constant| {
        if (std.mem.eql(u8, constant.name, name))
            return constant;
    }
    return null;
}

fn line_belongs_to_segment(input: Input, line: Module.LineData, segment: Module.Segment) bool {
    const start = @as(u32, segment.hub_offset);
    const end = start + @as(u32, @intCast(segment.data.len));

    if (line.length == 0) {
        const label = label_name_at(input.ast_file, line.location) orelse return line.offset >= start and line.offset <= end;
        const symbol = find_symbol(input.module, label) orelse return false;
        return symbol.label.segment_id == segment.id;
    }

    return line.offset >= start and line.offset < end;
}

fn label_name_at(file: frontend.ast.File, location: frontend.ast.Location) ?[]const u8 {
    for (file.sequence) |item| {
        if (item != .label)
            continue;

        const label = item.label;
        if (same_location(label.location, location))
            return label.identifier;
    }
    return null;
}

fn same_location(lhs: frontend.ast.Location, rhs: frontend.ast.Location) bool {
    if (lhs.line != rhs.line or lhs.column != rhs.column)
        return false;

    if (lhs.source == null or rhs.source == null)
        return lhs.source == null and rhs.source == null;

    return std.mem.eql(u8, lhs.source.?, rhs.source.?);
}

fn source_line(source: []const u8, one_based_line: u32) []const u8 {
    if (one_based_line == 0)
        return "";

    var start: usize = 0;
    var current_line: u32 = 1;

    while (current_line < one_based_line) : (current_line += 1) {
        const newline = std.mem.indexOfScalarPos(u8, source, start, '\n') orelse return "";
        start = newline + 1;
    }

    var end = std.mem.indexOfScalarPos(u8, source, start, '\n') orelse source.len;
    if (end > start and source[end - 1] == '\r')
        end -= 1;

    return source[start..end];
}

fn format_exec_mode(allocator: std.mem.Allocator, mode: eval.ExecMode) ![]const u8 {
    return std.fmt.allocPrint(allocator, "{s}exec", .{@tagName(mode)});
}

fn format_hub(allocator: std.mem.Allocator, address: anytype) ![]const u8 {
    return std.fmt.allocPrint(allocator, "{X:0>5}", .{address});
}

fn format_pc(allocator: std.mem.Allocator, local: Module.TaggedAddress.Local) ![]const u8 {
    return switch (local) {
        .hub => "---",
        .cog, .lut => |value| std.fmt.allocPrint(allocator, "{X:0>3}", .{value}),
    };
}

fn format_segment_pc(allocator: std.mem.Allocator, segment: Module.Segment, hub_offset: u32) ![]const u8 {
    return switch (segment.exec_mode) {
        .hub => "---",
        .cog, .lut => std.fmt.allocPrint(allocator, "{X:0>3}", .{(hub_offset - segment.hub_offset) / 4}),
    };
}

fn format_value(allocator: std.mem.Allocator, value: eval.Value) ![]const u8 {
    var output: std.Io.Writer.Allocating = .init(allocator);
    defer output.deinit();

    switch (value.value) {
        .int => |int| try output.writer.print("{d}", .{int}),
        .string => |string| try output.writer.print("\"{f}\"", .{std.zig.fmtString(string)}),
        .address => |address| try output.writer.print("{f}", .{address}),
        .register => |register| try output.writer.print("{f}", .{register}),
        .enumerator => |enumerator| try output.writer.print("#{s}", .{enumerator}),
        .pointer_expr => |ptr| try output.writer.print("{f}", .{ptr}),
    }

    return try output.toOwnedSlice();
}

fn format_bytes(allocator: std.mem.Allocator, segment: Module.Segment, line: Module.LineData) ![]const u8 {
    if (line.length == 0)
        return "";

    const start = @as(usize, @intCast(line.offset - segment.hub_offset));
    const end = start + @as(usize, @intCast(line.length));
    const bytes = segment.data[start..end];

    var output: std.Io.Writer.Allocating = .init(allocator);
    defer output.deinit();

    for (bytes, 0..) |byte, index| {
        if (index > 0)
            try output.writer.writeAll(" ");
        try output.writer.print("{X:0>2}", .{byte});
    }

    return try output.toOwnedSlice();
}

test "source line extraction trims line endings" {
    const source = "first\r\nsecond\nthird";

    try std.testing.expectEqualStrings("first", source_line(source, 1));
    try std.testing.expectEqualStrings("second", source_line(source, 2));
    try std.testing.expectEqualStrings("third", source_line(source, 3));
    try std.testing.expectEqualStrings("", source_line(source, 4));
}

test "render list file with symbols and segment body" {
    const path = "test.propan";
    const source =
        \\const CLK = 200_000_000
        \\_start:
        \\    NOP
        \\var storage:
        \\    BYTE 1, 2
        \\
    ;

    const segment_id: Module.Segment_ID = @enumFromInt(0);
    const data = [_]u8{ 0xAA, 0xBB, 0xCC, 0xDD, 0x01, 0x02 };

    var module: Module = .{
        .arena = .init(std.testing.allocator),
        .segments = &.{
            .{
                .id = segment_id,
                .hub_offset = 0,
                .data = &data,
                .exec_mode = .cog,
            },
        },
        .line_data = &.{
            .{ .offset = 0, .length = 0, .location = loc(path, 2) },
            .{ .offset = 0, .length = 4, .location = loc(path, 3) },
            .{ .offset = 4, .length = 0, .location = loc(path, 4) },
            .{ .offset = 4, .length = 2, .location = loc(path, 5) },
        },
        .symbols = &.{
            .{
                .name = "_start",
                .label = .init_cog(segment_id, 0, 0),
                .type = .code,
            },
            .{
                .name = "storage",
                .label = .init_cog(segment_id, 4, 1),
                .type = .data,
            },
        },
        .constants = &.{
            .{
                .name = "CLK",
                .value = .int(200_000_000),
                .location = loc(path, 1),
            },
        },
    };
    defer module.deinit();

    const ast_file: frontend.ast.File = .{
        .sequence = &.{
            .{ .constant = .{
                .location = loc(path, 1),
                .identifier = "CLK",
                .value = .{ .integer = .{
                    .location = loc(path, 1),
                    .source_text = "200_000_000",
                    .value = 200_000_000,
                } },
            } },
            .{ .label = .{
                .location = loc(path, 2),
                .identifier = "_start",
                .type = .code,
            } },
            .{ .instruction = .{
                .location = loc(path, 3),
                .mnemonic = "NOP",
                .arguments = &.{},
                .condition = null,
                .effect = null,
            } },
            .{ .label = .{
                .location = loc(path, 4),
                .identifier = "storage",
                .type = .@"var",
            } },
            .{ .instruction = .{
                .location = loc(path, 5),
                .mnemonic = "BYTE",
                .arguments = &.{},
                .condition = null,
                .effect = null,
            } },
        },
    };

    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();

    try render(&output.writer, &.{
        .{
            .path = path,
            .source = source,
            .ast_file = ast_file,
            .module = module,
        },
    });

    const actual = try output.toOwnedSlice();
    defer std.testing.allocator.free(actual);

    try std.testing.expectEqualStrings(
        \\symbols
        \\kind  | hub   | pc  | name    | value     | source
        \\const | ----- | --- | CLK     | 200000000 | const CLK = 200_000_000
        \\cog   | 00000 | 000 | _start  |           | _start:
        \\cog   | 00004 | 001 | storage |           | var storage:
        \\
        \\segments
        \\id | mode    | hub   | size
        \\0  | cogexec | 00000 |    6
        \\
        \\cogexec segment 0
        \\hub   | pc  | bytes       | source
        \\00000 | 000 |             | _start:
        \\00000 | 000 | AA BB CC DD |     NOP
        \\00004 | 001 |             | var storage:
        \\00004 | 001 | 01 02       |     BYTE 1, 2
        \\
    , actual);
}

fn loc(source: []const u8, line: u32) frontend.ast.Location {
    return .{
        .source = source,
        .line = line,
        .column = 1,
    };
}
