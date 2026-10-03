const std = @import("std");
const ast = @import("ast.zig");
const mode_directive = @import("../mode_directive.zig");

const unary_precedence: u8 = 6;
const mnemonic_column: usize = 14; // zero-based: column 15

const Row = struct {
    line: ast.Line,
    trailing_comment: ?[]const u8 = null,
    inner_comments: []const ast.Comment = &.{},
    leading_comments: []const ast.Comment = &.{},
    rendered: []const u8 = "",
    condition: []const u8 = "",
};

const Entry = union(enum) {
    blank,
    comment: []const u8,
    row: Row,
};

pub fn pretty_print(writer: anytype, file: ast.File) !void {
    try pretty_print_alloc(std.heap.page_allocator, writer, file);
}

pub fn pretty_print_alloc(allocator: std.mem.Allocator, writer: anytype, file: ast.File) !void {
    var arena_state: std.heap.ArenaAllocator = .init(allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    var entries: std.ArrayListUnmanaged(Entry) = .empty;
    var comment_index: usize = 0;
    var next_line: u32 = 1;

    for (file.sequence, 0..) |line, index| {
        if (line == .empty) {
            if (file.source.len == 0) try entries.append(arena, .blank);
            continue;
        }

        const location = line_location(line);
        if (file.source.len > 0) {
            while (next_line < location.line) : (next_line += 1) {
                try append_gap_line(arena, &entries, file.comments, &comment_index, next_line);
            }
        }

        const end_line: u32 = switch (line) {
            .instruction => |instr| instr.span.endLocation().line,
            .constant => |con| con.span.endLocation().line,
            else => location.line,
        };
        const first_comment = comment_index;
        const next_same_line = for (file.sequence[index + 1 ..]) |later| {
            if (later == .empty) continue;
            break line_location(later).line == end_line;
        } else false;
        if (!(line == .label and next_same_line)) {
            while (comment_index < file.comments.len and file.comments[comment_index].span.location().line <= end_line)
                comment_index += 1;
        }
        var inner_end = comment_index;
        var trailing: ?[]const u8 = null;
        if (inner_end > first_comment and file.comments[inner_end - 1].span.location().line == end_line and !next_same_line) {
            inner_end -= 1;
            trailing = file.comments[inner_end].text;
        }
        try entries.append(arena, .{ .row = .{
            .line = line,
            .trailing_comment = trailing,
            .inner_comments = file.comments[first_comment..inner_end],
        } });
        next_line = @max(next_line, end_line + 1);
    }

    if (file.source.len > 0) {
        const newline_count: u32 = @intCast(std.mem.count(u8, file.source, "\n"));
        const line_count = newline_count + @as(u32, if (std.mem.endsWith(u8, file.source, "\n")) 0 else 1);
        while (next_line <= line_count) : (next_line += 1)
            try append_gap_line(arena, &entries, file.comments, &comment_index, next_line);
    }

    var block_start: usize = 0;
    for (entries.items, 0..) |entry, index| {
        if (entry != .row) continue;
        const boundary = switch (entry.row.line) {
            .label => |label| label.identifier[0] != '.',
            .instruction => |instr| mode_directive.from_name(instr.mnemonic) != null,
            else => false,
        };
        if (boundary and index > block_start) {
            try write_block(arena, writer, entries.items[block_start..index]);
            block_start = index;
        }
    }
    try write_block(arena, writer, entries.items[block_start..]);
}

fn line_location(line: ast.Line) ast.Location {
    return switch (line) {
        .label => |value| value.span.location(),
        .constant => |value| value.span.location(),
        .instruction => |value| value.span.location(),
        .empty => unreachable,
    };
}

fn append_gap_line(allocator: std.mem.Allocator, entries: *std.ArrayListUnmanaged(Entry), comments: []const ast.Comment, comment_index: *usize, line: u32) !void {
    if (comment_index.* < comments.len and comments[comment_index.*].span.location().line == line) {
        try entries.append(allocator, .{ .comment = comments[comment_index.*].text });
        comment_index.* += 1;
    } else try entries.append(allocator, .blank);
}

fn write_block(allocator: std.mem.Allocator, writer: anytype, entries: []Entry) !void {
    var mnemonic_width: usize = 0;
    for (entries) |*entry| {
        if (entry.* != .row) continue;
        const row = &entry.row;
        var inner: std.ArrayListUnmanaged(ast.Comment) = .empty;
        var leading: std.ArrayListUnmanaged(ast.Comment) = .empty;
        for (row.inner_comments) |comment| {
            const in_empty_call = switch (row.line) {
                .constant => |con| comment_in_empty_call(con.value, comment.span.location()),
                .instruction => |instr| blk: {
                    for (instr.arguments) |arg| {
                        if (comment_in_empty_call(arg, comment.span.location())) break :blk true;
                    }
                    break :blk false;
                },
                else => false,
            };
            if (in_empty_call) {
                try leading.append(allocator, comment);
            } else {
                try inner.append(allocator, comment);
            }
        }
        row.inner_comments = try inner.toOwnedSlice(allocator);
        row.leading_comments = try leading.toOwnedSlice(allocator);
        switch (row.line) {
            .instruction => |instr| {
                if (!is_directive(instr)) mnemonic_width = @max(mnemonic_width, instr.mnemonic.len);
                row.condition = try render_condition(allocator, instr.condition);
                row.rendered = try render_arguments(allocator, instr.arguments, row.inner_comments);
            },
            .constant => |con| row.rendered = try render_expression(allocator, con.value, row.inner_comments),
            else => {},
        }
    }
    const operand_column = mnemonic_column + mnemonic_width + 1;
    var operand_width: usize = 0;
    var effect_width: usize = 0;
    for (entries) |entry| {
        if (entry != .row or entry.row.line != .instruction or is_directive(entry.row.line.instruction)) continue;
        const row = entry.row;
        operand_width = @max(operand_width, last_line_width(row.rendered));
        if (row.line.instruction.effect) |effect| effect_width = @max(effect_width, 1 + @tagName(effect.type).len);
    }
    const effect_column = operand_column + operand_width + 1;
    var comment_column = effect_column + effect_width + 1;
    for (entries) |entry| {
        if (entry != .row or entry.row.trailing_comment == null) continue;
        switch (entry.row.line) {
            .constant => |con| comment_column = @max(comment_column, "const ".len + con.identifier.len + " = ".len + last_line_width(entry.row.rendered) + 1),
            .label => |label| comment_column = @max(comment_column, label.identifier.len + (if (label.type == .@"var") @as(usize, 5) else 1) + 1),
            else => {},
        }
    }

    for (entries) |entry| {
        switch (entry) {
            .blank => try writer.writeByte('\n'),
            .comment => |comment| try writer.print("{s}\n", .{comment}),
            .row => |row| {
                for (row.leading_comments) |comment| try writer.print("{s}\n", .{comment.text});
                var column: usize = 0;
                switch (row.line) {
                    .label => |label| {
                        if (label.type == .@"var") try write_text(writer, &column, "var ");
                        try write_text(writer, &column, label.identifier);
                        try write_text(writer, &column, ":");
                    },
                    .constant => |con| {
                        try write_text(writer, &column, "const ");
                        try write_text(writer, &column, con.identifier);
                        try write_text(writer, &column, " = ");
                        try write_multiline(writer, &column, column, row.rendered);
                    },
                    .instruction => |instr| {
                        const directive = is_directive(instr);
                        if (instr.condition != null) {
                            try pad_to(writer, &column, 2);
                            try write_text(writer, &column, row.condition);
                        }
                        if (directive) {
                            if (instr.condition != null) try pad_to(writer, &column, column + 1);
                        } else try pad_to(writer, &column, mnemonic_column);
                        try write_text(writer, &column, instr.mnemonic);
                        if (instr.arguments.len > 0) {
                            const argument_column = if (directive) column + 1 else operand_column;
                            try pad_to(writer, &column, argument_column);
                            try write_multiline(writer, &column, argument_column, row.rendered);
                        }
                        if (instr.effect) |effect| {
                            try pad_to(writer, &column, if (directive) column + 1 else effect_column);
                            try writer.print(":{s}", .{@tagName(effect.type)});
                            column += 1 + @tagName(effect.type).len;
                        }
                    },
                    .empty => unreachable,
                }
                if (row.trailing_comment) |comment| {
                    const directive = row.line == .instruction and is_directive(row.line.instruction);
                    if (directive) {
                        try pad_to(writer, &column, column + 2);
                    } else try pad_to(writer, &column, comment_column);
                    try write_text(writer, &column, comment);
                }
                try writer.writeByte('\n');
            },
        }
    }
}

pub fn pretty_print_expr(writer: anytype, expr: ast.Expression) !void {
    var printer: ExpressionPrinter = .{ .writer = writer, .comments = &.{} };
    try printer.expression(expr);
}

fn is_directive(instr: ast.Instruction) bool {
    return std.mem.startsWith(u8, instr.mnemonic, ".");
}

fn comment_in_empty_call(expr: ast.Expression, location: ast.Location) bool {
    return switch (expr) {
        .function_call => |call| blk: {
            if (call.arguments.len == 0 and
                location_before(call.span.location(), location) and location_before(location, call.span.at(call.span.end - 1).location())) break :blk true;
            for (call.arguments) |arg| {
                if (comment_in_empty_call(arg.value, location)) break :blk true;
            }
            break :blk false;
        },
        .wrapped => |value| comment_in_empty_call(value.value.*, location),
        .sequence => |value| blk: {
            for (value.items) |item| {
                if (comment_in_empty_call(item, location)) break :blk true;
            }
            break :blk false;
        },
        .unary_transform => |value| comment_in_empty_call(value.value.*, location),
        .binary_transform => |value| comment_in_empty_call(value.lhs.*, location) or comment_in_empty_call(value.rhs.*, location),
        else => false,
    };
}

fn last_line_width(value: []const u8) usize {
    return value.len - (if (std.mem.lastIndexOfScalar(u8, value, '\n')) |index| index + 1 else 0);
}

fn pad_to(writer: anytype, column: *usize, target: usize) !void {
    const count = if (column.* >= target) @as(usize, 1) else target - column.*;
    for (0..count) |_| try writer.writeByte(' ');
    column.* += count;
}

fn write_text(writer: anytype, column: *usize, text: []const u8) !void {
    try writer.writeAll(text);
    column.* += text.len;
}

fn write_multiline(writer: anytype, column: *usize, continuation: usize, text: []const u8) !void {
    for (text) |char| {
        if (char == '\n') {
            try writer.writeByte('\n');
            column.* = 0;
            for (0..continuation) |_| try writer.writeByte(' ');
            column.* = continuation;
        } else {
            try writer.writeByte(char);
            column.* += 1;
        }
    }
}

fn render_condition(allocator: std.mem.Allocator, maybe_condition: ?ast.ConditionNode) ![]const u8 {
    const condition = maybe_condition orelse return "";
    var output: std.Io.Writer.Allocating = .init(allocator);
    const writer = &output.writer;
    const c_strings: [2][]const u8 = .{ "!C", "C" };
    const z_strings: [2][]const u8 = .{ "!Z", "Z" };
    switch (condition.type) {
        .@"return" => try writer.writeAll("return"),
        .c_is_z => try writer.writeAll("if(C == Z)"),
        .c_is_not_z => try writer.writeAll("if(C != Z)"),
        .c_is => |value| try writer.print("if({s})", .{c_strings[@intFromBool(value)]}),
        .z_is => |value| try writer.print("if({s})", .{z_strings[@intFromBool(value)]}),
        .c_and_z => |value| try writer.print("if({s} & {s})", .{ c_strings[@intFromBool(value.c)], z_strings[@intFromBool(value.z)] }),
        .c_or_z => |value| try writer.print("if({s} | {s})", .{ c_strings[@intFromBool(value.c)], z_strings[@intFromBool(value.z)] }),
        .comparison => |value| try writer.print("if({s})", .{@tagName(value)}),
    }
    return try output.toOwnedSlice();
}

fn render_expression(allocator: std.mem.Allocator, expr: ast.Expression, comments: []const ast.Comment) ![]const u8 {
    var output: std.Io.Writer.Allocating = .init(allocator);
    var printer: ExpressionPrinter = .{ .writer = &output.writer, .comments = comments };
    try printer.expression(expr);
    try printer.finish();
    return try output.toOwnedSlice();
}

fn render_arguments(allocator: std.mem.Allocator, args: []const ast.Expression, comments: []const ast.Comment) ![]const u8 {
    var output: std.Io.Writer.Allocating = .init(allocator);
    var printer: ExpressionPrinter = .{ .writer = &output.writer, .comments = comments };
    for (args, 0..) |arg, index| {
        if (index > 0) try printer.write(", ");
        try printer.expression(arg);
    }
    try printer.finish();
    return try output.toOwnedSlice();
}

const ExpressionPrinter = struct {
    writer: *std.Io.Writer,
    comments: []const ast.Comment,
    next_comment: usize = 0,
    indent: usize = 0,
    at_line_start: bool = true,
    last_char: u8 = 0,

    fn write(self: *ExpressionPrinter, text: []const u8) !void {
        for (text) |char| {
            if (char == '\n') {
                try self.writer.writeByte('\n');
                self.at_line_start = true;
            } else {
                if (self.at_line_start) {
                    for (0..self.indent) |_| try self.writer.writeByte(' ');
                    self.at_line_start = false;
                }
                try self.writer.writeByte(char);
            }
            self.last_char = char;
        }
    }

    fn newline(self: *ExpressionPrinter) !void {
        if (!self.at_line_start) try self.write("\n");
    }

    fn before(self: *ExpressionPrinter, location: ast.Location) !void {
        while (self.next_comment < self.comments.len and location_before(self.comments[self.next_comment].span.location(), location)) {
            if (!self.at_line_start) try self.write(if (self.last_char == ' ') " " else "  ");
            try self.write(self.comments[self.next_comment].text);
            try self.write("\n");
            self.next_comment += 1;
        }
    }

    fn finish(self: *ExpressionPrinter) !void {
        while (self.next_comment < self.comments.len) {
            if (!self.at_line_start) try self.write("  ");
            try self.write(self.comments[self.next_comment].text);
            try self.write("\n");
            self.next_comment += 1;
        }
    }

    fn expression(self: *ExpressionPrinter, expr: ast.Expression) anyerror!void {
        switch (expr) {
            .current_pc => |span| {
                try self.before(span.location());
                try self.write("$");
            },
            .integer => |value| {
                try self.before(value.span.location());
                try self.write(value.source_text);
            },
            .symbol => |value| {
                try self.before(value.span.location());
                try self.write(value.symbol_name);
            },
            .enumerator => |value| {
                try self.before(value.span.location());
                try self.write("#");
                try self.write(value.symbol_name);
            },
            .string => |value| {
                try self.before(value.span.location());
                try self.write(value.source_text);
            },
            .sequence => |value| {
                try self.before(value.span.location());
                try self.write("[");
                for (value.items, 0..) |item, index| {
                    if (index > 0) try self.write(", ");
                    try self.expression(item);
                }
                try self.before(value.span.at(value.span.end - 1).location());
                try self.write("]");
            },
            .wrapped => |value| {
                try self.before(value.span.location());
                try self.write("(");
                try self.expression(value.value.*);
                try self.before(value.span.at(value.span.end - 1).location());
                try self.write(")");
            },
            .unary_transform => |value| {
                if (value.operator == .post_increment or value.operator == .post_decrement) {
                    try self.expression(value.value.*);
                    try self.before(value.operator_span.location());
                    try self.write(if (value.operator == .post_increment) "++" else "--");
                } else {
                    try self.before(value.operator_span.location());
                    try self.write(switch (value.operator) {
                        .pre_increment => "++",
                        .pre_decrement => "--",
                        else => @tagName(value.operator),
                    });
                    try self.operand(value.value.*, unary_precedence, false);
                }
            },
            .binary_transform => |value| {
                const precedence = binary_precedence(value.operator);
                try self.operand(value.lhs.*, precedence, false);
                try self.before(value.operator_span.location());
                if (value.operator == .array_index) {
                    try self.write("[");
                    try self.expression(value.rhs.*);
                    try self.before(value.span.at(value.span.end - 1).location());
                    try self.write("]");
                } else {
                    try self.write(" ");
                    try self.write(@tagName(value.operator));
                    try self.write(" ");
                    try self.operand(value.rhs.*, precedence, true);
                }
            },
            .function_call => |value| {
                try self.before(value.span.location());
                try self.write(value.function);
                try self.write("(");
                const multiline = value.has_trailing_comma or self.call_has_comment(value);
                if (multiline and value.arguments.len > 0) {
                    self.indent += 4;
                    for (value.arguments, 0..) |arg, index| {
                        if (index == 0) try self.newline();
                        try self.before(arg.span.location());
                        if (index > 0) try self.newline();
                        if (arg.name) |name| {
                            try self.write(name);
                            try self.write("=");
                        }
                        try self.expression(arg.value);
                        try self.write(",");
                    }
                    try self.before(value.span.at(value.span.end - 1).location());
                    self.indent -= 4;
                    try self.newline();
                } else {
                    for (value.arguments, 0..) |arg, index| {
                        if (index > 0) try self.write(", ");
                        if (arg.name) |name| {
                            try self.write(name);
                            try self.write("=");
                        }
                        try self.expression(arg.value);
                    }
                    try self.before(value.span.at(value.span.end - 1).location());
                }
                try self.write(")");
            },
        }
    }

    fn operand(self: *ExpressionPrinter, expr: ast.Expression, parent_precedence: u8, is_rhs: bool) !void {
        const needs_parens = switch (expr) {
            .binary_transform => |binary| blk: {
                const child_precedence = binary_precedence(binary.operator);
                break :blk child_precedence < parent_precedence or (is_rhs and child_precedence == parent_precedence);
            },
            else => false,
        };
        if (needs_parens) try self.write("(");
        try self.expression(expr);
        if (needs_parens) try self.write(")");
    }

    fn call_has_comment(self: *ExpressionPrinter, call: ast.FunctionInvocation) bool {
        const end = call.span.at(call.span.end - 1).location();
        for (self.comments) |comment| {
            if (location_before(call.span.location(), comment.span.location()) and location_before(comment.span.location(), end)) return true;
        }
        return false;
    }
};

fn location_before(a: ast.Location, b: ast.Location) bool {
    return a.line < b.line or (a.line == b.line and a.column < b.column);
}

fn binary_precedence(operator: ast.BinaryOperator) u8 {
    // Match parser.zig's opgroup_0 through opgroup_4; larger binds tighter.
    return switch (operator) {
        .@"and", .@"or", .xor => 1,
        .@"==", .@"!=", .@"<=>", .@"<", .@">", .@"<=", .@">=" => 2,
        .@"+", .@"-", .@"|", .@"^" => 3,
        .@"&", .@"*", .@"/", .@"%" => 4,
        .@"<<", .@">>" => 5,
        .array_index => unary_precedence + 1,
    };
}

test {
    _ = @import("render_tests.zig");
}
