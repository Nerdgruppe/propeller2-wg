const std = @import("std");
const ast = @import("ast.zig");
const mode_directive = @import("../mode_directive.zig");

const unary_precedence: u8 = 6;
const mnemonic_column: usize = 14; // zero-based: column 15

const Row = struct {
    line: ast.Line,
    trailing_comment: ?[]const u8 = null,
    inner_comments: []const ast.Comment = &.{},

    fn content(row: Row) Content {
        return .{ .line = row.line, .comments = row.inner_comments };
    }
};

const Entry = union(enum) {
    blank,
    comment: []const u8,
    row: Row,
};

pub fn pretty_print(writer: *std.Io.Writer, file: ast.File) !void {
    var entries: Entries = .{ .file = file };
    var block_start = entries;
    var block_length: usize = 0;
    while (true) {
        const before = entries;
        const entry = entries.next() orelse break;
        const boundary = entry == .row and switch (entry.row.line) {
            .label => |label| label.identifier[0] != '.',
            .instruction => |instr| mode_directive.from_name(instr.mnemonic) != null,
            else => false,
        };
        if (boundary and block_length > 0) {
            try write_block(writer, block_start, block_length);
            block_start = before;
            block_length = 0;
        }
        block_length += 1;
    }
    try write_block(writer, block_start, block_length);
}

// Copying the cursor lets measurement and output walk the same block.
const Entries = struct {
    file: ast.File,
    line_index: usize = 0,
    comment_index: usize = 0,
    next_line: u32 = 1,

    fn next(self: *Entries) ?Entry {
        const file = self.file;
        while (self.line_index < file.sequence.len) {
            const index = self.line_index;
            const line = file.sequence[index];
            if (line == .empty) {
                self.line_index += 1;
                if (file.source.text.len == 0) return .blank;
                continue;
            }

            const location = line_location(line);
            if (file.source.text.len > 0 and self.next_line < location.line)
                return self.gap();
            self.line_index += 1;

            const end_line = switch (line) {
                .instruction => |instr| instr.span.endLocation().line,
                .constant => |con| con.span.endLocation().line,
                else => location.line,
            };
            const first_comment = self.comment_index;
            const next_same_line = for (file.sequence[index + 1 ..]) |later| {
                if (later == .empty) continue;
                break line_location(later).line == end_line;
            } else false;
            if (!(line == .label and next_same_line)) {
                while (self.comment_index < file.comments.len and file.comments[self.comment_index].span.location().line <= end_line)
                    self.comment_index += 1;
            }
            var inner_end = self.comment_index;
            var trailing: ?[]const u8 = null;
            if (inner_end > first_comment and file.comments[inner_end - 1].span.location().line == end_line and !next_same_line) {
                inner_end -= 1;
                trailing = file.comments[inner_end].text;
            }
            self.next_line = @max(self.next_line, end_line + 1);
            return .{ .row = .{
                .line = line,
                .trailing_comment = trailing,
                .inner_comments = file.comments[first_comment..inner_end],
            } };
        }

        if (file.source.text.len > 0) {
            const line_count = file.source.line_starts.len - @as(usize, if (std.mem.endsWith(u8, file.source.text, "\n")) 1 else 0);
            if (self.next_line <= line_count) return self.gap();
        }
        return null;
    }

    fn gap(self: *Entries) Entry {
        const line = self.next_line;
        self.next_line += 1;
        if (self.comment_index < self.file.comments.len and self.file.comments[self.comment_index].span.location().line == line) {
            const comment = self.file.comments[self.comment_index];
            self.comment_index += 1;
            return .{ .comment = comment.text };
        }
        return .blank;
    }
};

fn line_location(line: ast.Line) ast.Location {
    return switch (line) {
        .label => |value| value.span.location(),
        .constant => |value| value.span.location(),
        .instruction => |value| value.span.location(),
        .empty => unreachable,
    };
}

fn write_block(writer: *std.Io.Writer, start: Entries, length: usize) !void {
    var mnemonic_width: usize = 0;
    var entries = start;
    for (0..length) |_| {
        const entry = entries.next().?;
        if (entry == .row and entry.row.line == .instruction) {
            const instr = entry.row.line.instruction;
            if (!is_directive(instr)) mnemonic_width = @max(mnemonic_width, instr.mnemonic.len);
        }
    }
    const operand_column = mnemonic_column + mnemonic_width + 1;
    var operand_width: usize = 0;
    var effect_width: usize = 0;
    var comment_column: usize = 0;
    entries = start;
    for (0..length) |_| {
        const entry = entries.next().?;
        if (entry != .row) continue;
        const row = entry.row;
        switch (row.line) {
            .instruction => |instr| {
                if (!is_directive(instr)) {
                    operand_width = @max(operand_width, row.content().width());
                    if (instr.effect) |effect| effect_width = @max(effect_width, std.fmt.count(":{t}", .{effect.type}));
                }
            },
            .constant => |con| {
                if (row.trailing_comment != null)
                    comment_column = @max(comment_column, std.fmt.count("const {s} = ", .{con.identifier}) + row.content().width() + 1);
            },
            .label => |label| {
                if (row.trailing_comment != null)
                    comment_column = @max(comment_column, label.identifier.len + (if (label.type == .@"var") @as(usize, 5) else 1) + 1);
            },
            .empty => unreachable,
        }
    }
    const effect_column = operand_column + operand_width + 1;
    comment_column = @max(comment_column, effect_column + effect_width + 1);

    entries = start;
    for (0..length) |_| {
        const entry = entries.next().?;
        switch (entry) {
            .blank => try writer.writeByte('\n'),
            .comment => |comment| try writer.print("{s}\n", .{comment}),
            .row => |row| {
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
                        try row.content().write(writer, &column);
                    },
                    .instruction => |instr| {
                        const directive = is_directive(instr);
                        if (instr.condition) |condition| {
                            try pad_to(writer, &column, 2);
                            const formatted: Condition = .{ .condition = condition };
                            try writer.print("{f}", .{formatted});
                            column += std.fmt.count("{f}", .{formatted});
                        }
                        if (directive) {
                            if (instr.condition != null) try pad_to(writer, &column, column + 1);
                        } else try pad_to(writer, &column, mnemonic_column);
                        try write_text(writer, &column, instr.mnemonic);
                        if (instr.arguments.len > 0) {
                            const argument_column = if (directive) column + 1 else operand_column;
                            try pad_to(writer, &column, argument_column);
                            try row.content().write(writer, &column);
                        }
                        if (instr.effect) |effect| {
                            try pad_to(writer, &column, if (directive) column + 1 else effect_column);
                            try writer.print(":{t}", .{effect.type});
                            column += std.fmt.count(":{t}", .{effect.type});
                        }
                    },
                    .empty => unreachable,
                }
                if (row.trailing_comment) |comment| {
                    const directive = row.line == .instruction and is_directive(row.line.instruction);
                    try pad_to(writer, &column, if (directive) column + 2 else comment_column);
                    try write_text(writer, &column, comment);
                }
                try writer.writeByte('\n');
            },
        }
    }
}

pub fn pretty_print_expr(writer: *std.Io.Writer, expr: ast.Expression) !void {
    var printer: ExpressionPrinter = .{ .writer = writer, .comments = &.{} };
    try printer.expression(expr);
}

fn is_directive(instr: ast.Instruction) bool {
    return std.mem.startsWith(u8, instr.mnemonic, ".");
}

fn pad_to(writer: *std.Io.Writer, column: *usize, target: usize) !void {
    const count = if (column.* >= target) @as(usize, 1) else target - column.*;
    try writer.splatByteAll(' ', count);
    column.* += count;
}

fn write_text(writer: *std.Io.Writer, column: *usize, text: []const u8) !void {
    try writer.writeAll(text);
    column.* += text.len;
}

const Condition = struct {
    condition: ast.ConditionNode,

    pub fn format(self: Condition, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        const c_strings: [2][]const u8 = .{ "!C", "C" };
        const z_strings: [2][]const u8 = .{ "!Z", "Z" };
        switch (self.condition.type) {
            .@"return" => try writer.writeAll("return"),
            .c_is_z => try writer.writeAll("if(C == Z)"),
            .c_is_not_z => try writer.writeAll("if(C != Z)"),
            .c_is => |value| try writer.print("if({s})", .{c_strings[@intFromBool(value)]}),
            .z_is => |value| try writer.print("if({s})", .{z_strings[@intFromBool(value)]}),
            .c_and_z => |value| try writer.print("if({s} & {s})", .{ c_strings[@intFromBool(value.c)], z_strings[@intFromBool(value.z)] }),
            .c_or_z => |value| try writer.print("if({s} | {s})", .{ c_strings[@intFromBool(value.c)], z_strings[@intFromBool(value.z)] }),
            .comparison => |value| try writer.print("if({t})", .{value}),
        }
    }
};

const Content = struct {
    line: ast.Line,
    comments: []const ast.Comment,
    continuation: usize = 0,
    last_line_start: ?*usize = null,
    end_column: ?*usize = null,

    fn width(self: Content) usize {
        var last_line_start: usize = 0;
        var measured = self;
        measured.last_line_start = &last_line_start;
        const total = std.fmt.count("{f}", .{measured});
        return total - last_line_start;
    }

    fn write(self: Content, writer: *std.Io.Writer, column: *usize) !void {
        var formatted = self;
        formatted.continuation = column.*;
        formatted.end_column = column;
        try writer.print("{f}", .{formatted});
    }

    pub fn format(self: Content, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        var printer: ExpressionPrinter = .{ .writer = writer, .comments = self.comments, .continuation = self.continuation };
        switch (self.line) {
            .constant => |con| try printer.expression(con.value),
            .instruction => |instr| for (instr.arguments, 0..) |arg, index| {
                if (index > 0) try printer.write(", ");
                try printer.expression(arg);
            },
            else => unreachable,
        }
        try printer.finish();
        if (self.last_line_start) |start| start.* = printer.last_line_start;
        if (self.end_column) |column|
            column.* = printer.written - printer.last_line_start + (if (printer.last_line_start == 0) self.continuation else 0);
    }
};

const ExpressionPrinter = struct {
    writer: *std.Io.Writer,
    comments: []const ast.Comment,
    next_comment: usize = 0,
    indent: usize = 0,
    at_line_start: bool = true,
    last_char: u8 = 0,
    continuation: usize = 0,
    written: usize = 0,
    last_line_start: usize = 0,

    fn write(self: *ExpressionPrinter, text: []const u8) std.Io.Writer.Error!void {
        for (text) |char| {
            if (char == '\n') {
                try self.writer.writeByte('\n');
                self.written += 1;
                self.last_line_start = self.written;
                try self.writer.splatByteAll(' ', self.continuation);
                self.written += self.continuation;
                self.at_line_start = true;
            } else {
                if (self.at_line_start) {
                    try self.writer.splatByteAll(' ', self.indent);
                    self.written += self.indent;
                    self.at_line_start = false;
                }
                try self.writer.writeByte(char);
                self.written += 1;
            }
            self.last_char = char;
        }
    }

    fn newline(self: *ExpressionPrinter) std.Io.Writer.Error!void {
        if (!self.at_line_start) try self.write("\n");
    }

    fn before(self: *ExpressionPrinter, location: ast.Location) std.Io.Writer.Error!void {
        while (self.next_comment < self.comments.len and location_before(self.comments[self.next_comment].span.location(), location)) {
            if (!self.at_line_start) try self.write(if (self.last_char == ' ') " " else "  ");
            try self.write(self.comments[self.next_comment].text);
            try self.write("\n");
            self.next_comment += 1;
        }
    }

    fn finish(self: *ExpressionPrinter) std.Io.Writer.Error!void {
        while (self.next_comment < self.comments.len) {
            if (!self.at_line_start) try self.write("  ");
            try self.write(self.comments[self.next_comment].text);
            try self.write("\n");
            self.next_comment += 1;
        }
    }

    fn expression(self: *ExpressionPrinter, expr: ast.Expression) std.Io.Writer.Error!void {
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
                    const prefix = switch (value.operator) {
                        .pre_increment => "++",
                        .pre_decrement => "--",
                        else => @tagName(value.operator),
                    };
                    try self.write(prefix);
                    if (value.value.* == .unary_transform) {
                        const child_prefix: u8 = switch (value.value.unary_transform.operator) {
                            .@"+", .pre_increment => '+',
                            .@"-", .pre_decrement => '-',
                            else => 0,
                        };
                        if (prefix[prefix.len - 1] == child_prefix) try self.write(" ");
                    }
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

    fn operand(self: *ExpressionPrinter, expr: ast.Expression, parent_precedence: u8, is_rhs: bool) std.Io.Writer.Error!void {
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
