const std = @import("std");
const ast = @import("ast.zig");
const mode_directive = @import("../mode_directive.zig");

const unary_precedence: u8 = 6;
const mnemonic_column: usize = 16;
const condition_column: usize = 4;
const first_operand_column: usize = 24;

const Row = struct {
    line: ast.Line,
    trailing_comment: ?[]const u8 = null,
    inner_comments: []const ast.Comment = &.{},
    label: ?ast.Label = null,

    fn content(row: Row) Content {
        return .{ .line = row.line, .comments = row.inner_comments };
    }
};

const Entry = union(enum) {
    blank,
    comment: struct { text: []const u8, indent: usize },
    row: Row,
};

pub fn pretty_print(writer: *std.Io.Writer, file: ast.File) !void {
    var entries: Entries = .{ .file = file };
    var block_start = entries;
    var block_length: usize = 0;
    var trailing_blanks: usize = 0;
    while (true) {
        const before = entries;
        const entry = entries.next() orelse break;
        trailing_blanks = if (entry == .blank) trailing_blanks + 1 else 0;
        const boundary = entry == .row and (if (entry.row.label) |label| label.identifier[0] != '.' else switch (entry.row.line) {
            .label => |label| label.identifier[0] != '.',
            .instruction => |instr| mode_directive.from_name(instr.mnemonic) != null,
            else => false,
        });
        if (boundary and block_length > 0) {
            try write_block(writer, block_start, block_length);
            block_start = before;
            block_length = 0;
        }
        block_length += 1;
    }
    try write_block(writer, block_start, block_length);
    if (trailing_blanks == 0) try writer.writeByte('\n');
}

// Copying the cursor lets measurement and output walk the same block.
const Entries = struct {
    file: ast.File,
    line_index: usize = 0,
    comment_index: usize = 0,
    next_line: u32 = 1,
    blank_run: usize = 0,
    blank_limit: usize = 2,
    last_was_constant: bool = false,

    fn next(self: *Entries) ?Entry {
        while (self.next_raw()) |entry| {
            if (entry == .blank) {
                self.blank_run += 1;
                if (self.blank_run == 1) {
                    self.blank_limit = 2;
                    if (self.last_was_constant) {
                        var following = self.*;
                        while (following.next_raw()) |next_entry| {
                            if (next_entry == .blank) continue;
                            if (next_entry == .row and next_entry.row.line == .constant) self.blank_limit = 1;
                            break;
                        }
                    }
                }
                if (self.blank_run > self.blank_limit) continue;
            } else {
                self.blank_run = 0;
                self.last_was_constant = entry == .row and entry.row.line == .constant;
            }
            if (entry == .row and entry.row.line == .label and entry.row.trailing_comment == null) {
                var following = self.*;
                while (following.next_raw()) |next_entry| {
                    if (next_entry == .blank) continue;
                    if (next_entry == .row and next_entry.row.line == .instruction and can_fold(entry.row.line.label, next_entry.row.line.instruction)) {
                        self.* = following;
                        var row = next_entry.row;
                        row.label = entry.row.line.label;
                        return .{ .row = row };
                    }
                    break;
                }
            }
            return entry;
        }
        return null;
    }

    fn next_raw(self: *Entries) ?Entry {
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
            return .{ .comment = .{ .text = comment.text, .indent = self.comment_indent(self.comment_index - 1) } };
        }
        return .blank;
    }

    fn comment_indent(self: Entries, index: usize) usize {
        const comments = self.file.comments;
        var first = index;
        while (first > 0 and comments[first - 1].span.location().line + 1 == comments[first].span.location().line and standalone(comments[first - 1])) first -= 1;
        var last = index;
        while (last + 1 < comments.len and comments[last].span.location().line + 1 == comments[last + 1].span.location().line and standalone(comments[last + 1])) last += 1;
        const first_line = comments[first].span.location().line;
        const last_line = comments[last].span.location().line;
        if (first_line > 1 and self.blank_line(first_line - 1) and self.blank_line(last_line + 1))
            return std.mem.alignForward(usize, comments[first].span.location().column - 1, 4);
        for (self.file.sequence[self.line_index..]) |line| {
            if (line == .empty or line_location(line).line <= last_line) continue;
            return if (line == .instruction and !is_directive(line.instruction)) mnemonic_column else 0;
        }
        return std.mem.alignForward(usize, comments[first].span.location().column - 1, 4);
    }

    fn blank_line(self: Entries, line: u32) bool {
        const text = self.file.source.line(line) orelse return false;
        const start = self.file.source.line_starts[line - 1];
        return start < self.file.source.text.len and std.mem.trim(u8, text, " \t\r").len == 0;
    }
};

fn standalone(comment: ast.Comment) bool {
    const source = comment.span.source orelse return true;
    const start = source.line_starts[comment.span.location().line - 1];
    return std.mem.trim(u8, source.text[start..comment.span.start], " \t\r").len == 0;
}

fn can_fold(label: ast.Label, instr: ast.Instruction) bool {
    if (instr.condition != null) return false;
    const width = label.identifier.len + 2 + (if (label.type == .@"var") @as(usize, 4) else 0);
    if (width > mnemonic_column) return false;
    if (is_directive(instr)) return false;
    if (label.identifier[0] == '.') return true;
    for ([_][]const u8{ "LONG", "WORD", "BYTE", "FILE", "RES" }) |name| {
        if (std.ascii.eqlIgnoreCase(instr.mnemonic, name)) return true;
    }
    return false;
}

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
    const operand_column = @max(first_operand_column, next_column(mnemonic_column + mnemonic_width));
    const columns: OperandColumns = .{ .start = start, .length = length, .first = operand_column };
    var operand_width: usize = 0;
    var effect_width: usize = 0;
    var comment_column: usize = 0;
    var constant_width: usize = 0;
    entries = start;
    for (0..length) |index| {
        const before = entries;
        const entry = entries.next().?;
        if (entry == .row and entry.row.line == .constant) {
            if (constant_width == 0) constant_width = constant_block_width(before, length - index);
        } else constant_width = 0;
        if (entry != .row) continue;
        const row = entry.row;
        switch (row.line) {
            .instruction => |instr| {
                if (!is_directive(instr)) {
                    var content = row.content();
                    content.columns = columns;
                    operand_width = @max(operand_width, content.width());
                    if (instr.effect) |effect| effect_width = @max(effect_width, std.fmt.count(":{t}", .{effect.type}));
                }
            },
            .constant => {
                if (row.trailing_comment != null) {
                    var content = row.content();
                    content.continuation = "const ".len + constant_width + " = ".len;
                    comment_column = @max(comment_column, next_column(content.continuation + content.width()));
                }
            },
            .label => |label| {
                if (row.trailing_comment != null)
                    comment_column = @max(comment_column, next_column(label.identifier.len + (if (label.type == .@"var") @as(usize, 5) else 1)));
            },
            .empty => unreachable,
        }
    }
    const effect_column = next_column(operand_column + operand_width);
    comment_column = @max(comment_column, next_column(effect_column + effect_width));

    entries = start;
    constant_width = 0;
    for (0..length) |index| {
        const before = entries;
        const entry = entries.next().?;
        if (entry == .row and entry.row.line == .constant) {
            if (constant_width == 0) constant_width = constant_block_width(before, length - index);
        } else constant_width = 0;
        switch (entry) {
            .blank => try writer.writeByte('\n'),
            .comment => |comment| {
                try writer.splatByteAll(' ', comment.indent);
                try writer.print("{s}\n", .{comment.text});
            },
            .row => |row| {
                var column: usize = 0;
                if (row.label) |label| try write_label(writer, &column, label);
                switch (row.line) {
                    .label => |label| try write_label(writer, &column, label),
                    .constant => |con| {
                        try write_text(writer, &column, "const ");
                        try write_text(writer, &column, con.identifier);
                        const padding = constant_width - con.identifier.len;
                        try writer.splatByteAll(' ', padding);
                        column += padding;
                        try write_text(writer, &column, " = ");
                        try row.content().write(writer, &column);
                    },
                    .instruction => |instr| {
                        const directive = is_directive(instr);
                        if (instr.condition) |condition| {
                            try pad_to(writer, &column, condition_column);
                            const formatted: Condition = .{ .condition = condition };
                            try writer.print("{f}", .{formatted});
                            column += std.fmt.count("{f}", .{formatted});
                        }
                        if (directive) {
                            if (instr.condition != null) try pad_to(writer, &column, next_column(column));
                        } else try pad_to(writer, &column, mnemonic_column);
                        if (directive) {
                            try write_text(writer, &column, instr.mnemonic);
                        } else {
                            for (instr.mnemonic) |char| try writer.writeByte(std.ascii.toUpper(char));
                            column += instr.mnemonic.len;
                        }
                        if (instr.arguments.len > 0) {
                            const argument_column = if (directive) next_column(column) else operand_column;
                            try pad_to(writer, &column, argument_column);
                            var content = row.content();
                            if (!directive) content.columns = columns;
                            try content.write(writer, &column);
                        }
                        if (instr.effect) |effect| {
                            try pad_to(writer, &column, if (directive) next_column(column) else effect_column);
                            try writer.print(":{t}", .{effect.type});
                            column += std.fmt.count(":{t}", .{effect.type});
                        }
                    },
                    .empty => unreachable,
                }
                if (row.trailing_comment) |comment| {
                    const directive = row.line == .instruction and is_directive(row.line.instruction);
                    try pad_to(writer, &column, if (directive) next_column(column) else comment_column);
                    try write_text(writer, &column, comment);
                }
                try writer.writeByte('\n');
            },
        }
    }
}

fn constant_block_width(start: Entries, length: usize) usize {
    var entries = start;
    var width: usize = 0;
    for (0..length) |_| {
        const entry = entries.next().?;
        if (entry != .row or entry.row.line != .constant) break;
        width = @max(width, entry.row.line.constant.identifier.len);
    }
    return width;
}

pub fn pretty_print_expr(writer: *std.Io.Writer, expr: ast.Expression) !void {
    var printer: ExpressionPrinter = .{ .writer = writer, .comments = &.{} };
    try printer.expression(expr);
}

fn is_directive(instr: ast.Instruction) bool {
    return std.mem.startsWith(u8, instr.mnemonic, ".");
}

fn pad_to(writer: *std.Io.Writer, column: *usize, target: usize) !void {
    const count = @max(target, next_column(column.*)) - column.*;
    try writer.splatByteAll(' ', count);
    column.* += count;
}

fn next_column(column: usize) usize {
    return std.mem.alignForward(usize, column + 1, 4);
}

fn write_label(writer: *std.Io.Writer, column: *usize, label: ast.Label) !void {
    if (label.type == .@"var") try write_text(writer, column, "var ");
    try write_text(writer, column, label.identifier);
    try write_text(writer, column, ":");
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
    columns: ?OperandColumns = null,
    expression: ?ast.Expression = null,

    fn width(self: Content) usize {
        var last_line_start: usize = 0;
        var measured = self;
        measured.last_line_start = &last_line_start;
        const total = std.fmt.count("{f}", .{measured});
        return total - last_line_start - (if (last_line_start > 0) self.continuation else 0);
    }

    fn write(self: Content, writer: *std.Io.Writer, column: *usize) !void {
        var formatted = self;
        formatted.continuation = column.*;
        formatted.end_column = column;
        try writer.print("{f}", .{formatted});
    }

    pub fn format(self: Content, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        var printer: ExpressionPrinter = .{ .writer = writer, .comments = self.comments, .continuation = self.continuation };
        if (self.expression) |expression| {
            try printer.expression(expression);
        } else switch (self.line) {
            .constant => |con| try printer.expression(con.value),
            .instruction => |instr| {
                var argument_offset: usize = 0;
                for (instr.arguments, 0..) |arg, index| {
                    if (index > 0) {
                        try printer.write(",");
                        if (self.columns != null) {
                            try printer.pad_to(self.continuation + argument_offset);
                        } else try printer.write(" ");
                    }
                    try printer.expression(arg);
                    if (self.columns) |columns| {
                        if (index + 1 < instr.arguments.len)
                            argument_offset = next_column(columns.first + argument_offset + columns.width(index) + 1) - columns.first;
                    }
                }
            },
            else => unreachable,
        }
        try printer.finish();
        if (self.last_line_start) |start| start.* = printer.last_line_start;
        if (self.end_column) |column|
            column.* = printer.written - printer.last_line_start + (if (printer.last_line_start == 0) self.continuation else 0);
    }
};

const OperandColumns = struct {
    start: Entries,
    length: usize,
    first: usize,

    fn width(self: OperandColumns, index: usize) usize {
        var entries = self.start;
        var width_value: usize = 0;
        for (0..self.length) |_| {
            const entry = entries.next().?;
            if (entry != .row or entry.row.line != .instruction) continue;
            const instr = entry.row.line.instruction;
            if (is_directive(instr) or instr.arguments.len <= index + 1) continue;
            const content: Content = .{ .line = entry.row.line, .comments = &.{}, .expression = instr.arguments[index] };
            width_value = @max(width_value, content.width());
        }
        return width_value;
    }
};

const ExpressionPrinter = struct {
    writer: *std.Io.Writer,
    comments: []const ast.Comment,
    next_comment: usize = 0,
    indent: usize = 0,
    at_line_start: bool = true,
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
        }
    }

    fn newline(self: *ExpressionPrinter) std.Io.Writer.Error!void {
        if (!self.at_line_start) try self.write("\n");
    }

    fn before(self: *ExpressionPrinter, location: ast.Location) std.Io.Writer.Error!void {
        while (self.next_comment < self.comments.len and location_before(self.comments[self.next_comment].span.location(), location)) {
            if (!self.at_line_start) try self.pad_to(next_column(self.column()));
            try self.write(self.comments[self.next_comment].text);
            try self.write("\n");
            self.next_comment += 1;
        }
    }

    fn finish(self: *ExpressionPrinter) std.Io.Writer.Error!void {
        while (self.next_comment < self.comments.len) {
            if (!self.at_line_start) try self.pad_to(next_column(self.column()));
            try self.write(self.comments[self.next_comment].text);
            try self.write("\n");
            self.next_comment += 1;
        }
    }

    fn column(self: ExpressionPrinter) usize {
        return self.written - self.last_line_start + (if (self.last_line_start == 0) self.continuation else 0);
    }

    fn pad_to(self: *ExpressionPrinter, target: usize) std.Io.Writer.Error!void {
        const count = @max(target, next_column(self.column())) - self.column();
        for (0..count) |_| try self.write(" ");
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
