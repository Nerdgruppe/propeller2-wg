const std = @import("std");
const ast = @import("ast.zig");

pub fn dump_ast(raw_writer: anytype, file: ast.File) !void {
    var _writer = IndentingStream(@TypeOf(raw_writer)){ .inner = raw_writer };
    defer std.debug.assert(_writer.indent == 0);

    const writer = &_writer;

    try writer.writeAll("ast:\n");

    writer.push();
    defer writer.pop();

    for (file.sequence, 0..) |node, i| {
        if (i > 0)
            try writer.writeAll("\n");
        switch (node) {
            .empty => try writer.writeAll("empty\n"),

            .label => |lbl| switch (lbl.type) {
                .code => try writer.print("code-label: {s}\n", .{lbl.identifier}),
                .@"var" => try writer.print("data-label: {s}\n", .{lbl.identifier}),
            },

            .constant => |con| {
                try writer.writeAll("constant\n");

                writer.push();
                defer writer.pop();

                try writer.writeAll("id:    '");
                try writer.writeAll(con.identifier);
                try writer.writeAll("'\nvalue:\n");
                try pretty_print_expr(writer, con.value);
            },

            .instruction => |instr| {
                try writer.writeAll("instruction\n");

                writer.push();
                defer writer.pop();

                if (instr.condition) |cond| {
                    try writer.writeAll("condition: ");

                    switch (cond.type) {
                        .@"return" => try writer.writeAll("return\n"),
                        .c_is_z => try writer.writeAll("C == Z\n"),
                        .c_is_not_z => try writer.writeAll("C != Z\n"),

                        .c_is => |val| try writer.print("C={}\n", .{val}),
                        .z_is => |val| try writer.print("Z={}\n", .{val}),

                        .c_and_z, .c_or_z => |val| {
                            try writer.print("{s}\n", .{@tagName(cond.type)});

                            writer.push();
                            defer writer.pop();

                            try writer.print("C: {}", .{val.c});
                            try writer.print("Z: {}", .{val.z});
                        },

                        .comparison => |comp| try writer.print("{s}\n", .{@tagName(comp)}),
                    }
                }

                try writer.print("mnemonic: {s}\n", .{instr.mnemonic});

                if (instr.arguments.len > 0) {
                    try writer.writeAll("arguments\n");

                    writer.push();
                    defer writer.pop();

                    for (instr.arguments) |arg| {
                        try pretty_print_expr(writer, arg);
                    }
                }

                if (instr.effect) |effect| {
                    try writer.print(" :{s}", .{@tagName(effect)});
                }

                try writer.writeAll("\n");
            },
        }
    }
}

fn pretty_print_expr(writer: anytype, expr: ast.Expression) !void {
    writer.push();
    defer writer.pop();

    switch (expr) {
        .integer => |int| {
            try writer.print("integer: {d} \"{}\"\n", .{ int.value, std.zig.fmtEscapes(int.source_text) });
        },

        .symbol => |sym| {
            try writer.print("symbol: '{s}'\n", .{sym.symbol_name});
        },

        .string => |str| {
            try writer.print("integer: \"{}\" \"{}\"\n", .{ std.zig.fmtEscapes(str.value), std.zig.fmtEscapes(str.source_text) });
        },

        .wrapped => |inner| {
            try writer.writeAll("wrapped\n");
            try pretty_print_expr(writer, inner.*);
        },

        .unary_transform => |op| {
            try writer.writeAll("unary\n");

            writer.push();
            defer writer.pop();

            try writer.print("op: {s}\n", .{@tagName(op.operator)});
            try writer.writeAll("value:\n");

            try pretty_print_expr(writer, op.value.*);
        },

        .binary_transform => |op| {
            try writer.writeAll("binary\n");

            writer.push();
            defer writer.pop();

            try writer.print("op: {s}\n", .{@tagName(op.operator)});

            try writer.writeAll("lhs:\n");
            try pretty_print_expr(writer, op.lhs.*);

            try writer.writeAll("rhs:\n");
            try pretty_print_expr(writer, op.rhs.*);
        },

        .function_call => |func| {
            try writer.writeAll("fncall\n");

            writer.push();
            defer writer.pop();

            try writer.print("function: {s}\n", .{func.function});
            try writer.print("trailing: {}\n", .{func.has_trailing_comma});

            if (func.arguments.len > 0) {
                try writer.writeAll("args\n");

                writer.push();
                defer writer.pop();

                for (func.arguments) |arg| {
                    try writer.writeAll("arg\n");
                    writer.push();
                    defer writer.pop();

                    try writer.print("name: {?s}\n", .{arg.name});

                    try writer.writeAll("value\n");
                    try pretty_print_expr(writer, arg.value);
                }
            }
        },
    }
}

fn IndentingStream(InnerWriter: type) type {
    return struct {
        inner: InnerWriter,
        indent: usize = 0,
        indent_str: []const u8 = "    ",

        head_of_line: bool = true,

        pub const Writer = std.io.Writer(*@This(), InnerWriter.Error, write);

        pub fn writer(is: *@This()) Writer {
            return .{ .context = is };
        }

        pub fn push(is: *@This()) void {
            is.indent += 1;
        }

        pub fn pop(is: *@This()) void {
            is.indent -= 1;
        }

        pub fn writeAll(is: *@This(), buffer: []const u8) !void {
            try is.writer().writeAll(buffer);
        }

        pub fn print(is: *@This(), comptime fmt: []const u8, args: anytype) !void {
            try is.writer().print(fmt, args);
        }

        fn write(is: *@This(), buffer: []const u8) InnerWriter.Error!usize {
            var pos: usize = 0;
            while (std.mem.indexOfScalarPos(u8, buffer, pos, '\n')) |index| {
                try is.append(buffer[pos..index], true);
                pos = index + 1;
            }

            try is.append(buffer[pos..], false);

            return buffer.len;
        }

        fn append(is: *@This(), buffer: []const u8, eol: bool) InnerWriter.Error!void {
            if (buffer.len == 0 and !eol)
                return;
            if (is.head_of_line) {
                for (0..is.indent) |_| {
                    try is.inner.writeAll(is.indent_str);
                }
                is.head_of_line = false;
            }

            try is.inner.writeAll(buffer);

            if (eol) {
                is.head_of_line = true;
                try is.inner.writeAll("\n");
            }
        }
    };
}
