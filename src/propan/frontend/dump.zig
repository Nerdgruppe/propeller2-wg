const std = @import("std");
const ast = @import("ast.zig");

pub fn dump_ast(raw_writer: *std.Io.Writer, file: ast.File) !void {
    var stream: IndentingStream = .{ .inner = raw_writer };
    defer std.debug.assert(stream.indent == 0);

    const writer = &stream.interface;

    try writer.writeAll("ast:\n");

    stream.push();
    defer stream.pop();

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

                stream.push();
                defer stream.pop();

                try writer.writeAll("id:    '");
                try writer.writeAll(con.identifier);
                try writer.writeAll("'\nvalue:\n");
                try pretty_print_expr(&stream, con.value);
            },

            .instruction => |instr| {
                try writer.writeAll("instruction\n");

                stream.push();
                defer stream.pop();

                if (instr.condition) |cond| {
                    try writer.writeAll("condition: ");

                    switch (cond.type) {
                        .@"return" => try writer.writeAll("return\n"),
                        .c_is_z => try writer.writeAll("C == Z\n"),
                        .c_is_not_z => try writer.writeAll("C != Z\n"),

                        .c_is => |val| try writer.print("C={}\n", .{val}),
                        .z_is => |val| try writer.print("Z={}\n", .{val}),

                        .c_and_z, .c_or_z => |val| {
                            try writer.print("{t}\n", .{cond.type});

                            stream.push();
                            defer stream.pop();

                            try writer.print("C: {}", .{val.c});
                            try writer.print("Z: {}", .{val.z});
                        },

                        .comparison => |comp| try writer.print("{t}\n", .{comp}),
                    }
                }

                try writer.print("mnemonic: {s}\n", .{instr.mnemonic});

                if (instr.arguments.len > 0) {
                    try writer.writeAll("arguments\n");

                    stream.push();
                    defer stream.pop();

                    for (instr.arguments) |arg| {
                        try pretty_print_expr(&stream, arg);
                    }
                }

                if (instr.effect) |effect| {
                    try writer.print(" :{t}", .{effect.type});
                }

                try writer.writeAll("\n");
            },
        }
    }
}

fn pretty_print_expr(stream: *IndentingStream, expr: ast.Expression) !void {
    const writer = &stream.interface;
    stream.push();
    defer stream.pop();

    switch (expr) {
        .current_pc => try writer.writeAll("current PC: $\n"),
        .integer => |int| {
            try writer.print("integer: {d} \"{f}\"\n", .{ int.value, std.zig.fmtString(int.source_text) });
        },

        .symbol => |sym| {
            try writer.print("symbol: '{s}'\n", .{sym.symbol_name});
        },

        .enumerator => |sym| try writer.print("enumerator: '#{s}'\n", .{sym.symbol_name}),

        .string => |str| {
            try writer.print("string: \"{f}\" \"{f}\"\n", .{ std.zig.fmtString(str.value), std.zig.fmtString(str.source_text) });
        },

        .sequence => |seq| {
            try writer.writeAll("sequence\n");
            for (seq.items) |item| try pretty_print_expr(stream, item);
        },

        .wrapped => |inner| {
            try writer.writeAll("wrapped\n");
            try pretty_print_expr(stream, inner.value.*);
        },

        .unary_transform => |op| {
            try writer.writeAll("unary\n");

            stream.push();
            defer stream.pop();

            try writer.print("op: {t}\n", .{op.operator});
            try writer.writeAll("value:\n");

            try pretty_print_expr(stream, op.value.*);
        },

        .binary_transform => |op| {
            try writer.writeAll("binary\n");

            stream.push();
            defer stream.pop();

            try writer.print("op: {t}\n", .{op.operator});

            try writer.writeAll("lhs:\n");
            try pretty_print_expr(stream, op.lhs.*);

            try writer.writeAll("rhs:\n");
            try pretty_print_expr(stream, op.rhs.*);
        },

        .function_call => |func| {
            try writer.writeAll("fncall\n");

            stream.push();
            defer stream.pop();

            try writer.print("function: {s}\n", .{func.function});
            try writer.print("trailing: {}\n", .{func.has_trailing_comma});

            if (func.arguments.len > 0) {
                try writer.writeAll("args\n");

                stream.push();
                defer stream.pop();

                for (func.arguments) |arg| {
                    try writer.writeAll("arg\n");
                    stream.push();
                    defer stream.pop();

                    try writer.print("name: {?s}\n", .{arg.name});

                    try writer.writeAll("value\n");
                    try pretty_print_expr(stream, arg.value);
                }
            }
        },
    }
}

const IndentingStream = struct {
    inner: *std.Io.Writer,
    indent: usize = 0,
    indent_str: []const u8 = "    ",

    head_of_line: bool = true,
    interface: std.Io.Writer = .{
        .vtable = &.{
            .drain = drain,
            .flush = std.Io.Writer.noopFlush,
            .rebase = std.Io.Writer.failingRebase,
        },
        .buffer = &.{},
    },

    pub fn push(is: *@This()) void {
        is.indent += 1;
    }

    pub fn pop(is: *@This()) void {
        is.indent -= 1;
    }

    fn drain(io_writer: *std.Io.Writer, data: []const []const u8, splat: usize) std.Io.Writer.Error!usize {
        const is: *@This() = @alignCast(@fieldParentPtr("interface", io_writer));
        var written: usize = 0;
        for (data[0 .. data.len - 1]) |buffer| {
            _ = try is.write(buffer);
            written += buffer.len;
        }
        for (0..splat) |_| {
            const buffer = data[data.len - 1];
            _ = try is.write(buffer);
            written += buffer.len;
        }
        return written;
    }

    fn write(is: *@This(), buffer: []const u8) std.Io.Writer.Error!usize {
        var pos: usize = 0;
        while (std.mem.indexOfScalarPos(u8, buffer, pos, '\n')) |index| {
            try is.append(buffer[pos..index], true);
            pos = index + 1;
        }

        try is.append(buffer[pos..], false);

        return buffer.len;
    }

    fn append(is: *@This(), buffer: []const u8, eol: bool) std.Io.Writer.Error!void {
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
