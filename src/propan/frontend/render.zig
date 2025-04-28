const std = @import("std");
const ast = @import("ast.zig");

pub fn pretty_print(writer: anytype, file: ast.File) !void {
    for (file.sequence) |node| {
        switch (node) {
            .empty => try writer.writeAll("\n"),

            .label => |lbl| switch (lbl.type) {
                .code => try writer.print("{s}:\n", .{lbl.identifier}),
                .@"var" => try writer.print("var {s}:\n", .{lbl.identifier}),
            },

            .constant => |con| {
                try writer.writeAll("const ");
                try writer.writeAll(con.identifier);
                try writer.writeAll(" = ");
                try pretty_print_expr(writer, con.value);
                try writer.writeAll("\n");
            },

            .instruction => {
                try writer.print("<instruction>\n", .{});
            },
        }
    }
}

fn pretty_print_expr(writer: anytype, expr: ast.Expression) !void {
    switch (expr) {
        .integer => |int| {
            try writer.writeAll(int.source_text);
        },

        .symbol => |sym| {
            try writer.writeAll(sym.symbol_name);
        },

        .string => |str| {
            try writer.writeAll(str.source_text);
        },
    }
}
