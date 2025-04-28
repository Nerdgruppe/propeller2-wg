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

            .instruction => |instr| {
                try writer.writeAll("    ");
                const c_strings: [2][]const u8 = .{ "!C", "C" };
                const z_strings: [2][]const u8 = .{ "!Z", "Z" };
                if (instr.condition) |cond| {
                    switch (cond.type) {
                        .@"return" => try writer.writeAll("return"),
                        .c_is_z => try writer.writeAll("if(C == Z)"),
                        .c_is_not_z => try writer.writeAll("if(C != Z)"),

                        .c_is => |val| try writer.print("if({s})", .{c_strings[@intFromBool(val)]}),

                        .z_is => |val| try writer.print("if({s})", .{z_strings[@intFromBool(val)]}),

                        .c_and_z => |val| try writer.print("if({s} & {s})", .{
                            c_strings[@intFromBool(val.c)],
                            z_strings[@intFromBool(val.z)],
                        }),

                        .c_or_z => |val| try writer.print("if({s} | {s})", .{
                            c_strings[@intFromBool(val.c)],
                            z_strings[@intFromBool(val.z)],
                        }),

                        .comparison => |comp| try writer.print("if({s})", .{@tagName(comp)}),
                    }
                    try writer.writeAll(" ");
                }

                try writer.writeAll(instr.mnemonic);

                for (instr.arguments, 0..) |arg, i| {
                    if (i > 0) {
                        try writer.writeAll(",");
                    }
                    try writer.writeAll(" ");

                    try pretty_print_expr(writer, arg);
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

        .wrapped => |inner| {
            try writer.writeAll("(");
            try pretty_print_expr(writer, inner.*);
            try writer.writeAll(")");
        },

        .function_call => |func| {
            try writer.writeAll(func.function);
            try writer.writeAll("(");
            if (func.has_trailing_comma) {
                for (func.arguments) |arg| {
                    try writer.writeAll("\n        ");

                    if (arg.name) |name| {
                        try writer.print("{s}=", .{name});
                    }

                    try pretty_print_expr(writer, arg.value);

                    try writer.writeAll(",");
                }
                try writer.writeAll("\n    )");
            } else {
                for (func.arguments, 0..) |arg, i| {
                    if (i > 0) {
                        try writer.writeAll(", ");
                    }

                    if (arg.name) |name| {
                        try writer.print("{s}=", .{name});
                    }

                    try pretty_print_expr(writer, arg.value);
                }
                try writer.writeAll(")");
            }
        },
    }
}
