const std = @import("std");
const ast = @import("ast.zig");

const unary_precedence: u8 = 6;

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
        .current_pc => try writer.writeAll("$"),
        .integer => |int| {
            try writer.writeAll(int.source_text);
        },

        .symbol => |sym| {
            try writer.writeAll(sym.symbol_name);
        },

        .enumerator => |sym| try writer.print("#{s}", .{sym.symbol_name}),

        .string => |str| {
            try writer.writeAll(str.source_text);
        },

        .sequence => |seq| {
            try writer.writeAll("[");
            for (seq.items, 0..) |item, i| {
                if (i != 0) try writer.writeAll(", ");
                try pretty_print_expr(writer, item);
            }
            try writer.writeAll("]");
        },

        .wrapped => |inner| {
            try writer.writeAll("(");
            try pretty_print_expr(writer, inner.*);
            try writer.writeAll(")");
        },

        .unary_transform => |op| {
            switch (op.operator) {
                .post_increment, .post_decrement => {
                    try pretty_print_expr(writer, op.value.*);
                    try writer.writeAll(if (op.operator == .post_increment) "++" else "--");
                },
                else => {
                    try writer.writeAll(switch (op.operator) {
                        .pre_increment => "++",
                        .pre_decrement => "--",
                        else => @tagName(op.operator),
                    });
                    try pretty_print_operand(writer, op.value.*, unary_precedence, false);
                },
            }
        },

        .binary_transform => |op| {
            const parent_precedence = binary_precedence(op.operator);
            try pretty_print_operand(writer, op.lhs.*, parent_precedence, false);
            if (op.operator == .array_index) {
                try writer.writeAll("[");
                try pretty_print_expr(writer, op.rhs.*);
                try writer.writeAll("]");
            } else {
                try writer.print(" {s} ", .{@tagName(op.operator)});
                try pretty_print_operand(writer, op.rhs.*, parent_precedence, true);
            }
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

fn pretty_print_operand(writer: anytype, expr: ast.Expression, parent_precedence: u8, is_rhs: bool) anyerror!void {
    const needs_parens = switch (expr) {
        .binary_transform => |binary| blk: {
            const child_precedence = binary_precedence(binary.operator);
            break :blk child_precedence < parent_precedence or
                (is_rhs and child_precedence == parent_precedence);
        },
        else => false,
    };
    if (needs_parens) try writer.writeAll("(");
    try pretty_print_expr(writer, expr);
    if (needs_parens) try writer.writeAll(")");
}

test {
    _ = @import("render_tests.zig");
}
