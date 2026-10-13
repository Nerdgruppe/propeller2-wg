//! Canonical, independent-word PASM2 disassembly using the simulator decoder.
const std = @import("std");
const decode = @import("decode.zig");
const encoding = @import("encoding.zig");
const p2 = @import("p2");
const types = p2.types;

/// Write one instruction, without combining preceding AUGS/AUGD words.
/// Signed immediates display encoded displacements from fallthrough; long displacements are in bytes.
pub fn disassemble(word: u32, writer: *std.Io.Writer) !void {
    @setEvalBranchQuota(100_000);
    const opcode = decode.decode(word);
    if (opcode == .invalid) {
        try writer.print("INVALID ${X:0>8}", .{word});
        return;
    }
    const metadata = decode.metadata(opcode);
    const instruction: encoding.Instruction = .{ .raw = word };
    switch (opcode) {
        inline else => |tag| {
            const field = comptime decode.instruction_type.get(tag);
            if (comptime field.len == 0)
                unreachable;
            const args = @field(instruction, field);
            const T = @TypeOf(args);
            if (@hasField(T, "cond") and args.cond != .IF_ALWAYS) {
                try writer.print("{t} ", .{args.cond});
            }
            try writer.writeAll(metadata.mnemonic);
            for (metadata.operands, 0..) |operand, index| {
                if (opcode == .getrnd and word & (1 << 18) != 0)
                    continue;
                try writer.writeAll(if (index == 0) " " else ", ");
                switch (operand) {
                    .dest_either, .dest_reg => {
                        if (@hasField(T, "d")) {
                            const immediate = if (@hasField(T, "d_imm")) args.d_imm else false;
                            try register(writer, args.d, immediate);
                        }
                    },
                    .src_either, .src_either_pcrel, .ptrexpr => {
                        if (@hasField(T, "s")) {
                            if (operand == .ptrexpr and args.s_imm) {
                                try pointer(writer, @bitCast(@intFromEnum(args.s)));
                            } else if (operand == .src_either_pcrel and args.s_imm) {
                                const offset: i9 = @bitCast(@intFromEnum(args.s));
                                try writer.print("#{s}${X:0>3}", .{ if (offset < 0) "-" else "+", @abs(offset) });
                            } else {
                                try register(writer, args.s, args.s_imm);
                            }
                        }
                    },
                    .address => {
                        if (@hasField(T, "address")) {
                            if (args.relative) {
                                const offset: i20 = @bitCast(args.address);
                                try writer.print("#{s}${X:0>5}", .{ if (offset < 0) "-" else "+", @abs(offset) });
                            } else {
                                try writer.print("#\\${X:0>5}", .{args.address});
                            }
                        }
                    },
                    .augment => {
                        if (@hasField(T, "augment")) {
                            try writer.print("#${X:0>8}", .{@as(u32, args.augment) << 9});
                        }
                    },
                    .preg => {
                        if (@hasField(T, "pointer")) {
                            try writer.print("{t}", .{args.pointer});
                        }
                    },
                    .selector => {
                        if (@hasField(T, "n")) {
                            try writer.print("#{}", .{args.n});
                        }
                    },
                    .c_remap => {
                        if (@hasField(T, "c_value")) {
                            try flagExpression(writer, args.c_value);
                        }
                    },
                    .z_remap => {
                        if (@hasField(T, "z_value")) {
                            try flagExpression(writer, args.z_value);
                        }
                    },
                }
            }
            const c = if (@hasField(T, "c_mod")) args.c_mod == .write else false;
            const z = if (@hasField(T, "z_mod")) args.z_mod == .write else false;
            if (c or z) {
                try writer.print(" {s}{s}", .{ metadata.flag_prefix, if (c and z) "CZ" else if (c) "C" else "Z" });
            }
        },
    }
}

/// Format a literal or a register, preserving architectural special-register names.
fn register(writer: *std.Io.Writer, reg: types.Register, immediate: bool) !void {
    if (!immediate and @intFromEnum(reg) >= 0x1f0) {
        try writer.print("{t}", .{reg});
    } else {
        try writer.print("{s}${X:0>3}", .{ if (immediate) "#" else "", @intFromEnum(reg) });
    }
}

/// Use the canonical PASM2 truth-table operand names, including their leading underscore.
fn flagExpression(writer: *std.Io.Writer, expression: types.FlagExpression) !void {
    const names = [_][]const u8{
        "_CLR",     "_NC_AND_NZ", "_NC_AND_Z", "_NC",      "_C_AND_NZ", "_NZ",      "_C_NE_Z", "_NC_OR_NZ",
        "_C_AND_Z", "_C_EQ_Z",    "_Z",        "_NC_OR_Z", "_C",        "_C_OR_NZ", "_C_OR_Z", "_SET",
    };
    try writer.writeAll(names[@intFromEnum(expression)]);
}

/// Decode the nine-bit pointer expression without borrowing preceding augmentation state.
fn pointer(writer: *std.Io.Writer, expression: types.PointerExpr) !void {
    if (!expression.is_ptr_expr) {
        try writer.print("#${X:0>2}", .{expression.value.index});
        return;
    }
    const expr = expression.value.expr;
    if (expr.update) {
        const update = expr.index.update;
        const amount: i6 = if (update.offset == 0) 16 else update.offset;
        const sign = if (amount < 0) "--" else "++";
        try writer.print("{s}{t}{s}[{}]", .{
            if (update.post) "" else sign,
            expr.pointer,
            if (update.post) sign else "",
            @abs(amount),
        });
    } else {
        try writer.print("{t}[{}]", .{ expr.pointer, expr.index.offset });
    }
}

test "disassembly uses canonical fields and keeps augmentations independent" {
    const cases = .{
        .{ @as(u32, 0), "NOP" },
        .{ @as(u32, 0xf6040202), "MOV $001, #$002" },
        .{ @as(u32, 0x161c0202), "IF_NC_AND_NZ MOV $001, #$002 WCZ" },
        .{ @as(u32, 0xff000001), "AUGS #$00000200" },
        .{ @as(u32, 0xfd600002), "INVALID $FD600002" },
        .{ @as(u32, 0xf60003f8), "MOV $001, PTRA" },
        .{ @as(u32, 0xfb042900), "RDLONG $014, PTRA[0]" },
        .{ @as(u32, 0xfb04297f), "RDLONG $014, PTRA--[1]" },
        .{ @as(u32, 0xfc0c0403), "WRPIN #$002, #$003" },
        .{ @as(u32, 0xf8fc0403), "GETBYTE $002, #$003, #3" },
        .{ @as(u32, 0xfd900004), "JMP #+$00004" },
        .{ @as(u32, 0xfd800400), "JMP #\\$00400" },
        .{ @as(u32, 0xfe800400), "LOC PA, #\\$00400" },
        .{ @as(u32, 0xfb6c03fe), "DJNZ $001, #-$002" },
        .{ @as(u32, 0xf4500403), "TESTB $002, $003 ANDC" },
        .{ @as(u32, 0xfd7c001b), "GETRND WCZ" },
        .{ @as(u32, 0xfd7c026f), "MODCZ _CLR, _NC_AND_NZ WCZ" },
        .{ @as(u32, 0x06040202), "_RET_ MOV $001, #$002" },
    };
    inline for (cases) |case| {
        var buffer: [256]u8 = undefined;
        var writer = std.Io.Writer.fixed(&buffer);
        try disassemble(case[0], &writer);
        try std.testing.expectEqualStrings(case[1], writer.buffered());
    }
}
