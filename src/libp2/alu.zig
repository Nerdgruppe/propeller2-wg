//!
//! This file implements the basic atomic arithmetic operations of the Propeller 2
//! as pure implementations.
//! Flag write enables are applied by the caller. Instructions without flag
//! effects preserve the input flags, and comparisons/tests preserve D.
//!

const std = @import("std");

const types = @import("types.zig");

const Flag = types.Flag;
const Decomposition = types.Decomposition;

/// The most generic input of an ALU instruction.
pub const Input = struct {
    d: u32,
    s: u32,
    c: Flag,
    z: Flag,
    q: u32,
    setq_prefix: bool, // if true, this instruction was prefixed with a SETQ

    /// C = parity of result. Z = (result = 0)
    fn parity_zero_output(input: Input, result: u32) Output {
        return .{
            .result = result,
            .c = .from_parity(result),
            .z = .from_zero(result),
            .q = input.q,
        };
    }

    fn zero_output(input: Input, result: u32, c: Flag) Output {
        return .{ .result = result, .c = c, .z = .from_zero(result), .q = input.q };
    }

    fn unchanged_flags_output(input: Input, result: u32) Output {
        return .{ .result = result, .c = input.c, .z = input.z, .q = input.q };
    }

    fn msb_zero_output(input: Input, result: u32) Output {
        return input.zero_output(result, .from_msb(result));
    }

    fn right_shift_output(input: Input, result: u32) Output {
        const shift = input.bit_index();
        return input.zero_output(result, .from_lsb(input.d >> (if (shift == 0) 0 else shift - 1)));
    }

    fn left_shift_output(input: Input, result: u32) Output {
        const shift = input.bit_index();
        return input.zero_output(result, .from_msb(input.d << (if (shift == 0) 0 else shift - 1)));
    }

    fn bit_output(input: Input, result: u32) Output {
        const bit = Flag.from_lsb(input.d >> input.bit_index());
        return .{ .result = result, .c = bit, .z = bit, .q = input.q };
    }

    fn bit_index(input: Input) u5 {
        const bits: types.BitIndexAndCount = @bitCast(input.s);
        return bits.index;
    }

    fn bit_mask(input: Input) u32 {
        const bits: types.BitIndexAndCount = @bitCast(input.s);
        const extra: u5 = if (input.setq_prefix) @truncate(input.q) else bits.count;
        return std.math.rotl(u32, ~@as(u32, 0) >> (31 - extra), bits.index);
    }

    fn write_bits(input: Input, value: u32) Output {
        const mask = input.bit_mask();
        return input.bit_output((input.d & ~mask) | (value & mask));
    }
};

/// The most generic output of an ALU instruction.
pub const Output = struct {
    result: u32,
    c: Flag,
    z: Flag,
    q: u32,
    /// Source substitution for the next instruction (SCA, SCAS, XORO32).
    next_s: ?u32 = null,
};

fn arithmetic(input: Input, comptime signed: bool, comptime subtract: bool, comptime extended: bool) Output {
    const d: i64 = if (signed) @as(i32, @bitCast(input.d)) else input.d;
    const s: i64 = if (signed) @as(i32, @bitCast(input.s)) else input.s;
    const operand = s + (if (extended) input.c.as_int(i64) else 0);
    const value = if (subtract) d - operand else d + operand;
    const result: u32 = @truncate(@as(u64, @bitCast(value)));
    var out = input.zero_output(result, .from_bool(if (signed or subtract) value < 0 else value > 0xFFFF_FFFF));
    if (extended) out.z = .from_bool(input.z == .set and result == 0);
    return out;
}

// * Z = (result == 0).
// ROR     D,{#}S   {WC/WZ/WCZ}               | Rotate right.           D = [31:0]  of ({D[31:0], D[31:0]}     >> S[4:0]). C = last bit shifted out if S[4:0] > 0, else D[0].  *
pub fn ROR(input: Input) Output {
    return input.right_shift_output(std.math.rotr(u32, input.d, input.bit_index()));
}

// ROL     D,{#}S   {WC/WZ/WCZ}               | Rotate left.            D = [63:32] of ({D[31:0], D[31:0]}     << S[4:0]). C = last bit shifted out if S[4:0] > 0, else D[31]. *
pub fn ROL(input: Input) Output {
    return input.left_shift_output(std.math.rotl(u32, input.d, input.bit_index()));
}

// SHR     D,{#}S   {WC/WZ/WCZ}               | Shift right.            D = [31:0]  of ({32'b0, D[31:0]}       >> S[4:0]). C = last bit shifted out if S[4:0] > 0, else D[0].  *
pub fn SHR(input: Input) Output {
    return input.right_shift_output(input.d >> input.bit_index());
}

// SHL     D,{#}S   {WC/WZ/WCZ}               | Shift left.             D = [63:32] of ({D[31:0], 32'b0}       << S[4:0]). C = last bit shifted out if S[4:0] > 0, else D[31]. *
pub fn SHL(input: Input) Output {
    return input.left_shift_output(input.d << input.bit_index());
}

// RCR     D,{#}S   {WC/WZ/WCZ}               | Rotate carry right.     D = [31:0]  of ({{32{C}}, D[31:0]}     >> S[4:0]). C = last bit shifted out if S[4:0] > 0, else D[0].  *
pub fn RCR(input: Input) Output {
    const shift = input.bit_index();
    const value = (@as(u64, input.c.as_mask(u32)) << 32) | input.d;
    return input.right_shift_output(@truncate(value >> shift));
}

// RCL     D,{#}S   {WC/WZ/WCZ}               | Rotate carry left.      D = [63:32] of ({D[31:0], {32{C}}}     << S[4:0]). C = last bit shifted out if S[4:0] > 0, else D[31]. *
pub fn RCL(input: Input) Output {
    const shift = input.bit_index();
    const value = (@as(u64, input.d) << 32) | input.c.as_mask(u32);
    return input.left_shift_output(@truncate((value << shift) >> 32));
}

// SAR     D,{#}S   {WC/WZ/WCZ}               | Shift arithmetic right. D = [31:0]  of ({{32{D[31]}}, D[31:0]} >> S[4:0]). C = last bit shifted out if S[4:0] > 0, else D[0].  *
pub fn SAR(input: Input) Output {
    const d: i32 = @bitCast(input.d);
    return input.right_shift_output(@bitCast(d >> input.bit_index()));
}

// SAL     D,{#}S   {WC/WZ/WCZ}               | Shift arithmetic left.  D = [63:32] of ({D[31:0], {32{D[0]}}}  << S[4:0]). C = last bit shifted out if S[4:0] > 0, else D[31]. *
pub fn SAL(input: Input) Output {
    const shift = input.bit_index();
    const value = (@as(u64, input.d) << 32) | Flag.from_lsb(input.d).as_mask(u32);
    return input.left_shift_output(@truncate((value << shift) >> 32));
}

// ADD     D,{#}S   {WC/WZ/WCZ}               | Add S into D.                                  D = D + S.        C = carry of (D + S).               *
pub fn ADD(input: Input) Output {
    return arithmetic(input, false, false, false);
}

// ADDX    D,{#}S   {WC/WZ/WCZ}               | Add (S + C) into D, extended.                  D = D + S + C.    C = carry of (D + S + C).           Z = Z AND (result == 0).
pub fn ADDX(input: Input) Output {
    return arithmetic(input, false, false, true);
}

// ADDS    D,{#}S   {WC/WZ/WCZ}               | Add S into D, signed.                          D = D + S.        C = correct sign of (D + S).        *
pub fn ADDS(input: Input) Output {
    return arithmetic(input, true, false, false);
}

// ADDSX   D,{#}S   {WC/WZ/WCZ}               | Add (S + C) into D, signed and extended.       D = D + S + C.    C = correct sign of (D + S + C).    Z = Z AND (result == 0).
pub fn ADDSX(input: Input) Output {
    return arithmetic(input, true, false, true);
}

// SUB     D,{#}S   {WC/WZ/WCZ}               | Subtract S from D.                             D = D - S.        C = borrow of (D - S).              *
pub fn SUB(input: Input) Output {
    return arithmetic(input, false, true, false);
}

// SUBX    D,{#}S   {WC/WZ/WCZ}               | Subtract (S + C) from D, extended.             D = D - (S + C).  C = borrow of (D - (S + C)).        Z = Z AND (result == 0).
pub fn SUBX(input: Input) Output {
    return arithmetic(input, false, true, true);
}

// SUBS    D,{#}S   {WC/WZ/WCZ}               | Subtract S from D, signed.                     D = D - S.        C = correct sign of (D - S).        *
pub fn SUBS(input: Input) Output {
    return arithmetic(input, true, true, false);
}

// SUBSX   D,{#}S   {WC/WZ/WCZ}               | Subtract (S + C) from D, signed and extended.  D = D - (S + C).  C = correct sign of (D - (S + C)).  Z = Z AND (result == 0).
pub fn SUBSX(input: Input) Output {
    return arithmetic(input, true, true, true);
}

// CMP     D,{#}S   {WC/WZ/WCZ}               | Compare D to S.                                                  C = borrow of (D - S).              Z = (D == S).
pub fn CMP(input: Input) Output {
    var out = SUB(input);
    out.result = input.d;
    return out;
}

// CMPX    D,{#}S   {WC/WZ/WCZ}               | Compare D to (S + C), extended.                                  C = borrow of (D - (S + C)).        Z = Z AND (D == S + C).
pub fn CMPX(input: Input) Output {
    var out = SUBX(input);
    out.result = input.d;
    return out;
}

// CMPS    D,{#}S   {WC/WZ/WCZ}               | Compare D to S, signed.                                          C = correct sign of (D - S).        Z = (D == S).
pub fn CMPS(input: Input) Output {
    var out = SUBS(input);
    out.result = input.d;
    return out;
}

// CMPSX   D,{#}S   {WC/WZ/WCZ}               | Compare D to (S + C), signed and extended.                       C = correct sign of (D - (S + C)).  Z = Z AND (D == S + C).
pub fn CMPSX(input: Input) Output {
    var out = SUBSX(input);
    out.result = input.d;
    return out;
}

// CMPR    D,{#}S   {WC/WZ/WCZ}               | Compare S to D (reverse).                                        C = borrow of (S - D).              Z = (D == S).
pub fn CMPR(input: Input) Output {
    var out = SUBR(input);
    out.result = input.d;
    return out;
}

// CMPM    D,{#}S   {WC/WZ/WCZ}               | Compare D to S, get MSB of difference into C.                    C = MSB of (D - S).                 Z = (D == S).
pub fn CMPM(input: Input) Output {
    const difference = input.d -% input.s;
    return .{ .result = input.d, .c = .from_msb(difference), .z = .from_zero(difference), .q = input.q };
}

// SUBR    D,{#}S   {WC/WZ/WCZ}               | Subtract D from S (reverse).                   D = S - D.        C = borrow of (S - D).              *
pub fn SUBR(input: Input) Output {
    var reversed = input;
    reversed.d = input.s;
    reversed.s = input.d;
    return SUB(reversed);
}

// CMPSUB  D,{#}S   {WC/WZ/WCZ}               | Compare and subtract S from D if D >= S. If D => S then D = D - S and C = 1, else D same and C = 0.  *
pub fn CMPSUB(input: Input) Output {
    return input.zero_output(if (input.d >= input.s) input.d - input.s else input.d, .from_bool(input.d >= input.s));
}

// FGE     D,{#}S   {WC/WZ/WCZ}               | Force D >= S. If D < S then D = S and C = 1, else D same and C = 0. *
pub fn FGE(input: Input) Output {
    const replace = input.d < input.s;
    return input.zero_output(if (replace) input.s else input.d, .from_bool(replace));
}

// FLE     D,{#}S   {WC/WZ/WCZ}               | Force D <= S. If D > S then D = S and C = 1, else D same and C = 0. *
pub fn FLE(input: Input) Output {
    const replace = input.d > input.s;
    return input.zero_output(if (replace) input.s else input.d, .from_bool(replace));
}

// FGES    D,{#}S   {WC/WZ/WCZ}               | Force D >= S, signed. If D < S then D = S and C = 1, else D same and C = 0. *
pub fn FGES(input: Input) Output {
    const replace = Decomposition.from(input.d).i32 < Decomposition.from(input.s).i32;
    return input.zero_output(if (replace) input.s else input.d, .from_bool(replace));
}

// FLES    D,{#}S   {WC/WZ/WCZ}               | Force D <= S, signed. If D > S then D = S and C = 1, else D same and C = 0. *
pub fn FLES(input: Input) Output {
    const replace = Decomposition.from(input.d).i32 > Decomposition.from(input.s).i32;
    return input.zero_output(if (replace) input.s else input.d, .from_bool(replace));
}

// SUMC    D,{#}S   {WC/WZ/WCZ}               | Sum +/-S into D by  C. If C = 1 then D = D - S, else D = D + S. C = correct sign of (D +/- S). *
pub fn SUMC(input: Input) Output {
    return if (input.c == .set) SUBS(input) else ADDS(input);
}

// SUMNC   D,{#}S   {WC/WZ/WCZ}               | Sum +/-S into D by !C. If C = 0 then D = D - S, else D = D + S. C = correct sign of (D +/- S). *
pub fn SUMNC(input: Input) Output {
    return if (input.c == .unset) SUBS(input) else ADDS(input);
}

// SUMZ    D,{#}S   {WC/WZ/WCZ}               | Sum +/-S into D by  Z. If Z = 1 then D = D - S, else D = D + S. C = correct sign of (D +/- S). *
pub fn SUMZ(input: Input) Output {
    return if (input.z == .set) SUBS(input) else ADDS(input);
}

// SUMNZ   D,{#}S   {WC/WZ/WCZ}               | Sum +/-S into D by !Z. If Z = 0 then D = D - S, else D = D + S. C = correct sign of (D +/- S). *
pub fn SUMNZ(input: Input) Output {
    return if (input.z == .unset) SUBS(input) else ADDS(input);
}

// TESTB   D,{#}S         WC/WZ               | Test bit S[4:0] of  D, write to C/Z. C/Z =          D[S[4:0]].
pub fn TESTB(input: Input) Output {
    const bit = Flag.from_lsb(input.d >> input.bit_index());
    return .{ .result = input.d, .c = bit, .z = bit, .q = input.q };
}

// TESTBN  D,{#}S         WC/WZ               | Test bit S[4:0] of !D, write to C/Z. C/Z =         !D[S[4:0]].
pub fn TESTBN(input: Input) Output {
    const bit = Flag.from_lsb(input.d >> input.bit_index()).not();
    return .{ .result = input.d, .c = bit, .z = bit, .q = input.q };
}

// TESTB   D,{#}S     ANDC/ANDZ               | Test bit S[4:0] of  D, AND into C/Z. C/Z = C/Z AND  D[S[4:0]].
pub fn TESTB_AND(input: Input) Output {
    var out = TESTB(input);
    out.c = .from_bool(input.c == .set and out.c == .set);
    out.z = .from_bool(input.z == .set and out.z == .set);
    return out;
}

// TESTBN  D,{#}S     ANDC/ANDZ               | Test bit S[4:0] of !D, AND into C/Z. C/Z = C/Z AND !D[S[4:0]].
pub fn TESTBN_AND(input: Input) Output {
    var out = TESTBN(input);
    out.c = .from_bool(input.c == .set and out.c == .set);
    out.z = .from_bool(input.z == .set and out.z == .set);
    return out;
}

// TESTB   D,{#}S       ORC/ORZ               | Test bit S[4:0] of  D, OR  into C/Z. C/Z = C/Z OR   D[S[4:0]].
pub fn TESTB_OR(input: Input) Output {
    var out = TESTB(input);
    out.c = .from_bool(input.c == .set or out.c == .set);
    out.z = .from_bool(input.z == .set or out.z == .set);
    return out;
}

// TESTBN  D,{#}S       ORC/ORZ               | Test bit S[4:0] of !D, OR  into C/Z. C/Z = C/Z OR  !D[S[4:0]].
pub fn TESTBN_OR(input: Input) Output {
    var out = TESTBN(input);
    out.c = .from_bool(input.c == .set or out.c == .set);
    out.z = .from_bool(input.z == .set or out.z == .set);
    return out;
}

// TESTB   D,{#}S     XORC/XORZ               | Test bit S[4:0] of  D, XOR into C/Z. C/Z = C/Z XOR  D[S[4:0]].
pub fn TESTB_XOR(input: Input) Output {
    var out = TESTB(input);
    out.c = .from_bool(input.c != out.c);
    out.z = .from_bool(input.z != out.z);
    return out;
}

// TESTBN  D,{#}S     XORC/XORZ               | Test bit S[4:0] of !D, XOR into C/Z. C/Z = C/Z XOR !D[S[4:0]].
pub fn TESTBN_XOR(input: Input) Output {
    var out = TESTBN(input);
    out.c = .from_bool(input.c != out.c);
    out.z = .from_bool(input.z != out.z);
    return out;
}

// BITL    D,{#}S         {WCZ}               | Bits D[S[9:5]+S[4:0]:S[4:0]] = 0.    Other bits unaffected. Prior SETQ overrides S[9:5]. C,Z = original D[S[4:0]].
pub fn BITL(input: Input) Output {
    return input.write_bits(0);
}

// BITH    D,{#}S         {WCZ}               | Bits D[S[9:5]+S[4:0]:S[4:0]] = 1.    Other bits unaffected. Prior SETQ overrides S[9:5]. C,Z = original D[S[4:0]].
pub fn BITH(input: Input) Output {
    return input.write_bits(~@as(u32, 0));
}

// BITC    D,{#}S         {WCZ}               | Bits D[S[9:5]+S[4:0]:S[4:0]] = C.    Other bits unaffected. Prior SETQ overrides S[9:5]. C,Z = original D[S[4:0]].
pub fn BITC(input: Input) Output {
    return input.write_bits(input.c.as_mask(u32));
}

// BITNC   D,{#}S         {WCZ}               | Bits D[S[9:5]+S[4:0]:S[4:0]] = !C.   Other bits unaffected. Prior SETQ overrides S[9:5]. C,Z = original D[S[4:0]].
pub fn BITNC(input: Input) Output {
    return input.write_bits(input.c.not().as_mask(u32));
}

// BITZ    D,{#}S         {WCZ}               | Bits D[S[9:5]+S[4:0]:S[4:0]] = Z.    Other bits unaffected. Prior SETQ overrides S[9:5]. C,Z = original D[S[4:0]].
pub fn BITZ(input: Input) Output {
    return input.write_bits(input.z.as_mask(u32));
}

// BITNZ   D,{#}S         {WCZ}               | Bits D[S[9:5]+S[4:0]:S[4:0]] = !Z.   Other bits unaffected. Prior SETQ overrides S[9:5]. C,Z = original D[S[4:0]].
pub fn BITNZ(input: Input) Output {
    return input.write_bits(input.z.not().as_mask(u32));
}

// BITRND  D,{#}S         {WCZ}               | Bits D[S[9:5]+S[4:0]:S[4:0]] = RNDs. Other bits unaffected. Prior SETQ overrides S[9:5]. C,Z = original D[S[4:0]].
pub fn BITRND(input: Input, random: u32) Output {
    return input.write_bits(random);
}

// BITNOT  D,{#}S         {WCZ}               | Toggle bits D[S[9:5]+S[4:0]:S[4:0]]. Other bits unaffected. Prior SETQ overrides S[9:5]. C,Z = original D[S[4:0]].
pub fn BITNOT(input: Input) Output {
    return input.bit_output(input.d ^ input.bit_mask());
}

/// AND     D,{#}S   {WC/WZ/WCZ}               | AND S into D.    D = D & S.    C = parity of result. *
pub fn AND(input: Input) Output {
    return input.parity_zero_output(input.d & input.s);
}

// ANDN    D,{#}S   {WC/WZ/WCZ}               | AND !S into D.   D = D & !S.   C = parity of result. *
pub fn ANDN(input: Input) Output {
    return input.parity_zero_output(input.d & ~input.s);
}

// OR      D,{#}S   {WC/WZ/WCZ}               | OR S into D.     D = D | S.    C = parity of result. *
pub fn OR(input: Input) Output {
    return input.parity_zero_output(input.d | input.s);
}

// XOR     D,{#}S   {WC/WZ/WCZ}               | XOR S into D.    D = D ^ S.    C = parity of result. *
pub fn XOR(input: Input) Output {
    return input.parity_zero_output(input.d ^ input.s);
}

// MUXC    D,{#}S   {WC/WZ/WCZ}               | Mux  C into each D bit that is '1' in S. D = (!S & D ) | (S & {32{ C}}). C = parity of result. *
pub fn MUXC(input: Input) Output {
    return input.parity_zero_output(
        (input.d & ~input.s) | (input.c.as_mask(u32) & input.s),
    );
}

// MUXNC   D,{#}S   {WC/WZ/WCZ}               | Mux !C into each D bit that is '1' in S. D = (!S & D ) | (S & {32{!C}}). C = parity of result. *
pub fn MUXNC(input: Input) Output {
    return input.parity_zero_output(
        (input.d & ~input.s) | (input.c.not().as_mask(u32) & input.s),
    );
}

// MUXZ    D,{#}S   {WC/WZ/WCZ}               | Mux  Z into each D bit that is '1' in S. D = (!S & D ) | (S & {32{ Z}}). C = parity of result. *
pub fn MUXZ(input: Input) Output {
    return input.parity_zero_output(
        (input.d & ~input.s) | (input.z.as_mask(u32) & input.s),
    );
}

// MUXNZ   D,{#}S   {WC/WZ/WCZ}               | Mux !Z into each D bit that is '1' in S. D = (!S & D ) | (S & {32{!Z}}). C = parity of result. *
pub fn MUXNZ(input: Input) Output {
    return input.parity_zero_output(
        (input.d & ~input.s) | (input.z.not().as_mask(u32) & input.s),
    );
}

// MOV     D,{#}S   {WC/WZ/WCZ}               | Move S into D. D = S. C = S[31]. *
pub fn MOV(input: Input) Output {
    return input.msb_zero_output(input.s);
}

// NOT     D,{#}S   {WC/WZ/WCZ}               | Get !S into D. D = !S. C = !S[31]. *
pub fn NOT(input: Input) Output {
    return input.msb_zero_output(~input.s);
}

// ABS     D,{#}S   {WC/WZ/WCZ}               | Get absolute value of S into D. D = ABS(S). C = S[31]. *
pub fn ABS(input: Input) Output {
    return input.zero_output(if (Flag.from_msb(input.s) == .set) 0 -% input.s else input.s, .from_msb(input.s));
}

// NEG     D,{#}S   {WC/WZ/WCZ}               | Negate S into D. D = -S. C = MSB of result. *
pub fn NEG(input: Input) Output {
    return input.msb_zero_output(0 -% input.s);
}

// NEGC    D,{#}S   {WC/WZ/WCZ}               | Negate S by  C into D. If C = 1 then D = -S, else D = S. C = MSB of result. *
pub fn NEGC(input: Input) Output {
    return input.msb_zero_output(if (input.c == .set) 0 -% input.s else input.s);
}

// NEGNC   D,{#}S   {WC/WZ/WCZ}               | Negate S by !C into D. If C = 0 then D = -S, else D = S. C = MSB of result. *
pub fn NEGNC(input: Input) Output {
    return input.msb_zero_output(if (input.c == .unset) 0 -% input.s else input.s);
}

// NEGZ    D,{#}S   {WC/WZ/WCZ}               | Negate S by  Z into D. If Z = 1 then D = -S, else D = S. C = MSB of result. *
pub fn NEGZ(input: Input) Output {
    return input.msb_zero_output(if (input.z == .set) 0 -% input.s else input.s);
}

// NEGNZ   D,{#}S   {WC/WZ/WCZ}               | Negate S by !Z into D. If Z = 0 then D = -S, else D = S. C = MSB of result. *
pub fn NEGNZ(input: Input) Output {
    return input.msb_zero_output(if (input.z == .unset) 0 -% input.s else input.s);
}

// INCMOD  D,{#}S   {WC/WZ/WCZ}               | Increment with modulus. If D = S then D = 0 and C = 1, else D = D + 1 and C = 0. *
pub fn INCMOD(input: Input) Output {
    return input.zero_output(if (input.d == input.s) 0 else input.d +% 1, .from_bool(input.d == input.s));
}

// DECMOD  D,{#}S   {WC/WZ/WCZ}               | Decrement with modulus. If D = 0 then D = S and C = 1, else D = D - 1 and C = 0. *
pub fn DECMOD(input: Input) Output {
    return input.zero_output(if (input.d == 0) input.s else input.d - 1, .from_zero(input.d));
}

// ZEROX   D,{#}S   {WC/WZ/WCZ}               | Zero-extend D above bit S[4:0]. C = MSB of result. *
pub fn ZEROX(input: Input) Output {
    return input.msb_zero_output(input.d & (~@as(u32, 0) >> (31 - input.bit_index())));
}

// SIGNX   D,{#}S   {WC/WZ/WCZ}               | Sign-extend D from bit S[4:0]. C = MSB of result. *
pub fn SIGNX(input: Input) Output {
    const shift = 31 - input.bit_index();
    const value: i32 = @bitCast(input.d << shift);
    return input.msb_zero_output(@bitCast(value >> shift));
}

// ENCOD   D,{#}S   {WC/WZ/WCZ}               | Get bit position of top-most '1' in S into D. D = position of top '1' in S (0..31). C = (S != 0). *
pub fn ENCOD(input: Input) Output {
    return input.zero_output(if (input.s == 0) 0 else 31 - @as(u32, @clz(input.s)), .from_bool(input.s != 0));
}

// ONES    D,{#}S   {WC/WZ/WCZ}               | Get number of '1's in S into D. D = number of '1's in S (0..32). C = LSB of result. *
pub fn ONES(input: Input) Output {
    const result = @popCount(input.s);
    return input.zero_output(result, .from_lsb(result));
}

// TEST    D,{#}S   {WC/WZ/WCZ}               | Test D with S. C = parity of (D & S). Z = ((D & S) == 0).
pub fn TEST(input: Input) Output {
    var out = AND(input);
    out.result = input.d;
    return out;
}

// TESTN   D,{#}S   {WC/WZ/WCZ}               | Test D with !S. C = parity of (D & !S). Z = ((D & !S) == 0).
pub fn TESTN(input: Input) Output {
    var out = ANDN(input);
    out.result = input.d;
    return out;
}

// SETNIB  D,{#}S,#N                          | Set S[3:0] into nibble N in D, keeping rest of D same.
pub fn SETNIB(input: Input, n: u3) Output {
    var d = Decomposition.from(input.d).u4;
    d.set(n, @truncate(input.s));
    return input.unchanged_flags_output(d.raw);
}

// GETNIB  D,{#}S,#N                          | Get nibble N of S into D. D = {28'b0, S.NIBBLE[N]).
pub fn GETNIB(input: Input, n: u3) Output {
    return input.unchanged_flags_output(Decomposition.from(input.s).u4.get(n));
}

// ROLNIB  D,{#}S,#N                          | Rotate-left nibble N of S into D. D = {D[27:0], S.NIBBLE[N]).
pub fn ROLNIB(input: Input, n: u3) Output {
    return input.unchanged_flags_output((input.d << 4) | @as(u32, Decomposition.from(input.s).u4.get(n)));
}

// SETBYTE D,{#}S,#N                          | Set S[7:0] into byte N in D, keeping rest of D same.
pub fn SETBYTE(input: Input, n: u2) Output {
    var d = Decomposition.from(input.d).u8;
    d.set(n, @truncate(input.s));
    return input.unchanged_flags_output(d.raw);
}

// GETBYTE D,{#}S,#N                          | Get byte N of S into D. D = {24'b0, S.BYTE[N]).
pub fn GETBYTE(input: Input, n: u2) Output {
    return input.unchanged_flags_output(Decomposition.from(input.s).u8.get(n));
}

// ROLBYTE D,{#}S,#N                          | Rotate-left byte N of S into D. D = {D[23:0], S.BYTE[N]).
pub fn ROLBYTE(input: Input, n: u2) Output {
    return input.unchanged_flags_output((input.d << 8) | @as(u32, Decomposition.from(input.s).u8.get(n)));
}

// SETWORD D,{#}S,#N                          | Set S[15:0] into word N in D, keeping rest of D same.
pub fn SETWORD(input: Input, n: u1) Output {
    var d = Decomposition.from(input.d).u16;
    d.set(n, @truncate(input.s));
    return input.unchanged_flags_output(d.raw);
}

// GETWORD D,{#}S,#N                          | Get word N of S into D. D = {16'b0, S.WORD[N]).
pub fn GETWORD(input: Input, n: u1) Output {
    return input.unchanged_flags_output(Decomposition.from(input.s).u16.get(n));
}

// ROLWORD D,{#}S,#N                          | Rotate-left word N of S into D. D = {D[15:0], S.WORD[N]).
pub fn ROLWORD(input: Input, n: u1) Output {
    return input.unchanged_flags_output((input.d << 16) | @as(u32, Decomposition.from(input.s).u16.get(n)));
}

// SETR    D,{#}S                             | Set R field of D to S[8:0]. D = {D[31:28], S[8:0], D[18:0]}.
pub fn SETR(input: Input) Output {
    var fields: types.InstructionFields = @bitCast(input.d);
    fields.r = @truncate(input.s);
    return input.unchanged_flags_output(@bitCast(fields));
}

// SETD    D,{#}S                             | Set D field of D to S[8:0]. D = {D[31:18], S[8:0], D[8:0]}.
pub fn SETD(input: Input) Output {
    var fields: types.InstructionFields = @bitCast(input.d);
    fields.d = @truncate(input.s);
    return input.unchanged_flags_output(@bitCast(fields));
}

// SETS    D,{#}S                             | Set S field of D to S[8:0]. D = {D[31:9], S[8:0]}.
pub fn SETS(input: Input) Output {
    var fields: types.InstructionFields = @bitCast(input.d);
    fields.s = @truncate(input.s);
    return input.unchanged_flags_output(@bitCast(fields));
}

// DECOD   D,{#}S                             | Decode S[4:0] into D. D = 1 << S[4:0].
pub fn DECOD(input: Input) Output {
    return input.unchanged_flags_output(@as(u32, 1) << input.bit_index());
}

// BMASK   D,{#}S                             | Get LSB-justified bit mask of size (S[4:0] + 1) into D. D = ($0_0000_0002 << S[4:0]) - 1.
pub fn BMASK(input: Input) Output {
    return input.unchanged_flags_output(~@as(u32, 0) >> (31 - input.bit_index()));
}

// CRCBIT  D,{#}S                             | Iterate CRC value in D using C and polynomial in S. If (C XOR D[0]) then D = (D >> 1) XOR S, else D = (D >> 1).
pub fn CRCBIT(input: Input) Output {
    return input.unchanged_flags_output((input.d >> 1) ^ (if (input.c != Flag.from_lsb(input.d)) input.s else 0));
}

// CRCNIB  D,{#}S                             | Iterate CRC value in D using Q[31:28] and polynomial in S. Like CRCBIT x 4. Q = Q << 4. For long, use SETQ+'REP #1,#8'+CRCNIB.
pub fn CRCNIB(input: Input) Output {
    var result = input.d;
    var q = input.q;
    for (0..4) |_| {
        result = (result >> 1) ^ (if (Flag.from_msb(q) != Flag.from_lsb(result)) input.s else 0);
        q <<= 1;
    }
    var out = input.unchanged_flags_output(result);
    out.q = q;
    return out;
}

// MUXNITS D,{#}S                             | For each non-zero bit pair in S, copy that bit pair into the corresponding D bits, else leave that D bit pair the same.
pub fn MUXNITS(input: Input) Output {
    var d = Decomposition.from(input.d).u2;
    const source = Decomposition.from(input.s).u2.to_array();
    for (source, 0..) |value, i| {
        if (value != 0) d.set(@intCast(i), value);
    }
    return input.unchanged_flags_output(d.raw);
}

// MUXNIBS D,{#}S                             | For each non-zero nibble in S, copy that nibble into the corresponding D nibble, else leave that D nibble the same.
pub fn MUXNIBS(input: Input) Output {
    var d = Decomposition.from(input.d).u4;
    const source = Decomposition.from(input.s).u4.to_array();
    for (source, 0..) |value, i| {
        if (value != 0) d.set(@intCast(i), value);
    }
    return input.unchanged_flags_output(d.raw);
}

// MUXQ    D,{#}S                             | Used after SETQ. For each '1' bit in Q, copy the corresponding bit in S into D. D = (D & !Q) | (S & Q).
pub fn MUXQ(input: Input) Output {
    return input.unchanged_flags_output((input.d & ~input.q) | (input.s & input.q));
}

// MOVBYTS D,{#}S                             | Move bytes within D, per S. D = {D.BYTE[S[7:6]], D.BYTE[S[5:4]], D.BYTE[S[3:2]], D.BYTE[S[1:0]]}.
pub fn MOVBYTS(input: Input) Output {
    const d = Decomposition.from(input.d).u8;
    const selectors: types.BytePermutation = @bitCast(input.s);
    const result = types.BitArray(u8).from_array(.{
        d.get(selectors.byte0),
        d.get(selectors.byte1),
        d.get(selectors.byte2),
        d.get(selectors.byte3),
    });
    return input.unchanged_flags_output(result.raw);
}

// MUL     D,{#}S          {WZ}               | D = unsigned (D[15:0] * S[15:0]). Z = (S == 0) | (D == 0).
pub fn MUL(input: Input) Output {
    const result = (input.d & 0xFFFF) * (input.s & 0xFFFF);
    var out = input.unchanged_flags_output(result);
    out.z = .from_bool(input.d == 0 or input.s == 0);
    return out;
}

// MULS    D,{#}S          {WZ}               | D = signed (D[15:0] * S[15:0]).   Z = (S == 0) | (D == 0).
pub fn MULS(input: Input) Output {
    const d: i16 = @bitCast(@as(u16, @truncate(input.d)));
    const s: i16 = @bitCast(@as(u16, @truncate(input.s)));
    var out = input.unchanged_flags_output(@bitCast(@as(i32, d) * s));
    out.z = .from_bool(input.d == 0 or input.s == 0);
    return out;
}

// SCA     D,{#}S          {WZ}               | Next instruction's S value = unsigned (D[15:0] * S[15:0]) >> 16. *
pub fn SCA(input: Input) Output {
    const product = (input.d & 0xFFFF) * (input.s & 0xFFFF);
    var out = input.unchanged_flags_output(input.d);
    out.z = .from_zero(product);
    out.next_s = product >> 16;
    return out;
}

// SCAS    D,{#}S          {WZ}               | Next instruction's S value = signed (D[15:0] * S[15:0]) >> 14. In this scheme, $4000 = 1.0 and $C000 = -1.0. *
pub fn SCAS(input: Input) Output {
    const d: i16 = @bitCast(@as(u16, @truncate(input.d)));
    const s: i16 = @bitCast(@as(u16, @truncate(input.s)));
    const product = @as(i32, d) * s;
    var out = input.unchanged_flags_output(input.d);
    out.z = .from_bool(product == 0);
    out.next_s = @bitCast(product >> 14);
    return out;
}

// SPLITB  D                                  | Split every 4th bit of D into bytes. D = {D[31], D[27], D[23], D[19], ...D[12], D[8], D[4], D[0]}.
pub fn SPLITB(input: Input) Output {
    const d = Decomposition.from(input.d).u1;
    var result: types.BitArray(u1) = .zero;
    for (0..32) |i| {
        const other = (i % 4) * 8 + i / 4;
        result.set(@intCast(other), d.get(@intCast(i)));
    }
    return input.unchanged_flags_output(result.raw);
}

// MERGEB  D                                  | Merge bits of bytes in D. D = {D[31], D[23], D[15], D[7], ...D[24], D[16], D[8], D[0]}.
pub fn MERGEB(input: Input) Output {
    const d = Decomposition.from(input.d).u1;
    var result: types.BitArray(u1) = .zero;
    for (0..32) |i| {
        const other = (i % 4) * 8 + i / 4;
        result.set(@intCast(i), d.get(@intCast(other)));
    }
    return input.unchanged_flags_output(result.raw);
}

// SPLITW  D                                  | Split odd/even bits of D into words. D = {D[31], D[29], D[27], D[25], ...D[6], D[4], D[2], D[0]}.
pub fn SPLITW(input: Input) Output {
    const d = Decomposition.from(input.d).u1;
    var result: types.BitArray(u1) = .zero;
    for (0..32) |i| {
        const other = (i % 2) * 16 + i / 2;
        result.set(@intCast(other), d.get(@intCast(i)));
    }
    return input.unchanged_flags_output(result.raw);
}

// MERGEW  D                                  | Merge bits of words in D. D = {D[31], D[15], D[30], D[14], ...D[17], D[1], D[16], D[0]}.
pub fn MERGEW(input: Input) Output {
    const d = Decomposition.from(input.d).u1;
    var result: types.BitArray(u1) = .zero;
    for (0..32) |i| {
        const other = (i % 2) * 16 + i / 2;
        result.set(@intCast(i), d.get(@intCast(other)));
    }
    return input.unchanged_flags_output(result.raw);
}

// SEUSSF  D                                  | Relocate and periodically invert bits within D. Returns to original value on 32nd iteration. Forward pattern.
pub fn SEUSSF(input: Input) Output {
    const d = Decomposition.from(input.d ^ seuss_invert).u1;
    var result: types.BitArray(u1) = .zero;
    for (seuss_forward, 0..) |destination, source| {
        result.set(destination, d.get(@intCast(source)));
    }
    return input.unchanged_flags_output(result.raw);
}

// SEUSSR  D                                  | Relocate and periodically invert bits within D. Returns to original value on 32nd iteration. Reverse pattern.
pub fn SEUSSR(input: Input) Output {
    const d = Decomposition.from(input.d).u1;
    var result: types.BitArray(u1) = .zero;
    for (seuss_forward, 0..) |source, destination| {
        result.set(@intCast(destination), d.get(source));
    }
    return input.unchanged_flags_output(result.raw ^ seuss_invert);
}

// Bit mapping and input inversion from docs/p2docs.github.io/page/alu.md.
const seuss_forward = [32]u5{
    11, 5,  18, 24, 27, 19, 20, 30, 28, 26, 21, 25, 3,  8, 7, 23,
    13, 12, 16, 2,  15, 1,  9,  31, 0,  29, 17, 10, 14, 4, 6, 22,
};
const seuss_invert: u32 = 0b11101011010101010000001100101101;

// RGBSQZ  D                                  | Squeeze 8:8:8 RGB value in D[31:8] into 5:6:5 value in D[15:0]. D = {15'b0, D[31:27], D[23:18], D[15:11]}.
pub fn RGBSQZ(input: Input) Output {
    const rgb: types.RGBA8888 = @bitCast(input.d);
    const result: types.RGB565 = .{
        .r = @intCast(rgb.r >> 3),
        .g = @intCast(rgb.g >> 2),
        .b = @intCast(rgb.b >> 3),
    };
    return input.unchanged_flags_output(@as(u16, @bitCast(result)));
}

// RGBEXP  D                                  | Expand 5:6:5 RGB value in D[15:0] into 8:8:8 value in D[31:8]. D = {D[15:11,15:13], D[10:5,10:9], D[4:0,4:2], 8'b0}.
pub fn RGBEXP(input: Input) Output {
    const rgb: types.RGB565 = @bitCast(@as(u16, @truncate(input.d)));
    const result: types.RGBA8888 = .{
        .r = (@as(u8, rgb.r) << 3) | (rgb.r >> 2),
        .g = (@as(u8, rgb.g) << 2) | (rgb.g >> 4),
        .b = (@as(u8, rgb.b) << 3) | (rgb.b >> 2),
        .a = 0,
    };
    return input.unchanged_flags_output(@bitCast(result));
}

// XORO32  D                                  | Iterate D with xoroshiro32+ PRNG algorithm and put PRNG result into next instruction's S. D must be non-zero to iterate.
pub fn XORO32(input: Input) Output {
    // Rev B/C uses xoroshiro32++ [13, 5, 10, 9], twice for a 32-bit output.
    // See docs/p2docs.github.io/page/alu.md (xoro32_soft).
    var low: u16 = @truncate(input.d);
    var high: u16 = @truncate(input.d >> 16);
    var random: types.BitArray(u16) = .zero;
    for (0..2) |i| {
        random.set(@intCast(i), std.math.rotl(u16, low +% high, 9) +% low);
        high ^= low;
        low = std.math.rotl(u16, low, 13) ^ (high << 5) ^ high;
        high = std.math.rotl(u16, high, 10);
    }
    var out = input.unchanged_flags_output((@as(u32, high) << 16) | low);
    out.q = random.raw;
    out.next_s = random.raw;
    return out;
}

// REV     D                                  | Reverse D bits. D = D[0:31].
pub fn REV(input: Input) Output {
    return input.unchanged_flags_output(@bitReverse(input.d));
}

// RCZR    D        {WC/WZ/WCZ}               | Rotate C,Z right through D. D = {C, Z, D[31:2]}. C = D[1],  Z = D[0].
pub fn RCZR(input: Input) Output {
    return .{
        .result = (input.c.as_int(u32) << 31) | (input.z.as_int(u32) << 30) | (input.d >> 2),
        .c = .from_lsb(input.d >> 1),
        .z = .from_lsb(input.d),
        .q = input.q,
    };
}

// RCZL    D        {WC/WZ/WCZ}               | Rotate C,Z left through D.  D = {D[29:0], C, Z}. C = D[31], Z = D[30].
pub fn RCZL(input: Input) Output {
    return .{
        .result = (input.d << 2) | (input.c.as_int(u32) << 1) | input.z.as_int(u32),
        .c = .from_msb(input.d),
        .z = .from_msb(input.d << 1),
        .q = input.q,
    };
}

// WRC     D                                  | Write 0 or 1 to D, according to  C. D = {31'b0,  C}.
pub fn WRC(input: Input) Output {
    return input.unchanged_flags_output(input.c.as_int(u32));
}

// WRNC    D                                  | Write 0 or 1 to D, according to !C. D = {31'b0, !C}.
pub fn WRNC(input: Input) Output {
    return input.unchanged_flags_output(input.c.not().as_int(u32));
}

// WRZ     D                                  | Write 0 or 1 to D, according to  Z. D = {31'b0,  Z}.
pub fn WRZ(input: Input) Output {
    return input.unchanged_flags_output(input.z.as_int(u32));
}

// WRNZ    D                                  | Write 0 or 1 to D, according to !Z. D = {31'b0, !Z}.
pub fn WRNZ(input: Input) Output {
    return input.unchanged_flags_output(input.z.not().as_int(u32));
}

// MODCZ   c,z      {WC/WZ/WCZ}               | Modify C and Z according to cccc and zzzz. C = cccc[{C,Z}], Z = zzzz[{C,Z}]. See "MODCZ Operand" list.
pub fn MODCZ(input: Input) Output {
    const conditions: types.FlagConditions = @bitCast(@as(u9, @truncate(input.d)));
    return .{
        .result = input.d,
        .c = conditions.c.evaluate(input.c, input.z),
        .z = conditions.z.evaluate(input.c, input.z),
        .q = input.q,
    };
}

test "all instruction functions preserve unrelated state" {
    const words = [_]u32{ 0, 1, 0x8000_0000, 0xFFFF_FFFF, 0x1234_5678 };
    inline for (@typeInfo(@This()).@"struct".decls) |decl| {
        const function = @field(@This(), decl.name);
        switch (@typeInfo(@TypeOf(function))) {
            .@"fn" => |info| {
                for (words) |word| {
                    const input: Input = .{ .d = word, .s = word, .c = .set, .z = .unset, .q = 0xDEAD_BEEF, .setq_prefix = false };
                    const out = switch (info.params.len) {
                        1 => function(input),
                        2 => function(input, 0),
                        else => unreachable,
                    };
                    if (comptime !std.mem.eql(u8, decl.name, "CRCNIB") and !std.mem.eql(u8, decl.name, "XORO32")) {
                        try std.testing.expectEqual(input.q, out.q);
                    }
                    if (comptime !std.mem.eql(u8, decl.name, "SCA") and !std.mem.eql(u8, decl.name, "SCAS") and !std.mem.eql(u8, decl.name, "XORO32")) {
                        try std.testing.expectEqual(@as(?u32, null), out.next_s);
                    }
                }
            },
            else => {},
        }
    }
}

test "arithmetic boundaries and extended flags" {
    const Case = struct {
        op: *const fn (Input) Output,
        d: u32,
        s: u32,
        c: Flag = .unset,
        z: Flag = .set,
        result: u32,
        carry: Flag,
        zero: Flag = .unset,
    };
    const cases = [_]Case{
        .{ .op = ADD, .d = 0xFFFF_FFFF, .s = 1, .result = 0, .carry = .set, .zero = .set },
        .{ .op = ADDX, .d = 0xFFFF_FFFF, .s = 0xFFFF_FFFF, .c = .set, .result = 0xFFFF_FFFF, .carry = .set },
        .{ .op = ADDX, .d = 0xFFFF_FFFF, .s = 0, .c = .set, .z = .unset, .result = 0, .carry = .set },
        .{ .op = ADDS, .d = 0x7FFF_FFFF, .s = 1, .result = 0x8000_0000, .carry = .unset },
        .{ .op = ADDS, .d = 0x8000_0000, .s = 0xFFFF_FFFF, .result = 0x7FFF_FFFF, .carry = .set },
        .{ .op = ADDSX, .d = 0xFFFF_FFFF, .s = 0, .c = .set, .result = 0, .carry = .unset, .zero = .set },
        .{ .op = SUB, .d = 0, .s = 1, .result = 0xFFFF_FFFF, .carry = .set },
        .{ .op = SUBX, .d = 0, .s = 0xFFFF_FFFF, .c = .set, .result = 0, .carry = .set, .zero = .set },
        .{ .op = SUBX, .d = 1, .s = 0, .c = .set, .z = .unset, .result = 0, .carry = .unset },
        .{ .op = SUBS, .d = 0x8000_0000, .s = 1, .result = 0x7FFF_FFFF, .carry = .set },
        .{ .op = SUBSX, .d = 0x7FFF_FFFF, .s = 0xFFFF_FFFF, .c = .set, .result = 0x7FFF_FFFF, .carry = .unset },
        .{ .op = CMPSX, .d = 0xFFFF_FFFF, .s = 0xFFFF_FFFF, .result = 0xFFFF_FFFF, .carry = .unset, .zero = .set },
        .{ .op = CMP, .d = 7, .s = 8, .result = 7, .carry = .set },
        .{ .op = CMPX, .d = 0, .s = 0xFFFF_FFFF, .c = .set, .result = 0, .carry = .set, .zero = .set },
        .{ .op = CMPS, .d = 0x8000_0000, .s = 1, .result = 0x8000_0000, .carry = .set },
        .{ .op = CMPR, .d = 8, .s = 7, .result = 8, .carry = .set },
        .{ .op = CMPM, .d = 0, .s = 0x8000_0000, .result = 0, .carry = .set },
        .{ .op = SUBR, .d = 8, .s = 7, .result = 0xFFFF_FFFF, .carry = .set },
        .{ .op = CMPSUB, .d = 7, .s = 8, .result = 7, .carry = .unset },
        .{ .op = CMPSUB, .d = 8, .s = 8, .result = 0, .carry = .set, .zero = .set },
        .{ .op = FGES, .d = 0xFFFF_FFFF, .s = 0, .result = 0, .carry = .set, .zero = .set },
        .{ .op = FLES, .d = 0, .s = 0xFFFF_FFFF, .result = 0xFFFF_FFFF, .carry = .set },
        .{ .op = FGE, .d = 0xFFFF_FFFF, .s = 0, .result = 0xFFFF_FFFF, .carry = .unset },
        .{ .op = FLE, .d = 0, .s = 0xFFFF_FFFF, .result = 0, .carry = .unset, .zero = .set },
        .{ .op = SUMC, .d = 0x7FFF_FFFF, .s = 1, .result = 0x8000_0000, .carry = .unset },
        .{ .op = SUMNC, .d = 0x8000_0000, .s = 1, .result = 0x7FFF_FFFF, .carry = .set },
        .{ .op = SUMZ, .d = 5, .s = 5, .result = 0, .carry = .unset, .zero = .set },
        .{ .op = SUMNZ, .d = 0x7FFF_FFFF, .s = 1, .result = 0x8000_0000, .carry = .unset },
        .{ .op = ABS, .d = 0, .s = 0x8000_0000, .result = 0x8000_0000, .carry = .set },
        .{ .op = NEG, .d = 0, .s = 0x8000_0000, .result = 0x8000_0000, .carry = .set },
        .{ .op = INCMOD, .d = 0xFFFF_FFFF, .s = 7, .result = 0, .carry = .unset, .zero = .set },
        .{ .op = DECMOD, .d = 0, .s = 7, .result = 7, .carry = .set },
        .{ .op = ENCOD, .d = 5, .s = 0, .result = 0, .carry = .unset, .zero = .set },
        .{ .op = ENCOD, .d = 5, .s = 1, .result = 0, .carry = .set, .zero = .set },
        .{ .op = ONES, .d = 5, .s = 0xFFFF_FFFF, .result = 32, .carry = .unset },
    };
    for (cases) |case| {
        const out = case.op(.{ .d = case.d, .s = case.s, .c = case.c, .z = case.z, .q = 123, .setq_prefix = false });
        try std.testing.expectEqualDeep(Output{ .result = case.result, .c = case.carry, .z = case.zero, .q = 123 }, out);
    }
}

test "shift counts, fill bits, and carry at zero" {
    var input: Input = .{ .d = 0x8000_0001, .s = 32, .c = .set, .z = .unset, .q = 0, .setq_prefix = false };
    for ([_]*const fn (Input) Output{ ROR, ROL, SHR, SHL, RCR, RCL, SAR, SAL }) |op| {
        const out = op(input);
        try std.testing.expectEqual(input.d, out.result);
        try std.testing.expectEqual(Flag.set, out.c);
        try std.testing.expectEqual(Flag.unset, out.z);
    }
    input.s = 1;
    const expected = [_]u32{ 0xC000_0000, 3, 0x4000_0000, 2, 0xC000_0000, 3, 0xC000_0000, 3 };
    for ([_]*const fn (Input) Output{ ROR, ROL, SHR, SHL, RCR, RCL, SAR, SAL }, expected) |op, result| {
        try std.testing.expectEqual(result, op(input).result);
        try std.testing.expectEqual(Flag.set, op(input).c);
    }
    input.s = 31;
    try std.testing.expectEqual(@as(u32, 0xFFFF_FFFF), SAR(input).result);
    try std.testing.expectEqual(@as(u32, 0xFFFF_FFFF), SAL(input).result);
    input.c = .unset;
    try std.testing.expectEqual(@as(u32, 1), RCR(input).result);
    try std.testing.expectEqual(@as(u32, 0x8000_0000), RCL(input).result);
    input.s = 0;
    try std.testing.expectEqual(@as(u32, 1), ZEROX(input).result);
    try std.testing.expectEqual(@as(u32, 0xFFFF_FFFF), SIGNX(input).result);
    input.s = 31;
    try std.testing.expectEqual(input.d, ZEROX(input).result);
    try std.testing.expectEqual(input.d, SIGNX(input).result);
}

test "bit ranges, tests, and condition truth tables" {
    var input: Input = .{ .d = 0x8000_0000, .s = (1 << 5) | 31, .c = .set, .z = .unset, .q = 0, .setq_prefix = false };
    try std.testing.expectEqualDeep(Output{ .result = 0x8000_0001, .c = .set, .z = .set, .q = 0 }, BITH(input));
    try std.testing.expectEqual(@as(u32, 0), BITL(input).result);
    try std.testing.expectEqual(@as(u32, 1), BITNOT(input).result);
    try std.testing.expectEqual(@as(u32, 0x8000_0000), BITRND(input, 0x8000_0000).result);
    input.setq_prefix = true;
    input.q = 0xFFFF_FFFF;
    try std.testing.expectEqual(@as(u32, 0xFFFF_FFFF), BITC(input).result);
    try std.testing.expectEqual(@as(u32, 0), BITNC(input).result);
    try std.testing.expectEqual(@as(u32, 0), BITZ(input).result);
    try std.testing.expectEqual(@as(u32, 0xFFFF_FFFF), BITNZ(input).result);
    input.q = 0;
    try std.testing.expectEqual(@as(u32, 0x8000_0000), BITH(input).result);
    try std.testing.expectEqual(Flag.set, TESTB(input).c);
    try std.testing.expectEqual(Flag.unset, TESTBN(input).z);
    try std.testing.expectEqual(Flag.unset, TESTB_AND(input).z);
    try std.testing.expectEqual(Flag.set, TESTBN_OR(input).c);
    try std.testing.expectEqual(Flag.unset, TESTB_XOR(input).c);
    try std.testing.expectEqual(Flag.set, TESTB_XOR(input).z);
    try std.testing.expectEqual(Flag.set, TESTBN_XOR(input).c);
    input.s = 0x8000_0000;
    try std.testing.expectEqualDeep(Output{ .result = input.d, .c = .set, .z = .unset, .q = 0 }, TEST(input));
    try std.testing.expectEqualDeep(Output{ .result = input.d, .c = .unset, .z = .set, .q = 0 }, TESTN(input));
    for (0..4) |state| {
        input.c = .from_bool(state & 2 != 0);
        input.z = .from_bool(state & 1 != 0);
        for (0..256) |modifiers| {
            var flags = input;
            flags.d = @intCast(modifiers);
            const out = MODCZ(flags);
            const mask = @as(usize, 1) << @intCast(state);
            try std.testing.expectEqual(Flag.from_bool((modifiers >> 4) & mask != 0), out.c);
            try std.testing.expectEqual(Flag.from_bool(modifiers & mask != 0), out.z);
            try std.testing.expectEqual(flags.d, out.result);
        }
        try std.testing.expectEqualDeep(input.unchanged_flags_output(input.d), RCZL(.{ .d = RCZR(input).result, .s = 0, .c = RCZR(input).c, .z = RCZR(input).z, .q = 0, .setq_prefix = false }));
    }
}

test "lane operations, permutations, and RGB conversions" {
    var input: Input = .{ .d = 0x1234_5678, .s = 0x89AB_CDEF, .c = .set, .z = .set, .q = 5, .setq_prefix = false };
    try std.testing.expectEqual(@as(u32, 0xF234_5678), SETNIB(input, 7).result);
    try std.testing.expectEqual(@as(u32, 8), GETNIB(input, 7).result);
    try std.testing.expectEqual(@as(u32, 0x2345_678C), ROLNIB(input, 3).result);
    try std.testing.expectEqual(@as(u32, 0x12EF_5678), SETBYTE(input, 2).result);
    try std.testing.expectEqual(@as(u32, 0xCD), GETBYTE(input, 1).result);
    try std.testing.expectEqual(@as(u32, 0x3456_78AB), ROLBYTE(input, 2).result);
    try std.testing.expectEqual(@as(u32, 0xCDEF_5678), SETWORD(input, 1).result);
    try std.testing.expectEqual(@as(u32, 0x89AB), GETWORD(input, 1).result);
    try std.testing.expectEqual(@as(u32, 0x5678_89AB), ROLWORD(input, 1).result);
    input.s = 0x1B;
    try std.testing.expectEqual(@as(u32, 0x7856_3412), MOVBYTS(input).result);
    input.s = 0x0000_0201;
    try std.testing.expectEqual(@as(u32, 0x100C_5678), SETR(input).result);
    try std.testing.expectEqual(@as(u32, 0x1234_0278), SETD(input).result);
    try std.testing.expectEqual(@as(u32, 0x1234_5601), SETS(input).result);
    input.s = 0x000A_0005;
    try std.testing.expectEqual(@as(u32, 0x123A_5675), MUXNIBS(input).result);
    try std.testing.expectEqual(@as(u32, 0x123A_5675), MUXNITS(input).result);
    for (0..32) |bit| {
        input.d = @as(u32, 1) << @intCast(bit);
        try std.testing.expectEqual(input.d, MERGEB(.{ .d = SPLITB(input).result, .s = 0, .c = input.c, .z = input.z, .q = input.q, .setq_prefix = false }).result);
        try std.testing.expectEqual(input.d, MERGEW(.{ .d = SPLITW(input).result, .s = 0, .c = input.c, .z = input.z, .q = input.q, .setq_prefix = false }).result);
        try std.testing.expectEqual(input.d, SEUSSR(.{ .d = SEUSSF(input).result, .s = 0, .c = input.c, .z = input.z, .q = input.q, .setq_prefix = false }).result);
        var forward = input;
        var reverse = input;
        for (0..32) |_| {
            forward.d = SEUSSF(forward).result;
            reverse.d = SEUSSR(reverse).result;
        }
        try std.testing.expectEqual(input.d, forward.d);
        try std.testing.expectEqual(input.d, reverse.d);
    }
    input.d = 0x1111_1111;
    try std.testing.expectEqual(@as(u32, 0x0000_00FF), SPLITB(input).result);
    input.d = 0x5555_5555;
    try std.testing.expectEqual(@as(u32, 0x0000_FFFF), SPLITW(input).result);
    for (0..65536) |rgb| {
        input.d = @intCast(rgb);
        var expanded = input;
        expanded.d = RGBEXP(input).result;
        try std.testing.expectEqual(input.d, RGBSQZ(expanded).result);
    }
    input.d = 0xFFFF;
    try std.testing.expectEqual(@as(u32, 0xFFFF_FF00), RGBEXP(input).result);
}

test "multiplication and source substitution" {
    var input: Input = .{ .d = 0xFFFF, .s = 0xFFFF, .c = .set, .z = .set, .q = 7, .setq_prefix = false };
    try std.testing.expectEqual(@as(u32, 0xFFFE_0001), MUL(input).result);
    try std.testing.expectEqual(@as(u32, 1), MULS(input).result);
    input.d = 0x1_0000;
    try std.testing.expectEqual(@as(u32, 0), MUL(input).result);
    try std.testing.expectEqual(Flag.unset, MUL(input).z);
    try std.testing.expectEqual(Flag.unset, MULS(input).z);
    input.d = 1;
    input.s = 1;
    try std.testing.expectEqualDeep(Output{ .result = 1, .c = .set, .z = .unset, .q = 7, .next_s = 0 }, SCA(input));
    try std.testing.expectEqualDeep(Output{ .result = 1, .c = .set, .z = .unset, .q = 7, .next_s = 0 }, SCAS(input));
    input.d = 0xC000;
    input.s = 0x4000;
    try std.testing.expectEqual(@as(?u32, 0xFFFF_C000), SCAS(input).next_s);
    input.d = 0;
    try std.testing.expectEqual(Flag.set, SCA(input).z);
    try std.testing.expectEqual(Flag.set, SCAS(input).z);
    try std.testing.expectEqualDeep(Output{ .result = 0, .c = .set, .z = .set, .q = 0, .next_s = 0 }, XORO32(input));
    input.d = 1;
    const random = XORO32(input);
    // Evaluated from the two-stage Verilog [13, 5, 10, 9] implementation.
    try std.testing.expectEqual(@as(u32, 0x8490_8405), random.result);
    try std.testing.expectEqual(@as(u32, 0x6269_0201), random.q);
    try std.testing.expectEqual(@as(?u32, random.q), random.next_s);
}

test "CRC consumes Q most significant bit first and preserves flags" {
    var input: Input = .{ .d = 0xFFFF_FFFF, .s = 0xEDB8_8320, .c = .set, .z = .set, .q = 0, .setq_prefix = false };
    // Standard reflected CRC-32 check vector, fed LSB first per byte.
    for ("123456789") |byte| {
        for (0..8) |bit| {
            input.c = .from_lsb(@as(u32, byte) >> @as(u5, @intCast(bit)));
            const out = CRCBIT(input);
            try std.testing.expectEqual(input.c, out.c);
            try std.testing.expectEqual(input.z, out.z);
            input.d = out.result;
        }
    }
    try std.testing.expectEqual(@as(u32, 0xCBF4_3926), ~input.d);
    input.d = 0;
    input.q = 0x8000_0001;
    input.c = .unset;
    const nibble = CRCNIB(input);
    try std.testing.expectEqualDeep(Output{ .result = 0x1DB7_1064, .c = .unset, .z = .set, .q = 0x10 }, nibble);
}
