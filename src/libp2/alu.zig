//!
//! This file implements the basic atomic arithmetic operations of the Propeller 2
//! as pure implementations.
//!

pub const Flag = enum(u1) {
    unset = 0,
    set = 1,

    pub fn from_bool(value: bool) Flag {
        return @enumFromInt(@intFromBool(value));
    }

    pub fn from_lsb(value: u32) Flag {
        return @enumFromInt(Decomposition.from(value).view.lsb);
    }

    pub fn from_msb(value: u32) Flag {
        return @enumFromInt(Decomposition.from(value).view.msb);
    }

    pub fn from_zero(value: u32) Flag {
        return from_bool(value == 0);
    }

    pub fn from_parity(value: u32) Flag {
        return @intFromEnum(@popCount(value) & 1);
    }

    pub fn as_int(flag: Flag, comptime T: type) T {
        return @intFromEnum(flag);
    }

    pub fn as_mask(flag: Flag, comptime T: type) T {
        return switch (flag) {
            .unset => 0,
            .set => ~@as(T, 0),
        };
    }

    pub fn not(flag: Flag) Flag {
        return switch (flag) {
            .unset => .set,
            .set => .unset,
        };
    }
};

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
};

/// The most generic output of an ALU instruction.
pub const Output = struct {
    result: u32,
    c: Flag,
    z: Flag,
    q: u32,
};

// * Z = (result == 0).
// ROR     D,{#}S   {WC/WZ/WCZ}               | Rotate right.           D = [31:0]  of ({D[31:0], D[31:0]}     >> S[4:0]). C = last bit shifted out if S[4:0] > 0, else D[0].  *
// ROL     D,{#}S   {WC/WZ/WCZ}               | Rotate left.            D = [63:32] of ({D[31:0], D[31:0]}     << S[4:0]). C = last bit shifted out if S[4:0] > 0, else D[31]. *
// SHR     D,{#}S   {WC/WZ/WCZ}               | Shift right.            D = [31:0]  of ({32'b0, D[31:0]}       >> S[4:0]). C = last bit shifted out if S[4:0] > 0, else D[0].  *
// SHL     D,{#}S   {WC/WZ/WCZ}               | Shift left.             D = [63:32] of ({D[31:0], 32'b0}       << S[4:0]). C = last bit shifted out if S[4:0] > 0, else D[31]. *
// RCR     D,{#}S   {WC/WZ/WCZ}               | Rotate carry right.     D = [31:0]  of ({{32{C}}, D[31:0]}     >> S[4:0]). C = last bit shifted out if S[4:0] > 0, else D[0].  *
// RCL     D,{#}S   {WC/WZ/WCZ}               | Rotate carry left.      D = [63:32] of ({D[31:0], {32{C}}}     << S[4:0]). C = last bit shifted out if S[4:0] > 0, else D[31]. *
// SAR     D,{#}S   {WC/WZ/WCZ}               | Shift arithmetic right. D = [31:0]  of ({{32{D[31]}}, D[31:0]} >> S[4:0]). C = last bit shifted out if S[4:0] > 0, else D[0].  *
// SAL     D,{#}S   {WC/WZ/WCZ}               | Shift arithmetic left.  D = [63:32] of ({D[31:0], {32{D[0]}}}  << S[4:0]). C = last bit shifted out if S[4:0] > 0, else D[31]. *
// ADD     D,{#}S   {WC/WZ/WCZ}               | Add S into D.                                  D = D + S.        C = carry of (D + S).               *
// ADDX    D,{#}S   {WC/WZ/WCZ}               | Add (S + C) into D, extended.                  D = D + S + C.    C = carry of (D + S + C).           Z = Z AND (result == 0).
// ADDS    D,{#}S   {WC/WZ/WCZ}               | Add S into D, signed.                          D = D + S.        C = correct sign of (D + S).        *
// ADDSX   D,{#}S   {WC/WZ/WCZ}               | Add (S + C) into D, signed and extended.       D = D + S + C.    C = correct sign of (D + S + C).    Z = Z AND (result == 0).
// SUB     D,{#}S   {WC/WZ/WCZ}               | Subtract S from D.                             D = D - S.        C = borrow of (D - S).              *
// SUBX    D,{#}S   {WC/WZ/WCZ}               | Subtract (S + C) from D, extended.             D = D - (S + C).  C = borrow of (D - (S + C)).        Z = Z AND (result == 0).
// SUBS    D,{#}S   {WC/WZ/WCZ}               | Subtract S from D, signed.                     D = D - S.        C = correct sign of (D - S).        *
// SUBSX   D,{#}S   {WC/WZ/WCZ}               | Subtract (S + C) from D, signed and extended.  D = D - (S + C).  C = correct sign of (D - (S + C)).  Z = Z AND (result == 0).
// CMP     D,{#}S   {WC/WZ/WCZ}               | Compare D to S.                                                  C = borrow of (D - S).              Z = (D == S).
// CMPX    D,{#}S   {WC/WZ/WCZ}               | Compare D to (S + C), extended.                                  C = borrow of (D - (S + C)).        Z = Z AND (D == S + C).
// CMPS    D,{#}S   {WC/WZ/WCZ}               | Compare D to S, signed.                                          C = correct sign of (D - S).        Z = (D == S).
// CMPSX   D,{#}S   {WC/WZ/WCZ}               | Compare D to (S + C), signed and extended.                       C = correct sign of (D - (S + C)).  Z = Z AND (D == S + C).
// CMPR    D,{#}S   {WC/WZ/WCZ}               | Compare S to D (reverse).                                        C = borrow of (S - D).              Z = (D == S).
// CMPM    D,{#}S   {WC/WZ/WCZ}               | Compare D to S, get MSB of difference into C.                    C = MSB of (D - S).                 Z = (D == S).
// SUBR    D,{#}S   {WC/WZ/WCZ}               | Subtract D from S (reverse).                   D = S - D.        C = borrow of (S - D).              *
// CMPSUB  D,{#}S   {WC/WZ/WCZ}               | Compare and subtract S from D if D >= S. If D => S then D = D - S and C = 1, else D same and C = 0.  *
// FGE     D,{#}S   {WC/WZ/WCZ}               | Force D >= S. If D < S then D = S and C = 1, else D same and C = 0. *
// FLE     D,{#}S   {WC/WZ/WCZ}               | Force D <= S. If D > S then D = S and C = 1, else D same and C = 0. *
// FGES    D,{#}S   {WC/WZ/WCZ}               | Force D >= S, signed. If D < S then D = S and C = 1, else D same and C = 0. *
// FLES    D,{#}S   {WC/WZ/WCZ}               | Force D <= S, signed. If D > S then D = S and C = 1, else D same and C = 0. *
// SUMC    D,{#}S   {WC/WZ/WCZ}               | Sum +/-S into D by  C. If C = 1 then D = D - S, else D = D + S. C = correct sign of (D +/- S). *
// SUMNC   D,{#}S   {WC/WZ/WCZ}               | Sum +/-S into D by !C. If C = 0 then D = D - S, else D = D + S. C = correct sign of (D +/- S). *
// SUMZ    D,{#}S   {WC/WZ/WCZ}               | Sum +/-S into D by  Z. If Z = 1 then D = D - S, else D = D + S. C = correct sign of (D +/- S). *
// SUMNZ   D,{#}S   {WC/WZ/WCZ}               | Sum +/-S into D by !Z. If Z = 0 then D = D - S, else D = D + S. C = correct sign of (D +/- S). *
// TESTB   D,{#}S         WC/WZ               | Test bit S[4:0] of  D, write to C/Z. C/Z =          D[S[4:0]].
// TESTBN  D,{#}S         WC/WZ               | Test bit S[4:0] of !D, write to C/Z. C/Z =         !D[S[4:0]].
// TESTB   D,{#}S     ANDC/ANDZ               | Test bit S[4:0] of  D, AND into C/Z. C/Z = C/Z AND  D[S[4:0]].
// TESTBN  D,{#}S     ANDC/ANDZ               | Test bit S[4:0] of !D, AND into C/Z. C/Z = C/Z AND !D[S[4:0]].
// TESTB   D,{#}S       ORC/ORZ               | Test bit S[4:0] of  D, OR  into C/Z. C/Z = C/Z OR   D[S[4:0]].
// TESTBN  D,{#}S       ORC/ORZ               | Test bit S[4:0] of !D, OR  into C/Z. C/Z = C/Z OR  !D[S[4:0]].
// TESTB   D,{#}S     XORC/XORZ               | Test bit S[4:0] of  D, XOR into C/Z. C/Z = C/Z XOR  D[S[4:0]].
// TESTBN  D,{#}S     XORC/XORZ               | Test bit S[4:0] of !D, XOR into C/Z. C/Z = C/Z XOR !D[S[4:0]].
// BITL    D,{#}S         {WCZ}               | Bits D[S[9:5]+S[4:0]:S[4:0]] = 0.    Other bits unaffected. Prior SETQ overrides S[9:5]. C,Z = original D[S[4:0]].
// BITH    D,{#}S         {WCZ}               | Bits D[S[9:5]+S[4:0]:S[4:0]] = 1.    Other bits unaffected. Prior SETQ overrides S[9:5]. C,Z = original D[S[4:0]].
// BITC    D,{#}S         {WCZ}               | Bits D[S[9:5]+S[4:0]:S[4:0]] = C.    Other bits unaffected. Prior SETQ overrides S[9:5]. C,Z = original D[S[4:0]].
// BITNC   D,{#}S         {WCZ}               | Bits D[S[9:5]+S[4:0]:S[4:0]] = !C.   Other bits unaffected. Prior SETQ overrides S[9:5]. C,Z = original D[S[4:0]].
// BITZ    D,{#}S         {WCZ}               | Bits D[S[9:5]+S[4:0]:S[4:0]] = Z.    Other bits unaffected. Prior SETQ overrides S[9:5]. C,Z = original D[S[4:0]].
// BITNZ   D,{#}S         {WCZ}               | Bits D[S[9:5]+S[4:0]:S[4:0]] = !Z.   Other bits unaffected. Prior SETQ overrides S[9:5]. C,Z = original D[S[4:0]].
// BITRND  D,{#}S         {WCZ}               | Bits D[S[9:5]+S[4:0]:S[4:0]] = RNDs. Other bits unaffected. Prior SETQ overrides S[9:5]. C,Z = original D[S[4:0]].
// BITNOT  D,{#}S         {WCZ}               | Toggle bits D[S[9:5]+S[4:0]:S[4:0]]. Other bits unaffected. Prior SETQ overrides S[9:5]. C,Z = original D[S[4:0]].

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
// NOT     D,{#}S   {WC/WZ/WCZ}               | Get !S into D. D = !S. C = !S[31]. *
// ABS     D,{#}S   {WC/WZ/WCZ}               | Get absolute value of S into D. D = ABS(S). C = S[31]. *
// NEG     D,{#}S   {WC/WZ/WCZ}               | Negate S into D. D = -S. C = MSB of result. *
// NEGC    D,{#}S   {WC/WZ/WCZ}               | Negate S by  C into D. If C = 1 then D = -S, else D = S. C = MSB of result. *
// NEGNC   D,{#}S   {WC/WZ/WCZ}               | Negate S by !C into D. If C = 0 then D = -S, else D = S. C = MSB of result. *
// NEGZ    D,{#}S   {WC/WZ/WCZ}               | Negate S by  Z into D. If Z = 1 then D = -S, else D = S. C = MSB of result. *
// NEGNZ   D,{#}S   {WC/WZ/WCZ}               | Negate S by !Z into D. If Z = 0 then D = -S, else D = S. C = MSB of result. *
// INCMOD  D,{#}S   {WC/WZ/WCZ}               | Increment with modulus. If D = S then D = 0 and C = 1, else D = D + 1 and C = 0. *
// DECMOD  D,{#}S   {WC/WZ/WCZ}               | Decrement with modulus. If D = 0 then D = S and C = 1, else D = D - 1 and C = 0. *
// ZEROX   D,{#}S   {WC/WZ/WCZ}               | Zero-extend D above bit S[4:0]. C = MSB of result. *
// SIGNX   D,{#}S   {WC/WZ/WCZ}               | Sign-extend D from bit S[4:0]. C = MSB of result. *
// ENCOD   D,{#}S   {WC/WZ/WCZ}               | Get bit position of top-most '1' in S into D. D = position of top '1' in S (0..31). C = (S != 0). *
// ONES    D,{#}S   {WC/WZ/WCZ}               | Get number of '1's in S into D. D = number of '1's in S (0..32). C = LSB of result. *
// TEST    D,{#}S   {WC/WZ/WCZ}               | Test D with S. C = parity of (D & S). Z = ((D & S) == 0).
// TESTN   D,{#}S   {WC/WZ/WCZ}               | Test D with !S. C = parity of (D & !S). Z = ((D & !S) == 0).
// SETNIB  D,{#}S,#N                          | Set S[3:0] into nibble N in D, keeping rest of D same.
// GETNIB  D,{#}S,#N                          | Get nibble N of S into D. D = {28'b0, S.NIBBLE[N]).
// ROLNIB  D,{#}S,#N                          | Rotate-left nibble N of S into D. D = {D[27:0], S.NIBBLE[N]).
// SETBYTE D,{#}S,#N                          | Set S[7:0] into byte N in D, keeping rest of D same.
// GETBYTE D,{#}S,#N                          | Get byte N of S into D. D = {24'b0, S.BYTE[N]).
// ROLBYTE D,{#}S,#N                          | Rotate-left byte N of S into D. D = {D[23:0], S.BYTE[N]).
// SETWORD D,{#}S,#N                          | Set S[15:0] into word N in D, keeping rest of D same.
// GETWORD D,{#}S,#N                          | Get word N of S into D. D = {16'b0, S.WORD[N]).
// ROLWORD D,{#}S,#N                          | Rotate-left word N of S into D. D = {D[15:0], S.WORD[N]).
// SETR    D,{#}S                             | Set R field of D to S[8:0]. D = {D[31:28], S[8:0], D[18:0]}.
// SETD    D,{#}S                             | Set D field of D to S[8:0]. D = {D[31:18], S[8:0], D[8:0]}.
// SETS    D,{#}S                             | Set S field of D to S[8:0]. D = {D[31:9], S[8:0]}.
// DECOD   D,{#}S                             | Decode S[4:0] into D. D = 1 << S[4:0].
// BMASK   D,{#}S                             | Get LSB-justified bit mask of size (S[4:0] + 1) into D. D = ($0_0000_0002 << S[4:0]) - 1.
// CRCBIT  D,{#}S                             | Iterate CRC value in D using C and polynomial in S. If (C XOR D[0]) then D = (D >> 1) XOR S, else D = (D >> 1).
// CRCNIB  D,{#}S                             | Iterate CRC value in D using Q[31:28] and polynomial in S. Like CRCBIT x 4. Q = Q << 4. For long, use SETQ+'REP #1,#8'+CRCNIB.
// MUXNITS D,{#}S                             | For each non-zero bit pair in S, copy that bit pair into the corresponding D bits, else leave that D bit pair the same.
// MUXNIBS D,{#}S                             | For each non-zero nibble in S, copy that nibble into the corresponding D nibble, else leave that D nibble the same.
// MUXQ    D,{#}S                             | Used after SETQ. For each '1' bit in Q, copy the corresponding bit in S into D. D = (D & !Q) | (S & Q).
// MOVBYTS D,{#}S                             | Move bytes within D, per S. D = {D.BYTE[S[7:6]], D.BYTE[S[5:4]], D.BYTE[S[3:2]], D.BYTE[S[1:0]]}.
// MUL     D,{#}S          {WZ}               | D = unsigned (D[15:0] * S[15:0]). Z = (S == 0) | (D == 0).
// MULS    D,{#}S          {WZ}               | D = signed (D[15:0] * S[15:0]).   Z = (S == 0) | (D == 0).
// SCA     D,{#}S          {WZ}               | Next instruction's S value = unsigned (D[15:0] * S[15:0]) >> 16. *
// SCAS    D,{#}S          {WZ}               | Next instruction's S value = signed (D[15:0] * S[15:0]) >> 14. In this scheme, $4000 = 1.0 and $C000 = -1.0. *
// SPLITB  D                                  | Split every 4th bit of D into bytes. D = {D[31], D[27], D[23], D[19], ...D[12], D[8], D[4], D[0]}.
// MERGEB  D                                  | Merge bits of bytes in D. D = {D[31], D[23], D[15], D[7], ...D[24], D[16], D[8], D[0]}.
// SPLITW  D                                  | Split odd/even bits of D into words. D = {D[31], D[29], D[27], D[25], ...D[6], D[4], D[2], D[0]}.
// MERGEW  D                                  | Merge bits of words in D. D = {D[31], D[15], D[30], D[14], ...D[17], D[1], D[16], D[0]}.
// SEUSSF  D                                  | Relocate and periodically invert bits within D. Returns to original value on 32nd iteration. Forward pattern.
// SEUSSR  D                                  | Relocate and periodically invert bits within D. Returns to original value on 32nd iteration. Reverse pattern.
// RGBSQZ  D                                  | Squeeze 8:8:8 RGB value in D[31:8] into 5:6:5 value in D[15:0]. D = {15'b0, D[31:27], D[23:18], D[15:11]}.
// RGBEXP  D                                  | Expand 5:6:5 RGB value in D[15:0] into 8:8:8 value in D[31:8]. D = {D[15:11,15:13], D[10:5,10:9], D[4:0,4:2], 8'b0}.
// XORO32  D                                  | Iterate D with xoroshiro32+ PRNG algorithm and put PRNG result into next instruction's S. D must be non-zero to iterate.
// REV     D                                  | Reverse D bits. D = D[0:31].
// RCZR    D        {WC/WZ/WCZ}               | Rotate C,Z right through D. D = {C, Z, D[31:2]}. C = D[1],  Z = D[0].
// RCZL    D        {WC/WZ/WCZ}               | Rotate C,Z left through D.  D = {D[29:0], C, Z}. C = D[31], Z = D[30].
// WRC     D                                  | Write 0 or 1 to D, according to  C. D = {31'b0,  C}.
// WRNC    D                                  | Write 0 or 1 to D, according to !C. D = {31'b0, !C}.
// WRZ     D                                  | Write 0 or 1 to D, according to  Z. D = {31'b0,  Z}.
// WRNZ    D                                  | Write 0 or 1 to D, according to !Z. D = {31'b0, !Z}.
// MODCZ   c,z      {WC/WZ/WCZ}               | Modify C and Z according to cccc and zzzz. C = cccc[{C,Z}], Z = zzzz[{C,Z}]. See "MODCZ Operand" list.

// Not possible:
// LOC     PA/PB/PTRA/PTRB,#{\}A                                        |Get {12'b0, address[19:0]} into PA/PB/PTRA/PTRB (per W).          If R = 1, address = PC + A, else address = A. "\" forces R = 0.

/// A 32-bit wide array of `T` elements.
fn BitArray(comptime T: type) type {
    return packed struct(u32) {
        const Array = @This();

        pub const Index = @Int(.unsigned, @log2(len));

        pub const len = @divExact(32, @bitSizeOf(T));
        pub const zero: Array = .{ .raw = 0 };

        raw: u32,

        pub fn from_array(arr: [len]T) Array {
            var out: Array = .zero;
            for (arr, 0..) |v, i| {
                out.set(@intCast(i), v);
            }
            return out;
        }

        pub fn to_array(arr: Array) [len]T {
            var out: [len]T = @splat(0);
            for (&out, 0..) |*d, i| {
                d.* = arr.get(@intCast(i));
            }
            return out;
        }

        pub fn set(arr: *Array, index: Index, value: T) void {
            const shift = @bitSizeOf(T) * @as(u5, index);

            const mask = @as(u32, ~@as(T, 0)) << shift;

            arr.raw &= ~mask;
            arr.raw |= @as(u32, value) << shift;
        }

        pub fn get(arr: Array, index: Index) T {
            const shift = @bitSizeOf(T) * @as(u5, index);
            return @truncate(arr.raw >> shift);
        }
    };
}

/// A struct that allows viewing a `u32` as multiple different types.
const Decomposition = packed union(u32) {
    u32: u32,
    i32: i32,
    u16: BitArray(u16),
    u8: BitArray(u8),
    u4: BitArray(u4),
    u2: BitArray(u2),
    u1: BitArray(u1),
    view: packed struct(u32) {
        lsb: u1,
        center: u30,
        msb: u1,
    },

    pub fn from(value: u32) Decomposition {
        return .{ .u32 = value };
    }
};
