const std = @import("std");
const logger = std.log.scoped(.exec);

const encoding = @import("encoding.zig");
const Cog = @import("Cog.zig");

//
// GROUP: Branch A - Call
//

/// CALL #{\}A
/// EEEE 1101101 RAA AAAAAAAAA AAAAAAAAA
///
/// description: Call to A by pushing {C, Z, 10'b0, PC[19:0]} onto stack.                    If R = 1 then PC += A, else PC = A. "\" forces R = 0.
/// cog timing:  4
/// hub timing:  13...20
/// access:      mem=None, reg=None, stack=Push
pub fn call_a(cog: *Cog, args: encoding.AbsPointer) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CALL #{\\}A is not implemented yet!");
    // return .next;
}

/// CALLA #{\}A
/// EEEE 1101110 RAA AAAAAAAAA AAAAAAAAA
///
/// description: Call to A by writing {C, Z, 10'b0, PC[19:0]} to hub long at PTRA++.         If R = 1 then PC += A, else PC = A. "\" forces R = 0.
/// cog timing:  5...12 *
/// hub timing:  14...32 *
/// access:      mem=Write, reg=None, stack=None
pub fn calla_a(cog: *Cog, args: encoding.AbsPointer) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CALLA #{\\}A is not implemented yet!");
    // return .next;
}

/// CALLB #{\}A
/// EEEE 1101111 RAA AAAAAAAAA AAAAAAAAA
///
/// description: Call to A by writing {C, Z, 10'b0, PC[19:0]} to hub long at PTRB++.         If R = 1 then PC += A, else PC = A. "\" forces R = 0.
/// cog timing:  5...12 *
/// hub timing:  14...32 *
/// access:      mem=Write, reg=None, stack=None
pub fn callb_a(cog: *Cog, args: encoding.AbsPointer) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CALLB #{\\}A is not implemented yet!");
    // return .next;
}

/// CALLD PA/PB/PTRA/PTRB, #{\}A
/// EEEE 11100WW RAA AAAAAAAAA AAAAAAAAA
///
/// description: Call to A by writing {C, Z, 10'b0, PC[19:0]} to PA/PB/PTRA/PTRB (per W).    If R = 1 then PC += A, else PC = A. "\" forces R = 0.
/// cog timing:  4
/// hub timing:  13...20
/// access:      mem=None, reg=Per W, stack=None
pub fn calld_a(cog: *Cog, args: encoding.LocStyle) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CALLD PA/PB/PTRA/PTRB, #{\\}A is not implemented yet!");
    // return .next;
}

//
// GROUP: Branch A - Jump
//

/// JMP #{\}A
/// EEEE 1101100 RAA AAAAAAAAA AAAAAAAAA
///
/// description: Jump to A.                                                                  If R = 1 then PC += A, else PC = A. "\" forces R = 0.
/// cog timing:  4
/// hub timing:  13...20
/// access:      mem=None, reg=None, stack=None
pub fn jmp_a(cog: *Cog, args: encoding.AbsPointer) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JMP #{\\}A is not implemented yet!");
    // return .next;
}

//
// GROUP: Branch D - Call
//

/// CALL D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000101101
///
/// description: Call to D by pushing {C, Z, 10'b0, PC[19:0]} onto stack.                C = D[31], Z = D[30], PC = D[19:0].
/// cog timing:  4
/// hub timing:  13...20
/// access:      mem=None, reg=None, stack=Push
pub fn call_d(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CALL D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// CALLA D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000101110
///
/// description: Call to D by writing {C, Z, 10'b0, PC[19:0]} to hub long at PTRA++.     C = D[31], Z = D[30], PC = D[19:0].
/// cog timing:  5...12 *
/// hub timing:  14...32 *
/// access:      mem=Write, reg=None, stack=None
pub fn calla_d(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CALLA D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// CALLB D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000101111
///
/// description: Call to D by writing {C, Z, 10'b0, PC[19:0]} to hub long at PTRB++.     C = D[31], Z = D[30], PC = D[19:0].
/// cog timing:  5...12 *
/// hub timing:  14...32 *
/// access:      mem=Write, reg=None, stack=None
pub fn callb_d(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CALLB D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

//
// GROUP: Branch D - Call+Skip
//

/// EXECF {#}D
/// EEEE 1101011 00L DDDDDDDDD 000110011
///
/// description: Jump to D[9:0] in cog/LUT and set SKIPF pattern to D[31:10]. PC = {10'b0, D[9:0]}.
/// cog timing:  4
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn execf(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("EXECF {#}D is not implemented yet!");
    // return .next;
}

//
// GROUP: Branch D - Jump
//

/// JMP D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000101100
///
/// description: Jump to D.                                                              C = D[31], Z = D[30], PC = D[19:0].
/// cog timing:  4
/// hub timing:  13...20
/// access:      mem=None, reg=None, stack=None
pub fn jmp_d(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JMP D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// JMPREL {#}D
/// EEEE 1101011 00L DDDDDDDDD 000110000
///
/// description: Jump ahead/back by D instructions. For cogex, PC += D[19:0]. For hubex, PC += D[17:0] << 2.
/// cog timing:  4
/// hub timing:  13...20
/// access:      mem=None, reg=None, stack=None
pub fn jmprel(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JMPREL {#}D is not implemented yet!");
    // return .next;
}

//
// GROUP: Branch D - Jump+Skip
//

/// SKIPF {#}D
/// EEEE 1101011 00L DDDDDDDDD 000110010
///
/// description: Skip cog/LUT instructions fast per D. Like SKIP, but instead of cancelling instructions, the PC leaps over them.
/// cog timing:  2
/// hub timing:  ILLEGAL
/// access:      mem=None, reg=None, stack=None
pub fn skipf(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SKIPF {#}D is not implemented yet!");
    // return .next;
}

//
// GROUP: Branch D - Skip
//

/// SKIP {#}D
/// EEEE 1101011 00L DDDDDDDDD 000110001
///
/// description: Skip instructions per D. Subsequent instructions 0..31 get cancelled for each '1' bit in D[0]..D[31].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn skip(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SKIP {#}D is not implemented yet!");
    // return .next;
}

//
// GROUP: Branch Repeat
//

/// REP {#}D, {#}S
/// EEEE 1100110 1LI DDDDDDDDD SSSSSSSSS
///
/// description: Execute next D[8:0] instructions S times. If S = 0, repeat instructions infinitely. If D[8:0] = 0, nothing repeats.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn rep(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("REP {#}D, {#}S is not implemented yet!");
    // return .next;
}

//
// GROUP: Branch Return
//

/// RET {WC/WZ/WCZ}
/// EEEE 1101011 CZ1 000000000 000101101
///
/// description: Return by popping stack (K).                                            C = K[31], Z = K[30], PC = K[19:0].
/// cog timing:  4
/// hub timing:  13...20
/// access:      mem=None, reg=None, stack=Pop
pub fn ret(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RET {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// RETA {WC/WZ/WCZ}
/// EEEE 1101011 CZ1 000000000 000101110
///
/// description: Return by reading hub long (L) at --PTRA.                               C = L[31], Z = L[30], PC = L[19:0].
/// cog timing:  11...18 *
/// hub timing:  20...40 *
/// access:      mem=Read, reg=None, stack=None
pub fn reta(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RETA {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// RETB {WC/WZ/WCZ}
/// EEEE 1101011 CZ1 000000000 000101111
///
/// description: Return by reading hub long (L) at --PTRB.                               C = L[31], Z = L[30], PC = L[19:0].
/// cog timing:  11...18 *
/// hub timing:  20...40 *
/// access:      mem=Read, reg=None, stack=None
pub fn retb(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RETB {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

//
// GROUP: Branch S - Call
//

/// CALLD D, {#}S** {WC/WZ/WCZ}
/// EEEE 1011001 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Call to S** by writing {C, Z, 10'b0, PC[19:0]} to D.                    C = S[31], Z = S[30].
/// cog timing:  4
/// hub timing:  13...20
/// access:      mem=None, reg=D, stack=None
pub fn calld_s(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CALLD D, {#}S** {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// CALLPA {#}D, {#}S**
/// EEEE 1011010 0LI DDDDDDDDD SSSSSSSSS
///
/// description: Call to S** by pushing {C, Z, 10'b0, PC[19:0]} onto stack, copy D to PA.
/// cog timing:  4
/// hub timing:  13...20
/// access:      mem=None, reg=PA, stack=Push
pub fn callpa(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CALLPA {#}D, {#}S** is not implemented yet!");
    // return .next;
}

/// CALLPB {#}D, {#}S**
/// EEEE 1011010 1LI DDDDDDDDD SSSSSSSSS
///
/// description: Call to S** by pushing {C, Z, 10'b0, PC[19:0]} onto stack, copy D to PB.
/// cog timing:  4
/// hub timing:  13...20
/// access:      mem=None, reg=PB, stack=Push
pub fn callpb(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CALLPB {#}D, {#}S** is not implemented yet!");
    // return .next;
}

//
// GROUP: Branch S - Mod & Test
//

/// DJZ D, {#}S**
/// EEEE 1011011 00I DDDDDDDDD SSSSSSSSS
///
/// description: Decrement D and jump to S** if result is zero.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=D, stack=None
pub fn djz(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DJZ D, {#}S** is not implemented yet!");
    // return .next;
}

/// DJNZ D, {#}S**
/// EEEE 1011011 01I DDDDDDDDD SSSSSSSSS
///
/// description: Decrement D and jump to S** if result is not zero.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=D, stack=None
pub fn djnz(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DJNZ D, {#}S** is not implemented yet!");
    // return .next;
}

/// DJF D, {#}S**
/// EEEE 1011011 10I DDDDDDDDD SSSSSSSSS
///
/// description: Decrement D and jump to S** if result is $FFFF_FFFF.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=D, stack=None
pub fn djf(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DJF D, {#}S** is not implemented yet!");
    // return .next;
}

/// DJNF D, {#}S**
/// EEEE 1011011 11I DDDDDDDDD SSSSSSSSS
///
/// description: Decrement D and jump to S** if result is not $FFFF_FFFF.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=D, stack=None
pub fn djnf(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DJNF D, {#}S** is not implemented yet!");
    // return .next;
}

/// IJZ D, {#}S**
/// EEEE 1011100 00I DDDDDDDDD SSSSSSSSS
///
/// description: Increment D and jump to S** if result is zero.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=D, stack=None
pub fn ijz(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("IJZ D, {#}S** is not implemented yet!");
    // return .next;
}

/// IJNZ D, {#}S**
/// EEEE 1011100 01I DDDDDDDDD SSSSSSSSS
///
/// description: Increment D and jump to S** if result is not zero.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=D, stack=None
pub fn ijnz(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("IJNZ D, {#}S** is not implemented yet!");
    // return .next;
}

//
// GROUP: Branch S - Test
//

/// TJZ D, {#}S**
/// EEEE 1011100 10I DDDDDDDDD SSSSSSSSS
///
/// description: Test D and jump to S** if D is zero.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn tjz(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TJZ D, {#}S** is not implemented yet!");
    // return .next;
}

/// TJNZ D, {#}S**
/// EEEE 1011100 11I DDDDDDDDD SSSSSSSSS
///
/// description: Test D and jump to S** if D is not zero.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn tjnz(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TJNZ D, {#}S** is not implemented yet!");
    // return .next;
}

/// TJF D, {#}S**
/// EEEE 1011101 00I DDDDDDDDD SSSSSSSSS
///
/// description: Test D and jump to S** if D is full (D = $FFFF_FFFF).
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn tjf(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TJF D, {#}S** is not implemented yet!");
    // return .next;
}

/// TJNF D, {#}S**
/// EEEE 1011101 01I DDDDDDDDD SSSSSSSSS
///
/// description: Test D and jump to S** if D is not full (D != $FFFF_FFFF).
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn tjnf(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TJNF D, {#}S** is not implemented yet!");
    // return .next;
}

/// TJS D, {#}S**
/// EEEE 1011101 10I DDDDDDDDD SSSSSSSSS
///
/// description: Test D and jump to S** if D is signed (D[31] = 1).
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn tjs(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TJS D, {#}S** is not implemented yet!");
    // return .next;
}

/// TJNS D, {#}S**
/// EEEE 1011101 11I DDDDDDDDD SSSSSSSSS
///
/// description: Test D and jump to S** if D is not signed (D[31] = 0).
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn tjns(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TJNS D, {#}S** is not implemented yet!");
    // return .next;
}

/// TJV D, {#}S**
/// EEEE 1011110 00I DDDDDDDDD SSSSSSSSS
///
/// description: Test D and jump to S** if D overflowed (D[31] != C, C = 'correct sign' from last addition/subtraction).
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn tjv(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TJV D, {#}S** is not implemented yet!");
    // return .next;
}

//
// GROUP: CORDIC Solver
//

/// QMUL {#}D, {#}S
/// EEEE 1101000 0LI DDDDDDDDD SSSSSSSSS
///
/// description: Begin CORDIC unsigned multiplication of D * S. GETQX/GETQY retrieves lower/upper product.
/// cog timing:  2...9
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn qmul(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("QMUL {#}D, {#}S is not implemented yet!");
    // return .next;
}

/// QDIV {#}D, {#}S
/// EEEE 1101000 1LI DDDDDDDDD SSSSSSSSS
///
/// description: Begin CORDIC unsigned division of {SETQ value or 32'b0, D} / S. GETQX/GETQY retrieves quotient/remainder.
/// cog timing:  2...9
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn qdiv(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("QDIV {#}D, {#}S is not implemented yet!");
    // return .next;
}

/// QFRAC {#}D, {#}S
/// EEEE 1101001 0LI DDDDDDDDD SSSSSSSSS
///
/// description: Begin CORDIC unsigned division of {D, SETQ value or 32'b0} / S. GETQX/GETQY retrieves quotient/remainder.
/// cog timing:  2...9
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn qfrac(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("QFRAC {#}D, {#}S is not implemented yet!");
    // return .next;
}

/// QSQRT {#}D, {#}S
/// EEEE 1101001 1LI DDDDDDDDD SSSSSSSSS
///
/// description: Begin CORDIC square root of {S, D}. GETQX retrieves root.
/// cog timing:  2...9
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn qsqrt(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("QSQRT {#}D, {#}S is not implemented yet!");
    // return .next;
}

/// QROTATE {#}D, {#}S
/// EEEE 1101010 0LI DDDDDDDDD SSSSSSSSS
///
/// description: Begin CORDIC rotation of point (D, SETQ value or 32'b0) by angle S. GETQX/GETQY retrieves X/Y.
/// cog timing:  2...9
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn qrotate(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("QROTATE {#}D, {#}S is not implemented yet!");
    // return .next;
}

/// QVECTOR {#}D, {#}S
/// EEEE 1101010 1LI DDDDDDDDD SSSSSSSSS
///
/// description: Begin CORDIC vectoring of point (D, S). GETQX/GETQY retrieves length/angle.
/// cog timing:  2...9
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn qvector(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("QVECTOR {#}D, {#}S is not implemented yet!");
    // return .next;
}

/// QLOG {#}D
/// EEEE 1101011 00L DDDDDDDDD 000001110
///
/// description: Begin CORDIC number-to-logarithm conversion of D. GETQX retrieves log {5'whole_exponent, 27'fractional_exponent}.
/// cog timing:  2...9
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn qlog(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("QLOG {#}D is not implemented yet!");
    // return .next;
}

/// QEXP {#}D
/// EEEE 1101011 00L DDDDDDDDD 000001111
///
/// description: Begin CORDIC logarithm-to-number conversion of D. GETQX retrieves number.
/// cog timing:  2...9
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn qexp(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("QEXP {#}D is not implemented yet!");
    // return .next;
}

/// GETQX D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000011000
///
/// description: Retrieve CORDIC result X into D. Waits, in case result not ready. C = X[31]. *
/// cog timing:  2...58
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn getqx(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("GETQX D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// GETQY D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000011001
///
/// description: Retrieve CORDIC result Y into D. Waits, in case result not ready. C = Y[31]. *
/// cog timing:  2...58
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn getqy(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("GETQY D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

//
// GROUP: Color Space Converter
//

/// SETCY {#}D
/// EEEE 1101011 00L DDDDDDDDD 000111000
///
/// description: Set the colorspace converter "CY" parameter to D[31:0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setcy(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETCY {#}D is not implemented yet!");
    // return .next;
}

/// SETCI {#}D
/// EEEE 1101011 00L DDDDDDDDD 000111001
///
/// description: Set the colorspace converter "CI" parameter to D[31:0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setci(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETCI {#}D is not implemented yet!");
    // return .next;
}

/// SETCQ {#}D
/// EEEE 1101011 00L DDDDDDDDD 000111010
///
/// description: Set the colorspace converter "CQ" parameter to D[31:0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setcq(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETCQ {#}D is not implemented yet!");
    // return .next;
}

/// SETCFRQ {#}D
/// EEEE 1101011 00L DDDDDDDDD 000111011
///
/// description: Set the colorspace converter "CFRQ" parameter to D[31:0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setcfrq(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETCFRQ {#}D is not implemented yet!");
    // return .next;
}

/// SETCMOD {#}D
/// EEEE 1101011 00L DDDDDDDDD 000111100
///
/// description: Set the colorspace converter "CMOD" parameter to D[8:0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setcmod(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETCMOD {#}D is not implemented yet!");
    // return .next;
}

//
// GROUP: Events - Attention
//

/// COGATN {#}D
/// EEEE 1101011 00L DDDDDDDDD 000111111
///
/// description: Strobe "attention" of all cogs whose corresponding bits are high in D[15:0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn cogatn(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("COGATN {#}D is not implemented yet!");
    // return .next;
}

//
// GROUP: Events - Branch
//

/// JINT {#}S**
/// EEEE 1011110 01I 000000000 SSSSSSSSS
///
/// description: Jump to S** if INT event flag is set.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jint(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JINT {#}S** is not implemented yet!");
    // return .next;
}

/// JCT1 {#}S**
/// EEEE 1011110 01I 000000001 SSSSSSSSS
///
/// description: Jump to S** if CT1 event flag is set.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jct1(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JCT1 {#}S** is not implemented yet!");
    // return .next;
}

/// JCT2 {#}S**
/// EEEE 1011110 01I 000000010 SSSSSSSSS
///
/// description: Jump to S** if CT2 event flag is set.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jct2(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JCT2 {#}S** is not implemented yet!");
    // return .next;
}

/// JCT3 {#}S**
/// EEEE 1011110 01I 000000011 SSSSSSSSS
///
/// description: Jump to S** if CT3 event flag is set.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jct3(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JCT3 {#}S** is not implemented yet!");
    // return .next;
}

/// JSE1 {#}S**
/// EEEE 1011110 01I 000000100 SSSSSSSSS
///
/// description: Jump to S** if SE1 event flag is set.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jse1(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JSE1 {#}S** is not implemented yet!");
    // return .next;
}

/// JSE2 {#}S**
/// EEEE 1011110 01I 000000101 SSSSSSSSS
///
/// description: Jump to S** if SE2 event flag is set.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jse2(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JSE2 {#}S** is not implemented yet!");
    // return .next;
}

/// JSE3 {#}S**
/// EEEE 1011110 01I 000000110 SSSSSSSSS
///
/// description: Jump to S** if SE3 event flag is set.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jse3(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JSE3 {#}S** is not implemented yet!");
    // return .next;
}

/// JSE4 {#}S**
/// EEEE 1011110 01I 000000111 SSSSSSSSS
///
/// description: Jump to S** if SE4 event flag is set.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jse4(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JSE4 {#}S** is not implemented yet!");
    // return .next;
}

/// JPAT {#}S**
/// EEEE 1011110 01I 000001000 SSSSSSSSS
///
/// description: Jump to S** if PAT event flag is set.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jpat(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JPAT {#}S** is not implemented yet!");
    // return .next;
}

/// JFBW {#}S**
/// EEEE 1011110 01I 000001001 SSSSSSSSS
///
/// description: Jump to S** if FBW event flag is set.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jfbw(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JFBW {#}S** is not implemented yet!");
    // return .next;
}

/// JXMT {#}S**
/// EEEE 1011110 01I 000001010 SSSSSSSSS
///
/// description: Jump to S** if XMT event flag is set.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jxmt(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JXMT {#}S** is not implemented yet!");
    // return .next;
}

/// JXFI {#}S**
/// EEEE 1011110 01I 000001011 SSSSSSSSS
///
/// description: Jump to S** if XFI event flag is set.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jxfi(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JXFI {#}S** is not implemented yet!");
    // return .next;
}

/// JXRO {#}S**
/// EEEE 1011110 01I 000001100 SSSSSSSSS
///
/// description: Jump to S** if XRO event flag is set.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jxro(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JXRO {#}S** is not implemented yet!");
    // return .next;
}

/// JXRL {#}S**
/// EEEE 1011110 01I 000001101 SSSSSSSSS
///
/// description: Jump to S** if XRL event flag is set.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jxrl(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JXRL {#}S** is not implemented yet!");
    // return .next;
}

/// JATN {#}S**
/// EEEE 1011110 01I 000001110 SSSSSSSSS
///
/// description: Jump to S** if ATN event flag is set.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jatn(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JATN {#}S** is not implemented yet!");
    // return .next;
}

/// JQMT {#}S**
/// EEEE 1011110 01I 000001111 SSSSSSSSS
///
/// description: Jump to S** if QMT event flag is set.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jqmt(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JQMT {#}S** is not implemented yet!");
    // return .next;
}

/// JNINT {#}S**
/// EEEE 1011110 01I 000010000 SSSSSSSSS
///
/// description: Jump to S** if INT event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnint(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNINT {#}S** is not implemented yet!");
    // return .next;
}

/// JNCT1 {#}S**
/// EEEE 1011110 01I 000010001 SSSSSSSSS
///
/// description: Jump to S** if CT1 event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnct1(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNCT1 {#}S** is not implemented yet!");
    // return .next;
}

/// JNCT2 {#}S**
/// EEEE 1011110 01I 000010010 SSSSSSSSS
///
/// description: Jump to S** if CT2 event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnct2(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNCT2 {#}S** is not implemented yet!");
    // return .next;
}

/// JNCT3 {#}S**
/// EEEE 1011110 01I 000010011 SSSSSSSSS
///
/// description: Jump to S** if CT3 event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnct3(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNCT3 {#}S** is not implemented yet!");
    // return .next;
}

/// JNSE1 {#}S**
/// EEEE 1011110 01I 000010100 SSSSSSSSS
///
/// description: Jump to S** if SE1 event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnse1(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNSE1 {#}S** is not implemented yet!");
    // return .next;
}

/// JNSE2 {#}S**
/// EEEE 1011110 01I 000010101 SSSSSSSSS
///
/// description: Jump to S** if SE2 event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnse2(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNSE2 {#}S** is not implemented yet!");
    // return .next;
}

/// JNSE3 {#}S**
/// EEEE 1011110 01I 000010110 SSSSSSSSS
///
/// description: Jump to S** if SE3 event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnse3(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNSE3 {#}S** is not implemented yet!");
    // return .next;
}

/// JNSE4 {#}S**
/// EEEE 1011110 01I 000010111 SSSSSSSSS
///
/// description: Jump to S** if SE4 event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnse4(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNSE4 {#}S** is not implemented yet!");
    // return .next;
}

/// JNPAT {#}S**
/// EEEE 1011110 01I 000011000 SSSSSSSSS
///
/// description: Jump to S** if PAT event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnpat(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNPAT {#}S** is not implemented yet!");
    // return .next;
}

/// JNFBW {#}S**
/// EEEE 1011110 01I 000011001 SSSSSSSSS
///
/// description: Jump to S** if FBW event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnfbw(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNFBW {#}S** is not implemented yet!");
    // return .next;
}

/// JNXMT {#}S**
/// EEEE 1011110 01I 000011010 SSSSSSSSS
///
/// description: Jump to S** if XMT event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnxmt(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNXMT {#}S** is not implemented yet!");
    // return .next;
}

/// JNXFI {#}S**
/// EEEE 1011110 01I 000011011 SSSSSSSSS
///
/// description: Jump to S** if XFI event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnxfi(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNXFI {#}S** is not implemented yet!");
    // return .next;
}

/// JNXRO {#}S**
/// EEEE 1011110 01I 000011100 SSSSSSSSS
///
/// description: Jump to S** if XRO event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnxro(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNXRO {#}S** is not implemented yet!");
    // return .next;
}

/// JNXRL {#}S**
/// EEEE 1011110 01I 000011101 SSSSSSSSS
///
/// description: Jump to S** if XRL event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnxrl(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNXRL {#}S** is not implemented yet!");
    // return .next;
}

/// JNATN {#}S**
/// EEEE 1011110 01I 000011110 SSSSSSSSS
///
/// description: Jump to S** if ATN event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnatn(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNATN {#}S** is not implemented yet!");
    // return .next;
}

/// JNQMT {#}S**
/// EEEE 1011110 01I 000011111 SSSSSSSSS
///
/// description: Jump to S** if QMT event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnqmt(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNQMT {#}S** is not implemented yet!");
    // return .next;
}

//
// GROUP: Events - Configuration
//

/// ADDCT1 D, {#}S
/// EEEE 1010011 00I DDDDDDDDD SSSSSSSSS
///
/// description: Set CT1 event to trigger on CT = D + S. Adds S into D.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn addct1(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ADDCT1 D, {#}S is not implemented yet!");
    // return .next;
}

/// ADDCT2 D, {#}S
/// EEEE 1010011 01I DDDDDDDDD SSSSSSSSS
///
/// description: Set CT2 event to trigger on CT = D + S. Adds S into D.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn addct2(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ADDCT2 D, {#}S is not implemented yet!");
    // return .next;
}

/// ADDCT3 D, {#}S
/// EEEE 1010011 10I DDDDDDDDD SSSSSSSSS
///
/// description: Set CT3 event to trigger on CT = D + S. Adds S into D.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn addct3(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ADDCT3 D, {#}S is not implemented yet!");
    // return .next;
}

/// SETPAT {#}D, {#}S
/// EEEE 1011111 1LI DDDDDDDDD SSSSSSSSS
///
/// description: Set pin pattern for PAT event. C selects INA/INB, Z selects =/!=, D provides mask value, S provides match value.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setpat(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETPAT {#}D, {#}S is not implemented yet!");
    // return .next;
}

/// SETSE1 {#}D
/// EEEE 1101011 00L DDDDDDDDD 000100000
///
/// description: Set SE1 event configuration to D[8:0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setse1(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETSE1 {#}D is not implemented yet!");
    // return .next;
}

/// SETSE2 {#}D
/// EEEE 1101011 00L DDDDDDDDD 000100001
///
/// description: Set SE2 event configuration to D[8:0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setse2(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETSE2 {#}D is not implemented yet!");
    // return .next;
}

/// SETSE3 {#}D
/// EEEE 1101011 00L DDDDDDDDD 000100010
///
/// description: Set SE3 event configuration to D[8:0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setse3(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETSE3 {#}D is not implemented yet!");
    // return .next;
}

/// SETSE4 {#}D
/// EEEE 1101011 00L DDDDDDDDD 000100011
///
/// description: Set SE4 event configuration to D[8:0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setse4(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETSE4 {#}D is not implemented yet!");
    // return .next;
}

//
// GROUP: Events - Poll
//

/// POLLINT {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000000000 000100100
///
/// description: Get INT event flag into C/Z, then clear it.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn pollint(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLINT {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// POLLCT1 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000000001 000100100
///
/// description: Get CT1 event flag into C/Z, then clear it.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn pollct1(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLCT1 {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// POLLCT2 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000000010 000100100
///
/// description: Get CT2 event flag into C/Z, then clear it.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn pollct2(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLCT2 {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// POLLCT3 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000000011 000100100
///
/// description: Get CT3 event flag into C/Z, then clear it.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn pollct3(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLCT3 {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// POLLSE1 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000000100 000100100
///
/// description: Get SE1 event flag into C/Z, then clear it.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn pollse1(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLSE1 {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// POLLSE2 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000000101 000100100
///
/// description: Get SE2 event flag into C/Z, then clear it.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn pollse2(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLSE2 {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// POLLSE3 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000000110 000100100
///
/// description: Get SE3 event flag into C/Z, then clear it.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn pollse3(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLSE3 {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// POLLSE4 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000000111 000100100
///
/// description: Get SE4 event flag into C/Z, then clear it.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn pollse4(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLSE4 {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// POLLPAT {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000001000 000100100
///
/// description: Get PAT event flag into C/Z, then clear it.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn pollpat(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLPAT {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// POLLFBW {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000001001 000100100
///
/// description: Get FBW event flag into C/Z, then clear it.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn pollfbw(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLFBW {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// POLLXMT {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000001010 000100100
///
/// description: Get XMT event flag into C/Z, then clear it.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn pollxmt(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLXMT {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// POLLXFI {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000001011 000100100
///
/// description: Get XFI event flag into C/Z, then clear it.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn pollxfi(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLXFI {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// POLLXRO {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000001100 000100100
///
/// description: Get XRO event flag into C/Z, then clear it.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn pollxro(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLXRO {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// POLLXRL {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000001101 000100100
///
/// description: Get XRL event flag into C/Z, then clear it.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn pollxrl(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLXRL {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// POLLATN {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000001110 000100100
///
/// description: Get ATN event flag into C/Z, then clear it.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn pollatn(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLATN {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// POLLQMT {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000001111 000100100
///
/// description: Get QMT event flag into C/Z, then clear it.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn pollqmt(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLQMT {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

//
// GROUP: Events - Wait
//

/// WAITINT {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000010000 000100100
///
/// description: Wait for INT event flag, then clear it. Prior SETQ sets optional CT timeout value. C/Z = timeout.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn waitint(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITINT {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WAITCT1 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000010001 000100100
///
/// description: Wait for CT1 event flag, then clear it. Prior SETQ sets optional CT timeout value. C/Z = timeout.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn waitct1(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITCT1 {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WAITCT2 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000010010 000100100
///
/// description: Wait for CT2 event flag, then clear it. Prior SETQ sets optional CT timeout value. C/Z = timeout.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn waitct2(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITCT2 {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WAITCT3 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000010011 000100100
///
/// description: Wait for CT3 event flag, then clear it. Prior SETQ sets optional CT timeout value. C/Z = timeout.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn waitct3(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITCT3 {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WAITSE1 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000010100 000100100
///
/// description: Wait for SE1 event flag, then clear it. Prior SETQ sets optional CT timeout value. C/Z = timeout.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn waitse1(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITSE1 {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WAITSE2 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000010101 000100100
///
/// description: Wait for SE2 event flag, then clear it. Prior SETQ sets optional CT timeout value. C/Z = timeout.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn waitse2(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITSE2 {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WAITSE3 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000010110 000100100
///
/// description: Wait for SE3 event flag, then clear it. Prior SETQ sets optional CT timeout value. C/Z = timeout.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn waitse3(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITSE3 {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WAITSE4 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000010111 000100100
///
/// description: Wait for SE4 event flag, then clear it. Prior SETQ sets optional CT timeout value. C/Z = timeout.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn waitse4(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITSE4 {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WAITPAT {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000011000 000100100
///
/// description: Wait for PAT event flag, then clear it. Prior SETQ sets optional CT timeout value. C/Z = timeout.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn waitpat(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITPAT {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WAITFBW {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000011001 000100100
///
/// description: Wait for FBW event flag, then clear it. Prior SETQ sets optional CT timeout value. C/Z = timeout.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn waitfbw(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITFBW {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WAITXMT {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000011010 000100100
///
/// description: Wait for XMT event flag, then clear it. Prior SETQ sets optional CT timeout value. C/Z = timeout.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn waitxmt(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITXMT {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WAITXFI {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000011011 000100100
///
/// description: Wait for XFI event flag, then clear it. Prior SETQ sets optional CT timeout value. C/Z = timeout.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn waitxfi(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITXFI {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WAITXRO {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000011100 000100100
///
/// description: Wait for XRO event flag, then clear it. Prior SETQ sets optional CT timeout value. C/Z = timeout.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn waitxro(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITXRO {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WAITXRL {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000011101 000100100
///
/// description: Wait for XRL event flag, then clear it. Prior SETQ sets optional CT timeout value. C/Z = timeout.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn waitxrl(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITXRL {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WAITATN {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000011110 000100100
///
/// description: Wait for ATN event flag, then clear it. Prior SETQ sets optional CT timeout value. C/Z = timeout.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn waitatn(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITATN {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

//
// GROUP: Hub Control - Cogs
//

/// COGINIT {#}D, {#}S {WC}
/// EEEE 1100111 CLI DDDDDDDDD SSSSSSSSS
///
/// description: Start cog selected by D. S[19:0] sets hub startup address and PTRB of cog. Prior SETQ sets PTRA of cog. C = 1 if no cog available.
/// cog timing:  2...9, +2 if result
/// hub timing:  same
/// access:      mem=None, reg=D if reg and WC, stack=None
pub fn coginit(cog: *Cog, args: encoding.Both_Dimm_Simm_CFlag) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("COGINIT {#}D, {#}S {WC} is not implemented yet!");
    // return .next;
}

/// COGID {#}D {WC}
/// EEEE 1101011 C0L DDDDDDDDD 000000001
///
/// description: If D is register and no WC, get cog ID (0 to 15) into D. If WC, check status of cog D[3:0], C = 1 if on.
/// cog timing:  2...9, +2 if result
/// hub timing:  same
/// access:      mem=None, reg=D if reg and !WC, stack=None
pub fn cogid(cog: *Cog, args: encoding.Only_Dimm_CFlag) Cog.ExecResult {
    if (!cog.is_condition_met(args.cond))
        return .skip;

    if (args.c_mod == .write) {
        // If COGID is used with WC, it will not overwrite D, but will return the status of
        // cog D/# into C, where C=0 indicates the cog is free (stopped or never started)
        // and C=1 indicates the cog is busy (started).

        const id: u3 = @truncate(cog.resolve_operand(args.d, args.d_imm));

        // COGID ThatCog WC ' C=1 if ThatCog is busy
        cog.c = (cog.hub.cogs[id].exec_mode != .stopped);
        return .next;
    } else {
        // A cog can discover its own ID by doing a COGID instruction, which will
        // return its ID into D[3:0], with upper bits cleared.
        // This is useful, in case the cog wants to restart or stop itself, as shown above.

        if (args.d_imm)
            return .trap; // TODO: Figure this out
        cog.write_reg(args.d, cog.id);
        return .next;
    }
}

/// COGSTOP {#}D
/// EEEE 1101011 00L DDDDDDDDD 000000011
///
/// description: Stop cog D[3:0].
/// cog timing:  2...9
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn cogstop(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    if (!cog.is_condition_met(args.cond))
        return .skip;

    const id: u3 = @truncate(cog.resolve_operand(args.d, args.d_imm));

    logger.info("stop cog {}", .{id});
    cog.hub.cogs[id].reset();

    return .next;
}

//
// GROUP: Hub Control - Locks
//

/// LOCKNEW D {WC}
/// EEEE 1101011 C00 DDDDDDDDD 000000100
///
/// description: Request a LOCK. D will be written with the LOCK number (0 to 15). C = 1 if no LOCK available.
/// cog timing:  4...11
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn locknew(cog: *Cog, args: encoding.Only_D_CFlag) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("LOCKNEW D {WC} is not implemented yet!");
    // return .next;
}

/// LOCKRET {#}D
/// EEEE 1101011 00L DDDDDDDDD 000000101
///
/// description: Return LOCK D[3:0] for reallocation.
/// cog timing:  2...9
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn lockret(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("LOCKRET {#}D is not implemented yet!");
    // return .next;
}

/// LOCKTRY {#}D {WC}
/// EEEE 1101011 C0L DDDDDDDDD 000000110
///
/// description: Try to get LOCK D[3:0]. C = 1 if got LOCK. LOCKREL releases LOCK. LOCK is also released if owner cog stops or restarts.
/// cog timing:  2...9, +2 if result
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn locktry(cog: *Cog, args: encoding.Only_Dimm_CFlag) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("LOCKTRY {#}D {WC} is not implemented yet!");
    // return .next;
}

/// LOCKREL {#}D {WC}
/// EEEE 1101011 C0L DDDDDDDDD 000000111
///
/// description: Release LOCK D[3:0]. If D is a register and WC, get current/last cog ID of LOCK owner into D and LOCK status into C.
/// cog timing:  2...9, +2 if result
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn lockrel(cog: *Cog, args: encoding.Only_Dimm_CFlag) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("LOCKREL {#}D {WC} is not implemented yet!");
    // return .next;
}

//
// GROUP: Hub Control - Multi
//

/// HUBSET {#}D
/// EEEE 1101011 00L DDDDDDDDD 000000000
///
/// description: Set hub configuration to D.
/// cog timing:  2...9
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn hubset(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("HUBSET {#}D is not implemented yet!");
    // return .next;
}

//
// GROUP: Hub FIFO
//

/// GETPTR D
/// EEEE 1101011 000 DDDDDDDDD 000110100
///
/// description: Get current FIFO hub pointer into D.
/// cog timing:  2
/// hub timing:  FIFO IN USE
/// access:      mem=None, reg=D, stack=None
pub fn getptr(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("GETPTR D is not implemented yet!");
    // return .next;
}

//
// GROUP: Hub FIFO - New Block
//

/// FBLOCK {#}D, {#}S
/// EEEE 1100100 1LI DDDDDDDDD SSSSSSSSS
///
/// description: Set next block for when block wraps. D[13:0] = block size in 64-byte units (0 = max), S[19:0] = block start address.
/// cog timing:  2
/// hub timing:  FIFO IN USE
/// access:      mem=None, reg=None, stack=None
pub fn fblock(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("FBLOCK {#}D, {#}S is not implemented yet!");
    // return .next;
}

//
// GROUP: Hub FIFO - New Read
//

/// RDFAST {#}D, {#}S
/// EEEE 1100011 1LI DDDDDDDDD SSSSSSSSS
///
/// description: Begin new fast hub read via FIFO.  D[31] = no wait, D[13:0] = block size in 64-byte units (0 = max), S[19:0] = block start address.
/// cog timing:  2 or WRFAST finish + 10...17
/// hub timing:  FIFO IN USE
/// access:      mem=None, reg=None, stack=None
pub fn rdfast(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RDFAST {#}D, {#}S is not implemented yet!");
    // return .next;
}

//
// GROUP: Hub FIFO - New Write
//

/// WRFAST {#}D, {#}S
/// EEEE 1100100 0LI DDDDDDDDD SSSSSSSSS
///
/// description: Begin new fast hub write via FIFO. D[31] = no wait, D[13:0] = block size in 64-byte units (0 = max), S[19:0] = block start address.
/// cog timing:  2 or WRFAST finish + 3
/// hub timing:  FIFO IN USE
/// access:      mem=None, reg=None, stack=None
pub fn wrfast(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WRFAST {#}D, {#}S is not implemented yet!");
    // return .next;
}

//
// GROUP: Hub FIFO - Read
//

/// RFBYTE D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000010000
///
/// description: Used after RDFAST. Read zero-extended byte from FIFO into D. C = MSB of byte. *
/// cog timing:  2
/// hub timing:  FIFO IN USE
/// access:      mem=Read, reg=D, stack=None
pub fn rfbyte(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RFBYTE D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// RFWORD D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000010001
///
/// description: Used after RDFAST. Read zero-extended word from FIFO into D. C = MSB of word. *
/// cog timing:  2
/// hub timing:  FIFO IN USE
/// access:      mem=Read, reg=D, stack=None
pub fn rfword(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RFWORD D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// RFLONG D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000010010
///
/// description: Used after RDFAST. Read long from FIFO into D. C = MSB of long. *
/// cog timing:  2
/// hub timing:  FIFO IN USE
/// access:      mem=Read, reg=D, stack=None
pub fn rflong(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RFLONG D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// RFVAR D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000010011
///
/// description: Used after RDFAST. Read zero-extended 1..4-byte value from FIFO into D. C = 0. *
/// cog timing:  2
/// hub timing:  FIFO IN USE
/// access:      mem=Read, reg=D, stack=None
pub fn rfvar(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RFVAR D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// RFVARS D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000010100
///
/// description: Used after RDFAST. Read sign-extended 1..4-byte value from FIFO into D. C = MSB of value. *
/// cog timing:  2
/// hub timing:  FIFO IN USE
/// access:      mem=Read, reg=D, stack=None
pub fn rfvars(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RFVARS D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

//
// GROUP: Hub FIFO - Write
//

/// WFBYTE {#}D
/// EEEE 1101011 00L DDDDDDDDD 000010101
///
/// description: Used after WRFAST. Write byte in D[7:0] into FIFO.
/// cog timing:  2
/// hub timing:  FIFO IN USE
/// access:      mem=Write, reg=None, stack=None
pub fn wfbyte(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WFBYTE {#}D is not implemented yet!");
    // return .next;
}

/// WFWORD {#}D
/// EEEE 1101011 00L DDDDDDDDD 000010110
///
/// description: Used after WRFAST. Write word in D[15:0] into FIFO.
/// cog timing:  2
/// hub timing:  FIFO IN USE
/// access:      mem=Write, reg=None, stack=None
pub fn wfword(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WFWORD {#}D is not implemented yet!");
    // return .next;
}

/// WFLONG {#}D
/// EEEE 1101011 00L DDDDDDDDD 000010111
///
/// description: Used after WRFAST. Write long in D[31:0] into FIFO.
/// cog timing:  2
/// hub timing:  FIFO IN USE
/// access:      mem=Write, reg=None, stack=None
pub fn wflong(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WFLONG {#}D is not implemented yet!");
    // return .next;
}

//
// GROUP: Hub RAM - Read
//

/// RDBYTE D, {#}S/P {WC/WZ/WCZ}
/// EEEE 1010110 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Read zero-extended byte from hub address {#}S/PTRx into D. C = MSB of byte. *
/// cog timing:  9...16
/// hub timing:  9...26
/// access:      mem=Read, reg=D, stack=None
pub fn rdbyte(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RDBYTE D, {#}S/P {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// RDWORD D, {#}S/P {WC/WZ/WCZ}
/// EEEE 1010111 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Read zero-extended word from hub address {#}S/PTRx into D. C = MSB of word. *
/// cog timing:  9...16 *
/// hub timing:  9...26 *
/// access:      mem=Read, reg=D, stack=None
pub fn rdword(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RDWORD D, {#}S/P {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// RDLONG D, {#}S/P {WC/WZ/WCZ}
/// EEEE 1011000 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Read long from hub address {#}S/PTRx into D. C = MSB of long. *   Prior SETQ/SETQ2 invokes cog/LUT block transfer.
/// cog timing:  9...16 *
/// hub timing:  9...26 *
/// access:      mem=Read, reg=D, stack=None
pub fn rdlong(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RDLONG D, {#}S/P {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

//
// GROUP: Hub RAM - Write
//

/// WMLONG D, {#}S/P
/// EEEE 1010011 11I DDDDDDDDD SSSSSSSSS
///
/// description: Write only non-$00 bytes in D[31:0] to hub address {#}S/PTRx.     Prior SETQ/SETQ2 invokes cog/LUT block transfer.
/// cog timing:  3...10 *
/// hub timing:  3...20 *
/// access:      mem=Write, reg=None, stack=None
pub fn wmlong(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WMLONG D, {#}S/P is not implemented yet!");
    // return .next;
}

/// WRBYTE {#}D, {#}S/P
/// EEEE 1100010 0LI DDDDDDDDD SSSSSSSSS
///
/// description: Write byte in D[7:0] to hub address {#}S/PTRx.
/// cog timing:  3...10
/// hub timing:  3...20
/// access:      mem=Write, reg=None, stack=None
pub fn wrbyte(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WRBYTE {#}D, {#}S/P is not implemented yet!");
    // return .next;
}

/// WRWORD {#}D, {#}S/P
/// EEEE 1100010 1LI DDDDDDDDD SSSSSSSSS
///
/// description: Write word in D[15:0] to hub address {#}S/PTRx.
/// cog timing:  3...10*
/// hub timing:  3...20 *
/// access:      mem=Write, reg=None, stack=None
pub fn wrword(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WRWORD {#}D, {#}S/P is not implemented yet!");
    // return .next;
}

/// WRLONG {#}D, {#}S/P
/// EEEE 1100011 0LI DDDDDDDDD SSSSSSSSS
///
/// description: Write long in D[31:0] to hub address {#}S/PTRx.                   Prior SETQ/SETQ2 invokes cog/LUT block transfer.
/// cog timing:  3...10*
/// hub timing:  3...20 *
/// access:      mem=Write, reg=None, stack=None
pub fn wrlong(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WRLONG {#}D, {#}S/P is not implemented yet!");
    // return .next;
}

//
// GROUP: Interrupts
//

/// ALLOWI
/// EEEE 1101011 000 000100000 000100100
///
/// description: Allow interrupts (default).
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn allowi(cog: *Cog, args: encoding.NoOperands) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ALLOWI is not implemented yet!");
    // return .next;
}

/// STALLI
/// EEEE 1101011 000 000100001 000100100
///
/// description: Stall Interrupts.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn stalli(cog: *Cog, args: encoding.NoOperands) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("STALLI is not implemented yet!");
    // return .next;
}

/// TRGINT1
/// EEEE 1101011 000 000100010 000100100
///
/// description: Trigger INT1, regardless of STALLI mode.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn trgint1(cog: *Cog, args: encoding.NoOperands) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TRGINT1 is not implemented yet!");
    // return .next;
}

/// TRGINT2
/// EEEE 1101011 000 000100011 000100100
///
/// description: Trigger INT2, regardless of STALLI mode.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn trgint2(cog: *Cog, args: encoding.NoOperands) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TRGINT2 is not implemented yet!");
    // return .next;
}

/// TRGINT3
/// EEEE 1101011 000 000100100 000100100
///
/// description: Trigger INT3, regardless of STALLI mode.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn trgint3(cog: *Cog, args: encoding.NoOperands) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TRGINT3 is not implemented yet!");
    // return .next;
}

/// NIXINT1
/// EEEE 1101011 000 000100101 000100100
///
/// description: Cancel INT1.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn nixint1(cog: *Cog, args: encoding.NoOperands) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("NIXINT1 is not implemented yet!");
    // return .next;
}

/// NIXINT2
/// EEEE 1101011 000 000100110 000100100
///
/// description: Cancel INT2.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn nixint2(cog: *Cog, args: encoding.NoOperands) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("NIXINT2 is not implemented yet!");
    // return .next;
}

/// NIXINT3
/// EEEE 1101011 000 000100111 000100100
///
/// description: Cancel INT3.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn nixint3(cog: *Cog, args: encoding.NoOperands) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("NIXINT3 is not implemented yet!");
    // return .next;
}

/// SETINT1 {#}D
/// EEEE 1101011 00L DDDDDDDDD 000100101
///
/// description: Set INT1 source to D[3:0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setint1(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETINT1 {#}D is not implemented yet!");
    // return .next;
}

/// SETINT2 {#}D
/// EEEE 1101011 00L DDDDDDDDD 000100110
///
/// description: Set INT2 source to D[3:0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setint2(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETINT2 {#}D is not implemented yet!");
    // return .next;
}

/// SETINT3 {#}D
/// EEEE 1101011 00L DDDDDDDDD 000100111
///
/// description: Set INT3 source to D[3:0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setint3(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETINT3 {#}D is not implemented yet!");
    // return .next;
}

/// GETBRK D WC/WZ/WCZ
/// EEEE 1101011 CZ0 DDDDDDDDD 000110101
///
/// description: Get breakpoint/cog status into D according to WC/WZ/WCZ. See documentation for details.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn getbrk(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("GETBRK D WC/WZ/WCZ is not implemented yet!");
    // return .next;
}

/// COGBRK {#}D
/// EEEE 1101011 00L DDDDDDDDD 000110101
///
/// description: If in debug ISR, trigger asynchronous breakpoint in cog D[3:0]. Cog D[3:0] must have asynchronous breakpoint enabled.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn cogbrk(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("COGBRK {#}D is not implemented yet!");
    // return .next;
}

/// BRK {#}D
/// EEEE 1101011 00L DDDDDDDDD 000110110
///
/// description: If in debug ISR, set next break condition to D. Else, set BRK code to D[7:0] and unconditionally trigger BRK interrupt, if enabled.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn brk(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("BRK {#}D is not implemented yet!");
    // return .next;
}

//
// GROUP: Lookup Table
//

/// RDLUT D, {#}S/P {WC/WZ/WCZ}
/// EEEE 1010101 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Read data from LUT address {#}S/PTRx into D. C = MSB of data. *
/// cog timing:  3
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn rdlut(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RDLUT D, {#}S/P {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WRLUT {#}D, {#}S/P
/// EEEE 1100001 1LI DDDDDDDDD SSSSSSSSS
///
/// description: Write D to LUT address {#}S/PTRx.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn wrlut(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WRLUT {#}D, {#}S/P is not implemented yet!");
    // return .next;
}

/// SETLUTS {#}D
/// EEEE 1101011 00L DDDDDDDDD 000110111
///
/// description: If D[0] = 1 then enable LUT sharing, where LUT writes within the adjacent odd/even companion cog are copied to this cog's LUT.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setluts(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETLUTS {#}D is not implemented yet!");
    // return .next;
}

//
// GROUP: Math and Logic
//

/// ROR D, {#}S {WC/WZ/WCZ}
/// EEEE 0000000 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Rotate right.           D = [31:0]  of ({D[31:0], D[31:0]}     >> S[4:0]). C = last bit shifted out if S[4:0] > 0, else D[0].  *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn ror(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ROR D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// ROL D, {#}S {WC/WZ/WCZ}
/// EEEE 0000001 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Rotate left.            D = [63:32] of ({D[31:0], D[31:0]}     << S[4:0]). C = last bit shifted out if S[4:0] > 0, else D[31]. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn rol(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ROL D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// SHR D, {#}S {WC/WZ/WCZ}
/// EEEE 0000010 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Shift right.            D = [31:0]  of ({32'b0, D[31:0]}       >> S[4:0]). C = last bit shifted out if S[4:0] > 0, else D[0].  *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn shr(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SHR D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// SHL D, {#}S {WC/WZ/WCZ}
/// EEEE 0000011 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Shift left.             D = [63:32] of ({D[31:0], 32'b0}       << S[4:0]). C = last bit shifted out if S[4:0] > 0, else D[31]. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn shl(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SHL D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// RCR D, {#}S {WC/WZ/WCZ}
/// EEEE 0000100 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Rotate carry right.     D = [31:0]  of ({{32{C}}, D[31:0]}     >> S[4:0]). C = last bit shifted out if S[4:0] > 0, else D[0].  *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn rcr(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RCR D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// RCL D, {#}S {WC/WZ/WCZ}
/// EEEE 0000101 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Rotate carry left.      D = [63:32] of ({D[31:0], {32{C}}}     << S[4:0]). C = last bit shifted out if S[4:0] > 0, else D[31]. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn rcl(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RCL D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// SAR D, {#}S {WC/WZ/WCZ}
/// EEEE 0000110 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Shift arithmetic right. D = [31:0]  of ({{32{D[31]}}, D[31:0]} >> S[4:0]). C = last bit shifted out if S[4:0] > 0, else D[0].  *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn sar(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SAR D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// SAL D, {#}S {WC/WZ/WCZ}
/// EEEE 0000111 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Shift arithmetic left.  D = [63:32] of ({D[31:0], {32{D[0]}}}  << S[4:0]). C = last bit shifted out if S[4:0] > 0, else D[31]. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn sal(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SAL D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// ADD D, {#}S {WC/WZ/WCZ}
/// EEEE 0001000 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Add S into D.                                  D = D + S.        C = carry of (D + S).               *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn add(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ADD D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// ADDX D, {#}S {WC/WZ/WCZ}
/// EEEE 0001001 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Add (S + C) into D, extended.                  D = D + S + C.    C = carry of (D + S + C).           Z = Z AND (result == 0).
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn addx(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ADDX D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// ADDS D, {#}S {WC/WZ/WCZ}
/// EEEE 0001010 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Add S into D, signed.                          D = D + S.        C = correct sign of (D + S).        *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn adds(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ADDS D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// ADDSX D, {#}S {WC/WZ/WCZ}
/// EEEE 0001011 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Add (S + C) into D, signed and extended.       D = D + S + C.    C = correct sign of (D + S + C).    Z = Z AND (result == 0).
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn addsx(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ADDSX D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// SUB D, {#}S {WC/WZ/WCZ}
/// EEEE 0001100 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Subtract S from D.                             D = D - S.        C = borrow of (D - S).              *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn sub(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SUB D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// SUBX D, {#}S {WC/WZ/WCZ}
/// EEEE 0001101 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Subtract (S + C) from D, extended.             D = D - (S + C).  C = borrow of (D - (S + C)).        Z = Z AND (result == 0).
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn subx(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SUBX D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// SUBS D, {#}S {WC/WZ/WCZ}
/// EEEE 0001110 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Subtract S from D, signed.                     D = D - S.        C = correct sign of (D - S).        *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn subs(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SUBS D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// SUBSX D, {#}S {WC/WZ/WCZ}
/// EEEE 0001111 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Subtract (S + C) from D, signed and extended.  D = D - (S + C).  C = correct sign of (D - (S + C)).  Z = Z AND (result == 0).
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn subsx(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SUBSX D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// CMP D, {#}S {WC/WZ/WCZ}
/// EEEE 0010000 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Compare D to S.                                                  C = borrow of (D - S).              Z = (D == S).
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn cmp(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CMP D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// CMPX D, {#}S {WC/WZ/WCZ}
/// EEEE 0010001 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Compare D to (S + C), extended.                                  C = borrow of (D - (S + C)).        Z = Z AND (D == S + C).
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn cmpx(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CMPX D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// CMPS D, {#}S {WC/WZ/WCZ}
/// EEEE 0010010 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Compare D to S, signed.                                          C = correct sign of (D - S).        Z = (D == S).
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn cmps(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CMPS D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// CMPSX D, {#}S {WC/WZ/WCZ}
/// EEEE 0010011 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Compare D to (S + C), signed and extended.                       C = correct sign of (D - (S + C)).  Z = Z AND (D == S + C).
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn cmpsx(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CMPSX D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// CMPR D, {#}S {WC/WZ/WCZ}
/// EEEE 0010100 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Compare S to D (reverse).                                        C = borrow of (S - D).              Z = (D == S).
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn cmpr(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CMPR D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// CMPM D, {#}S {WC/WZ/WCZ}
/// EEEE 0010101 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Compare D to S, get MSB of difference into C.                    C = MSB of (D - S).                 Z = (D == S).
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn cmpm(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CMPM D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// SUBR D, {#}S {WC/WZ/WCZ}
/// EEEE 0010110 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Subtract D from S (reverse).                   D = S - D.        C = borrow of (S - D).              *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn subr(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SUBR D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// CMPSUB D, {#}S {WC/WZ/WCZ}
/// EEEE 0010111 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Compare and subtract S from D if D >= S. If D => S then D = D - S and C = 1, else D same and C = 0.  *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn cmpsub(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CMPSUB D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// FGE D, {#}S {WC/WZ/WCZ}
/// EEEE 0011000 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Force D >= S. If D < S then D = S and C = 1, else D same and C = 0. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn fge(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("FGE D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// FLE D, {#}S {WC/WZ/WCZ}
/// EEEE 0011001 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Force D <= S. If D > S then D = S and C = 1, else D same and C = 0. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn fle(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("FLE D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// FGES D, {#}S {WC/WZ/WCZ}
/// EEEE 0011010 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Force D >= S, signed. If D < S then D = S and C = 1, else D same and C = 0. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn fges(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("FGES D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// FLES D, {#}S {WC/WZ/WCZ}
/// EEEE 0011011 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Force D <= S, signed. If D > S then D = S and C = 1, else D same and C = 0. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn fles(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("FLES D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// SUMC D, {#}S {WC/WZ/WCZ}
/// EEEE 0011100 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Sum +/-S into D by  C. If C = 1 then D = D - S, else D = D + S. C = correct sign of (D +/- S). *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn sumc(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SUMC D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// SUMNC D, {#}S {WC/WZ/WCZ}
/// EEEE 0011101 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Sum +/-S into D by !C. If C = 0 then D = D - S, else D = D + S. C = correct sign of (D +/- S). *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn sumnc(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SUMNC D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// SUMZ D, {#}S {WC/WZ/WCZ}
/// EEEE 0011110 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Sum +/-S into D by  Z. If Z = 1 then D = D - S, else D = D + S. C = correct sign of (D +/- S). *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn sumz(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SUMZ D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// SUMNZ D, {#}S {WC/WZ/WCZ}
/// EEEE 0011111 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Sum +/-S into D by !Z. If Z = 0 then D = D - S, else D = D + S. C = correct sign of (D +/- S). *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn sumnz(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SUMNZ D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// TESTB D, {#}S WC/WZ
/// EEEE 0100000 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Test bit S[4:0] of  D, write to C/Z. C/Z =          D[S[4:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn testb(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTB D, {#}S WC/WZ is not implemented yet!");
    // return .next;
}

/// TESTBN D, {#}S WC/WZ
/// EEEE 0100001 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Test bit S[4:0] of !D, write to C/Z. C/Z =         !D[S[4:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn testbn(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTBN D, {#}S WC/WZ is not implemented yet!");
    // return .next;
}

/// TESTB D, {#}S ANDC/ANDZ
/// EEEE 0100010 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Test bit S[4:0] of  D, AND into C/Z. C/Z = C/Z AND  D[S[4:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn testb_and(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTB D, {#}S ANDC/ANDZ is not implemented yet!");
    // return .next;
}

/// TESTBN D, {#}S ANDC/ANDZ
/// EEEE 0100011 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Test bit S[4:0] of !D, AND into C/Z. C/Z = C/Z AND !D[S[4:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn testbn_and(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTBN D, {#}S ANDC/ANDZ is not implemented yet!");
    // return .next;
}

/// TESTB D, {#}S ORC/ORZ
/// EEEE 0100100 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Test bit S[4:0] of  D, OR  into C/Z. C/Z = C/Z OR   D[S[4:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn testb_or(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTB D, {#}S ORC/ORZ is not implemented yet!");
    // return .next;
}

/// TESTBN D, {#}S ORC/ORZ
/// EEEE 0100101 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Test bit S[4:0] of !D, OR  into C/Z. C/Z = C/Z OR  !D[S[4:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn testbn_or(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTBN D, {#}S ORC/ORZ is not implemented yet!");
    // return .next;
}

/// TESTB D, {#}S XORC/XORZ
/// EEEE 0100110 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Test bit S[4:0] of  D, XOR into C/Z. C/Z = C/Z XOR  D[S[4:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn testb_xor(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTB D, {#}S XORC/XORZ is not implemented yet!");
    // return .next;
}

/// TESTBN D, {#}S XORC/XORZ
/// EEEE 0100111 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Test bit S[4:0] of !D, XOR into C/Z. C/Z = C/Z XOR !D[S[4:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn testbn_xor(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTBN D, {#}S XORC/XORZ is not implemented yet!");
    // return .next;
}

/// BITL D, {#}S {WCZ}
/// EEEE 0100000 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Bits D[S[9:5]+S[4:0]:S[4:0]] = 0.    Other bits unaffected. Prior SETQ overrides S[9:5]. C,Z = original D[S[4:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn bitl(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("BITL D, {#}S {WCZ} is not implemented yet!");
    // return .next;
}

/// BITH D, {#}S {WCZ}
/// EEEE 0100001 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Bits D[S[9:5]+S[4:0]:S[4:0]] = 1.    Other bits unaffected. Prior SETQ overrides S[9:5]. C,Z = original D[S[4:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn bith(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("BITH D, {#}S {WCZ} is not implemented yet!");
    // return .next;
}

/// BITC D, {#}S {WCZ}
/// EEEE 0100010 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Bits D[S[9:5]+S[4:0]:S[4:0]] = C.    Other bits unaffected. Prior SETQ overrides S[9:5]. C,Z = original D[S[4:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn bitc(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("BITC D, {#}S {WCZ} is not implemented yet!");
    // return .next;
}

/// BITNC D, {#}S {WCZ}
/// EEEE 0100011 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Bits D[S[9:5]+S[4:0]:S[4:0]] = !C.   Other bits unaffected. Prior SETQ overrides S[9:5]. C,Z = original D[S[4:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn bitnc(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("BITNC D, {#}S {WCZ} is not implemented yet!");
    // return .next;
}

/// BITZ D, {#}S {WCZ}
/// EEEE 0100100 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Bits D[S[9:5]+S[4:0]:S[4:0]] = Z.    Other bits unaffected. Prior SETQ overrides S[9:5]. C,Z = original D[S[4:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn bitz(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("BITZ D, {#}S {WCZ} is not implemented yet!");
    // return .next;
}

/// BITNZ D, {#}S {WCZ}
/// EEEE 0100101 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Bits D[S[9:5]+S[4:0]:S[4:0]] = !Z.   Other bits unaffected. Prior SETQ overrides S[9:5]. C,Z = original D[S[4:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn bitnz(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("BITNZ D, {#}S {WCZ} is not implemented yet!");
    // return .next;
}

/// BITRND D, {#}S {WCZ}
/// EEEE 0100110 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Bits D[S[9:5]+S[4:0]:S[4:0]] = RNDs. Other bits unaffected. Prior SETQ overrides S[9:5]. C,Z = original D[S[4:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn bitrnd(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("BITRND D, {#}S {WCZ} is not implemented yet!");
    // return .next;
}

/// BITNOT D, {#}S {WCZ}
/// EEEE 0100111 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Toggle bits D[S[9:5]+S[4:0]:S[4:0]]. Other bits unaffected. Prior SETQ overrides S[9:5]. C,Z = original D[S[4:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn bitnot(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("BITNOT D, {#}S {WCZ} is not implemented yet!");
    // return .next;
}

/// AND D, {#}S {WC/WZ/WCZ}
/// EEEE 0101000 CZI DDDDDDDDD SSSSSSSSS
///
/// description: AND S into D.    D = D & S.    C = parity of result. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn @"and"(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("AND D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// ANDN D, {#}S {WC/WZ/WCZ}
/// EEEE 0101001 CZI DDDDDDDDD SSSSSSSSS
///
/// description: AND !S into D.   D = D & !S.   C = parity of result. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn andn(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ANDN D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// OR D, {#}S {WC/WZ/WCZ}
/// EEEE 0101010 CZI DDDDDDDDD SSSSSSSSS
///
/// description: OR S into D.     D = D | S.    C = parity of result. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn @"or"(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("OR D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// XOR D, {#}S {WC/WZ/WCZ}
/// EEEE 0101011 CZI DDDDDDDDD SSSSSSSSS
///
/// description: XOR S into D.    D = D ^ S.    C = parity of result. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn xor(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("XOR D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// MUXC D, {#}S {WC/WZ/WCZ}
/// EEEE 0101100 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Mux  C into each D bit that is '1' in S. D = (!S & D ) | (S & {32{ C}}). C = parity of result. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn muxc(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MUXC D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// MUXNC D, {#}S {WC/WZ/WCZ}
/// EEEE 0101101 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Mux !C into each D bit that is '1' in S. D = (!S & D ) | (S & {32{!C}}). C = parity of result. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn muxnc(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MUXNC D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// MUXZ D, {#}S {WC/WZ/WCZ}
/// EEEE 0101110 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Mux  Z into each D bit that is '1' in S. D = (!S & D ) | (S & {32{ Z}}). C = parity of result. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn muxz(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MUXZ D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// MUXNZ D, {#}S {WC/WZ/WCZ}
/// EEEE 0101111 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Mux !Z into each D bit that is '1' in S. D = (!S & D ) | (S & {32{!Z}}). C = parity of result. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn muxnz(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MUXNZ D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// MOV D, {#}S {WC/WZ/WCZ}
/// EEEE 0110000 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Move S into D. D = S. C = S[31]. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn mov(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MOV D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// NOT D, {#}S {WC/WZ/WCZ}
/// EEEE 0110001 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Get !S into D. D = !S. C = !S[31]. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn not(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("NOT D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// ABS D, {#}S {WC/WZ/WCZ}
/// EEEE 0110010 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Get absolute value of S into D. D = ABS(S). C = S[31]. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn abs(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ABS D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// NEG D, {#}S {WC/WZ/WCZ}
/// EEEE 0110011 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Negate S into D. D = -S. C = MSB of result. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn neg(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("NEG D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// NEGC D, {#}S {WC/WZ/WCZ}
/// EEEE 0110100 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Negate S by  C into D. If C = 1 then D = -S, else D = S. C = MSB of result. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn negc(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("NEGC D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// NEGNC D, {#}S {WC/WZ/WCZ}
/// EEEE 0110101 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Negate S by !C into D. If C = 0 then D = -S, else D = S. C = MSB of result. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn negnc(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("NEGNC D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// NEGZ D, {#}S {WC/WZ/WCZ}
/// EEEE 0110110 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Negate S by  Z into D. If Z = 1 then D = -S, else D = S. C = MSB of result. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn negz(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("NEGZ D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// NEGNZ D, {#}S {WC/WZ/WCZ}
/// EEEE 0110111 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Negate S by !Z into D. If Z = 0 then D = -S, else D = S. C = MSB of result. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn negnz(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("NEGNZ D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// INCMOD D, {#}S {WC/WZ/WCZ}
/// EEEE 0111000 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Increment with modulus. If D = S then D = 0 and C = 1, else D = D + 1 and C = 0. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn incmod(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("INCMOD D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// DECMOD D, {#}S {WC/WZ/WCZ}
/// EEEE 0111001 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Decrement with modulus. If D = 0 then D = S and C = 1, else D = D - 1 and C = 0. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn decmod(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DECMOD D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// ZEROX D, {#}S {WC/WZ/WCZ}
/// EEEE 0111010 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Zero-extend D above bit S[4:0]. C = MSB of result. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn zerox(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ZEROX D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// SIGNX D, {#}S {WC/WZ/WCZ}
/// EEEE 0111011 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Sign-extend D from bit S[4:0]. C = MSB of result. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn signx(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SIGNX D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// ENCOD D, {#}S {WC/WZ/WCZ}
/// EEEE 0111100 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Get bit position of top-most '1' in S into D. D = position of top '1' in S (0..31). C = (S != 0). *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn encod(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ENCOD D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// ONES D, {#}S {WC/WZ/WCZ}
/// EEEE 0111101 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Get number of '1's in S into D. D = number of '1's in S (0..32). C = LSB of result. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn ones(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ONES D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// TEST D, {#}S {WC/WZ/WCZ}
/// EEEE 0111110 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Test D with S. C = parity of (D & S). Z = ((D & S) == 0).
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn @"test"(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TEST D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// TESTN D, {#}S {WC/WZ/WCZ}
/// EEEE 0111111 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Test D with !S. C = parity of (D & !S). Z = ((D & !S) == 0).
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn testn(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTN D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// SETNIB D, {#}S, #N
/// EEEE 100000N NNI DDDDDDDDD SSSSSSSSS
///
/// description: Set S[3:0] into nibble N in D, keeping rest of D same.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn setnib(cog: *Cog, args: encoding.Both_D_Simm_N3) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETNIB D, {#}S, #N is not implemented yet!");
    // return .next;
}

/// GETNIB D, {#}S, #N
/// EEEE 100001N NNI DDDDDDDDD SSSSSSSSS
///
/// description: Get nibble N of S into D. D = {28'b0, S.NIBBLE[N]).
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn getnib(cog: *Cog, args: encoding.Both_D_Simm_N3) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("GETNIB D, {#}S, #N is not implemented yet!");
    // return .next;
}

/// ROLNIB D, {#}S, #N
/// EEEE 100010N NNI DDDDDDDDD SSSSSSSSS
///
/// description: Rotate-left nibble N of S into D. D = {D[27:0], S.NIBBLE[N]).
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn rolnib(cog: *Cog, args: encoding.Both_D_Simm_N3) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ROLNIB D, {#}S, #N is not implemented yet!");
    // return .next;
}

/// SETBYTE D, {#}S, #N
/// EEEE 1000110 NNI DDDDDDDDD SSSSSSSSS
///
/// description: Set S[7:0] into byte N in D, keeping rest of D same.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn setbyte(cog: *Cog, args: encoding.Both_D_Simm_N2) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETBYTE D, {#}S, #N is not implemented yet!");
    // return .next;
}

/// GETBYTE D, {#}S, #N
/// EEEE 1000111 NNI DDDDDDDDD SSSSSSSSS
///
/// description: Get byte N of S into D. D = {24'b0, S.BYTE[N]).
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn getbyte(cog: *Cog, args: encoding.Both_D_Simm_N2) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("GETBYTE D, {#}S, #N is not implemented yet!");
    // return .next;
}

/// ROLBYTE D, {#}S, #N
/// EEEE 1001000 NNI DDDDDDDDD SSSSSSSSS
///
/// description: Rotate-left byte N of S into D. D = {D[23:0], S.BYTE[N]).
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn rolbyte(cog: *Cog, args: encoding.Both_D_Simm_N2) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ROLBYTE D, {#}S, #N is not implemented yet!");
    // return .next;
}

/// SETWORD D, {#}S, #N
/// EEEE 1001001 0NI DDDDDDDDD SSSSSSSSS
///
/// description: Set S[15:0] into word N in D, keeping rest of D same.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn setword(cog: *Cog, args: encoding.Both_D_Simm_N1) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETWORD D, {#}S, #N is not implemented yet!");
    // return .next;
}

/// GETWORD D, {#}S, #N
/// EEEE 1001001 1NI DDDDDDDDD SSSSSSSSS
///
/// description: Get word N of S into D. D = {16'b0, S.WORD[N]).
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn getword(cog: *Cog, args: encoding.Both_D_Simm_N1) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("GETWORD D, {#}S, #N is not implemented yet!");
    // return .next;
}

/// ROLWORD D, {#}S, #N
/// EEEE 1001010 0NI DDDDDDDDD SSSSSSSSS
///
/// description: Rotate-left word N of S into D. D = {D[15:0], S.WORD[N]).
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn rolword(cog: *Cog, args: encoding.Both_D_Simm_N1) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ROLWORD D, {#}S, #N is not implemented yet!");
    // return .next;
}

/// SETR D, {#}S
/// EEEE 1001101 01I DDDDDDDDD SSSSSSSSS
///
/// description: Set R field of D to S[8:0]. D = {D[31:28], S[8:0], D[18:0]}.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn setr(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETR D, {#}S is not implemented yet!");
    // return .next;
}

/// SETD D, {#}S
/// EEEE 1001101 10I DDDDDDDDD SSSSSSSSS
///
/// description: Set D field of D to S[8:0]. D = {D[31:18], S[8:0], D[8:0]}.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn setd(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETD D, {#}S is not implemented yet!");
    // return .next;
}

/// SETS D, {#}S
/// EEEE 1001101 11I DDDDDDDDD SSSSSSSSS
///
/// description: Set S field of D to S[8:0]. D = {D[31:9], S[8:0]}.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn sets(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETS D, {#}S is not implemented yet!");
    // return .next;
}

/// DECOD D, {#}S
/// EEEE 1001110 00I DDDDDDDDD SSSSSSSSS
///
/// description: Decode S[4:0] into D. D = 1 << S[4:0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn decod(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DECOD D, {#}S is not implemented yet!");
    // return .next;
}

/// BMASK D, {#}S
/// EEEE 1001110 01I DDDDDDDDD SSSSSSSSS
///
/// description: Get LSB-justified bit mask of size (S[4:0] + 1) into D. D = ($0_0000_0002 << S[4:0]) - 1.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn bmask(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("BMASK D, {#}S is not implemented yet!");
    // return .next;
}

/// CRCBIT D, {#}S
/// EEEE 1001110 10I DDDDDDDDD SSSSSSSSS
///
/// description: Iterate CRC value in D using C and polynomial in S. If (C XOR D[0]) then D = (D >> 1) XOR S, else D = (D >> 1).
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn crcbit(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CRCBIT D, {#}S is not implemented yet!");
    // return .next;
}

/// CRCNIB D, {#}S
/// EEEE 1001110 11I DDDDDDDDD SSSSSSSSS
///
/// description: Iterate CRC value in D using Q[31:28] and polynomial in S. Like CRCBIT x 4. Q = Q << 4. For long, use SETQ+'REP #1,#8'+CRCNIB.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn crcnib(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CRCNIB D, {#}S is not implemented yet!");
    // return .next;
}

/// MUXNITS D, {#}S
/// EEEE 1001111 00I DDDDDDDDD SSSSSSSSS
///
/// description: For each non-zero bit pair in S, copy that bit pair into the corresponding D bits, else leave that D bit pair the same.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn muxnits(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MUXNITS D, {#}S is not implemented yet!");
    // return .next;
}

/// MUXNIBS D, {#}S
/// EEEE 1001111 01I DDDDDDDDD SSSSSSSSS
///
/// description: For each non-zero nibble in S, copy that nibble into the corresponding D nibble, else leave that D nibble the same.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn muxnibs(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MUXNIBS D, {#}S is not implemented yet!");
    // return .next;
}

/// MUXQ D, {#}S
/// EEEE 1001111 10I DDDDDDDDD SSSSSSSSS
///
/// description: Used after SETQ. For each '1' bit in Q, copy the corresponding bit in S into D. D = (D & !Q) | (S & Q).
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn muxq(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MUXQ D, {#}S is not implemented yet!");
    // return .next;
}

/// MOVBYTS D, {#}S
/// EEEE 1001111 11I DDDDDDDDD SSSSSSSSS
///
/// description: Move bytes within D, per S. D = {D.BYTE[S[7:6]], D.BYTE[S[5:4]], D.BYTE[S[3:2]], D.BYTE[S[1:0]]}.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn movbyts(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MOVBYTS D, {#}S is not implemented yet!");
    // return .next;
}

/// MUL D, {#}S {WZ}
/// EEEE 1010000 0ZI DDDDDDDDD SSSSSSSSS
///
/// description: D = unsigned (D[15:0] * S[15:0]). Z = (S == 0) | (D == 0).
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn mul(cog: *Cog, args: encoding.Both_D_Simm_ZFlag) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MUL D, {#}S {WZ} is not implemented yet!");
    // return .next;
}

/// MULS D, {#}S {WZ}
/// EEEE 1010000 1ZI DDDDDDDDD SSSSSSSSS
///
/// description: D = signed (D[15:0] * S[15:0]).   Z = (S == 0) | (D == 0).
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn muls(cog: *Cog, args: encoding.Both_D_Simm_ZFlag) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MULS D, {#}S {WZ} is not implemented yet!");
    // return .next;
}

/// SCA D, {#}S {WZ}
/// EEEE 1010001 0ZI DDDDDDDDD SSSSSSSSS
///
/// description: Next instruction's S value = unsigned (D[15:0] * S[15:0]) >> 16. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn sca(cog: *Cog, args: encoding.Both_D_Simm_ZFlag) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SCA D, {#}S {WZ} is not implemented yet!");
    // return .next;
}

/// SCAS D, {#}S {WZ}
/// EEEE 1010001 1ZI DDDDDDDDD SSSSSSSSS
///
/// description: Next instruction's S value = signed (D[15:0] * S[15:0]) >> 14. In this scheme, $4000 = 1.0 and $C000 = -1.0. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn scas(cog: *Cog, args: encoding.Both_D_Simm_ZFlag) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SCAS D, {#}S {WZ} is not implemented yet!");
    // return .next;
}

/// SPLITB D
/// EEEE 1101011 000 DDDDDDDDD 001100000
///
/// description: Split every 4th bit of D into bytes. D = {D[31], D[27], D[23], D[19], ...D[12], D[8], D[4], D[0]}.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn splitb(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SPLITB D is not implemented yet!");
    // return .next;
}

/// MERGEB D
/// EEEE 1101011 000 DDDDDDDDD 001100001
///
/// description: Merge bits of bytes in D. D = {D[31], D[23], D[15], D[7], ...D[24], D[16], D[8], D[0]}.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn mergeb(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MERGEB D is not implemented yet!");
    // return .next;
}

/// SPLITW D
/// EEEE 1101011 000 DDDDDDDDD 001100010
///
/// description: Split odd/even bits of D into words. D = {D[31], D[29], D[27], D[25], ...D[6], D[4], D[2], D[0]}.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn splitw(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SPLITW D is not implemented yet!");
    // return .next;
}

/// MERGEW D
/// EEEE 1101011 000 DDDDDDDDD 001100011
///
/// description: Merge bits of words in D. D = {D[31], D[15], D[30], D[14], ...D[17], D[1], D[16], D[0]}.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn mergew(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MERGEW D is not implemented yet!");
    // return .next;
}

/// SEUSSF D
/// EEEE 1101011 000 DDDDDDDDD 001100100
///
/// description: Relocate and periodically invert bits within D. Returns to original value on 32nd iteration. Forward pattern.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn seussf(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SEUSSF D is not implemented yet!");
    // return .next;
}

/// SEUSSR D
/// EEEE 1101011 000 DDDDDDDDD 001100101
///
/// description: Relocate and periodically invert bits within D. Returns to original value on 32nd iteration. Reverse pattern.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn seussr(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SEUSSR D is not implemented yet!");
    // return .next;
}

/// RGBSQZ D
/// EEEE 1101011 000 DDDDDDDDD 001100110
///
/// description: Squeeze 8:8:8 RGB value in D[31:8] into 5:6:5 value in D[15:0]. D = {15'b0, D[31:27], D[23:18], D[15:11]}.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn rgbsqz(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RGBSQZ D is not implemented yet!");
    // return .next;
}

/// RGBEXP D
/// EEEE 1101011 000 DDDDDDDDD 001100111
///
/// description: Expand 5:6:5 RGB value in D[15:0] into 8:8:8 value in D[31:8]. D = {D[15:11,15:13], D[10:5,10:9], D[4:0,4:2], 8'b0}.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn rgbexp(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RGBEXP D is not implemented yet!");
    // return .next;
}

/// XORO32 D
/// EEEE 1101011 000 DDDDDDDDD 001101000
///
/// description: Iterate D with xoroshiro32+ PRNG algorithm and put PRNG result into next instruction's S. D must be non-zero to iterate.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn xoro32(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("XORO32 D is not implemented yet!");
    // return .next;
}

/// REV D
/// EEEE 1101011 000 DDDDDDDDD 001101001
///
/// description: Reverse D bits. D = D[0:31].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn rev(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("REV D is not implemented yet!");
    // return .next;
}

/// RCZR D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 001101010
///
/// description: Rotate C,Z right through D. D = {C, Z, D[31:2]}. C = D[1],  Z = D[0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn rczr(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RCZR D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// RCZL D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 001101011
///
/// description: Rotate C,Z left through D.  D = {D[29:0], C, Z}. C = D[31], Z = D[30].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn rczl(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RCZL D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WRC D
/// EEEE 1101011 000 DDDDDDDDD 001101100
///
/// description: Write 0 or 1 to D, according to  C. D = {31'b0,  C}.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn wrc(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WRC D is not implemented yet!");
    // return .next;
}

/// WRNC D
/// EEEE 1101011 000 DDDDDDDDD 001101101
///
/// description: Write 0 or 1 to D, according to !C. D = {31'b0, !C}.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn wrnc(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WRNC D is not implemented yet!");
    // return .next;
}

/// WRZ D
/// EEEE 1101011 000 DDDDDDDDD 001101110
///
/// description: Write 0 or 1 to D, according to  Z. D = {31'b0,  Z}.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn wrz(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WRZ D is not implemented yet!");
    // return .next;
}

/// WRNZ D
/// EEEE 1101011 000 DDDDDDDDD 001101111
///
/// description: Write 0 or 1 to D, according to !Z. D = {31'b0, !Z}.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn wrnz(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WRNZ D is not implemented yet!");
    // return .next;
}

/// MODCZ c, z {WC/WZ/WCZ}
/// EEEE 1101011 CZ1 0cccczzzz 001101111
///
/// description: Modify C and Z according to cccc and zzzz. C = cccc[{C,Z}], Z = zzzz[{C,Z}]. See "MODCZ Operand" list.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn modcz(cog: *Cog, args: encoding.UpdateFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MODCZ c, z {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// LOC PA/PB/PTRA/PTRB, #{\}A
/// EEEE 11101WW RAA AAAAAAAAA AAAAAAAAA
///
/// description: Get {12'b0, address[19:0]} into PA/PB/PTRA/PTRB (per W).          If R = 1, address = PC + A, else address = A. "\" forces R = 0.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=Per W, stack=None
pub fn loc(cog: *Cog, args: encoding.LocStyle) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("LOC PA/PB/PTRA/PTRB, #{\\}A is not implemented yet!");
    // return .next;
}

//
// GROUP: Miscellaneous
//

/// NOP
/// 0000 0000000 000 000000000 000000000
///
/// description: No operation.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn nop(cog: *Cog, args: encoding.Nop) Cog.ExecResult {
    _ = cog;
    _ = args;
    return .next;
}

/// GETCT D {WC}
/// EEEE 1101011 C00 DDDDDDDDD 000011010
///
/// description: Get CT[31:0] or CT[63:32] if WC into D. GETCT WC + GETCT captures entire CT. CT=0 on reset, CT++ on every clock. C = same.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn getct(cog: *Cog, args: encoding.Only_D_CFlag) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("GETCT D {WC} is not implemented yet!");
    // return .next;
}

/// GETRND D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000011011
///
/// description: Get RND into D/C/Z. RND is the PRNG that updates on every clock. D = RND[31:0], C = RND[31], Z = RND[30], unique per cog.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn getrnd(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("GETRND D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WAITX {#}D {WC/WZ/WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 000011111
///
/// description: Wait 2 + D clocks if no WC/WZ/WCZ. If WC/WZ/WCZ, wait 2 + (D & RND) clocks. C/Z = 0.
/// cog timing:  2 + D
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn waitx(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITX {#}D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// SETQ {#}D
/// EEEE 1101011 00L DDDDDDDDD 000101000
///
/// description: Set Q to D. Use before RDLONG/WRLONG/WMLONG to set block transfer. Also used before MUXQ/COGINIT/QDIV/QFRAC/QROTATE/WAITxxx.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setq(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETQ {#}D is not implemented yet!");
    // return .next;
}

/// SETQ2 {#}D
/// EEEE 1101011 00L DDDDDDDDD 000101001
///
/// description: Set Q to D. Use before RDLONG/WRLONG/WMLONG to set LUT block transfer.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setq2(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETQ2 {#}D is not implemented yet!");
    // return .next;
}

/// PUSH {#}D
/// EEEE 1101011 00L DDDDDDDDD 000101010
///
/// description: Push D onto stack.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=Push
pub fn push(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("PUSH {#}D is not implemented yet!");
    // return .next;
}

/// POP D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000101011
///
/// description: Pop stack (K). D = K. C = K[31]. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=Pop
pub fn pop(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POP D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// AUGS #n
/// EEEE 11110nn nnn nnnnnnnnn nnnnnnnnn
///
/// description: Queue #n to be used as upper 23 bits for next #S occurrence, so that the next 9-bit #S will be augmented to 32 bits.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn augs(cog: *Cog, args: encoding.Augment) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("AUGS #n is not implemented yet!");
    // return .next;
}

/// AUGD #n
/// EEEE 11111nn nnn nnnnnnnnn nnnnnnnnn
///
/// description: Queue #n to be used as upper 23 bits for next #D occurrence, so that the next 9-bit #D will be augmented to 32 bits.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn augd(cog: *Cog, args: encoding.Augment) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("AUGD #n is not implemented yet!");
    // return .next;
}

//
// GROUP: Pins
//

/// TESTP {#}D WC/WZ
/// EEEE 1101011 CZL DDDDDDDDD 001000000
///
/// description: Test  IN bit of pin D[5:0], write to C/Z. C/Z =          IN[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn testp(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTP {#}D WC/WZ is not implemented yet!");
    // return .next;
}

/// TESTPN {#}D WC/WZ
/// EEEE 1101011 CZL DDDDDDDDD 001000001
///
/// description: Test !IN bit of pin D[5:0], write to C/Z. C/Z =         !IN[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn testpn(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTPN {#}D WC/WZ is not implemented yet!");
    // return .next;
}

/// TESTP {#}D ANDC/ANDZ
/// EEEE 1101011 CZL DDDDDDDDD 001000010
///
/// description: Test  IN bit of pin D[5:0], AND into C/Z. C/Z = C/Z AND  IN[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn testp_and(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTP {#}D ANDC/ANDZ is not implemented yet!");
    // return .next;
}

/// TESTPN {#}D ANDC/ANDZ
/// EEEE 1101011 CZL DDDDDDDDD 001000011
///
/// description: Test !IN bit of pin D[5:0], AND into C/Z. C/Z = C/Z AND !IN[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn testpn_and(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTPN {#}D ANDC/ANDZ is not implemented yet!");
    // return .next;
}

/// TESTP {#}D ORC/ORZ
/// EEEE 1101011 CZL DDDDDDDDD 001000100
///
/// description: Test  IN bit of pin D[5:0], OR  into C/Z. C/Z = C/Z OR   IN[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn testp_or(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTP {#}D ORC/ORZ is not implemented yet!");
    // return .next;
}

/// TESTPN {#}D ORC/ORZ
/// EEEE 1101011 CZL DDDDDDDDD 001000101
///
/// description: Test !IN bit of pin D[5:0], OR  into C/Z. C/Z = C/Z OR  !IN[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn testpn_or(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTPN {#}D ORC/ORZ is not implemented yet!");
    // return .next;
}

/// TESTP {#}D XORC/XORZ
/// EEEE 1101011 CZL DDDDDDDDD 001000110
///
/// description: Test  IN bit of pin D[5:0], XOR into C/Z. C/Z = C/Z XOR  IN[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn testp_xor(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTP {#}D XORC/XORZ is not implemented yet!");
    // return .next;
}

/// TESTPN {#}D XORC/XORZ
/// EEEE 1101011 CZL DDDDDDDDD 001000111
///
/// description: Test !IN bit of pin D[5:0], XOR into C/Z. C/Z = C/Z XOR !IN[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn testpn_xor(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTPN {#}D XORC/XORZ is not implemented yet!");
    // return .next;
}

/// DIRL {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001000000
///
/// description: DIR bits of pins D[10:6]+D[5:0]..D[5:0] = 0.                  Wraps within DIRA/DIRB. Prior SETQ overrides D[10:6]. C,Z = DIR[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx, stack=None
pub fn dirl(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DIRL {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// DIRH {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001000001
///
/// description: DIR bits of pins D[10:6]+D[5:0]..D[5:0] = 1.                  Wraps within DIRA/DIRB. Prior SETQ overrides D[10:6]. C,Z = DIR[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx, stack=None
pub fn dirh(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DIRH {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// DIRC {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001000010
///
/// description: DIR bits of pins D[10:6]+D[5:0]..D[5:0] = C.                  Wraps within DIRA/DIRB. Prior SETQ overrides D[10:6]. C,Z = DIR[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx, stack=None
pub fn dirc(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DIRC {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// DIRNC {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001000011
///
/// description: DIR bits of pins D[10:6]+D[5:0]..D[5:0] = !C.                 Wraps within DIRA/DIRB. Prior SETQ overrides D[10:6]. C,Z = DIR[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx, stack=None
pub fn dirnc(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DIRNC {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// DIRZ {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001000100
///
/// description: DIR bits of pins D[10:6]+D[5:0]..D[5:0] = Z.                  Wraps within DIRA/DIRB. Prior SETQ overrides D[10:6]. C,Z = DIR[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx, stack=None
pub fn dirz(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DIRZ {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// DIRNZ {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001000101
///
/// description: DIR bits of pins D[10:6]+D[5:0]..D[5:0] = !Z.                 Wraps within DIRA/DIRB. Prior SETQ overrides D[10:6]. C,Z = DIR[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx, stack=None
pub fn dirnz(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DIRNZ {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// DIRRND {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001000110
///
/// description: DIR bits of pins D[10:6]+D[5:0]..D[5:0] = RNDs.               Wraps within DIRA/DIRB. Prior SETQ overrides D[10:6]. C,Z = DIR[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx, stack=None
pub fn dirrnd(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DIRRND {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// DIRNOT {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001000111
///
/// description: Toggle DIR bits of pins D[10:6]+D[5:0]..D[5:0].               Wraps within DIRA/DIRB. Prior SETQ overrides D[10:6]. C,Z = DIR[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx, stack=None
pub fn dirnot(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DIRNOT {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// OUTL {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001001000
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = 0.                  Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=OUTx, stack=None
pub fn outl(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("OUTL {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// OUTH {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001001001
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = 1.                  Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=OUTx, stack=None
pub fn outh(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("OUTH {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// OUTC {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001001010
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = C.                  Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=OUTx, stack=None
pub fn outc(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("OUTC {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// OUTNC {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001001011
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = !C.                 Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=OUTx, stack=None
pub fn outnc(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("OUTNC {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// OUTZ {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001001100
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = Z.                  Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=OUTx, stack=None
pub fn outz(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("OUTZ {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// OUTNZ {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001001101
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = !Z.                 Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=OUTx, stack=None
pub fn outnz(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("OUTNZ {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// OUTRND {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001001110
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = RNDs.               Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=OUTx, stack=None
pub fn outrnd(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("OUTRND {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// OUTNOT {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001001111
///
/// description: Toggle OUT bits of pins D[10:6]+D[5:0]..D[5:0].               Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=OUTx, stack=None
pub fn outnot(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("OUTNOT {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// FLTL {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001010000
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = 0.    DIR bits = 0. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn fltl(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("FLTL {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// FLTH {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001010001
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = 1.    DIR bits = 0. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn flth(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("FLTH {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// FLTC {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001010010
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = C.    DIR bits = 0. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn fltc(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("FLTC {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// FLTNC {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001010011
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = !C.   DIR bits = 0. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn fltnc(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("FLTNC {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// FLTZ {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001010100
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = Z.    DIR bits = 0. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn fltz(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("FLTZ {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// FLTNZ {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001010101
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = !Z.   DIR bits = 0. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn fltnz(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("FLTNZ {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// FLTRND {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001010110
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = RNDs. DIR bits = 0. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn fltrnd(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("FLTRND {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// FLTNOT {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001010111
///
/// description: Toggle OUT bits of pins D[10:6]+D[5:0]..D[5:0]. DIR bits = 0. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn fltnot(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("FLTNOT {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// DRVL {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001011000
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = 0.    DIR bits = 1. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn drvl(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DRVL {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// DRVH {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001011001
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = 1.    DIR bits = 1. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn drvh(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DRVH {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// DRVC {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001011010
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = C.    DIR bits = 1. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn drvc(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DRVC {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// DRVNC {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001011011
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = !C.   DIR bits = 1. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn drvnc(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DRVNC {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// DRVZ {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001011100
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = Z.    DIR bits = 1. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn drvz(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DRVZ {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// DRVNZ {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001011101
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = !Z.   DIR bits = 1. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn drvnz(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DRVNZ {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// DRVRND {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001011110
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = RNDs. DIR bits = 1. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn drvrnd(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DRVRND {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// DRVNOT {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001011111
///
/// description: Toggle OUT bits of pins D[10:6]+D[5:0]..D[5:0]. DIR bits = 1. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn drvnot(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DRVNOT {#}D {WCZ} is not implemented yet!");
    // return .next;
}

//
// GROUP: Pixel Mixer
//

/// ADDPIX D, {#}S
/// EEEE 1010010 00I DDDDDDDDD SSSSSSSSS
///
/// description: Add bytes of S into bytes of D, with $FF saturation.
/// cog timing:  7
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn addpix(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ADDPIX D, {#}S is not implemented yet!");
    // return .next;
}

/// MULPIX D, {#}S
/// EEEE 1010010 01I DDDDDDDDD SSSSSSSSS
///
/// description: Multiply bytes of S into bytes of D, where $FF = 1.0 and $00 = 0.0.
/// cog timing:  7
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn mulpix(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MULPIX D, {#}S is not implemented yet!");
    // return .next;
}

/// BLNPIX D, {#}S
/// EEEE 1010010 10I DDDDDDDDD SSSSSSSSS
///
/// description: Alpha-blend bytes of S into bytes of D, using SETPIV value.
/// cog timing:  7
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn blnpix(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("BLNPIX D, {#}S is not implemented yet!");
    // return .next;
}

/// MIXPIX D, {#}S
/// EEEE 1010010 11I DDDDDDDDD SSSSSSSSS
///
/// description: Mix bytes of S into bytes of D, using SETPIX and SETPIV values.
/// cog timing:  7
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn mixpix(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MIXPIX D, {#}S is not implemented yet!");
    // return .next;
}

/// SETPIV {#}D
/// EEEE 1101011 00L DDDDDDDDD 000111101
///
/// description: Set BLNPIX/MIXPIX blend factor to D[7:0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setpiv(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETPIV {#}D is not implemented yet!");
    // return .next;
}

/// SETPIX {#}D
/// EEEE 1101011 00L DDDDDDDDD 000111110
///
/// description: Set MIXPIX mode to D[5:0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setpix(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETPIX {#}D is not implemented yet!");
    // return .next;
}

//
// GROUP: Register Indirection
//

/// ALTSN D, {#}S
/// EEEE 1001010 10I DDDDDDDDD SSSSSSSSS
///
/// description: Alter subsequent SETNIB instruction. Next D field = (D[11:3] + S) & $1FF, N field = D[2:0].          D += sign-extended S[17:9].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn altsn(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ALTSN D, {#}S is not implemented yet!");
    // return .next;
}

/// ALTGN D, {#}S
/// EEEE 1001010 11I DDDDDDDDD SSSSSSSSS
///
/// description: Alter subsequent GETNIB/ROLNIB instruction. Next S field = (D[11:3] + S) & $1FF, N field = D[2:0].   D += sign-extended S[17:9].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn altgn(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ALTGN D, {#}S is not implemented yet!");
    // return .next;
}

/// ALTSB D, {#}S
/// EEEE 1001011 00I DDDDDDDDD SSSSSSSSS
///
/// description: Alter subsequent SETBYTE instruction. Next D field = (D[10:2] + S) & $1FF, N field = D[1:0].         D += sign-extended S[17:9].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn altsb(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ALTSB D, {#}S is not implemented yet!");
    // return .next;
}

/// ALTGB D, {#}S
/// EEEE 1001011 01I DDDDDDDDD SSSSSSSSS
///
/// description: Alter subsequent GETBYTE/ROLBYTE instruction. Next S field = (D[10:2] + S) & $1FF, N field = D[1:0]. D += sign-extended S[17:9].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn altgb(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ALTGB D, {#}S is not implemented yet!");
    // return .next;
}

/// ALTSW D, {#}S
/// EEEE 1001011 10I DDDDDDDDD SSSSSSSSS
///
/// description: Alter subsequent SETWORD instruction. Next D field = (D[9:1] + S) & $1FF, N field = D[0].            D += sign-extended S[17:9].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn altsw(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ALTSW D, {#}S is not implemented yet!");
    // return .next;
}

/// ALTGW D, {#}S
/// EEEE 1001011 11I DDDDDDDDD SSSSSSSSS
///
/// description: Alter subsequent GETWORD/ROLWORD instruction. Next S field = ((D[9:1] + S) & $1FF), N field = D[0].  D += sign-extended S[17:9].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn altgw(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ALTGW D, {#}S is not implemented yet!");
    // return .next;
}

/// ALTR D, {#}S
/// EEEE 1001100 00I DDDDDDDDD SSSSSSSSS
///
/// description: Alter result register address (normally D field) of next instruction to (D + S) & $1FF.              D += sign-extended S[17:9].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn altr(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ALTR D, {#}S is not implemented yet!");
    // return .next;
}

/// ALTD D, {#}S
/// EEEE 1001100 01I DDDDDDDDD SSSSSSSSS
///
/// description: Alter D field of next instruction to (D + S) & $1FF.                                                 D += sign-extended S[17:9].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn altd(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ALTD D, {#}S is not implemented yet!");
    // return .next;
}

/// ALTS D, {#}S
/// EEEE 1001100 10I DDDDDDDDD SSSSSSSSS
///
/// description: Alter S field of next instruction to (D + S) & $1FF.                                                 D += sign-extended S[17:9].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn alts(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ALTS D, {#}S is not implemented yet!");
    // return .next;
}

/// ALTB D, {#}S
/// EEEE 1001100 11I DDDDDDDDD SSSSSSSSS
///
/// description: Alter D field of next instruction to (D[13:5] + S) & $1FF.                                           D += sign-extended S[17:9].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn altb(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ALTB D, {#}S is not implemented yet!");
    // return .next;
}

/// ALTI D, {#}S
/// EEEE 1001101 00I DDDDDDDDD SSSSSSSSS
///
/// description: Substitute next instruction's I/R/D/S fields with fields from D, per S. Modify D per S.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn alti(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ALTI D, {#}S is not implemented yet!");
    // return .next;
}

//
// GROUP: Smart Pins
//

/// RQPIN D, {#}S {WC}
/// EEEE 1010100 C0I DDDDDDDDD SSSSSSSSS
///
/// description: Read smart pin S[5:0] result "Z" into D, don't acknowledge pin ("Q" in RQPIN means "quiet"). C = modal result.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn rqpin(cog: *Cog, args: encoding.Both_D_Simm_CFlag) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RQPIN D, {#}S {WC} is not implemented yet!");
    // return .next;
}

/// RDPIN D, {#}S {WC}
/// EEEE 1010100 C1I DDDDDDDDD SSSSSSSSS
///
/// description: Read smart pin S[5:0] result "Z" into D, acknowledge pin.                                    C = modal result.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn rdpin(cog: *Cog, args: encoding.Both_D_Simm_CFlag) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RDPIN D, {#}S {WC} is not implemented yet!");
    // return .next;
}

/// WRPIN {#}D, {#}S
/// EEEE 1100000 0LI DDDDDDDDD SSSSSSSSS
///
/// description: Set mode of smart pins S[10:6]+S[5:0]..S[5:0] to D, acknowledge pins. Wraps within A/B pins. Prior SETQ D[4:0] overrides S[10:6].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn wrpin(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WRPIN {#}D, {#}S is not implemented yet!");
    // return .next;
}

/// WXPIN {#}D, {#}S
/// EEEE 1100000 1LI DDDDDDDDD SSSSSSSSS
///
/// description: Set "X"  of smart pins S[10:6]+S[5:0]..S[5:0] to D, acknowledge pins. Wraps within A/B pins. Prior SETQ D[4:0] overrides S[10:6].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn wxpin(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WXPIN {#}D, {#}S is not implemented yet!");
    // return .next;
}

/// WYPIN {#}D, {#}S
/// EEEE 1100001 0LI DDDDDDDDD SSSSSSSSS
///
/// description: Set "Y"  of smart pins S[10:6]+S[5:0]..S[5:0] to D, acknowledge pins. Wraps within A/B pins. Prior SETQ D[4:0] overrides S[10:6].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn wypin(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WYPIN {#}D, {#}S is not implemented yet!");
    // return .next;
}

/// SETDACS {#}D
/// EEEE 1101011 00L DDDDDDDDD 000011100
///
/// description: DAC3 = D[31:24], DAC2 = D[23:16], DAC1 = D[15:8], DAC0 = D[7:0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setdacs(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETDACS {#}D is not implemented yet!");
    // return .next;
}

/// SETSCP {#}D
/// EEEE 1101011 00L DDDDDDDDD 001110000
///
/// description: Set four-channel oscilloscope enable to D[6] and set input pin base to D[5:2].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setscp(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETSCP {#}D is not implemented yet!");
    // return .next;
}

/// GETSCP D
/// EEEE 1101011 000 DDDDDDDDD 001110001
///
/// description: Get four-channel oscilloscope samples into D. D = {ch3[7:0],ch2[7:0],ch1[7:0],ch0[7:0]}.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn getscp(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("GETSCP D is not implemented yet!");
    // return .next;
}

//
// GROUP: Streamer
//

/// XINIT {#}D, {#}S
/// EEEE 1100101 0LI DDDDDDDDD SSSSSSSSS
///
/// description: Issue streamer command immediately, zeroing phase. Prior SETQ sets NCO frequency.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn xinit(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("XINIT {#}D, {#}S is not implemented yet!");
    // return .next;
}

/// XZERO {#}D, {#}S
/// EEEE 1100101 1LI DDDDDDDDD SSSSSSSSS
///
/// description: Buffer new streamer command to be issued on final NCO rollover of current command, zeroing phase. Prior SETQ sets NCO frequency.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn xzero(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("XZERO {#}D, {#}S is not implemented yet!");
    // return .next;
}

/// XCONT {#}D, {#}S
/// EEEE 1100110 0LI DDDDDDDDD SSSSSSSSS
///
/// description: Buffer new streamer command to be issued on final NCO rollover of current command, continuing phase. Prior SETQ sets NCO frequency.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn xcont(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("XCONT {#}D, {#}S is not implemented yet!");
    // return .next;
}

/// SETXFRQ {#}D
/// EEEE 1101011 00L DDDDDDDDD 000011101
///
/// description: Set streamer NCO frequency to D.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setxfrq(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETXFRQ {#}D is not implemented yet!");
    // return .next;
}

/// GETXACC D
/// EEEE 1101011 000 DDDDDDDDD 000011110
///
/// description: Get the streamer's Goertzel X accumulator into D and the Y accumulator into the next instruction's S, clear accumulators.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn getxacc(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("GETXACC D is not implemented yet!");
    // return .next;
}
