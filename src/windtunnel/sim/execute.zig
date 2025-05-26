const std = @import("std");

const encoding = @import("encoding.zig");
const Cog = @import("Cog.zig");

/// ABS D, {#}S {WC/WZ/WCZ}
/// EEEE 0110010 CZI DDDDDDDDD SSSSSSSSS
pub fn abs(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ABS D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// ADD D, {#}S {WC/WZ/WCZ}
/// EEEE 0001000 CZI DDDDDDDDD SSSSSSSSS
pub fn add(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ADD D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// ADDCT1 D, {#}S
/// EEEE 1010011 00I DDDDDDDDD SSSSSSSSS
pub fn addct1(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ADDCT1 D, {#}S  is not implemented yet!");
    // return .next;
}

/// ADDCT2 D, {#}S
/// EEEE 1010011 01I DDDDDDDDD SSSSSSSSS
pub fn addct2(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ADDCT2 D, {#}S  is not implemented yet!");
    // return .next;
}

/// ADDCT3 D, {#}S
/// EEEE 1010011 10I DDDDDDDDD SSSSSSSSS
pub fn addct3(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ADDCT3 D, {#}S  is not implemented yet!");
    // return .next;
}

/// ADDPIX D, {#}S
/// EEEE 1010010 00I DDDDDDDDD SSSSSSSSS
pub fn addpix(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ADDPIX D, {#}S  is not implemented yet!");
    // return .next;
}

/// ADDS D, {#}S {WC/WZ/WCZ}
/// EEEE 0001010 CZI DDDDDDDDD SSSSSSSSS
pub fn adds(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ADDS D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// ADDSX D, {#}S {WC/WZ/WCZ}
/// EEEE 0001011 CZI DDDDDDDDD SSSSSSSSS
pub fn addsx(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ADDSX D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// ADDX D, {#}S {WC/WZ/WCZ}
/// EEEE 0001001 CZI DDDDDDDDD SSSSSSSSS
pub fn addx(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ADDX D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// AKPIN {#}S
/// EEEE 1100000 01I 000000001 SSSSSSSSS
pub fn akpin(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("AKPIN {#}S  is not implemented yet!");
    // return .next;
}

/// ALLOWI
/// EEEE 1101011 000 000100000 000100100
pub fn allowi(cog: *Cog, args: encoding.NoOperands) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ALLOWI  is not implemented yet!");
    // return .next;
}

/// ALTB D, {#}S
/// EEEE 1001100 11I DDDDDDDDD SSSSSSSSS
pub fn altb(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ALTB D, {#}S  is not implemented yet!");
    // return .next;
}

/// ALTD D, {#}S
/// EEEE 1001100 01I DDDDDDDDD SSSSSSSSS
pub fn altd(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ALTD D, {#}S  is not implemented yet!");
    // return .next;
}

/// ALTGB D, {#}S
/// EEEE 1001011 01I DDDDDDDDD SSSSSSSSS
pub fn altgb(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ALTGB D, {#}S  is not implemented yet!");
    // return .next;
}

/// ALTGN D, {#}S
/// EEEE 1001010 11I DDDDDDDDD SSSSSSSSS
pub fn altgn(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ALTGN D, {#}S  is not implemented yet!");
    // return .next;
}

/// ALTGW D, {#}S
/// EEEE 1001011 11I DDDDDDDDD SSSSSSSSS
pub fn altgw(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ALTGW D, {#}S  is not implemented yet!");
    // return .next;
}

/// ALTI D, {#}S
/// EEEE 1001101 00I DDDDDDDDD SSSSSSSSS
pub fn alti(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ALTI D, {#}S  is not implemented yet!");
    // return .next;
}

/// ALTR D, {#}S
/// EEEE 1001100 00I DDDDDDDDD SSSSSSSSS
pub fn altr(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ALTR D, {#}S  is not implemented yet!");
    // return .next;
}

/// ALTS D, {#}S
/// EEEE 1001100 10I DDDDDDDDD SSSSSSSSS
pub fn alts(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ALTS D, {#}S  is not implemented yet!");
    // return .next;
}

/// ALTSB D, {#}S
/// EEEE 1001011 00I DDDDDDDDD SSSSSSSSS
pub fn altsb(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ALTSB D, {#}S  is not implemented yet!");
    // return .next;
}

/// ALTSN D, {#}S
/// EEEE 1001010 10I DDDDDDDDD SSSSSSSSS
pub fn altsn(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ALTSN D, {#}S  is not implemented yet!");
    // return .next;
}

/// ALTSW D, {#}S
/// EEEE 1001011 10I DDDDDDDDD SSSSSSSSS
pub fn altsw(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ALTSW D, {#}S  is not implemented yet!");
    // return .next;
}

/// AND D, {#}S {WC/WZ/WCZ}
/// EEEE 0101000 CZI DDDDDDDDD SSSSSSSSS
pub fn @"and"(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("AND D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// ANDN D, {#}S {WC/WZ/WCZ}
/// EEEE 0101001 CZI DDDDDDDDD SSSSSSSSS
pub fn andn(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ANDN D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// AUGD #n
/// EEEE 11111nn nnn nnnnnnnnn nnnnnnnnn
pub fn augd(cog: *Cog, args: encoding.Augment) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("AUGD #n  is not implemented yet!");
    // return .next;
}

/// AUGS #n
/// EEEE 11110nn nnn nnnnnnnnn nnnnnnnnn
pub fn augs(cog: *Cog, args: encoding.Augment) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("AUGS #n  is not implemented yet!");
    // return .next;
}

/// BITC D, {#}S {WCZ}
/// EEEE 0100010 CZI DDDDDDDDD SSSSSSSSS
pub fn bitc(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("BITC D, {#}S {WCZ} is not implemented yet!");
    // return .next;
}

/// BITH D, {#}S {WCZ}
/// EEEE 0100001 CZI DDDDDDDDD SSSSSSSSS
pub fn bith(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("BITH D, {#}S {WCZ} is not implemented yet!");
    // return .next;
}

/// BITL D, {#}S {WCZ}
/// EEEE 0100000 CZI DDDDDDDDD SSSSSSSSS
pub fn bitl(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("BITL D, {#}S {WCZ} is not implemented yet!");
    // return .next;
}

/// BITNC D, {#}S {WCZ}
/// EEEE 0100011 CZI DDDDDDDDD SSSSSSSSS
pub fn bitnc(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("BITNC D, {#}S {WCZ} is not implemented yet!");
    // return .next;
}

/// BITNOT D, {#}S {WCZ}
/// EEEE 0100111 CZI DDDDDDDDD SSSSSSSSS
pub fn bitnot(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("BITNOT D, {#}S {WCZ} is not implemented yet!");
    // return .next;
}

/// BITNZ D, {#}S {WCZ}
/// EEEE 0100101 CZI DDDDDDDDD SSSSSSSSS
pub fn bitnz(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("BITNZ D, {#}S {WCZ} is not implemented yet!");
    // return .next;
}

/// BITRND D, {#}S {WCZ}
/// EEEE 0100110 CZI DDDDDDDDD SSSSSSSSS
pub fn bitrnd(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("BITRND D, {#}S {WCZ} is not implemented yet!");
    // return .next;
}

/// BITZ D, {#}S {WCZ}
/// EEEE 0100100 CZI DDDDDDDDD SSSSSSSSS
pub fn bitz(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("BITZ D, {#}S {WCZ} is not implemented yet!");
    // return .next;
}

/// BLNPIX D, {#}S
/// EEEE 1010010 10I DDDDDDDDD SSSSSSSSS
pub fn blnpix(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("BLNPIX D, {#}S  is not implemented yet!");
    // return .next;
}

/// BMASK D, {#}S
/// EEEE 1001110 01I DDDDDDDDD SSSSSSSSS
pub fn bmask(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("BMASK D, {#}S  is not implemented yet!");
    // return .next;
}

/// BRK {#}D
/// EEEE 1101011 00L DDDDDDDDD 000110110
pub fn brk(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("BRK {#}D  is not implemented yet!");
    // return .next;
}

/// CALL #{\}A
/// EEEE 1101101 RAA AAAAAAAAA AAAAAAAAA
pub fn call_a(cog: *Cog, args: encoding.AbsPointer) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CALL #{\\}A  is not implemented yet!");
    // return .next;
}

/// CALL D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000101101
pub fn call_d(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CALL D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// CALLA #{\}A
/// EEEE 1101110 RAA AAAAAAAAA AAAAAAAAA
pub fn calla_a(cog: *Cog, args: encoding.AbsPointer) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CALLA #{\\}A  is not implemented yet!");
    // return .next;
}

/// CALLA D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000101110
pub fn calla_d(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CALLA D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// CALLB #{\}A
/// EEEE 1101111 RAA AAAAAAAAA AAAAAAAAA
pub fn callb_a(cog: *Cog, args: encoding.AbsPointer) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CALLB #{\\}A  is not implemented yet!");
    // return .next;
}

/// CALLB D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000101111
pub fn callb_d(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CALLB D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// CALLD PA/PB/PTRA/PTRB, #{\}A
/// EEEE 11100WW RAA AAAAAAAAA AAAAAAAAA
pub fn calld_a(cog: *Cog, args: encoding.LocStyle) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CALLD PA/PB/PTRA/PTRB, #{\\}A  is not implemented yet!");
    // return .next;
}

/// CALLD D, {#}S** {WC/WZ/WCZ}
/// EEEE 1011001 CZI DDDDDDDDD SSSSSSSSS
pub fn calld_s(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CALLD D, {#}S** {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// CALLPA {#}D, {#}S**
/// EEEE 1011010 0LI DDDDDDDDD SSSSSSSSS
pub fn callpa(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CALLPA {#}D, {#}S**  is not implemented yet!");
    // return .next;
}

/// CALLPB {#}D, {#}S**
/// EEEE 1011010 1LI DDDDDDDDD SSSSSSSSS
pub fn callpb(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CALLPB {#}D, {#}S**  is not implemented yet!");
    // return .next;
}

/// CMP D, {#}S {WC/WZ/WCZ}
/// EEEE 0010000 CZI DDDDDDDDD SSSSSSSSS
pub fn cmp(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CMP D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// CMPM D, {#}S {WC/WZ/WCZ}
/// EEEE 0010101 CZI DDDDDDDDD SSSSSSSSS
pub fn cmpm(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CMPM D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// CMPR D, {#}S {WC/WZ/WCZ}
/// EEEE 0010100 CZI DDDDDDDDD SSSSSSSSS
pub fn cmpr(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CMPR D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// CMPS D, {#}S {WC/WZ/WCZ}
/// EEEE 0010010 CZI DDDDDDDDD SSSSSSSSS
pub fn cmps(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CMPS D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// CMPSUB D, {#}S {WC/WZ/WCZ}
/// EEEE 0010111 CZI DDDDDDDDD SSSSSSSSS
pub fn cmpsub(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CMPSUB D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// CMPSX D, {#}S {WC/WZ/WCZ}
/// EEEE 0010011 CZI DDDDDDDDD SSSSSSSSS
pub fn cmpsx(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CMPSX D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// CMPX D, {#}S {WC/WZ/WCZ}
/// EEEE 0010001 CZI DDDDDDDDD SSSSSSSSS
pub fn cmpx(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CMPX D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// COGATN {#}D
/// EEEE 1101011 00L DDDDDDDDD 000111111
pub fn cogatn(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("COGATN {#}D  is not implemented yet!");
    // return .next;
}

/// COGBRK {#}D
/// EEEE 1101011 00L DDDDDDDDD 000110101
pub fn cogbrk(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("COGBRK {#}D  is not implemented yet!");
    // return .next;
}

/// COGID {#}D {WC}
/// EEEE 1101011 C0L DDDDDDDDD 000000001
pub fn cogid(cog: *Cog, args: encoding.Only_Dimm_CFlag) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("COGID {#}D {WC} is not implemented yet!");
    // return .next;
}

/// COGINIT {#}D, {#}S {WC}
/// EEEE 1100111 CLI DDDDDDDDD SSSSSSSSS
pub fn coginit(cog: *Cog, args: encoding.Both_Dimm_Simm_CFlag) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("COGINIT {#}D, {#}S {WC} is not implemented yet!");
    // return .next;
}

/// COGSTOP {#}D
/// EEEE 1101011 00L DDDDDDDDD 000000011
pub fn cogstop(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("COGSTOP {#}D  is not implemented yet!");
    // return .next;
}

/// CRCBIT D, {#}S
/// EEEE 1001110 10I DDDDDDDDD SSSSSSSSS
pub fn crcbit(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CRCBIT D, {#}S  is not implemented yet!");
    // return .next;
}

/// CRCNIB D, {#}S
/// EEEE 1001110 11I DDDDDDDDD SSSSSSSSS
pub fn crcnib(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("CRCNIB D, {#}S  is not implemented yet!");
    // return .next;
}

/// DECMOD D, {#}S {WC/WZ/WCZ}
/// EEEE 0111001 CZI DDDDDDDDD SSSSSSSSS
pub fn decmod(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DECMOD D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// DECOD D, {#}S
/// EEEE 1001110 00I DDDDDDDDD SSSSSSSSS
pub fn decod(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DECOD D, {#}S  is not implemented yet!");
    // return .next;
}

/// DIRC {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001000010
pub fn dirc(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DIRC {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// DIRH {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001000001
pub fn dirh(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DIRH {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// DIRL {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001000000
pub fn dirl(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DIRL {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// DIRNC {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001000011
pub fn dirnc(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DIRNC {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// DIRNOT {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001000111
pub fn dirnot(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DIRNOT {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// DIRNZ {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001000101
pub fn dirnz(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DIRNZ {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// DIRRND {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001000110
pub fn dirrnd(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DIRRND {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// DIRZ {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001000100
pub fn dirz(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DIRZ {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// DJF D, {#}S**
/// EEEE 1011011 10I DDDDDDDDD SSSSSSSSS
pub fn djf(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DJF D, {#}S**  is not implemented yet!");
    // return .next;
}

/// DJNF D, {#}S**
/// EEEE 1011011 11I DDDDDDDDD SSSSSSSSS
pub fn djnf(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DJNF D, {#}S**  is not implemented yet!");
    // return .next;
}

/// DJNZ D, {#}S**
/// EEEE 1011011 01I DDDDDDDDD SSSSSSSSS
pub fn djnz(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DJNZ D, {#}S**  is not implemented yet!");
    // return .next;
}

/// DJZ D, {#}S**
/// EEEE 1011011 00I DDDDDDDDD SSSSSSSSS
pub fn djz(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DJZ D, {#}S**  is not implemented yet!");
    // return .next;
}

/// DRVC {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001011010
pub fn drvc(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DRVC {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// DRVH {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001011001
pub fn drvh(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DRVH {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// DRVL {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001011000
pub fn drvl(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DRVL {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// DRVNC {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001011011
pub fn drvnc(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DRVNC {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// DRVNOT {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001011111
pub fn drvnot(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DRVNOT {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// DRVNZ {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001011101
pub fn drvnz(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DRVNZ {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// DRVRND {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001011110
pub fn drvrnd(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DRVRND {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// DRVZ {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001011100
pub fn drvz(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("DRVZ {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// ENCOD D, {#}S {WC/WZ/WCZ}
/// EEEE 0111100 CZI DDDDDDDDD SSSSSSSSS
pub fn encod(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ENCOD D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// EXECF {#}D
/// EEEE 1101011 00L DDDDDDDDD 000110011
pub fn execf(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("EXECF {#}D  is not implemented yet!");
    // return .next;
}

/// FBLOCK {#}D, {#}S
/// EEEE 1100100 1LI DDDDDDDDD SSSSSSSSS
pub fn fblock(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("FBLOCK {#}D, {#}S  is not implemented yet!");
    // return .next;
}

/// FGE D, {#}S {WC/WZ/WCZ}
/// EEEE 0011000 CZI DDDDDDDDD SSSSSSSSS
pub fn fge(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("FGE D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// FGES D, {#}S {WC/WZ/WCZ}
/// EEEE 0011010 CZI DDDDDDDDD SSSSSSSSS
pub fn fges(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("FGES D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// FLE D, {#}S {WC/WZ/WCZ}
/// EEEE 0011001 CZI DDDDDDDDD SSSSSSSSS
pub fn fle(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("FLE D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// FLES D, {#}S {WC/WZ/WCZ}
/// EEEE 0011011 CZI DDDDDDDDD SSSSSSSSS
pub fn fles(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("FLES D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// FLTC {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001010010
pub fn fltc(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("FLTC {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// FLTH {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001010001
pub fn flth(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("FLTH {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// FLTL {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001010000
pub fn fltl(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("FLTL {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// FLTNC {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001010011
pub fn fltnc(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("FLTNC {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// FLTNOT {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001010111
pub fn fltnot(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("FLTNOT {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// FLTNZ {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001010101
pub fn fltnz(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("FLTNZ {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// FLTRND {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001010110
pub fn fltrnd(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("FLTRND {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// FLTZ {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001010100
pub fn fltz(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("FLTZ {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// GETBRK D WC/WZ/WCZ
/// EEEE 1101011 CZ0 DDDDDDDDD 000110101
pub fn getbrk(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("GETBRK D WC/WZ/WCZ is not implemented yet!");
    // return .next;
}

/// GETBYTE D, {#}S, #N
/// EEEE 1000111 NNI DDDDDDDDD SSSSSSSSS
pub fn getbyte(cog: *Cog, args: encoding.Both_D_Simm_N2) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("GETBYTE D, {#}S, #N  is not implemented yet!");
    // return .next;
}

/// GETCT D {WC}
/// EEEE 1101011 C00 DDDDDDDDD 000011010
pub fn getct(cog: *Cog, args: encoding.Only_D_CFlag) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("GETCT D {WC} is not implemented yet!");
    // return .next;
}

/// GETNIB D, {#}S, #N
/// EEEE 100001N NNI DDDDDDDDD SSSSSSSSS
pub fn getnib(cog: *Cog, args: encoding.Both_D_Simm_N3) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("GETNIB D, {#}S, #N  is not implemented yet!");
    // return .next;
}

/// GETPTR D
/// EEEE 1101011 000 DDDDDDDDD 000110100
pub fn getptr(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("GETPTR D  is not implemented yet!");
    // return .next;
}

/// GETQX D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000011000
pub fn getqx(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("GETQX D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// GETQY D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000011001
pub fn getqy(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("GETQY D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// GETRND D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000011011
pub fn getrnd(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("GETRND D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// GETSCP D
/// EEEE 1101011 000 DDDDDDDDD 001110001
pub fn getscp(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("GETSCP D  is not implemented yet!");
    // return .next;
}

/// GETWORD D, {#}S, #N
/// EEEE 1001001 1NI DDDDDDDDD SSSSSSSSS
pub fn getword(cog: *Cog, args: encoding.Both_D_Simm_N1) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("GETWORD D, {#}S, #N  is not implemented yet!");
    // return .next;
}

/// GETXACC D
/// EEEE 1101011 000 DDDDDDDDD 000011110
pub fn getxacc(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("GETXACC D  is not implemented yet!");
    // return .next;
}

/// HUBSET {#}D
/// EEEE 1101011 00L DDDDDDDDD 000000000
pub fn hubset(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("HUBSET {#}D  is not implemented yet!");
    // return .next;
}

/// IJNZ D, {#}S**
/// EEEE 1011100 01I DDDDDDDDD SSSSSSSSS
pub fn ijnz(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("IJNZ D, {#}S**  is not implemented yet!");
    // return .next;
}

/// IJZ D, {#}S**
/// EEEE 1011100 00I DDDDDDDDD SSSSSSSSS
pub fn ijz(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("IJZ D, {#}S**  is not implemented yet!");
    // return .next;
}

/// INCMOD D, {#}S {WC/WZ/WCZ}
/// EEEE 0111000 CZI DDDDDDDDD SSSSSSSSS
pub fn incmod(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("INCMOD D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// JATN {#}S**
/// EEEE 1011110 01I 000001110 SSSSSSSSS
pub fn jatn(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JATN {#}S**  is not implemented yet!");
    // return .next;
}

/// JCT1 {#}S**
/// EEEE 1011110 01I 000000001 SSSSSSSSS
pub fn jct1(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JCT1 {#}S**  is not implemented yet!");
    // return .next;
}

/// JCT2 {#}S**
/// EEEE 1011110 01I 000000010 SSSSSSSSS
pub fn jct2(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JCT2 {#}S**  is not implemented yet!");
    // return .next;
}

/// JCT3 {#}S**
/// EEEE 1011110 01I 000000011 SSSSSSSSS
pub fn jct3(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JCT3 {#}S**  is not implemented yet!");
    // return .next;
}

/// JFBW {#}S**
/// EEEE 1011110 01I 000001001 SSSSSSSSS
pub fn jfbw(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JFBW {#}S**  is not implemented yet!");
    // return .next;
}

/// JINT {#}S**
/// EEEE 1011110 01I 000000000 SSSSSSSSS
pub fn jint(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JINT {#}S**  is not implemented yet!");
    // return .next;
}

/// JMP #{\}A
/// EEEE 1101100 RAA AAAAAAAAA AAAAAAAAA
pub fn jmp_a(cog: *Cog, args: encoding.AbsPointer) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JMP #{\\}A  is not implemented yet!");
    // return .next;
}

/// JMP D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000101100
pub fn jmp_d(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JMP D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// JMPREL {#}D
/// EEEE 1101011 00L DDDDDDDDD 000110000
pub fn jmprel(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JMPREL {#}D  is not implemented yet!");
    // return .next;
}

/// JNATN {#}S**
/// EEEE 1011110 01I 000011110 SSSSSSSSS
pub fn jnatn(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNATN {#}S**  is not implemented yet!");
    // return .next;
}

/// JNCT1 {#}S**
/// EEEE 1011110 01I 000010001 SSSSSSSSS
pub fn jnct1(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNCT1 {#}S**  is not implemented yet!");
    // return .next;
}

/// JNCT2 {#}S**
/// EEEE 1011110 01I 000010010 SSSSSSSSS
pub fn jnct2(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNCT2 {#}S**  is not implemented yet!");
    // return .next;
}

/// JNCT3 {#}S**
/// EEEE 1011110 01I 000010011 SSSSSSSSS
pub fn jnct3(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNCT3 {#}S**  is not implemented yet!");
    // return .next;
}

/// JNFBW {#}S**
/// EEEE 1011110 01I 000011001 SSSSSSSSS
pub fn jnfbw(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNFBW {#}S**  is not implemented yet!");
    // return .next;
}

/// JNINT {#}S**
/// EEEE 1011110 01I 000010000 SSSSSSSSS
pub fn jnint(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNINT {#}S**  is not implemented yet!");
    // return .next;
}

/// JNPAT {#}S**
/// EEEE 1011110 01I 000011000 SSSSSSSSS
pub fn jnpat(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNPAT {#}S**  is not implemented yet!");
    // return .next;
}

/// JNQMT {#}S**
/// EEEE 1011110 01I 000011111 SSSSSSSSS
pub fn jnqmt(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNQMT {#}S**  is not implemented yet!");
    // return .next;
}

/// JNSE1 {#}S**
/// EEEE 1011110 01I 000010100 SSSSSSSSS
pub fn jnse1(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNSE1 {#}S**  is not implemented yet!");
    // return .next;
}

/// JNSE2 {#}S**
/// EEEE 1011110 01I 000010101 SSSSSSSSS
pub fn jnse2(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNSE2 {#}S**  is not implemented yet!");
    // return .next;
}

/// JNSE3 {#}S**
/// EEEE 1011110 01I 000010110 SSSSSSSSS
pub fn jnse3(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNSE3 {#}S**  is not implemented yet!");
    // return .next;
}

/// JNSE4 {#}S**
/// EEEE 1011110 01I 000010111 SSSSSSSSS
pub fn jnse4(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNSE4 {#}S**  is not implemented yet!");
    // return .next;
}

/// JNXFI {#}S**
/// EEEE 1011110 01I 000011011 SSSSSSSSS
pub fn jnxfi(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNXFI {#}S**  is not implemented yet!");
    // return .next;
}

/// JNXMT {#}S**
/// EEEE 1011110 01I 000011010 SSSSSSSSS
pub fn jnxmt(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNXMT {#}S**  is not implemented yet!");
    // return .next;
}

/// JNXRL {#}S**
/// EEEE 1011110 01I 000011101 SSSSSSSSS
pub fn jnxrl(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNXRL {#}S**  is not implemented yet!");
    // return .next;
}

/// JNXRO {#}S**
/// EEEE 1011110 01I 000011100 SSSSSSSSS
pub fn jnxro(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JNXRO {#}S**  is not implemented yet!");
    // return .next;
}

/// JPAT {#}S**
/// EEEE 1011110 01I 000001000 SSSSSSSSS
pub fn jpat(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JPAT {#}S**  is not implemented yet!");
    // return .next;
}

/// JQMT {#}S**
/// EEEE 1011110 01I 000001111 SSSSSSSSS
pub fn jqmt(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JQMT {#}S**  is not implemented yet!");
    // return .next;
}

/// JSE1 {#}S**
/// EEEE 1011110 01I 000000100 SSSSSSSSS
pub fn jse1(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JSE1 {#}S**  is not implemented yet!");
    // return .next;
}

/// JSE2 {#}S**
/// EEEE 1011110 01I 000000101 SSSSSSSSS
pub fn jse2(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JSE2 {#}S**  is not implemented yet!");
    // return .next;
}

/// JSE3 {#}S**
/// EEEE 1011110 01I 000000110 SSSSSSSSS
pub fn jse3(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JSE3 {#}S**  is not implemented yet!");
    // return .next;
}

/// JSE4 {#}S**
/// EEEE 1011110 01I 000000111 SSSSSSSSS
pub fn jse4(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JSE4 {#}S**  is not implemented yet!");
    // return .next;
}

/// JXFI {#}S**
/// EEEE 1011110 01I 000001011 SSSSSSSSS
pub fn jxfi(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JXFI {#}S**  is not implemented yet!");
    // return .next;
}

/// JXMT {#}S**
/// EEEE 1011110 01I 000001010 SSSSSSSSS
pub fn jxmt(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JXMT {#}S**  is not implemented yet!");
    // return .next;
}

/// JXRL {#}S**
/// EEEE 1011110 01I 000001101 SSSSSSSSS
pub fn jxrl(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JXRL {#}S**  is not implemented yet!");
    // return .next;
}

/// JXRO {#}S**
/// EEEE 1011110 01I 000001100 SSSSSSSSS
pub fn jxro(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("JXRO {#}S**  is not implemented yet!");
    // return .next;
}

/// LOC PA/PB/PTRA/PTRB, #{\}A
/// EEEE 11101WW RAA AAAAAAAAA AAAAAAAAA
pub fn loc(cog: *Cog, args: encoding.LocStyle) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("LOC PA/PB/PTRA/PTRB, #{\\}A  is not implemented yet!");
    // return .next;
}

/// LOCKNEW D {WC}
/// EEEE 1101011 C00 DDDDDDDDD 000000100
pub fn locknew(cog: *Cog, args: encoding.Only_D_CFlag) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("LOCKNEW D {WC} is not implemented yet!");
    // return .next;
}

/// LOCKREL {#}D {WC}
/// EEEE 1101011 C0L DDDDDDDDD 000000111
pub fn lockrel(cog: *Cog, args: encoding.Only_Dimm_CFlag) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("LOCKREL {#}D {WC} is not implemented yet!");
    // return .next;
}

/// LOCKRET {#}D
/// EEEE 1101011 00L DDDDDDDDD 000000101
pub fn lockret(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("LOCKRET {#}D  is not implemented yet!");
    // return .next;
}

/// LOCKTRY {#}D {WC}
/// EEEE 1101011 C0L DDDDDDDDD 000000110
pub fn locktry(cog: *Cog, args: encoding.Only_Dimm_CFlag) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("LOCKTRY {#}D {WC} is not implemented yet!");
    // return .next;
}

/// MERGEB D
/// EEEE 1101011 000 DDDDDDDDD 001100001
pub fn mergeb(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MERGEB D  is not implemented yet!");
    // return .next;
}

/// MERGEW D
/// EEEE 1101011 000 DDDDDDDDD 001100011
pub fn mergew(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MERGEW D  is not implemented yet!");
    // return .next;
}

/// MIXPIX D, {#}S
/// EEEE 1010010 11I DDDDDDDDD SSSSSSSSS
pub fn mixpix(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MIXPIX D, {#}S  is not implemented yet!");
    // return .next;
}

/// MODCZ c, z {WC/WZ/WCZ}
/// EEEE 1101011 CZ1 0cccczzzz 001101111
pub fn modcz(cog: *Cog, args: encoding.UpdateFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MODCZ c, z {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// MOV D, {#}S {WC/WZ/WCZ}
/// EEEE 0110000 CZI DDDDDDDDD SSSSSSSSS
pub fn mov(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MOV D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// MOVBYTS D, {#}S
/// EEEE 1001111 11I DDDDDDDDD SSSSSSSSS
pub fn movbyts(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MOVBYTS D, {#}S  is not implemented yet!");
    // return .next;
}

/// MUL D, {#}S {WZ}
/// EEEE 1010000 0ZI DDDDDDDDD SSSSSSSSS
pub fn mul(cog: *Cog, args: encoding.Both_D_Simm_ZFlag) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MUL D, {#}S {WZ} is not implemented yet!");
    // return .next;
}

/// MULPIX D, {#}S
/// EEEE 1010010 01I DDDDDDDDD SSSSSSSSS
pub fn mulpix(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MULPIX D, {#}S  is not implemented yet!");
    // return .next;
}

/// MULS D, {#}S {WZ}
/// EEEE 1010000 1ZI DDDDDDDDD SSSSSSSSS
pub fn muls(cog: *Cog, args: encoding.Both_D_Simm_ZFlag) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MULS D, {#}S {WZ} is not implemented yet!");
    // return .next;
}

/// MUXC D, {#}S {WC/WZ/WCZ}
/// EEEE 0101100 CZI DDDDDDDDD SSSSSSSSS
pub fn muxc(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MUXC D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// MUXNC D, {#}S {WC/WZ/WCZ}
/// EEEE 0101101 CZI DDDDDDDDD SSSSSSSSS
pub fn muxnc(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MUXNC D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// MUXNIBS D, {#}S
/// EEEE 1001111 01I DDDDDDDDD SSSSSSSSS
pub fn muxnibs(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MUXNIBS D, {#}S  is not implemented yet!");
    // return .next;
}

/// MUXNITS D, {#}S
/// EEEE 1001111 00I DDDDDDDDD SSSSSSSSS
pub fn muxnits(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MUXNITS D, {#}S  is not implemented yet!");
    // return .next;
}

/// MUXNZ D, {#}S {WC/WZ/WCZ}
/// EEEE 0101111 CZI DDDDDDDDD SSSSSSSSS
pub fn muxnz(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MUXNZ D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// MUXQ D, {#}S
/// EEEE 1001111 10I DDDDDDDDD SSSSSSSSS
pub fn muxq(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MUXQ D, {#}S  is not implemented yet!");
    // return .next;
}

/// MUXZ D, {#}S {WC/WZ/WCZ}
/// EEEE 0101110 CZI DDDDDDDDD SSSSSSSSS
pub fn muxz(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("MUXZ D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// NEG D, {#}S {WC/WZ/WCZ}
/// EEEE 0110011 CZI DDDDDDDDD SSSSSSSSS
pub fn neg(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("NEG D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// NEGC D, {#}S {WC/WZ/WCZ}
/// EEEE 0110100 CZI DDDDDDDDD SSSSSSSSS
pub fn negc(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("NEGC D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// NEGNC D, {#}S {WC/WZ/WCZ}
/// EEEE 0110101 CZI DDDDDDDDD SSSSSSSSS
pub fn negnc(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("NEGNC D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// NEGNZ D, {#}S {WC/WZ/WCZ}
/// EEEE 0110111 CZI DDDDDDDDD SSSSSSSSS
pub fn negnz(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("NEGNZ D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// NEGZ D, {#}S {WC/WZ/WCZ}
/// EEEE 0110110 CZI DDDDDDDDD SSSSSSSSS
pub fn negz(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("NEGZ D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// NIXINT1
/// EEEE 1101011 000 000100101 000100100
pub fn nixint1(cog: *Cog, args: encoding.NoOperands) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("NIXINT1  is not implemented yet!");
    // return .next;
}

/// NIXINT2
/// EEEE 1101011 000 000100110 000100100
pub fn nixint2(cog: *Cog, args: encoding.NoOperands) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("NIXINT2  is not implemented yet!");
    // return .next;
}

/// NIXINT3
/// EEEE 1101011 000 000100111 000100100
pub fn nixint3(cog: *Cog, args: encoding.NoOperands) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("NIXINT3  is not implemented yet!");
    // return .next;
}

/// NOP
/// 0000 0000000 000 000000000 000000000
pub fn nop(cog: *Cog, args: encoding.Nop) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("NOP  is not implemented yet!");
    // return .next;
}

/// NOT D, {#}S {WC/WZ/WCZ}
/// EEEE 0110001 CZI DDDDDDDDD SSSSSSSSS
pub fn not(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("NOT D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// ONES D, {#}S {WC/WZ/WCZ}
/// EEEE 0111101 CZI DDDDDDDDD SSSSSSSSS
pub fn ones(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ONES D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// OR D, {#}S {WC/WZ/WCZ}
/// EEEE 0101010 CZI DDDDDDDDD SSSSSSSSS
pub fn @"or"(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("OR D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// OUTC {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001001010
pub fn outc(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("OUTC {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// OUTH {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001001001
pub fn outh(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("OUTH {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// OUTL {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001001000
pub fn outl(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("OUTL {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// OUTNC {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001001011
pub fn outnc(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("OUTNC {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// OUTNOT {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001001111
pub fn outnot(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("OUTNOT {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// OUTNZ {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001001101
pub fn outnz(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("OUTNZ {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// OUTRND {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001001110
pub fn outrnd(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("OUTRND {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// OUTZ {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001001100
pub fn outz(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("OUTZ {#}D {WCZ} is not implemented yet!");
    // return .next;
}

/// POLLATN {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000001110 000100100
pub fn pollatn(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLATN {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// POLLCT1 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000000001 000100100
pub fn pollct1(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLCT1 {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// POLLCT2 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000000010 000100100
pub fn pollct2(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLCT2 {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// POLLCT3 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000000011 000100100
pub fn pollct3(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLCT3 {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// POLLFBW {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000001001 000100100
pub fn pollfbw(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLFBW {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// POLLINT {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000000000 000100100
pub fn pollint(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLINT {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// POLLPAT {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000001000 000100100
pub fn pollpat(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLPAT {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// POLLQMT {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000001111 000100100
pub fn pollqmt(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLQMT {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// POLLSE1 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000000100 000100100
pub fn pollse1(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLSE1 {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// POLLSE2 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000000101 000100100
pub fn pollse2(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLSE2 {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// POLLSE3 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000000110 000100100
pub fn pollse3(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLSE3 {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// POLLSE4 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000000111 000100100
pub fn pollse4(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLSE4 {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// POLLXFI {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000001011 000100100
pub fn pollxfi(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLXFI {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// POLLXMT {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000001010 000100100
pub fn pollxmt(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLXMT {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// POLLXRL {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000001101 000100100
pub fn pollxrl(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLXRL {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// POLLXRO {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000001100 000100100
pub fn pollxro(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POLLXRO {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// POP D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000101011
pub fn pop(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("POP D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// PUSH {#}D
/// EEEE 1101011 00L DDDDDDDDD 000101010
pub fn push(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("PUSH {#}D  is not implemented yet!");
    // return .next;
}

/// QDIV {#}D, {#}S
/// EEEE 1101000 1LI DDDDDDDDD SSSSSSSSS
pub fn qdiv(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("QDIV {#}D, {#}S  is not implemented yet!");
    // return .next;
}

/// QEXP {#}D
/// EEEE 1101011 00L DDDDDDDDD 000001111
pub fn qexp(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("QEXP {#}D  is not implemented yet!");
    // return .next;
}

/// QFRAC {#}D, {#}S
/// EEEE 1101001 0LI DDDDDDDDD SSSSSSSSS
pub fn qfrac(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("QFRAC {#}D, {#}S  is not implemented yet!");
    // return .next;
}

/// QLOG {#}D
/// EEEE 1101011 00L DDDDDDDDD 000001110
pub fn qlog(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("QLOG {#}D  is not implemented yet!");
    // return .next;
}

/// QMUL {#}D, {#}S
/// EEEE 1101000 0LI DDDDDDDDD SSSSSSSSS
pub fn qmul(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("QMUL {#}D, {#}S  is not implemented yet!");
    // return .next;
}

/// QROTATE {#}D, {#}S
/// EEEE 1101010 0LI DDDDDDDDD SSSSSSSSS
pub fn qrotate(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("QROTATE {#}D, {#}S  is not implemented yet!");
    // return .next;
}

/// QSQRT {#}D, {#}S
/// EEEE 1101001 1LI DDDDDDDDD SSSSSSSSS
pub fn qsqrt(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("QSQRT {#}D, {#}S  is not implemented yet!");
    // return .next;
}

/// QVECTOR {#}D, {#}S
/// EEEE 1101010 1LI DDDDDDDDD SSSSSSSSS
pub fn qvector(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("QVECTOR {#}D, {#}S  is not implemented yet!");
    // return .next;
}

/// RCL D, {#}S {WC/WZ/WCZ}
/// EEEE 0000101 CZI DDDDDDDDD SSSSSSSSS
pub fn rcl(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RCL D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// RCR D, {#}S {WC/WZ/WCZ}
/// EEEE 0000100 CZI DDDDDDDDD SSSSSSSSS
pub fn rcr(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RCR D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// RCZL D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 001101011
pub fn rczl(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RCZL D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// RCZR D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 001101010
pub fn rczr(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RCZR D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// RDBYTE D, {#}S/P {WC/WZ/WCZ}
/// EEEE 1010110 CZI DDDDDDDDD SSSSSSSSS
pub fn rdbyte(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RDBYTE D, {#}S/P {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// RDFAST {#}D, {#}S
/// EEEE 1100011 1LI DDDDDDDDD SSSSSSSSS
pub fn rdfast(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RDFAST {#}D, {#}S  is not implemented yet!");
    // return .next;
}

/// RDLONG D, {#}S/P {WC/WZ/WCZ}
/// EEEE 1011000 CZI DDDDDDDDD SSSSSSSSS
pub fn rdlong(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RDLONG D, {#}S/P {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// RDLUT D, {#}S/P {WC/WZ/WCZ}
/// EEEE 1010101 CZI DDDDDDDDD SSSSSSSSS
pub fn rdlut(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RDLUT D, {#}S/P {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// RDPIN D, {#}S {WC}
/// EEEE 1010100 C1I DDDDDDDDD SSSSSSSSS
pub fn rdpin(cog: *Cog, args: encoding.Both_D_Simm_CFlag) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RDPIN D, {#}S {WC} is not implemented yet!");
    // return .next;
}

/// RDWORD D, {#}S/P {WC/WZ/WCZ}
/// EEEE 1010111 CZI DDDDDDDDD SSSSSSSSS
pub fn rdword(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RDWORD D, {#}S/P {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// REP {#}D, {#}S
/// EEEE 1100110 1LI DDDDDDDDD SSSSSSSSS
pub fn rep(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("REP {#}D, {#}S  is not implemented yet!");
    // return .next;
}

/// RET {WC/WZ/WCZ}
/// EEEE 1101011 CZ1 000000000 000101101
pub fn ret(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RET {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// RETA {WC/WZ/WCZ}
/// EEEE 1101011 CZ1 000000000 000101110
pub fn reta(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RETA {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// RETB {WC/WZ/WCZ}
/// EEEE 1101011 CZ1 000000000 000101111
pub fn retb(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RETB {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// REV D
/// EEEE 1101011 000 DDDDDDDDD 001101001
pub fn rev(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("REV D  is not implemented yet!");
    // return .next;
}

/// RFBYTE D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000010000
pub fn rfbyte(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RFBYTE D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// RFLONG D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000010010
pub fn rflong(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RFLONG D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// RFVAR D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000010011
pub fn rfvar(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RFVAR D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// RFVARS D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000010100
pub fn rfvars(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RFVARS D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// RFWORD D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000010001
pub fn rfword(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RFWORD D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// RGBEXP D
/// EEEE 1101011 000 DDDDDDDDD 001100111
pub fn rgbexp(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RGBEXP D  is not implemented yet!");
    // return .next;
}

/// RGBSQZ D
/// EEEE 1101011 000 DDDDDDDDD 001100110
pub fn rgbsqz(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RGBSQZ D  is not implemented yet!");
    // return .next;
}

/// ROL D, {#}S {WC/WZ/WCZ}
/// EEEE 0000001 CZI DDDDDDDDD SSSSSSSSS
pub fn rol(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ROL D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// ROLBYTE D, {#}S, #N
/// EEEE 1001000 NNI DDDDDDDDD SSSSSSSSS
pub fn rolbyte(cog: *Cog, args: encoding.Both_D_Simm_N2) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ROLBYTE D, {#}S, #N  is not implemented yet!");
    // return .next;
}

/// ROLNIB D, {#}S, #N
/// EEEE 100010N NNI DDDDDDDDD SSSSSSSSS
pub fn rolnib(cog: *Cog, args: encoding.Both_D_Simm_N3) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ROLNIB D, {#}S, #N  is not implemented yet!");
    // return .next;
}

/// ROLWORD D, {#}S, #N
/// EEEE 1001010 0NI DDDDDDDDD SSSSSSSSS
pub fn rolword(cog: *Cog, args: encoding.Both_D_Simm_N1) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ROLWORD D, {#}S, #N  is not implemented yet!");
    // return .next;
}

/// ROR D, {#}S {WC/WZ/WCZ}
/// EEEE 0000000 CZI DDDDDDDDD SSSSSSSSS
pub fn ror(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ROR D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// RQPIN D, {#}S {WC}
/// EEEE 1010100 C0I DDDDDDDDD SSSSSSSSS
pub fn rqpin(cog: *Cog, args: encoding.Both_D_Simm_CFlag) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("RQPIN D, {#}S {WC} is not implemented yet!");
    // return .next;
}

/// SAL D, {#}S {WC/WZ/WCZ}
/// EEEE 0000111 CZI DDDDDDDDD SSSSSSSSS
pub fn sal(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SAL D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// SAR D, {#}S {WC/WZ/WCZ}
/// EEEE 0000110 CZI DDDDDDDDD SSSSSSSSS
pub fn sar(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SAR D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// SCA D, {#}S {WZ}
/// EEEE 1010001 0ZI DDDDDDDDD SSSSSSSSS
pub fn sca(cog: *Cog, args: encoding.Both_D_Simm_ZFlag) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SCA D, {#}S {WZ} is not implemented yet!");
    // return .next;
}

/// SCAS D, {#}S {WZ}
/// EEEE 1010001 1ZI DDDDDDDDD SSSSSSSSS
pub fn scas(cog: *Cog, args: encoding.Both_D_Simm_ZFlag) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SCAS D, {#}S {WZ} is not implemented yet!");
    // return .next;
}

/// SETBYTE D, {#}S, #N
/// EEEE 1000110 NNI DDDDDDDDD SSSSSSSSS
pub fn setbyte(cog: *Cog, args: encoding.Both_D_Simm_N2) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETBYTE D, {#}S, #N  is not implemented yet!");
    // return .next;
}

/// SETCFRQ {#}D
/// EEEE 1101011 00L DDDDDDDDD 000111011
pub fn setcfrq(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETCFRQ {#}D  is not implemented yet!");
    // return .next;
}

/// SETCI {#}D
/// EEEE 1101011 00L DDDDDDDDD 000111001
pub fn setci(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETCI {#}D  is not implemented yet!");
    // return .next;
}

/// SETCMOD {#}D
/// EEEE 1101011 00L DDDDDDDDD 000111100
pub fn setcmod(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETCMOD {#}D  is not implemented yet!");
    // return .next;
}

/// SETCQ {#}D
/// EEEE 1101011 00L DDDDDDDDD 000111010
pub fn setcq(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETCQ {#}D  is not implemented yet!");
    // return .next;
}

/// SETCY {#}D
/// EEEE 1101011 00L DDDDDDDDD 000111000
pub fn setcy(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETCY {#}D  is not implemented yet!");
    // return .next;
}

/// SETD D, {#}S
/// EEEE 1001101 10I DDDDDDDDD SSSSSSSSS
pub fn setd(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETD D, {#}S  is not implemented yet!");
    // return .next;
}

/// SETDACS {#}D
/// EEEE 1101011 00L DDDDDDDDD 000011100
pub fn setdacs(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETDACS {#}D  is not implemented yet!");
    // return .next;
}

/// SETINT1 {#}D
/// EEEE 1101011 00L DDDDDDDDD 000100101
pub fn setint1(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETINT1 {#}D  is not implemented yet!");
    // return .next;
}

/// SETINT2 {#}D
/// EEEE 1101011 00L DDDDDDDDD 000100110
pub fn setint2(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETINT2 {#}D  is not implemented yet!");
    // return .next;
}

/// SETINT3 {#}D
/// EEEE 1101011 00L DDDDDDDDD 000100111
pub fn setint3(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETINT3 {#}D  is not implemented yet!");
    // return .next;
}

/// SETLUTS {#}D
/// EEEE 1101011 00L DDDDDDDDD 000110111
pub fn setluts(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETLUTS {#}D  is not implemented yet!");
    // return .next;
}

/// SETNIB D, {#}S, #N
/// EEEE 100000N NNI DDDDDDDDD SSSSSSSSS
pub fn setnib(cog: *Cog, args: encoding.Both_D_Simm_N3) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETNIB D, {#}S, #N  is not implemented yet!");
    // return .next;
}

/// SETPAT {#}D, {#}S
/// EEEE 1011111 1LI DDDDDDDDD SSSSSSSSS
pub fn setpat(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETPAT {#}D, {#}S  is not implemented yet!");
    // return .next;
}

/// SETPIV {#}D
/// EEEE 1101011 00L DDDDDDDDD 000111101
pub fn setpiv(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETPIV {#}D  is not implemented yet!");
    // return .next;
}

/// SETPIX {#}D
/// EEEE 1101011 00L DDDDDDDDD 000111110
pub fn setpix(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETPIX {#}D  is not implemented yet!");
    // return .next;
}

/// SETQ {#}D
/// EEEE 1101011 00L DDDDDDDDD 000101000
pub fn setq(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETQ {#}D  is not implemented yet!");
    // return .next;
}

/// SETQ2 {#}D
/// EEEE 1101011 00L DDDDDDDDD 000101001
pub fn setq2(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETQ2 {#}D  is not implemented yet!");
    // return .next;
}

/// SETR D, {#}S
/// EEEE 1001101 01I DDDDDDDDD SSSSSSSSS
pub fn setr(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETR D, {#}S  is not implemented yet!");
    // return .next;
}

/// SETS D, {#}S
/// EEEE 1001101 11I DDDDDDDDD SSSSSSSSS
pub fn sets(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETS D, {#}S  is not implemented yet!");
    // return .next;
}

/// SETSCP {#}D
/// EEEE 1101011 00L DDDDDDDDD 001110000
pub fn setscp(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETSCP {#}D  is not implemented yet!");
    // return .next;
}

/// SETSE1 {#}D
/// EEEE 1101011 00L DDDDDDDDD 000100000
pub fn setse1(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETSE1 {#}D  is not implemented yet!");
    // return .next;
}

/// SETSE2 {#}D
/// EEEE 1101011 00L DDDDDDDDD 000100001
pub fn setse2(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETSE2 {#}D  is not implemented yet!");
    // return .next;
}

/// SETSE3 {#}D
/// EEEE 1101011 00L DDDDDDDDD 000100010
pub fn setse3(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETSE3 {#}D  is not implemented yet!");
    // return .next;
}

/// SETSE4 {#}D
/// EEEE 1101011 00L DDDDDDDDD 000100011
pub fn setse4(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETSE4 {#}D  is not implemented yet!");
    // return .next;
}

/// SETWORD D, {#}S, #N
/// EEEE 1001001 0NI DDDDDDDDD SSSSSSSSS
pub fn setword(cog: *Cog, args: encoding.Both_D_Simm_N1) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETWORD D, {#}S, #N  is not implemented yet!");
    // return .next;
}

/// SETXFRQ {#}D
/// EEEE 1101011 00L DDDDDDDDD 000011101
pub fn setxfrq(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SETXFRQ {#}D  is not implemented yet!");
    // return .next;
}

/// SEUSSF D
/// EEEE 1101011 000 DDDDDDDDD 001100100
pub fn seussf(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SEUSSF D  is not implemented yet!");
    // return .next;
}

/// SEUSSR D
/// EEEE 1101011 000 DDDDDDDDD 001100101
pub fn seussr(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SEUSSR D  is not implemented yet!");
    // return .next;
}

/// SHL D, {#}S {WC/WZ/WCZ}
/// EEEE 0000011 CZI DDDDDDDDD SSSSSSSSS
pub fn shl(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SHL D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// SHR D, {#}S {WC/WZ/WCZ}
/// EEEE 0000010 CZI DDDDDDDDD SSSSSSSSS
pub fn shr(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SHR D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// SIGNX D, {#}S {WC/WZ/WCZ}
/// EEEE 0111011 CZI DDDDDDDDD SSSSSSSSS
pub fn signx(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SIGNX D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// SKIP {#}D
/// EEEE 1101011 00L DDDDDDDDD 000110001
pub fn skip(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SKIP {#}D  is not implemented yet!");
    // return .next;
}

/// SKIPF {#}D
/// EEEE 1101011 00L DDDDDDDDD 000110010
pub fn skipf(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SKIPF {#}D  is not implemented yet!");
    // return .next;
}

/// SPLITB D
/// EEEE 1101011 000 DDDDDDDDD 001100000
pub fn splitb(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SPLITB D  is not implemented yet!");
    // return .next;
}

/// SPLITW D
/// EEEE 1101011 000 DDDDDDDDD 001100010
pub fn splitw(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SPLITW D  is not implemented yet!");
    // return .next;
}

/// STALLI
/// EEEE 1101011 000 000100001 000100100
pub fn stalli(cog: *Cog, args: encoding.NoOperands) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("STALLI  is not implemented yet!");
    // return .next;
}

/// SUB D, {#}S {WC/WZ/WCZ}
/// EEEE 0001100 CZI DDDDDDDDD SSSSSSSSS
pub fn sub(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SUB D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// SUBR D, {#}S {WC/WZ/WCZ}
/// EEEE 0010110 CZI DDDDDDDDD SSSSSSSSS
pub fn subr(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SUBR D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// SUBS D, {#}S {WC/WZ/WCZ}
/// EEEE 0001110 CZI DDDDDDDDD SSSSSSSSS
pub fn subs(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SUBS D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// SUBSX D, {#}S {WC/WZ/WCZ}
/// EEEE 0001111 CZI DDDDDDDDD SSSSSSSSS
pub fn subsx(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SUBSX D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// SUBX D, {#}S {WC/WZ/WCZ}
/// EEEE 0001101 CZI DDDDDDDDD SSSSSSSSS
pub fn subx(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SUBX D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// SUMC D, {#}S {WC/WZ/WCZ}
/// EEEE 0011100 CZI DDDDDDDDD SSSSSSSSS
pub fn sumc(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SUMC D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// SUMNC D, {#}S {WC/WZ/WCZ}
/// EEEE 0011101 CZI DDDDDDDDD SSSSSSSSS
pub fn sumnc(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SUMNC D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// SUMNZ D, {#}S {WC/WZ/WCZ}
/// EEEE 0011111 CZI DDDDDDDDD SSSSSSSSS
pub fn sumnz(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SUMNZ D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// SUMZ D, {#}S {WC/WZ/WCZ}
/// EEEE 0011110 CZI DDDDDDDDD SSSSSSSSS
pub fn sumz(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("SUMZ D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// TEST D, {#}S {WC/WZ/WCZ}
/// EEEE 0111110 CZI DDDDDDDDD SSSSSSSSS
pub fn @"test"(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TEST D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// TESTB D, {#}S WC/WZ
/// EEEE 0100000 CZI DDDDDDDDD SSSSSSSSS
pub fn testb(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTB D, {#}S WC/WZ is not implemented yet!");
    // return .next;
}

/// TESTB D, {#}S ANDC/ANDZ
/// EEEE 0100010 CZI DDDDDDDDD SSSSSSSSS
pub fn testb_and(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTB D, {#}S ANDC/ANDZ is not implemented yet!");
    // return .next;
}

/// TESTB D, {#}S ORC/ORZ
/// EEEE 0100100 CZI DDDDDDDDD SSSSSSSSS
pub fn testb_or(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTB D, {#}S ORC/ORZ is not implemented yet!");
    // return .next;
}

/// TESTB D, {#}S XORC/XORZ
/// EEEE 0100110 CZI DDDDDDDDD SSSSSSSSS
pub fn testb_xor(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTB D, {#}S XORC/XORZ is not implemented yet!");
    // return .next;
}

/// TESTBN D, {#}S WC/WZ
/// EEEE 0100001 CZI DDDDDDDDD SSSSSSSSS
pub fn testbn(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTBN D, {#}S WC/WZ is not implemented yet!");
    // return .next;
}

/// TESTBN D, {#}S ANDC/ANDZ
/// EEEE 0100011 CZI DDDDDDDDD SSSSSSSSS
pub fn testbn_and(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTBN D, {#}S ANDC/ANDZ is not implemented yet!");
    // return .next;
}

/// TESTBN D, {#}S ORC/ORZ
/// EEEE 0100101 CZI DDDDDDDDD SSSSSSSSS
pub fn testbn_or(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTBN D, {#}S ORC/ORZ is not implemented yet!");
    // return .next;
}

/// TESTBN D, {#}S XORC/XORZ
/// EEEE 0100111 CZI DDDDDDDDD SSSSSSSSS
pub fn testbn_xor(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTBN D, {#}S XORC/XORZ is not implemented yet!");
    // return .next;
}

/// TESTN D, {#}S {WC/WZ/WCZ}
/// EEEE 0111111 CZI DDDDDDDDD SSSSSSSSS
pub fn testn(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTN D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// TESTP {#}D WC/WZ
/// EEEE 1101011 CZL DDDDDDDDD 001000000
pub fn testp(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTP {#}D WC/WZ is not implemented yet!");
    // return .next;
}

/// TESTP {#}D ANDC/ANDZ
/// EEEE 1101011 CZL DDDDDDDDD 001000010
pub fn testp_and(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTP {#}D ANDC/ANDZ is not implemented yet!");
    // return .next;
}

/// TESTP {#}D ORC/ORZ
/// EEEE 1101011 CZL DDDDDDDDD 001000100
pub fn testp_or(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTP {#}D ORC/ORZ is not implemented yet!");
    // return .next;
}

/// TESTP {#}D XORC/XORZ
/// EEEE 1101011 CZL DDDDDDDDD 001000110
pub fn testp_xor(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTP {#}D XORC/XORZ is not implemented yet!");
    // return .next;
}

/// TESTPN {#}D WC/WZ
/// EEEE 1101011 CZL DDDDDDDDD 001000001
pub fn testpn(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTPN {#}D WC/WZ is not implemented yet!");
    // return .next;
}

/// TESTPN {#}D ANDC/ANDZ
/// EEEE 1101011 CZL DDDDDDDDD 001000011
pub fn testpn_and(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTPN {#}D ANDC/ANDZ is not implemented yet!");
    // return .next;
}

/// TESTPN {#}D ORC/ORZ
/// EEEE 1101011 CZL DDDDDDDDD 001000101
pub fn testpn_or(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTPN {#}D ORC/ORZ is not implemented yet!");
    // return .next;
}

/// TESTPN {#}D XORC/XORZ
/// EEEE 1101011 CZL DDDDDDDDD 001000111
pub fn testpn_xor(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TESTPN {#}D XORC/XORZ is not implemented yet!");
    // return .next;
}

/// TJF D, {#}S**
/// EEEE 1011101 00I DDDDDDDDD SSSSSSSSS
pub fn tjf(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TJF D, {#}S**  is not implemented yet!");
    // return .next;
}

/// TJNF D, {#}S**
/// EEEE 1011101 01I DDDDDDDDD SSSSSSSSS
pub fn tjnf(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TJNF D, {#}S**  is not implemented yet!");
    // return .next;
}

/// TJNS D, {#}S**
/// EEEE 1011101 11I DDDDDDDDD SSSSSSSSS
pub fn tjns(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TJNS D, {#}S**  is not implemented yet!");
    // return .next;
}

/// TJNZ D, {#}S**
/// EEEE 1011100 11I DDDDDDDDD SSSSSSSSS
pub fn tjnz(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TJNZ D, {#}S**  is not implemented yet!");
    // return .next;
}

/// TJS D, {#}S**
/// EEEE 1011101 10I DDDDDDDDD SSSSSSSSS
pub fn tjs(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TJS D, {#}S**  is not implemented yet!");
    // return .next;
}

/// TJV D, {#}S**
/// EEEE 1011110 00I DDDDDDDDD SSSSSSSSS
pub fn tjv(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TJV D, {#}S**  is not implemented yet!");
    // return .next;
}

/// TJZ D, {#}S**
/// EEEE 1011100 10I DDDDDDDDD SSSSSSSSS
pub fn tjz(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TJZ D, {#}S**  is not implemented yet!");
    // return .next;
}

/// TRGINT1
/// EEEE 1101011 000 000100010 000100100
pub fn trgint1(cog: *Cog, args: encoding.NoOperands) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TRGINT1  is not implemented yet!");
    // return .next;
}

/// TRGINT2
/// EEEE 1101011 000 000100011 000100100
pub fn trgint2(cog: *Cog, args: encoding.NoOperands) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TRGINT2  is not implemented yet!");
    // return .next;
}

/// TRGINT3
/// EEEE 1101011 000 000100100 000100100
pub fn trgint3(cog: *Cog, args: encoding.NoOperands) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("TRGINT3  is not implemented yet!");
    // return .next;
}

/// WAITATN {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000011110 000100100
pub fn waitatn(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITATN {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WAITCT1 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000010001 000100100
pub fn waitct1(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITCT1 {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WAITCT2 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000010010 000100100
pub fn waitct2(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITCT2 {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WAITCT3 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000010011 000100100
pub fn waitct3(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITCT3 {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WAITFBW {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000011001 000100100
pub fn waitfbw(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITFBW {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WAITINT {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000010000 000100100
pub fn waitint(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITINT {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WAITPAT {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000011000 000100100
pub fn waitpat(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITPAT {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WAITSE1 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000010100 000100100
pub fn waitse1(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITSE1 {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WAITSE2 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000010101 000100100
pub fn waitse2(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITSE2 {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WAITSE3 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000010110 000100100
pub fn waitse3(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITSE3 {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WAITSE4 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000010111 000100100
pub fn waitse4(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITSE4 {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WAITX {#}D {WC/WZ/WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 000011111
pub fn waitx(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITX {#}D {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WAITXFI {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000011011 000100100
pub fn waitxfi(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITXFI {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WAITXMT {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000011010 000100100
pub fn waitxmt(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITXMT {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WAITXRL {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000011101 000100100
pub fn waitxrl(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITXRL {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WAITXRO {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000011100 000100100
pub fn waitxro(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WAITXRO {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// WFBYTE {#}D
/// EEEE 1101011 00L DDDDDDDDD 000010101
pub fn wfbyte(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WFBYTE {#}D  is not implemented yet!");
    // return .next;
}

/// WFLONG {#}D
/// EEEE 1101011 00L DDDDDDDDD 000010111
pub fn wflong(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WFLONG {#}D  is not implemented yet!");
    // return .next;
}

/// WFWORD {#}D
/// EEEE 1101011 00L DDDDDDDDD 000010110
pub fn wfword(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WFWORD {#}D  is not implemented yet!");
    // return .next;
}

/// WMLONG D, {#}S/P
/// EEEE 1010011 11I DDDDDDDDD SSSSSSSSS
pub fn wmlong(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WMLONG D, {#}S/P  is not implemented yet!");
    // return .next;
}

/// WRBYTE {#}D, {#}S/P
/// EEEE 1100010 0LI DDDDDDDDD SSSSSSSSS
pub fn wrbyte(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WRBYTE {#}D, {#}S/P  is not implemented yet!");
    // return .next;
}

/// WRC D
/// EEEE 1101011 000 DDDDDDDDD 001101100
pub fn wrc(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WRC D  is not implemented yet!");
    // return .next;
}

/// WRFAST {#}D, {#}S
/// EEEE 1100100 0LI DDDDDDDDD SSSSSSSSS
pub fn wrfast(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WRFAST {#}D, {#}S  is not implemented yet!");
    // return .next;
}

/// WRLONG {#}D, {#}S/P
/// EEEE 1100011 0LI DDDDDDDDD SSSSSSSSS
pub fn wrlong(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WRLONG {#}D, {#}S/P  is not implemented yet!");
    // return .next;
}

/// WRLUT {#}D, {#}S/P
/// EEEE 1100001 1LI DDDDDDDDD SSSSSSSSS
pub fn wrlut(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WRLUT {#}D, {#}S/P  is not implemented yet!");
    // return .next;
}

/// WRNC D
/// EEEE 1101011 000 DDDDDDDDD 001101101
pub fn wrnc(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WRNC D  is not implemented yet!");
    // return .next;
}

/// WRNZ D
/// EEEE 1101011 000 DDDDDDDDD 001101111
pub fn wrnz(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WRNZ D  is not implemented yet!");
    // return .next;
}

/// WRPIN {#}D, {#}S
/// EEEE 1100000 0LI DDDDDDDDD SSSSSSSSS
pub fn wrpin(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WRPIN {#}D, {#}S  is not implemented yet!");
    // return .next;
}

/// WRWORD {#}D, {#}S/P
/// EEEE 1100010 1LI DDDDDDDDD SSSSSSSSS
pub fn wrword(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WRWORD {#}D, {#}S/P  is not implemented yet!");
    // return .next;
}

/// WRZ D
/// EEEE 1101011 000 DDDDDDDDD 001101110
pub fn wrz(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WRZ D  is not implemented yet!");
    // return .next;
}

/// WXPIN {#}D, {#}S
/// EEEE 1100000 1LI DDDDDDDDD SSSSSSSSS
pub fn wxpin(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WXPIN {#}D, {#}S  is not implemented yet!");
    // return .next;
}

/// WYPIN {#}D, {#}S
/// EEEE 1100001 0LI DDDDDDDDD SSSSSSSSS
pub fn wypin(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("WYPIN {#}D, {#}S  is not implemented yet!");
    // return .next;
}

/// XCONT {#}D, {#}S
/// EEEE 1100110 0LI DDDDDDDDD SSSSSSSSS
pub fn xcont(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("XCONT {#}D, {#}S  is not implemented yet!");
    // return .next;
}

/// XINIT {#}D, {#}S
/// EEEE 1100101 0LI DDDDDDDDD SSSSSSSSS
pub fn xinit(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("XINIT {#}D, {#}S  is not implemented yet!");
    // return .next;
}

/// XOR D, {#}S {WC/WZ/WCZ}
/// EEEE 0101011 CZI DDDDDDDDD SSSSSSSSS
pub fn xor(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("XOR D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}

/// XORO32 D
/// EEEE 1101011 000 DDDDDDDDD 001101000
pub fn xoro32(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("XORO32 D  is not implemented yet!");
    // return .next;
}

/// XZERO {#}D, {#}S
/// EEEE 1100101 1LI DDDDDDDDD SSSSSSSSS
pub fn xzero(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("XZERO {#}D, {#}S  is not implemented yet!");
    // return .next;
}

/// ZEROX D, {#}S {WC/WZ/WCZ}
/// EEEE 0111010 CZI DDDDDDDDD SSSSSSSSS
pub fn zerox(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    _ = cog;
    _ = args;
    @panic("ZEROX D, {#}S {WC/WZ/WCZ} is not implemented yet!");
    // return .next;
}
