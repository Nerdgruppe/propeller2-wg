const std = @import("std");

const sema = @import("../../sema.zig");

const Instruction = sema.EncodedInstruction;
const Slot = Instruction.Slot;
const Operand = Instruction.Operand;

/// Instruction Groups:
/// ```
///       1 ____ _______ ___ _________ _________
///      17 EEEE _______ ___ _________ _________
///       1 EEEE _______ C__ _cccc____ _________
///       2 EEEE _______ C__ DDDDDDDDD _________
///       2 EEEE _______ C_I DDDDDDDDD SSSSSSSSS
///       3 EEEE _______ C_L DDDDDDDDD _________
///       1 EEEE _______ CLI DDDDDDDDD SSSSSSSSS
///      34 EEEE _______ CZ_ _________ _________
///       1 EEEE _______ CZ_ _cccczzzz _________
///      18 EEEE _______ CZ_ DDDDDDDDD _________
///      77 EEEE _______ CZI DDDDDDDDD SSSSSSSSS
///      41 EEEE _______ CZL DDDDDDDDD _________
///      17 EEEE _______ ___ DDDDDDDDD _________
///      43 EEEE _______ __I DDDDDDDDD SSSSSSSSS
///      33 EEEE _______ __I _________ SSSSSSSSS
///      36 EEEE _______ __L DDDDDDDDD _________
///       2 EEEE _______ _L_ DDDDDDDDD _________
///      25 EEEE _______ _LI DDDDDDDDD SSSSSSSSS
///       3 EEEE _______ _NI DDDDDDDDD SSSSSSSSS
///       3 EEEE _______ NNI DDDDDDDDD SSSSSSSSS
///       3 EEEE ______N NNI DDDDDDDDD SSSSSSSSS
///       2 EEEE _____nn nnn nnnnnnnnn nnnnnnnnn
///       4 EEEE _______ RAA AAAAAAAAA AAAAAAAAA
///       2 EEEE _____WW RAA AAAAAAAAA AAAAAAAAA
///       4 EEEE _______ _ZI DDDDDDDDD SSSSSSSSS
///       1 EEEE _______ _Z_ _____zzzz _________
/// ```
///
const slots = struct {
    const E: Slot = slot_from_str("EEEE _______ ___ _________ _________");
    const n: Slot = slot_from_str("____ _____nn nnn nnnnnnnnn nnnnnnnnn");
    const C: Slot = slot_from_str("____ _______ C__ _________ _________");
    const Z: Slot = slot_from_str("____ _______ _Z_ _________ _________");
    const I: Slot = slot_from_str("____ _______ __I _________ _________");
    const Lh: Slot = slot_from_str("____ _______ _L_ _________ _________");
    const Ll: Slot = slot_from_str("____ _______ __L _________ _________");
    const D: Slot = slot_from_str("____ _______ ___ DDDDDDDDD _________");
    const c: Slot = slot_from_str("____ _______ ___ _cccc____ _________");
    const z: Slot = slot_from_str("____ _______ ___ _____zzzz _________");
    const S: Slot = slot_from_str("____ _______ ___ _________ SSSSSSSSS");

    const N3: Slot = slot_from_str("____ ______N NN_ _________ _________");
    const N2: Slot = slot_from_str("____ _______ NN_ _________ _________");
    const N1: Slot = slot_from_str("____ _______ _N_ _________ _________");

    const W: Slot = slot_from_str("____ _____WW ___ _________ _________");
    const R: Slot = slot_from_str("____ _______ R__ _________ _________");
    const A: Slot = slot_from_str("____ _______ _AA AAAAAAAAA AAAAAAAAA");
};

const limits = struct {
    const imm512: Operand.Type = .{ .immediate = 512 };
    const immreg512: Operand.Type = .{ .reg_or_imm = 512 };
    const aug: Operand.Type = .{ .immediate = (1 << 23) };
    const modcz: Operand.Type = .{ .enumeration = modcz_enum };
};

const flags = struct {
    const p2_std: Instruction.Flags = Instruction.Flags.none
        .add(.wz)
        .add(.wc)
        .add(.wcz);
    const cztest: Instruction.Flags = Instruction.Flags.none
        .add(.wz)
        .add(.wc)
        .add(.and_c)
        .add(.and_z)
        .add(.or_c)
        .add(.or_z)
        .add(.xor_c)
        .add(.xor_z);
    const wcz: Instruction.Flags = Instruction.Flags.none.add(.wcz);
    const wc: Instruction.Flags = Instruction.Flags.none.add(.wc);
    const wz: Instruction.Flags = Instruction.Flags.none.add(.wz);
    const jmp: Instruction.Flags = p2_std.add(.wr);
};

const opspec_lut = struct {
    const c: Operand = .init(limits.modcz, slots.c);
    const z: Operand = .init(limits.modcz, slots.z);
    const S: Operand = .init(.register, slots.S);
    const @"{#}S": Operand = .init(limits.immreg512, slots.S);
    const @"#S": Operand = .init(limits.imm512, slots.S);
    const @"{#}S/P": Operand = .init(.pointer_expr, slots.S);
    const D: Operand = .init(.register, slots.D);
    const @"{#}D": Operand = .init(limits.immreg512, slots.D);
    const @"#D": Operand = .init(limits.imm512, slots.D);
    const @"#n": Operand = .init(limits.aug, slots.n);
    const @"#N1": Operand = .init(.{ .immediate = 2 }, slots.N1);
    const @"#N2": Operand = .init(.{ .immediate = 4 }, slots.N2);
    const @"#N3": Operand = .init(.{ .immediate = 8 }, slots.N3);
    const @"#{\\}A": Operand = .init(.address, slots.A);
    const @"PA/PB/PTRA/PTRB": Operand = .init(.pointer_reg, slots.W);
};

fn instr(name: []const u8, binary: u32, opspec: []const u8, flagset: Instruction.Flags) Instruction {
    @setEvalBranchQuota(20_000);
    var ops_array: []const Operand = &.{};

    var iter = std.mem.tokenizeScalar(u8, opspec, ',');
    while (iter.next()) |word| {
        const def = std.mem.trim(u8, word, " ");

        const op = @field(opspec_lut, def);

        ops_array = ops_array ++ &[_]Operand{op};
    }

    return .{
        .mnemonic = name,
        .binary = binary,
        .operands = ops_array,
        .flags = flagset,
    };
}

pub const p2_instructions: []const Instruction = &.{
    instr("nop", 0x00000000, "", .none),
    instr("ror", 0x00000000, "D, {#}S", flags.p2_std),
    instr("rol", 0x00200000, "D, {#}S", flags.p2_std),
    instr("shr", 0x00400000, "D, {#}S", flags.p2_std),
    instr("shl", 0x00600000, "D, {#}S", flags.p2_std),
    instr("rcr", 0x00800000, "D, {#}S", flags.p2_std),
    instr("rcl", 0x00a00000, "D, {#}S", flags.p2_std),
    instr("sar", 0x00c00000, "D, {#}S", flags.p2_std),
    instr("sal", 0x00e00000, "D, {#}S", flags.p2_std),
    instr("add", 0x01000000, "D, {#}S", flags.p2_std),
    instr("addx", 0x01200000, "D, {#}S", flags.p2_std),
    instr("adds", 0x01400000, "D, {#}S", flags.p2_std),
    instr("addsx", 0x01600000, "D, {#}S", flags.p2_std),
    instr("sub", 0x01800000, "D, {#}S", flags.p2_std),
    instr("subx", 0x01a00000, "D, {#}S", flags.p2_std),
    instr("subs", 0x01c00000, "D, {#}S", flags.p2_std),
    instr("subsx", 0x01e00000, "D, {#}S", flags.p2_std),

    instr("cmp", 0x02000000, "D, {#}S", flags.p2_std.with(.wcz_not_used, .warn)),
    instr("cmpx", 0x02200000, "D, {#}S", flags.p2_std.with(.wcz_not_used, .warn)),
    instr("cmps", 0x02400000, "D, {#}S", flags.p2_std.with(.wcz_not_used, .warn)),
    instr("cmpsx", 0x02600000, "D, {#}S", flags.p2_std.with(.wcz_not_used, .warn)),
    instr("cmpr", 0x02800000, "D, {#}S", flags.p2_std.with(.wcz_not_used, .warn)),
    instr("cmpm", 0x02a00000, "D, {#}S", flags.p2_std.with(.wcz_not_used, .warn)),
    instr("subr", 0x02c00000, "D, {#}S", flags.p2_std),
    instr("cmpsub", 0x02e00000, "D, {#}S", flags.p2_std),

    instr("fge", 0x03000000, "D, {#}S", flags.p2_std),
    instr("fle", 0x03200000, "D, {#}S", flags.p2_std),
    instr("fges", 0x03400000, "D, {#}S", flags.p2_std),
    instr("fles", 0x03600000, "D, {#}S", flags.p2_std),

    instr("sumc", 0x03800000, "D, {#}S", flags.p2_std),
    instr("sumnc", 0x03a00000, "D, {#}S", flags.p2_std),
    instr("sumz", 0x03c00000, "D, {#}S", flags.p2_std),
    instr("sumnz", 0x03e00000, "D, {#}S", flags.p2_std),

    instr("testb", 0x04000000, "D, {#}S", flags.cztest),
    instr("testbn", 0x04200000, "D, {#}S", flags.cztest),

    instr("bitl", 0x04000000, "D, {#}S", flags.wcz),
    instr("bith", 0x04200000, "D, {#}S", flags.wcz),
    instr("bitc", 0x04400000, "D, {#}S", flags.wcz),
    instr("bitnc", 0x04600000, "D, {#}S", flags.wcz),
    instr("bitz", 0x04800000, "D, {#}S", flags.wcz),
    instr("bitnz", 0x04a00000, "D, {#}S", flags.wcz),
    instr("bitrnd", 0x04c00000, "D, {#}S", flags.wcz),
    instr("bitnot", 0x04e00000, "D, {#}S", flags.wcz),

    instr("and", 0x05000000, "D, {#}S", flags.p2_std),
    instr("andn", 0x05200000, "D, {#}S", flags.p2_std),
    instr("or", 0x05400000, "D, {#}S", flags.p2_std),
    instr("xor", 0x05600000, "D, {#}S", flags.p2_std),
    instr("muxc", 0x05800000, "D, {#}S", flags.p2_std),
    instr("muxnc", 0x05a00000, "D, {#}S", flags.p2_std),
    instr("muxz", 0x05c00000, "D, {#}S", flags.p2_std),
    instr("muxnz", 0x05e00000, "D, {#}S", flags.p2_std),

    instr("mov", 0x06000000, "D, {#}S", flags.p2_std),
    instr("not", 0x06200000, "D, {#}S", flags.p2_std),
    instr("not", 0x06200000, "D", flags.p2_std),
    instr("abs", 0x06400000, "D, {#}S", flags.p2_std),
    instr("abs", 0x06400000, "D", flags.p2_std),
    instr("neg", 0x06600000, "D, {#}S", flags.p2_std),
    instr("neg", 0x06600000, "D", flags.p2_std),
    instr("negc", 0x06800000, "D, {#}S", flags.p2_std),
    instr("negc", 0x06800000, "D", flags.p2_std),
    instr("negnc", 0x06a00000, "D, {#}S", flags.p2_std),
    instr("negnc", 0x06a00000, "D", flags.p2_std),
    instr("negz", 0x06c00000, "D, {#}S", flags.p2_std),
    instr("negz", 0x06c00000, "D", flags.p2_std),
    instr("negnz", 0x06e00000, "D, {#}S", flags.p2_std),
    instr("negnz", 0x06e00000, "D", flags.p2_std),

    instr("incmod", 0x07000000, "D, {#}S", flags.p2_std),
    instr("decmod", 0x07200000, "D, {#}S", flags.p2_std),
    instr("zerox", 0x07400000, "D, {#}S", flags.p2_std),
    instr("signx", 0x07600000, "D, {#}S", flags.p2_std),

    instr("encod", 0x07800000, "D, {#}S", flags.p2_std),
    instr("encod", 0x07800000, "D", flags.p2_std),
    instr("ones", 0x07a00000, "D, {#}S", flags.p2_std),
    instr("ones", 0x07a00000, "D", flags.p2_std),

    instr("test", 0x07c00000, "D, {#}S", flags.p2_std.with(.wcz_not_used, .warn)),
    instr("test", 0x07c00000, "D", flags.p2_std.with(.wcz_not_used, .warn)),
    instr("testn", 0x07e00000, "D, {#}S", flags.p2_std.with(.wcz_not_used, .warn)),

    instr("setnib", 0x08000000, "D, {#}S, #N3", .none),
    instr("setnib", 0x08000000, "D", .none),
    instr("getnib", 0x08400000, "D, {#}S, #N3", .none),
    instr("getnib", 0x08400000, "D", .none),
    instr("rolnib", 0x08800000, "D, {#}S, #N3", .none),
    instr("rolnib", 0x08800000, "D", .none),
    instr("setbyte", 0x08c00000, "D, {#}S, #N2", .none),
    instr("setbyte", 0x08c00000, "D", .none),
    instr("getbyte", 0x08e00000, "D, {#}S, #N2", .none),
    instr("getbyte", 0x08e00000, "D", .none),
    instr("rolbyte", 0x09000000, "D, {#}S, #N2", .none),
    instr("rolbyte", 0x09000000, "D", .none),
    instr("setword", 0x09200000, "D, {#}S, #N1", .none),
    instr("setword", 0x09200000, "D", .none),
    instr("getword", 0x09300000, "D, {#}S, #N1", .none),
    instr("getword", 0x09300000, "D", .none),
    instr("rolword", 0x09400000, "D, {#}S, #N1", .none),
    instr("rolword", 0x09400000, "D", .none),

    instr("altsn", 0x09500000, "D, {#}S", .none),
    instr("altsn", 0x09500000, "D", .none),
    instr("altgn", 0x09580000, "D, {#}S", .none),
    instr("altgn", 0x09580000, "D", .none),
    instr("altsb", 0x09600000, "D, {#}S", .none),
    instr("altsb", 0x09600000, "D", .none),
    instr("altgb", 0x09680000, "D, {#}S", .none),
    instr("altgb", 0x09680000, "D", .none),
    instr("altsw", 0x09700000, "D, {#}S", .none),
    instr("altsw", 0x09700000, "D", .none),
    instr("altgw", 0x09780000, "D, {#}S", .none),
    instr("altgw", 0x09780000, "D", .none),
    instr("altr", 0x09800000, "D, {#}S", .none),
    instr("altr", 0x09800000, "D", .none),
    instr("altd", 0x09880000, "D, {#}S", .none),
    instr("altd", 0x09880000, "D", .none),
    instr("alts", 0x09900000, "D, {#}S", .none),
    instr("alts", 0x09900000, "D", .none),
    instr("altb", 0x09980000, "D, {#}S", .none),
    instr("altb", 0x09980000, "D", .none),
    instr("alti", 0x09a00000, "D, {#}S", .none),
    instr("alti", 0x09a00000, "D", .none),
    instr("setr", 0x09a80000, "D, {#}S", .none),
    instr("setd", 0x09b00000, "D, {#}S", .none),
    instr("sets", 0x09b80000, "D, {#}S", .none),

    instr("decod", 0x09c00000, "D, {#}S", .none),
    instr("decod", 0x09c00000, "D", .none),
    instr("bmask", 0x09c80000, "D, {#}S", .none),
    instr("bmask", 0x09c80000, "D", .none),

    instr("crcbit", 0x09d00000, "D, {#}S", .none),
    instr("crcnib", 0x09d80000, "D, {#}S", .none),

    instr("muxnits", 0x09e00000, "D, {#}S", .none),
    instr("muxnibs", 0x09e80000, "D, {#}S", .none),
    instr("muxq", 0x09f00000, "D, {#}S", .none),
    instr("movbyts", 0x09f80000, "D, {#}S", .none),

    instr("mul", 0x0a000000, "D, {#}S", flags.wz),
    instr("muls", 0x0a100000, "D, {#}S", flags.wz),
    instr("sca", 0x0a200000, "D, {#}S", flags.wz),
    instr("scas", 0x0a300000, "D, {#}S", flags.wz),

    instr("addpix", 0x0a400000, "D, {#}S", .none),
    instr("mulpix", 0x0a480000, "D, {#}S", .none),
    instr("blnpix", 0x0a500000, "D, {#}S", .none),
    instr("mixpix", 0x0a580000, "D, {#}S", .none),

    instr("addct1", 0x0a600000, "D, {#}S", .none),
    instr("addct2", 0x0a680000, "D, {#}S", .none),
    instr("addct3", 0x0a700000, "D, {#}S", .none),
    instr("wmlong", 0x0a780000, "D, {#}S/P", .none),

    instr("rqpin", 0x0a800000, "D, {#}S", flags.wc),
    instr("rdpin", 0x0a880000, "D, {#}S", flags.wc),

    instr("rdlut", 0x0aa00000, "D, {#}S/P", flags.p2_std),
    instr("rdbyte", 0x0ac00000, "D, {#}S/P", flags.p2_std),
    instr("rdword", 0x0ae00000, "D, {#}S/P", flags.p2_std),
    instr("rdlong", 0x0b000000, "D, {#}S/P", flags.p2_std),

    // some aliases from rdlong x, ++ptra
    instr("popa", 0x0b00015f, "D", flags.p2_std),
    instr("popb", 0x0b0001df, "D", flags.p2_std),

    instr("calld", 0x0b200000, "D, {#}S", flags.jmp),
    instr("reti0", 0x0b3bffff, "", flags.p2_std),
    instr("reti1", 0x0b3bfff5, "", flags.p2_std),
    instr("reti2", 0x0b3bfff3, "", flags.p2_std),
    instr("reti3", 0x0b3bfff1, "", flags.p2_std),
    instr("resi0", 0x0b3bfdff, "", flags.p2_std),
    instr("resi1", 0x0b3be9f5, "", flags.p2_std),
    instr("resi2", 0x0b3be5f3, "", flags.p2_std),
    instr("resi3", 0x0b3be1f1, "", flags.p2_std),

    instr("callpa", 0x0b400000, "{#}D, {#}S", .none),
    instr("callpb", 0x0b500000, "{#}D, {#}S", .none),

    instr("djz", 0x0b600000, "D, {#}S", .none),
    instr("djnz", 0x0b680000, "D, {#}S", .none),
    instr("djf", 0x0b700000, "D, {#}S", .none),
    instr("djnf", 0x0b780000, "D, {#}S", .none),
    instr("ijz", 0x0b800000, "D, {#}S", .none),
    instr("ijnz", 0x0b880000, "D, {#}S", .none),
    instr("tjz", 0x0b900000, "D, {#}S", .none),
    instr("tjnz", 0x0b980000, "D, {#}S", .none),
    instr("tjf", 0x0ba00000, "D, {#}S", .none),
    instr("tjnf", 0x0ba80000, "D, {#}S", .none),
    instr("tjs", 0x0bb00000, "D, {#}S", .none),
    instr("tjns", 0x0bb80000, "D, {#}S", .none),
    instr("tjv", 0x0bc00000, "D, {#}S", .none),

    //  { "jp",     0x0ba00000, P2_TJZ_OPERANDS },
    //  { "jnp",    0x0bb00000, P2_TJZ_OPERANDS },

    instr("jint", 0x0bc80000, "{#}S", .none),
    instr("jct1", 0x0bc80200, "{#}S", .none),
    instr("jct2", 0x0bc80400, "{#}S", .none),
    instr("jct3", 0x0bc80600, "{#}S", .none),
    instr("jse1", 0x0bc80800, "{#}S", .none),
    instr("jse2", 0x0bc80a00, "{#}S", .none),
    instr("jse3", 0x0bc80c00, "{#}S", .none),
    instr("jse4", 0x0bc80e00, "{#}S", .none),
    instr("jpat", 0x0bc81000, "{#}S", .none),
    instr("jfbw", 0x0bc81200, "{#}S", .none),
    instr("jxmt", 0x0bc81400, "{#}S", .none),
    instr("jxfi", 0x0bc81600, "{#}S", .none),
    instr("jxro", 0x0bc81800, "{#}S", .none),
    instr("jxrl", 0x0bc81a00, "{#}S", .none),
    instr("jatn", 0x0bc81c00, "{#}S", .none),
    instr("jqmt", 0x0bc81e00, "{#}S", .none),

    instr("jnint", 0x0bc82000, "{#}S", .none),
    instr("jnct1", 0x0bc82200, "{#}S", .none),
    instr("jnct2", 0x0bc82400, "{#}S", .none),
    instr("jnct3", 0x0bc82600, "{#}S", .none),
    instr("jnse1", 0x0bc82800, "{#}S", .none),
    instr("jnse2", 0x0bc82a00, "{#}S", .none),
    instr("jnse3", 0x0bc82c00, "{#}S", .none),
    instr("jnse4", 0x0bc82e00, "{#}S", .none),
    instr("jnpat", 0x0bc83000, "{#}S", .none),
    instr("jnfbw", 0x0bc83200, "{#}S", .none),
    instr("jnxmt", 0x0bc83400, "{#}S", .none),
    instr("jnxfi", 0x0bc83600, "{#}S", .none),
    instr("jnxro", 0x0bc83800, "{#}S", .none),
    instr("jnxrl", 0x0bc83a00, "{#}S", .none),
    instr("jnatn", 0x0bc83c00, "{#}S", .none),
    instr("jnqmt", 0x0bc83e00, "{#}S", .none),

    instr("setpat", 0x0bf00000, "D, {#}S", .none),
    instr("wrpin", 0x0c000000, "D, {#}S", .none),
    instr("akpin", 0x0c080200, "{#}S", .none),
    instr("wxpin", 0x0c100000, "D, {#}S", .none),
    instr("wypin", 0x0c200000, "D, {#}S", .none),

    instr("wrlut", 0x0c300000, "{#}D, {#}S/P", .none),
    instr("wrbyte", 0x0c400000, "{#}D, {#}S/P", .none),
    instr("wrword", 0x0c500000, "{#}D, {#}S/P", .none),
    instr("wrlong", 0x0c600000, "{#}D, {#}S/P", .none),

    //  some aliases for wrlong x, ptra++ et),
    instr("pusha", 0x0c600161, "{#}D", .none),
    instr("pushb", 0x0c6001e1, "{#}D", .none),

    instr("rdfast", 0x0c700000, "D, {#}S", .none),
    instr("wrfast", 0x0c800000, "D, {#}S", .none),
    instr("fblock", 0x0c900000, "D, {#}S", .none),

    instr("xinit", 0x0ca00000, "D, {#}S", .none),
    instr("xstop", 0x0cac0000, "", .none),
    instr("xzero", 0x0cb00000, "D, {#}S", .none),
    instr("xcont", 0x0cc00000, "D, {#}S", .none),

    instr("rep", 0x0cd00000, "D, {#}S", .none),

    instr("coginit", 0x0ce00000, "D, {#}S", flags.wc),
    instr("qmul", 0x0d000000, "D, {#}S", .none),
    instr("qdiv", 0x0d100000, "D, {#}S", .none),
    instr("qfrac", 0x0d200000, "D, {#}S", .none),
    instr("qsqrt", 0x0d300000, "D, {#}S", .none),
    instr("qrotate", 0x0d400000, "D, {#}S", .none),
    instr("qvector", 0x0d500000, "D, {#}S", .none),

    instr("hubset", 0x0d600000, "{#}D", .none),
    instr("cogid", 0x0d600001, "{#}D", flags.wc),
    instr("cogstop", 0x0d600003, "{#}D", .none),
    instr("locknew", 0x0d600004, "D", flags.wc),
    instr("lockret", 0x0d600005, "{#}D", .none),
    instr("locktry", 0x0d600006, "{#}D", flags.wc),
    instr("lockrel", 0x0d600007, "{#}D", flags.wc),

    instr("qlog", 0x0d60000e, "{#}D", .none),
    instr("qexp", 0x0d60000f, "{#}D", .none),

    instr("rfbyte", 0x0d600010, "D", flags.p2_std),
    instr("rfword", 0x0d600011, "D", flags.p2_std),
    instr("rflong", 0x0d600012, "D", flags.p2_std),
    instr("rfvar", 0x0d600013, "D", flags.p2_std),
    instr("rfvars", 0x0d600014, "D", flags.p2_std),

    instr("wfbyte", 0x0d600015, "{#}D", .none),
    instr("wfword", 0x0d600016, "{#}D", .none),
    instr("wflong", 0x0d600017, "{#}D", .none),

    instr("getqx", 0x0d600018, "D", flags.p2_std),
    instr("getqy", 0x0d600019, "D", flags.p2_std),
    instr("getct", 0x0d60001a, "D", flags.wc),
    instr("getrnd", 0x0d60001b, "{#}D", flags.p2_std),

    instr("setdacs", 0x0d60001c, "{#}D", .none),
    instr("setxfrq", 0x0d60001d, "{#}D", .none),
    instr("getxacc", 0x0d60001e, "D", .none),
    instr("waitx", 0x0d60001f, "{#}D", flags.p2_std),

    instr("setse1", 0x0d600020, "{#}D", .none),
    instr("setse2", 0x0d600021, "{#}D", .none),
    instr("setse3", 0x0d600022, "{#}D", .none),
    instr("setse4", 0x0d600023, "{#}D", .none),

    instr("pollint", 0x0d600024, "", flags.p2_std),
    instr("pollct1", 0x0d600224, "", flags.p2_std),
    instr("pollct2", 0x0d600424, "", flags.p2_std),
    instr("pollct3", 0x0d600624, "", flags.p2_std),
    instr("pollse1", 0x0d600824, "", flags.p2_std),
    instr("pollse2", 0x0d600a24, "", flags.p2_std),
    instr("pollse3", 0x0d600c24, "", flags.p2_std),
    instr("pollse4", 0x0d600e24, "", flags.p2_std),
    instr("pollpat", 0x0d601024, "", flags.p2_std),
    instr("pollfbw", 0x0d601224, "", flags.p2_std),
    instr("pollxmt", 0x0d601424, "", flags.p2_std),
    instr("pollxfi", 0x0d601624, "", flags.p2_std),
    instr("pollxro", 0x0d601824, "", flags.p2_std),
    instr("pollxrl", 0x0d601a24, "", flags.p2_std),
    instr("pollatn", 0x0d601c24, "", flags.p2_std),
    instr("pollqmt", 0x0d601e24, "", flags.p2_std),

    instr("waitint", 0x0d602024, "", flags.p2_std),
    instr("waitct1", 0x0d602224, "", flags.p2_std),
    instr("waitct2", 0x0d602424, "", flags.p2_std),
    instr("waitct3", 0x0d602624, "", flags.p2_std),
    instr("waitse1", 0x0d602824, "", flags.p2_std),
    instr("waitse2", 0x0d602a24, "", flags.p2_std),
    instr("waitse3", 0x0d602c24, "", flags.p2_std),
    instr("waitse4", 0x0d602e24, "", flags.p2_std),
    instr("waitpat", 0x0d603024, "", flags.p2_std),
    instr("waitfbw", 0x0d603224, "", flags.p2_std),
    instr("waitxmt", 0x0d603424, "", flags.p2_std),
    instr("waitxfi", 0x0d603624, "", flags.p2_std),
    instr("waitxro", 0x0d603824, "", flags.p2_std),
    instr("waitxrl", 0x0d603a24, "", flags.p2_std),
    instr("waitatn", 0x0d603c24, "", flags.p2_std),

    instr("allowi", 0x0d604024, "", .none),
    instr("stalli", 0x0d604224, "", .none),
    instr("trgint1", 0x0d604424, "", .none),
    instr("trgint2", 0x0d604624, "", .none),
    instr("trgint3", 0x0d604824, "", .none),

    instr("nixint1", 0x0d604a24, "", .none),
    instr("nixint2", 0x0d604c24, "", .none),
    instr("nixint3", 0x0d604e24, "", .none),

    instr("setint1", 0x0d600025, "{#}D", .none),
    instr("setint2", 0x0d600026, "{#}D", .none),
    instr("setint3", 0x0d600027, "{#}D", .none),

    instr("setq", 0x0d600028, "{#}D", .none),
    instr("setq2", 0x0d600029, "{#}D", .none),

    instr("push", 0x0d60002a, "{#}D", .none),
    instr("pop", 0x0d60002b, "D", flags.p2_std),

    // indirect jumps via register
    // normally the user will write "jmp x" and the assembler will
    // recognize if x is a register and rewrite it as "jmp.ind x"
    instr("jmp", 0x0d60002c, "D", flags.p2_std),
    instr("call", 0x0d60002d, "D", flags.p2_std),
    instr("calla", 0x0d60002e, "D", flags.p2_std),
    instr("callb", 0x0d60002f, "D", flags.p2_std),
    instr("ret", 0x0d64002d, "", flags.p2_std),
    instr("reta", 0x0d64002e, "", flags.p2_std),
    instr("retb", 0x0d64002f, "", flags.p2_std),

    instr("jmprel", 0x0d600030, "{#}D", .none),

    instr("skip", 0x0d600031, "{#}D", .none),
    instr("skipf", 0x0d600032, "{#}D", .none),
    instr("execf", 0x0d600033, "{#}D", .none),

    instr("getptr", 0x0d600034, "D", .none),
    instr("getbrk", 0x0d600035, "D", flags.p2_std.with(.wcz_not_used, .err)),
    instr("cogbrk", 0x0d600035, "{#}D", .none),
    instr("brk", 0x0d600036, "{#}D", .none),
    instr("setluts", 0x0d600037, "{#}D", .none),
    instr("lutsoff", 0x0d640037, "", .none),
    instr("lutson", 0x0d640237, "", .none),

    instr("setcy", 0x0d600038, "{#}D", .none),
    instr("setci", 0x0d600039, "{#}D", .none),
    instr("setcq", 0x0d60003a, "{#}D", .none),
    instr("setcfrq", 0x0d60003b, "{#}D", .none),
    instr("setcmod", 0x0d60003c, "{#}D", .none),
    instr("setpiv", 0x0d60003d, "{#}D", .none),
    instr("setpix", 0x0d60003e, "{#}D", .none),
    instr("cogatn", 0x0d60003f, "{#}D", .none),

    instr("testp", 0x0d600040, "{#}D", flags.cztest),
    instr("testpn", 0x0d600041, "{#}D", flags.cztest),

    instr("dirl", 0x0d600040, "{#}D", flags.wcz),
    instr("dirh", 0x0d600041, "{#}D", flags.wcz),
    instr("dirc", 0x0d600042, "{#}D", flags.wcz),
    instr("dirnc", 0x0d600043, "{#}D", flags.wcz),
    instr("dirz", 0x0d600044, "{#}D", flags.wcz),
    instr("dirnz", 0x0d600045, "{#}D", flags.wcz),
    instr("dirrnd", 0x0d600046, "{#}D", flags.wcz),
    instr("dirnot", 0x0d600047, "{#}D", flags.wcz),

    instr("outl", 0x0d600048, "{#}D", flags.wcz),
    instr("outh", 0x0d600049, "{#}D", flags.wcz),
    instr("outc", 0x0d60004a, "{#}D", flags.wcz),
    instr("outnc", 0x0d60004b, "{#}D", flags.wcz),
    instr("outz", 0x0d60004c, "{#}D", flags.wcz),
    instr("outnz", 0x0d60004d, "{#}D", flags.wcz),
    instr("outrnd", 0x0d60004e, "{#}D", flags.wcz),
    instr("outnot", 0x0d60004f, "{#}D", flags.wcz),

    instr("fltl", 0x0d600050, "{#}D", flags.wcz),
    instr("flth", 0x0d600051, "{#}D", flags.wcz),
    instr("fltc", 0x0d600052, "{#}D", flags.wcz),
    instr("fltnc", 0x0d600053, "{#}D", flags.wcz),
    instr("fltz", 0x0d600054, "{#}D", flags.wcz),
    instr("fltnz", 0x0d600055, "{#}D", flags.wcz),
    instr("fltrnd", 0x0d600056, "{#}D", flags.wcz),
    instr("fltnot", 0x0d600057, "{#}D", flags.wcz),

    instr("drvl", 0x0d600058, "{#}D", flags.wcz),
    instr("drvh", 0x0d600059, "{#}D", flags.wcz),
    instr("drvc", 0x0d60005a, "{#}D", flags.wcz),
    instr("drvnc", 0x0d60005b, "{#}D", flags.wcz),
    instr("drvz", 0x0d60005c, "{#}D", flags.wcz),
    instr("drvnz", 0x0d60005d, "{#}D", flags.wcz),
    instr("drvrnd", 0x0d60005e, "{#}D", flags.wcz),
    instr("drvnot", 0x0d60005f, "{#}D", flags.wcz),

    instr("splitb", 0x0d600060, "D", .none),
    instr("mergeb", 0x0d600061, "D", .none),
    instr("splitw", 0x0d600062, "D", .none),
    instr("mergew", 0x0d600063, "D", .none),
    instr("seussf", 0x0d600064, "D", .none),
    instr("seussr", 0x0d600065, "D", .none),
    instr("rgbsqz", 0x0d600066, "D", .none),
    instr("rgbexp", 0x0d600067, "D", .none),
    instr("xoro32", 0x0d600068, "D", .none),
    instr("rev", 0x0d600069, "D", .none),

    instr("rczr", 0x0d60006a, "D", flags.p2_std),
    instr("rczl", 0x0d60006b, "D", flags.p2_std),
    instr("wrc", 0x0d60006c, "D", .none),
    instr("wrnc", 0x0d60006d, "D", .none),
    instr("wrz", 0x0d60006e, "D", .none),
    instr("wrnz", 0x0d60006f, "D", .none),
    instr("modcz", 0x0d64006f, "c, z", flags.p2_std.with(.wcz_not_used, .err)),
    instr("modc", 0x0d64006f, "c", flags.wc.with(.wcz_not_used, .err)),
    instr("modz", 0x0d64006f, "z", flags.wz.with(.wcz_not_used, .err)),
    instr("setscp", 0x0d600070, "{#}D", .none),
    instr("getscp", 0x0d600071, "D", .none),

    // long jumps
    instr("jmp", 0x0d800000, "#{\\}A", flags.jmp),
    instr("call", 0x0da00000, "#{\\}A", flags.jmp),
    instr("calla", 0x0dc00000, "#{\\}A", flags.jmp),
    instr("callb", 0x0de00000, "#{\\}A", flags.jmp),

    instr("calld", 0x0e000000, "PA/PB/PTRA/PTRB, #{\\}A", .none),
    instr("loc", 0x0e800000, "PA/PB/PTRA/PTRB, #{\\}A", .none),

    instr("augs", 0x0f000000, "#n", .none),
    instr("augd", 0x0f800000, "#n", .none),
};

/// "____ _______ ___ _________ _________"
fn slot_from_str(comptime str: *const [36:0]u8) Slot {
    if (str[4] != ' ') @compileError("invalid slot spec");
    if (str[12] != ' ') @compileError("invalid slot spec");
    if (str[16] != ' ') @compileError("invalid slot spec");
    if (str[26] != ' ') @compileError("invalid slot spec");

    var slotc: ?u8 = null;

    var first: usize = 255;
    var last: usize = 0;
    for (str, 0..) |c, i| {
        if (c == '_' or c == ' ')
            continue;
        slotc = slotc orelse c;
        if (c != slotc.?)
            @compileError("multiple slot spec characters found");
        first = @min(first, i);
        last = @max(last, i);
    }
    if (first > last)
        @compileError("no slot found");

    // patch the string index
    if (first > 26) first -= 1;
    if (first > 16) first -= 1;
    if (first > 12) first -= 1;
    if (first > 4) first -= 1;

    // patch the string index
    if (last > 26) last -= 1;
    if (last > 16) last -= 1;
    if (last > 12) last -= 1;
    if (last > 4) last -= 1;

    std.debug.assert(first < 32);
    std.debug.assert(last < 32);

    const length = last - first + 1;
    const offset = 31 - last;

    return .{
        .bits = length,
        .shift = offset,
    };
}

const modcz_enum: std.StaticStringMap(u32) = .initComptime(&.{
    .{ "_CLR", 0x0 },
    .{ "_NC_AND_NZ", 0x1 },
    .{ "_NZ_AND_NC", 0x1 },
    .{ "_GT", 0x1 },
    .{ "_NC_AND_Z", 0x2 },
    .{ "_Z_AND_NC", 0x2 },
    .{ "_NC", 0x3 },
    .{ "_GE", 0x3 },
    .{ "_C_AND_NZ", 0x4 },
    .{ "_NZ_AND_C", 0x4 },
    .{ "_NZ", 0x5 },
    .{ "_NE", 0x5 },
    .{ "_C_NE_Z", 0x6 },
    .{ "_Z_NE_C", 0x6 },
    .{ "_NZ_OR_NC", 0x7 },
    .{ "_NC_OR_NZ", 0x7 },

    .{ "_C_AND_Z", 0x8 },
    .{ "_Z_AND_C", 0x8 },
    .{ "_C_EQ_Z", 0x9 },
    .{ "_Z_EQ_C", 0x9 },
    .{ "_Z", 0xA },
    .{ "_E", 0xA },
    .{ "_Z_OR_NC", 0xB },
    .{ "_NC_OR_Z", 0xB },
    .{ "_C", 0xC },
    .{ "_LT", 0xC },
    .{ "_C_OR_NZ", 0xD },
    .{ "_NZ_OR_C", 0xD },
    .{ "_Z_OR_C", 0xE },
    .{ "_C_OR_Z", 0xE },
    .{ "_LE", 0xE },
    .{ "_SET", 0xF },
});
