const std = @import("std");
const logger = std.log.scoped(.execute);

const decode = @import("decode.zig");
const encoding = @import("encoding.zig");
const Cog = @import("Cog.zig");
const alu = @import("p2").alu;

// codegen: begin:runtimehelpers
const EventId = @import("p2").types.EventId;

fn operandD(cog: *Cog, reg: Cog.Register, immediate: bool) u32 {
    return if (immediate) @intFromEnum(reg) | cog.fetch_augd() else cog.read_reg(reg);
}

fn operandS(cog: *Cog, reg: Cog.Register, immediate: bool) u32 {
    const selected = if (cog.current_instruction) |state| state.alt_s orelse reg else reg;
    const source = if (immediate) @intFromEnum(selected) | cog.fetch_augs() else cog.read_reg(selected);
    return if (cog.current_instruction) |state| state.s_value orelse source else source;
}

fn branchA(cog: *Cog, args: encoding.AbsPointer) u20 {
    // Long relative branches encode a signed byte displacement even in cog RAM.
    const in_hub = cog.exec_mode == .hub;
    const displacement: i20 = @bitCast(args.address);
    return if (args.relative) cog.dispatch_pc +% @as(u20, if (in_hub) 4 else 1) +% @as(u20, @bitCast(if (in_hub) displacement else displacement >> 2)) else args.address;
}

fn branchS(cog: *Cog, reg: Cog.Register, immediate: bool) u20 {
    const augmented = cog.augs_pending;
    return branchValue(cog, operandS(cog, reg, immediate), immediate, augmented);
}

fn branchValue(cog: *Cog, value: u32, immediate: bool, augmented: bool) u20 {
    const substituted = if (cog.current_instruction) |state| state.s_value != null else false;
    const raw: u20 = @truncate(value);
    if (!immediate) return raw;
    const displacement: u20 = if (augmented or substituted) raw else @bitCast(@as(i20, @as(i9, @bitCast(@as(u9, @truncate(raw))))));
    const scale: u20 = if (cog.exec_mode == .hub) 4 else 1;
    return cog.dispatch_pc +% scale +% displacement *% scale;
}

fn setFlags(cog: *Cog, args: anytype, value: u32, sign_bit: u5) void {
    if (args.c_mod == .write) cog.c = value & (@as(u32, 1) << sign_bit) != 0;
    if (args.z_mod == .write) cog.z = value == 0;
}

fn jumpFlags(cog: *Cog, args: anytype, value: u32) void {
    if (args.c_mod == .write) cog.c = value >> 31 != 0;
    if (args.z_mod == .write) cog.z = value & 0x4000_0000 != 0;
    cog.jump(@truncate(value));
}

fn pointerReg(pointer: encoding.PointerReg) Cog.Register {
    return switch (pointer) {
        .PA => .PA,
        .PB => .PB,
        .PTRA => .PTRA,
        .PTRB => .PTRB,
    };
}

fn memoryAddress(cog: *Cog, reg: Cog.Register, immediate: bool, scale: u3, block: bool) u32 {
    const augmented = cog.augs_pending;
    const value = operandS(cog, reg, immediate);
    if (!immediate) return value;
    const shift: u5 = if (augmented) 15 else 0;
    if (value & (@as(u32, 0x100) << shift) == 0) return value;
    const pointer: Cog.Register = if (value & (@as(u32, 0x80) << shift) == 0) .PTRA else .PTRB;
    const update = value & (@as(u32, 0x40) << shift) != 0;
    const post = update and value & (@as(u32, 0x20) << shift) != 0;
    var delta: i32 = if (augmented)
        @as(i20, @bitCast(@as(u20, @truncate(value))))
    else if (update)
        @as(i5, @bitCast(@as(u5, @truncate(value))))
    else
        @as(i6, @bitCast(@as(u6, @truncate(value))));
    if (!augmented) {
        if (update and delta == 0) delta = 16;
        delta *= scale;
    }
    if (block and cog.block_pointer_delta) {
        delta = if (!update) 0 else @bitCast((cog.q +% 1) *% 4);
        if (update and value & (@as(u32, 0x10) << shift) != 0) delta = -%delta;
    }
    const base = cog.read_reg(pointer);
    const adjusted = base +% @as(u32, @bitCast(delta));
    if (update) cog.write_reg(pointer, adjusted);
    return if (post) base else adjusted;
}

fn readMemory(cog: *Cog, args: encoding.Both_D_Simm_Flags, size: u3) Cog.ExecResult {
    if (cog.memory_transfer == null) {
        const block = size == 4 and cog.setq_pending;
        const state = cog.current_instruction;
        cog.memory_transfer = .{
            .address = memoryAddress(cog, args.s, args.s_imm, size, block),
            .remaining = if (block) @as(u64, cog.q) + 1 else 1,
            .reg = @intFromEnum(if (state) |instruction| instruction.alt_r orelse args.d else args.d),
            .lut = block and cog.q2,
            .block = block,
            .no_result = if (state) |instruction| instruction.no_result else false,
        };
    }
    const transfer = &cog.memory_transfer.?;
    const value = cog.hub.read_memory(transfer.address, size);
    if (!transfer.no_result) {
        if (transfer.lut) cog.write_lut(transfer.reg, value) else if (transfer.block and transfer.reg >= 504) {
            cog.ram_tail[transfer.reg - 504] = value;
        } else cog.write_reg(@enumFromInt(transfer.reg), value);
    }
    transfer.address +%= size;
    transfer.reg +%= 1;
    transfer.remaining -= 1;
    // ponytail: one long per step; add initial hub latency with cycle-exact scheduling.
    if (transfer.remaining != 0) return .wait;
    cog.memory_transfer = null;
    setFlags(cog, args, value, @intCast(@as(u6, size) * 8 - 1));
    return .next;
}

fn writeMemory(cog: *Cog, args: anytype, size: u3, masked: bool) Cog.ExecResult {
    if (cog.memory_transfer == null) {
        const immediate = if (@hasField(@TypeOf(args), "d_imm")) args.d_imm else false;
        const value = operandD(cog, args.d, immediate);
        const block = size == 4 and cog.setq_pending;
        cog.memory_transfer = .{
            .address = memoryAddress(cog, args.s, args.s_imm, size, block),
            .remaining = if (block) @as(u64, cog.q) + 1 else 1,
            .reg = @intFromEnum(args.d),
            .lut = block and cog.q2,
            .block = block,
            .immediate = if (!block or immediate) value else null,
        };
    }
    const transfer = &cog.memory_transfer.?;
    const value = transfer.immediate orelse if (transfer.lut) cog.read_lut(transfer.reg) else if (transfer.block and transfer.reg >= 504) cog.ram_tail[transfer.reg - 504] else cog.registers.values[transfer.reg];
    cog.hub.write_memory(transfer.address, value, size, masked);
    transfer.address +%= size;
    transfer.reg +%= 1;
    transfer.remaining -= 1;
    if (transfer.remaining != 0) return .wait;
    cog.memory_transfer = null;
    return .next;
}

fn callPointer(cog: *Cog, pointer: Cog.Register, target: u20) void {
    const address = cog.read_reg(pointer);
    cog.hub.write_memory(address, cog.return_address(), 4, false);
    cog.write_reg(pointer, address +% 4);
    cog.jump(target);
}

fn returnPointer(cog: *Cog, pointer: Cog.Register, args: encoding.OnlyFlags) Cog.ExecResult {
    const address = cog.read_reg(pointer) -% 4;
    cog.write_reg(pointer, address);
    jumpFlags(cog, args, cog.hub.read_memory(address, 4));
    return .next;
}

fn pollEvent(cog: *Cog, args: encoding.OnlyFlags, event: EventId) Cog.ExecResult {
    const mask = event.mask();
    const occurred = cog.events & mask != 0;
    cog.events &= ~mask;
    if (args.c_mod == .write) cog.c = occurred;
    if (args.z_mod == .write) cog.z = occurred;
    return .next;
}

fn waitEvent(cog: *Cog, args: encoding.OnlyFlags, event: EventId) Cog.ExecResult {
    const mask = event.mask();
    const occurred = cog.events & mask != 0;
    if (cog.wait_until == null and cog.setq_pending and !cog.q2) {
        const delta = cog.q -% @as(u32, @truncate(cog.hub.counter));
        cog.wait_until = cog.hub.counter +% delta;
    }
    const timeout = if (cog.wait_until) |deadline| cog.hub.counter == deadline else false;
    if (!occurred and !timeout) return .wait;
    cog.wait_until = null;
    cog.events &= ~mask;
    if (args.c_mod == .write) cog.c = !occurred;
    if (args.z_mod == .write) cog.z = !occurred;
    return .next;
}

fn branchEvent(cog: *Cog, args: encoding.Only_Simm, event: EventId, positive: bool) Cog.ExecResult {
    const target = branchS(cog, args.s, args.s_imm);
    const mask = event.mask();
    const occurred = cog.events & mask != 0;
    cog.events &= ~mask;
    if (occurred == positive) cog.jump(target);
    return .next;
}

fn addCounter(cog: *Cog, args: encoding.Both_D_Simm, event: EventId) Cog.ExecResult {
    const index = @intFromEnum(event) - @intFromEnum(EventId.CT1);
    const value = cog.read_reg(args.d) +% operandS(cog, args.s, args.s_imm);
    cog.write_result(args.d, value);
    cog.ct_targets[index] = value;
    cog.events &= ~event.mask();
    return .next;
}

fn setSelectable(cog: *Cog, args: encoding.Only_Dimm, event: EventId) Cog.ExecResult {
    const index = @intFromEnum(event) - @intFromEnum(EventId.SE1);
    const value = operandD(cog, args.d, args.d_imm) & 0x1ff;
    // Pin events are outside the isolated-core model.
    if (value >= 0x40) return .unsupported;
    cog.selectable_events[index] = @intCast(value);
    cog.events &= ~event.mask();
    return .next;
}

fn alter(cog: *Cog, args: encoding.Both_D_Simm, comptime field: []const u8, comptime lane_bits: u3) Cog.ExecResult {
    const d = cog.read_reg(args.d);
    // Silicon erratum: ALTx uses AUGS without consuming it.
    const saved = cog.augs;
    const pending = cog.augs_pending;
    const source = operandS(cog, args.s, args.s_imm);
    cog.augs = saved;
    cog.augs_pending = pending;
    if (cog.next_instruction) |*next| {
        const reg: Cog.Register = @enumFromInt(@as(u9, @truncate((d >> lane_bits) +% source)));
        @field(next, field) = reg;
        if (lane_bits > 0 and lane_bits < 5) {
            const lane_mask: u32 = (@as(u32, 1) << lane_bits) - 1;
            next.instr = (next.instr & ~(lane_mask << 19)) | ((d & lane_mask) << 19);
        }
    }
    const increment: i32 = @as(i9, @bitCast(@as(u9, @truncate(source >> 9))));
    cog.write_result(args.d, d +% @as(u32, @bitCast(increment)));
    return .next;
}

fn pixelTerm(mode: u3, d: u32, s: u32, pivot: u8) u32 {
    return switch (mode) {
        0 => 0,
        1 => 255,
        2 => pivot,
        3 => 255 - @as(u32, pivot),
        4 => s,
        5 => 255 - s,
        6 => d,
        7 => 255 - d,
    };
}

fn pixel(cog: *Cog, args: encoding.Both_D_Simm, comptime mode: enum { add, multiply, blend, mix }) Cog.ExecResult {
    const d = cog.read_reg(args.d);
    const s = operandS(cog, args.s, args.s_imm);
    var result: u32 = 0;
    for (0..4) |lane| {
        const shift: u5 = @intCast(lane * 8);
        const db = (d >> shift) & 255;
        const sb = (s >> shift) & 255;
        const dmix: u32 = switch (mode) {
            .add => 255,
            .multiply => sb,
            .blend => 255 - @as(u32, cog.pixel_pivot),
            .mix => pixelTerm(@truncate(cog.pixel_mode >> 3), db, sb, cog.pixel_pivot),
        };
        const smix: u32 = switch (mode) {
            .add => 255,
            .multiply => 0,
            .blend => cog.pixel_pivot,
            .mix => pixelTerm(@truncate(cog.pixel_mode), db, sb, cog.pixel_pivot),
        };
        result |= @as(u32, @min((db * dmix + sb * smix + 255) >> 8, 255)) << shift;
    }
    cog.write_result(args.d, result);
    return .next;
}
// codegen: end:runtimehelpers

pub fn execute_instruction(cog: *Cog, state: Cog.PipelineState) Cog.ExecResult {
    const result = dispatch(cog, state);
    if (result == .wait or result == .skip) return result;
    const opcode = decode.decode(state.instr);
    if (opcode != .getct) cog.ct_low = null;
    switch (opcode) {
        .setq, .setq2 => {},
        .augs, .augd, .altsn, .altgn, .altsb, .altgb, .altsw, .altgw, .altr, .altd, .alts, .altb, .alti => cog.block_pointer_delta = false,
        else => {
            cog.setq_pending = false;
            cog.q2 = false;
            cog.block_pointer_delta = false;
        },
    }
    return result;
}

fn dispatch(cog: *Cog, state: Cog.PipelineState) Cog.ExecResult {
    if (state.instr != 0 and !cog.is_condition_met(@enumFromInt(@as(u4, @truncate(state.instr >> 28))))) return .skip;
    var raw = state.instr;
    if (state.alt_d) |reg| raw = (raw & ~@as(u32, 0x3fe00)) | (@as(u32, @intFromEnum(reg)) << 9);
    if (state.alt_s) |reg| raw = (raw & ~@as(u32, 0x1ff)) | @intFromEnum(reg);
    const opcode = decode.decode(state.instr);

    const enc: encoding.Instruction = .{ .raw = raw };

    switch (opcode) {
        .invalid => return .trap,
        .ror => return execute_alu(cog, state, enc.both_d_simm_flags, alu.ROR, true),
        .rol => return execute_alu(cog, state, enc.both_d_simm_flags, alu.ROL, true),
        .shr => return execute_alu(cog, state, enc.both_d_simm_flags, alu.SHR, true),
        .shl => return execute_alu(cog, state, enc.both_d_simm_flags, alu.SHL, true),
        .rcr => return execute_alu(cog, state, enc.both_d_simm_flags, alu.RCR, true),
        .rcl => return execute_alu(cog, state, enc.both_d_simm_flags, alu.RCL, true),
        .sar => return execute_alu(cog, state, enc.both_d_simm_flags, alu.SAR, true),
        .sal => return execute_alu(cog, state, enc.both_d_simm_flags, alu.SAL, true),
        .add => return execute_alu(cog, state, enc.both_d_simm_flags, alu.ADD, true),
        .addx => return execute_alu(cog, state, enc.both_d_simm_flags, alu.ADDX, true),
        .adds => return execute_alu(cog, state, enc.both_d_simm_flags, alu.ADDS, true),
        .addsx => return execute_alu(cog, state, enc.both_d_simm_flags, alu.ADDSX, true),
        .sub => return execute_alu(cog, state, enc.both_d_simm_flags, alu.SUB, true),
        .subx => return execute_alu(cog, state, enc.both_d_simm_flags, alu.SUBX, true),
        .subs => return execute_alu(cog, state, enc.both_d_simm_flags, alu.SUBS, true),
        .subsx => return execute_alu(cog, state, enc.both_d_simm_flags, alu.SUBSX, true),
        .cmp => return execute_alu(cog, state, enc.both_d_simm_flags, alu.CMP, false),
        .cmpx => return execute_alu(cog, state, enc.both_d_simm_flags, alu.CMPX, false),
        .cmps => return execute_alu(cog, state, enc.both_d_simm_flags, alu.CMPS, false),
        .cmpsx => return execute_alu(cog, state, enc.both_d_simm_flags, alu.CMPSX, false),
        .cmpr => return execute_alu(cog, state, enc.both_d_simm_flags, alu.CMPR, false),
        .cmpm => return execute_alu(cog, state, enc.both_d_simm_flags, alu.CMPM, false),
        .subr => return execute_alu(cog, state, enc.both_d_simm_flags, alu.SUBR, true),
        .cmpsub => return execute_alu(cog, state, enc.both_d_simm_flags, alu.CMPSUB, true),
        .fge => return execute_alu(cog, state, enc.both_d_simm_flags, alu.FGE, true),
        .fle => return execute_alu(cog, state, enc.both_d_simm_flags, alu.FLE, true),
        .fges => return execute_alu(cog, state, enc.both_d_simm_flags, alu.FGES, true),
        .fles => return execute_alu(cog, state, enc.both_d_simm_flags, alu.FLES, true),
        .sumc => return execute_alu(cog, state, enc.both_d_simm_flags, alu.SUMC, true),
        .sumnc => return execute_alu(cog, state, enc.both_d_simm_flags, alu.SUMNC, true),
        .sumz => return execute_alu(cog, state, enc.both_d_simm_flags, alu.SUMZ, true),
        .sumnz => return execute_alu(cog, state, enc.both_d_simm_flags, alu.SUMNZ, true),
        .testb => return execute_alu(cog, state, enc.both_d_simm_flags, alu.TESTB, false),
        .testbn => return execute_alu(cog, state, enc.both_d_simm_flags, alu.TESTBN, false),
        .testb_and => return execute_alu(cog, state, enc.both_d_simm_flags, alu.TESTB_AND, false),
        .testbn_and => return execute_alu(cog, state, enc.both_d_simm_flags, alu.TESTBN_AND, false),
        .testb_or => return execute_alu(cog, state, enc.both_d_simm_flags, alu.TESTB_OR, false),
        .testbn_or => return execute_alu(cog, state, enc.both_d_simm_flags, alu.TESTBN_OR, false),
        .testb_xor => return execute_alu(cog, state, enc.both_d_simm_flags, alu.TESTB_XOR, false),
        .testbn_xor => return execute_alu(cog, state, enc.both_d_simm_flags, alu.TESTBN_XOR, false),
        .bitl => return execute_alu(cog, state, enc.both_d_simm_flags, alu.BITL, true),
        .bith => return execute_alu(cog, state, enc.both_d_simm_flags, alu.BITH, true),
        .bitc => return execute_alu(cog, state, enc.both_d_simm_flags, alu.BITC, true),
        .bitnc => return execute_alu(cog, state, enc.both_d_simm_flags, alu.BITNC, true),
        .bitz => return execute_alu(cog, state, enc.both_d_simm_flags, alu.BITZ, true),
        .bitnz => return execute_alu(cog, state, enc.both_d_simm_flags, alu.BITNZ, true),
        .bitrnd => return .unsupported,
        .bitnot => return execute_alu(cog, state, enc.both_d_simm_flags, alu.BITNOT, true),
        .@"and" => return execute_alu(cog, state, enc.both_d_simm_flags, alu.AND, true),
        .andn => return execute_alu(cog, state, enc.both_d_simm_flags, alu.ANDN, true),
        .@"or" => return execute_alu(cog, state, enc.both_d_simm_flags, alu.OR, true),
        .xor => return execute_alu(cog, state, enc.both_d_simm_flags, alu.XOR, true),
        .muxc => return execute_alu(cog, state, enc.both_d_simm_flags, alu.MUXC, true),
        .muxnc => return execute_alu(cog, state, enc.both_d_simm_flags, alu.MUXNC, true),
        .muxz => return execute_alu(cog, state, enc.both_d_simm_flags, alu.MUXZ, true),
        .muxnz => return execute_alu(cog, state, enc.both_d_simm_flags, alu.MUXNZ, true),
        .mov => return execute_alu(cog, state, enc.both_d_simm_flags, alu.MOV, true),
        .not => return execute_alu(cog, state, enc.both_d_simm_flags, alu.NOT, true),
        .abs => return execute_alu(cog, state, enc.both_d_simm_flags, alu.ABS, true),
        .neg => return execute_alu(cog, state, enc.both_d_simm_flags, alu.NEG, true),
        .negc => return execute_alu(cog, state, enc.both_d_simm_flags, alu.NEGC, true),
        .negnc => return execute_alu(cog, state, enc.both_d_simm_flags, alu.NEGNC, true),
        .negz => return execute_alu(cog, state, enc.both_d_simm_flags, alu.NEGZ, true),
        .negnz => return execute_alu(cog, state, enc.both_d_simm_flags, alu.NEGNZ, true),
        .incmod => return execute_alu(cog, state, enc.both_d_simm_flags, alu.INCMOD, true),
        .decmod => return execute_alu(cog, state, enc.both_d_simm_flags, alu.DECMOD, true),
        .zerox => return execute_alu(cog, state, enc.both_d_simm_flags, alu.ZEROX, true),
        .signx => return execute_alu(cog, state, enc.both_d_simm_flags, alu.SIGNX, true),
        .encod => return execute_alu(cog, state, enc.both_d_simm_flags, alu.ENCOD, true),
        .ones => return execute_alu(cog, state, enc.both_d_simm_flags, alu.ONES, true),
        .@"test" => return execute_alu(cog, state, enc.both_d_simm_flags, alu.TEST, false),
        .testn => return execute_alu(cog, state, enc.both_d_simm_flags, alu.TESTN, false),
        .setnib => return execute_alu(cog, state, enc.both_d_simm_n3, alu.SETNIB, true),
        .getnib => return execute_alu(cog, state, enc.both_d_simm_n3, alu.GETNIB, true),
        .rolnib => return execute_alu(cog, state, enc.both_d_simm_n3, alu.ROLNIB, true),
        .setbyte => return execute_alu(cog, state, enc.both_d_simm_n2, alu.SETBYTE, true),
        .getbyte => return execute_alu(cog, state, enc.both_d_simm_n2, alu.GETBYTE, true),
        .rolbyte => return execute_alu(cog, state, enc.both_d_simm_n2, alu.ROLBYTE, true),
        .setword => return execute_alu(cog, state, enc.both_d_simm_n1, alu.SETWORD, true),
        .getword => return execute_alu(cog, state, enc.both_d_simm_n1, alu.GETWORD, true),
        .rolword => return execute_alu(cog, state, enc.both_d_simm_n1, alu.ROLWORD, true),
        .setr => return execute_alu(cog, state, enc.both_d_simm, alu.SETR, true),
        .setd => return execute_alu(cog, state, enc.both_d_simm, alu.SETD, true),
        .sets => return execute_alu(cog, state, enc.both_d_simm, alu.SETS, true),
        .decod => return execute_alu(cog, state, enc.both_d_simm, alu.DECOD, true),
        .bmask => return execute_alu(cog, state, enc.both_d_simm, alu.BMASK, true),
        .crcbit => return execute_alu(cog, state, enc.both_d_simm, alu.CRCBIT, true),
        .crcnib => return execute_alu(cog, state, enc.both_d_simm, alu.CRCNIB, true),
        .muxnits => return execute_alu(cog, state, enc.both_d_simm, alu.MUXNITS, true),
        .muxnibs => return execute_alu(cog, state, enc.both_d_simm, alu.MUXNIBS, true),
        .muxq => return execute_alu(cog, state, enc.both_d_simm, alu.MUXQ, true),
        .movbyts => return execute_alu(cog, state, enc.both_d_simm, alu.MOVBYTS, true),
        .mul => return execute_alu(cog, state, enc.both_d_simm_zflag, alu.MUL, true),
        .muls => return execute_alu(cog, state, enc.both_d_simm_zflag, alu.MULS, true),
        .sca => return execute_alu(cog, state, enc.both_d_simm_zflag, alu.SCA, false),
        .scas => return execute_alu(cog, state, enc.both_d_simm_zflag, alu.SCAS, false),
        .splitb => return execute_alu(cog, state, enc.only_d, alu.SPLITB, true),
        .mergeb => return execute_alu(cog, state, enc.only_d, alu.MERGEB, true),
        .splitw => return execute_alu(cog, state, enc.only_d, alu.SPLITW, true),
        .mergew => return execute_alu(cog, state, enc.only_d, alu.MERGEW, true),
        .seussf => return execute_alu(cog, state, enc.only_d, alu.SEUSSF, true),
        .seussr => return execute_alu(cog, state, enc.only_d, alu.SEUSSR, true),
        .rgbsqz => return execute_alu(cog, state, enc.only_d, alu.RGBSQZ, true),
        .rgbexp => return execute_alu(cog, state, enc.only_d, alu.RGBEXP, true),
        .xoro32 => return execute_alu(cog, state, enc.only_d, alu.XORO32, true),
        .rev => return execute_alu(cog, state, enc.only_d, alu.REV, true),
        .rczr => return execute_alu(cog, state, enc.only_d_flags, alu.RCZR, true),
        .rczl => return execute_alu(cog, state, enc.only_d_flags, alu.RCZL, true),
        .wrc => return execute_alu(cog, state, enc.only_d, alu.WRC, true),
        .wrnc => return execute_alu(cog, state, enc.only_d, alu.WRNC, true),
        .wrz => return execute_alu(cog, state, enc.only_d, alu.WRZ, true),
        .wrnz => return execute_alu(cog, state, enc.only_d, alu.WRNZ, true),
        .modcz => return execute_alu(cog, state, enc.update_flags, alu.MODCZ, false),

        inline else => |opc| {
            @setEvalBranchQuota(10_000);
            const field = comptime decode.instruction_type.get(opc);
            const params = @field(enc, field);

            logger.info("0x{X:0>5}: 0x{X:0>8} {t}: {f}", .{ state.pc, state.instr, opc, params });

            return @field(@This(), @tagName(opc))(cog, params);
        },
    }
}

// codegen: begin:globalcode
fn execute_alu(
    cog: *Cog,
    state: Cog.PipelineState,
    operands: anytype,
    comptime function: anytype,
    comptime writes_result: bool,
) Cog.ExecResult {
    const Operands = @TypeOf(operands);
    const is_modcz = Operands == encoding.UpdateFlags;
    const d_reg: ?Cog.Register = if (is_modcz) null else state.alt_d orelse operands.d;
    const source = if (@hasField(Operands, "s")) operandS(cog, state.alt_s orelse operands.s, operands.s_imm) else 0;
    const input: alu.Input = .{
        .d = if (is_modcz) (state.instr >> 9) & 0x1FF else cog.read_reg(d_reg.?),
        .s = if (@hasField(Operands, "s")) state.s_value orelse source else 0,
        .c = .from_bool(cog.c),
        .z = .from_bool(cog.z),
        .q = cog.q,
        .setq_prefix = cog.setq_pending,
    };
    const output = if (@hasField(Operands, "n")) function(input, operands.n) else function(input);
    if (writes_result and !state.no_result) cog.write_result(state.alt_r orelse d_reg.?, output.result);
    if (@hasField(Operands, "c_mod") and operands.c_mod == .write) cog.c = output.c == .set;
    if (@hasField(Operands, "z_mod") and operands.z_mod == .write) cog.z = output.z == .set;
    cog.q = output.q;
    if (output.next_s) |forwarded| {
        // Cog.step prefetches the successor before dispatching this instruction.
        if (cog.next_instruction) |*next| next.s_value = forwarded;
    }
    return .next;
}
// codegen: end:globalcode

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
    // codegen: begin:call_a
    if (args.relative and args.address & 3 != 0) return .unsupported;
    cog.call(branchA(cog, args));
    return .next;
    // codegen: end:call_a
}

/// CALLA #{\}A
/// EEEE 1101110 RAA AAAAAAAAA AAAAAAAAA
///
/// description: Call to A by writing {C, Z, 10'b0, PC[19:0]} to hub long at PTRA++.         If R = 1 then PC += A, else PC = A. "\" forces R = 0.
/// cog timing:  5...12 *
/// hub timing:  14...32 *
/// access:      mem=Write, reg=None, stack=None
pub fn calla_a(cog: *Cog, args: encoding.AbsPointer) Cog.ExecResult {
    // codegen: begin:calla_a
    callPointer(cog, .PTRA, branchA(cog, args));
    return .next;
    // codegen: end:calla_a
}

/// CALLB #{\}A
/// EEEE 1101111 RAA AAAAAAAAA AAAAAAAAA
///
/// description: Call to A by writing {C, Z, 10'b0, PC[19:0]} to hub long at PTRB++.         If R = 1 then PC += A, else PC = A. "\" forces R = 0.
/// cog timing:  5...12 *
/// hub timing:  14...32 *
/// access:      mem=Write, reg=None, stack=None
pub fn callb_a(cog: *Cog, args: encoding.AbsPointer) Cog.ExecResult {
    // codegen: begin:callb_a
    callPointer(cog, .PTRB, branchA(cog, args));
    return .next;
    // codegen: end:callb_a
}

/// CALLD PA/PB/PTRA/PTRB, #{\}A
/// EEEE 11100WW RAA AAAAAAAAA AAAAAAAAA
///
/// description: Call to A by writing {C, Z, 10'b0, PC[19:0]} to PA/PB/PTRA/PTRB (per W).    If R = 1 then PC += A, else PC = A. "\" forces R = 0.
/// cog timing:  4
/// hub timing:  13...20
/// access:      mem=None, reg=Per W, stack=None
pub fn calld_a(cog: *Cog, args: encoding.LocStyle) Cog.ExecResult {
    // codegen: begin:calld_a
    const target = branchA(cog, .{ .cond = args.cond, .relative = args.relative, .address = args.address, ._mask1 = 0 });
    const value = cog.return_address();
    cog.write_result(pointerReg(args.pointer), value);
    cog.jump(target);
    return .next;
    // codegen: end:calld_a
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
    // codegen: begin:jmp_a
    if (args.relative and args.address & 3 != 0) return .unsupported;
    cog.jump(branchA(cog, args));
    return .next;
    // codegen: end:jmp_a
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
    // codegen: begin:call_d
    const value = cog.read_reg(args.d);
    cog.call(@truncate(value));
    jumpFlags(cog, args, value);
    return .next;
    // codegen: end:call_d
}

/// CALLA D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000101110
///
/// description: Call to D by writing {C, Z, 10'b0, PC[19:0]} to hub long at PTRA++.     C = D[31], Z = D[30], PC = D[19:0].
/// cog timing:  5...12 *
/// hub timing:  14...32 *
/// access:      mem=Write, reg=None, stack=None
pub fn calla_d(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    // codegen: begin:calla_d
    const value = cog.read_reg(args.d);
    callPointer(cog, .PTRA, @truncate(value));
    jumpFlags(cog, args, value);
    return .next;
    // codegen: end:calla_d
}

/// CALLB D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000101111
///
/// description: Call to D by writing {C, Z, 10'b0, PC[19:0]} to hub long at PTRB++.     C = D[31], Z = D[30], PC = D[19:0].
/// cog timing:  5...12 *
/// hub timing:  14...32 *
/// access:      mem=Write, reg=None, stack=None
pub fn callb_d(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    // codegen: begin:callb_d
    const value = cog.read_reg(args.d);
    callPointer(cog, .PTRB, @truncate(value));
    jumpFlags(cog, args, value);
    return .next;
    // codegen: end:callb_d
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
    // codegen: begin:execf
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:execf
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
    // codegen: begin:jmp_d
    const value = cog.read_reg(args.d);

    jumpFlags(cog, args, value);
    return .next;
    // codegen: end:jmp_d
}

/// JMPREL {#}D
/// EEEE 1101011 00L DDDDDDDDD 000110000
///
/// description: Jump ahead/back by D instructions. For cogex, PC += D[19:0]. For hubex, PC += D[17:0] << 2.
/// cog timing:  4
/// hub timing:  13...20
/// access:      mem=None, reg=None, stack=None
pub fn jmprel(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    // codegen: begin:jmprel
    const offset: u20 = @truncate(operandD(cog, args.d, args.d_imm));
    const scale: u20 = if (cog.exec_mode == .hub) 4 else 1;
    cog.jump(cog.dispatch_pc +% scale +% offset *% scale);
    return .next;
    // codegen: end:jmprel
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
    // codegen: begin:skipf
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:skipf
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
    // codegen: begin:skip
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:skip
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
    // codegen: begin:rep
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:rep
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
    // codegen: begin:ret
    const value = cog.pop();
    if (args.c_mod == .write) cog.c = value >> 31 != 0;
    if (args.z_mod == .write) cog.z = value & 0x4000_0000 != 0;
    cog.jump(@truncate(value));
    return .next;
    // codegen: end:ret
}

/// RETA {WC/WZ/WCZ}
/// EEEE 1101011 CZ1 000000000 000101110
///
/// description: Return by reading hub long (L) at --PTRA.                               C = L[31], Z = L[30], PC = L[19:0].
/// cog timing:  11...18 *
/// hub timing:  20...40 *
/// access:      mem=Read, reg=None, stack=None
pub fn reta(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    // codegen: begin:reta
    return returnPointer(cog, .PTRA, args);
    // codegen: end:reta
}

/// RETB {WC/WZ/WCZ}
/// EEEE 1101011 CZ1 000000000 000101111
///
/// description: Return by reading hub long (L) at --PTRB.                               C = L[31], Z = L[30], PC = L[19:0].
/// cog timing:  11...18 *
/// hub timing:  20...40 *
/// access:      mem=Read, reg=None, stack=None
pub fn retb(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    // codegen: begin:retb
    return returnPointer(cog, .PTRB, args);
    // codegen: end:retb
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
    // codegen: begin:calld_s
    const augmented = cog.augs_pending;
    const source = operandS(cog, args.s, args.s_imm);
    const target = branchValue(cog, source, args.s_imm, augmented);
    const value = cog.return_address();
    cog.write_result(args.d, value);
    if (args.c_mod == .write) cog.c = source >> 31 != 0;
    if (args.z_mod == .write) cog.z = source & 0x4000_0000 != 0;
    cog.jump(target);
    return .next;
    // codegen: end:calld_s
}

/// CALLPA {#}D, {#}S**
/// EEEE 1011010 0LI DDDDDDDDD SSSSSSSSS
///
/// description: Call to S** by pushing {C, Z, 10'b0, PC[19:0]} onto stack, copy D to PA.
/// cog timing:  4
/// hub timing:  13...20
/// access:      mem=None, reg=PA, stack=Push
pub fn callpa(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    // codegen: begin:callpa
    const value = operandD(cog, args.d, args.d_imm);
    const target = branchS(cog, args.s, args.s_imm);
    cog.write_reg(.PA, value);
    cog.call(target);
    return .next;
    // codegen: end:callpa
}

/// CALLPB {#}D, {#}S**
/// EEEE 1011010 1LI DDDDDDDDD SSSSSSSSS
///
/// description: Call to S** by pushing {C, Z, 10'b0, PC[19:0]} onto stack, copy D to PB.
/// cog timing:  4
/// hub timing:  13...20
/// access:      mem=None, reg=PB, stack=Push
pub fn callpb(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    // codegen: begin:callpb
    const value = operandD(cog, args.d, args.d_imm);
    const target = branchS(cog, args.s, args.s_imm);
    cog.write_reg(.PB, value);
    cog.call(target);
    return .next;
    // return .next;
    // codegen: end:callpb
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
    // codegen: begin:djz
    const value = cog.read_reg(args.d) -% 1;
    const target = branchS(cog, args.s, args.s_imm);
    cog.write_result(args.d, value);
    if (value == 0) cog.jump(target);
    return .next;
    // codegen: end:djz
}

/// DJNZ D, {#}S**
/// EEEE 1011011 01I DDDDDDDDD SSSSSSSSS
///
/// description: Decrement D and jump to S** if result is not zero.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=D, stack=None
pub fn djnz(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    // codegen: begin:djnz
    const value = cog.read_reg(args.d) -% 1;
    const target = branchS(cog, args.s, args.s_imm);
    cog.write_result(args.d, value);
    if (value != 0) cog.jump(target);
    return .next;
    // codegen: end:djnz
}

/// DJF D, {#}S**
/// EEEE 1011011 10I DDDDDDDDD SSSSSSSSS
///
/// description: Decrement D and jump to S** if result is $FFFF_FFFF.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=D, stack=None
pub fn djf(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    // codegen: begin:djf
    const value = cog.read_reg(args.d) -% 1;
    const target = branchS(cog, args.s, args.s_imm);
    cog.write_result(args.d, value);
    if (value == 0xffffffff) cog.jump(target);
    return .next;
    // codegen: end:djf
}

/// DJNF D, {#}S**
/// EEEE 1011011 11I DDDDDDDDD SSSSSSSSS
///
/// description: Decrement D and jump to S** if result is not $FFFF_FFFF.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=D, stack=None
pub fn djnf(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    // codegen: begin:djnf
    const value = cog.read_reg(args.d) -% 1;
    const target = branchS(cog, args.s, args.s_imm);
    cog.write_result(args.d, value);
    if (value != 0xffffffff) cog.jump(target);
    return .next;
    // codegen: end:djnf
}

/// IJZ D, {#}S**
/// EEEE 1011100 00I DDDDDDDDD SSSSSSSSS
///
/// description: Increment D and jump to S** if result is zero.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=D, stack=None
pub fn ijz(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    // codegen: begin:ijz
    const value = cog.read_reg(args.d) +% 1;
    const target = branchS(cog, args.s, args.s_imm);
    cog.write_result(args.d, value);
    if (value == 0) cog.jump(target);
    return .next;
    // codegen: end:ijz
}

/// IJNZ D, {#}S**
/// EEEE 1011100 01I DDDDDDDDD SSSSSSSSS
///
/// description: Increment D and jump to S** if result is not zero.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=D, stack=None
pub fn ijnz(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    // codegen: begin:ijnz
    const value = cog.read_reg(args.d) +% 1;
    const target = branchS(cog, args.s, args.s_imm);
    cog.write_result(args.d, value);
    if (value != 0) cog.jump(target);
    return .next;
    // codegen: end:ijnz
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
    // codegen: begin:tjz
    const value = cog.read_reg(args.d);
    const target = branchS(cog, args.s, args.s_imm);
    if (value == 0) cog.jump(target);
    return .next;
    // codegen: end:tjz
}

/// TJNZ D, {#}S**
/// EEEE 1011100 11I DDDDDDDDD SSSSSSSSS
///
/// description: Test D and jump to S** if D is not zero.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn tjnz(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    // codegen: begin:tjnz
    const value = cog.read_reg(args.d);
    const target = branchS(cog, args.s, args.s_imm);
    if (value != 0) cog.jump(target);
    return .next;
    // codegen: end:tjnz
}

/// TJF D, {#}S**
/// EEEE 1011101 00I DDDDDDDDD SSSSSSSSS
///
/// description: Test D and jump to S** if D is full (D = $FFFF_FFFF).
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn tjf(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    // codegen: begin:tjf
    const value = cog.read_reg(args.d);
    const target = branchS(cog, args.s, args.s_imm);
    if (value == 0xffffffff) cog.jump(target);
    return .next;
    // codegen: end:tjf
}

/// TJNF D, {#}S**
/// EEEE 1011101 01I DDDDDDDDD SSSSSSSSS
///
/// description: Test D and jump to S** if D is not full (D != $FFFF_FFFF).
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn tjnf(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    // codegen: begin:tjnf
    const value = cog.read_reg(args.d);
    const target = branchS(cog, args.s, args.s_imm);
    if (value != 0xffffffff) cog.jump(target);
    return .next;
    // codegen: end:tjnf
}

/// TJS D, {#}S**
/// EEEE 1011101 10I DDDDDDDDD SSSSSSSSS
///
/// description: Test D and jump to S** if D is signed (D[31] = 1).
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn tjs(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    // codegen: begin:tjs
    const value = cog.read_reg(args.d);
    const target = branchS(cog, args.s, args.s_imm);
    if (value >> 31 != 0) cog.jump(target);
    return .next;
    // codegen: end:tjs
}

/// TJNS D, {#}S**
/// EEEE 1011101 11I DDDDDDDDD SSSSSSSSS
///
/// description: Test D and jump to S** if D is not signed (D[31] = 0).
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn tjns(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    // codegen: begin:tjns
    const value = cog.read_reg(args.d);
    const target = branchS(cog, args.s, args.s_imm);
    if (value >> 31 == 0) cog.jump(target);
    return .next;
    // codegen: end:tjns
}

/// TJV D, {#}S**
/// EEEE 1011110 00I DDDDDDDDD SSSSSSSSS
///
/// description: Test D and jump to S** if D overflowed (D[31] != C, C = 'correct sign' from last addition/subtraction).
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn tjv(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    // codegen: begin:tjv
    const value = cog.read_reg(args.d);
    const target = branchS(cog, args.s, args.s_imm);
    if ((value >> 31 != 0) != cog.c) cog.jump(target);
    return .next;
    // codegen: end:tjv
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
    // codegen: begin:qmul
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:qmul
}

/// QDIV {#}D, {#}S
/// EEEE 1101000 1LI DDDDDDDDD SSSSSSSSS
///
/// description: Begin CORDIC unsigned division of {SETQ value or 32'b0, D} / S. GETQX/GETQY retrieves quotient/remainder.
/// cog timing:  2...9
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn qdiv(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    // codegen: begin:qdiv
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:qdiv
}

/// QFRAC {#}D, {#}S
/// EEEE 1101001 0LI DDDDDDDDD SSSSSSSSS
///
/// description: Begin CORDIC unsigned division of {D, SETQ value or 32'b0} / S. GETQX/GETQY retrieves quotient/remainder.
/// cog timing:  2...9
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn qfrac(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    // codegen: begin:qfrac
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:qfrac
}

/// QSQRT {#}D, {#}S
/// EEEE 1101001 1LI DDDDDDDDD SSSSSSSSS
///
/// description: Begin CORDIC square root of {S, D}. GETQX retrieves root.
/// cog timing:  2...9
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn qsqrt(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    // codegen: begin:qsqrt
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:qsqrt
}

/// QROTATE {#}D, {#}S
/// EEEE 1101010 0LI DDDDDDDDD SSSSSSSSS
///
/// description: Begin CORDIC rotation of point (D, SETQ value or 32'b0) by angle S. GETQX/GETQY retrieves X/Y.
/// cog timing:  2...9
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn qrotate(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    // codegen: begin:qrotate
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:qrotate
}

/// QVECTOR {#}D, {#}S
/// EEEE 1101010 1LI DDDDDDDDD SSSSSSSSS
///
/// description: Begin CORDIC vectoring of point (D, S). GETQX/GETQY retrieves length/angle.
/// cog timing:  2...9
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn qvector(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    // codegen: begin:qvector
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:qvector
}

/// QLOG {#}D
/// EEEE 1101011 00L DDDDDDDDD 000001110
///
/// description: Begin CORDIC number-to-logarithm conversion of D. GETQX retrieves log {5'whole_exponent, 27'fractional_exponent}.
/// cog timing:  2...9
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn qlog(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    // codegen: begin:qlog
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:qlog
}

/// QEXP {#}D
/// EEEE 1101011 00L DDDDDDDDD 000001111
///
/// description: Begin CORDIC logarithm-to-number conversion of D. GETQX retrieves number.
/// cog timing:  2...9
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn qexp(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    // codegen: begin:qexp
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:qexp
}

/// GETQX D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000011000
///
/// description: Retrieve CORDIC result X into D. Waits, in case result not ready. C = X[31]. *
/// cog timing:  2...58
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn getqx(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    // codegen: begin:getqx
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:getqx
}

/// GETQY D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000011001
///
/// description: Retrieve CORDIC result Y into D. Waits, in case result not ready. C = Y[31]. *
/// cog timing:  2...58
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn getqy(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    // codegen: begin:getqy
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:getqy
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
    // codegen: begin:setcy
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:setcy
}

/// SETCI {#}D
/// EEEE 1101011 00L DDDDDDDDD 000111001
///
/// description: Set the colorspace converter "CI" parameter to D[31:0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setci(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    // codegen: begin:setci
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:setci
}

/// SETCQ {#}D
/// EEEE 1101011 00L DDDDDDDDD 000111010
///
/// description: Set the colorspace converter "CQ" parameter to D[31:0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setcq(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    // codegen: begin:setcq
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:setcq
}

/// SETCFRQ {#}D
/// EEEE 1101011 00L DDDDDDDDD 000111011
///
/// description: Set the colorspace converter "CFRQ" parameter to D[31:0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setcfrq(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    // codegen: begin:setcfrq
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:setcfrq
}

/// SETCMOD {#}D
/// EEEE 1101011 00L DDDDDDDDD 000111100
///
/// description: Set the colorspace converter "CMOD" parameter to D[8:0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setcmod(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    // codegen: begin:setcmod
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:setcmod
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
    // codegen: begin:cogatn
    const mask = operandD(cog, args.d, args.d_imm);
    for (&cog.hub.cogs, 0..) |*target, id| if (mask & (@as(u32, 1) << @intCast(id)) != 0) {
        target.events |= EventId.ATN.mask();
    };
    return .next;
    // codegen: end:cogatn
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
    // codegen: begin:jint
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:jint
}

/// JCT1 {#}S**
/// EEEE 1011110 01I 000000001 SSSSSSSSS
///
/// description: Jump to S** if CT1 event flag is set.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jct1(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    // codegen: begin:jct1
    return branchEvent(cog, args, .CT1, true);
    // codegen: end:jct1
}

/// JCT2 {#}S**
/// EEEE 1011110 01I 000000010 SSSSSSSSS
///
/// description: Jump to S** if CT2 event flag is set.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jct2(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    // codegen: begin:jct2
    return branchEvent(cog, args, .CT2, true);
    // codegen: end:jct2
}

/// JCT3 {#}S**
/// EEEE 1011110 01I 000000011 SSSSSSSSS
///
/// description: Jump to S** if CT3 event flag is set.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jct3(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    // codegen: begin:jct3
    return branchEvent(cog, args, .CT3, true);
    // codegen: end:jct3
}

/// JSE1 {#}S**
/// EEEE 1011110 01I 000000100 SSSSSSSSS
///
/// description: Jump to S** if SE1 event flag is set.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jse1(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    // codegen: begin:jse1
    return branchEvent(cog, args, .SE1, true);
    // codegen: end:jse1
}

/// JSE2 {#}S**
/// EEEE 1011110 01I 000000101 SSSSSSSSS
///
/// description: Jump to S** if SE2 event flag is set.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jse2(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    // codegen: begin:jse2
    return branchEvent(cog, args, .SE2, true);
    // codegen: end:jse2
}

/// JSE3 {#}S**
/// EEEE 1011110 01I 000000110 SSSSSSSSS
///
/// description: Jump to S** if SE3 event flag is set.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jse3(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    // codegen: begin:jse3
    return branchEvent(cog, args, .SE3, true);
    // codegen: end:jse3
}

/// JSE4 {#}S**
/// EEEE 1011110 01I 000000111 SSSSSSSSS
///
/// description: Jump to S** if SE4 event flag is set.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jse4(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    // codegen: begin:jse4
    return branchEvent(cog, args, .SE4, true);
    // codegen: end:jse4
}

/// JPAT {#}S**
/// EEEE 1011110 01I 000001000 SSSSSSSSS
///
/// description: Jump to S** if PAT event flag is set.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jpat(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    // codegen: begin:jpat
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:jpat
}

/// JFBW {#}S**
/// EEEE 1011110 01I 000001001 SSSSSSSSS
///
/// description: Jump to S** if FBW event flag is set.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jfbw(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    // codegen: begin:jfbw
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:jfbw
}

/// JXMT {#}S**
/// EEEE 1011110 01I 000001010 SSSSSSSSS
///
/// description: Jump to S** if XMT event flag is set.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jxmt(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    // codegen: begin:jxmt
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:jxmt
}

/// JXFI {#}S**
/// EEEE 1011110 01I 000001011 SSSSSSSSS
///
/// description: Jump to S** if XFI event flag is set.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jxfi(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    // codegen: begin:jxfi
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:jxfi
}

/// JXRO {#}S**
/// EEEE 1011110 01I 000001100 SSSSSSSSS
///
/// description: Jump to S** if XRO event flag is set.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jxro(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    // codegen: begin:jxro
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:jxro
}

/// JXRL {#}S**
/// EEEE 1011110 01I 000001101 SSSSSSSSS
///
/// description: Jump to S** if XRL event flag is set.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jxrl(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    // codegen: begin:jxrl
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:jxrl
}

/// JATN {#}S**
/// EEEE 1011110 01I 000001110 SSSSSSSSS
///
/// description: Jump to S** if ATN event flag is set.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jatn(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    // codegen: begin:jatn
    return branchEvent(cog, args, .ATN, true);
    // codegen: end:jatn
}

/// JQMT {#}S**
/// EEEE 1011110 01I 000001111 SSSSSSSSS
///
/// description: Jump to S** if QMT event flag is set.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jqmt(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    // codegen: begin:jqmt
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:jqmt
}

/// JNINT {#}S**
/// EEEE 1011110 01I 000010000 SSSSSSSSS
///
/// description: Jump to S** if INT event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnint(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    // codegen: begin:jnint
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:jnint
}

/// JNCT1 {#}S**
/// EEEE 1011110 01I 000010001 SSSSSSSSS
///
/// description: Jump to S** if CT1 event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnct1(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    // codegen: begin:jnct1
    return branchEvent(cog, args, .CT1, false);
    // codegen: end:jnct1
}

/// JNCT2 {#}S**
/// EEEE 1011110 01I 000010010 SSSSSSSSS
///
/// description: Jump to S** if CT2 event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnct2(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    // codegen: begin:jnct2
    return branchEvent(cog, args, .CT2, false);
    // codegen: end:jnct2
}

/// JNCT3 {#}S**
/// EEEE 1011110 01I 000010011 SSSSSSSSS
///
/// description: Jump to S** if CT3 event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnct3(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    // codegen: begin:jnct3
    return branchEvent(cog, args, .CT3, false);
    // codegen: end:jnct3
}

/// JNSE1 {#}S**
/// EEEE 1011110 01I 000010100 SSSSSSSSS
///
/// description: Jump to S** if SE1 event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnse1(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    // codegen: begin:jnse1
    return branchEvent(cog, args, .SE1, false);
    // codegen: end:jnse1
}

/// JNSE2 {#}S**
/// EEEE 1011110 01I 000010101 SSSSSSSSS
///
/// description: Jump to S** if SE2 event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnse2(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    // codegen: begin:jnse2
    return branchEvent(cog, args, .SE2, false);
    // codegen: end:jnse2
}

/// JNSE3 {#}S**
/// EEEE 1011110 01I 000010110 SSSSSSSSS
///
/// description: Jump to S** if SE3 event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnse3(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    // codegen: begin:jnse3
    return branchEvent(cog, args, .SE3, false);
    // codegen: end:jnse3
}

/// JNSE4 {#}S**
/// EEEE 1011110 01I 000010111 SSSSSSSSS
///
/// description: Jump to S** if SE4 event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnse4(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    // codegen: begin:jnse4
    return branchEvent(cog, args, .SE4, false);
    // codegen: end:jnse4
}

/// JNPAT {#}S**
/// EEEE 1011110 01I 000011000 SSSSSSSSS
///
/// description: Jump to S** if PAT event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnpat(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    // codegen: begin:jnpat
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:jnpat
}

/// JNFBW {#}S**
/// EEEE 1011110 01I 000011001 SSSSSSSSS
///
/// description: Jump to S** if FBW event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnfbw(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    // codegen: begin:jnfbw
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:jnfbw
}

/// JNXMT {#}S**
/// EEEE 1011110 01I 000011010 SSSSSSSSS
///
/// description: Jump to S** if XMT event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnxmt(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    // codegen: begin:jnxmt
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:jnxmt
}

/// JNXFI {#}S**
/// EEEE 1011110 01I 000011011 SSSSSSSSS
///
/// description: Jump to S** if XFI event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnxfi(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    // codegen: begin:jnxfi
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:jnxfi
}

/// JNXRO {#}S**
/// EEEE 1011110 01I 000011100 SSSSSSSSS
///
/// description: Jump to S** if XRO event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnxro(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    // codegen: begin:jnxro
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:jnxro
}

/// JNXRL {#}S**
/// EEEE 1011110 01I 000011101 SSSSSSSSS
///
/// description: Jump to S** if XRL event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnxrl(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    // codegen: begin:jnxrl
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:jnxrl
}

/// JNATN {#}S**
/// EEEE 1011110 01I 000011110 SSSSSSSSS
///
/// description: Jump to S** if ATN event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnatn(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    // codegen: begin:jnatn
    return branchEvent(cog, args, .ATN, false);
    // codegen: end:jnatn
}

/// JNQMT {#}S**
/// EEEE 1011110 01I 000011111 SSSSSSSSS
///
/// description: Jump to S** if QMT event flag is clear.
/// cog timing:  2 or 4
/// hub timing:  2 or 13...20
/// access:      mem=None, reg=None, stack=None
pub fn jnqmt(cog: *Cog, args: encoding.Only_Simm) Cog.ExecResult {
    // codegen: begin:jnqmt
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:jnqmt
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
    // codegen: begin:addct1
    return addCounter(cog, args, .CT1);
    // codegen: end:addct1
}

/// ADDCT2 D, {#}S
/// EEEE 1010011 01I DDDDDDDDD SSSSSSSSS
///
/// description: Set CT2 event to trigger on CT = D + S. Adds S into D.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn addct2(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    // codegen: begin:addct2
    return addCounter(cog, args, .CT2);
    // codegen: end:addct2
}

/// ADDCT3 D, {#}S
/// EEEE 1010011 10I DDDDDDDDD SSSSSSSSS
///
/// description: Set CT3 event to trigger on CT = D + S. Adds S into D.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn addct3(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    // codegen: begin:addct3
    return addCounter(cog, args, .CT3);
    // codegen: end:addct3
}

/// SETPAT {#}D, {#}S
/// EEEE 1011111 1LI DDDDDDDDD SSSSSSSSS
///
/// description: Set pin pattern for PAT event. C selects INA/INB, Z selects =/!=, D provides mask value, S provides match value.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setpat(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    // codegen: begin:setpat
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:setpat
}

/// SETSE1 {#}D
/// EEEE 1101011 00L DDDDDDDDD 000100000
///
/// description: Set SE1 event configuration to D[8:0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setse1(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    // codegen: begin:setse1
    return setSelectable(cog, args, .SE1);
    // codegen: end:setse1
}

/// SETSE2 {#}D
/// EEEE 1101011 00L DDDDDDDDD 000100001
///
/// description: Set SE2 event configuration to D[8:0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setse2(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    // codegen: begin:setse2
    return setSelectable(cog, args, .SE2);
    // codegen: end:setse2
}

/// SETSE3 {#}D
/// EEEE 1101011 00L DDDDDDDDD 000100010
///
/// description: Set SE3 event configuration to D[8:0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setse3(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    // codegen: begin:setse3
    return setSelectable(cog, args, .SE3);
    // codegen: end:setse3
}

/// SETSE4 {#}D
/// EEEE 1101011 00L DDDDDDDDD 000100011
///
/// description: Set SE4 event configuration to D[8:0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setse4(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    // codegen: begin:setse4
    return setSelectable(cog, args, .SE4);
    // codegen: end:setse4
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
    // codegen: begin:pollint
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:pollint
}

/// POLLCT1 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000000001 000100100
///
/// description: Get CT1 event flag into C/Z, then clear it.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn pollct1(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    // codegen: begin:pollct1
    return pollEvent(cog, args, .CT1);
    // codegen: end:pollct1
}

/// POLLCT2 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000000010 000100100
///
/// description: Get CT2 event flag into C/Z, then clear it.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn pollct2(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    // codegen: begin:pollct2
    return pollEvent(cog, args, .CT2);
    // codegen: end:pollct2
}

/// POLLCT3 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000000011 000100100
///
/// description: Get CT3 event flag into C/Z, then clear it.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn pollct3(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    // codegen: begin:pollct3
    return pollEvent(cog, args, .CT3);
    // codegen: end:pollct3
}

/// POLLSE1 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000000100 000100100
///
/// description: Get SE1 event flag into C/Z, then clear it.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn pollse1(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    // codegen: begin:pollse1
    return pollEvent(cog, args, .SE1);
    // codegen: end:pollse1
}

/// POLLSE2 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000000101 000100100
///
/// description: Get SE2 event flag into C/Z, then clear it.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn pollse2(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    // codegen: begin:pollse2
    return pollEvent(cog, args, .SE2);
    // codegen: end:pollse2
}

/// POLLSE3 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000000110 000100100
///
/// description: Get SE3 event flag into C/Z, then clear it.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn pollse3(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    // codegen: begin:pollse3
    return pollEvent(cog, args, .SE3);
    // codegen: end:pollse3
}

/// POLLSE4 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000000111 000100100
///
/// description: Get SE4 event flag into C/Z, then clear it.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn pollse4(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    // codegen: begin:pollse4
    return pollEvent(cog, args, .SE4);
    // codegen: end:pollse4
}

/// POLLPAT {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000001000 000100100
///
/// description: Get PAT event flag into C/Z, then clear it.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn pollpat(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    // codegen: begin:pollpat
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:pollpat
}

/// POLLFBW {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000001001 000100100
///
/// description: Get FBW event flag into C/Z, then clear it.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn pollfbw(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    // codegen: begin:pollfbw
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:pollfbw
}

/// POLLXMT {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000001010 000100100
///
/// description: Get XMT event flag into C/Z, then clear it.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn pollxmt(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    // codegen: begin:pollxmt
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:pollxmt
}

/// POLLXFI {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000001011 000100100
///
/// description: Get XFI event flag into C/Z, then clear it.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn pollxfi(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    // codegen: begin:pollxfi
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:pollxfi
}

/// POLLXRO {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000001100 000100100
///
/// description: Get XRO event flag into C/Z, then clear it.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn pollxro(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    // codegen: begin:pollxro
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:pollxro
}

/// POLLXRL {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000001101 000100100
///
/// description: Get XRL event flag into C/Z, then clear it.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn pollxrl(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    // codegen: begin:pollxrl
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:pollxrl
}

/// POLLATN {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000001110 000100100
///
/// description: Get ATN event flag into C/Z, then clear it.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn pollatn(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    // codegen: begin:pollatn
    return pollEvent(cog, args, .ATN);
    // codegen: end:pollatn
}

/// POLLQMT {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000001111 000100100
///
/// description: Get QMT event flag into C/Z, then clear it.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn pollqmt(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    // codegen: begin:pollqmt
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:pollqmt
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
    // codegen: begin:waitint
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:waitint
}

/// WAITCT1 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000010001 000100100
///
/// description: Wait for CT1 event flag, then clear it. Prior SETQ sets optional CT timeout value. C/Z = timeout.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn waitct1(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    // codegen: begin:waitct1
    return waitEvent(cog, args, .CT1);
    // codegen: end:waitct1
}

/// WAITCT2 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000010010 000100100
///
/// description: Wait for CT2 event flag, then clear it. Prior SETQ sets optional CT timeout value. C/Z = timeout.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn waitct2(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    // codegen: begin:waitct2
    return waitEvent(cog, args, .CT2);
    // codegen: end:waitct2
}

/// WAITCT3 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000010011 000100100
///
/// description: Wait for CT3 event flag, then clear it. Prior SETQ sets optional CT timeout value. C/Z = timeout.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn waitct3(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    // codegen: begin:waitct3
    return waitEvent(cog, args, .CT3);
    // codegen: end:waitct3
}

/// WAITSE1 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000010100 000100100
///
/// description: Wait for SE1 event flag, then clear it. Prior SETQ sets optional CT timeout value. C/Z = timeout.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn waitse1(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    // codegen: begin:waitse1
    return waitEvent(cog, args, .SE1);
    // codegen: end:waitse1
}

/// WAITSE2 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000010101 000100100
///
/// description: Wait for SE2 event flag, then clear it. Prior SETQ sets optional CT timeout value. C/Z = timeout.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn waitse2(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    // codegen: begin:waitse2
    return waitEvent(cog, args, .SE2);
    // codegen: end:waitse2
}

/// WAITSE3 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000010110 000100100
///
/// description: Wait for SE3 event flag, then clear it. Prior SETQ sets optional CT timeout value. C/Z = timeout.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn waitse3(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    // codegen: begin:waitse3
    return waitEvent(cog, args, .SE3);
    // codegen: end:waitse3
}

/// WAITSE4 {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000010111 000100100
///
/// description: Wait for SE4 event flag, then clear it. Prior SETQ sets optional CT timeout value. C/Z = timeout.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn waitse4(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    // codegen: begin:waitse4
    return waitEvent(cog, args, .SE4);
    // codegen: end:waitse4
}

/// WAITPAT {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000011000 000100100
///
/// description: Wait for PAT event flag, then clear it. Prior SETQ sets optional CT timeout value. C/Z = timeout.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn waitpat(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    // codegen: begin:waitpat
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:waitpat
}

/// WAITFBW {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000011001 000100100
///
/// description: Wait for FBW event flag, then clear it. Prior SETQ sets optional CT timeout value. C/Z = timeout.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn waitfbw(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    // codegen: begin:waitfbw
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:waitfbw
}

/// WAITXMT {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000011010 000100100
///
/// description: Wait for XMT event flag, then clear it. Prior SETQ sets optional CT timeout value. C/Z = timeout.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn waitxmt(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    // codegen: begin:waitxmt
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:waitxmt
}

/// WAITXFI {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000011011 000100100
///
/// description: Wait for XFI event flag, then clear it. Prior SETQ sets optional CT timeout value. C/Z = timeout.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn waitxfi(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    // codegen: begin:waitxfi
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:waitxfi
}

/// WAITXRO {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000011100 000100100
///
/// description: Wait for XRO event flag, then clear it. Prior SETQ sets optional CT timeout value. C/Z = timeout.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn waitxro(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    // codegen: begin:waitxro
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:waitxro
}

/// WAITXRL {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000011101 000100100
///
/// description: Wait for XRL event flag, then clear it. Prior SETQ sets optional CT timeout value. C/Z = timeout.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn waitxrl(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    // codegen: begin:waitxrl
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:waitxrl
}

/// WAITATN {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 000011110 000100100
///
/// description: Wait for ATN event flag, then clear it. Prior SETQ sets optional CT timeout value. C/Z = timeout.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn waitatn(cog: *Cog, args: encoding.OnlyFlags) Cog.ExecResult {
    // codegen: begin:waitatn
    return waitEvent(cog, args, .ATN);
    // codegen: end:waitatn
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
    // codegen: begin:coginit
    const d = operandD(cog, args.d, args.d_imm);
    const source = operandS(cog, args.s, args.s_imm);
    const ptra = if (cog.setq_pending and !cog.q2) cog.q else 0;
    var selected: ?u3 = null;
    const pair = d & 0x10 != 0 and d & 1 != 0;
    if (d & 0x10 == 0) selected = @truncate(d) else {
        for (0..8) |index| {
            if (pair and index & 1 != 0) continue;
            if (cog.hub.cogs[index].exec_mode == .stopped and (!pair or cog.hub.cogs[index + 1].exec_mode == .stopped)) {
                selected = @intCast(index);
                break;
            }
        }
    }
    if (args.c_mod == .write) {
        cog.c = selected == null;
        if (!args.d_imm) cog.write_result(args.d, if (selected) |id| @as(u32, id) else 15);
    }
    if (!(cog.setq_pending and !cog.q2)) cog.q = 0;
    if (selected) |id| {
        const hub = cog.hub;
        hub.start_cog(id, .{ .hub_address = @truncate(source), .ptra = ptra, .load_image = d & 0x20 == 0 }) catch return .trap;
        hub.cogs[id].write_reg(.PTRB, source);
        if (pair) {
            hub.start_cog(id + 1, .{ .hub_address = @truncate(source), .ptra = ptra, .load_image = d & 0x20 == 0 }) catch return .trap;
            hub.cogs[id + 1].write_reg(.PTRB, source);
        }
        if (id == cog.id) cog.branched = true;
    }
    return .next;
    // codegen: end:coginit
}

/// COGID {#}D {WC}
/// EEEE 1101011 C0L DDDDDDDDD 000000001
///
/// description: If D is register and no WC, get cog ID (0 to 15) into D. If WC, check status of cog D[3:0], C = 1 if on.
/// cog timing:  2...9, +2 if result
/// hub timing:  same
/// access:      mem=None, reg=D if reg and !WC, stack=None
pub fn cogid(cog: *Cog, args: encoding.Only_Dimm_CFlag) Cog.ExecResult {
    // codegen: begin:cogid
    if (!cog.is_condition_met(args.cond))
        return .skip;

    if (args.c_mod == .write) {
        // If COGID is used with WC, it will not overwrite D, but will return the status of
        // cog D/# into C, where C=0 indicates the cog is free (stopped or never started)
        // and C=1 indicates the cog is busy (started).

        const id: u3 = @truncate(operandD(cog, args.d, args.d_imm));

        // COGID ThatCog WC ' C=1 if ThatCog is busy
        cog.c = (cog.hub.cogs[id].exec_mode != .stopped);
        return .next;
    } else {
        // A cog can discover its own ID by doing a COGID instruction, which will
        // return its ID into D[3:0], with upper bits cleared.
        // This is useful, in case the cog wants to restart or stop itself, as shown above.

        if (args.d_imm)
            return .trap; // TODO: Figure this out
        cog.write_result(args.d, cog.id);
        return .next;
    }
    // codegen: end:cogid
}

/// COGSTOP {#}D
/// EEEE 1101011 00L DDDDDDDDD 000000011
///
/// description: Stop cog D[3:0].
/// cog timing:  2...9
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn cogstop(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    // codegen: begin:cogstop
    if (!cog.is_condition_met(args.cond))
        return .skip;

    const id: u3 = @truncate(operandD(cog, args.d, args.d_imm));

    logger.info("stop cog {}", .{id});
    cog.hub.cogs[id].reset();

    return .next;
    // codegen: end:cogstop
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
    // codegen: begin:locknew
    for (&cog.hub.locks) |*lock| {
        if (!lock.allocated) {
            lock.allocated = true;
            cog.write_result(args.d, lock.id);
            if (args.c_mod == .write) cog.c = false;
            return .next;
        }
    }
    cog.write_result(args.d, 15);
    if (args.c_mod == .write) cog.c = true;
    return .next;
    // codegen: end:locknew
}

/// LOCKRET {#}D
/// EEEE 1101011 00L DDDDDDDDD 000000101
///
/// description: Return LOCK D[3:0] for reallocation.
/// cog timing:  2...9
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn lockret(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    // codegen: begin:lockret
    const id: u4 = @truncate(operandD(cog, args.d, args.d_imm));
    cog.hub.locks[id].allocated = false;
    return .next;
    // codegen: end:lockret
}

/// LOCKTRY {#}D {WC}
/// EEEE 1101011 C0L DDDDDDDDD 000000110
///
/// description: Try to get LOCK D[3:0]. C = 1 if got LOCK. LOCKREL releases LOCK. LOCK is also released if owner cog stops or restarts.
/// cog timing:  2...9, +2 if result
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn locktry(cog: *Cog, args: encoding.Only_Dimm_CFlag) Cog.ExecResult {
    // codegen: begin:locktry
    const id: u4 = @truncate(operandD(cog, args.d, args.d_imm));
    const lock = &cog.hub.locks[id];
    const success = lock.allocated and !lock.taken;
    if (success) {
        lock.taken = true;
        lock.owner = cog.id;
        cog.hub.signal_lock(id, true);
    }
    if (args.c_mod == .write) cog.c = success;
    return .next;
    // codegen: end:locktry
}

/// LOCKREL {#}D {WC}
/// EEEE 1101011 C0L DDDDDDDDD 000000111
///
/// description: Release LOCK D[3:0]. If D is a register and WC, get current/last cog ID of LOCK owner into D and LOCK status into C.
/// cog timing:  2...9, +2 if result
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn lockrel(cog: *Cog, args: encoding.Only_Dimm_CFlag) Cog.ExecResult {
    // codegen: begin:lockrel
    const id: u4 = @truncate(operandD(cog, args.d, args.d_imm));
    const lock = &cog.hub.locks[id];
    if (args.c_mod == .write) {
        cog.c = lock.taken;
        if (!args.d_imm) cog.write_result(args.d, lock.owner);
    }
    if (lock.taken and lock.owner == cog.id) cog.hub.release_lock(lock);
    return .next;
    // codegen: end:lockrel
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
    // codegen: begin:hubset
    const mode = operandD(cog, args.d, args.d_imm);
    // The 200 MHz crystal/PLL setup used by the terminal fixtures.
    if (mode != 0x0100_09fb) return .unsupported;
    cog.hub.io.clock_mode = mode;
    return .next;
    // codegen: end:hubset
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
    // codegen: begin:getptr
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:getptr
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
    // codegen: begin:fblock
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:fblock
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
    // codegen: begin:rdfast
    const config = operandD(cog, args.d, args.d_imm);
    const address = operandS(cog, args.s, args.s_imm);
    // Only unbounded read FIFO is implemented; block wrapping needs more work.
    if (config != 0) return .unsupported;
    cog.fifo_address = @truncate(address);
    return .next;
    // codegen: end:rdfast
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
    // codegen: begin:wrfast
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:wrfast
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
    // codegen: begin:rfbyte
    const address = cog.fifo_address orelse return .trap;
    const value = cog.hub.memory[address];
    cog.fifo_address = address +% 1;
    cog.write_result(args.d, value);
    if (args.c_mod == .write) cog.c = value & 0x80 != 0;
    if (args.z_mod == .write) cog.z = value == 0;
    return .next;
    // codegen: end:rfbyte
}

/// RFWORD D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000010001
///
/// description: Used after RDFAST. Read zero-extended word from FIFO into D. C = MSB of word. *
/// cog timing:  2
/// hub timing:  FIFO IN USE
/// access:      mem=Read, reg=D, stack=None
pub fn rfword(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    // codegen: begin:rfword
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:rfword
}

/// RFLONG D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000010010
///
/// description: Used after RDFAST. Read long from FIFO into D. C = MSB of long. *
/// cog timing:  2
/// hub timing:  FIFO IN USE
/// access:      mem=Read, reg=D, stack=None
pub fn rflong(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    // codegen: begin:rflong
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:rflong
}

/// RFVAR D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000010011
///
/// description: Used after RDFAST. Read zero-extended 1..4-byte value from FIFO into D. C = 0. *
/// cog timing:  2
/// hub timing:  FIFO IN USE
/// access:      mem=Read, reg=D, stack=None
pub fn rfvar(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    // codegen: begin:rfvar
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:rfvar
}

/// RFVARS D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000010100
///
/// description: Used after RDFAST. Read sign-extended 1..4-byte value from FIFO into D. C = MSB of value. *
/// cog timing:  2
/// hub timing:  FIFO IN USE
/// access:      mem=Read, reg=D, stack=None
pub fn rfvars(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    // codegen: begin:rfvars
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:rfvars
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
    // codegen: begin:wfbyte
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:wfbyte
}

/// WFWORD {#}D
/// EEEE 1101011 00L DDDDDDDDD 000010110
///
/// description: Used after WRFAST. Write word in D[15:0] into FIFO.
/// cog timing:  2
/// hub timing:  FIFO IN USE
/// access:      mem=Write, reg=None, stack=None
pub fn wfword(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    // codegen: begin:wfword
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:wfword
}

/// WFLONG {#}D
/// EEEE 1101011 00L DDDDDDDDD 000010111
///
/// description: Used after WRFAST. Write long in D[31:0] into FIFO.
/// cog timing:  2
/// hub timing:  FIFO IN USE
/// access:      mem=Write, reg=None, stack=None
pub fn wflong(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    // codegen: begin:wflong
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:wflong
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
    // codegen: begin:rdbyte
    return readMemory(cog, args, 1);
    // codegen: end:rdbyte
}

/// RDWORD D, {#}S/P {WC/WZ/WCZ}
/// EEEE 1010111 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Read zero-extended word from hub address {#}S/PTRx into D. C = MSB of word. *
/// cog timing:  9...16 *
/// hub timing:  9...26 *
/// access:      mem=Read, reg=D, stack=None
pub fn rdword(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    // codegen: begin:rdword
    return readMemory(cog, args, 2);
    // codegen: end:rdword
}

/// RDLONG D, {#}S/P {WC/WZ/WCZ}
/// EEEE 1011000 CZI DDDDDDDDD SSSSSSSSS
///
/// description: Read long from hub address {#}S/PTRx into D. C = MSB of long. *   Prior SETQ/SETQ2 invokes cog/LUT block transfer.
/// cog timing:  9...16 *
/// hub timing:  9...26 *
/// access:      mem=Read, reg=D, stack=None
pub fn rdlong(cog: *Cog, args: encoding.Both_D_Simm_Flags) Cog.ExecResult {
    // codegen: begin:rdlong
    return readMemory(cog, args, 4);
    // codegen: end:rdlong
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
    // codegen: begin:wmlong
    return writeMemory(cog, args, 4, true);
    // codegen: end:wmlong
}

/// WRBYTE {#}D, {#}S/P
/// EEEE 1100010 0LI DDDDDDDDD SSSSSSSSS
///
/// description: Write byte in D[7:0] to hub address {#}S/PTRx.
/// cog timing:  3...10
/// hub timing:  3...20
/// access:      mem=Write, reg=None, stack=None
pub fn wrbyte(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    // codegen: begin:wrbyte
    return writeMemory(cog, args, 1, false);
    // codegen: end:wrbyte
}

/// WRWORD {#}D, {#}S/P
/// EEEE 1100010 1LI DDDDDDDDD SSSSSSSSS
///
/// description: Write word in D[15:0] to hub address {#}S/PTRx.
/// cog timing:  3...10*
/// hub timing:  3...20 *
/// access:      mem=Write, reg=None, stack=None
pub fn wrword(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    // codegen: begin:wrword
    return writeMemory(cog, args, 2, false);
    // codegen: end:wrword
}

/// WRLONG {#}D, {#}S/P
/// EEEE 1100011 0LI DDDDDDDDD SSSSSSSSS
///
/// description: Write long in D[31:0] to hub address {#}S/PTRx.                   Prior SETQ/SETQ2 invokes cog/LUT block transfer.
/// cog timing:  3...10*
/// hub timing:  3...20 *
/// access:      mem=Write, reg=None, stack=None
pub fn wrlong(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    // codegen: begin:wrlong
    return writeMemory(cog, args, 4, false);
    // codegen: end:wrlong
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
    // codegen: begin:allowi
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:allowi
}

/// STALLI
/// EEEE 1101011 000 000100001 000100100
///
/// description: Stall Interrupts.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn stalli(cog: *Cog, args: encoding.NoOperands) Cog.ExecResult {
    // codegen: begin:stalli
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:stalli
}

/// TRGINT1
/// EEEE 1101011 000 000100010 000100100
///
/// description: Trigger INT1, regardless of STALLI mode.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn trgint1(cog: *Cog, args: encoding.NoOperands) Cog.ExecResult {
    // codegen: begin:trgint1
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:trgint1
}

/// TRGINT2
/// EEEE 1101011 000 000100011 000100100
///
/// description: Trigger INT2, regardless of STALLI mode.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn trgint2(cog: *Cog, args: encoding.NoOperands) Cog.ExecResult {
    // codegen: begin:trgint2
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:trgint2
}

/// TRGINT3
/// EEEE 1101011 000 000100100 000100100
///
/// description: Trigger INT3, regardless of STALLI mode.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn trgint3(cog: *Cog, args: encoding.NoOperands) Cog.ExecResult {
    // codegen: begin:trgint3
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:trgint3
}

/// NIXINT1
/// EEEE 1101011 000 000100101 000100100
///
/// description: Cancel INT1.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn nixint1(cog: *Cog, args: encoding.NoOperands) Cog.ExecResult {
    // codegen: begin:nixint1
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:nixint1
}

/// NIXINT2
/// EEEE 1101011 000 000100110 000100100
///
/// description: Cancel INT2.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn nixint2(cog: *Cog, args: encoding.NoOperands) Cog.ExecResult {
    // codegen: begin:nixint2
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:nixint2
}

/// NIXINT3
/// EEEE 1101011 000 000100111 000100100
///
/// description: Cancel INT3.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn nixint3(cog: *Cog, args: encoding.NoOperands) Cog.ExecResult {
    // codegen: begin:nixint3
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:nixint3
}

/// SETINT1 {#}D
/// EEEE 1101011 00L DDDDDDDDD 000100101
///
/// description: Set INT1 source to D[3:0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setint1(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    // codegen: begin:setint1
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:setint1
}

/// SETINT2 {#}D
/// EEEE 1101011 00L DDDDDDDDD 000100110
///
/// description: Set INT2 source to D[3:0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setint2(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    // codegen: begin:setint2
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:setint2
}

/// SETINT3 {#}D
/// EEEE 1101011 00L DDDDDDDDD 000100111
///
/// description: Set INT3 source to D[3:0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setint3(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    // codegen: begin:setint3
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:setint3
}

/// GETBRK D WC/WZ/WCZ
/// EEEE 1101011 CZ0 DDDDDDDDD 000110101
///
/// description: Get breakpoint/cog status into D according to WC/WZ/WCZ. See documentation for details.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn getbrk(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    // codegen: begin:getbrk
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:getbrk
}

/// COGBRK {#}D
/// EEEE 1101011 00L DDDDDDDDD 000110101
///
/// description: If in debug ISR, trigger asynchronous breakpoint in cog D[3:0]. Cog D[3:0] must have asynchronous breakpoint enabled.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn cogbrk(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    // codegen: begin:cogbrk
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:cogbrk
}

/// BRK {#}D
/// EEEE 1101011 00L DDDDDDDDD 000110110
///
/// description: If in debug ISR, set next break condition to D. Else, set BRK code to D[7:0] and unconditionally trigger BRK interrupt, if enabled.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn brk(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    // codegen: begin:brk
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:brk
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
    // codegen: begin:rdlut
    const address: u9 = @truncate(memoryAddress(cog, args.s, args.s_imm, 1, false));
    const value = cog.read_lut(address);
    cog.q = value;
    cog.write_result(args.d, value);
    setFlags(cog, args, value, 31);
    return .next;
    // codegen: end:rdlut
}

/// WRLUT {#}D, {#}S/P
/// EEEE 1100001 1LI DDDDDDDDD SSSSSSSSS
///
/// description: Write D to LUT address {#}S/PTRx.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn wrlut(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    // codegen: begin:wrlut
    const value = operandD(cog, args.d, args.d_imm);
    const address: u9 = @truncate(memoryAddress(cog, args.s, args.s_imm, 1, false));
    cog.write_lut(address, value);
    return .next;
    // codegen: end:wrlut
}

/// SETLUTS {#}D
/// EEEE 1101011 00L DDDDDDDDD 000110111
///
/// description: If D[0] = 1 then enable LUT sharing, where LUT writes within the adjacent odd/even companion cog are copied to this cog's LUT.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setluts(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    // codegen: begin:setluts
    cog.lut_sharing = operandD(cog, args.d, args.d_imm) & 1 != 0;
    return .next;
    // codegen: end:setluts
}

//
// GROUP: Math and Logic
//

/// LOC PA/PB/PTRA/PTRB, #{\}A
/// EEEE 11101WW RAA AAAAAAAAA AAAAAAAAA
///
/// description: Get {12'b0, address[19:0]} into PA/PB/PTRA/PTRB (per W).          If R = 1, address = PC + A, else address = A. "\" forces R = 0.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=Per W, stack=None
pub fn loc(cog: *Cog, args: encoding.LocStyle) Cog.ExecResult {
    // codegen: begin:loc
    const value = branchA(cog, .{ .cond = args.cond, .relative = args.relative, .address = args.address, ._mask1 = 0 });
    cog.write_result(pointerReg(args.pointer), value);
    return .next;
    // codegen: end:loc
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
    // codegen: begin:nop
    _ = cog;
    _ = args;
    return .next;
    // codegen: end:nop
}

/// GETCT D {WC}
/// EEEE 1101011 C00 DDDDDDDDD 000011010
///
/// description: Get CT[31:0] or CT[63:32] if WC into D. GETCT WC + GETCT captures entire CT. CT=0 on reset, CT++ on every clock. C = same.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn getct(cog: *Cog, args: encoding.Only_D_CFlag) Cog.ExecResult {
    // codegen: begin:getct
    const value: u32 = if (args.c_mod == .write) @truncate(cog.hub.counter >> 32) else cog.ct_low orelse @as(u32, @truncate(cog.hub.counter));
    cog.ct_low = if (args.c_mod == .write) @truncate(cog.hub.counter) else null;
    cog.write_result(args.d, value);
    return .next;
    // codegen: end:getct
}

/// GETRND D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000011011
///
/// description: Get RND into D/C/Z. RND is the PRNG that updates on every clock. D = RND[31:0], C = RND[31], Z = RND[30], unique per cog.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn getrnd(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    // codegen: begin:getrnd
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:getrnd
}

/// WAITX {#}D {WC/WZ/WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 000011111
///
/// description: Wait 2 + D clocks if no WC/WZ/WCZ. If WC/WZ/WCZ, wait 2 + (D & RND) clocks. C/Z = 0.
/// cog timing:  2 + D
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn waitx(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:waitx
    if (args.c_mod == .write or args.z_mod == .write) return .unsupported;
    if (cog.wait_until == null) cog.wait_until = cog.hub.counter + 2 + @as(u64, operandD(cog, args.d, args.d_imm));
    if (cog.hub.counter < cog.wait_until.?) return .wait;
    cog.wait_until = null;
    return .next;
    // codegen: end:waitx
}

/// SETQ {#}D
/// EEEE 1101011 00L DDDDDDDDD 000101000
///
/// description: Set Q to D. Use before RDLONG/WRLONG/WMLONG to set block transfer. Also used before MUXQ/COGINIT/QDIV/QFRAC/QROTATE/WAITxxx.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setq(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    // codegen: begin:setq
    cog.q = operandD(cog, args.d, args.d_imm);
    cog.setq_pending = true;
    cog.q2 = false;
    cog.block_pointer_delta = true;
    return .next;
    // codegen: end:setq
}

/// SETQ2 {#}D
/// EEEE 1101011 00L DDDDDDDDD 000101001
///
/// description: Set Q to D. Use before RDLONG/WRLONG/WMLONG to set LUT block transfer.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setq2(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    // codegen: begin:setq2
    cog.q = operandD(cog, args.d, args.d_imm);
    cog.setq_pending = true;
    cog.q2 = true;
    cog.block_pointer_delta = true;
    return .next;
    // codegen: end:setq2
}

/// PUSH {#}D
/// EEEE 1101011 00L DDDDDDDDD 000101010
///
/// description: Push D onto stack.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=Push
pub fn push(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    // codegen: begin:push
    cog.push(operandD(cog, args.d, args.d_imm));
    return .next;
    // codegen: end:push
}

/// POP D {WC/WZ/WCZ}
/// EEEE 1101011 CZ0 DDDDDDDDD 000101011
///
/// description: Pop stack (K). D = K. C = K[31]. *
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=Pop
pub fn pop(cog: *Cog, args: encoding.Only_D_Flags) Cog.ExecResult {
    // codegen: begin:pop
    const value = cog.pop();
    cog.write_result(args.d, value);
    if (args.c_mod == .write) cog.c = value >> 31 != 0;
    if (args.z_mod == .write) cog.z = value == 0;
    return .next;
    // codegen: end:pop
}

/// AUGS #n
/// EEEE 11110nn nnn nnnnnnnnn nnnnnnnnn
///
/// description: Queue #n to be used as upper 23 bits for next #S occurrence, so that the next 9-bit #S will be augmented to 32 bits.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn augs(cog: *Cog, args: encoding.Augment) Cog.ExecResult {
    // codegen: begin:augs
    if (!cog.is_condition_met(args.cond))
        return .skip;
    cog.augs = @as(u32, args.augment) << 9;
    cog.augs_pending = true;
    return .next;
    // codegen: end:augs
}

/// AUGD #n
/// EEEE 11111nn nnn nnnnnnnnn nnnnnnnnn
///
/// description: Queue #n to be used as upper 23 bits for next #D occurrence, so that the next 9-bit #D will be augmented to 32 bits.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn augd(cog: *Cog, args: encoding.Augment) Cog.ExecResult {
    // codegen: begin:augd
    if (!cog.is_condition_met(args.cond))
        return .skip;
    cog.augd = @as(u32, args.augment) << 9;
    return .next;
    // codegen: end:augd
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
    // codegen: begin:testp
    const pin = operandD(cog, args.d, args.d_imm) & 63;
    if (pin != 62 and pin != 63) return .unsupported;
    const value = cog.hub.io.pins[pin].ready;
    if (args.c_mod == .write) cog.c = value;
    if (args.z_mod == .write) cog.z = value;
    return .next;
    // codegen: end:testp
}

/// TESTPN {#}D WC/WZ
/// EEEE 1101011 CZL DDDDDDDDD 001000001
///
/// description: Test !IN bit of pin D[5:0], write to C/Z. C/Z =         !IN[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn testpn(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:testpn
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:testpn
}

/// TESTP {#}D ANDC/ANDZ
/// EEEE 1101011 CZL DDDDDDDDD 001000010
///
/// description: Test  IN bit of pin D[5:0], AND into C/Z. C/Z = C/Z AND  IN[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn testp_and(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:testp_and
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:testp_and
}

/// TESTPN {#}D ANDC/ANDZ
/// EEEE 1101011 CZL DDDDDDDDD 001000011
///
/// description: Test !IN bit of pin D[5:0], AND into C/Z. C/Z = C/Z AND !IN[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn testpn_and(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:testpn_and
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:testpn_and
}

/// TESTP {#}D ORC/ORZ
/// EEEE 1101011 CZL DDDDDDDDD 001000100
///
/// description: Test  IN bit of pin D[5:0], OR  into C/Z. C/Z = C/Z OR   IN[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn testp_or(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:testp_or
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:testp_or
}

/// TESTPN {#}D ORC/ORZ
/// EEEE 1101011 CZL DDDDDDDDD 001000101
///
/// description: Test !IN bit of pin D[5:0], OR  into C/Z. C/Z = C/Z OR  !IN[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn testpn_or(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:testpn_or
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:testpn_or
}

/// TESTP {#}D XORC/XORZ
/// EEEE 1101011 CZL DDDDDDDDD 001000110
///
/// description: Test  IN bit of pin D[5:0], XOR into C/Z. C/Z = C/Z XOR  IN[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn testp_xor(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:testp_xor
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:testp_xor
}

/// TESTPN {#}D XORC/XORZ
/// EEEE 1101011 CZL DDDDDDDDD 001000111
///
/// description: Test !IN bit of pin D[5:0], XOR into C/Z. C/Z = C/Z XOR !IN[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn testpn_xor(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:testpn_xor
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:testpn_xor
}

/// DIRL {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001000000
///
/// description: DIR bits of pins D[10:6]+D[5:0]..D[5:0] = 0.                  Wraps within DIRA/DIRB. Prior SETQ overrides D[10:6]. C,Z = DIR[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx, stack=None
pub fn dirl(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:dirl
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:dirl
}

/// DIRH {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001000001
///
/// description: DIR bits of pins D[10:6]+D[5:0]..D[5:0] = 1.                  Wraps within DIRA/DIRB. Prior SETQ overrides D[10:6]. C,Z = DIR[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx, stack=None
pub fn dirh(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:dirh
    const pin = operandD(cog, args.d, args.d_imm);
    if (pin != 62 and pin != 63) return .unsupported;
    const reg: Cog.Register = .DIRB;
    cog.write_reg(reg, cog.read_reg(reg) | (@as(u32, 1) << @as(u5, @intCast(pin - 32))));
    cog.hub.io.pins[pin].enabled = true;
    if (args.c_mod == .write) cog.c = true;
    if (args.z_mod == .write) cog.z = true;
    return .next;
    // codegen: end:dirh
}

/// DIRC {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001000010
///
/// description: DIR bits of pins D[10:6]+D[5:0]..D[5:0] = C.                  Wraps within DIRA/DIRB. Prior SETQ overrides D[10:6]. C,Z = DIR[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx, stack=None
pub fn dirc(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:dirc
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:dirc
}

/// DIRNC {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001000011
///
/// description: DIR bits of pins D[10:6]+D[5:0]..D[5:0] = !C.                 Wraps within DIRA/DIRB. Prior SETQ overrides D[10:6]. C,Z = DIR[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx, stack=None
pub fn dirnc(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:dirnc
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:dirnc
}

/// DIRZ {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001000100
///
/// description: DIR bits of pins D[10:6]+D[5:0]..D[5:0] = Z.                  Wraps within DIRA/DIRB. Prior SETQ overrides D[10:6]. C,Z = DIR[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx, stack=None
pub fn dirz(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:dirz
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:dirz
}

/// DIRNZ {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001000101
///
/// description: DIR bits of pins D[10:6]+D[5:0]..D[5:0] = !Z.                 Wraps within DIRA/DIRB. Prior SETQ overrides D[10:6]. C,Z = DIR[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx, stack=None
pub fn dirnz(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:dirnz
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:dirnz
}

/// DIRRND {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001000110
///
/// description: DIR bits of pins D[10:6]+D[5:0]..D[5:0] = RNDs.               Wraps within DIRA/DIRB. Prior SETQ overrides D[10:6]. C,Z = DIR[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx, stack=None
pub fn dirrnd(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:dirrnd
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:dirrnd
}

/// DIRNOT {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001000111
///
/// description: Toggle DIR bits of pins D[10:6]+D[5:0]..D[5:0].               Wraps within DIRA/DIRB. Prior SETQ overrides D[10:6]. C,Z = DIR[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx, stack=None
pub fn dirnot(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:dirnot
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:dirnot
}

/// OUTL {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001001000
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = 0.                  Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=OUTx, stack=None
pub fn outl(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:outl
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:outl
}

/// OUTH {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001001001
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = 1.                  Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=OUTx, stack=None
pub fn outh(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:outh
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:outh
}

/// OUTC {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001001010
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = C.                  Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=OUTx, stack=None
pub fn outc(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:outc
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:outc
}

/// OUTNC {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001001011
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = !C.                 Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=OUTx, stack=None
pub fn outnc(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:outnc
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:outnc
}

/// OUTZ {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001001100
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = Z.                  Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=OUTx, stack=None
pub fn outz(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:outz
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:outz
}

/// OUTNZ {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001001101
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = !Z.                 Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=OUTx, stack=None
pub fn outnz(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:outnz
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:outnz
}

/// OUTRND {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001001110
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = RNDs.               Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=OUTx, stack=None
pub fn outrnd(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:outrnd
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:outrnd
}

/// OUTNOT {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001001111
///
/// description: Toggle OUT bits of pins D[10:6]+D[5:0]..D[5:0].               Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=OUTx, stack=None
pub fn outnot(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:outnot
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:outnot
}

/// FLTL {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001010000
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = 0.    DIR bits = 0. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn fltl(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:fltl
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:fltl
}

/// FLTH {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001010001
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = 1.    DIR bits = 0. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn flth(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:flth
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:flth
}

/// FLTC {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001010010
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = C.    DIR bits = 0. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn fltc(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:fltc
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:fltc
}

/// FLTNC {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001010011
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = !C.   DIR bits = 0. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn fltnc(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:fltnc
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:fltnc
}

/// FLTZ {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001010100
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = Z.    DIR bits = 0. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn fltz(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:fltz
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:fltz
}

/// FLTNZ {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001010101
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = !Z.   DIR bits = 0. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn fltnz(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:fltnz
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:fltnz
}

/// FLTRND {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001010110
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = RNDs. DIR bits = 0. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn fltrnd(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:fltrnd
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:fltrnd
}

/// FLTNOT {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001010111
///
/// description: Toggle OUT bits of pins D[10:6]+D[5:0]..D[5:0]. DIR bits = 0. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn fltnot(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:fltnot
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:fltnot
}

/// DRVL {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001011000
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = 0.    DIR bits = 1. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn drvl(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:drvl
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:drvl
}

/// DRVH {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001011001
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = 1.    DIR bits = 1. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn drvh(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:drvh
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:drvh
}

/// DRVC {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001011010
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = C.    DIR bits = 1. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn drvc(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:drvc
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:drvc
}

/// DRVNC {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001011011
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = !C.   DIR bits = 1. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn drvnc(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:drvnc
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:drvnc
}

/// DRVZ {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001011100
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = Z.    DIR bits = 1. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn drvz(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:drvz
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:drvz
}

/// DRVNZ {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001011101
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = !Z.   DIR bits = 1. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn drvnz(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:drvnz
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:drvnz
}

/// DRVRND {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001011110
///
/// description: OUT bits of pins D[10:6]+D[5:0]..D[5:0] = RNDs. DIR bits = 1. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn drvrnd(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:drvrnd
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:drvrnd
}

/// DRVNOT {#}D {WCZ}
/// EEEE 1101011 CZL DDDDDDDDD 001011111
///
/// description: Toggle OUT bits of pins D[10:6]+D[5:0]..D[5:0]. DIR bits = 1. Wraps within OUTA/OUTB. Prior SETQ overrides D[10:6]. C,Z = OUT[D[5:0]].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=DIRx* + OUTx, stack=None
pub fn drvnot(cog: *Cog, args: encoding.Only_Dimm_Flags) Cog.ExecResult {
    // codegen: begin:drvnot
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:drvnot
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
    // codegen: begin:addpix
    return pixel(cog, args, .add);
    // codegen: end:addpix
}

/// MULPIX D, {#}S
/// EEEE 1010010 01I DDDDDDDDD SSSSSSSSS
///
/// description: Multiply bytes of S into bytes of D, where $FF = 1.0 and $00 = 0.0.
/// cog timing:  7
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn mulpix(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    // codegen: begin:mulpix
    return pixel(cog, args, .multiply);
    // codegen: end:mulpix
}

/// BLNPIX D, {#}S
/// EEEE 1010010 10I DDDDDDDDD SSSSSSSSS
///
/// description: Alpha-blend bytes of S into bytes of D, using SETPIV value.
/// cog timing:  7
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn blnpix(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    // codegen: begin:blnpix
    return pixel(cog, args, .blend);
    // codegen: end:blnpix
}

/// MIXPIX D, {#}S
/// EEEE 1010010 11I DDDDDDDDD SSSSSSSSS
///
/// description: Mix bytes of S into bytes of D, using SETPIX and SETPIV values.
/// cog timing:  7
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn mixpix(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    // codegen: begin:mixpix
    return pixel(cog, args, .mix);
    // codegen: end:mixpix
}

/// SETPIV {#}D
/// EEEE 1101011 00L DDDDDDDDD 000111101
///
/// description: Set BLNPIX/MIXPIX blend factor to D[7:0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setpiv(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    // codegen: begin:setpiv
    cog.pixel_pivot = @truncate(operandD(cog, args.d, args.d_imm));
    return .next;
    // codegen: end:setpiv
}

/// SETPIX {#}D
/// EEEE 1101011 00L DDDDDDDDD 000111110
///
/// description: Set MIXPIX mode to D[5:0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setpix(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    // codegen: begin:setpix
    cog.pixel_mode = @truncate(operandD(cog, args.d, args.d_imm));
    return .next;
    // codegen: end:setpix
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
    // codegen: begin:altsn
    return alter(cog, args, "alt_d", 3);
    // codegen: end:altsn
}

/// ALTGN D, {#}S
/// EEEE 1001010 11I DDDDDDDDD SSSSSSSSS
///
/// description: Alter subsequent GETNIB/ROLNIB instruction. Next S field = (D[11:3] + S) & $1FF, N field = D[2:0].   D += sign-extended S[17:9].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn altgn(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    // codegen: begin:altgn
    return alter(cog, args, "alt_s", 3);
    // codegen: end:altgn
}

/// ALTSB D, {#}S
/// EEEE 1001011 00I DDDDDDDDD SSSSSSSSS
///
/// description: Alter subsequent SETBYTE instruction. Next D field = (D[10:2] + S) & $1FF, N field = D[1:0].         D += sign-extended S[17:9].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn altsb(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    // codegen: begin:altsb
    return alter(cog, args, "alt_d", 2);
    // codegen: end:altsb
}

/// ALTGB D, {#}S
/// EEEE 1001011 01I DDDDDDDDD SSSSSSSSS
///
/// description: Alter subsequent GETBYTE/ROLBYTE instruction. Next S field = (D[10:2] + S) & $1FF, N field = D[1:0]. D += sign-extended S[17:9].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn altgb(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    // codegen: begin:altgb
    return alter(cog, args, "alt_s", 2);
    // codegen: end:altgb
}

/// ALTSW D, {#}S
/// EEEE 1001011 10I DDDDDDDDD SSSSSSSSS
///
/// description: Alter subsequent SETWORD instruction. Next D field = (D[9:1] + S) & $1FF, N field = D[0].            D += sign-extended S[17:9].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn altsw(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    // codegen: begin:altsw
    return alter(cog, args, "alt_d", 1);
    // codegen: end:altsw
}

/// ALTGW D, {#}S
/// EEEE 1001011 11I DDDDDDDDD SSSSSSSSS
///
/// description: Alter subsequent GETWORD/ROLWORD instruction. Next S field = ((D[9:1] + S) & $1FF), N field = D[0].  D += sign-extended S[17:9].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn altgw(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    // codegen: begin:altgw
    return alter(cog, args, "alt_s", 1);
    // codegen: end:altgw
}

/// ALTR D, {#}S
/// EEEE 1001100 00I DDDDDDDDD SSSSSSSSS
///
/// description: Alter result register address (normally D field) of next instruction to (D + S) & $1FF.              D += sign-extended S[17:9].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn altr(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    // codegen: begin:altr
    return alter(cog, args, "alt_r", 0);
    // codegen: end:altr
}

/// ALTD D, {#}S
/// EEEE 1001100 01I DDDDDDDDD SSSSSSSSS
///
/// description: Alter D field of next instruction to (D + S) & $1FF.                                                 D += sign-extended S[17:9].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn altd(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    // codegen: begin:altd
    return alter(cog, args, "alt_d", 0);
    // codegen: end:altd
}

/// ALTS D, {#}S
/// EEEE 1001100 10I DDDDDDDDD SSSSSSSSS
///
/// description: Alter S field of next instruction to (D + S) & $1FF.                                                 D += sign-extended S[17:9].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn alts(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    // codegen: begin:alts
    return alter(cog, args, "alt_s", 0);
    // codegen: end:alts
}

/// ALTB D, {#}S
/// EEEE 1001100 11I DDDDDDDDD SSSSSSSSS
///
/// description: Alter D field of next instruction to (D[13:5] + S) & $1FF.                                           D += sign-extended S[17:9].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn altb(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    // codegen: begin:altb
    return alter(cog, args, "alt_d", 5);
    // codegen: end:altb
}

/// ALTI D, {#}S
/// EEEE 1001101 00I DDDDDDDDD SSSSSSSSS
///
/// description: Substitute next instruction's I/R/D/S fields with fields from D, per S. Modify D per S.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn alti(cog: *Cog, args: encoding.Both_D_Simm) Cog.ExecResult {
    // codegen: begin:alti
    const d = cog.read_reg(args.d);
    const saved = cog.augs;
    const pending = cog.augs_pending;
    const mode = operandS(cog, args.s, args.s_imm);
    cog.augs = saved;
    cog.augs_pending = pending;
    const rmode = (mode >> 6) & 7;
    if (cog.next_instruction) |*next| {
        if (rmode == 1) next.no_result = true;
        if (rmode == 5) {
            next.instr = (next.instr & 0x3ffff) | (d & 0xfffc0000);
            if (mode & 0x20 != 0) next.instr = (next.instr & ~@as(u32, 0x3fe00)) | (d & 0x3fe00);
            if (mode & 4 != 0) next.instr = (next.instr & ~@as(u32, 0x1ff)) | (d & 0x1ff);
        } else {
            if (rmode & 4 != 0) next.alt_r = @enumFromInt(@as(u9, @truncate(d >> 19)));
            if (mode & 0x20 != 0) next.alt_d = @enumFromInt(@as(u9, @truncate(d >> 9)));
            if (mode & 4 != 0) next.alt_s = @enumFromInt(@as(u9, @truncate(d)));
        }
    }
    var value = d;
    inline for (.{ .{ 0, 0 }, .{ 9, 3 }, .{ 19, 6 } }) |field| {
        const control = (mode >> field[1]) & 7;
        if (control & 2 != 0) {
            const width: u5 = @intCast(9 - ((mode >> (field[1] + 9)) & 7));
            const mask = ((@as(u32, 1) << width) - 1) << field[0];
            const delta: u32 = @as(u32, 1) << field[0];
            const modified = if (control & 1 != 0) d +% delta else d -% delta;
            value = (value & ~mask) | (modified & mask);
        }
    }
    cog.write_result(args.d, value);
    return .next;
    // codegen: end:alti
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
    // codegen: begin:rqpin
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:rqpin
}

/// RDPIN D, {#}S {WC}
/// EEEE 1010100 C1I DDDDDDDDD SSSSSSSSS
///
/// description: Read smart pin S[5:0] result "Z" into D, acknowledge pin.                                    C = modal result.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn rdpin(cog: *Cog, args: encoding.Both_D_Simm_CFlag) Cog.ExecResult {
    // codegen: begin:rdpin
    const pin = operandS(cog, args.s, args.s_imm) & 63;
    if (pin != 62 and pin != 63) return .unsupported;
    cog.write_result(args.d, cog.hub.io.pins[pin].result);
    if (args.c_mod == .write) cog.c = pin == 62 and cog.hub.io.txBusy();
    cog.hub.io.pins[pin].ready = false;
    return .next;
    // codegen: end:rdpin
}

/// WRPIN {#}D, {#}S
/// EEEE 1100000 0LI DDDDDDDDD SSSSSSSSS
///
/// description: Set mode of smart pins S[10:6]+S[5:0]..S[5:0] to D, acknowledge pins. Wraps within A/B pins. Prior SETQ D[4:0] overrides S[10:6].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn wrpin(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    // codegen: begin:wrpin
    const mode = operandD(cog, args.d, args.d_imm);
    const pin = operandS(cog, args.s, args.s_imm);
    if (!((pin == 62 and mode == 0x7c) or (pin == 63 and mode == 0x3e))) return .unsupported;
    cog.hub.io.pins[pin].mode = mode;
    cog.hub.io.pins[pin].ready = false;
    return .next;
    // codegen: end:wrpin
}

/// WXPIN {#}D, {#}S
/// EEEE 1100000 1LI DDDDDDDDD SSSSSSSSS
///
/// description: Set "X"  of smart pins S[10:6]+S[5:0]..S[5:0] to D, acknowledge pins. Wraps within A/B pins. Prior SETQ D[4:0] overrides S[10:6].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn wxpin(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    // codegen: begin:wxpin
    const x = operandD(cog, args.d, args.d_imm);
    const pin = operandS(cog, args.s, args.s_imm);
    if ((pin != 62 and pin != 63) or x & 31 != 7 or x >> 16 == 0) return .unsupported;
    cog.hub.io.pins[pin].x = x;
    cog.hub.io.pins[pin].ready = false;
    return .next;
    // codegen: end:wxpin
}

/// WYPIN {#}D, {#}S
/// EEEE 1100001 0LI DDDDDDDDD SSSSSSSSS
///
/// description: Set "Y"  of smart pins S[10:6]+S[5:0]..S[5:0] to D, acknowledge pins. Wraps within A/B pins. Prior SETQ D[4:0] overrides S[10:6].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn wypin(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    // codegen: begin:wypin
    const data = operandD(cog, args.d, args.d_imm);
    const pin = operandS(cog, args.s, args.s_imm);
    if (pin != 62 or !cog.hub.io.transmit(data)) return .unsupported;
    return .next;
    // codegen: end:wypin
}

/// SETDACS {#}D
/// EEEE 1101011 00L DDDDDDDDD 000011100
///
/// description: DAC3 = D[31:24], DAC2 = D[23:16], DAC1 = D[15:8], DAC0 = D[7:0].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setdacs(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    // codegen: begin:setdacs
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:setdacs
}

/// SETSCP {#}D
/// EEEE 1101011 00L DDDDDDDDD 001110000
///
/// description: Set four-channel oscilloscope enable to D[6] and set input pin base to D[5:2].
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setscp(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    // codegen: begin:setscp
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:setscp
}

/// GETSCP D
/// EEEE 1101011 000 DDDDDDDDD 001110001
///
/// description: Get four-channel oscilloscope samples into D. D = {ch3[7:0],ch2[7:0],ch1[7:0],ch0[7:0]}.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn getscp(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    // codegen: begin:getscp
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:getscp
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
    // codegen: begin:xinit
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:xinit
}

/// XZERO {#}D, {#}S
/// EEEE 1100101 1LI DDDDDDDDD SSSSSSSSS
///
/// description: Buffer new streamer command to be issued on final NCO rollover of current command, zeroing phase. Prior SETQ sets NCO frequency.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn xzero(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    // codegen: begin:xzero
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:xzero
}

/// XCONT {#}D, {#}S
/// EEEE 1100110 0LI DDDDDDDDD SSSSSSSSS
///
/// description: Buffer new streamer command to be issued on final NCO rollover of current command, continuing phase. Prior SETQ sets NCO frequency.
/// cog timing:  2+
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn xcont(cog: *Cog, args: encoding.Both_Dimm_Simm) Cog.ExecResult {
    // codegen: begin:xcont
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:xcont
}

/// SETXFRQ {#}D
/// EEEE 1101011 00L DDDDDDDDD 000011101
///
/// description: Set streamer NCO frequency to D.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=None, stack=None
pub fn setxfrq(cog: *Cog, args: encoding.Only_Dimm) Cog.ExecResult {
    // codegen: begin:setxfrq
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:setxfrq
}

/// GETXACC D
/// EEEE 1101011 000 DDDDDDDDD 000011110
///
/// description: Get the streamer's Goertzel X accumulator into D and the Y accumulator into the next instruction's S, clear accumulators.
/// cog timing:  2
/// hub timing:  same
/// access:      mem=None, reg=D, stack=None
pub fn getxacc(cog: *Cog, args: encoding.Only_D) Cog.ExecResult {
    // codegen: begin:getxacc
    _ = cog;
    _ = args;
    return .unsupported;
    // return .next;
    // codegen: end:getxacc
}

test "all deterministic ALU opcodes delegate to libp2" {
    try test_alu_dispatch(.ror, 0x00000000, alu.ROR, true);
    try test_alu_dispatch(.rol, 0x00200000, alu.ROL, true);
    try test_alu_dispatch(.shr, 0x00400000, alu.SHR, true);
    try test_alu_dispatch(.shl, 0x00600000, alu.SHL, true);
    try test_alu_dispatch(.rcr, 0x00800000, alu.RCR, true);
    try test_alu_dispatch(.rcl, 0x00A00000, alu.RCL, true);
    try test_alu_dispatch(.sar, 0x00C00000, alu.SAR, true);
    try test_alu_dispatch(.sal, 0x00E00000, alu.SAL, true);
    try test_alu_dispatch(.add, 0x01000000, alu.ADD, true);
    try test_alu_dispatch(.addx, 0x01200000, alu.ADDX, true);
    try test_alu_dispatch(.adds, 0x01400000, alu.ADDS, true);
    try test_alu_dispatch(.addsx, 0x01600000, alu.ADDSX, true);
    try test_alu_dispatch(.sub, 0x01800000, alu.SUB, true);
    try test_alu_dispatch(.subx, 0x01A00000, alu.SUBX, true);
    try test_alu_dispatch(.subs, 0x01C00000, alu.SUBS, true);
    try test_alu_dispatch(.subsx, 0x01E00000, alu.SUBSX, true);
    try test_alu_dispatch(.cmp, 0x02000000, alu.CMP, false);
    try test_alu_dispatch(.cmpx, 0x02200000, alu.CMPX, false);
    try test_alu_dispatch(.cmps, 0x02400000, alu.CMPS, false);
    try test_alu_dispatch(.cmpsx, 0x02600000, alu.CMPSX, false);
    try test_alu_dispatch(.cmpr, 0x02800000, alu.CMPR, false);
    try test_alu_dispatch(.cmpm, 0x02A00000, alu.CMPM, false);
    try test_alu_dispatch(.subr, 0x02C00000, alu.SUBR, true);
    try test_alu_dispatch(.cmpsub, 0x02E00000, alu.CMPSUB, true);
    try test_alu_dispatch(.fge, 0x03000000, alu.FGE, true);
    try test_alu_dispatch(.fle, 0x03200000, alu.FLE, true);
    try test_alu_dispatch(.fges, 0x03400000, alu.FGES, true);
    try test_alu_dispatch(.fles, 0x03600000, alu.FLES, true);
    try test_alu_dispatch(.sumc, 0x03800000, alu.SUMC, true);
    try test_alu_dispatch(.sumnc, 0x03A00000, alu.SUMNC, true);
    try test_alu_dispatch(.sumz, 0x03C00000, alu.SUMZ, true);
    try test_alu_dispatch(.sumnz, 0x03E00000, alu.SUMNZ, true);
    try test_alu_dispatch(.testb, 0x04000000, alu.TESTB, false);
    try test_alu_dispatch(.testbn, 0x04200000, alu.TESTBN, false);
    try test_alu_dispatch(.testb_and, 0x04400000, alu.TESTB_AND, false);
    try test_alu_dispatch(.testbn_and, 0x04600000, alu.TESTBN_AND, false);
    try test_alu_dispatch(.testb_or, 0x04800000, alu.TESTB_OR, false);
    try test_alu_dispatch(.testbn_or, 0x04A00000, alu.TESTBN_OR, false);
    try test_alu_dispatch(.testb_xor, 0x04C00000, alu.TESTB_XOR, false);
    try test_alu_dispatch(.testbn_xor, 0x04E00000, alu.TESTBN_XOR, false);
    try test_alu_dispatch(.bitl, 0x04000000, alu.BITL, true);
    try test_alu_dispatch(.bith, 0x04200000, alu.BITH, true);
    try test_alu_dispatch(.bitc, 0x04400000, alu.BITC, true);
    try test_alu_dispatch(.bitnc, 0x04600000, alu.BITNC, true);
    try test_alu_dispatch(.bitz, 0x04800000, alu.BITZ, true);
    try test_alu_dispatch(.bitnz, 0x04A00000, alu.BITNZ, true);
    try test_alu_dispatch(.bitnot, 0x04E00000, alu.BITNOT, true);
    try test_alu_dispatch(.@"and", 0x05000000, alu.AND, true);
    try test_alu_dispatch(.andn, 0x05200000, alu.ANDN, true);
    try test_alu_dispatch(.@"or", 0x05400000, alu.OR, true);
    try test_alu_dispatch(.xor, 0x05600000, alu.XOR, true);
    try test_alu_dispatch(.muxc, 0x05800000, alu.MUXC, true);
    try test_alu_dispatch(.muxnc, 0x05A00000, alu.MUXNC, true);
    try test_alu_dispatch(.muxz, 0x05C00000, alu.MUXZ, true);
    try test_alu_dispatch(.muxnz, 0x05E00000, alu.MUXNZ, true);
    try test_alu_dispatch(.mov, 0x06000000, alu.MOV, true);
    try test_alu_dispatch(.not, 0x06200000, alu.NOT, true);
    try test_alu_dispatch(.abs, 0x06400000, alu.ABS, true);
    try test_alu_dispatch(.neg, 0x06600000, alu.NEG, true);
    try test_alu_dispatch(.negc, 0x06800000, alu.NEGC, true);
    try test_alu_dispatch(.negnc, 0x06A00000, alu.NEGNC, true);
    try test_alu_dispatch(.negz, 0x06C00000, alu.NEGZ, true);
    try test_alu_dispatch(.negnz, 0x06E00000, alu.NEGNZ, true);
    try test_alu_dispatch(.incmod, 0x07000000, alu.INCMOD, true);
    try test_alu_dispatch(.decmod, 0x07200000, alu.DECMOD, true);
    try test_alu_dispatch(.zerox, 0x07400000, alu.ZEROX, true);
    try test_alu_dispatch(.signx, 0x07600000, alu.SIGNX, true);
    try test_alu_dispatch(.encod, 0x07800000, alu.ENCOD, true);
    try test_alu_dispatch(.ones, 0x07A00000, alu.ONES, true);
    try test_alu_dispatch(.@"test", 0x07C00000, alu.TEST, false);
    try test_alu_dispatch(.testn, 0x07E00000, alu.TESTN, false);
    try test_alu_dispatch(.setnib, 0x08000000, alu.SETNIB, true);
    try test_alu_dispatch(.getnib, 0x08400000, alu.GETNIB, true);
    try test_alu_dispatch(.rolnib, 0x08800000, alu.ROLNIB, true);
    try test_alu_dispatch(.setbyte, 0x08C00000, alu.SETBYTE, true);
    try test_alu_dispatch(.getbyte, 0x08E00000, alu.GETBYTE, true);
    try test_alu_dispatch(.rolbyte, 0x09000000, alu.ROLBYTE, true);
    try test_alu_dispatch(.setword, 0x09200000, alu.SETWORD, true);
    try test_alu_dispatch(.getword, 0x09300000, alu.GETWORD, true);
    try test_alu_dispatch(.rolword, 0x09400000, alu.ROLWORD, true);
    try test_alu_dispatch(.setr, 0x09A80000, alu.SETR, true);
    try test_alu_dispatch(.setd, 0x09B00000, alu.SETD, true);
    try test_alu_dispatch(.sets, 0x09B80000, alu.SETS, true);
    try test_alu_dispatch(.decod, 0x09C00000, alu.DECOD, true);
    try test_alu_dispatch(.bmask, 0x09C80000, alu.BMASK, true);
    try test_alu_dispatch(.crcbit, 0x09D00000, alu.CRCBIT, true);
    try test_alu_dispatch(.crcnib, 0x09D80000, alu.CRCNIB, true);
    try test_alu_dispatch(.muxnits, 0x09E00000, alu.MUXNITS, true);
    try test_alu_dispatch(.muxnibs, 0x09E80000, alu.MUXNIBS, true);
    try test_alu_dispatch(.muxq, 0x09F00000, alu.MUXQ, true);
    try test_alu_dispatch(.movbyts, 0x09F80000, alu.MOVBYTS, true);
    try test_alu_dispatch(.mul, 0x0A000000, alu.MUL, true);
    try test_alu_dispatch(.muls, 0x0A100000, alu.MULS, true);
    try test_alu_dispatch(.sca, 0x0A200000, alu.SCA, false);
    try test_alu_dispatch(.scas, 0x0A300000, alu.SCAS, false);
    try test_alu_dispatch(.splitb, 0x0D600060, alu.SPLITB, true);
    try test_alu_dispatch(.mergeb, 0x0D600061, alu.MERGEB, true);
    try test_alu_dispatch(.splitw, 0x0D600062, alu.SPLITW, true);
    try test_alu_dispatch(.mergew, 0x0D600063, alu.MERGEW, true);
    try test_alu_dispatch(.seussf, 0x0D600064, alu.SEUSSF, true);
    try test_alu_dispatch(.seussr, 0x0D600065, alu.SEUSSR, true);
    try test_alu_dispatch(.rgbsqz, 0x0D600066, alu.RGBSQZ, true);
    try test_alu_dispatch(.rgbexp, 0x0D600067, alu.RGBEXP, true);
    try test_alu_dispatch(.xoro32, 0x0D600068, alu.XORO32, true);
    try test_alu_dispatch(.rev, 0x0D600069, alu.REV, true);
    try test_alu_dispatch(.rczr, 0x0D60006A, alu.RCZR, true);
    try test_alu_dispatch(.rczl, 0x0D60006B, alu.RCZL, true);
    try test_alu_dispatch(.wrc, 0x0D60006C, alu.WRC, true);
    try test_alu_dispatch(.wrnc, 0x0D60006D, alu.WRNC, true);
    try test_alu_dispatch(.wrz, 0x0D60006E, alu.WRZ, true);
    try test_alu_dispatch(.wrnz, 0x0D60006F, alu.WRNZ, true);
    try test_alu_dispatch(.modcz, 0x0D64006F, alu.MODCZ, false);
}

// codegen: begin:executortests
test "forwarded source stays with a waiting instruction" {
    const Hub = @import("Hub.zig");
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);
    hub.init();
    const cog = &hub.cogs[0];
    cog.exec_mode = .cog;
    cog.write_reg(@enumFromInt(24), 0x8000);
    // SCA r24, #2; WAITX #2; MOV r25, #7.
    cog.write_reg(@enumFromInt(0), 0xFA24_3002);
    cog.write_reg(@enumFromInt(1), 0xFD64_041F);
    cog.write_reg(@enumFromInt(2), 0xF604_3207);
    hub.step();
    hub.step();
    try std.testing.expectEqual(@as(?u32, 1), cog.next_instruction.?.s_value);
    hub.step();
    try std.testing.expectEqual(@as(?u32, 1), cog.current_instruction.?.s_value);
    try std.testing.expect(cog.wait_until != null);
    while (cog.current_instruction != null and hub.counter < 16) {
        try std.testing.expectEqual(@as(?u32, 1), cog.current_instruction.?.s_value);
        hub.step();
    }
    try std.testing.expect(cog.current_instruction == null);
    try std.testing.expectEqual(@as(?u32, null), cog.next_instruction.?.s_value);
    hub.step();
    try std.testing.expectEqual(@as(u32, 7), cog.read_reg(@enumFromInt(25)));
}

test "COGINIT allocates free cogs and pairs, reports failure, and retains RAM on restart" {
    const Hub = @import("Hub.zig");
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);
    hub.init();
    const cog = &hub.cogs[0];
    cog.exec_mode = .cog;
    cog.write_reg(@enumFromInt(20), 0x30); // Start a free cog without loading RAM.
    cog.write_reg(@enumFromInt(21), 0x4000_0210);
    hub.cogs[1].write_reg(@enumFromInt(100), 0x1234);
    hub.cogs[1].lut[16] = 0x5678;
    cog.q = 0x8765_4321;
    cog.setq_pending = true;
    const start = 0xFCE0_0000 | (1 << 20) | (20 << 9) | 21; // COGINIT r20, r21 WC
    try std.testing.expectEqual(Cog.ExecResult.next, execute_instruction(cog, .{ .pc = 0, .instr = start }));
    try std.testing.expectEqual(@as(u32, 1), cog.read_reg(@enumFromInt(20)));
    try std.testing.expect(!cog.c);
    try std.testing.expectEqual(Cog.ExecMode.cog, hub.cogs[1].exec_mode);
    try std.testing.expectEqual(@as(u20, 0x210), hub.cogs[1].pc);
    try std.testing.expectEqual(@as(u32, 0x4000_0210), hub.cogs[1].read_reg(.PTRB));
    try std.testing.expectEqual(@as(u32, 0x8765_4321), hub.cogs[1].read_reg(.PTRA));
    try std.testing.expectEqual(@as(u32, 0x1234), hub.cogs[1].read_reg(@enumFromInt(100)));
    try std.testing.expectEqual(@as(u32, 0x5678), hub.cogs[1].lut[16]);
    cog.write_reg(@enumFromInt(20), 0x31); // First available even/odd pair is 2,3.
    try std.testing.expectEqual(Cog.ExecResult.next, execute_instruction(cog, .{ .pc = 0, .instr = start }));
    try std.testing.expectEqual(@as(u32, 2), cog.read_reg(@enumFromInt(20)));
    try std.testing.expectEqual(Cog.ExecMode.cog, hub.cogs[3].exec_mode);
    try std.testing.expectEqual(@as(u32, 0), cog.q);
    for (&hub.cogs) |*active| active.exec_mode = .cog;
    cog.write_reg(@enumFromInt(20), 0x30);
    try std.testing.expectEqual(Cog.ExecResult.next, execute_instruction(cog, .{ .pc = 0, .instr = start }));
    try std.testing.expect(cog.c);
    try std.testing.expectEqual(@as(u32, 15), cog.read_reg(@enumFromInt(20)));

    hub.locks[4] = .{ .id = 4, .allocated = true, .taken = true, .owner = 1 };
    cog.selectable_events[0] = 0x24;
    hub.cogs[1].reset();
    try std.testing.expect(!hub.locks[4].taken);
    try std.testing.expect(hub.locks[4].allocated);
    try std.testing.expectEqual(@as(u3, 1), hub.locks[4].owner);
    try std.testing.expect(cog.events & EventId.SE1.mask() != 0);
    try std.testing.expectEqual(@as(u32, 0x1234), hub.cogs[1].read_reg(@enumFromInt(100)));
    try std.testing.expectEqual(@as(u32, 0x5678), hub.cogs[1].lut[16]);

    // Loading an image at the end of hub RAM wraps through the physical RAM.
    hub.write_memory(0x7fffc, 0x1122_3344, 4, false);
    hub.write_memory(0, 0x5566_7788, 4, false);
    try hub.start_cog(1, .{ .hub_address = 0x7fffc });
    try std.testing.expectEqual(@as(u32, 0x1122_3344), hub.cogs[1].read_reg(@enumFromInt(0)));
    try std.testing.expectEqual(@as(u32, 0x5566_7788), hub.cogs[1].read_reg(@enumFromInt(1)));
}

test "LUT sharing receives companion writes and selectable events track both cogs" {
    const Hub = @import("Hub.zig");
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);
    hub.init();
    const even = &hub.cogs[2];
    const odd = even.other();
    even.lut_sharing = true;
    even.selectable_events = .{ 12, 8, 7, null };
    odd.selectable_events[0] = 4;
    odd.write_lut(508, 0x8000_0000);
    try std.testing.expectEqual(@as(u32, 0x8000_0000), even.lut[508]);
    try std.testing.expect(even.events & EventId.SE1.mask() != 0);
    try std.testing.expect(odd.events & EventId.SE1.mask() != 0);
    _ = odd.read_lut(508);
    try std.testing.expect(even.events & EventId.SE2.mask() != 0);
    even.write_lut(511, 99);
    try std.testing.expect(even.events & EventId.SE3.mask() != 0);
    try std.testing.expectEqual(@as(u32, 0), odd.lut[511]);
    even.lut_sharing = false;
    odd.write_lut(508, 1);
    try std.testing.expectEqual(@as(u32, 0x8000_0000), even.lut[508]);
}

test "GETCT captures both halves and counter waits cross the 32-bit wrap" {
    const Hub = @import("Hub.zig");
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);
    hub.init();
    const cog = &hub.cogs[0];
    hub.counter = 0x1234_5678_ffff_ffff;
    cog.c = false;
    const high = 0xFD70_001A | (20 << 9); // GETCT r20 WC
    const low = 0xFD60_001A | (21 << 9); // GETCT r21
    try std.testing.expectEqual(Cog.ExecResult.next, execute_instruction(cog, .{ .pc = 0, .instr = high }));
    hub.counter += 1;
    try std.testing.expectEqual(Cog.ExecResult.next, execute_instruction(cog, .{ .pc = 1, .instr = low }));
    try std.testing.expectEqual(@as(u32, 0x1234_5678), cog.read_reg(@enumFromInt(20)));
    try std.testing.expectEqual(@as(u32, 0xffff_ffff), cog.read_reg(@enumFromInt(21)));
    try std.testing.expect(!cog.c);
    try std.testing.expectEqual(Cog.ExecResult.next, execute_instruction(cog, .{ .pc = 2, .instr = low }));
    try std.testing.expectEqual(@as(u32, 0), cog.read_reg(@enumFromInt(21)));

    hub.counter = 0xffff_fffe;
    cog.exec_mode = .cog;
    cog.ct_targets[2] = 0;
    hub.step();
    hub.step();
    try std.testing.expect(cog.events & EventId.CT3.mask() == 0);
    hub.step();
    try std.testing.expect(cog.events & EventId.CT3.mask() != 0);
    cog.q = 4;
    cog.setq_pending = true;
    const wait = 0xFD70_3C24; // WAITATN WC
    try std.testing.expectEqual(Cog.ExecResult.wait, execute_instruction(cog, .{ .pc = 3, .instr = wait }));
    try std.testing.expect(cog.setq_pending);
    hub.counter = 0x1_0000_0004;
    try std.testing.expectEqual(Cog.ExecResult.next, execute_instruction(cog, .{ .pc = 3, .instr = wait }));
    try std.testing.expect(cog.c);
    try std.testing.expect(!cog.setq_pending);
}

test "unaligned hub memory wraps, extended pointer indices are unscaled, and zero AUGS is honored" {
    const Hub = @import("Hub.zig");
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);
    hub.init();
    hub.write_memory(0xffff_ffff, 0x1122_3344, 4, false);
    try std.testing.expectEqual(@as(u32, 0x1122_3344), hub.read_memory(0x7ffff, 4));
    try std.testing.expectEqual(@as(u8, 0x33), hub.memory[0]);
    hub.write_memory(0x7ffff, 0x0055_0066, 4, true);
    try std.testing.expectEqual(@as(u32, 0x1155_3366), hub.read_memory(0x7ffff, 4));
    const cog = &hub.cogs[0];
    cog.write_reg(.PTRA, 100);
    cog.augs_pending = true;
    cog.augs = 0x800000;
    // Augmented PTR expression: no update, offset +3 bytes, even for RDLONG.
    try std.testing.expectEqual(@as(u32, 103), memoryAddress(cog, @enumFromInt(3), true, 4, false));
    cog.augs_pending = true;
    cog.augs = 0;
    // AUGS #0 turns #256 into a literal address rather than plain PTRA.
    try std.testing.expectEqual(@as(u32, 256), memoryAddress(cog, @enumFromInt(256), true, 4, false));
    try std.testing.expect(!cog.augs_pending);
    // Unaugmented negative pointer offset is scaled; post-update -16 is valid.
    try std.testing.expectEqual(@as(u32, 96), memoryAddress(cog, @enumFromInt(0x13f), true, 4, false));
    try std.testing.expectEqual(@as(u32, 100), memoryAddress(cog, @enumFromInt(0x170), true, 4, false));
    try std.testing.expectEqual(@as(u32, 36), cog.read_reg(.PTRA));
}

test "block transfers bypass special registers and wrap register addresses" {
    const Hub = @import("Hub.zig");
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);
    hub.init();
    const cog = &hub.cogs[0];
    cog.write_reg(.PTRA, 0x1000);
    cog.q = 9;
    cog.setq_pending = true;
    cog.block_pointer_delta = true;
    for (0..10) |i| hub.write_memory(0x1000 + @as(u32, @intCast(i * 4)), @intCast(i + 1), 4, false);
    const read = 0xFB04_0000 | (503 << 9) | 0x100; // RDLONG r503, PTRA
    for (0..10) |i| {
        try std.testing.expectEqual(if (i == 9) Cog.ExecResult.next else .wait, execute_instruction(cog, .{ .pc = 0, .instr = read }));
    }
    try std.testing.expectEqual(@as(u32, 1), cog.read_reg(.PB));
    try std.testing.expectEqual(@as(u32, 10), cog.read_reg(@enumFromInt(0)));
    try std.testing.expectEqual(@as(u32, 0x1000), cog.read_reg(.PTRA));
    try std.testing.expectEqual(@as(u32, 2), cog.ram_tail[0]);
    try std.testing.expectEqual(@as(u32, 9), cog.ram_tail[7]);
    cog.write_reg(.PTRA, 0x2000);
    cog.q = 9;
    cog.setq_pending = true;
    const write = 0xFC64_0000 | (503 << 9) | 0x100; // WRLONG r503, PTRA
    for (0..10) |i| {
        try std.testing.expectEqual(if (i == 9) Cog.ExecResult.next else .wait, execute_instruction(cog, .{ .pc = 1, .instr = write }));
    }
    for (0..10) |i| try std.testing.expectEqual(@as(u32, @intCast(i + 1)), hub.read_memory(0x2000 + @as(u32, @intCast(i * 4)), 4));
    try std.testing.expect(cog.memory_transfer == null);
    try std.testing.expect(!cog.setq_pending);
    try std.testing.expectEqual(@as(u32, 0x2000), cog.read_reg(.PTRA));
}

fn test_alu_dispatch(comptime opcode: decode.OpCode, template: u32, comptime function: anytype, comptime writes_result: bool) !void {
    const Hub = @import("Hub.zig");
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);
    const field = comptime decode.instruction_type.get(opcode);
    const Operands = @FieldType(encoding.Instruction, field);
    var exercised: usize = 0;
    for (0..4) |flags| {
        for (0..4) |effects| {
            for ([_]bool{ false, true }) |immediate| {
                for ([_]bool{ false, true }) |altered| {
                    hub.init();
                    const cog = &hub.cogs[0];
                    cog.c = flags & 2 != 0;
                    cog.z = flags & 1 != 0;
                    cog.q = 0x1234_5678;
                    cog.setq_pending = true;
                    cog.augs = 0x1234_5600;
                    cog.augd = 0x8765_4200;
                    var operands: Operands = @bitCast(template | 0xF000_0000);
                    if (@hasField(Operands, "d")) operands.d = @enumFromInt(24);
                    if (@hasField(Operands, "s")) operands.s = @enumFromInt(25);
                    if (@hasField(Operands, "s_imm")) operands.s_imm = immediate;
                    if (@hasField(Operands, "n")) operands.n = std.math.maxInt(@TypeOf(operands.n));
                    if (@hasField(Operands, "c_mod")) operands.c_mod = @enumFromInt(@as(u1, @truncate(effects >> 1)));
                    if (@hasField(Operands, "z_mod")) operands.z_mod = @enumFromInt(@as(u1, @truncate(effects)));
                    if (Operands == encoding.UpdateFlags) {
                        operands.c_value = @enumFromInt(10);
                        operands.z_value = @enumFromInt(12);
                    }
                    const state: Cog.PipelineState = .{
                        .pc = 1,
                        .instr = @bitCast(operands),
                        .alt_d = if (altered) .PTRA else null,
                        .alt_s = if (altered) .PB else null,
                        .alt_r = .PA,
                    };
                    // Some flag combinations select another opcode (BITx/TESTBx).
                    if (decode.decode(state.instr) != opcode) continue;
                    exercised += 1;
                    const d_reg: Cog.Register = if (altered) .PTRA else @enumFromInt(24);
                    const s_reg: Cog.Register = if (altered) .PB else @enumFromInt(25);
                    cog.write_reg(d_reg, 0x8000_0001);
                    cog.write_reg(s_reg, 0xDEAD_BEEF);
                    cog.write_reg(.PA, 0xFEED_FACE);
                    cog.current_instruction = state;
                    cog.next_instruction = .{ .pc = 2, .instr = 0 };
                    const has_s = @hasField(Operands, "s");
                    const input: alu.Input = .{
                        .d = if (Operands == encoding.UpdateFlags) 0xAC else 0x8000_0001,
                        .s = if (!has_s) 0 else if (immediate) 0x1234_5600 | @as(u32, @intFromEnum(s_reg)) else 0xDEAD_BEEF,
                        .c = .from_bool(cog.c),
                        .z = .from_bool(cog.z),
                        .q = cog.q,
                        .setq_prefix = true,
                    };
                    const expected = if (@hasField(Operands, "n")) function(input, operands.n) else function(input);
                    try std.testing.expectEqual(Cog.ExecResult.next, execute_instruction(cog, state));
                    try std.testing.expectEqual(if (writes_result) expected.result else @as(u32, 0xFEED_FACE), cog.read_reg(.PA));
                    try std.testing.expectEqual(@as(u32, 0x8000_0001), cog.read_reg(d_reg));
                    try std.testing.expectEqual(if (@hasField(Operands, "c_mod") and operands.c_mod == .write) expected.c == .set else flags & 2 != 0, cog.c);
                    try std.testing.expectEqual(if (@hasField(Operands, "z_mod") and operands.z_mod == .write) expected.z == .set else flags & 1 != 0, cog.z);
                    try std.testing.expectEqual(expected.q, cog.q);
                    try std.testing.expectEqual(expected.next_s, cog.next_instruction.?.s_value);
                    try std.testing.expectEqual(if (has_s and immediate) @as(u32, 0) else @as(u32, 0x1234_5600), cog.augs);
                    try std.testing.expectEqual(@as(u32, 0x8765_4200), cog.augd);
                    try std.testing.expect(!cog.setq_pending);
                }
            }
        }
    }
    try std.testing.expect(exercised > 0);
}
// codegen: end:executortests
