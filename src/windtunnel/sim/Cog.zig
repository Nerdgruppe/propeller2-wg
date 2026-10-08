const std = @import("std");
const logger = std.log.scoped(.cog);

const Cog = @This();
const Hub = @import("Hub.zig");

const decode = @import("decode.zig");
const encoding = @import("encoding.zig");
const execute = @import("execute.zig");
const enums = @import("enums.zig");

pub const Register = enums.Register;

pub const ExecMode = enum {
    stopped,
    cog,
    hub,
};

hub: *Hub,
id: u3,

registers: std.EnumArray(Register, u32) = .initFill(0),
lut: [512]u32 = @splat(0),

pc: u20 = 0,
q: u32 = 0,
setq_pending: bool = false,
z: bool = false,
c: bool = false,
q2: bool = false,

augs: u32 = 0,
augd: u32 = 0,

lut_sharing: bool = false,
interrupts: bool = false,

exec_mode: ExecMode = .stopped,

current_instruction: ?PipelineState = null,
next_instruction: ?PipelineState = null,
dispatch_pc: u20 = 0,
branched: bool = false,
stack: [8]u32 = @splat(0),
wait_until: ?u64 = null,
fifo_address: ?u19 = null,

pub fn init(hub: *Hub, id: u3) Cog {
    return .{
        .id = id,
        .hub = hub,
    };
}

pub fn reset(cog: *Cog) void {
    const id = cog.id;
    const hub = cog.hub;
    cog.* = .init(hub, id);
}

pub fn step(cog: *Cog) void {
    if (cog.exec_mode == .stopped) {
        std.debug.assert(cog.next_instruction == null);
        std.debug.assert(cog.current_instruction == null);
        return;
    }

    if (cog.current_instruction == null) {
        cog.current_instruction = cog.next_instruction;
        cog.next_instruction = null;
    }

    if (cog.next_instruction == null) {
        if (cog.fetch_instruction()) |instr| {
            cog.next_instruction = instr;
        }
    }

    if (cog.current_instruction) |instr| {
        cog.dispatch_pc = instr.pc;
        cog.branched = false;
        const result = execute.execute_instruction(cog, instr);
        if (result == .next and instr.instr != 0 and instr.instr >> 28 == 0 and !cog.branched) {
            cog.jump(@truncate(cog.pop()));
        }
        switch (result) {
            .wait => {},
            .next => cog.current_instruction = null,
            .trap, .unsupported => {
                cog.hub.fault = .{ .cog = cog.id, .pc = instr.pc, .instruction = instr.instr, .unsupported = result == .unsupported };
                cog.exec_mode = .stopped;
                cog.current_instruction = null;
                cog.next_instruction = null;
            },
            .skip => cog.current_instruction = null,
        }
    }
}

pub fn jump(cog: *Cog, target: u20) void {
    cog.pc = target;
    cog.exec_mode = if (target < 0x400) .cog else .hub;
    cog.next_instruction = null;
    cog.branched = true;
}

pub fn push(cog: *Cog, value: u32) void {
    std.mem.copyBackwards(u32, cog.stack[1..], cog.stack[0..7]);
    cog.stack[0] = value;
}

pub fn pop(cog: *Cog) u32 {
    const value = cog.stack[0];
    // Entry 7 stays unchanged, so repeated pops eventually repeat that value.
    std.mem.copyForwards(u32, cog.stack[0..7], cog.stack[1..]);
    return value;
}

pub fn call(cog: *Cog, target: u20) void {
    const return_pc = cog.dispatch_pc +% @as(u20, if (cog.exec_mode == .hub) 4 else 1);
    cog.push((@as(u32, @intFromBool(cog.c)) << 31) | (@as(u32, @intFromBool(cog.z)) << 30) | return_pc);
    cog.jump(target);
}

pub fn other(cog: *Cog) *Cog {
    return &cog.hub.cogs[cog.id ^ 1];
}

pub fn ram_slice(cog: *Cog) u3 {
    return (cog.hub.counter + cog.id) % 8;
}

pub fn write_reg(cog: *Cog, reg: Register, value: u32) void {
    cog.registers.set(reg, value);
    switch (reg) {
        .INA, .INB => {},
        .OUTA, .OUTB => cog.hub.io.update_out(),
        else => {},
    }
}

pub fn read_reg(cog: *Cog, reg: Register) u32 {
    return switch (reg) {
        .INA => @truncate(cog.hub.io.get_in() >> 0),
        .INB => @truncate(cog.hub.io.get_in() >> 32),
        else => cog.registers.get(reg),
    };
}

pub fn write_lut(cog: *Cog, addr: u9, value: u32) void {
    cog.lut[addr] = value;
    if (cog.lut_sharing)
        cog.other().lut[addr] = value;
}

pub fn read_lut(cog: *Cog, addr: u9) u32 {
    return cog.lut[addr];
}

/// Checks if `cond` would currently apply to this cog or not.
pub fn is_condition_met(cog: *Cog, cond: enums.Condition) bool {
    return switch (cond) {
        ._RET_ => true,
        .IF_NC_AND_NZ => !cog.c and !cog.z,
        .IF_NC_AND_Z => !cog.c and cog.z,
        .IF_NC => !cog.c,
        .IF_C_AND_NZ => cog.c and !cog.z,
        .IF_NZ => !cog.z,
        .IF_C_NE_Z => (cog.c != cog.z),
        .IF_NC_OR_NZ => !cog.c or !cog.z,
        .IF_C_AND_Z => cog.c and cog.z,
        .IF_C_EQ_Z => (cog.c == cog.z),
        .IF_Z => cog.z,
        .IF_NC_OR_Z => !cog.c or cog.z,
        .IF_C => cog.c,
        .IF_C_OR_NZ => cog.c or !cog.z,
        .IF_C_OR_Z => cog.c or cog.z,
        .IF_ALWAYS => true,
    };
}

pub fn resolve_operand(cog: *Cog, reg: Register, imm: bool) u32 {
    return if (imm)
        @intFromEnum(reg)
    else
        return cog.read_reg(reg);
}

pub const PipelineState = struct {
    pc: u20,
    instr: u32,

    alt_r: ?Register = null,
    alt_s: ?Register = null,
    alt_d: ?Register = null,
};

/// Fetches the last value set up by 'AUGS' and resets the value.
pub fn fetch_augs(cog: *Cog) u32 {
    const augs = cog.augs;
    cog.augs = 0;
    return augs;
}

/// Fetches the last value set up by 'AUGD' and resets the value.
pub fn fetch_augd(cog: *Cog) u32 {
    const augd = cog.augd;
    cog.augd = 0;
    return augd;
}

fn fetch_instruction(cog: *Cog) ?PipelineState {
    switch (cog.exec_mode) {
        .stopped => unreachable,
        .cog => {
            const instr: PipelineState = .{
                .pc = cog.pc,
                .instr = if (cog.pc < 0x200)
                    cog.read_reg(@enumFromInt(@as(u9, @intCast(cog.pc))))
                else
                    cog.read_lut(@intCast(cog.pc - 0x200)),
            };
            cog.pc += 1;
            if (cog.pc == 0x400) {
                cog.pc = 0;
            }
            std.debug.assert(cog.pc < 0x400);
            return instr;
        },
        .hub => {
            const offset = cog.pc & 0x7fffc;
            const instr: PipelineState = .{ .pc = cog.pc, .instr = std.mem.readInt(u32, cog.hub.memory[offset..][0..4], .little) };
            cog.pc +%= 4;
            return instr;
        },
    }
}

pub const ExecResult = enum {
    wait,
    next,
    trap,
    skip,
    unsupported,
};

pub const Fault = struct { cog: u3, pc: u20, instruction: u32, unsupported: bool };
