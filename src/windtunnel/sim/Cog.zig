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
lut: [512]u32 = undefined,

pc: u20 = 0,
q: u32 = 0,
z: bool = false,
c: bool = false,
q2: bool = false,

lut_sharing: bool = false,
interrupts: bool = false,

exec_mode: ExecMode = .stopped,

current_instruction: ?Instruction = null,
next_instruction: ?Instruction = null,

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
        const result = cog.execute_instruction(instr);
        switch (result) {
            .wait => {},
            .next => cog.current_instruction = null,
            .trap => @panic("TODO: Implement trapping!"),
            .skip => logger.info("skipped instruction", .{}),
        }
    }
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
        .IF_C => cog.z,
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

const Instruction = struct {
    pc: u20,
    instr: u32,
};

fn fetch_instruction(cog: *Cog) ?Instruction {
    switch (cog.exec_mode) {
        .stopped => unreachable,
        .cog => {
            const instr: Instruction = .{
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
        .hub => @panic("TODO: Implement HUB exec"),
    }
}

pub const ExecResult = enum {
    wait,
    next,
    trap,
    skip,
};

fn execute_instruction(cog: *Cog, instr: Instruction) ExecResult {
    const opcode = decode.decode(instr.instr);

    const enc: encoding.Instruction = .{ .raw = instr.instr };

    switch (opcode) {
        .invalid => return .trap,
        inline else => |opc| {
            @setEvalBranchQuota(10_000);
            const field = comptime decode.instruction_type.get(opc);
            const params = @field(enc, field);

            logger.info("0x{X:0>5}: 0x{X:0>8} {s}: {}", .{ instr.pc, instr.instr, @tagName(opc), params });

            return @field(execute, @tagName(opc))(cog, params);
        },
    }
}
