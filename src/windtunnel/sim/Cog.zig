const std = @import("std");

const Cog = @This();
const Hub = @import("Hub.zig");

const decode = @import("decode.zig");
const encoding = @import("encoding.zig");
const execute = @import("execute.zig");

pub const Register = @import("enums.zig").Register;

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
z: u1 = 0,
c: u1 = 0,
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
        }
    }
}

pub fn other(cog: *Cog) *Cog {
    return &cog.hub.cogs[cog.id ^ 1];
}

pub fn ram_slice(cog: *Cog) u3 {
    return (cog.hub.counter + cog.id) % 8;
}

pub fn write_reg(cog: *Cog, addr: u9, value: u32) void {
    const reg: Register = @enumFromInt(addr);
    cog.registers.set(reg, value);
    switch (reg) {
        .INA, .INB => {},
        .OUTA, .OUTB => cog.hub.io.update_out(),
    }
}

pub fn read_reg(cog: *Cog, addr: u9) u32 {
    const reg: Register = @enumFromInt(addr);
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
                    cog.read_reg(@intCast(cog.pc))
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
            return @field(execute, @tagName(opc))(cog, params);
        },
    }
}
