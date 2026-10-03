const std = @import("std");

const eval = @import("stdlib/eval.zig");
const ast = @import("frontend/ast.zig");

pub const TaggedAddress = eval.TaggedAddress;
pub const Segment_ID = eval.Segment_ID;

const Module = @This();

arena: std.heap.ArenaAllocator,

segments: []const Segment,
regspace_segments: []const u32 = &.{},
line_data: []const LineData,
symbols: []const Symbol,
constants: []const Constant,

pub fn deinit(mod: *Module) void {
    mod.arena.deinit();
    mod.* = undefined;
}

pub fn line_for_address(mod: Module, hub_offset: u32) ?ast.Location {
    for (mod.line_data) |line| {
        if (hub_offset >= line.offset and hub_offset < line.offset + line.length)
            return line.location;
    }
    return null;
}

pub const Symbol = struct {
    name: []const u8,
    label: TaggedAddress,
    type: Type,
    source_location: ?ast.Location = null,

    pub const Type = enum {
        code,
        data,
    };
};

pub const Constant = struct {
    name: []const u8,
    value: eval.Value,
    location: ast.Location,
};

pub const Segment = struct {
    id: Segment_ID,
    hub_offset: u20,
    data: []const u8,
    exec_mode: eval.ExecMode,
};

pub const LineData = struct {
    offset: u32,
    length: u32,
    location: ast.Location,
    pc: ?u32 = null,
    kind: Kind = .label,
    mnemonic: ?[]const u8 = null,
    operands: []const Operand = &.{},
    condition: ?ast.Condition = null,
    effect: ?ast.Effect = null,

    pub const Kind = enum { label, code, byte, word, long, file };
    pub const Operand = struct {
        value: eval.Value,
        syntax: []const u8,
        source_kind: enum { symbol, function_call, other },
        encoding: Encoding,
        pcrel: bool = false,

        pub const Encoding = enum { address, register, immediate, reg_or_imm, pointer_expr, pointer_reg, enumeration };
    };
};
