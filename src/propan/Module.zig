const std = @import("std");

const eval = @import("stdlib/eval.zig");
const ast = @import("frontend/ast.zig");

pub const TaggedAddress = eval.TaggedAddress;
pub const Segment_ID = eval.Segment_ID;

const Module = @This();

arena: std.heap.ArenaAllocator,

segments: []const Segment,
line_data: []const LineData,
symbols: []const Symbol,

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

    pub const Type = enum {
        code,
        data,
    };
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
};
