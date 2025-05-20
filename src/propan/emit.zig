const std = @import("std");

const Module = @import("Module.zig");

pub const BinaryFormat = enum {
    flat,
    none,

    pub fn is_binary(bf: BinaryFormat) bool {
        return switch (bf) {
            .flat => true,

            .none => false,
        };
    }
};

pub fn emit(file: std.fs.File, module: Module, format: BinaryFormat) !void {
    switch (format) {
        .flat => try emit_flat(file, module),
        .none => {},
    }
}

fn emit_flat(file: std.fs.File, module: Module) !void {
    var total_size: u64 = 0;
    for (module.segments) |seg| {
        total_size = @max(total_size, seg.hub_offset + seg.data.len);
    }

    if (total_size == 0)
        return;

    try file.seekTo(total_size - 1);
    try file.writeAll("\x00");

    for (module.segments) |seg| {
        try file.seekTo(seg.hub_offset);
        try file.writeAll(seg.data);
    }
}
