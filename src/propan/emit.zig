const std = @import("std");

const Module = @import("Module.zig");
const flat = @import("emit/flat.zig");
const json = @import("emit/json.zig");
const spin2 = @import("emit/spin2.zig");

pub const BinaryFormat = enum {
    none,
    flat,
    json,
    spin2,

    pub fn is_binary(bf: BinaryFormat) bool {
        return switch (bf) {
            .flat => true,

            .none, .json, .spin2 => false,
        };
    }
};

pub fn emit(io: std.Io, allocator: std.mem.Allocator, file: std.Io.File, modules: []const Module, flat_data: []const u8, format: BinaryFormat) !void {
    var buffer: [8192]u8 = undefined;
    var writer = file.writer(io, &buffer);
    try write(allocator, &writer.interface, modules, flat_data, format);
    try writer.interface.flush();
}

pub fn write(allocator: std.mem.Allocator, writer: *std.Io.Writer, modules: []const Module, flat_data: []const u8, format: BinaryFormat) !void {
    switch (format) {
        .flat => try flat.emit(writer, flat_data),
        .json => try json.emit(allocator, writer, modules, flat_data.len),
        .spin2 => try spin2.emit(allocator, writer, modules, flat_data),
        .none => {},
    }
}
