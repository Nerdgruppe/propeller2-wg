const std = @import("std");

const Module = @import("Module.zig");

pub const BinaryFormat = enum {
    none,
    flat,
    json,

    pub fn is_binary(bf: BinaryFormat) bool {
        return switch (bf) {
            .flat => true,

            .none, .json => false,
        };
    }
};

pub fn emit(allocator: std.mem.Allocator, file: std.fs.File, module: Module, format: BinaryFormat) !void {
    switch (format) {
        .flat => try emit_flat(file, module),
        .json => try emit_json(allocator, file, module),

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

fn create_b64(allocator: std.mem.Allocator, buffer: []const u8) ![]const u8 {
    var writer: std.Io.Writer.Allocating = .init(allocator);
    defer writer.deinit();

    try std.base64.standard.Encoder.encodeWriter(&writer.writer, buffer);

    return try writer.toOwnedSlice();
}

fn emit_json(allocator: std.mem.Allocator, file: std.fs.File, module: Module) !void {
    var arena_allocator: std.heap.ArenaAllocator = .init(allocator);
    defer arena_allocator.deinit();

    const arena = arena_allocator.allocator();

    const JSeg = struct {
        id: u32,
        offset: u20,
        size: usize,
        data: []const u8,
        mode: []const u8,
    };

    const JSym = struct {
        name: []const u8,
        segment_id: u32,
        offset: u32,
        type: []const u8,
        mode: []const u8,
        jump: union(enum) {
            cog: u9,
            lut: u9,
            hub: u20,
        },
    };

    const JLine = struct {
        offset: u32,
        size: u32,
        file: ?[]const u8,
        line: u32,
        column: u32,
    };

    const JMod = struct {
        total_size: u64,
        segments: []JSeg,
        symbols: []JSym,
        line_map: []JLine,
    };

    var total_size: u64 = 0;
    for (module.segments) |seg| {
        total_size = @max(total_size, seg.hub_offset + seg.data.len);
    }

    const mod: JMod = .{
        .total_size = total_size,
        .segments = try arena.alloc(JSeg, module.segments.len),
        .symbols = try arena.alloc(JSym, module.symbols.len),
        .line_map = try arena.alloc(JLine, module.line_data.len),
    };

    for (mod.segments, module.segments) |*out, in| {
        out.* = .{
            .id = @intFromEnum(in.id),
            .offset = in.hub_offset,
            .size = in.data.len,
            .data = try create_b64(arena, in.data),
            .mode = @tagName(in.exec_mode),
        };
    }

    for (mod.line_map, module.line_data) |*out, in| {
        out.* = .{
            .offset = in.offset,
            .size = in.length,
            .file = in.location.source,
            .line = in.location.line,
            .column = in.location.column,
        };
    }

    for (mod.symbols, module.symbols) |*out, in| {
        out.* = .{
            .name = in.name,
            .type = @tagName(in.type),
            .segment_id = @intFromEnum(in.label.segment_id),
            .offset = in.label.hub_address,
            .mode = @tagName(in.label.local),
            .jump = switch (in.label.local) {
                .cog => |v| .{ .cog = v },
                .lut => |v| .{ .lut = v },
                .hub => .{ .hub = in.label.hub_address },
            },
        };
    }

    var buffer: [4096]u8 = undefined;
    var writer = file.writer(&buffer);

    try std.json.Stringify.value(mod, .{
        .whitespace = .indent_2,
        .escape_unicode = false,
        .emit_null_optional_fields = true,
        .emit_strings_as_arrays = false,
        .emit_nonportable_numbers_as_strings = false,
    }, &writer.interface);
    try writer.interface.writeAll("\n");
    try writer.interface.flush();
}
