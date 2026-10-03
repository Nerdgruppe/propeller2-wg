const std = @import("std");

const Module = @import("Module.zig");

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
    switch (format) {
        .flat => try file.writeStreamingAll(io, flat_data),
        .json => try emit_json(io, allocator, file, modules, flat_data.len),
        .spin2 => try emit_spin2(io, allocator, file, modules, flat_data),

        .none => {},
    }
}

fn emit_spin2(io: std.Io, allocator: std.mem.Allocator, file: std.Io.File, modules: []const Module, data: []const u8) !void {
    const Kind = enum { padding, code, byte, word, long };
    const kinds = try allocator.alloc(Kind, data.len);
    defer allocator.free(kinds);
    @memset(kinds, .padding);
    for (modules) |module| for (module.line_data) |line| {
        const kind: Kind = switch (line.kind) {
            .label => continue,
            .code => .code,
            .byte, .file => .byte,
            .word => .word,
            .long => .long,
        };
        const start = @min(line.offset, data.len);
        const end = @min(@as(usize, line.offset) + line.length, data.len);
        @memset(kinds[start..end], kind);
    };

    var buffer: [4096]u8 = undefined;
    var writer = file.writer(io, &buffer);
    const out = &writer.interface;
    try out.writeAll("DAT\n");
    var offset: usize = 0;
    while (offset <= data.len) {
        for (modules) |module| {
            for (module.segments) |segment| {
                if (segment.hub_offset == offset and offset < data.len)
                    try out.print("  ' segment {d}: {s} at ${X:0>5}\n", .{ @intFromEnum(segment.id), @tagName(segment.exec_mode), offset });
            }
            for (module.symbols, 0..) |symbol, index| {
                if (symbol.label.hub_address) |hub| {
                    if (hub == offset) try out.print("p2_label_{d} ' {s}\n", .{ index, symbol.name });
                }
            }
            for (module.line_data) |line| {
                if (line.offset == offset and line.kind == .code and line.mnemonic != null)
                    try out.print("  ' {s}\n", .{line.mnemonic.?});
            }
        }
        if (offset == data.len) break;

        const kind = kinds[offset];
        const width: usize = switch (kind) {
            .word => 2,
            .code, .long => 4,
            else => 1,
        };
        const max_count: usize = switch (width) { 1 => 16, 2 => 8, else => 4 };
        const name = switch (width) { 1 => "BYTE", 2 => "WORD", else => "LONG" };
        try out.print("  {s} ", .{name});
        var count: usize = 0;
        while (count < max_count and offset + width <= data.len) {
            if (kinds[offset] != kind) break;
            if (count > 0) {
                var boundary = false;
                for (modules) |module| {
                    for (module.segments) |segment| if (segment.hub_offset == offset) { boundary = true; };
                    for (module.symbols) |symbol| if (symbol.label.hub_address) |hub| { if (hub == offset) boundary = true; };
                    for (module.line_data) |line| if (line.offset == offset) { boundary = true; };
                }
                if (boundary) break;
                try out.writeAll(", ");
            }
            const value = switch (width) {
                1 => @as(u32, data[offset]),
                2 => @as(u32, std.mem.readInt(u16, data[offset..][0..2], .little)),
                else => std.mem.readInt(u32, data[offset..][0..4], .little),
            };
            try out.print("${X}", .{value});
            offset += width;
            count += 1;
        }
        if (count == 0) {
            try out.print("${X}", .{data[offset]});
            offset += 1;
        }
        if (kind == .padding) try out.writeAll(" ' padding");
        try out.writeByte('\n');
    }
    try out.flush();
}

fn create_b64(allocator: std.mem.Allocator, buffer: []const u8) ![]const u8 {
    var writer: std.Io.Writer.Allocating = .init(allocator);
    defer writer.deinit();

    try std.base64.standard.Encoder.encodeWriter(&writer.writer, buffer);

    return try writer.toOwnedSlice();
}

fn emit_json(io: std.Io, allocator: std.mem.Allocator, file: std.Io.File, modules: []const Module, total_size: usize) !void {
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
        offset: ?u20,
        type: []const u8,
        mode: []const u8,
        jump: union(enum) {
            none,
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

    var segment_count: usize = 0;
    var symbol_count: usize = 0;
    var line_count: usize = 0;
    for (modules) |module| {
        segment_count += module.segments.len;
        symbol_count += module.symbols.len;
        line_count += module.line_data.len;
    }

    const mod: JMod = .{
        .total_size = total_size,
        .segments = try arena.alloc(JSeg, segment_count),
        .symbols = try arena.alloc(JSym, symbol_count),
        .line_map = try arena.alloc(JLine, line_count),
    };

    var segment_index: usize = 0;
    var symbol_index: usize = 0;
    var line_index: usize = 0;
    var id_base: u32 = 0;
    for (modules) |module| {
        var max_id: u32 = 0;
        for (module.segments) |in| {
            const id = @intFromEnum(in.id);
            max_id = @max(max_id, id);
            mod.segments[segment_index] = .{
                .id = id_base + id,
                .offset = in.hub_offset,
                .size = in.data.len,
                .data = try create_b64(arena, in.data),
                .mode = @tagName(in.exec_mode),
            };
            segment_index += 1;
        }

        for (module.line_data) |in| {
            mod.line_map[line_index] = .{
                .offset = in.offset,
                .size = in.length,
                .file = in.location.source,
                .line = in.location.line,
                .column = in.location.column,
            };
            line_index += 1;
        }

        for (module.symbols) |in| {
            const id = @intFromEnum(in.label.segment_id);
            max_id = @max(max_id, id);
            mod.symbols[symbol_index] = .{
                .name = in.name,
                .type = @tagName(in.type),
                .segment_id = id_base + id,
                .offset = in.label.hub_address,
                .mode = @tagName(in.label.local),
                .jump = switch (in.label.local) {
                    .cog => |v| .{ .cog = v },
                    .lut => |v| .{ .lut = v },
                    .regspace => |v| .{ .cog = v },
                    .hub => .{ .hub = in.label.hub_address.? },
                    .data => .none,
                },
            };
            symbol_index += 1;
        }
        id_base += max_id + 1;
    }

    var buffer: [4096]u8 = undefined;
    var writer = file.writer(io, &buffer);

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
