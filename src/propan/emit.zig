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
    for (modules) |module| {
        for (module.constants, 0..) |constant, index| {
            if (constant.value.value != .int) continue;
            const value = constant.value.value.int;
            if (value < std.math.minInt(i32) or value > std.math.maxInt(u32)) continue;
            try out.writeAll("CON\n  ");
            try emit_name(out, "const", constant.name, index);
            try out.print(" = {d} ' {s}\n", .{ value, constant.name });
        }
    }
    try out.writeAll("DAT\n");
    var offset: usize = 0;
    emit_loop: while (offset <= data.len) {
        for (modules) |module| {
            for (module.segments) |segment| {
                if (segment.hub_offset == offset and offset < data.len)
                    try out.print("  ' segment {d}: {s} at ${X:0>5}\n", .{ @intFromEnum(segment.id), @tagName(segment.exec_mode), offset });
            }
            for (module.symbols, 0..) |symbol, index| {
                if (symbol.label.hub_address) |hub| {
                    if (hub == offset) {
                        try emit_name(out, "label", symbol.name, index);
                        try out.print(" ' {s}\n", .{symbol.name});
                    }
                }
            }
            for (module.line_data) |line| {
                if (line.offset == offset and line.kind == .code and line.mnemonic != null)
                    try emit_instruction_comment(out, line);
            }
        }
        if (offset == data.len) break;

        const kind = kinds[offset];
        if (kind == .code) {
            for (modules) |module| {
                for (module.line_data) |line| {
                    if (line.offset == offset and offset % 4 == 0 and try emit_readable_instruction(out, module, line)) {
                        offset += 4;
                        continue :emit_loop;
                    }
                }
            }
        }
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

fn emit_instruction_comment(out: *std.Io.Writer, line: Module.LineData) !void {
    try out.writeAll("  ' ");
    if (line.condition) |condition| try out.print("{s} ", .{@tagName(condition.encode())});
    try out.writeAll(line.mnemonic.?);
    for (line.operands, 0..) |operand, index| {
        try out.writeAll(if (index == 0) " " else ", ");
        if (operand.source_kind == .function_call and operand.value.value == .int) {
            if (operand.value.flags.usage == .literal) try out.writeByte('#');
            try out.print("{d}", .{operand.value.value.int});
        } else {
            try out.writeAll(operand.syntax);
        }
    }
    if (line.effect) |effect| try out.print(" {s}", .{@tagName(effect)});
    try out.writeByte('\n');
}

fn emit_readable_instruction(out: *std.Io.Writer, module: Module, line: Module.LineData) !bool {
    if (line.kind != .code or line.length != 4 or line.mnemonic == null) return false;
    if (line.condition) |condition| if (condition == .@"return") return false;
    if (line.effect != null and line.operands.len == 0) return false;
    if (line.effect) |effect| switch (effect) {
        .wc, .wz, .wcz => {},
        else => return false,
    };
    for (line.operands) |operand| {
        if ((operand.encoding != .register and operand.encoding != .reg_or_imm) or operand.pcrel) return false;
        if (operand.value.flags.augment or operand.value.flags.addressing != .auto) return false;
        if (operand_number(operand) == null) return false;
    }

    try out.writeAll("  ");
    if (line.condition) |condition| try out.print("{s} ", .{@tagName(condition.encode())});
    try out.writeAll(line.mnemonic.?);
    for (line.operands, 0..) |operand, index| {
        try out.writeAll(if (index == 0) " " else ", ");
        if (operand.value.flags.usage == .literal) try out.writeByte('#');
        if (operand.source_kind == .symbol) {
            for (module.constants, 0..) |constant, constant_index| {
                if (std.ascii.eqlIgnoreCase(operand.syntax, constant.name) and constant.value.value == .int and constant.value.value.int >= std.math.minInt(i32) and constant.value.value.int <= std.math.maxInt(u32)) {
                    try emit_name(out, "const", constant.name, constant_index);
                    break;
                }
            } else {
                for (module.symbols, 0..) |symbol, symbol_index| {
                    if (std.ascii.eqlIgnoreCase(operand.syntax, symbol.name) and symbol.label.hub_address != null) {
                        try emit_name(out, "label", symbol.name, symbol_index);
                        break;
                    }
                } else {
                    try out.print("${X}", .{operand_number(operand).?});
                }
            }
        } else {
            try out.print("${X}", .{operand_number(operand).?});
        }
    }
    if (line.effect) |effect| try out.print(" {s}", .{@tagName(effect)});
    try out.writeByte('\n');
    return true;
}

fn emit_name(out: *std.Io.Writer, kind: []const u8, name: []const u8, index: usize) !void {
    try out.print("p2_{s}_", .{kind});
    for (name) |character| {
        try out.writeByte(if (std.ascii.isAlphanumeric(character) or character == '_') character else '_');
    }
    try out.print("_{d}", .{index});
}

fn operand_number(operand: Module.LineData.Operand) ?u32 {
    return switch (operand.value.value) {
        .int => |value| if (value >= 0 and value <= 511) @intCast(value) else null,
        .register => |value| @intFromEnum(value),
        .address => |address| switch (address.local) {
            .cog, .regspace => |value| value,
            else => null,
        },
        else => null,
    };
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
