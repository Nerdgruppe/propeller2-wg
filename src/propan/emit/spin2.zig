const std = @import("std");

const Module = @import("../Module.zig");
const source_line = @import("../source_line.zig");
const instructions = @import("../stdlib/p2/instructions.zig").p2_instructions;
const stdlib = @import("../stdlib/stdlib.zig");

pub fn emit(io: std.Io, allocator: std.mem.Allocator, file: std.Io.File, modules: []const Module, data: []const u8) !void {
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
            try emit_constant_name(out, module, constant.name, index);
            try out.print(" = {d}\n", .{value});
        }
    }
    try out.writeAll("DAT\n");
    var comment_cursors: std.StringHashMapUnmanaged(u32) = .empty;
    defer comment_cursors.deinit(allocator);
    var offset: usize = 0;
    emit_loop: while (offset <= data.len) {
        for (modules) |module| {
            for (module.segments) |segment| {
                if (segment.hub_offset == offset and offset < data.len)
                    try out.print("' segment {d}: {t} at ${X:0>5}\n", .{ @intFromEnum(segment.id), segment.exec_mode, offset });
            }
            for (module.line_data) |line| {
                if (line.offset != offset) continue;
                try emit_source_comments(out, allocator, &comment_cursors, module, line.location);
                if (line.kind == .label) {
                    for (module.symbols, 0..) |symbol, index| {
                        if (symbol.label.hub_address != null and symbol.label.hub_address.? == offset and same_location(symbol.source_location, line.location)) {
                            try emit_label_name(out, module, symbol.name, index);
                            try out.writeByte('\n');
                        }
                    }
                }
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
                    if (line.offset == offset and line.kind == .code and line.mnemonic != null)
                        try emit_instruction_comment(out, line);
                }
            }
        }
        if (kind == .padding) {
            const byte = data[offset];
            var count: usize = 1;
            while (count < 16 and offset + count < data.len and kinds[offset + count] == .padding and data[offset + count] == byte and !has_boundary(modules, offset + count)) : (count += 1) {}
            try out.writeAll("  BYTE ");
            try emit_number(out, byte, .hex, false);
            if (count > 1) try out.print("[{d}]", .{count});
            if (padding_directive(modules, offset, offset + count)) |directive|
                try out.print(" ' {s}", .{directive})
            else
                try out.writeAll(" ' padding");
            try out.writeByte('\n');
            offset += count;
            continue;
        }
        const width: usize = switch (kind) {
            .word => 2,
            .code, .long => 4,
            else => 1,
        };
        const max_count: usize = switch (width) {
            1 => 16,
            2 => 8,
            else => 8,
        };
        const name = switch (width) {
            1 => "BYTE",
            2 => "WORD",
            else => "LONG",
        };
        try out.print("  {s} ", .{name});
        var count: usize = 0;
        while (count < max_count and offset + width <= data.len) {
            if (kinds[offset] != kind) break;
            if (count > 0) {
                if (has_boundary(modules, offset)) break;
                try out.writeAll(", ");
            }
            const value = switch (width) {
                1 => @as(u32, data[offset]),
                2 => @as(u32, std.mem.readInt(u16, data[offset..][0..2], .little)),
                else => std.mem.readInt(u32, data[offset..][0..4], .little),
            };
            try emit_number(out, value, data_style(modules, offset, width), false);
            offset += width;
            count += 1;
        }
        if (count == 0) {
            try emit_number(out, data[offset], .hex, false);
            offset += 1;
        }
        try out.writeByte('\n');
    }
    for (modules) |module| for (module.sources) |source| {
        const last_line: u32 = @intCast(std.mem.count(u8, source.text, "\n") + 1);
        try emit_source_comments(out, allocator, &comment_cursors, module, .{ .source = source.path, .line = last_line, .column = 1 });
    };
    try out.flush();
}

fn same_location(a: ?@import("../frontend/ast.zig").Location, b: @import("../frontend/ast.zig").Location) bool {
    const left = a orelse return false;
    return left.line == b.line and left.column == b.column and
        (if (left.source) |path| b.source != null and std.mem.eql(u8, path, b.source.?) else b.source == null);
}

fn source_for(module: Module, path: []const u8) ?[]const u8 {
    for (module.sources) |source| if (std.mem.eql(u8, source.path, path)) return source.text;
    return null;
}

fn comment_start(line: []const u8) ?usize {
    var quote: ?u8 = null;
    var index: usize = 0;
    while (index + 1 < line.len) : (index += 1) {
        if (quote) |delimiter| {
            if (line[index] == '\\') {
                index += 1;
            } else if (line[index] == delimiter) {
                quote = null;
            }
        } else if (line[index] == '"' or line[index] == '\'') {
            quote = line[index];
        } else if (line[index] == '/' and line[index + 1] == '/') {
            return index;
        }
    }
    return null;
}

fn emit_source_comments(out: *std.Io.Writer, allocator: std.mem.Allocator, cursors: *std.StringHashMapUnmanaged(u32), module: Module, location: @import("../frontend/ast.zig").Location) !void {
    const path = location.source orelse return;
    const source = source_for(module, path) orelse return;
    const previous = cursors.get(path) orelse 0;
    if (location.line <= previous) return;
    for (previous + 1..@as(usize, location.line) + 1) |line_number| {
        const line = source_line.get(source, @intCast(line_number)) orelse continue;
        if (comment_start(line)) |start| {
            if (!std.mem.startsWith(u8, line[start..], "//?"))
                try out.print("'{s}\n", .{line[start + 2 ..]});
        } else if (std.mem.trim(u8, line, " \t").len == 0 and line_number > 1) {
            try out.writeByte('\n');
        }
    }
    try cursors.put(allocator, path, location.line);
}

fn has_boundary(modules: []const Module, offset: usize) bool {
    for (modules) |module| {
        for (module.segments) |segment| if (segment.hub_offset == offset) return true;
        for (module.symbols) |symbol| if (symbol.label.hub_address) |hub| {
            if (hub == offset) return true;
        };
        for (module.line_data) |line| if (line.offset == offset) return true;
    }
    return false;
}

fn data_style(modules: []const Module, offset: usize, width: usize) Module.LineData.NumberStyle {
    for (modules) |module| for (module.line_data) |line| {
        if (line.kind != .byte and line.kind != .word and line.kind != .long) continue;
        if (offset < line.offset or offset >= line.offset + line.length) continue;
        const index = (offset - line.offset) / width;
        if (index < line.number_styles.len) return line.number_styles[index];
    };
    return .hex;
}

fn padding_directive(modules: []const Module, start: usize, end: usize) ?[]const u8 {
    for (modules) |module| {
        var next: ?Module.LineData = null;
        for (module.line_data) |line| {
            if (line.offset >= end and (next == null or line.offset < next.?.offset)) next = line;
        }
        const target = next orelse continue;
        const path = target.location.source orelse continue;
        const source = source_for(module, path) orelse continue;
        var previous_line: u32 = 0;
        for (module.line_data) |line| {
            if (line.offset + line.length <= start and line.location.source != null and std.mem.eql(u8, line.location.source.?, path))
                previous_line = @max(previous_line, line.location.line);
        }
        if (target.location.line <= previous_line + 1) continue;
        var found: ?[]const u8 = null;
        for (@as(usize, previous_line) + 1..@as(usize, target.location.line)) |line_number| {
            const text = source_line.get(source, @intCast(line_number)) orelse continue;
            const trimmed = std.mem.trim(u8, text, " \t\r");
            if (trimmed.len >= 6 and std.ascii.eqlIgnoreCase(trimmed[0..6], ".align")) found = trimmed;
        }
        if (found) |text| return text;
    }
    return null;
}

fn emit_number(out: *std.Io.Writer, value: u32, style: Module.LineData.NumberStyle, register: bool) !void {
    if (value == 0 or register or style == .decimal)
        try out.print("{d}", .{value})
    else
        try out.print("${X}", .{value});
}

fn emit_instruction_comment(out: *std.Io.Writer, line: Module.LineData) !void {
    try out.writeAll("  ' ");
    if (line.condition) |condition| try out.print("{t} ", .{condition.encode()});
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
    if (line.effect) |effect| try out.print(" {t}", .{effect});
    try out.writeByte('\n');
}

fn emit_readable_instruction(out: *std.Io.Writer, module: Module, line: Module.LineData) !bool {
    if (line.kind != .code or line.length != 4 or line.mnemonic == null) return false;
    if (line.condition) |condition| if (condition == .@"return") return false;
    if (line.effect != null and line.operands.len == 0 and !std.ascii.eqlIgnoreCase(line.mnemonic.?, "RET")) return false;
    if (line.effect) |effect| switch (effect) {
        .wc, .wz, .wcz => {},
        else => return false,
    };
    for (line.operands, 0..) |operand, index| {
        if (std.ascii.eqlIgnoreCase(line.mnemonic.?, "REP") and index == 0 and std.mem.startsWith(u8, operand.syntax, "@")) {
            if (rep_target_index(module, line, operand) == null) return false;
            continue;
        }
        if ((operand.encoding != .register and operand.encoding != .reg_or_imm) or operand.pcrel) return false;
        if (operand.value.flags.augment or operand.value.flags.addressing != .auto) return false;
        if (operand_number(operand) == null) return false;
    }

    try out.writeAll("  ");
    if (line.condition) |condition| try out.print("{t} ", .{condition.encode()});
    try out.writeAll(line.mnemonic.?);
    for (line.operands, 0..) |operand, index| {
        try out.writeAll(if (index == 0) " " else ", ");
        if (std.ascii.eqlIgnoreCase(line.mnemonic.?, "REP") and index == 0 and std.mem.startsWith(u8, operand.syntax, "@")) {
            try out.writeByte('@');
            const target_index = rep_target_index(module, line, operand).?;
            try emit_label_name(out, module, module.symbols[target_index].name, target_index);
            continue;
        }
        if (operand.value.flags.usage == .literal) try out.writeByte('#');
        if (operand.source_kind == .symbol and std.mem.eql(u8, operand.syntax, "altered")) {
            try out.writeAll("0-0");
            continue;
        }
        if (operand.source_kind == .symbol) {
            for (module.constants, 0..) |constant, constant_index| {
                if (std.mem.eql(u8, operand.syntax, constant.name) and constant.value.value == .int and constant.value.value.int >= std.math.minInt(i32) and constant.value.value.int <= std.math.maxInt(u32)) {
                    try emit_constant_name(out, module, constant.name, constant_index);
                    break;
                }
            } else {
                for (module.symbols, 0..) |symbol, symbol_index| {
                    if (matches_label(operand, symbol) and symbol.label.hub_address != null and
                        symbol.label.hub_address.? / 4 == operand_number(operand).?)
                    {
                        try emit_label_name(out, module, symbol.name, symbol_index);
                        break;
                    }
                } else {
                    try emit_operand_number(out, operand);
                }
            }
        } else {
            try emit_operand_number(out, operand);
        }
    }
    if (line.effect) |effect| try out.print(" {t}", .{effect});
    try out.writeByte('\n');
    return true;
}

fn matches_label(operand: Module.LineData.Operand, symbol: Module.Symbol) bool {
    if (std.mem.eql(u8, operand.syntax, symbol.name)) return true;
    if (!std.mem.startsWith(u8, operand.syntax, ".") or operand.value.value != .address) return false;
    const colon = std.mem.lastIndexOfScalar(u8, symbol.name, ':') orelse return false;
    return std.mem.eql(u8, operand.syntax[1..], symbol.name[colon + 1 ..]) and std.meta.eql(operand.value.value.address, symbol.label);
}

fn rep_target_index(module: Module, line: Module.LineData, operand: Module.LineData.Operand) ?usize {
    if (operand.value.value != .int) return null;
    const target = operand.syntax[1..];
    for (module.symbols, 0..) |symbol, index| {
        const name = if (std.mem.startsWith(u8, target, ".")) blk: {
            const colon = std.mem.lastIndexOfScalar(u8, symbol.name, ':') orelse break :blk symbol.name;
            break :blk symbol.name[colon..];
        } else symbol.name;
        // Scoped local names use ':' where their source spelling uses '.'.
        const matches = if (name.len > 0 and name[0] == ':')
            std.mem.eql(u8, target[1..], name[1..])
        else
            std.mem.eql(u8, target, name);
        if (!matches) continue;
        const hub = symbol.label.hub_address orelse continue;
        const bytes = @as(i64, hub) - (@as(i64, line.offset) + line.length);
        if (@mod(bytes, 4) == 0 and @divTrunc(bytes, 4) == operand.value.value.int) return index;
    }
    return null;
}

fn emit_operand_number(out: *std.Io.Writer, operand: Module.LineData.Operand) !void {
    const style: Module.LineData.NumberStyle = if (operand.source_kind == .integer and !std.mem.startsWith(u8, operand.syntax, "$") and !std.mem.startsWith(u8, operand.syntax, "0x") and !std.mem.startsWith(u8, operand.syntax, "0X")) .decimal else .hex;
    try emit_number(out, operand_number(operand).?, style, operand.encoding == .register or operand.value.flags.usage == .register);
}

fn keyword(name: []const u8) bool {
    if (name.len >= 3 and std.ascii.eqlIgnoreCase(name[0..3], "IF_")) return true;
    const words = [_][]const u8{
        // Spin2 language and DAT keywords, including names used in operands.
        "_",         "__ANDTHEN__", "__ORELSE__", "__REG__",   "__BUILTIN_ALLOCA",
        "ADDBITS",   "ADDPINS",     "ALIGNL",     "ALIGNW",    "ASM",
        "ASM_CONST", "ASMCLK",      "BMASK",      "BYTE",      "BYTEFIT",
        "CASE",      "CASE_FAST",   "COGNEW",     "COGSPIN",   "CON",
        "COUNT",     "DAT",         "DEBUG",      "ELSE",      "ELSEIF",
        "ELSEIFNOT", "END",         "ENDASM",     "FABS",      "FILE",
        "FIT",       "FLOAT",       "FRAC",       "FROM",      "FSQRT",
        "FVAR",      "FVARS",       "IF",         "IFNOT",     "LONG",
        "LOOKDOWN",  "LOOKDOWNZ",   "LOOKUP",     "LOOKUPZ",   "NAN",
        "NEXT",      "OBJ",         "ORG",        "ORGH",      "ORGF",
        "OTHER",     "PINH",        "PINHIGH",    "PINL",      "PINLOW",
        "PINR",      "PINREAD",     "PINT",       "PINTOGGLE", "PINW",
        "PINWRITE",  "PRI",         "PUB",        "QUIT",      "REG",
        "REGEXEC",   "REGLOAD",     "REPEAT",     "RES",       "RETURN",
        "ROUND",     "SQRT",        "STEP",       "STRING",    "THEN",
        "TO",        "TRUNC",       "UNTIL",      "VAR",       "WHILE",
        "WITH",      "WORD",        "WORDFIT",
        // PASM condition and effect keywords.
           "_RET_",     "WC",
        "WZ",        "WCZ",         "ANDC",       "ANDZ",      "ORC",
        "ORZ",       "XORC",        "XORZ",
    };
    for (words) |word| if (std.ascii.eqlIgnoreCase(name, word)) return true;
    for (instructions) |instruction| if (std.ascii.eqlIgnoreCase(name, instruction.mnemonic)) return true;
    for (stdlib.common.constants.keys()) |constant| if (std.ascii.eqlIgnoreCase(name, constant)) return true;
    for (stdlib.p2.constants.keys(), stdlib.p2.constants.values()) |constant, value| {
        if (value.value == .register and std.ascii.eqlIgnoreCase(name, constant)) return true;
    }
    return false;
}

fn emit_label_name(out: *std.Io.Writer, module: Module, name: []const u8, index: usize) !void {
    if (std.mem.lastIndexOfScalar(u8, name, ':')) |colon| {
        const local_name = name[colon + 1 ..];
        try out.writeByte('.');
        for (module.symbols, 0..) |other, other_index| {
            if (other_index == index) continue;
            const other_colon = std.mem.lastIndexOfScalar(u8, other.name, ':') orelse continue;
            if (std.ascii.eqlIgnoreCase(name[0..colon], other.name[0..other_colon]) and
                same_identifier(local_name, other.name[other_colon + 1 ..]))
            {
                return emit_conflicted_identifier(out, module, local_name, 's', index);
            }
        }
        return emit_identifier(out, local_name);
    }
    var conflict = keyword(name);
    for (module.symbols, 0..) |other, other_index| {
        if (other_index != index and std.mem.indexOfScalar(u8, other.name, ':') == null and same_identifier(name, other.name)) conflict = true;
    }
    for (module.constants) |constant| if (same_identifier(name, constant.name)) {
        conflict = true;
    };
    if (conflict)
        try emit_conflicted_identifier(out, module, name, 's', index)
    else
        try emit_identifier(out, name);
}

fn emit_conflicted_identifier(out: *std.Io.Writer, module: Module, name: []const u8, kind: u8, index: usize) !void {
    // Reserve a prefix absent from every source identifier. Kind and index then
    // distinguish generated names without colliding with existing declarations.
    var buffer: [64]u8 = undefined;
    var attempt: usize = 0;
    const prefix = search: while (true) : (attempt += 1) {
        const candidate = try std.fmt.bufPrint(&buffer, "_propan{d}_", .{attempt});
        for (module.constants) |constant| {
            if (constant.name.len >= candidate.len and same_identifier(constant.name[0..candidate.len], candidate)) continue :search;
        }
        for (module.symbols) |symbol| {
            const identifier = if (std.mem.lastIndexOfScalar(u8, symbol.name, ':')) |colon| symbol.name[colon + 1 ..] else symbol.name;
            if (identifier.len >= candidate.len and same_identifier(identifier[0..candidate.len], candidate)) continue :search;
        }
        break :search candidate;
    };
    try out.print("{s}{c}{d}_", .{ prefix, kind, index });
    try emit_identifier(out, name);
}

fn emit_identifier(out: *std.Io.Writer, name: []const u8) !void {
    for (name) |character| try out.writeByte(if (std.ascii.isAlphanumeric(character) or character == '_') character else '_');
}

fn same_identifier(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;
    for (a, b) |left, right| {
        const normalized_left = if (std.ascii.isAlphanumeric(left) or left == '_') left else '_';
        const normalized_right = if (std.ascii.isAlphanumeric(right) or right == '_') right else '_';
        if (std.ascii.toLower(normalized_left) != std.ascii.toLower(normalized_right)) return false;
    }
    return true;
}

fn emit_constant_name(out: *std.Io.Writer, module: Module, name: []const u8, index: usize) !void {
    var conflict = keyword(name);
    for (module.symbols) |symbol| if (std.mem.indexOfScalar(u8, symbol.name, ':') == null and same_identifier(name, symbol.name)) {
        conflict = true;
    };
    for (module.constants, 0..) |other, other_index| {
        if (other_index != index and same_identifier(name, other.name)) conflict = true;
    }
    if (conflict)
        try emit_conflicted_identifier(out, module, name, 'c', index)
    else
        try emit_identifier(out, name);
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
