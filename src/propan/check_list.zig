const std = @import("std");
const Module = @import("Module.zig");
const diagnostics = @import("diagnostics.zig");
const eval = @import("stdlib/eval.zig");
const stdlib = @import("stdlib/stdlib.zig");

const Location = @import("frontend/ast.zig").Location;
const Address = union(enum) { absent, value: u32 };
const SymbolType = diagnostics.ChecklistSymbolType;
const MemoryFormat = enum { u8, u16, u32, hex };

const SymbolCheck = struct {
    name: []const u8,
    kind: SymbolType,
    hub: Address,
    local: ?Address,
};
const SegmentCheck = struct {
    start: u32,
    length: ?u32,
    mode: ?eval.ExecMode,
};
const MemoryCheck = struct {
    start: u32,
    whole: bool,
    bytes: []const u8,
};
const Check = struct {
    location: Location,
    value: union(enum) { symbol: SymbolCheck, segment: SegmentCheck, memory: MemoryCheck },
};

pub const List = struct {
    arena: std.heap.ArenaAllocator,
    checks: []const Check,

    pub fn deinit(list: *List) void {
        list.arena.deinit();
        list.* = undefined;
    }

    pub fn evaluate(list: List, module: Module, flat: []const u8, errors: *diagnostics.Collection) !void {
        for (list.checks) |check| switch (check.value) {
            .symbol => |symbol| try evaluateSymbol(check.location, symbol, module, errors),
            .segment => |segment| try evaluateSegment(check.location, segment, module, errors),
            .memory => |memory| try evaluateMemory(check.location, memory, flat, errors),
        };
    }
};

const PendingMemory = struct {
    location: Location,
    start: u32,
    whole: bool,
    format: MemoryFormat,
    bytes: std.ArrayList(u8) = .empty,
    invalid: bool = false,
};

pub fn parse(allocator: std.mem.Allocator, path: []const u8, source: []const u8, errors: *diagnostics.Collection) !?List {
    var lines = std.mem.splitScalar(u8, source, '\n');
    const header = std.mem.trimEnd(u8, lines.next() orelse return null, "\r");
    if (!std.mem.eql(u8, header, "//? PROPAN CHECK LIST")) return null;

    var list: List = .{ .arena = .init(allocator), .checks = &.{} };
    errdefer list.deinit();
    const arena = list.arena.allocator();
    var checks: std.ArrayList(Check) = .empty;
    var pending: ?PendingMemory = null;
    var line_number: u32 = 1;

    while (lines.next()) |raw_line| {
        line_number += 1;
        const line = std.mem.trimEnd(u8, raw_line, "\r");
        if (!std.mem.startsWith(u8, line, "//?")) break;
        const location: Location = .{ .source = path, .line = line_number, .column = 4 };
        const tokens = try tokenize(arena, line[3..]);
        if (pending) |current| {
            var memory = current;
            if (try addMemoryTokens(arena, &memory, tokens, location, errors)) {
                if (!memory.invalid) try checks.append(arena, .{ .location = memory.location, .value = .{ .memory = .{
                    .start = memory.start,
                    .whole = memory.whole,
                    .bytes = try memory.bytes.toOwnedSlice(arena),
                } } });
                pending = null;
            } else pending = memory;
            continue;
        }
        if (tokens.len == 0) continue;
        if (std.mem.eql(u8, tokens[0], "sym:")) {
            if (tokens.len != 3) {
                try errors.emit_diag(location, .err_checklist_sym_requires_a_name_and_type_hub_local);
                continue;
            }
            const symbol = parseSymbol(tokens[1], tokens[2]) orelse {
                try errors.emit_diag(location, .err_invalid_checklist_symbol_specification);
                continue;
            };
            try checks.append(arena, .{ .location = location, .value = .{ .symbol = symbol } });
        } else if (std.mem.eql(u8, tokens[0], "seg:")) {
            const segment = parseSegment(tokens) orelse {
                try errors.emit_diag(location, .err_invalid_checklist_segment_specification);
                continue;
            };
            try checks.append(arena, .{ .location = location, .value = .{ .segment = segment } });
        } else if (std.mem.eql(u8, tokens[0], "mem:")) {
            if (tokens.len < 5 or !std.mem.eql(u8, tokens[4], "[")) {
                try errors.emit_diag(location, .err_checklist_mem_requires_address_comparison_format_and);
                continue;
            }
            const start = parseUnsigned(tokens[1]) orelse {
                try errors.emit_diag(location, .err_invalid_checklist_memory_address);
                continue;
            };
            const whole = if (std.mem.eql(u8, tokens[2], "==")) true else if (std.mem.eql(u8, tokens[2], "<-")) false else {
                try errors.emit_diag(location, .err_invalid_checklist_memory_comparison);
                continue;
            };
            const format = std.meta.stringToEnum(MemoryFormat, tokens[3]) orelse {
                try errors.emit_diag(location, .err_invalid_checklist_memory_format);
                continue;
            };
            var memory: PendingMemory = .{ .location = location, .start = start, .whole = whole, .format = format };
            if (try addMemoryTokens(arena, &memory, tokens[5..], location, errors)) {
                if (!memory.invalid) try checks.append(arena, .{ .location = location, .value = .{ .memory = .{
                    .start = start,
                    .whole = whole,
                    .bytes = try memory.bytes.toOwnedSlice(arena),
                } } });
            } else pending = memory;
        } else {
            try errors.emit_diag(location, .{
                .err_unknown_checklist_check = .{
                    .check = tokens[0],
                },
            });
        }
    }
    if (pending) |memory| try errors.emit_diag(memory.location, .err_unterminated_checklist_memory_block);
    list.checks = try checks.toOwnedSlice(arena);
    return list;
}

fn tokenize(allocator: std.mem.Allocator, line: []const u8) ![]const []const u8 {
    var tokens: std.ArrayList([]const u8) = .empty;
    var i: usize = 0;
    while (i < line.len) {
        if (std.ascii.isWhitespace(line[i]) or line[i] == ',') {
            i += 1;
            continue;
        }
        if (line[i] == '[' or line[i] == ']') {
            try tokens.append(allocator, line[i .. i + 1]);
            i += 1;
            continue;
        }
        const start = i;
        while (i < line.len and !std.ascii.isWhitespace(line[i]) and line[i] != ',' and line[i] != '[' and line[i] != ']') : (i += 1) {}
        try tokens.append(allocator, line[start..i]);
    }
    return try tokens.toOwnedSlice(allocator);
}

fn parseUnsigned(text: []const u8) ?u32 {
    const hex = std.mem.startsWith(u8, text, "0x") or std.mem.startsWith(u8, text, "0X");
    return std.fmt.parseInt(u32, if (hex) text[2..] else text, if (hex) 16 else 10) catch null;
}

fn parseAddress(text: []const u8) ?Address {
    if (std.mem.eql(u8, text, "-")) return .absent;
    return .{ .value = parseUnsigned(text) orelse return null };
}

fn parseSymbol(name: []const u8, spec: []const u8) ?SymbolCheck {
    if (name.len == 0) return null;
    var fields = std.mem.splitScalar(u8, spec, ':');
    const kind = std.meta.stringToEnum(SymbolType, fields.next() orelse return null) orelse return null;
    const hub = parseAddress(fields.next() orelse return null) orelse return null;
    const local = if (fields.next()) |field| parseAddress(field) orelse return null else null;
    if (fields.next() != null) return null;
    return .{ .name = name, .kind = kind, .hub = hub, .local = local };
}

fn parseSegment(tokens: []const []const u8) ?SegmentCheck {
    if (tokens.len < 2 or tokens.len > 4) return null;
    const start = parseUnsigned(tokens[1]) orelse return null;
    var length: ?u32 = null;
    var mode: ?eval.ExecMode = null;
    if (tokens.len >= 3) {
        if (parseUnsigned(tokens[2])) |value| length = value else mode = parseMode(tokens[2]) orelse return null;
    }
    if (tokens.len == 4) {
        if (length == null or mode != null) return null;
        mode = parseMode(tokens[3]) orelse return null;
    }
    return .{ .start = start, .length = length, .mode = mode };
}

fn parseMode(text: []const u8) ?eval.ExecMode {
    if (std.mem.eql(u8, text, "cogexec")) return .cog;
    if (std.mem.eql(u8, text, "lutexec")) return .lut;
    if (std.mem.eql(u8, text, "hubexec")) return .hub;
    if (std.mem.eql(u8, text, "regspace")) return .regspace;
    if (std.mem.eql(u8, text, "data")) return .data;
    return null;
}

fn parseNumber(text: []const u8) ?i64 {
    const negative = std.mem.startsWith(u8, text, "-");
    const unsigned = if (negative) text[1..] else text;
    const hex = std.mem.startsWith(u8, unsigned, "0x") or std.mem.startsWith(u8, unsigned, "0X");
    const magnitude = std.fmt.parseInt(u64, if (hex) unsigned[2..] else unsigned, if (hex) 16 else 10) catch return null;
    if (magnitude > (if (negative) @as(u64, 0x80000000) else @as(u64, 0xFFFFFFFF))) return null;
    return if (negative) -@as(i64, @intCast(magnitude)) else @intCast(magnitude);
}

fn addMemoryTokens(allocator: std.mem.Allocator, memory: *PendingMemory, tokens: []const []const u8, location: Location, errors: *diagnostics.Collection) !bool {
    for (tokens, 0..) |token, index| {
        if (std.mem.eql(u8, token, "]")) {
            if (index + 1 != tokens.len) {
                memory.invalid = true;
                try errors.emit_diag(location, .err_text_after_checklist_memory_block);
            }
            return true;
        }
        if (std.mem.eql(u8, token, "[")) {
            memory.invalid = true;
            try errors.emit_diag(location, .err_unexpected_in_checklist_memory_block);
            continue;
        }
        if (memory.format == .hex) {
            if (token.len % 2 != 0 or token.len == 0) {
                memory.invalid = true;
                try errors.emit_diag(location, .err_checklist_hex_bytes_require_pairs_of_digits);
                continue;
            }
            var offset: usize = 0;
            while (offset < token.len) : (offset += 2) {
                const byte = std.fmt.parseInt(u8, token[offset..][0..2], 16) catch {
                    memory.invalid = true;
                    try errors.emit_diag(location, .err_invalid_checklist_hex_byte);
                    break;
                };
                try memory.bytes.append(allocator, byte);
            }
        } else {
            const value = parseNumber(token) orelse {
                memory.invalid = true;
                try errors.emit_diag(location, .{
                    .err_invalid_checklist_memory_integer = .{
                        .token = token,
                    },
                });
                continue;
            };
            const width: usize = switch (memory.format) {
                .u8 => 1,
                .u16 => 2,
                .u32 => 4,
                .hex => unreachable,
            };
            const bits: u32 = @truncate(@as(u64, @bitCast(value)));
            for (0..width) |i| try memory.bytes.append(allocator, @truncate(bits >> @as(u5, @intCast(8 * i))));
        }
    }
    return false;
}

fn addressMatches(expected: Address, actual: ?u32) bool {
    return switch (expected) {
        .absent => actual == null,
        .value => |value| actual != null and actual.? == value,
    };
}

fn evaluateSymbol(location: Location, check: SymbolCheck, module: Module, errors: *diagnostics.Collection) !void {
    var kind: ?SymbolType = null;
    var hub: ?u32 = null;
    var local: ?u32 = null;
    for (module.symbols) |symbol| {
        if (!std.mem.eql(u8, symbol.name, check.name)) continue;
        kind = switch (symbol.type) {
            .code => .code,
            .data => .data,
        };
        if (symbol.label.hub_address) |value| hub = value;
        local = symbol.label.get_local(.data);
        break;
    }
    if (kind == null) for (module.constants) |constant| {
        if (std.mem.eql(u8, constant.name, check.name)) {
            kind = .constant;
            break;
        }
    };
    if (kind == null and (stdlib.common.constants.get(check.name) != null or stdlib.p2.constants.get(check.name) != null)) kind = .builtin;
    if (kind == null) {
        try errors.emit_diag(location, .{
            .err_checklist_symbol_does_not_exist = .{
                .name = check.name,
            },
        });
    } else if (kind.? != check.kind or !addressMatches(check.hub, hub) or (check.local != null and !addressMatches(check.local.?, local))) {
        try errors.emit_diag(location, .{
            .err_checklist_symbol_does_not_match_actual_type_hub_local = .{
                .name = check.name,
                .kind = kind.?,
                .hub = hub,
                .local = local,
            },
        });
    }
}

fn evaluateSegment(location: Location, check: SegmentCheck, module: Module, errors: *diagnostics.Collection) !void {
    for (module.segments) |segment| {
        if (segment.hub_offset == check.start and
            (check.length == null or segment.data.len == check.length.?) and
            (check.mode == null or segment.exec_mode == check.mode.?)) return;
    }
    for (module.regspace_segments) |start| {
        if (start == check.start and
            (check.length == null or check.length.? == 0) and
            (check.mode == null or check.mode.? == .regspace)) return;
    }
    try errors.emit_diag(location, .{
        .err_checklist_segment_at_0x_x_does_not_match_length_or_mode = .{
            .start = check.start,
        },
    });
}

fn evaluateMemory(location: Location, check: MemoryCheck, flat: []const u8, errors: *diagnostics.Collection) !void {
    if (check.whole and check.start != 0) {
        try errors.emit_diag(location, .err_checklist_whole_memory_comparison_must_start_at_zero);
        return;
    }
    if (check.whole and check.bytes.len != flat.len) {
        try errors.emit_diag(location, .{
            .err_checklist_memory_length_mismatch_expected_got = .{
                .expected = check.bytes.len,
                .actual = flat.len,
            },
        });
        return;
    }
    const start: usize = check.start;
    if (start > flat.len or check.bytes.len > flat.len - start) {
        try errors.emit_diag(location, .err_checklist_memory_range_exceeds_assembled_output);
        return;
    }
    for (check.bytes, flat[start..][0..check.bytes.len], 0..) |expected, actual, offset| {
        if (expected != actual) {
            try errors.emit_diag(location, .{
                .err_checklist_memory_byte_mismatch = .{
                    .offset = start + offset,
                    .expected = expected,
                    .actual = actual,
                },
            });
            return;
        }
    }
}

test "parser accepts symbol, segment, and multiline memory checks" {
    const source =
        \\//? PROPAN CHECK LIST
        \\//? sym: target code:0x100:4
        \\//? sym: spare data:-:-
        \\//? seg: 0x100 8 lutexec
        \\//? seg: 0x108 regspace
        \\//? mem: 0 <- u16 [ -1, 0x1234 ]
        \\//? mem: 4 <- hex [ aabb
        \\//? CC ]
        \\LONG 0
        \\//? sym: ignored code:0
    ;
    var errors: diagnostics.Collection = .init(std.testing.allocator);
    defer errors.deinit();
    var list = (try parse(std.testing.allocator, "parser.propan", source, &errors)).?;
    defer list.deinit();
    try std.testing.expectEqual(@as(usize, 6), list.checks.len);
    try std.testing.expectEqual(@as(u32, 2), list.checks[0].location.line);
    try std.testing.expectEqual(@as(u32, 0x100), list.checks[0].value.symbol.hub.value);
    try std.testing.expectEqual(@as(u32, 4), list.checks[0].value.symbol.local.?.value);
    try std.testing.expect(list.checks[1].value.symbol.hub == .absent);
    try std.testing.expectEqual(eval.ExecMode.lut, list.checks[2].value.segment.mode.?);
    try std.testing.expectEqual(eval.ExecMode.regspace, list.checks[3].value.segment.mode.?);
    try std.testing.expectEqualSlices(u8, &.{ 0xFF, 0xFF, 0x34, 0x12 }, list.checks[4].value.memory.bytes);
    try std.testing.expectEqualSlices(u8, &.{ 0xAA, 0xBB, 0xCC }, list.checks[5].value.memory.bytes);
    try std.testing.expect(!errors.has_errors());
}

test "parser ignores unmarked files and reports malformed checks" {
    var errors: diagnostics.Collection = .init(std.testing.allocator);
    defer errors.deinit();
    try std.testing.expect((try parse(std.testing.allocator, "plain.propan", "LONG 1\n", &errors)) == null);
    const source =
        \\//? PROPAN CHECK LIST
        \\//? sym: x code:garbage
        \\//? seg: 3 4 nonsense
        \\//? mem: 0 <- hex [ ABC ]
        \\//? mem: 0 == u8 [ 4294967296 ]
        \\//? mem: 0 == u32 [ 1
    ;
    var list = (try parse(std.testing.allocator, "bad.propan", source, &errors)).?;
    defer list.deinit();
    try std.testing.expectEqual(@as(usize, 0), list.checks.len);
    try std.testing.expectEqual(@as(usize, 5), errors.diagnostics.items.len);
    try std.testing.expectEqual(@as(u32, 2), errors.diagnostics.items[0].location.?.line);
}

test "evaluator checks symbols, virtual segments, and both memory modes" {
    const source =
        \\//? PROPAN CHECK LIST
        \\//? sym: target code:0:4
        \\//? sym: storage data:-:2
        \\//? sym: C constant:-:-
        \\//? sym: TRUE builtin:-:-
        \\//? seg: 0 4 lutexec
        \\//? seg: 4 0 regspace
        \\//? mem: 0 == u8 [ 1 2 3 4 ]
        \\//? mem: 1 <- hex [0203]
    ;
    var errors: diagnostics.Collection = .init(std.testing.allocator);
    defer errors.deinit();
    var list = (try parse(std.testing.allocator, "checks.propan", source, &errors)).?;
    defer list.deinit();
    const seg: Module.Segment_ID = @enumFromInt(0);
    var module: Module = .{
        .arena = .init(std.testing.allocator),
        .segments = &.{.{ .id = seg, .hub_offset = 0, .data = &.{ 1, 2, 3, 4 }, .exec_mode = .lut }},
        .regspace_segments = &.{4},
        .line_data = &.{},
        .symbols = &.{
            .{ .name = "target", .label = .init_lut(seg, 0, 4), .type = .code },
            .{ .name = "storage", .label = .init(seg, null, .{ .regspace = 2 }), .type = .data },
        },
        .constants = &.{.{ .name = "C", .value = .int(1), .location = .empty }},
    };
    defer module.deinit();
    try list.evaluate(module, &.{ 1, 2, 3, 4 }, &errors);
    try std.testing.expect(!errors.has_errors());
}

test "evaluator reports every mismatch" {
    const source =
        \\//? PROPAN CHECK LIST
        \\//? sym: absent code:0
        \\//? seg: 0 2 cogexec
        \\//? mem: 0 == hex [ 02 ]
        \\//? mem: 3 <- u32 [ 1 ]
        \\//? mem: 1 == hex [ 01 ]
    ;
    var errors: diagnostics.Collection = .init(std.testing.allocator);
    defer errors.deinit();
    var list = (try parse(std.testing.allocator, "mismatch.propan", source, &errors)).?;
    defer list.deinit();
    const seg: Module.Segment_ID = @enumFromInt(0);
    var module: Module = .{
        .arena = .init(std.testing.allocator),
        .segments = &.{.{ .id = seg, .hub_offset = 0, .data = &.{1}, .exec_mode = .cog }},
        .line_data = &.{},
        .symbols = &.{},
        .constants = &.{},
    };
    defer module.deinit();
    try list.evaluate(module, &.{1}, &errors);
    try std.testing.expectEqual(@as(usize, 5), errors.diagnostics.items.len);
    try std.testing.expectEqual(@as(u32, 4), errors.diagnostics.items[2].location.?.line);
}
