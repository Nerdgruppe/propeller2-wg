const std = @import("std");
const Issue = @import("sim/Issue.zig");
pub const IssueKind = std.meta.Tag(Issue.Kind);
pub const IssueExpectation = struct { kind: IssueKind, count: u64 };

test "issue expectations merge by kind and reject invalid counts and names" {
    var arena: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer arena.deinit();

    var errors: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer errors.deinit();

    const source = "//? WINDTUNNEL CHECK LIST\n" ++
        "//? issues: streamer_mode = 2\n//? issues: pc_domain = 3\n" ++
        "//?\n//? run: \"first\"\n//? issues: streamer_mode = 0\n//?\n//? run: \"second\"\n";
    const list = try parse(arena.allocator(), "issues", source, &errors.writer);
    const first = try list.forRun(arena.allocator(), list.runs[0]);
    try std.testing.expectEqual(@as(usize, 2), first.issues.len);
    try std.testing.expectEqual(IssueKind.pc_domain, first.issues[0].kind);
    try std.testing.expectEqual(@as(u64, 0), first.issues[1].count);
    const second = try list.forRun(arena.allocator(), list.runs[1]);
    try std.testing.expectEqual(@as(u64, 2), second.issues[0].count);
    for ([_][]const u8{
        "//? issues: streamer_mode = -1\n",
        "//? issues: imaginary = 1\n",
        "//? issues: streamer_mode = 1\n//? issues: streamer_mode = 2\n",
    }) |invalid| {
        const text = try std.mem.concat(arena.allocator(), u8, &.{ "//? WINDTUNNEL CHECK LIST\n", invalid });
        try std.testing.expectError(error.InvalidChecklist, parse(arena.allocator(), "issues", text, &errors.writer));
    }
}

pub const Address = union(enum) {
    number: u32,
    symbol: []const u8,
};
pub const Target = union(enum) {
    sym: []const u8,
    reg: Address,
    hub: Address,
    c,
    z,
    q,
    dacs,
};
pub const Assignment = struct {
    line: u32,
    cog: ?u3 = null,
    target: Target,
    bytes: []const u8,
};
pub const Profile = enum { cog, program };
pub const Constant = struct {
    name: []const u8,
    value: i64,
};
pub const Run = struct {
    cog: ?u3 = null,
    constants: []const Constant = &.{},
    name: ?[]const u8 = null,
    issues: []const IssueExpectation = &.{},
    pre: []const Assignment = &.{},
    post: []const Assignment = &.{},
    stdin: ?[]const u8 = null,
    stdout: ?[]const u8 = null,
    stdin_after: ?[]const u8 = null,
};
pub const List = struct {
    profile: Profile = .cog,
    cog: u3 = 0,
    entry: ?[]const u8 = null,
    stop: ?[]const u8 = null,
    oracle_off: ?[]const u8 = null,
    fast_io_off: ?[]const u8 = null,
    max_cycles: u64 = 1_000_000,
    timeout_ms: u32 = 5000,
    baudrate: u32 = 2_000_000,
    stdin: []const u8 = "",
    stdout: []const u8 = "",
    stdin_after: []const u8 = "",
    issues: []const IssueExpectation = &.{},
    pre: []const Assignment = &.{},
    post: []const Assignment = &.{},
    runs: []const Run = &.{},
    constants: []const Constant = &.{},

    /// Common patches apply first; run-local flag/Q seeds replace common seeds.
    fn forRun(list: List, allocator: std.mem.Allocator, run: Run) !List {
        var result = list;
        result.cog = run.cog orelse list.cog;
        result.stdin = run.stdin orelse list.stdin;
        result.stdout = run.stdout orelse list.stdout;
        result.stdin_after = run.stdin_after orelse list.stdin_after;
        var constants: std.ArrayList(Constant) = .empty;
        for (list.constants) |common| {
            var replaced = false;
            for (run.constants) |local| {
                if (std.mem.eql(u8, common.name, local.name)) {
                    replaced = true;
                }
            }
            if (!replaced) {
                try constants.append(allocator, common);
            }
        }
        try constants.appendSlice(allocator, run.constants);
        result.constants = constants.items;
        var pre: std.ArrayList(Assignment) = .empty;
        for (list.pre) |common| {
            var replaced = false;
            if (common.target != .sym)
                for (run.pre) |local| {
                    if (std.meta.activeTag(common.target) == std.meta.activeTag(local.target)) {
                        replaced = true;
                    }
                };
            if (!replaced) {
                try pre.append(allocator, common);
            }
        }
        try pre.appendSlice(allocator, run.pre);
        result.pre = pre.items;
        result.post = try std.mem.concat(allocator, Assignment, &.{ list.post, run.post });
        var issues: std.ArrayList(IssueExpectation) = .empty;
        for (list.issues) |common| {
            var replaced = false;
            for (run.issues) |local| {
                if (common.kind == local.kind) {
                    replaced = true;
                }
            }
            if (!replaced) {
                try issues.append(allocator, common);
            }
        }
        try issues.appendSlice(allocator, run.issues);
        result.issues = issues.items;
        result.runs = &.{};
        return result;
    }
};

const Token = struct {
    text: []const u8,
    line: u32,
};
const Parser = struct {
    allocator: std.mem.Allocator,
    path: []const u8,
    errors: *std.Io.Writer,
    tokens: []const Token,
    index: usize = 0,
    target_cog: ?u3 = null,

    /// Report a checklist error at the current token.
    fn fail(p: *Parser, message: []const u8) error{InvalidChecklist} {
        const line = if (p.index < p.tokens.len)
            p.tokens[p.index].line
        else if (p.tokens.len > 0)
            p.tokens[p.tokens.len - 1].line
        else
            1;
        p.errors.print("{s}:{d}: checklist: {s}\n", .{ p.path, line, message }) catch {};
        return error.InvalidChecklist;
    }

    /// Inspect the current token without advancing.
    fn peek(p: *Parser) []const u8 {
        return if (p.index < p.tokens.len)
            p.tokens[p.index].text
        else
            "";
    }

    /// Consume the current token or report unexpected end of input.
    fn take(p: *Parser) ![]const u8 {
        if (p.index == p.tokens.len)
            return p.fail("unexpected end of checklist");
        const result = p.tokens[p.index].text;
        p.index += 1;
        return result;
    }

    /// Consume a token that matches the expected spelling.
    fn expect(p: *Parser, expected: []const u8) !void {
        if (!std.mem.eql(u8, p.peek(), expected))
            return p.fail(expected);
        p.index += 1;
    }

    /// Skip consecutive newline tokens.
    fn newlines(p: *Parser) void {
        while (std.mem.eql(u8, p.peek(), "\n")) {
            p.index += 1;
        }
    }

    /// Parse an integer token, allowing digit separators.
    fn integer(p: *Parser, token: []const u8) !i64 {
        const clean = try p.allocator.alloc(u8, token.len);
        var len: usize = 0;
        for (token) |c| {
            if (c != '_') {
                clean[len] = c;
                len += 1;
            }
        }
        return std.fmt.parseInt(i64, clean[0..len], 0) catch return p.fail("invalid integer");
    }

    /// Consume a strictly positive integer.
    fn positive(p: *Parser) !u64 {
        const n = try p.integer(try p.take());
        if (n <= 0)
            return p.fail("expected positive integer");
        return @intCast(n);
    }

    /// Parse a numeric address or symbol reference.
    fn address(p: *Parser) !Address {
        const value = try p.take();
        if (value.len == 0 or
            std.mem.indexOfScalar(u8, "\n[]=,:\"", value[0]) != null)
            return p.fail("expected address or symbol");
        if (std.ascii.isDigit(value[0]) or value[0] == '-') {
            const n = try p.integer(value);
            if (n < 0 or n > std.math.maxInt(u32))
                return p.fail("address outside u32 range");
            return .{ .number = @intCast(n) };
        }
        return .{ .symbol = value };
    }

    /// Parse a register, hub, symbol, or flag target.
    fn target(p: *Parser) !Target {
        const kind = try p.take();
        p.target_cog = null;
        try p.expect("[");
        if (std.mem.eql(u8, kind, "sym")) {
            const a = try p.address();
            if (a != .symbol)
                return p.fail("sym requires a symbol name");
            try p.expect("]");
            return .{ .sym = a.symbol };
        }
        if (std.mem.eql(u8, kind, "hub")) {
            const a = try p.address();
            try p.expect("]");
            return .{ .hub = a };
        }
        if (!std.mem.eql(u8, kind, "cog"))
            return p.fail("unknown state target");
        const selected = try p.take();
        if (!std.mem.eql(u8, selected, "*")) {
            const cog = try p.integer(selected);
            if (cog < 0 or cog > 7)
                return p.fail("cog must be 0..7");
            p.target_cog = @intCast(cog);
        }
        try p.expect("]");
        const field = try p.take();
        if (std.mem.eql(u8, field, ".c"))
            return .c;
        if (std.mem.eql(u8, field, ".z"))
            return .z;
        if (std.mem.eql(u8, field, ".q"))
            return .q;
        if (std.mem.eql(u8, field, ".dacs"))
            return .dacs;
        if (!std.mem.eql(u8, field, ".reg"))
            return p.fail("unsupported cog field");
        try p.expect("[");
        const a = try p.address();
        try p.expect("]");
        return .{ .reg = a };
    }

    /// Encode an integer at the requested little-endian width.
    fn numberBytes(p: *Parser, width: usize, n: i64) ![]const u8 {
        const bits: u6 = @intCast(width * 8);
        const min = -(@as(i64, 1) << @intCast(bits - 1));
        const max = (@as(i64, 1) << bits) - 1;
        if (n < min or n > max)
            return p.fail("integer does not fit element width");
        const bytes = try p.allocator.alloc(u8, width);
        const raw: u64 = @bitCast(n);
        for (bytes, 0..) |*byte, i| {
            byte.* = @truncate(raw >> @intCast(8 * i));
        }
        return bytes;
    }

    /// Decode a quoted string with checklist escape sequences.
    fn string(p: *Parser) ![]const u8 {
        const token = try p.take();
        if (token.len < 2 or token[0] != '"' or token[token.len - 1] != '"')
            return p.fail("expected quoted string");
        var bytes: std.ArrayList(u8) = .empty;
        var i: usize = 1;
        while (i < token.len - 1) : (i += 1) {
            var byte = token[i];
            if (byte == '\\') {
                i += 1;
                if (i >= token.len - 1)
                    return p.fail("incomplete string escape");
                byte = switch (token[i]) {
                    '\\', '"' => token[i],
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    'x' => blk: {
                        if (i + 2 >= token.len - 1)
                            return p.fail("incomplete hex escape");
                        const value = std.fmt.parseInt(u8, token[i + 1 ..][0..2], 16) catch return p.fail("invalid hex escape");
                        i += 2;
                        break :blk value;
                    },
                    else => return p.fail("unknown string escape"),
                };
            }
            try bytes.append(p.allocator, byte);
        }
        return try bytes.toOwnedSlice(p.allocator);
    }

    /// Decode a typed numeric or hexadecimal byte block.
    fn block(p: *Parser) ![]const u8 {
        const format = try p.take();
        const width: usize = if (std.mem.eql(u8, format, "u8"))
            1
        else if (std.mem.eql(u8, format, "u16"))
            2
        else if (std.mem.eql(u8, format, "u32"))
            4
        else if (std.mem.eql(u8, format, "hex"))
            0
        else
            return p.fail("expected u8/u16/u32/hex block");
        try p.expect("[");
        var bytes: std.ArrayList(u8) = .empty;
        while (true) {
            p.newlines();
            if (std.mem.eql(u8, p.peek(), "]")) {
                p.index += 1;
                break;
            }
            const token = try p.take();
            if (std.mem.eql(u8, token, ","))
                continue;
            if (width == 0) {
                if (token.len % 2 != 0)
                    return p.fail("hex bytes require pairs of digits");
                var i: usize = 0;
                while (i < token.len) : (i += 2) {
                    try bytes.append(p.allocator, std.fmt.parseInt(u8, token[i..][0..2], 16) catch return p.fail("invalid hex byte"));
                }
            } else {
                try bytes.appendSlice(p.allocator, try p.numberBytes(width, try p.integer(token)));
            }
            if (bytes.items.len > 1 << 20)
                return p.fail("block exceeds 1 MiB");
        }
        return try bytes.toOwnedSlice(p.allocator);
    }

    /// Parse a string or typed block as a byte stream.
    fn stream(p: *Parser) ![]const u8 {
        return if (std.mem.startsWith(u8, p.peek(), "\""))
            p.string()
        else
            p.block();
    }
};

/// Identify fixture sources without treating imported assembly helpers as standalone tests.
pub fn hasHeader(source: []const u8) bool {
    var lines = std.mem.splitScalar(u8, source, '\n');
    return std.mem.eql(u8, std.mem.trim(u8, lines.next() orelse "", " \t\r"), "//? WINDTUNNEL CHECK LIST");
}

/// One materialized run, ready for either simulator or oracle assembly.
pub const PreparedRun = struct {
    name: []const u8,
    source: []const u8,
    list: List,
};

const Macro = struct {
    name: []const u8,
    action: Action,
    text: []const u8,
    line: usize,

    const Action = enum { set, default, required, override };
};
const Section = struct {
    first: usize,
    macros: std.ArrayList(Macro) = .empty,
};

/// Report preprocessing failures against the original fixture and selected run.
fn preparationError(errors: *std.Io.Writer, path: []const u8, run: usize, line: usize, message: []const u8) error{InvalidChecklist} {
    errors.print("{s}:{d}: checklist [run {d}]: {s}\n", .{ path, line, run, message }) catch {};
    return error.InvalidChecklist;
}

/// Macro names are simple identifiers in a namespace separate from assembler constants.
fn macroName(name: []const u8) bool {
    if (name.len == 0 or !(std.ascii.isAlphabetic(name[0]) or name[0] == '_'))
        return false;
    for (name) |byte| {
        if (!(std.ascii.isAlphanumeric(byte) or byte == '_'))
            return false;
    }
    return true;
}

/// Expand a single line once, preferring the selected run's definitions over common ones.
fn expandLine(
    writer: *std.Io.Writer,
    text: []const u8,
    common: []const Macro,
    local: []const Macro,
    errors: *std.Io.Writer,
    path: []const u8,
    run: usize,
    line: usize,
) !void {
    var position: usize = 0;
    while (position < text.len) {
        const open = std.mem.findPos(u8, text, position, "{{") orelse text.len;
        const stray = std.mem.findPos(u8, text, position, "}}");
        if (stray != null and stray.? < open)
            return preparationError(errors, path, run, line, "unmatched macro closing braces");
        try writer.writeAll(text[position..open]);
        if (open == text.len)
            break;
        const close = std.mem.findPos(u8, text, open + 2, "}}") orelse
            return preparationError(errors, path, run, line, "unterminated macro placeholder");
        const name = std.mem.trim(u8, text[open + 2 .. close], " \t");
        if (!macroName(name))
            return preparationError(errors, path, run, line, "invalid macro placeholder name");
        const replacement = find: {
            for (local) |macro| {
                if (std.mem.eql(u8, name, macro.name))
                    break :find macro.text;
            }
            for (common) |macro| {
                if (std.mem.eql(u8, name, macro.name))
                    break :find macro.text;
            }
            try errors.print("{s}:{d}: checklist [run {d}]: undefined macro '{s}'\n", .{ path, line, run, name });
            return error.InvalidChecklist;
        };
        try writer.writeAll(replacement);
        position = close + 2;
    }
}

/// Collect physical sections, then materialize each run; all returned storage belongs to the caller's arena.
pub fn prepare(allocator: std.mem.Allocator, path: []const u8, source: []const u8, errors: *std.Io.Writer) ![]const PreparedRun {
    if (!hasHeader(source))
        return preparationError(errors, path, 0, 1, "missing WINDTUNNEL CHECK LIST header");
    var lines: std.ArrayList([]const u8) = .empty;
    var iterator = std.mem.splitScalar(u8, source, '\n');
    while (iterator.next()) |line| {
        try lines.append(allocator, line);
    }
    var sections: std.ArrayList(Section) = .empty;
    try sections.append(allocator, .{ .first = 1 });
    var checklist_end: usize = lines.items.len;
    var previous_empty = false;
    for (lines.items[1..], 1..) |raw, index| {
        const comment = std.mem.trimStart(u8, raw, " \t");
        if (!std.mem.startsWith(u8, comment, "//?")) {
            checklist_end = index;
            break;
        }
        const content = std.mem.trim(u8, comment[3..], " \t\r");
        defer previous_empty = content.len == 0;
        const colon = std.mem.indexOfScalar(u8, content, ':') orelse continue;
        const directive = std.mem.trimEnd(u8, content[0..colon], " \t");
        if (std.mem.find(u8, directive, "{{") != null or std.mem.find(u8, directive, "}}") != null)
            return preparationError(errors, path, sections.items.len - 1, index + 1, "directive names must be literal");
        if (std.mem.eql(u8, directive, "run")) {
            if (!previous_empty)
                return preparationError(errors, path, sections.items.len, index + 1, "run: must follow an empty //? line");
            try sections.append(allocator, .{ .first = index });
        } else if (std.mem.eql(u8, directive, "macro")) {
            const definition = std.mem.trimStart(u8, content[colon + 1 ..], " \t");
            const end = std.mem.indexOfAny(u8, definition, " \t") orelse definition.len;
            const name = definition[0..end];
            const tail = std.mem.trimStart(u8, definition[end..], " \t");
            const action_end = std.mem.indexOfAny(u8, tail, " \t") orelse tail.len;
            const action = std.meta.stringToEnum(Macro.Action, tail[0..action_end]) orelse
                return preparationError(errors, path, sections.items.len - 1, index + 1, "expected macro action: set, default, required, or override");
            const body = std.mem.trimStart(u8, tail[action_end..], " \t");
            if (!macroName(name))
                return preparationError(errors, path, sections.items.len - 1, index + 1, "invalid macro name");
            if (std.mem.find(u8, body, "{{") != null or std.mem.find(u8, body, "}}") != null)
                return preparationError(errors, path, sections.items.len - 1, index + 1, "macro bodies cannot contain macro references");
            const global = sections.items.len == 1;
            switch (action) {
                .set => {},
                .default, .required => {
                    if (!global)
                        return preparationError(errors, path, sections.items.len - 1, index + 1, "default and required are global macro declarations");
                },
                .override => {
                    if (global)
                        return preparationError(errors, path, 0, index + 1, "override is only allowed inside a run");
                },
            }
            if (action == .required and body.len != 0)
                return preparationError(errors, path, 0, index + 1, "required does not accept a macro value");
            const section = &sections.items[sections.items.len - 1];
            for (section.macros.items) |old| {
                if (std.mem.eql(u8, old.name, name))
                    return preparationError(errors, path, sections.items.len - 1, index + 1, "duplicate macro in section");
            }
            try section.macros.append(allocator, .{
                .name = name,
                .action = action,
                .text = body,
                .line = index + 1,
            });
        }
    }
    const common = sections.items[0].macros.items;
    for (sections.items[1..], 1..) |section, run| {
        for (section.macros.items) |local| {
            const declaration = find: {
                for (common) |global| {
                    if (std.mem.eql(u8, global.name, local.name))
                        break :find global;
                }
                return preparationError(errors, path, run, local.line, "run macro requires a global declaration");
            };
            const message: ?[]const u8 = switch (declaration.action) {
                .set => "a globally set macro cannot be assigned inside a run",
                .default => if (local.action != .override)
                    "a default macro requires override inside a run"
                else
                    null,
                .required => if (local.action != .set)
                    "a required macro requires set inside a run"
                else
                    null,
                .override => unreachable,
            };
            if (message) |text|
                return preparationError(errors, path, run, local.line, text);
        }
    }
    const count = @max(1, sections.items.len - 1);
    const prepared = try allocator.alloc(PreparedRun, count);
    var explicit_names: std.StringHashMap(void) = .init(allocator);
    for (prepared, 0..) |*result, run_index| {
        const selected = if (sections.items.len == 1)
            0
        else
            run_index + 1;
        for (common) |declaration| {
            if (declaration.action != .required)
                continue;
            const assigned = find: {
                if (selected != 0) {
                    for (sections.items[selected].macros.items) |local| {
                        if (std.mem.eql(u8, declaration.name, local.name))
                            break :find true;
                    }
                }
                break :find false;
            };
            if (!assigned) {
                const message = try std.fmt.allocPrint(
                    allocator,
                    "required macro '{s}' must be set in every run",
                    .{declaration.name},
                );
                return preparationError(errors, path, run_index + 1, sections.items[selected].first + 1, message);
            }
        }
        var expanded: std.Io.Writer.Allocating = .init(allocator);
        var section_index: usize = 0;
        for (lines.items, 0..) |raw, index| {
            if (section_index + 1 < sections.items.len and index == sections.items[section_index + 1].first) {
                section_index += 1;
            }
            const in_checklist = index > 0 and index < checklist_end;
            const content = if (in_checklist)
                std.mem.trim(u8, std.mem.trimStart(u8, raw, " \t")[3..], " \t\r")
            else
                "";
            const colon = std.mem.indexOfScalar(u8, content, ':');
            const is_macro = colon != null and std.mem.eql(u8, std.mem.trimEnd(u8, content[0..colon.?], " \t"), "macro");
            if (in_checklist and (is_macro or (section_index != 0 and section_index != selected))) {
                try expanded.writer.writeAll("//?");
            } else {
                try expandLine(
                    &expanded.writer,
                    raw,
                    sections.items[0].macros.items,
                    sections.items[selected].macros.items,
                    errors,
                    path,
                    run_index + 1,
                    index + 1,
                );
            }
            if (index + 1 < lines.items.len) {
                try expanded.writer.writeByte('\n');
            }
        }
        const text = try expanded.toOwnedSlice();
        const list = try parse(allocator, path, text, errors);
        const run = list.runs[0];
        const name = run.name orelse try std.fmt.allocPrint(allocator, "run {d}", .{run_index + 1});
        if (run.name != null) {
            const entry = try explicit_names.getOrPut(name);
            if (entry.found_existing)
                return preparationError(errors, path, run_index + 1, sections.items[selected].first + 1, "duplicate run name");
        }
        result.* = .{ .name = name, .source = text, .list = try list.forRun(allocator, run) };
    }
    return prepared;
}

/// Allocations belong to the caller's arena, including token and value storage.
fn parse(allocator: std.mem.Allocator, path: []const u8, source: []const u8, errors: *std.Io.Writer) !List {
    if (!hasHeader(source)) {
        try errors.print("{s}:1: checklist: missing WINDTUNNEL CHECK LIST header\n", .{path});
        return error.InvalidChecklist;
    }
    var lines = std.mem.splitScalar(u8, source, '\n');
    _ = lines.next();
    var tokens: std.ArrayList(Token) = .empty;
    var line_number: u32 = 1;
    var previous_empty = false;
    while (lines.next()) |raw| {
        line_number += 1;
        const comment = std.mem.trimStart(u8, raw, " \t");
        if (!std.mem.startsWith(u8, comment, "//?"))
            break;
        const line = std.mem.trimEnd(u8, comment[3..], "\r");
        const content = std.mem.trim(u8, line, " \t");
        if (std.mem.startsWith(u8, content, "run:") and !previous_empty)
            return preparationError(errors, path, 0, line_number, "run: must follow an empty //? line");
        previous_empty = content.len == 0;
        var i: usize = 0;
        while (i < line.len) {
            if (std.ascii.isWhitespace(line[i])) {
                i += 1;
                continue;
            }
            const start = i;
            if (line[i] == '"') {
                i += 1;
                var closed = false;
                while (i < line.len) : (i += 1) {
                    if (line[i] == '\\') {
                        i += 1;
                        continue;
                    }
                    if (line[i] == '"') {
                        i += 1;
                        closed = true;
                        break;
                    }
                }
                if (!closed) {
                    try errors.print("{s}:{d}: checklist: unterminated string\n", .{ path, line_number });
                    return error.InvalidChecklist;
                }
            } else if (std.mem.indexOfScalar(u8, "[]=:,", line[i]) != null) {
                i += 1;
                if (line[start] == '=' and i < line.len and line[i] == '=') {
                    i += 1;
                }
            } else {
                while (i < line.len and
                    !std.ascii.isWhitespace(line[i]) and
                    std.mem.indexOfScalar(u8, "[]=:,\"", line[i]) == null)
                {
                    i += 1;
                }
            }
            try tokens.append(allocator, .{ .text = line[start..i], .line = line_number });
        }
        try tokens.append(allocator, .{ .text = "\n", .line = line_number });
    }
    var p: Parser = .{ .allocator = allocator, .path = path, .errors = errors, .tokens = tokens.items };
    var list: List = .{};
    var pre: std.ArrayList(Assignment) = .empty;
    var post: std.ArrayList(Assignment) = .empty;
    var issues: std.ArrayList(IssueExpectation) = .empty;
    var runs: std.ArrayList(Run) = .empty;
    var constants: std.ArrayList(Constant) = .empty;
    var seen: std.StringHashMap(void) = .init(allocator);
    while (true) {
        p.newlines();
        if (p.index == p.tokens.len) {
            break;
        }
        const line = p.tokens[p.index].line;
        const name = try p.take();
        try p.expect(":");
        if (std.mem.eql(u8, name, "run")) {
            const run_name = if (std.mem.eql(u8, p.peek(), "\n"))
                null
            else
                try p.string();
            if (run_name) |value| {
                if (value.len == 0)
                    return p.fail("run name must not be empty");
                for (runs.items) |old| {
                    if (old.name) |old_name| {
                        if (std.mem.eql(u8, old_name, value))
                            return p.fail("duplicate run name");
                    }
                }
            }
            if (runs.items.len == 0) {
                list.pre = pre.items;
                list.post = post.items;
                list.constants = constants.items;
                list.issues = issues.items;
            } else {
                runs.items[runs.items.len - 1].pre = pre.items;
                runs.items[runs.items.len - 1].post = post.items;
                runs.items[runs.items.len - 1].constants = constants.items;
                runs.items[runs.items.len - 1].issues = issues.items;
            }
            constants = .empty;
            issues = .empty;
            pre = .empty;
            post = .empty;
            try runs.append(allocator, .{ .name = run_name });
        } else if (std.mem.eql(u8, name, "issues")) {
            const kind = std.meta.stringToEnum(IssueKind, try p.take()) orelse return p.fail("unknown issue kind");
            try p.expect("=");
            const count = try p.integer(try p.take());
            if (count < 0)
                return p.fail("issue count must be nonnegative");
            for (issues.items) |old| {
                if (old.kind == kind)
                    return p.fail("duplicate issue expectation");
            }
            try issues.append(allocator, .{ .kind = kind, .count = @intCast(count) });
        } else if (std.mem.eql(u8, name, "const")) {
            const symbol = try p.take();
            if (symbol.len == 0 or
                !(std.ascii.isAlphabetic(symbol[0]) or
                    symbol[0] == '_') or
                std.mem.startsWith(u8, symbol, "_wt_"))
                return p.fail("invalid constant name");
            for (symbol) |c| {
                if (!(std.ascii.isAlphanumeric(c) or c == '_'))
                    return p.fail("invalid constant name");
            }
            for (constants.items) |old| {
                if (std.mem.eql(u8, old.name, symbol))
                    return p.fail("duplicate constant");
            }
            try p.expect("=");
            try constants.append(allocator, .{ .name = symbol, .value = try p.integer(try p.take()) });
        } else if (runs.items.len != 0 and
            (std.mem.eql(u8, name, "stdin") or
                std.mem.eql(u8, name, "stdout") or
                std.mem.eql(u8, name, "stdin-after")))
        {
            const run = &runs.items[runs.items.len - 1];
            const stream = if (std.mem.eql(u8, name, "stdin"))
                &run.stdin
            else if (std.mem.eql(u8, name, "stdout"))
                &run.stdout
            else
                &run.stdin_after;
            if (stream.* != null)
                return p.fail("duplicate run stream");
            stream.* = try p.stream();
        } else if (std.mem.eql(u8, name, "cog") and runs.items.len != 0) {
            const run = &runs.items[runs.items.len - 1];
            if (run.cog != null)
                return p.fail("duplicate run cog");
            const n = try p.integer(try p.take());
            if (n < 0 or n > 7)
                return p.fail("cog must be 0..7");
            run.cog = @intCast(n);
        } else if (std.mem.eql(u8, name, "pre") or std.mem.eql(u8, name, "post")) {
            const target = try p.target();
            const is_pre = std.mem.eql(u8, name, "pre");
            if (is_pre and
                (target == .dacs or target == .reg or
                    target == .hub))
                return p.fail("preconditions support only sym, C, Z and Q");
            if (!is_pre and target == .sym)
                return p.fail("sym is only supported in preconditions");
            try p.expect(if (is_pre)
                "="
            else
                "==");
            const bytes = switch (target) {
                .c, .z => blk: {
                    const value = try p.take();
                    if (std.mem.eql(u8, value, "true"))
                        break :blk &[_]u8{1};
                    if (std.mem.eql(u8, value, "false"))
                        break :blk &[_]u8{0};
                    return p.fail("expected true or false");
                },
                .reg, .q => try p.numberBytes(4, try p.integer(try p.take())),
                .dacs => blk: {
                    if (!std.mem.eql(u8, p.peek(), "u8"))
                        return p.fail("DAC state requires exactly four u8 values");
                    const values = try p.block();
                    if (values.len != 4)
                        return p.fail("DAC state requires exactly four u8 values");
                    break :blk values;
                },
                .hub, .sym => try p.block(),
            };
            const assignment: Assignment = .{ .line = line, .cog = p.target_cog, .target = target, .bytes = bytes };
            try (if (is_pre)
                &pre
            else
                &post).append(allocator, assignment);
        } else {
            if (runs.items.len != 0)
                return p.fail("configuration directives must precede run sections");
            const entry = try seen.getOrPut(name);
            if (entry.found_existing)
                return p.fail("duplicate directive");
            if (std.mem.eql(u8, name, "profile")) {
                list.profile = std.meta.stringToEnum(Profile, try p.take()) orelse return p.fail("unknown profile");
            } else if (std.mem.eql(u8, name, "cog")) {
                const n = try p.integer(try p.take());
                if (n < 0 or n > 7)
                    return p.fail("cog must be 0..7");
                list.cog = @intCast(n);
            } else if (std.mem.eql(u8, name, "entry")) {
                list.entry = try p.take();
            } else if (std.mem.eql(u8, name, "stop")) {
                list.stop = try p.take();
            } else if (std.mem.eql(u8, name, "max-cycles")) {
                list.max_cycles = try p.positive();
            } else if (std.mem.eql(u8, name, "timeout-ms")) {
                const n = try p.positive();
                if (n < 100 or n > 10000)
                    return p.fail("timeout-ms must be 100..10000");
                list.timeout_ms = @intCast(n);
            } else if (std.mem.eql(u8, name, "baudrate")) {
                const n = try p.positive();
                if (n > std.math.maxInt(i32))
                    return p.fail("baudrate too large");
                list.baudrate = @intCast(n);
            } else if (std.mem.eql(u8, name, "fast-io")) {
                try p.expect("off");
                list.fast_io_off = try p.string();
                if (list.fast_io_off.?.len == 0)
                    return p.fail("fast-io off requires a reason");
            } else if (std.mem.eql(u8, name, "oracle")) {
                const value = try p.take();
                if (std.mem.eql(u8, value, "off")) {
                    list.oracle_off = try p.string();
                    if (list.oracle_off.?.len == 0)
                        return p.fail("oracle off requires a reason");
                } else if (!std.mem.eql(u8, value, "required"))
                    return p.fail("expected required or off");
            } else if (std.mem.eql(u8, name, "stdin")) {
                list.stdin = try p.stream();
            } else if (std.mem.eql(u8, name, "stdout")) {
                list.stdout = try p.stream();
            } else if (std.mem.eql(u8, name, "stdin-after")) {
                list.stdin_after = try p.stream();
            } else {
                return p.fail("unknown directive");
            }
        }
        if (p.index < p.tokens.len) {
            try p.expect("\n");
        }
    }
    if (runs.items.len == 0) {
        list.pre = pre.items;
        list.post = post.items;
        list.constants = constants.items;
        list.issues = issues.items;
        try runs.append(allocator, .{});
    } else {
        runs.items[runs.items.len - 1].pre = pre.items;
        runs.items[runs.items.len - 1].post = post.items;
        runs.items[runs.items.len - 1].constants = constants.items;
        runs.items[runs.items.len - 1].issues = issues.items;
    }
    list.runs = runs.items;
    for (list.runs) |run| {
        const stdin = run.stdin orelse list.stdin;
        const stdout = run.stdout orelse list.stdout;
        const stdin_after = run.stdin_after orelse list.stdin_after;
        if (list.profile == .cog and
            (stdin.len != 0 or
                stdout.len != 0 or
                stdin_after.len != 0))
            return p.fail("cog profile reserves serial I/O for the reporter");
        if (stdin.len > 0 and stdin_after.len == 0)
            return p.fail("nonempty stdin requires stdin-after readiness");
        if (stdin_after.len > 0 and
            !std.mem.startsWith(u8, stdout, stdin_after))
            return p.fail("stdin-after must be a prefix of expected stdout");
        const selected_cog = run.cog orelse list.cog;
        for ([_][]const Assignment{ list.pre, list.post, run.pre, run.post }) |assignments| {
            for (assignments) |a| {
                if (a.target == .dacs and list.oracle_off == null)
                    return p.fail("DAC state is local-only; specify oracle: off with a reason");
                if (a.cog) |cog| {
                    if (cog != selected_cog)
                        return p.fail("state target must match selected cog");
                }
            }
        }
        if (list.profile == .program) {
            if (selected_cog != 0 or
                list.entry != null or
                list.post.len != 0 or
                run.post.len != 0)
                return p.fail("program profile does not support entry or state assignments");
            for ([_][]const Assignment{ list.pre, run.pre }) |assignments| {
                for (assignments) |a| {
                    if (a.target != .sym)
                        return p.fail("program profile supports only symbol patches as preconditions");
                }
            }
        }
    }
    return list;
}

test "checklist values, CRLF, multiline blocks, and invalid directives" {
    var arena: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer arena.deinit();

    var errors: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer errors.deinit();

    const valid = try parse(arena.allocator(), "fixture", "//? WINDTUNNEL CHECK LIST\r\n//? pre: cog[0].q = -1\r\n//? post: hub[0x2000] == u16 [\r\n//? 0x1234, -1\r\n//? ]\r\n\n", &errors.writer);
    try std.testing.expectEqualSlices(u8, &.{ 0xff, 0xff, 0xff, 0xff }, valid.pre[0].bytes);
    try std.testing.expectEqualSlices(u8, &.{ 0x34, 0x12, 0xff, 0xff }, valid.post[0].bytes);
    // The native Propan formatter aligns these comments with the instruction column.
    const indented = try parse(arena.allocator(), "formatted", "                //? WINDTUNNEL CHECK LIST\r\n\t//? pre: cog[0].q = -1\r\n                //? post: hub[0x2000] == u16 [\r\n\t//? 0x1234, -1\r\n                //? ]\r\n\n", &errors.writer);
    try std.testing.expectEqualSlices(u8, valid.pre[0].bytes, indented.pre[0].bytes);
    try std.testing.expectEqualSlices(u8, valid.post[0].bytes, indented.post[0].bytes);
    const clocked = try parse(arena.allocator(), "clocked", "//? WINDTUNNEL CHECK LIST\n//? fast-io: off \"frame timing\"\n", &errors.writer);
    try std.testing.expectEqualStrings("frame timing", clocked.fast_io_off.?);
    try std.testing.expect(valid.fast_io_off == null);
    for ([_][]const u8{
        "//? typo: 1\n",                            "//? max-cycles: 0\n",                                                                  "//? timeout-ms: 99\n",                 "//? pre: cog[1].c = true\n",
        "//? pre: cog[0].reg[x] = 0x1_0000_0000\n", "//? post: hub[0] == u8 [256]\n",                                                       "//? post: hub[0] == hex [f]\n",        "//? stdout: \"\\q\"\n",
        "//? profile: program\n//? stdin: \"x\"\n", "//? oracle: off \"\"\n",                                                               "//? profile: cog\n//? profile: cog\n", "//? max-cycles: 1 extra\n",
        "//? fast-io: off \"\"\n",                  "//? fast-io: on\n",                                                                    "//? pre: cog[0].reg[value] = 1\n",     "//? pre: hub[buffer] = u8 [1]\n",
        "//? output: u32 [1]\n",                    "//? profile: program\n//? oracle: off \"removed output hook\"\n//? output: u32 [1]\n",
    }) |body| {
        const text = try std.fmt.allocPrint(arena.allocator(), "//? WINDTUNNEL CHECK LIST\n{s}", .{body});
        try std.testing.expectError(error.InvalidChecklist, parse(arena.allocator(), "fixture", text, &errors.writer));
    }
    try std.testing.expectError(error.InvalidChecklist, parse(arena.allocator(), "fixture", "MOV PA, 0\n", &errors.writer));
    const selected = try parse(arena.allocator(), "fixture", "//? WINDTUNNEL CHECK LIST\n//? pre: cog[7].q = 0x8000_0001\n//? cog: 7\n", &errors.writer);
    try std.testing.expectEqual(@as(u3, 7), selected.cog);
    try std.testing.expectEqualSlices(u8, &.{ 1, 0, 0, 0x80 }, selected.pre[0].bytes);
    const binary = try parse(arena.allocator(), "fixture", "//? WINDTUNNEL CHECK LIST\n//? profile: program\n//? stdin: hex [0080ff]\n//? stdin-after: \"READY\\n\"\n//? stdout: \"READY\\n\\x00\\x80\\xff\"\n", &errors.writer);
    try std.testing.expectEqualSlices(u8, &.{ 0, 128, 255 }, binary.stdin);
    try std.testing.expectEqualSlices(u8, "READY\n" ++ "\x00\x80\xff", binary.stdout);
}

test "run sections inherit common conditions and preserve local seeds" {
    var arena: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer arena.deinit();

    var errors: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer errors.deinit();

    const allocator = arena.allocator();
    const list = try parse(allocator, "fixture",
        \\//? WINDTUNNEL CHECK LIST
        \\//? pre: cog[0].c = true
        \\//? pre: sym[buffer] = u16 [ 0x1234, -1 ]
        \\//? post: cog[0].z == false
        \\//?
        \\//? run:
        \\//? pre: cog[0].c = false
        \\//? post: cog[0].reg[value] == 1
        \\//?
        \\//? run: "a named run"
        \\//? pre: sym[buffer] = hex [ abcd ]
    , &errors.writer);
    try std.testing.expectEqual(@as(usize, 2), list.runs.len);
    try std.testing.expect(list.runs[0].name == null);
    try std.testing.expectEqualStrings("a named run", list.runs[1].name.?);
    const first = try list.forRun(allocator, list.runs[0]);
    try std.testing.expectEqual(@as(usize, 2), first.pre.len);
    try std.testing.expectEqualSlices(u8, &.{ 0x34, 0x12, 0xff, 0xff }, first.pre[0].bytes);
    try std.testing.expectEqualSlices(u8, &.{0}, first.pre[1].bytes);
    try std.testing.expectEqual(@as(usize, 2), first.post.len);
    const second = try list.forRun(allocator, list.runs[1]);
    try std.testing.expectEqual(@as(usize, 3), second.pre.len);
    try std.testing.expectEqualSlices(u8, &.{1}, second.pre[0].bytes);
    try std.testing.expectEqualSlices(u8, &.{ 0xab, 0xcd }, second.pre[2].bytes);
    try std.testing.expectEqual(@as(usize, 1), second.post.len);
    const implicit = try parse(allocator, "fixture", "//? WINDTUNNEL CHECK LIST\n//? pre: sym[value] = u32 [1]\n", &errors.writer);
    try std.testing.expectEqual(@as(usize, 1), implicit.runs.len);
    try std.testing.expectEqual(@as(usize, 1), (try implicit.forRun(allocator, implicit.runs[0])).pre.len);
    for ([_][]const u8{
        "//?\n//? run: unquoted\n",
        "//?\n//? run: \"\"\n",
        "//?\n//? run: \"same\"\n//?\n//? run: \"same\"\n",
        "//?\n//? run:\n//? timeout-ms: 1000\n",
        "//?\n//? run:\n//? post: cog[1].c == true\n",
        "//? post: sym[value] == u32 [1]\n",
        "//? pre: sym[42] = u32 [1]\n",
        "//? profile: program\n//?\n//? run:\n//? pre: cog[0].c = true\n",
    }) |body| {
        const source = try std.fmt.allocPrint(allocator, "//? WINDTUNNEL CHECK LIST\n{s}", .{body});
        try std.testing.expectError(error.InvalidChecklist, parse(allocator, "fixture", source, &errors.writer));
    }
}

test "run constants and cog selection inherit without leaking across runs" {
    var arena: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer arena.deinit();

    var errors: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer errors.deinit();

    const allocator = arena.allocator();
    const list = try parse(
        allocator,
        "fixture",
        "//? WINDTUNNEL CHECK LIST\n" ++
            "//? const: width = 4\n" ++
            "//? post: cog[*].reg[result] == 0\n" ++
            "//?\n//? run: \"override\"\n" ++
            "//? cog: 7\n" ++
            "//? const: width = 1\n" ++
            "//?\n//? run: \"restored\"\n",
        &errors.writer,
    );
    const first = try list.forRun(allocator, list.runs[0]);
    const second = try list.forRun(allocator, list.runs[1]);
    try std.testing.expectEqual(@as(u3, 7), first.cog);
    try std.testing.expectEqual(@as(i64, 1), first.constants[0].value);
    try std.testing.expectEqual(@as(u3, 0), second.cog);
    try std.testing.expectEqual(@as(i64, 4), second.constants[0].value);
    for ([_][]const u8{
        "//? const: x = 1\n//? const: x = 2\n",
        "//? const: _wt_target_cog = 2\n",
        "//? const: x-y = 2\n",
        "//?\n//? run:\n//? cog: 8\n",
        "//?\n//? run:\n//? cog: 2\n//? cog: 3\n",
        "//?\n//? run:\n//? cog: 7\n//? post: cog[0].c == true\n",
    }) |body| {
        const source = try std.fmt.allocPrint(allocator, "//? WINDTUNNEL CHECK LIST\n{s}", .{body});
        try std.testing.expectError(error.InvalidChecklist, parse(allocator, "fixture", source, &errors.writer));
    }
}

test "program matrix streams override common values, including an empty stream" {
    var arena: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer arena.deinit();

    var errors: std.Io.Writer.Allocating = .init(arena.allocator());
    const source =
        "//? WINDTUNNEL CHECK LIST\n//? profile: program\n//? stdin: \"common\"\n//? stdout: \"ready\"\n" ++
        "//? stdin-after: \"ready\"\n//?\n//? run: \"first\"\n//? stdin: \"\"\n//? stdout: \"ready first\"\n" ++
        "//?\n//? run: \"second\"\n//? stdin-after: \"r\"\n";
    const list = try parse(arena.allocator(), "matrix", source, &errors.writer);
    const first = try list.forRun(arena.allocator(), list.runs[0]);
    const second = try list.forRun(arena.allocator(), list.runs[1]);
    try std.testing.expectEqualStrings("", first.stdin);
    try std.testing.expectEqualStrings("ready first", first.stdout);
    try std.testing.expectEqualStrings("ready", first.stdin_after);
    try std.testing.expectEqualStrings("common", second.stdin);
    try std.testing.expectEqualStrings("ready", second.stdout);
    try std.testing.expectEqualStrings("r", second.stdin_after);
    try std.testing.expectError(error.InvalidChecklist, parse(arena.allocator(), "matrix", source ++ "//? stdin-after: \"duplicate\"\n", &errors.writer));
    try std.testing.expectError(error.InvalidChecklist, parse(arena.allocator(), "matrix", "//? WINDTUNNEL CHECK LIST\n//?\n//? run:\n//? stdin: \"x\"\n", &errors.writer));
}

test "macros materialize common values and forward run overrides without leaking" {
    var arena: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer arena.deinit();

    var errors: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer errors.deinit();

    const source =
        "\t//? WINDTUNNEL CHECK LIST\r\n" ++
        "\t//? macro: operation required\r\n" ++
        "\t//? macro: expected default 7\r\n" ++
        "\t//? macro: optional default\r\n" ++
        "\t//? macro: label default base\r\n" ++
        "\t//? macro: fixed set 23\r\n" ++
        "\t//? post: cog[*].reg[result] == {{ expected }}\r\n" ++
        "\t//? \r\n" ++
        "\t//? run: \"{{ label }}\"\r\n" ++
        "\t//? macro: label override override\r\n" ++
        "\t//? macro: operation set ADD result, 8\r\n" ++
        "\t//? macro: expected override 8\r\n" ++
        "\t//?\r\n" ++
        "\t//? run:\r\n" ++
        "\t//? macro: operation set MOV result, 7\r\n" ++
        "\r\n{{operation}}\r\n{{ optional }}\r\nvar fixed_value: LONG {{fixed}}\r\n";
    const runs = try prepare(arena.allocator(), "macros", source, &errors.writer);
    try std.testing.expectEqual(@as(usize, 2), runs.len);
    try std.testing.expectEqualStrings("override", runs[0].name);
    try std.testing.expectEqualStrings("run 2", runs[1].name);
    try std.testing.expectEqualSlices(u8, &.{ 8, 0, 0, 0 }, runs[0].list.post[0].bytes);
    try std.testing.expectEqualSlices(u8, &.{ 7, 0, 0, 0 }, runs[1].list.post[0].bytes);
    try std.testing.expectEqual(@as(u32, 7), runs[0].list.post[0].line);
    try std.testing.expect(std.mem.find(u8, runs[0].source, "ADD result, 8\r\n\r\n") != null);
    try std.testing.expect(std.mem.find(u8, runs[1].source, "MOV result, 7\r\n\r\n") != null);
    try std.testing.expectEqual(std.mem.count(u8, source, "\n"), std.mem.count(u8, runs[0].source, "\n"));
    const implicit = try prepare(arena.allocator(), "implicit", "//? WINDTUNNEL CHECK LIST\n//? macro: value set 23\n//? post: cog[0].reg[result] == {{value}}\n\nMOV result, {{value}}\n", &errors.writer);
    try std.testing.expectEqual(@as(usize, 1), implicit.len);
    try std.testing.expectEqualStrings("run 1", implicit[0].name);
    try std.testing.expect(std.mem.find(u8, implicit[0].source, "MOV result, 23") != null);
}

test "macro definitions and placeholders reject invalid input with original line diagnostics" {
    var arena: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer arena.deinit();

    var errors: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer errors.deinit();

    for ([_][]const u8{
        "//? macro: value set 1\n//? macro: value set 2\n",
        "//? macro: 2bad set MOV result, 1\n",
        "//? macro: bad-name set MOV result, 1\n",
        "//? macro:\n",
        "//? macro: outer set {{inner}}\n",
        "//? macro: outer set }}\n",
        "//? post: cog[0].q == {{missing}}\n",
        "//? {{directive}}: 1\n",
        "//?\n//? run: \"{{missing}}\"\n",
        "//? macro: name set same\n//?\n//? run: \"{{name}}\"\n//?\n//? run: \"same\"\n",
        "//?\n//? run:\n//? macro: value set 1\n//? macro: value set 2\n",
        "\nMOV result, {{missing}}\n",
        "\nMOV result, {{}}\n",
        "\nMOV result, {{bad name}}\n",
        "\nMOV result, {{unfinished\n",
        "\nMOV result, unmatched}}\n",
    }) |body| {
        errors.clearRetainingCapacity();
        const source = try std.mem.concat(arena.allocator(), u8, &.{ "//? WINDTUNNEL CHECK LIST\n", body });
        try std.testing.expectError(error.InvalidChecklist, prepare(arena.allocator(), "bad.propan", source, &errors.writer));
        try std.testing.expect(std.mem.startsWith(u8, errors.written(), "bad.propan:"));
        try std.testing.expect(std.mem.find(u8, errors.written(), "run") != null);
    }
    errors.clearRetainingCapacity();
    try std.testing.expectError(error.InvalidChecklist, prepare(arena.allocator(), "location.propan", "//? WINDTUNNEL CHECK LIST\n//?\n//? run: \"broken\"\n\n{{missing}}\n", &errors.writer));
    try std.testing.expect(std.mem.startsWith(u8, errors.written(), "location.propan:5:"));
}

test "run spacing requires a physically preceding empty checklist comment" {
    var arena: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer arena.deinit();

    var errors: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer errors.deinit();

    for ([_][]const u8{
        "//? WINDTUNNEL CHECK LIST\n//? run:\n",
        "//? WINDTUNNEL CHECK LIST\n//? post: cog[0].q == 0\n//? run:\n",
        "//? WINDTUNNEL CHECK LIST\n//?\n//? run:\n//? run:\n",
    }) |source| {
        try std.testing.expectError(error.InvalidChecklist, prepare(arena.allocator(), "spacing", source, &errors.writer));
        try std.testing.expectError(error.InvalidChecklist, parse(arena.allocator(), "spacing", source, &errors.writer));
    }
    const source = "//? WINDTUNNEL CHECK LIST\n//?\n//?\n//? run:\n\n//? run:\n";
    const runs = try prepare(arena.allocator(), "end", source, &errors.writer);
    try std.testing.expectEqual(@as(usize, 1), runs.len);
}

test "macro policies enforce scope, declaration, and per-run assignment contracts" {
    var arena: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer arena.deinit();

    var errors: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer errors.deinit();

    const header = "//? WINDTUNNEL CHECK LIST\n";
    const run = "//?\n//? run: \"first\"\n";
    const Invalid = struct { body: []const u8, message: []const u8 };
    for ([_]Invalid{
        .{ .body = "//? macro: value MOV result, 1\n", .message = "expected macro action" },
        .{ .body = "//? macro: value\n", .message = "expected macro action" },
        .{ .body = "//? macro: value override 1\n", .message = "only allowed inside a run" },
        .{ .body = run ++ "//? macro: value default 1\n", .message = "global macro declarations" },
        .{ .body = run ++ "//? macro: value required\n", .message = "global macro declarations" },
        .{ .body = run ++ "//? macro: value set 1\n", .message = "requires a global declaration" },
        .{ .body = run ++ "//? macro: value override 1\n", .message = "requires a global declaration" },
        .{ .body = "//? macro: value required 1\n", .message = "does not accept a macro value" },
        .{ .body = "//? macro: value required\n", .message = "must be set in every run" },
        .{ .body = "//? macro: value required\n" ++ run, .message = "must be set in every run" },
        .{
            .body = "//? macro: value required\n" ++ run ++ "//? macro: value set 1\n//?\n//? run: \"second\"\n",
            .message = "must be set in every run",
        },
        .{
            .body = "//? macro: value required\n" ++ run ++ "//? macro: value override 1\n",
            .message = "required macro requires set",
        },
        .{
            .body = "//? macro: value default 1\n" ++ run ++ "//? macro: value set 1\n",
            .message = "default macro requires override",
        },
        .{
            .body = "//? macro: value set 1\n" ++ run ++ "//? macro: value set 1\n",
            .message = "globally set macro cannot be assigned",
        },
        .{
            .body = "//? macro: value set 1\n" ++ run ++ "//? macro: value override 2\n",
            .message = "globally set macro cannot be assigned",
        },
        .{
            .body = "//? macro: value required\n//? macro: value default 1\n",
            .message = "duplicate macro in section",
        },
        .{
            .body = "//? macro: value required\n" ++ run ++ "//? macro: value set 1\n//? macro: value set 2\n",
            .message = "duplicate macro in section",
        },
        .{
            .body = "//? macro: value default\n" ++ run ++ "//? macro: value override\n//? macro: value override 2\n",
            .message = "duplicate macro in section",
        },
    }) |invalid| {
        errors.clearRetainingCapacity();
        const source = try std.mem.concat(arena.allocator(), u8, &.{ header, invalid.body });
        try std.testing.expectError(error.InvalidChecklist, prepare(arena.allocator(), "policies", source, &errors.writer));
        try std.testing.expect(std.mem.find(u8, errors.written(), invalid.message) != null);
    }
    const empty = try prepare(arena.allocator(), "empty", header ++ "//? macro: value required\n" ++ run ++ "//? macro: value set\n\n{{value}}\n", &errors.writer);
    try std.testing.expect(std.mem.endsWith(u8, empty[0].source, "\n\n"));
    const implicit = try prepare(arena.allocator(), "default", header ++ "//? macro: value default 23\n\nMOV result, {{value}}\n", &errors.writer);
    try std.testing.expect(std.mem.endsWith(u8, implicit[0].source, "MOV result, 23\n"));
}

test "empty run overrides suppress defaults and subsequent runs restore them" {
    var arena: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer arena.deinit();

    var errors: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer errors.deinit();

    const source =
        \\//? WINDTUNNEL CHECK LIST
        \\//? macro: operation required
        \\//? macro: optional default NOP
        \\//? macro: fixed set JMP nrel(_end)
        \\//?
        \\//? run: "without optional instruction"
        \\//? macro: operation set MOV result, 1
        \\//? macro: optional override
        \\//?
        \\//? run: "default restored"
        \\//? macro: operation set MOV result, 2
        \\
        \\{{operation}}
        \\{{optional}}
        \\{{fixed}}
    ;
    const runs = try prepare(arena.allocator(), "restore", source, &errors.writer);
    try std.testing.expectEqual(@as(usize, 2), runs.len);
    try std.testing.expect(std.mem.endsWith(u8, runs[0].source, "MOV result, 1\n\nJMP nrel(_end)"));
    try std.testing.expect(std.mem.endsWith(u8, runs[1].source, "MOV result, 2\nNOP\nJMP nrel(_end)"));
}
