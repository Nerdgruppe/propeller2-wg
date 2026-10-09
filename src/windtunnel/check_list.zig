const std = @import("std");

pub const Address = union(enum) { number: u32, symbol: []const u8 };
pub const Target = union(enum) { sym: []const u8, reg: Address, hub: Address, c, z, q };
pub const Assignment = struct { line: u32, cog: ?u3 = null, target: Target, bytes: []const u8 };
pub const Profile = enum { cog, program };
pub const Constant = struct { name: []const u8, value: i64 };
pub const Run = struct {
    cog: ?u3 = null,
    constants: []const Constant = &.{},
    name: ?[]const u8 = null,
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
    max_cycles: u64 = 1_000_000,
    timeout_ms: u32 = 5000,
    baudrate: u32 = 115200,
    stdin: []const u8 = "",
    stdout: []const u8 = "",
    stdin_after: []const u8 = "",
    pre: []const Assignment = &.{},
    post: []const Assignment = &.{},
    runs: []const Run = &.{},
    constants: []const Constant = &.{},

    /// Common patches apply first; run-local flag/Q seeds replace common seeds.
    pub fn forRun(list: List, allocator: std.mem.Allocator, run: Run) !List {
        var result = list;
        result.cog = run.cog orelse list.cog;
        result.stdin = run.stdin orelse list.stdin;
        result.stdout = run.stdout orelse list.stdout;
        result.stdin_after = run.stdin_after orelse list.stdin_after;
        var constants: std.ArrayList(Constant) = .empty;
        for (list.constants) |common| {
            var replaced = false;
            for (run.constants) |local| if (std.mem.eql(u8, common.name, local.name)) {
                replaced = true;
            };
            if (!replaced) try constants.append(allocator, common);
        }
        try constants.appendSlice(allocator, run.constants);
        result.constants = constants.items;
        var pre: std.ArrayList(Assignment) = .empty;
        for (list.pre) |common| {
            var replaced = false;
            if (common.target != .sym) for (run.pre) |local| {
                if (std.meta.activeTag(common.target) == std.meta.activeTag(local.target)) replaced = true;
            };
            if (!replaced) try pre.append(allocator, common);
        }
        try pre.appendSlice(allocator, run.pre);
        result.pre = pre.items;
        result.post = try std.mem.concat(allocator, Assignment, &.{ list.post, run.post });
        result.runs = &.{};
        return result;
    }
};

const Token = struct { text: []const u8, line: u32 };
const Parser = struct {
    allocator: std.mem.Allocator,
    path: []const u8,
    errors: *std.Io.Writer,
    tokens: []const Token,
    index: usize = 0,
    target_cog: ?u3 = null,

    fn fail(p: *Parser, message: []const u8) error{InvalidChecklist} {
        const line = if (p.index < p.tokens.len) p.tokens[p.index].line else if (p.tokens.len > 0) p.tokens[p.tokens.len - 1].line else 1;
        p.errors.print("{s}:{d}: checklist: {s}\n", .{ p.path, line, message }) catch {};
        return error.InvalidChecklist;
    }

    fn peek(p: *Parser) []const u8 {
        return if (p.index < p.tokens.len) p.tokens[p.index].text else "";
    }

    fn take(p: *Parser) ![]const u8 {
        if (p.index == p.tokens.len) return p.fail("unexpected end of checklist");
        const result = p.tokens[p.index].text;
        p.index += 1;
        return result;
    }

    fn expect(p: *Parser, expected: []const u8) !void {
        if (!std.mem.eql(u8, p.peek(), expected)) return p.fail(expected);
        p.index += 1;
    }

    fn newlines(p: *Parser) void {
        while (std.mem.eql(u8, p.peek(), "\n")) p.index += 1;
    }

    fn integer(p: *Parser, token: []const u8) !i64 {
        const clean = try p.allocator.alloc(u8, token.len);
        var len: usize = 0;
        for (token) |c| if (c != '_') {
            clean[len] = c;
            len += 1;
        };
        return std.fmt.parseInt(i64, clean[0..len], 0) catch return p.fail("invalid integer");
    }

    fn positive(p: *Parser) !u64 {
        const n = try p.integer(try p.take());
        if (n <= 0) return p.fail("expected positive integer");
        return @intCast(n);
    }

    fn address(p: *Parser) !Address {
        const value = try p.take();
        if (value.len == 0 or std.mem.indexOfScalar(u8, "\n[]=,:\"", value[0]) != null) return p.fail("expected address or symbol");
        if (std.ascii.isDigit(value[0]) or value[0] == '-') {
            const n = try p.integer(value);
            if (n < 0 or n > std.math.maxInt(u32)) return p.fail("address outside u32 range");
            return .{ .number = @intCast(n) };
        }
        return .{ .symbol = value };
    }

    fn target(p: *Parser) !Target {
        const kind = try p.take();
        p.target_cog = null;
        try p.expect("[");
        if (std.mem.eql(u8, kind, "sym")) {
            const a = try p.address();
            if (a != .symbol) return p.fail("sym requires a symbol name");
            try p.expect("]");
            return .{ .sym = a.symbol };
        }
        if (std.mem.eql(u8, kind, "hub")) {
            const a = try p.address();
            try p.expect("]");
            return .{ .hub = a };
        }
        if (!std.mem.eql(u8, kind, "cog")) return p.fail("unknown state target");
        const selected = try p.take();
        if (!std.mem.eql(u8, selected, "*")) {
            const cog = try p.integer(selected);
            if (cog < 0 or cog > 7) return p.fail("cog must be 0..7");
            p.target_cog = @intCast(cog);
        }
        try p.expect("]");
        const field = try p.take();
        if (std.mem.eql(u8, field, ".c")) return .c;
        if (std.mem.eql(u8, field, ".z")) return .z;
        if (std.mem.eql(u8, field, ".q")) return .q;
        if (!std.mem.eql(u8, field, ".reg")) return p.fail("unsupported cog field");
        try p.expect("[");
        const a = try p.address();
        try p.expect("]");
        return .{ .reg = a };
    }

    fn numberBytes(p: *Parser, width: usize, n: i64) ![]const u8 {
        const bits: u6 = @intCast(width * 8);
        const min = -(@as(i64, 1) << @intCast(bits - 1));
        const max = (@as(i64, 1) << bits) - 1;
        if (n < min or n > max) return p.fail("integer does not fit element width");
        const bytes = try p.allocator.alloc(u8, width);
        const raw: u64 = @bitCast(n);
        for (bytes, 0..) |*byte, i| byte.* = @truncate(raw >> @intCast(8 * i));
        return bytes;
    }

    fn string(p: *Parser) ![]const u8 {
        const token = try p.take();
        if (token.len < 2 or token[0] != '"' or token[token.len - 1] != '"') return p.fail("expected quoted string");
        var bytes: std.ArrayList(u8) = .empty;
        var i: usize = 1;
        while (i < token.len - 1) : (i += 1) {
            var byte = token[i];
            if (byte == '\\') {
                i += 1;
                if (i >= token.len - 1) return p.fail("incomplete string escape");
                byte = switch (token[i]) {
                    '\\', '"' => token[i],
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    'x' => blk: {
                        if (i + 2 >= token.len - 1) return p.fail("incomplete hex escape");
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

    fn block(p: *Parser) ![]const u8 {
        const format = try p.take();
        const width: usize = if (std.mem.eql(u8, format, "u8")) 1 else if (std.mem.eql(u8, format, "u16")) 2 else if (std.mem.eql(u8, format, "u32")) 4 else if (std.mem.eql(u8, format, "hex")) 0 else return p.fail("expected u8/u16/u32/hex block");
        try p.expect("[");
        var bytes: std.ArrayList(u8) = .empty;
        while (true) {
            p.newlines();
            if (std.mem.eql(u8, p.peek(), "]")) {
                p.index += 1;
                break;
            }
            const token = try p.take();
            if (std.mem.eql(u8, token, ",")) continue;
            if (width == 0) {
                if (token.len % 2 != 0) return p.fail("hex bytes require pairs of digits");
                var i: usize = 0;
                while (i < token.len) : (i += 2) try bytes.append(p.allocator, std.fmt.parseInt(u8, token[i..][0..2], 16) catch return p.fail("invalid hex byte"));
            } else try bytes.appendSlice(p.allocator, try p.numberBytes(width, try p.integer(token)));
            if (bytes.items.len > 1 << 20) return p.fail("block exceeds 1 MiB");
        }
        return try bytes.toOwnedSlice(p.allocator);
    }

    fn stream(p: *Parser) ![]const u8 {
        return if (std.mem.startsWith(u8, p.peek(), "\"")) p.string() else p.block();
    }
};

/// Allocations belong to the caller's arena, including token and value storage.
pub fn parse(allocator: std.mem.Allocator, path: []const u8, source: []const u8, errors: *std.Io.Writer) !List {
    var lines = std.mem.splitScalar(u8, source, '\n');
    if (!std.mem.eql(u8, std.mem.trimEnd(u8, lines.next() orelse "", "\r"), "//? WINDTUNNEL CHECK LIST")) {
        try errors.print("{s}:1: checklist: missing WINDTUNNEL CHECK LIST header\n", .{path});
        return error.InvalidChecklist;
    }
    var tokens: std.ArrayList(Token) = .empty;
    var line_number: u32 = 1;
    while (lines.next()) |raw| {
        line_number += 1;
        if (!std.mem.startsWith(u8, raw, "//?")) break;
        const line = std.mem.trimEnd(u8, raw[3..], "\r");
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
                if (line[start] == '=' and i < line.len and line[i] == '=') i += 1;
            } else {
                while (i < line.len and !std.ascii.isWhitespace(line[i]) and std.mem.indexOfScalar(u8, "[]=:,\"", line[i]) == null) i += 1;
            }
            try tokens.append(allocator, .{ .text = line[start..i], .line = line_number });
        }
        try tokens.append(allocator, .{ .text = "\n", .line = line_number });
    }
    var p: Parser = .{ .allocator = allocator, .path = path, .errors = errors, .tokens = tokens.items };
    var list: List = .{};
    var pre: std.ArrayList(Assignment) = .empty;
    var post: std.ArrayList(Assignment) = .empty;
    var runs: std.ArrayList(Run) = .empty;
    var constants: std.ArrayList(Constant) = .empty;
    var seen: std.StringHashMap(void) = .init(allocator);
    while (true) {
        p.newlines();
        if (p.index == p.tokens.len) break;
        const line = p.tokens[p.index].line;
        const name = try p.take();
        try p.expect(":");
        if (std.mem.eql(u8, name, "run")) {
            const run_name = if (std.mem.eql(u8, p.peek(), "\n")) null else try p.string();
            if (run_name) |value| {
                if (value.len == 0) return p.fail("run name must not be empty");
                for (runs.items) |old| if (old.name) |old_name| {
                    if (std.mem.eql(u8, old_name, value)) return p.fail("duplicate run name");
                };
            }
            if (runs.items.len == 0) {
                list.pre = pre.items;
                list.post = post.items;
                list.constants = constants.items;
            } else {
                runs.items[runs.items.len - 1].pre = pre.items;
                runs.items[runs.items.len - 1].post = post.items;
                runs.items[runs.items.len - 1].constants = constants.items;
            }
            constants = .empty;
            pre = .empty;
            post = .empty;
            try runs.append(allocator, .{ .name = run_name });
        } else if (std.mem.eql(u8, name, "const")) {
            const symbol = try p.take();
            if (symbol.len == 0 or !(std.ascii.isAlphabetic(symbol[0]) or symbol[0] == '_') or std.mem.startsWith(u8, symbol, "_wt_")) return p.fail("invalid constant name");
            for (symbol) |c| if (!(std.ascii.isAlphanumeric(c) or c == '_')) return p.fail("invalid constant name");
            for (constants.items) |old| if (std.mem.eql(u8, old.name, symbol)) return p.fail("duplicate constant");
            try p.expect("=");
            try constants.append(allocator, .{ .name = symbol, .value = try p.integer(try p.take()) });
        } else if (runs.items.len != 0 and (std.mem.eql(u8, name, "stdin") or std.mem.eql(u8, name, "stdout") or std.mem.eql(u8, name, "stdin-after"))) {
            const run = &runs.items[runs.items.len - 1];
            const stream = if (std.mem.eql(u8, name, "stdin")) &run.stdin else if (std.mem.eql(u8, name, "stdout")) &run.stdout else &run.stdin_after;
            if (stream.* != null) return p.fail("duplicate run stream");
            stream.* = try p.stream();
        } else if (std.mem.eql(u8, name, "cog") and runs.items.len != 0) {
            const run = &runs.items[runs.items.len - 1];
            if (run.cog != null) return p.fail("duplicate run cog");
            const n = try p.integer(try p.take());
            if (n < 0 or n > 7) return p.fail("cog must be 0..7");
            run.cog = @intCast(n);
        } else if (std.mem.eql(u8, name, "pre") or std.mem.eql(u8, name, "post")) {
            const target = try p.target();
            const is_pre = std.mem.eql(u8, name, "pre");
            if (is_pre and (target == .reg or target == .hub)) return p.fail("preconditions support only sym, C, Z and Q");
            if (!is_pre and target == .sym) return p.fail("sym is only supported in preconditions");
            try p.expect(if (is_pre) "=" else "==");
            const bytes = switch (target) {
                .c, .z => blk: {
                    const value = try p.take();
                    if (std.mem.eql(u8, value, "true")) break :blk &[_]u8{1};
                    if (std.mem.eql(u8, value, "false")) break :blk &[_]u8{0};
                    return p.fail("expected true or false");
                },
                .reg, .q => try p.numberBytes(4, try p.integer(try p.take())),
                .hub, .sym => try p.block(),
            };
            const assignment: Assignment = .{ .line = line, .cog = p.target_cog, .target = target, .bytes = bytes };
            try (if (is_pre) &pre else &post).append(allocator, assignment);
        } else {
            if (runs.items.len != 0) return p.fail("configuration directives must precede run sections");
            const entry = try seen.getOrPut(name);
            if (entry.found_existing) return p.fail("duplicate directive");
            if (std.mem.eql(u8, name, "profile")) list.profile = std.meta.stringToEnum(Profile, try p.take()) orelse return p.fail("unknown profile") else if (std.mem.eql(u8, name, "cog")) {
                const n = try p.integer(try p.take());
                if (n < 0 or n > 7) return p.fail("cog must be 0..7");
                list.cog = @intCast(n);
            } else if (std.mem.eql(u8, name, "entry")) list.entry = try p.take() else if (std.mem.eql(u8, name, "stop")) list.stop = try p.take() else if (std.mem.eql(u8, name, "max-cycles")) list.max_cycles = try p.positive() else if (std.mem.eql(u8, name, "timeout-ms")) {
                const n = try p.positive();
                if (n < 100 or n > 10000) return p.fail("timeout-ms must be 100..10000");
                list.timeout_ms = @intCast(n);
            } else if (std.mem.eql(u8, name, "baudrate")) {
                const n = try p.positive();
                if (n > std.math.maxInt(i32)) return p.fail("baudrate too large");
                list.baudrate = @intCast(n);
            } else if (std.mem.eql(u8, name, "oracle")) {
                const value = try p.take();
                if (std.mem.eql(u8, value, "off")) {
                    list.oracle_off = try p.string();
                    if (list.oracle_off.?.len == 0) return p.fail("oracle off requires a reason");
                } else if (!std.mem.eql(u8, value, "required")) return p.fail("expected required or off");
            } else if (std.mem.eql(u8, name, "stdin")) list.stdin = try p.stream() else if (std.mem.eql(u8, name, "stdout")) list.stdout = try p.stream() else if (std.mem.eql(u8, name, "stdin-after")) list.stdin_after = try p.stream() else return p.fail("unknown directive");
        }
        if (p.index < p.tokens.len) try p.expect("\n");
    }
    if (runs.items.len == 0) {
        list.pre = pre.items;
        list.post = post.items;
        list.constants = constants.items;
        try runs.append(allocator, .{});
    } else {
        runs.items[runs.items.len - 1].pre = pre.items;
        runs.items[runs.items.len - 1].post = post.items;
        runs.items[runs.items.len - 1].constants = constants.items;
    }
    list.runs = runs.items;
    for (list.runs) |run| {
        const stdin = run.stdin orelse list.stdin;
        const stdout = run.stdout orelse list.stdout;
        const stdin_after = run.stdin_after orelse list.stdin_after;
        if (list.profile == .cog and (stdin.len != 0 or stdout.len != 0 or stdin_after.len != 0)) return p.fail("cog profile reserves serial I/O for the reporter");
        if (stdin.len > 0 and stdin_after.len == 0) return p.fail("nonempty stdin requires stdin-after readiness");
        if (stdin_after.len > 0 and !std.mem.startsWith(u8, stdout, stdin_after)) return p.fail("stdin-after must be a prefix of expected stdout");
        const selected_cog = run.cog orelse list.cog;
        for ([_][]const Assignment{ list.pre, list.post, run.pre, run.post }) |assignments| for (assignments) |a| {
            if (a.cog) |cog| if (cog != selected_cog) return p.fail("state target must match selected cog");
        };
        if (list.profile == .program) {
            if (selected_cog != 0 or list.entry != null or list.post.len != 0 or run.post.len != 0) return p.fail("program profile does not support entry or state assignments");
            for ([_][]const Assignment{ list.pre, run.pre }) |assignments| for (assignments) |a| {
                if (a.target != .sym) return p.fail("program profile supports only symbol patches as preconditions");
            };
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
    for ([_][]const u8{
        "//? typo: 1\n",                            "//? max-cycles: 0\n",             "//? timeout-ms: 99\n",                 "//? pre: cog[1].c = true\n",
        "//? pre: cog[0].reg[x] = 0x1_0000_0000\n", "//? post: hub[0] == u8 [256]\n",  "//? post: hub[0] == hex [f]\n",        "//? stdout: \"\\q\"\n",
        "//? profile: program\n//? stdin: \"x\"\n", "//? oracle: off \"\"\n",          "//? profile: cog\n//? profile: cog\n", "//? max-cycles: 1 extra\n",
        "//? pre: cog[0].reg[value] = 1\n",         "//? pre: hub[buffer] = u8 [1]\n", "//? output: u32 [1]\n",                "//? profile: program\n//? oracle: off \"removed output hook\"\n//? output: u32 [1]\n",
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
        \\//? run:
        \\//? pre: cog[0].c = false
        \\//? post: cog[0].reg[value] == 1
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
        "//? run: unquoted\n",
        "//? run: \"\"\n",
        "//? run: \"same\"\n//? run: \"same\"\n",
        "//? run:\n//? timeout-ms: 1000\n",
        "//? run:\n//? post: cog[1].c == true\n",
        "//? post: sym[value] == u32 [1]\n",
        "//? pre: sym[42] = u32 [1]\n",
        "//? profile: program\n//? run:\n//? pre: cog[0].c = true\n",
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
            "//? run: \"override\"\n" ++
            "//? cog: 7\n" ++
            "//? const: width = 1\n" ++
            "//? run: \"restored\"\n",
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
        "//? run:\n//? cog: 8\n",
        "//? run:\n//? cog: 2\n//? cog: 3\n",
        "//? run:\n//? cog: 7\n//? post: cog[0].c == true\n",
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
        "//? stdin-after: \"ready\"\n//? run: \"first\"\n//? stdin: \"\"\n//? stdout: \"ready first\"\n" ++
        "//? run: \"second\"\n//? stdin-after: \"r\"\n";
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
    try std.testing.expectError(error.InvalidChecklist, parse(arena.allocator(), "matrix", "//? WINDTUNNEL CHECK LIST\n//? run:\n//? stdin: \"x\"\n", &errors.writer));
}
