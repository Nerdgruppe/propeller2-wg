const std = @import("std");

pub const Address = union(enum) { number: u32, symbol: []const u8 };
pub const Target = union(enum) { reg: Address, hub: Address, c, z, q };
pub const Assignment = struct { line: u32, cog: ?u3 = null, target: Target, bytes: []const u8 };
pub const Profile = enum { cog, program };
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
        if (std.mem.eql(u8, kind, "hub")) {
            const a = try p.address();
            try p.expect("]");
            return .{ .hub = a };
        }
        if (!std.mem.eql(u8, kind, "cog")) return p.fail("unknown state target");
        const cog = try p.integer(try p.take());
        if (cog < 0 or cog > 7) return p.fail("cog must be 0..7");
        p.target_cog = @intCast(cog);
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
    var seen: std.StringHashMap(void) = .init(allocator);
    while (true) {
        p.newlines();
        if (p.index == p.tokens.len) break;
        const line = p.tokens[p.index].line;
        const name = try p.take();
        try p.expect(":");
        if (std.mem.eql(u8, name, "pre") or std.mem.eql(u8, name, "post")) {
            const target = try p.target();
            const is_pre = std.mem.eql(u8, name, "pre");
            if (is_pre and (target == .reg or target == .hub)) return p.fail("preconditions support only C, Z and Q");
            try p.expect(if (is_pre) "=" else "==");
            const bytes = switch (target) {
                .c, .z => blk: {
                    const value = try p.take();
                    if (std.mem.eql(u8, value, "true")) break :blk &[_]u8{1};
                    if (std.mem.eql(u8, value, "false")) break :blk &[_]u8{0};
                    return p.fail("expected true or false");
                },
                .reg, .q => try p.numberBytes(4, try p.integer(try p.take())),
                .hub => try p.block(),
            };
            try (if (is_pre) &pre else &post).append(allocator, .{ .line = line, .cog = p.target_cog, .target = target, .bytes = bytes });
        } else {
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
    for ([_][]const Assignment{ pre.items, post.items }) |assignments| for (assignments) |a| {
        if (a.cog) |cog| if (cog != list.cog) return p.fail("state target must match selected cog");
    };
    list.pre = pre.items;
    list.post = post.items;
    if (list.profile == .program and (list.cog != 0 or list.entry != null or pre.items.len != 0 or post.items.len != 0)) return p.fail("program profile does not support entry or state assignments");
    if (list.profile == .cog and (list.stdin.len != 0 or list.stdout.len != 0 or list.stdin_after.len != 0)) return p.fail("cog profile reserves serial I/O for the reporter");
    if (list.stdin.len > 0 and list.stdin_after.len == 0) return p.fail("nonempty stdin requires stdin-after readiness");
    if (list.stdin_after.len > 0 and !std.mem.startsWith(u8, list.stdout, list.stdin_after)) return p.fail("stdin-after must be a prefix of expected stdout");
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
