const std = @import("std");
const WebSocket = std.http.Server.WebSocket;
const max_output = 1 << 20;

pub const Request = struct {
    image: []const u8,
    stdin: []const u8,
    ready: []const u8,
    baudrate: u32,
    timeout_ms: u32,
    payload_length: usize = 0,
    observation_count: usize = 0,
};

// The URI and its query live in the caller's arena.
fn endpoint(arena: std.mem.Allocator, address: []const u8, request: Request) !std.Uri {
    var uri = try std.Uri.parse(address);
    uri.scheme = if (std.ascii.eqlIgnoreCase(uri.scheme, "ws")) "http" else if (std.ascii.eqlIgnoreCase(uri.scheme, "wss")) "https" else return error.InvalidEndpoint;
    if (uri.host == null or uri.host.?.isEmpty() or uri.fragment != null) return error.InvalidEndpoint;
    var query: std.Io.Writer.Allocating = .init(arena);
    defer query.deinit();
    if (uri.query) |component| {
        const raw = switch (component) {
            .raw, .percent_encoded => |s| s,
        };
        var items = std.mem.splitScalar(u8, raw, '&');
        while (items.next()) |item| {
            if (item.len == 0) continue;
            const name = item[0 .. std.mem.findScalar(u8, item, '=') orelse item.len];
            const key = try (std.Uri.Component{ .percent_encoded = name }).toRawMaybeAlloc(arena);
            if (std.mem.eql(u8, key, "code")) return error.IncompatibleUploadMode;
            if (std.mem.eql(u8, key, "baudrate") or std.mem.eql(u8, key, "timeout_ms")) continue;
            try query.writer.print("{s}&", .{item});
        }
    }
    try query.writer.print("baudrate={d}&timeout_ms={d}", .{ request.baudrate, request.timeout_ms });
    uri.query = .{ .percent_encoded = try query.toOwnedSlice() };
    return uri;
}

fn upload(allocator: std.mem.Allocator, image: []const u8) ![]u8 {
    if (image.len == 0 or image.len > 512 * 1024) return error.InvalidImageLength;
    const length = std.mem.alignForward(usize, image.len, 4);
    const packet = try allocator.alloc(u8, length + 4);
    std.mem.writeInt(u32, packet[0..4], @intCast(length), .little);
    @memcpy(packet[4..][0..image.len], image);
    @memset(packet[4 + image.len ..], 0);
    return packet;
}

pub fn observations(bytes: []const u8, request: Request) ![]const u8 {
    if (bytes.len < 20 or !std.mem.eql(u8, bytes[0..8], "WTOR\x01\x00\x00\x00")) return error.InvalidOracleFrame;
    const length = std.mem.readInt(u32, bytes[8..12], .little);
    const count = std.mem.readInt(u32, bytes[12..16], .little);
    if (length != request.payload_length or count != request.observation_count or bytes.len != @as(u64, length) + 20) return error.InvalidOracleLength;
    const checksum = std.mem.readInt(u32, bytes[bytes.len - 4 ..][0..4], .little);
    if (checksum != std.hash.crc.Crc32.hash(bytes[0 .. bytes.len - 4])) return error.InvalidOracleChecksum;
    return bytes[16 .. bytes.len - 4];
}

const Socket = struct {
    ws: WebSocket,
    allocator: std.mem.Allocator,
    io: std.Io,
    connection: ?*std.http.Client.Connection = null,

    fn send(socket: Socket, bytes: []const u8, opcode: WebSocket.Opcode) !void {
        var encoded: std.Io.Writer.Allocating = .init(socket.allocator);
        defer encoded.deinit();
        // Reserve mask space, then let the standard library encode the frame.
        try encoded.writer.splatByteAll(0, 4);
        var ws = socket.ws;
        ws.output = &encoded.writer;
        try ws.writeMessageUnflushed(bytes, opcode);
        const frame = encoded.written();
        const header_length: usize = switch (frame[5]) {
            126 => 4,
            127 => 10,
            else => 2,
        };
        std.mem.copyForwards(u8, frame[0..header_length], frame[4..][0..header_length]);
        frame[1] |= 0x80;
        const mask = frame[header_length..][0..4];
        try socket.io.randomSecure(mask);
        for (frame[header_length + 4 ..], 0..) |*byte, i| byte.* ^= mask[i % 4];
        try socket.ws.output.writeAll(frame);
        if (socket.connection) |connection| try connection.flush() else try socket.ws.output.flush();
    }

    fn receive(socket: Socket, request: Request, output: *std.ArrayList(u8), diagnostics: *std.Io.Writer) !void {
        var fragmented = false;
        var sent_input = request.stdin.len == 0;
        var closing = false;
        while (true) {
            const header = try socket.ws.input.takeArray(2);
            const h0: WebSocket.Header0 = @bitCast(header[0]);
            const h1: WebSocket.Header1 = @bitCast(header[1]);
            if (h0.rsv1 != 0 or h0.rsv2 != 0 or h0.rsv3 != 0 or h1.mask) return error.InvalidWebSocketFrame;
            const length: u64 = switch (h1.payload_len) {
                .len16 => blk: {
                    const length = try socket.ws.input.takeInt(u16, .big);
                    if (length < 126) return error.InvalidWebSocketFrame;
                    break :blk length;
                },
                .len64 => blk: {
                    const length = try socket.ws.input.takeInt(u64, .big);
                    if (length < 65536 or length >> 63 != 0) return error.InvalidWebSocketFrame;
                    break :blk length;
                },
                else => @intFromEnum(h1.payload_len),
            };
            if (@intFromEnum(h0.opcode) >= 8) {
                if (!h0.fin or length > 125) return error.InvalidWebSocketFrame;
                var buffer: [125]u8 = undefined;
                const data = buffer[0..@intCast(length)];
                try socket.ws.input.readSliceAll(data);
                switch (h0.opcode) {
                    .ping => try socket.send(data, .pong),
                    .pong => {},
                    .connection_close => {
                        if (data.len < 2 or !std.unicode.utf8ValidateSlice(data[2..])) return error.InvalidWebSocketClose;
                        const code = std.mem.readInt(u16, data[0..2], .big);
                        try diagnostics.print("P2AAS close {d}: {s}\n", .{ code, data[2..] });
                        if (!closing) try socket.send("\x03\xe8", .connection_close);
                        if (code != 1000 and !(code == 1008 and std.mem.eql(u8, data[2..], "No time quota left for user code."))) return error.HardwareClosed;
                        if (!sent_input) return error.ProgramNeverReady;
                        if (fragmented) return error.TruncatedWebSocketMessage;
                        return;
                    },
                    else => return error.InvalidWebSocketFrame,
                }
                continue;
            }
            if (closing) return error.TrailingOracleOutput;
            switch (h0.opcode) {
                .binary => if (fragmented) return error.InvalidWebSocketFrame,
                .continuation => if (!fragmented) return error.InvalidWebSocketFrame,
                else => return error.UnexpectedWebSocketOpcode,
            }
            fragmented = !h0.fin;
            if (length > max_output - output.items.len) return error.OutputLimit;
            const old_length = output.items.len;
            try output.resize(socket.allocator, old_length + @as(usize, @intCast(length)));
            // Retain only bytes that actually arrived if the stream ends early.
            const tail = output.items[old_length..];
            output.items.len = old_length;
            const received = try socket.ws.input.readSliceShort(tail);
            output.items.len = old_length + received;
            if (received != length) return error.TruncatedWebSocketMessage;
            if (!sent_input and output.items.len >= request.ready.len) {
                if (!std.mem.startsWith(u8, output.items, request.ready)) return error.ProgramReadinessMismatch;
                try socket.send(request.stdin, .binary);
                sent_input = true;
            }
            if (request.payload_length != 0 and output.items.len >= request.payload_length + 20 and !fragmented) {
                _ = try observations(output.items, request);
                try socket.send("\x03\xe8", .connection_close);
                closing = true;
            }
        }
    }
};

fn hasToken(value: []const u8, token: []const u8) bool {
    var tokens = std.mem.splitScalar(u8, value, ',');
    while (tokens.next()) |item| if (std.ascii.eqlIgnoreCase(std.mem.trim(u8, item, " \t"), token)) return true;
    return false;
}

fn session(allocator: std.mem.Allocator, io: std.Io, address: []const u8, request: Request, output: *std.ArrayList(u8), diagnostics: *std.Io.Writer) anyerror!void {
    var arena: std.heap.ArenaAllocator = .init(allocator);
    defer arena.deinit();
    const uri = try endpoint(arena.allocator(), address, request);
    const packet = try upload(allocator, request.image);
    defer allocator.free(packet);
    var nonce: [16]u8 = undefined;
    try io.randomSecure(&nonce);
    var key_buffer: [24]u8 = undefined;
    const key = std.base64.standard.Encoder.encode(&key_buffer, &nonce);
    var client: std.http.Client = .{ .allocator = allocator, .io = io };
    defer client.deinit();
    var http = try client.request(.GET, uri, .{
        .redirect_behavior = .unhandled,
        .headers = .{ .connection = .{ .override = "Upgrade" } },
        .extra_headers = &.{
            .{ .name = "Upgrade", .value = "websocket" },
            .{ .name = "Sec-WebSocket-Version", .value = "13" },
            .{ .name = "Sec-WebSocket-Key", .value = key },
        },
    });
    defer {
        // The upgraded stream has no HTTP response body to drain or pool.
        http.reader.state = .ready;
        http.connection.?.closing = true;
        http.deinit();
    }
    try http.sendBodiless();
    const response = try http.receiveHead(&.{});
    try diagnostics.print("HTTP {d}\n", .{@intFromEnum(response.head.status)});
    var hash = std.crypto.hash.Sha1.init(.{});
    hash.update(key);
    hash.update("258EAFA5-E914-47DA-95CA-C5AB0DC85B11");
    var digest: [20]u8 = undefined;
    hash.final(&digest);
    var accept_buffer: [28]u8 = undefined;
    const accept = std.base64.standard.Encoder.encode(&accept_buffer, &digest);
    var upgrade = false;
    var connection = false;
    var accepted = false;
    var headers = response.head.iterateHeaders();
    while (headers.next()) |header| {
        if (std.ascii.eqlIgnoreCase(header.name, "upgrade")) upgrade = hasToken(header.value, "websocket");
        if (std.ascii.eqlIgnoreCase(header.name, "connection")) connection = hasToken(header.value, "upgrade");
        if (std.ascii.eqlIgnoreCase(header.name, "sec-websocket-accept")) accepted = std.mem.eql(u8, header.value, accept);
        if (std.ascii.eqlIgnoreCase(header.name, "x-p2aas-error")) try diagnostics.print("P2AAS: {s}\n", .{header.value});
    }
    if (response.head.status != .switching_protocols) return error.HttpUpgradeRejected;
    if (!upgrade or !connection or !accepted) return error.InvalidWebSocketUpgrade;
    const socket: Socket = .{
        .ws = .{ .key = key, .input = http.connection.?.reader(), .output = http.connection.?.writer() },
        .allocator = allocator,
        .io = io,
        .connection = http.connection.?,
    };
    try socket.send(packet, .binary);
    try socket.receive(request, output, diagnostics);
}

pub fn run(allocator: std.mem.Allocator, io: std.Io, address: []const u8, request: Request, output: *std.ArrayList(u8), diagnostics: *std.Io.Writer) !void {
    if (request.baudrate == 0 or request.timeout_ms < 100 or request.timeout_ms > 10000 or request.payload_length > max_output - 20 or request.stdin.len > max_output or request.ready.len > max_output) return error.InvalidRequest;
    if (request.stdin.len != 0 and request.ready.len == 0) return error.MissingReadiness;
    if (request.payload_length != 0 and (request.stdin.len != 0 or request.ready.len != 0)) return error.InvalidRequest;
    const Select = std.Io.Select(union(enum) { response: anyerror!void, timeout: std.Io.Cancelable!void });
    var buffer: [2]Select.Union = undefined;
    var select = Select.init(io, &buffer);
    defer select.cancelDiscard();
    try select.concurrent(.timeout, std.Io.sleep, .{ io, .fromMilliseconds(@as(i64, request.timeout_ms) + 15000), .awake });
    try select.concurrent(.response, session, .{ allocator, io, address, request, output, diagnostics });
    switch (try select.await()) {
        .response => |result| try result,
        .timeout => |result| {
            try result;
            return error.HardwareTimeout;
        },
    }
}

const test_request: Request = .{ .image = "\x12", .stdin = "", .ready = "", .baudrate = 115200, .timeout_ms = 1000 };

test "upload padding and endpoint parameters" {
    const packet = try upload(std.testing.allocator, "\x12\x34\x56");
    defer std.testing.allocator.free(packet);
    try std.testing.expectEqualSlices(u8, "\x04\x00\x00\x00\x12\x34\x56\x00", packet);
    try std.testing.expectError(error.InvalidImageLength, upload(std.testing.allocator, ""));
    var arena: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer arena.deinit();
    const uri = try endpoint(arena.allocator(), "wss://localhost:12880/bridge?token=a%26b&baudrate=1&timeout_ms=2", test_request);
    try std.testing.expectEqualStrings("https", uri.scheme);
    try std.testing.expectEqualStrings("token=a%26b&baudrate=115200&timeout_ms=1000", uri.query.?.percent_encoded);
    try std.testing.expectEqualStrings("/bridge", uri.path.percent_encoded);
    try std.testing.expectError(error.IncompatibleUploadMode, endpoint(arena.allocator(), "ws://localhost/?%63ode=abc", test_request));
    try std.testing.expectError(error.InvalidEndpoint, endpoint(arena.allocator(), "http://localhost/", test_request));
}

test "client masks standard-library frames at all length boundaries" {
    var input = std.Io.Reader.fixed("");
    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();
    const socket: Socket = .{ .ws = .{ .key = "", .input = &input, .output = &output.writer }, .allocator = std.testing.allocator, .io = std.testing.io };
    for ([_]usize{ 0, 125, 126, 65535, 65536 }) |length| {
        output.clearRetainingCapacity();
        const payload = try std.testing.allocator.alloc(u8, length);
        defer std.testing.allocator.free(payload);
        for (payload, 0..) |*byte, i| byte.* = @truncate(i);
        try socket.send(payload, .binary);
        try std.testing.expect(output.written()[1] & 0x80 != 0);
        // Read the resulting client frame with the server-side stdlib helper.
        var reader = std.Io.Reader.fixed(output.written());
        var ws: WebSocket = .{ .key = "", .input = &reader, .output = &output.writer };
        const message = try ws.readSmallMessage();
        try std.testing.expectEqual(.binary, message.opcode);
        try std.testing.expectEqualSlices(u8, payload, message.data);
    }
}

fn receiveTest(bytes: []const u8, request: Request, output: *std.ArrayList(u8), replies: *std.Io.Writer) !void {
    var input = std.Io.Reader.fixed(bytes);
    var diagnostics: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer diagnostics.deinit();
    const socket: Socket = .{ .ws = .{ .key = "", .input = &input, .output = replies }, .allocator = std.testing.allocator, .io = std.testing.io };
    try socket.receive(request, output, &diagnostics.writer);
}

test "fragmented binary output, ping and readiness-gated stdin" {
    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(std.testing.allocator);
    var replies: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer replies.deinit();
    var request = test_request;
    request.stdin = "\x00\xff";
    request.ready = "ready\n";
    try receiveTest("\x02\x02re\x89\x01x\x80\x04ady\n\x82\x02\x00\xff" ++
        "\x88\x23\x03\xf0No time quota left for user code.", request, &output, &replies.writer);
    try std.testing.expectEqualSlices(u8, "ready\n\x00\xff", output.items);
    var reader = std.Io.Reader.fixed(replies.written());
    const pong_header = try reader.takeArray(2);
    try std.testing.expectEqualSlices(u8, "\x8a\x81", pong_header);
    const mask = (try reader.takeArray(4)).*;
    try std.testing.expectEqual(@as(u8, 'x'), (try reader.takeByte()) ^ mask[0]);
    var ws: WebSocket = .{ .key = "", .input = &reader, .output = &replies.writer };
    const stdin = try ws.readSmallMessage();
    try std.testing.expectEqual(.binary, stdin.opcode);
    try std.testing.expectEqualSlices(u8, request.stdin, stdin.data);
    try std.testing.expectEqual(@as(u8, 0x88), (try reader.takeArray(2))[0]);
}

test "malformed frames and closes fail without a P2AAS test server" {
    const cases = .{
        .{ "\x81\x01x", error.UnexpectedWebSocketOpcode },
        .{ "\x82\x80", error.InvalidWebSocketFrame },
        .{ "\x09\x00", error.InvalidWebSocketFrame },
        .{ "\x80\x00", error.InvalidWebSocketFrame },
        .{ "\x82\x7e\x00\x01x", error.InvalidWebSocketFrame },
        .{ "\x82\x03x", error.TruncatedWebSocketMessage },
        .{ "\x88\x01x", error.InvalidWebSocketClose },
        .{ "\x88\x02\x03\xf3", error.HardwareClosed },
        .{ "\x88\x15\x03\xf0No time quota left.", error.HardwareClosed },
    };
    inline for (cases) |case| {
        var output: std.ArrayList(u8) = .empty;
        defer output.deinit(std.testing.allocator);
        var replies: std.Io.Writer.Allocating = .init(std.testing.allocator);
        defer replies.deinit();
        try std.testing.expectError(case[1], receiveTest(case[0], test_request, &output, &replies.writer));
    }
}

test "oracle frame length, count and CRC reject corrupt and trailing bytes" {
    var frame: [25]u8 = undefined;
    @memcpy(frame[0..8], "WTOR\x01\x00\x00\x00");
    std.mem.writeInt(u32, frame[8..12], 4, .little);
    std.mem.writeInt(u32, frame[12..16], 1, .little);
    @memcpy(frame[16..20], "data");
    std.mem.writeInt(u32, frame[20..24], std.hash.crc.Crc32.hash(frame[0..20]), .little);
    frame[24] = 0;
    var request = test_request;
    request.payload_length = 4;
    request.observation_count = 1;
    try std.testing.expectEqualStrings("data", try observations(frame[0..24], request));
    try std.testing.expectError(error.InvalidOracleLength, observations(&frame, request));
    try std.testing.expectError(error.InvalidOracleLength, observations(frame[0..23], request));
    request.observation_count = 2;
    try std.testing.expectError(error.InvalidOracleLength, observations(frame[0..24], request));
    request.observation_count = 1;
    frame[16] ^= 1;
    try std.testing.expectError(error.InvalidOracleChecksum, observations(frame[0..24], request));
    frame[16] ^= 1;

    // A complete snapshot initiates closing; a later data frame still fails.
    var wire: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer wire.deinit();
    try wire.writer.writeAll("\x82\x18");
    try wire.writer.writeAll(frame[0..24]);
    try wire.writer.writeAll("\x88\x02\x03\xe8");
    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(std.testing.allocator);
    var replies: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer replies.deinit();
    try receiveTest(wire.written(), request, &output, &replies.writer);
    try std.testing.expectEqualSlices(u8, frame[0..24], output.items);
    try std.testing.expectEqual(@as(u8, 0x88), replies.written()[0]);
    wire.writer.undo(4);
    try wire.writer.writeAll("\x82\x01x");
    output.clearRetainingCapacity();
    try std.testing.expectError(error.TrailingOracleOutput, receiveTest(wire.written(), request, &output, &replies.writer));
}
