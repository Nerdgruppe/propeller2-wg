//! P2AAS-compatible upload and live UART bridge, with one simulated board per request.
//! The HTTP/WebSocket contract follows p2aas.cs; loader text and serial recovery are internal to hardware.
const std = @import("std");
const Hub = @import("sim/Hub.zig");
const smart_pin = @import("sim/smart_pin.zig");
const WebSocket = std.http.Server.WebSocket;
const max_payload = 512 * 1024;
const max_request_head = 768 * 1024;
const internal_reason = "The server experienced an unexpected error.";

const Options = struct {
    baudrate: u32 = 115200,
    timeout_ms: u32 = 2500,
    code: [max_payload]u8 = undefined,
    code_len: ?usize = null,

    /// Match HTTP query decoding, defaults for blank numbers, and URL-code upload precedence.
    fn parse(target: []const u8) !Options {
        var result: Options = .{};
        const start = std.mem.findScalar(u8, target, '?') orelse return result;
        if (target.len > max_request_head) return error.InvalidQuery;
        var name_buffer: [128]u8 = undefined;
        // Base64 is larger than its decoded image; allow the HTTP head bound plus padding.
        var value_buffer: [max_request_head + 3]u8 = undefined;
        var items = std.mem.splitScalar(u8, target[start + 1 ..], '&');
        var saw_baud = false;
        var saw_timeout = false;
        while (items.next()) |item| {
            const equal = std.mem.findScalar(u8, item, '=') orelse item.len;
            const name = try decodeQuery(&name_buffer, item[0..equal]);
            const value = try decodeQuery(value_buffer[0..max_request_head], if (equal < item.len) item[equal + 1 ..] else "");
            if (std.ascii.eqlIgnoreCase(name, "code")) {
                if (result.code_len != null) return error.DuplicateCode;
                // .NET removes whitespace, accepts URL alphabet and pads omitted '='.
                // Normalization writes forwards into the same buffer, behind the unread bytes.
                var normalized = std.Io.Writer.fixed(&value_buffer);
                for (value) |byte| switch (byte) {
                    ' ', '\t', '\r', '\n' => {},
                    '-' => try normalized.writeByte('+'),
                    '_' => try normalized.writeByte('/'),
                    else => try normalized.writeByte(byte),
                };
                if (normalized.buffered().len > (max_payload / 3 + 1) * 4) return error.PayloadTooLarge;
                while (normalized.buffered().len % 4 != 0) try normalized.writeByte('=');
                const decoder = std.base64.standard.Decoder;
                const len = decoder.calcSizeForSlice(normalized.buffered()) catch return error.InvalidCode;
                if (len > max_payload) return error.PayloadTooLarge;
                decoder.decode(result.code[0..len], normalized.buffered()) catch return error.InvalidCode;
                if (len % 4 != 0) return error.UnalignedPayload;
                result.code_len = len;
            } else if (std.ascii.eqlIgnoreCase(name, "baudrate") or std.ascii.eqlIgnoreCase(name, "timeout_ms")) {
                const baud = std.ascii.eqlIgnoreCase(name, "baudrate");
                const seen = if (baud) &saw_baud else &saw_timeout;
                if (seen.*) return error.InvalidNumber;
                seen.* = true;
                const number = std.mem.trim(u8, value, " \t\r\n");
                if (number.len == 0) continue;
                // Zig also accepts digit separators; .NET's decimal query parser rejects them.
                const digits = if (number[0] == '+' or number[0] == '-') number[1..] else number;
                if (digits.len == 0) return error.InvalidNumber;
                for (digits) |digit| if (!std.ascii.isDigit(digit)) return error.InvalidNumber;
                const parsed = std.fmt.parseInt(i32, number, 10) catch return error.InvalidNumber;
                if (parsed <= 0) return error.InvalidNumber;
                if (baud) result.baudrate = @intCast(parsed) else result.timeout_ms = @intCast(parsed);
            }
        }
        if (result.timeout_ms < 100 or result.timeout_ms > 10000) return error.InvalidTimeout;
        return result;
    }
};

/// Form queries map literal '+' to space before URI decoding; escaped '+' remains literal.
fn decodeQuery(buffer: []u8, raw: []const u8) ![]u8 {
    if (raw.len > buffer.len) return error.InvalidQuery;
    const encoded = buffer[0..raw.len];
    @memcpy(encoded, raw);
    var index: usize = 0;
    while (index < raw.len) : (index += 1) {
        if (encoded[index] == '+') encoded[index] = ' ';
        // std.Uri preserves malformed escapes; this protocol rejects them instead.
        if (encoded[index] == '%') {
            if (raw.len - index < 3 or !std.ascii.isHex(encoded[index + 1]) or !std.ascii.isHex(encoded[index + 2])) return error.InvalidQuery;
            index += 2;
        }
    }
    return std.Uri.percentDecodeInPlace(encoded);
}

const Close = struct { bytes: [125]u8 = undefined, len: usize = 0 };
const Chunk = struct { opcode: WebSocket.Opcode, data: []const u8 };

/// Streaming client frames: bounded storage, masking, continuations and interleaved control frames.
/// Keep the current frame position so upload reads can stop exactly at the payload boundary.
const Frames = struct {
    input: *std.Io.Reader,
    remaining: u64 = 0,
    mask: [4]u8 = undefined,
    position: usize = 0,
    opcode: WebSocket.Opcode = .binary,
    fragmented: ?WebSocket.Opcode = null,
    fin: bool = true,
    buffer: [4096]u8 = undefined,
    utf8_left: u3 = 0,
    utf8_value: u32 = 0,
    utf8_min: u32 = 0,

    /// Read up to limit data bytes, or a complete control payload; no message-sized allocations.
    fn next(frames: *Frames, limit: usize) !Chunk {
        if (frames.remaining == 0) {
            const header = (try frames.input.takeArray(2)).*;
            const h0: WebSocket.Header0 = @bitCast(header[0]);
            const h1: WebSocket.Header1 = @bitCast(header[1]);
            if (h0.rsv1 != 0 or h0.rsv2 != 0 or h0.rsv3 != 0 or !h1.mask) return error.InvalidFrame;
            frames.remaining = switch (h1.payload_len) {
                .len16 => blk: {
                    const len = try frames.input.takeInt(u16, .big);
                    if (len < 126) return error.InvalidFrame;
                    break :blk len;
                },
                .len64 => blk: {
                    const len = try frames.input.takeInt(u64, .big);
                    if (len < 65536 or len >> 63 != 0) return error.InvalidFrame;
                    break :blk len;
                },
                else => @intFromEnum(h1.payload_len),
            };
            frames.fin = h0.fin;
            if (@intFromEnum(h0.opcode) >= 8) {
                if (!h0.fin or frames.remaining > 125) return error.InvalidFrame;
                switch (h0.opcode) {
                    .ping, .pong, .connection_close => {},
                    else => return error.InvalidFrame,
                }
                frames.opcode = h0.opcode;
            } else {
                switch (h0.opcode) {
                    .binary, .text => {
                        if (frames.fragmented != null) return error.InvalidFrame;
                        frames.opcode = h0.opcode;
                        if (!h0.fin) frames.fragmented = h0.opcode;
                    },
                    .continuation => frames.opcode = frames.fragmented orelse return error.InvalidFrame,
                    else => return error.InvalidFrame,
                }
            }
            frames.mask = (try frames.input.takeArray(4)).*;
            frames.position = 0;
        }
        const control = @intFromEnum(frames.opcode) >= 8;
        const count: usize = @intCast(@min(frames.remaining, if (control) 125 else @min(limit, frames.buffer.len)));
        const data = frames.buffer[0..count];
        try frames.input.readSliceAll(data);
        for (data, 0..) |*byte, i| byte.* ^= frames.mask[(frames.position + i) % 4];
        frames.position += count;
        frames.remaining -= count;
        if (frames.opcode == .text) {
            for (data) |byte| try frames.validateText(byte);
            if (frames.remaining == 0 and frames.fin and frames.utf8_left != 0) return error.InvalidText;
        }
        if (!control and frames.remaining == 0 and frames.fin) frames.fragmented = null;
        if (frames.opcode == .connection_close) {
            if (data.len == 1 or (data.len >= 2 and !std.unicode.utf8ValidateSlice(data[2..]))) return error.InvalidClose;
            if (data.len >= 2) {
                const code = std.mem.readInt(u16, data[0..2], .big);
                if (!((code >= 1000 and code <= 1014 and code != 1004 and code != 1005 and code != 1006) or (code >= 3000 and code < 5000))) return error.InvalidClose;
            }
        }
        return .{ .opcode = frames.opcode, .data = data };
    }

    /// UTF-8 state spans frame and chunk boundaries, including fragmented terminal text.
    fn validateText(frames: *Frames, byte: u8) !void {
        if (frames.utf8_left == 0) {
            if (byte < 0x80) return;
            if (byte >= 0xc2 and byte <= 0xdf) {
                frames.utf8_left = 1;
                frames.utf8_value = byte & 31;
                frames.utf8_min = 0x80;
            } else if (byte >= 0xe0 and byte <= 0xef) {
                frames.utf8_left = 2;
                frames.utf8_value = byte & 15;
                frames.utf8_min = 0x800;
            } else if (byte >= 0xf0 and byte <= 0xf4) {
                frames.utf8_left = 3;
                frames.utf8_value = byte & 7;
                frames.utf8_min = 0x10000;
            } else return error.InvalidText;
        } else {
            if (byte & 0xc0 != 0x80) return error.InvalidText;
            frames.utf8_value = (frames.utf8_value << 6) | (byte & 63);
            frames.utf8_left -= 1;
            if (frames.utf8_left == 0 and (frames.utf8_value < frames.utf8_min or frames.utf8_value > 0x10ffff or (frames.utf8_value >= 0xd800 and frames.utf8_value <= 0xdfff))) return error.InvalidText;
        }
    }
};

const Event = union(enum) { peer: Close, receive_error: anyerror, execution_error: anyerror, timeout };
const Connection = struct {
    allocator: std.mem.Allocator,
    io: std.Io,
    frames: Frames,
    socket: WebSocket,
    network_reader: ?*std.Io.net.Stream.Reader = null,
    send_mutex: std.Io.Mutex = .init,
    closing: std.atomic.Value(bool) = .init(false),
    upgraded: bool = false,
    running: bool = false,
    peer_closed: bool = false,
    trace: bool,

    /// Buffered Reader reports ReadFailed; preserve cancellation from its underlying network reader.
    fn readChunk(connection: *Connection, limit: usize) !Chunk {
        return connection.frames.next(limit) catch |err| {
            if (connection.network_reader) |reader| if (reader.err) |read_error| if (read_error == error.Canceled) return error.Canceled;
            return err;
        };
    }

    /// Serialize UART, pong and close writes; runtime writes stop before the close frame.
    fn send(connection: *Connection, data: []const u8, opcode: WebSocket.Opcode) !void {
        try connection.send_mutex.lock(connection.io);
        defer connection.send_mutex.unlock(connection.io);
        if (connection.closing.load(.acquire) and opcode != .connection_close) return;
        try connection.socket.writeMessage(data, opcode);
    }

    /// Treat binary data as a stream while servicing control traffic during loading.
    fn uploadRead(connection: *Connection, out: []u8) !void {
        var offset: usize = 0;
        while (offset < out.len) {
            const chunk = try connection.readChunk(out.len - offset);
            switch (chunk.opcode) {
                .binary => {
                    @memcpy(out[offset..][0..chunk.data.len], chunk.data);
                    offset += chunk.data.len;
                },
                .ping => try connection.send(chunk.data, .pong),
                .pong => {},
                .connection_close => {
                    connection.peer_closed = true;
                    return error.TruncatedUpload;
                },
                else => return error.NonBinaryUpload,
            }
        }
    }
};

/// Receive into a bounded byte queue; the receiver remains alive to read the close reply.
fn receive(connection: *Connection, input: *std.Io.Queue(u8), events: *std.Io.Queue(Event)) !void {
    while (true) {
        const chunk = connection.readChunk(4096) catch |err| {
            if (err == error.Canceled) return err;
            try events.putOne(connection.io, .{ .receive_error = err });
            return;
        };
        switch (chunk.opcode) {
            .binary, .text => if (!connection.closing.load(.acquire)) {
                input.putAll(connection.io, chunk.data) catch |err| {
                    if (err != error.Closed) return err;
                };
            },
            .ping => connection.send(chunk.data, .pong) catch |err| {
                try events.putOne(connection.io, .{ .receive_error = err });
                return;
            },
            .pong => {},
            .connection_close => {
                var close: Close = .{ .len = chunk.data.len };
                @memcpy(close.bytes[0..close.len], chunk.data);
                try events.putOne(connection.io, .{ .peer = close });
                return;
            },
            else => unreachable,
        }
    }
}

/// Only this task mutates Hub. Wall pacing keeps virtual execution from outrunning interactive clients.
fn simulate(connection: *Connection, hub: *Hub, input: *std.Io.Queue(u8)) !void {
    var output_buffer: [4096]u8 = undefined;
    var output = std.Io.Writer.fixed(&output_buffer);
    var terminal_sink: smart_pin.DataSink = .{ .writer = &output };
    hub.io.pins[62].smart.registers.sink = &terminal_sink;
    var input_buffer: [4096]u8 = undefined;
    var reader = std.Io.Reader.fixed(&input_buffer);
    reader.end = 0;
    var terminal_source: smart_pin.DataSource = .{ .reader = &reader };
    hub.io.pins[63].smart.registers.source = &terminal_source;
    var trace_buffer: [4096]u8 = undefined;
    var trace = std.Io.File.stderr().writer(connection.io, &trace_buffer);
    if (connection.trace) hub.trace_writer = &trace.interface;
    defer trace.interface.flush() catch {};
    const started = std.Io.Timestamp.now(connection.io, .awake).toNanoseconds();
    var virtual_fixed: u128 = 0;
    while (true) {
        try connection.io.checkCancel();
        // Refill buffered input outside the simulation; smart pins never block on the host.
        if (reader.seek != 0) std.Io.Reader.defaultRebase(&reader, reader.buffer.len) catch unreachable;
        const count = input.get(connection.io, reader.buffer[reader.end..], 0) catch |err| switch (err) {
            error.Closed => 0,
            else => return err,
        };
        reader.end += count;
        // A millisecond ceiling bounds responsiveness even when WAITX can skip many clocks.
        const ceiling = hub.counter + @max(1, hub.io.clock_frequency / 1000);
        for (0..4096) |_| {
            const before = hub.counter;
            const frequency = hub.io.clock_frequency;
            // Hub edges still reset pin directions and service queued cog starts after self-stop.
            hub.step();
            if (hub.fault) |fault| {
                std.log.warn("P2AAS cog {d}, pc 0x{x}, instruction 0x{x:0>8}: {s}", .{ fault.cog, fault.pc, fault.instruction, fault.reason() });
                return error.SimulationFault;
            }
            if (hub.next_idle_clock()) |next| hub.counter = @min(next, ceiling);
            var pending_start = false;
            for (hub.pending_starts) |pending| if (pending != null) {
                pending_start = true;
            };
            // Stopping all cogs does not close the physical serial adapter. Idle clocks can skip,
            // but a queued COGINIT still has to run on its scheduled edge.
            if (!hub.is_any_cog_active() and !pending_start and !hub.io.pending()) hub.counter = ceiling;
            virtual_fixed += @as(u128, hub.counter -% before) * (@as(u128, std.time.ns_per_s) << 32) / frequency;
            if (hub.counter >= ceiling or output.end != 0) break;
        }
        const elapsed: u128 = @intCast(std.Io.Timestamp.now(connection.io, .awake).toNanoseconds() - started);
        const virtual_ns = virtual_fixed >> 32;
        if (virtual_ns > elapsed) try std.Io.sleep(connection.io, .fromNanoseconds(@intCast(virtual_ns - elapsed)), .awake);
        if (output.end != 0) {
            try connection.send(output.buffered(), .binary);
            output.end = 0;
        }
    }
}

/// Report a simulator failure through the same event path as socket termination and runtime timeout.
fn execution(connection: *Connection, hub: *Hub, input: *std.Io.Queue(u8), events: *std.Io.Queue(Event)) !void {
    simulate(connection, hub, input) catch |err| try events.putOne(connection.io, .{ .execution_error = err });
}

fn runtimeTimer(io: std.Io, events: *std.Io.Queue(Event), milliseconds: u32) !void {
    try std.Io.sleep(io, .fromMilliseconds(milliseconds), .awake);
    try events.putOne(io, .timeout);
}

/// Send the close frame and keep receiving until the peer replies. The caller bounds this to two seconds.
fn closeExchange(connection: *Connection, code: u16, reason: []const u8, events: ?*std.Io.Queue(Event)) !void {
    var payload: [125]u8 = undefined;
    std.mem.writeInt(u16, payload[0..2], code, .big);
    const length = @min(reason.len, 123);
    @memcpy(payload[2..][0..length], reason[0..length]);
    try connection.send(payload[0 .. length + 2], .connection_close);
    if (connection.peer_closed) return;
    if (events) |queue| {
        while (true) switch (try queue.getOne(connection.io)) {
            .peer, .receive_error => return,
            .execution_error => {},
            .timeout => {},
        };
    } else {
        while (true) {
            const chunk = try connection.readChunk(4096);
            if (chunk.opcode == .connection_close) return;
        }
    }
}

/// Bound both a blocked close write and waiting for the peer; unresponsive sockets are then released.
fn closeBounded(connection: *Connection, code: u16, reason: []const u8, events: ?*std.Io.Queue(Event)) !void {
    const Select = std.Io.Select(union(enum) { close: anyerror!void, timeout: std.Io.Cancelable!void });
    var buffer: [2]Select.Union = undefined;
    var select = Select.init(connection.io, &buffer);
    defer select.cancelDiscard();
    try select.concurrent(.close, closeExchange, .{ connection, code, reason, events });
    try select.concurrent(.timeout, std.Io.sleep, .{ connection.io, .fromSeconds(2), .awake });
    switch (try select.await()) {
        .close => |result| try result,
        .timeout => |result| try result,
    }
}

/// Validate the HTTP contract before upgrade, receive an image, and run a fresh simulated board.
fn process(connection: *Connection, http: *std.http.Server) !void {
    var arena: std.heap.ArenaAllocator = .init(connection.allocator);
    defer arena.deinit();
    var request = http.receiveHead() catch {
        if (connection.network_reader) |reader| if (reader.err) |read_error| if (read_error == error.Canceled) return error.Canceled;
        try http.out.writeAll("HTTP/1.1 400 Bad Request\r\nContent-Length: 0\r\nConnection: close\r\n\r\n");
        try http.out.flush();
        return;
    };
    const key = validateUpgrade(&request) catch {
        try request.respond("Expected a WebSocket upgrade.", .{ .status = .bad_request, .keep_alive = false });
        return;
    };
    const options = Options.parse(request.head.target) catch |err| {
        try request.respond(queryReason(err), .{ .status = .bad_request, .keep_alive = false, .extra_headers = &.{.{ .name = "Content-Type", .value = "text/plain; charset=utf-8" }} });
        return;
    };
    connection.socket = try request.respondWebSocket(.{ .key = key });
    try http.out.flush();
    connection.upgraded = true;
    const payload = if (options.code_len) |len| options.code[0..len] else blk: {
        var prefix: [4]u8 = undefined;
        connection.uploadRead(&prefix) catch |err| {
            if (err == error.Canceled) return err;
            connection.closing.store(true, .release);
            try closeBounded(connection, 1002, protocolReason(err), null);
            return;
        };
        const length = std.mem.readInt(u32, &prefix, .little);
        if (length == 0 or length > max_payload) {
            connection.closing.store(true, .release);
            try closeBounded(connection, 1002, if (length == 0) "Expected a non-empty payload." else "Payload exceeds 524288 bytes.", null);
            return;
        }
        const image = try arena.allocator().alloc(u8, length);
        connection.uploadRead(image) catch |err| {
            if (err == error.Canceled) return err;
            connection.closing.store(true, .release);
            try closeBounded(connection, 1002, protocolReason(err), null);
            return;
        };
        // The physical service validates word alignment only after reading the whole body.
        if (length % 4 != 0) {
            connection.closing.store(true, .release);
            try closeBounded(connection, 1002, "Payload length must be divisible by 4.", null);
            return;
        }
        break :blk image;
    };
    const hub = try arena.allocator().create(Hub);
    hub.init();
    @memcpy(hub.memory[0..payload.len], payload);
    // The loader stores its checksum complement as another long after the image.
    // At the 512 KiB boundary the next address is a hub hole, so it is not mirrored to address zero.
    var checksum: u32 = 0x706f7250;
    var words = std.mem.window(u8, payload, 4, 4);
    while (words.next()) |word| checksum -%= std.mem.readInt(u32, word[0..4], .little);
    hub.write_memory(@intCast(payload.len), checksum, 4, false);
    try hub.start_cog(0, .{});
    connection.running = true;
    var input_buffer: [65536]u8 = undefined;
    var input = std.Io.Queue(u8).init(&input_buffer);
    var event_buffer: [4]Event = undefined;
    var events = std.Io.Queue(Event).init(&event_buffer);
    var receiver = try std.Io.concurrent(connection.io, receive, .{ connection, &input, &events });
    defer receiver.cancel(connection.io) catch {};
    var worker = try std.Io.concurrent(connection.io, execution, .{ connection, hub, &input, &events });
    defer worker.cancel(connection.io) catch {};
    var timer = try std.Io.concurrent(connection.io, runtimeTimer, .{ connection.io, &events, options.timeout_ms });
    defer timer.cancel(connection.io) catch {};
    const event = try events.getOne(connection.io);
    connection.closing.store(true, .release);
    input.close(connection.io);
    // Finish every send before starting the close handshake; do not cancel the reader yet.
    worker.cancel(connection.io) catch {};
    switch (event) {
        .peer => {
            connection.peer_closed = true;
            try closeBounded(connection, 1000, "", null);
        },
        .timeout => try closeBounded(connection, 1008, "No time quota left for user code.", &events),
        .receive_error => |err| {
            std.log.warn("P2AAS session failed: {t}", .{err});
            receiver.await(connection.io) catch {};
            try closeBounded(connection, if (isProtocolError(err)) 1002 else 1011, if (isProtocolError(err)) protocolReason(err) else internal_reason, null);
        },
        .execution_error => try closeBounded(connection, 1011, internal_reason, &events),
    }
}

fn isProtocolError(err: anyerror) bool {
    return err == error.InvalidFrame or err == error.InvalidClose or err == error.InvalidText or err == error.NonBinaryUpload or err == error.TruncatedUpload;
}

fn protocolReason(err: anyerror) []const u8 {
    return switch (err) {
        error.NonBinaryUpload => "Expected binary websocket messages.",
        error.TruncatedUpload => "Connection closed before the full payload was received.",
        else => "Invalid WebSocket frame.",
    };
}

fn queryReason(err: anyerror) []const u8 {
    return switch (err) {
        error.DuplicateCode => "Query parameter 'code' must not appear more than once.",
        error.InvalidCode => "Query parameter 'code' must be valid base64 or base64url data.",
        error.PayloadTooLarge => "Query parameter 'code' must decode to at most 524288 bytes.",
        error.UnalignedPayload => "Query parameter 'code' must decode to a payload whose length is divisible by 4 bytes.",
        error.InvalidNumber => "Expected a positive integer baudrate or timeout_ms.",
        error.InvalidTimeout => "Query parameter 'timeout_ms' must be between 100 and 10000 milliseconds.",
        else => "Invalid query parameter.",
    };
}

/// Check required handshake fields explicitly; stdlib's upgrade helper assumes valid GET/HTTP/1.1.
fn validateUpgrade(request: *std.http.Server.Request) ![]const u8 {
    if (request.head.method != .GET or request.head.version != .@"HTTP/1.1" or request.head.expect != null) return error.InvalidUpgrade;
    var key: ?[]const u8 = null;
    var upgrade = false;
    var connection = false;
    var version = false;
    var headers = request.iterateHeaders();
    while (headers.next()) |header| {
        if (std.ascii.eqlIgnoreCase(header.name, "upgrade")) upgrade = std.ascii.eqlIgnoreCase(header.value, "websocket");
        if (std.ascii.eqlIgnoreCase(header.name, "connection")) {
            var tokens = std.mem.splitScalar(u8, header.value, ',');
            while (tokens.next()) |token| if (std.ascii.eqlIgnoreCase(std.mem.trim(u8, token, " \t"), "upgrade")) {
                connection = true;
            };
        }
        if (std.ascii.eqlIgnoreCase(header.name, "sec-websocket-version")) version = std.mem.eql(u8, header.value, "13");
        if (std.ascii.eqlIgnoreCase(header.name, "sec-websocket-key")) key = header.value;
    }
    if (!upgrade or !connection or !version) return error.InvalidUpgrade;
    const nonce = key orelse return error.InvalidUpgrade;
    if ((std.base64.standard.Decoder.calcSizeForSlice(nonce) catch return error.InvalidUpgrade) != 16) return error.InvalidUpgrade;
    var decoded: [16]u8 = undefined;
    std.base64.standard.Decoder.decode(&decoded, nonce) catch return error.InvalidUpgrade;
    return nonce;
}

/// Handle one TCP connection with the hardware server's ten-second total request deadline.
pub fn handleConnection(allocator: std.mem.Allocator, io: std.Io, stream: std.Io.net.Stream, trace: bool) !void {
    const read_buffer = try allocator.alloc(u8, max_request_head); // URL code may carry a complete base64 image.
    defer allocator.free(read_buffer);
    var write_buffer: [4096]u8 = undefined;
    var reader = stream.reader(io, read_buffer);
    var writer = stream.writer(io, &write_buffer);
    var http = std.http.Server.init(&reader.interface, &writer.interface);
    var connection: Connection = .{
        .allocator = allocator,
        .io = io,
        .network_reader = &reader,
        .frames = .{ .input = &reader.interface },
        .socket = .{ .input = &reader.interface, .output = &writer.interface, .key = "" },
        .trace = trace,
    };
    const Select = std.Io.Select(union(enum) { request: anyerror!void, timeout: std.Io.Cancelable!void });
    var buffer: [2]Select.Union = undefined;
    var select = Select.init(io, &buffer);
    defer select.cancelDiscard();
    try select.concurrent(.request, process, .{ &connection, &http });
    try select.concurrent(.timeout, std.Io.sleep, .{ io, .fromSeconds(10), .awake });
    switch (try select.await()) {
        .request => |result| result catch |err| {
            // Allocation and task-start failures after upgrade also need a protocol-visible close.
            if (connection.upgraded and !connection.closing.load(.acquire)) {
                connection.closing.store(true, .release);
                try closeBounded(&connection, 1011, internal_reason, null);
            }
            return err;
        },
        .timeout => |result| {
            try result;
            select.cancelDiscard();
            if (connection.upgraded) {
                connection.closing.store(true, .release);
                try closeBounded(&connection, if (connection.running) 1008 else 1011, if (connection.running) "No time quota left." else internal_reason, null);
            }
        },
    }
}

/// Bind a ws:// listener. Sessions are sequential, matching the physical service's one-board ownership.
pub fn serve(allocator: std.mem.Allocator, io: std.Io, address: []const u8, trace: bool) !void {
    var arena: std.heap.ArenaAllocator = .init(allocator);
    defer arena.deinit();
    const uri = try std.Uri.parse(address);
    if (!std.ascii.eqlIgnoreCase(uri.scheme, "ws") or uri.host == null or uri.user != null or uri.password != null or uri.query != null or uri.fragment != null) return error.InvalidListenUrl;
    const path = try uri.path.toRawMaybeAlloc(arena.allocator());
    if (!std.mem.eql(u8, path, "/") and path.len != 0) return error.InvalidListenUrl;
    const host = try uri.host.?.toRawMaybeAlloc(arena.allocator());
    // URI hosts retain IPv6 brackets; the networking API takes the bare address.
    const bare_host = if (std.mem.startsWith(u8, host, "[") and std.mem.endsWith(u8, host, "]")) host[1 .. host.len - 1] else host;
    const ip = try std.Io.net.IpAddress.resolve(io, if (std.ascii.eqlIgnoreCase(bare_host, "localhost")) "127.0.0.1" else bare_host, uri.port orelse 80);
    var listener = try ip.listen(io, .{ .reuse_address = true });
    defer listener.deinit(io);
    std.log.info("P2AAS listening at {s}", .{address});
    while (true) {
        const stream = try listener.accept(io);
        defer stream.close(io);
        handleConnection(allocator, io, stream, trace) catch |err| {
            try io.checkCancel();
            std.log.warn("P2AAS connection ended: {t}", .{err});
        };
    }
}

// Query and frame tests run without networking; the harness also exercises real TCP sessions.
test "P2AAS query matrix: defaults, URL uploads and validation before upgrade" {
    const defaults = try Options.parse("/");
    try std.testing.expectEqual(@as(u32, 115200), defaults.baudrate);
    try std.testing.expectEqual(@as(u32, 2500), defaults.timeout_ms);
    try std.testing.expect(defaults.code_len == null);
    try std.testing.expectEqual(@as(usize, 0), (try Options.parse("/?code=")).code_len.?);
    for ([_][]const u8{ "/?code=AAAAAA==", "/?code=AAAAAA", "/?code=__8AAA", "/?code=%2F%2F8AAA%3D%3D", "/?c%6fde=AA+AA%09AA%3D%3D" }) |target| {
        try std.testing.expectEqual(@as(usize, 4), (try Options.parse(target)).code_len.?);
    }
    const selected = try Options.parse("/?baudrate=%2B230400&timeout_ms=100&unused=ok");
    try std.testing.expectEqual(@as(u32, 230400), selected.baudrate);
    try std.testing.expectEqual(@as(u32, 100), selected.timeout_ms);
    try std.testing.expectEqual(@as(u32, 115200), (try Options.parse("/?baudrate=+&timeout_ms=")).baudrate);
    inline for (.{
        .{ "/?code=&code=", error.DuplicateCode },
        .{ "/?code=!", error.InvalidCode },
        .{ "/?code=AA==", error.UnalignedPayload },
        .{ "/?baudrate=0", error.InvalidNumber },
        .{ "/?baudrate=2147483648", error.InvalidNumber },
        .{ "/?baudrate=115_200", error.InvalidNumber },
        .{ "/?timeout_ms=1_000", error.InvalidNumber },
        .{ "/?baudrate=1&baudrate=2", error.InvalidNumber },
        .{ "/?timeout_ms=99", error.InvalidTimeout },
        .{ "/?timeout_ms=10001", error.InvalidTimeout },
        .{ "/?code=%xx", error.InvalidQuery },
        .{ "/?code=%", error.InvalidQuery },
        .{ "/?code=%0", error.InvalidQuery },
    }) |case| try std.testing.expectError(case[1], Options.parse(case[0]));
}

test "P2AAS URL code fits its fixed buffer at 512 KiB and rejects larger images" {
    const allocator = std.testing.allocator;
    const image = try allocator.alloc(u8, max_payload + 4);
    defer allocator.free(image);
    @memset(image, 0xff);
    const encoder = std.base64.url_safe_no_pad.Encoder;
    const prefix = "/?code=";
    const query = try allocator.alloc(u8, prefix.len + encoder.calcSize(image.len));
    defer allocator.free(query);
    @memcpy(query[0..prefix.len], prefix);
    const encoded = encoder.encode(query[prefix.len..], image[0..max_payload]);
    const options = try Options.parse(query[0 .. prefix.len + encoded.len]);
    try std.testing.expectEqual(max_payload, options.code_len.?);
    try std.testing.expectEqualSlices(u8, image[0..max_payload], options.code[0..options.code_len.?]);
    _ = encoder.encode(query[prefix.len..], image);
    try std.testing.expectError(error.PayloadTooLarge, Options.parse(query));
}

/// Build masked test frames with stdlib's encoder; a fixed mask makes boundary errors reproducible.
fn testFrame(writer: *std.Io.Writer, opcode: WebSocket.Opcode, fin: bool, bytes: []const u8) !void {
    var encoded: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer encoded.deinit();
    var input = std.Io.Reader.fixed("");
    var ws: WebSocket = .{ .input = &input, .output = &encoded.writer, .key = "" };
    try ws.writeMessageUnflushed(bytes, opcode);
    const frame = encoded.written();
    frame[0] = (frame[0] & 0x7f) | (@as(u8, @intFromBool(fin)) << 7);
    frame[1] |= 0x80;
    const header: usize = if (bytes.len < 126) 2 else if (bytes.len < 65536) 4 else 10;
    try writer.writeAll(frame[0..header]);
    const mask = [_]u8{ 1, 2, 3, 4 };
    try writer.writeAll(&mask);
    for (frame[header..], 0..) |byte, index| try writer.writeByte(byte ^ mask[index % 4]);
}

test "streaming frames preserve upload boundary, continuations, controls and large masked frames" {
    var wire: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer wire.deinit();
    try testFrame(&wire.writer, .binary, false, "\x04\x00");
    try testFrame(&wire.writer, .ping, true, "p");
    try testFrame(&wire.writer, .continuation, true, "\x00\x00CODEinput");
    var reader = std.Io.Reader.fixed(wire.written());
    var frames: Frames = .{ .input = &reader };
    try std.testing.expectEqualStrings("\x04\x00", (try frames.next(4)).data);
    const ping = try frames.next(2);
    try std.testing.expectEqual(.ping, ping.opcode);
    try std.testing.expectEqualStrings("p", ping.data);
    try std.testing.expectEqualStrings("\x00\x00", (try frames.next(2)).data);
    try std.testing.expectEqualStrings("CODE", (try frames.next(4)).data);
    try std.testing.expectEqualStrings("input", (try frames.next(4096)).data);
    for ([_]usize{ 0, 125, 126, 65535, 65536, max_payload }) |length| {
        wire.clearRetainingCapacity();
        const bytes = try std.testing.allocator.alloc(u8, length);
        defer std.testing.allocator.free(bytes);
        for (bytes, 0..) |*byte, i| byte.* = @truncate(i);
        try testFrame(&wire.writer, .binary, true, bytes);
        reader = .fixed(wire.written());
        frames = .{ .input = &reader };
        var offset: usize = 0;
        while (true) {
            const chunk = try frames.next(4096);
            try std.testing.expectEqualSlices(u8, bytes[offset..][0..chunk.data.len], chunk.data);
            offset += chunk.data.len;
            if (frames.remaining == 0) break;
        }
        try std.testing.expectEqual(length, offset);
    }
}

test "fragmented UTF-8 terminal text and invalid client frames" {
    var wire: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer wire.deinit();
    try testFrame(&wire.writer, .text, false, "\xe2");
    try testFrame(&wire.writer, .ping, true, "");
    try testFrame(&wire.writer, .continuation, true, "\x82\xac");
    var reader = std.Io.Reader.fixed(wire.written());
    var frames: Frames = .{ .input = &reader };
    _ = try frames.next(4096);
    _ = try frames.next(4096);
    _ = try frames.next(4096);
    inline for (.{
        .{ "\x82\x00", error.InvalidFrame },
        .{ "\x80\x80", error.InvalidFrame },
        .{ "\x89\xfe\x00\x7e", error.InvalidFrame },
        .{ "\x82\xfe\x00\x01", error.InvalidFrame },
        .{ "\x83\x80", error.InvalidFrame },
    }) |case| {
        reader = .fixed(case[0]);
        frames = .{ .input = &reader };
        try std.testing.expectError(case[1], frames.next(4096));
    }
    inline for (.{
        .{ WebSocket.Opcode.text, "\xc0\x80", error.InvalidText },
        .{ WebSocket.Opcode.text, "\xed\xa0\x80", error.InvalidText },
        .{ WebSocket.Opcode.text, "\xe2", error.InvalidText },
        .{ WebSocket.Opcode.connection_close, "x", error.InvalidClose },
        .{ WebSocket.Opcode.connection_close, "\x03\xed", error.InvalidClose },
    }) |case| {
        wire.clearRetainingCapacity();
        try testFrame(&wire.writer, case[0], true, case[1]);
        reader = .fixed(wire.written());
        frames = .{ .input = &reader };
        try std.testing.expectError(case[2], frames.next(4096));
    }
}
