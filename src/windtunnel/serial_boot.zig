//! Physical serial ROM boot and the nominal handoff used by direct-image runners.
const std = @import("std");
const Hub = @import("sim/Hub.zig");
const smart_pin = @import("sim/smart_pin.zig");

pub const baudrate: u32 = 2_000_000;

/// Prepare 8N1 RX/TX dividers for the configured initial RCFAST sample.
/// Real ROM boot calibrates these from the upload waveform instead, retaining them across handoff.
pub fn prepare(hub: *Hub, rate: u32) void {
    const configured = hub.io.setClock(0xf0);
    std.debug.assert(configured);
    std.debug.assert(rate != 0);
    const ticks: u64 = @intFromFloat(hub.io.clock.frequency() * 65536 / @as(f64, @floatFromInt(rate)));
    const divider: u32 = @intCast(ticks & 0xffff_ff00 | 7);
    hub.io.pins[62].smart.registers.x = divider;
    hub.io.pins[63].smart.registers.x = divider;
}

/// Start the recovered ROM with a 10 kOhm P59 pullup selecting serial boot and an idle P63 host.
/// Initialization remains explicit; this does not reset RAM or override other external boot straps.
pub fn start(hub: *Hub) !void {
    if (hub.is_any_cog_active())
        return error.CogsActive;
    try hub.io.setExternal(@enumFromInt(59), .{
        .voltage = hub.system_configuration.vio_voltage,
        .resistance = 10_000,
    });
    try hub.io.setExternal(@enumFromInt(63), .{
        .voltage = hub.system_configuration.vio_voltage,
        .resistance = 1,
    });
    try hub.start_cog(0, .{ .hub_address = 0xfc000 });
}

/// Fixed-baud external 8N1 host; drives the physical P63 pad so the ROM can measure autobaud.
pub const Host = struct {
    io: std.Io,
    rate: u32,
    next_bit_seconds: f64 = 0,

    /// Send complete UART frames, carrying fractional system clocks across bits and calls.
    pub fn write(host: *Host, hub: *Hub, bytes: []const u8) !void {
        if (host.rate == 0)
            return error.InvalidBaudrate;
        host.next_bit_seconds = @max(host.next_bit_seconds, hub.io.clock.seconds);
        for (bytes) |byte| {
            const frame = (@as(u10, byte) << 1) | 0x200;
            for (0..10) |bit| {
                const high = frame & (@as(u10, 1) << @intCast(bit)) != 0;
                try hub.io.setExternal(@enumFromInt(63), .{
                    .voltage = if (high)
                        hub.system_configuration.vio_voltage
                    else
                        0,
                    .resistance = 1,
                });
                host.next_bit_seconds += 1 / @as(f64, @floatFromInt(host.rate));
                while (hub.io.clock.seconds < host.next_bit_seconds) {
                    try host.io.checkCancel();
                    if (hub.step() != .normal)
                        return error.BootInterrupted;
                }
            }
        }
    }
};

/// Advance a bounded interval without hiding simulator faults, breakpoints or resets.
fn advance(io: std.Io, hub: *Hub, clocks: u64) !void {
    for (0..clocks) |index| {
        if (index % 256 == 0) {
            try io.checkCancel();
        }
        switch (hub.step()) {
            .normal => {},
            .trap => return error.BootTrap,
            .breakpoint => return error.BootBreakpoint,
            .reset => return error.BootReset,
        }
    }
}

/// Wait for a complete ROM reply and reject checksum errors or unexpected firmware responses.
fn reply(io: std.Io, hub: *Hub, output: *std.Io.Writer, expected: []const u8, rate: u32) !void {
    const timeout: u64 = @intFromFloat(
        hub.io.clock.frequency() * (0.01 + 256 / @as(f64, @floatFromInt(rate))),
    );
    for (0..timeout) |_| {
        if (output.buffered().len >= expected.len) {
            if (!std.mem.eql(u8, expected, output.buffered()))
                return error.UnexpectedBootReply;
            return;
        }
        try advance(io, hub, 1);
    }
    return error.BootTimeout;
}

/// Download through the unmodified ROM: autobaud, Prop_Chk, checksummed Prop_Txt, then cog-zero launch.
/// The host stays idle-high on P63. The temporary P59 pullup and ROM reply sink are restored on return.
/// Keep image outside hub RAM: the ROM overwrites its first 256 bytes during initialization.
pub fn load(io: std.Io, hub: *Hub, image: []const u8, rate: u32) !void {
    if (image.len == 0 or image.len % 4 != 0 or image.len > hub.memory.len - 4)
        return error.InvalidBootImage;
    if (rate < 9_600 or rate > 2_000_000)
        return error.InvalidBaudrate;

    const prior_strap = hub.io.external[59];
    const prior_tx = hub.io.pins[62].smart.tx_mode;
    defer {
        hub.io.setExternal(@enumFromInt(59), prior_strap) catch unreachable;
        hub.io.pins[62].smart.tx_mode = prior_tx;
    }

    var response_buffer: [32]u8 = undefined;
    var response = std.Io.Writer.fixed(&response_buffer);
    var sink: smart_pin.DataSink = .{ .writer = &response };
    hub.io.pins[62].smart.tx_mode = .{ .emulated = &sink };
    try start(hub);
    // Match the hardware loader's 20 ms reset-to-autobaud allowance, in simulated time.
    try advance(io, hub, @intFromFloat(hub.io.clock.frequency() / 50));
    var host: Host = .{ .io = io, .rate = rate };
    try host.write(hub, "> Prop_Chk 0 0 0 0\r");
    try reply(io, hub, &response, "\r\nProp_Ver G\r\n", rate);
    response.end = 0;

    var checksum: u32 = 0x706f7250;
    var words = std.mem.window(u8, image, 4, 4);
    while (words.next()) |word| {
        checksum -%= std.mem.readInt(u32, word[0..4], .little);
    }
    var checksum_bytes: [4]u8 = undefined;
    std.mem.writeInt(u32, &checksum_bytes, checksum, .little);
    try host.write(hub, "Prop_Txt 0 0 0 0 ");
    var offset: usize = 0;
    while (offset < image.len + checksum_bytes.len) {
        var block: [3]u8 = undefined;
        const count = @min(block.len, image.len + checksum_bytes.len - offset);
        for (block[0..count], offset..) |*byte, index| {
            byte.* = if (index < image.len)
                image[index]
            else
                checksum_bytes[index - image.len];
        }
        var encoded: [4]u8 = undefined;
        try host.write(hub, std.base64.standard_no_pad.Encoder.encode(&encoded, block[0..count]));
        offset += count;
    }
    try host.write(hub, "?");
    try reply(io, hub, &response, ".", rate);
}

test "serial boot ROM identifies itself, rejects a checksum and retries a hex download" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    var output_buffer: [32]u8 = undefined;
    var output = std.Io.Writer.fixed(&output_buffer);
    var sink: smart_pin.DataSink = .{ .writer = &output };
    hub.io.pins[62].smart.tx_mode = .{ .emulated = &sink };
    try start(hub);
    try advance(std.testing.io, hub, 32_000);
    var host: Host = .{ .io = std.testing.io, .rate = 115_200 };
    try host.write(hub, "> Prop_Chk 0 0 0 0\r");
    try reply(std.testing.io, hub, &output, "\r\nProp_Ver G\r\n", host.rate);
    try std.testing.expect(hub.io.pins[63].smart.registers.x != 0);
    try std.testing.expectEqual(hub.io.pins[62].smart.registers.x, hub.io.pins[63].smart.registers.x);

    output.end = 0;
    try host.write(hub, "Prop_Hex 0 0 0 0 00 00 00 00 ?");
    try reply(std.testing.io, hub, &output, "!", host.rate);
    try std.testing.expect(hub.is_any_cog_active());

    // COGSTOP #0 ($FD640003), followed by the little-endian checksum to sum to "Prop".
    output.end = 0;
    try host.write(hub, "Prop_Hex 0 0 0 0 03 00 64 FD 4D 72 0B 73 ?");
    try reply(std.testing.io, hub, &output, ".", host.rate);
    try advance(std.testing.io, hub, 2048);
    try std.testing.expect(!hub.is_any_cog_active());
    try std.testing.expectEqual(@as(u32, 0xfd640003), hub.read_memory(0, 4));
    try std.testing.expectEqual(smart_pin.Mode.gpio, hub.io.pins[63].configuration.mode);
    try std.testing.expectEqual(smart_pin.Mode.gpio, hub.io.pins[62].configuration.mode);
}

test "serial ROM loader rejects invalid input before running the chip" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    try std.testing.expectError(error.InvalidBootImage, load(std.testing.io, hub, "", 115_200));
    try std.testing.expectError(error.InvalidBootImage, load(std.testing.io, hub, "123", 115_200));
    try std.testing.expectError(error.InvalidBootImage, load(std.testing.io, hub, &hub.memory, 115_200));
    try std.testing.expectError(error.InvalidBaudrate, load(std.testing.io, hub, "1234", 0));
    try std.testing.expectError(error.InvalidBaudrate, load(std.testing.io, hub, "1234", 2_000_001));
    try std.testing.expectEqual(@as(u64, 0), hub.counter);
    try std.testing.expect(!hub.is_any_cog_active());
}

test "serial boot ROM enters TAQOZ" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    var output_buffer: [8192]u8 = undefined;
    var output = std.Io.Writer.fixed(&output_buffer);
    errdefer std.debug.print("TAQOZ output: {s}\n", .{output.buffered()});
    var sink: smart_pin.DataSink = .{ .writer = &output };
    hub.io.pins[62].smart.tx_mode = .{ .emulated = &sink };
    try start(hub);
    try advance(std.testing.io, hub, 32_000);
    var host: Host = .{ .io = std.testing.io, .rate = 115_200 };
    try host.write(hub, "> Prop_Chk 0 0 0 0\r");
    try reply(std.testing.io, hub, &output, "\r\nProp_Ver G\r\n", host.rate);
    output.end = 0;
    try host.write(hub, "\x1b");
    for (0..3_000_000) |_| {
        try std.testing.expectEqual(Hub.StepResult.normal, hub.step());
        if (std.mem.find(u8, output.buffered(), "TAQOZ# ") != null)
            break;
    }
    try std.testing.expect(std.mem.find(u8, output.buffered(), "TAQOZ# ") != null);
    output.end = 0;
    try host.write(hub, "1 2 + .\r");
    for (0..2_000_000) |_| {
        try std.testing.expectEqual(Hub.StepResult.normal, hub.step());
        if (std.mem.find(u8, output.buffered(), "TAQOZ# ") != null)
            break;
    }
    try std.testing.expect(std.mem.find(u8, output.buffered(), "3") != null);
    try std.testing.expect(std.mem.find(u8, output.buffered(), "TAQOZ# ") != null);
}
