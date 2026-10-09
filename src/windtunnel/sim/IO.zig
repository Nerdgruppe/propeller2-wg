//! Functional terminal peripherals: eight-bit UART TX on pin 62 and RX on pin 63.
//! This models frame completion and buffer readiness, not general smart-pin or electrical behavior.

const Hub = @import("Hub.zig");
const IO = @This();

// Functional terminal model: 8N1 UART on the board's TX=62 and RX=63.
// Smart-pin IN on TX means buffer available, not transmission complete.
pub const Pin = struct {
    mode: u32 = 0,
    x: u32 = 0,
    enabled: bool = false,
    ready: bool = false,
    result: u32 = 0,
};
pins: [64]Pin = @splat(.{}),
tx_buffer: ?u8 = null,
tx_shift: ?u8 = null,
tx_end: u64 = 0,
input: []const u8 = &.{},
input_index: usize = 0,
rx_end: u64 = 0,
clock_mode: u32 = 0,
/// Nominal board clocks; RC oscillator drift and PLL settling are not modeled.
clock_frequency: u64 = 20_000_000,
crystal_frequency: u64 = 20_000_000,
/// External serial adapter speed, independent of the firmware's WXPIN period.
host_baudrate: ?u32 = null,
rx_queue: [4096]u8 = undefined,
rx_head: usize = 0,
rx_count: usize = 0,
directions: u64 = 0,

/// Apply the documented HUBSET clock fields for RCFAST, RCSLOW, XI and PLL.
pub fn setClock(io: *IO, mode: u32) bool {
    const config: @import("p2").types.ClockMode = @bitCast(mode);
    io.clock_frequency = config.get_frequency(io.crystal_frequency) catch return false;
    io.clock_mode = mode;
    return true;
}

/// Host-side bytes keep their order and the active frame when another chunk arrives.
/// Unlike supplyInput(), this accepts bytes before RX is enabled; those frames are discarded.
pub fn queueInput(io: *IO, bytes: []const u8, counter: u64) bool {
    if (bytes.len > io.rx_queue.len - io.rx_count) return false;
    if (io.rx_count == 0 and bytes.len != 0) io.rx_end = counter +% io.hostFrameClocks();
    for (bytes) |byte| {
        io.rx_queue[(io.rx_head + io.rx_count) % io.rx_queue.len] = byte;
        io.rx_count += 1;
    }
    return true;
}

/// Round an external 8N1 frame up to whole system clocks.
fn hostFrameClocks(io: *const IO) u64 {
    const baudrate = io.host_baudrate orelse return frameClocks(io.pins[63]);
    return (io.clock_frequency * 10 + baudrate - 1) / baudrate;
}

/// Each pin's DIR is the OR of the direction bits contributed by all cogs.
pub fn updateDirections(io: *IO, hub: *const Hub) void {
    var directions: u64 = 0;
    for (&hub.cogs) |*cog| {
        directions |= @as(u64, cog.registers.get(.DIRA)) | (@as(u64, cog.registers.get(.DIRB)) << 32);
    }
    if (directions == io.directions) return;
    io.directions = directions;
    for (&io.pins, 0..) |*pin, index| {
        const enabled = directions & (@as(u64, 1) << @intCast(index)) != 0;
        if (pin.enabled and !enabled) {
            pin.ready = false;
            pin.result = 0;
            if (index == 62) {
                io.tx_buffer = null;
                io.tx_shift = null;
            }
        }
        pin.enabled = enabled;
    }
}

/// Convert the UART fixed-point bit period into a rounded-up ten-bit 8N1 frame duration.
pub fn frameClocks(pin: Pin) u64 {
    const fixed: u64 = if (pin.x >> 26 == 0) pin.x & 0xffff_fc00 else pin.x & 0xffff_0000;
    return (fixed * 10 + 0xffff) >> 16;
}

/// Check direction enable, eight-bit framing and a nonzero bit period for the limited UART model.
pub fn uartConfigured(pin: Pin) bool {
    return pin.enabled and pin.x & 31 == 7 and frameClocks(pin) > 0;
}

/// Schedule an external RX byte stream starting one frame after the given counter value.
pub fn supplyInput(io: *IO, bytes: []const u8, counter: u64) !void {
    if (io.pins[63].mode != 0x3e or !uartConfigured(io.pins[63])) return error.UartRxNotReady;
    io.input = bytes;
    io.input_index = 0;
    io.rx_end = counter +% frameClocks(io.pins[63]);
}

/// Replace the UART TX holding byte and clear buffer-ready; return false for unsupported configuration.
pub fn transmit(io: *IO, value: u32) bool {
    if (io.pins[62].mode != 0x7c or !uartConfigured(io.pins[62])) return false;
    io.tx_buffer = @truncate(value);
    io.pins[62].ready = false;
    return true;
}

/// Report whether either the UART holding register or shifting frame still contains data.
pub fn txBusy(io: *const IO) bool {
    return io.tx_shift != null or io.tx_buffer != null;
}

/// Complete UART frames at their scheduled clocks, refill TX, and deliver or discard external RX bytes.
pub fn step(io: *IO, hub: *Hub) void {
    if (io.tx_shift != null and Hub.clock_reached(hub.counter, io.tx_end)) {
        if (hub.output_writer) |writer| writer.writeByte(io.tx_shift.?) catch {
            hub.fault = .{ .cog = 0, .pc = hub.cogs[0].pc, .instruction = 0, .result = .trap };
        };
        io.tx_shift = null;
    }
    if (io.tx_shift == null) if (io.tx_buffer) |byte| {
        io.tx_shift = byte;
        io.tx_buffer = null;
        io.tx_end = hub.counter +% frameClocks(io.pins[62]);
        io.pins[62].ready = true;
    };
    if (io.input_index < io.input.len and Hub.clock_reached(hub.counter, io.rx_end)) {
        if (io.pins[63].enabled) {
            io.pins[63].result = @as(u32, io.input[io.input_index]) << 24;
            io.pins[63].ready = true;
        }
        io.input_index += 1;
        io.rx_end +%= frameClocks(io.pins[63]);
    }
    if (io.rx_count != 0 and Hub.clock_reached(hub.counter, io.rx_end)) {
        if (io.pins[63].mode == 0x3e and uartConfigured(io.pins[63])) {
            io.pins[63].result = @as(u32, io.rx_queue[io.rx_head]) << 24;
            io.pins[63].ready = true;
        }
        io.rx_head = (io.rx_head + 1) % io.rx_queue.len;
        io.rx_count -= 1;
        io.rx_end +%= io.hostFrameClocks();
    }
}

/// Expose smart-pin ready bits on INA/INB; TX ready denotes buffer space rather than wire completion.
pub fn get_in(io: *IO) u64 {
    return (@as(u64, @intFromBool(io.pins[62].ready)) << 62) | (@as(u64, @intFromBool(io.pins[63].ready)) << 63);
}

/// Placeholder for ordinary pin-output effects; the current terminal model uses smart-pin UART state.
pub fn update_out(io: *IO) void {
    _ = io;
}

test "clock fields and queued host UART frames remain independent of firmware RX readiness" {
    const std = @import("std");
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);
    hub.init();
    const io = &hub.io;
    try std.testing.expect(io.setClock(0x0100_09fb));
    try std.testing.expectEqual(@as(u64, 200_000_000), io.clock_frequency);
    try std.testing.expect(!io.setClock(3));
    try std.testing.expectEqual(@as(u64, 200_000_000), io.clock_frequency);
    try std.testing.expectEqual(@as(u32, 0x0100_09fb), io.clock_mode);
    _ = io.setClock(0xf0);
    io.host_baudrate = 100_000;
    try std.testing.expect(io.queueInput("ab", 0));
    hub.counter = 1999;
    io.step(hub);
    try std.testing.expectEqual(@as(usize, 2), io.rx_count);
    try std.testing.expect(io.queueInput("c", hub.counter));
    try std.testing.expectEqual(@as(u64, 2000), io.rx_end);
    hub.counter = 2000;
    io.step(hub); // 'a' arrived while RX was disabled, so it must not reappear later.
    try std.testing.expectEqual(@as(usize, 2), io.rx_count);
    io.pins[63] = .{ .enabled = true, .mode = 0x3e, .x = (200 << 16) | 7 };
    hub.counter = 4000;
    io.step(hub);
    try std.testing.expectEqual(@as(u32, 'b') << 24, io.pins[63].result);
    io.pins[63].ready = false;
    hub.counter = 6000;
    io.step(hub);
    try std.testing.expectEqual(@as(u32, 'c') << 24, io.pins[63].result);
    try std.testing.expectEqual(@as(usize, 0), io.rx_count);
    try std.testing.expect(!io.queueInput(&@as([4097]u8, @splat(0)), hub.counter));
}
