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
directions: u64 = 0,

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
}

/// Expose smart-pin ready bits on INA/INB; TX ready denotes buffer space rather than wire completion.
pub fn get_in(io: *IO) u64 {
    return (@as(u64, @intFromBool(io.pins[62].ready)) << 62) | (@as(u64, @intFromBool(io.pins[63].ready)) << 63);
}

/// Placeholder for ordinary pin-output effects; the current terminal model uses smart-pin UART state.
pub fn update_out(io: *IO) void {
    _ = io;
}
