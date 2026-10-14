//! Cog-local digital streamer, DAC overrides and TMDS serializer. No analog signal generation.
const std = @import("std");
const Cog = @import("Cog.zig");
const Debugger = @import("Debugger.zig");
const pipeline = @import("pipeline.zig");
const Streamer = @This();

pub const Start = enum { immediate, zero, continuous };
pub const Source = enum { immediate, fifo, pins };
pub const Pixel = enum { none, luma8, rgbi8, rgb8, rgb16, rgb24 };
pub const Video = enum(u2) { normal, normal_reverse, forward, reverse };

/// Decoded data format; widths describe arithmetic bit counts, not mode IDs.
pub const Format = struct {
    source: Source,
    bits: u6,
    channel_bits: u4,
    lut: bool,
    alternate: bool,
    pixel: Pixel,
};

/// Captured instruction operands survive command buffering and execute-stage stalls.
pub const Command = struct {
    control: u32,
    data: u32,
    frequency: ?u32,
    start: Start,
    format: Format,
    provenance: Debugger.Provenance,

    /// Decode every digital mode, distinguishing excluded hardware from reserved encodings.
    pub fn init(control: u32, data: u32, frequency: ?u32, start: Start, provenance: Debugger.Provenance) error{ UnsupportedMode, ReservedMode }!Command {
        const major: u4 = @truncate(control >> 28);
        const minor: u4 = @truncate(control >> 16);
        var format: Format = .{
            .source = .immediate,
            .bits = 0,
            .channel_bits = 0,
            .lut = false,
            .alternate = false,
            .pixel = .none,
        };
        if (major <= 3) {
            format.lut = true;
            format.bits = @as(u6, 1) << @as(u2, @intCast(major));
        } else {
            format.source = if (major >= 12) .pins else if (major >= 8) .fifo else .immediate;
            const group = if (major >= 12) major - 8 else if (major >= 8) major - 4 else major;
            switch (group) {
                4 => {
                    format.bits = 1;
                    format.channel_bits = 1;
                    format.alternate = minor & 1 != 0;
                },
                5 => {
                    format.bits = 2;
                    format.channel_bits = if (minor & 2 != 0) 2 else 1;
                    format.alternate = minor & 1 != 0;
                },
                6 => {
                    switch (minor) {
                        0...5 => {
                            format.bits = 4;
                            format.channel_bits = @as(u4, 1) << @as(u2, @intCast(minor >> 1));
                            format.alternate = minor & 1 != 0;
                        },
                        6 => {
                            format.bits = 8;
                            format.channel_bits = 2;
                        },
                        7 => {
                            format.bits = 8;
                            format.channel_bits = 4;
                        },
                        14 => {
                            format.bits = 8;
                            format.channel_bits = 8;
                        },
                        15 => {
                            format.bits = 16;
                            format.channel_bits = 4;
                        },
                        else => return error.ReservedMode,
                    }
                },
                7 => switch (minor) {
                    0, 1 => {
                        format.bits = if (minor == 0) 16 else 32;
                        format.channel_bits = 8;
                    },
                    2...9 => {
                        if (major == 15)
                            return error.UnsupportedMode;
                        if (major == 7) {
                            format.source = .fifo;
                            format.lut = true;
                            format.bits = @as(u6, 1) << @as(u2, @intCast((minor - 2) >> 1));
                            format.alternate = minor & 1 != 0;
                        } else if (minor <= 6) {
                            format.pixel = switch (minor) {
                                2 => .luma8,
                                3 => .rgbi8,
                                4 => .rgb8,
                                5 => .rgb16,
                                6 => .rgb24,
                                else => unreachable,
                            };
                            format.bits = if (minor == 5) 16 else if (minor == 6) 32 else 8;
                        } else {
                            return error.ReservedMode;
                        }
                    },
                    else => return error.ReservedMode,
                },
                else => unreachable,
            }
        }
        return .{
            .control = control,
            .data = data,
            .frequency = frequency,
            .start = start,
            .format = format,
            .provenance = provenance,
        };
    }

    /// Return the aligned starting pin within the circular 64-pin bus.
    pub fn pinBase(command: Command) u6 {
        const width = if (command.format.lut or command.format.pixel != .none) 32 else command.format.bits;
        const alignment: u6 = if (width >= 8) 7 else width - 1;
        return @as(u6, @truncate(command.control >> 17)) & ~alignment;
    }

    /// Identify FIFO ownership, including capture commands with recording disabled.
    pub fn fifoMode(command: Command) ?Cog.FifoMode {
        if (@as(u16, @truncate(command.control)) == 0)
            return null;
        return switch (command.format.source) {
            .immediate => null,
            .fifo => .read,
            .pins => if (command.control & (1 << 23) != 0) .write else null,
        };
    }
};

pub const Action = union(enum) {
    command: Command,
    frequency: u32,
    dacs: u32,
};

pub const Active = struct {
    command: Command,
    remaining: u16,
    shift: u32,
    samples_left: u6,
    capture: u32,
    capture_bits: u4,
    first_clock: bool,
};

frequency: u32 = 0x8000_0000,
phase: u32 = 0,
active: ?Active = null,
pending: ?Command = null,
issued: ?struct { command: Command, at: u64 } = null,
background: [4]u8 = @splat(0),
channels: [4]u8 = @splat(0),
pins: u64 = 0,
pin_data: u64 = 0,
pin_pipeline: pipeline.Pipeline(u64, 2) = .init(0),
video_pipeline: pipeline.Pipeline(?u64, 2) = .init(null),
/// Companion writes own the shared LUT port on this edge.
lut_write: ?u9 = null,
lut_value: u32 = 0,
tmds: Tmds = .{},
/// Flush the longest output path after stop without clocking eight idle engines forever.
drain_clocks: u3 = 0,

/// Commit an instruction effect at the same shared boundary as other cog writebacks.
pub fn apply(streamer: *Streamer, cog: *Cog, action: Action) void {
    switch (action) {
        .frequency => |value| streamer.frequency = value,
        .dacs => |value| {
            streamer.background = dacBytes(value);
            streamer.publishDacs(cog);
        },
        .command => |command| {
            cog.events.remove(.XMT);
            cog.events.remove(.XFI);
            cog.events.remove(.XRO);
            if (cog.hub.clocking) {
                streamer.issued = .{ .command = command, .at = cog.hub.counter +% 1 };
            } else {
                streamer.accept(cog, command);
            }
        },
    }
}

/// Deliver the registered command port independently of instruction execution.
fn accept(streamer: *Streamer, cog: *Cog, command: Command) void {
    if (command.start == .immediate) {
        streamer.pending = null;
        streamer.begin(cog, command);
    } else if (streamer.active == null) {
        streamer.begin(cog, command);
        cog.raise_event(.XMT);
    } else {
        std.debug.assert(streamer.pending == null);
        streamer.pending = command;
    }
}

/// Start a command with its captured frequency and initial sample before the first rollover.
fn begin(streamer: *Streamer, cog: *Cog, command: Command) void {
    if (command.start != .continuous) {
        streamer.phase = 0;
    }
    if (command.frequency) |value| {
        streamer.frequency = value;
    }
    const count: u16 = @truncate(command.control);
    if (count == 0) {
        if (streamer.active != null) {
            cog.raise_event(.XFI);
        }
        streamer.active = null;
        streamer.pin_data = 0;
        streamer.publishDacs(cog);
        return;
    }
    streamer.active = .{
        .command = command,
        .remaining = count,
        .shift = if (command.format.alternate) reorder(command.data, command.format.bits) else command.data,
        .samples_left = if (command.format.source == .immediate) @intCast(32 / command.format.bits) else 0,
        .capture = 0,
        .capture_bits = 0,
        .first_clock = true,
    };
    streamer.sample(cog, true);
}

/// Advance the NCO even while its cog waits; serialization advances on every system clock.
pub fn step(streamer: *Streamer, cog: *Cog) void {
    if (streamer.active == null and streamer.issued == null) {
        if (streamer.drain_clocks == 0)
            return;
        streamer.drain_clocks -= 1;
    } else {
        streamer.drain_clocks = 5;
    }
    if (streamer.issued) |issued| {
        if (issued.at == cog.hub.counter) {
            streamer.issued = null;
            if (issued.command.start == .immediate and streamer.active != null and !streamer.active.?.first_clock) {
                const old_phase = (streamer.phase & 0x7fff_ffff) +% streamer.frequency;
                if (old_phase & 0x8000_0000 != 0) {
                    cog.raise_event(.XRO);
                }
            }
            streamer.accept(cog, issued.command);
        }
    }
    if (streamer.active) |*active| {
        if (active.first_clock) {
            active.first_clock = false;
        } else {
            streamer.phase = (streamer.phase & 0x7fff_ffff) +% streamer.frequency;
            if (streamer.phase & 0x8000_0000 != 0) {
                cog.raise_event(.XRO);
                if (active.remaining == 1 or (active.remaining == 0xffff and streamer.pending != null)) {
                    if (streamer.pending) |command| {
                        streamer.pending = null;
                        streamer.begin(cog, command);
                        if (streamer.active) |*next| {
                            next.first_clock = false;
                        }
                        cog.raise_event(.XMT);
                    } else {
                        streamer.active = null;
                        streamer.pin_data = 0;
                        streamer.publishDacs(cog);
                        cog.raise_event(.XFI);
                    }
                } else {
                    if (active.remaining != 0xffff) {
                        active.remaining -= 1;
                    }
                    streamer.sample(cog, false);
                }
            }
        }
    }
    const video = videoMode(cog) == .forward or videoMode(cog) == .reverse;
    var serialized: ?u64 = null;
    if (video) {
        if (streamer.active) |active| {
            const command = active.command;
            const enabled = command.control & (1 << 23) != 0;
            serialized = if (enabled)
                std.math.rotl(u64, streamer.tmds.step(videoMode(cog)), command.pinBase())
            else
                0;
            // The encoder publishes its initial clock/zero-data pairs before the first character arrives.
            const idle: u8 = if (videoMode(cog) == .reverse) @bitReverse(@as(u8, 0x56)) else 0x56;
            streamer.pin_data = if (enabled) std.math.rotl(u64, idle, command.pinBase()) else 0;
        }
    }
    const video_output = streamer.video_pipeline.step(serialized);
    if (video) {
        if (video_output) |value| {
            streamer.pin_data = value;
        }
    }
    // Both instruction phases resolve one raw-data clock and four total TMDS clocks before GPIO transport.
    streamer.pin_pipeline.resize(if (video) 2 else 1) catch unreachable;
    streamer.pins = streamer.pin_pipeline.step(streamer.pin_data);
}

/// Route selected streamer channels over the retained background values without analog consumers.
fn publishDacs(streamer: *const Streamer, cog: *Cog) void {
    cog.raw_dacs = streamer.background;
    if (streamer.active == null) {
        cog.dacs = cog.converter.output(cog.raw_dacs);
        return;
    }
    const active = streamer.active.?;
    const selector: u4 = @truncate(active.command.control >> 24);
    const routes = [16][4]i8{
        .{ -1, -1, -1, -1 }, .{ 0, 0, 0, 0 },    .{ 0, 0, -1, -1 },  .{ -1, -1, 0, 0 },
        .{ 0, -1, -1, -1 },  .{ -1, 0, -1, -1 }, .{ -1, -1, 0, -1 }, .{ -1, -1, -1, 0 },
        .{ 0, 4, 0, 4 },     .{ 0, 4, -1, -1 },  .{ -1, -1, 0, 4 },  .{ 0, 1, 0, 1 },
        .{ 0, 1, -1, -1 },   .{ -1, -1, 0, 1 },  .{ 0, 4, 1, 5 },    .{ 0, 1, 2, 3 },
    };
    for (&cog.raw_dacs, routes[selector]) |*out, route| {
        if (route >= 0) {
            const value = streamer.channels[@as(u3, @intCast(route)) & 3];
            out.* = if (route >= 4) ~value else value;
        }
    }
    cog.dacs = cog.converter.output(cog.raw_dacs);
}

/// Read the independent TMDS configuration from the authoritative CMOD register.
fn videoMode(cog: *const Cog) Video {
    const control = cog.converter.control;
    return @enumFromInt((@as(u2, @intFromBool(control.tmds)) << 1) | @intFromBool(control.reverse_tmds));
}

/// Read one packed source sample and publish its pin and DAC representations.
fn sample(streamer: *Streamer, cog: *Cog, initial: bool) void {
    const active = &streamer.active.?;
    const command = active.command;
    const format = command.format;
    cog.hub.access_context = command.provenance;
    if (command.fifoMode()) |mode| {
        if (cog.exec_mode == .hub or cog.fifo_state.engine == null or cog.fifo_state.engine.?.mode != mode) {
            cog.hub.fault = .{
                .cog = cog.id,
                .pc = command.provenance.pc orelse 0,
                .instruction = command.provenance.instruction orelse 0,
                .result = .{ .illegal = "active streamer lost its RDFAST/WRFAST FIFO" },
            };
            return;
        }
    }
    var value: u32 = 0;
    if (format.source == .pins) {
        value = @truncate(std.math.rotr(u64, cog.hub.io.get_in(), command.pinBase()));
    } else {
        if (format.source == .fifo and active.samples_left == 0) {
            const bytes: u3 = if (format.lut) 4 else @intCast(@max(@as(u6, 8), format.bits) / 8);
            active.shift = cog.read_fifo(bytes) orelse 0;
            if (format.alternate) {
                active.shift = reorder(active.shift, format.bits);
            }
            active.samples_left = @intCast(@as(u6, bytes) * 8 / format.bits);
        }
        value = active.shift;
    }
    if (format.bits != 32) {
        value &= (@as(u32, 1) << @as(u5, @intCast(format.bits))) - 1;
    }
    if (format.lut) {
        const base: u9 = if (format.source == .immediate)
            @as(u9, @as(u4, @truncate(command.control >> 16))) << 5
        else
            @as(u9, @as(u4, @truncate(command.data))) << 5;
        const address: u9 = base +% @as(u9, @truncate(value));
        // The initial lookup is prefetched before command publication; subsequent reads share the write port.
        if (initial or streamer.lut_write == null) {
            streamer.lut_value = cog.read_lut(address);
            if (address == 0x1ff) {
                cog.raise_event(.XRL);
            }
        }
        value = streamer.lut_value;
        streamer.channels = dacBytes(value);
    } else if (format.pixel != .none) {
        value = pixel(format.pixel, value, command.data);
        streamer.channels = dacBytes(value);
    } else {
        streamer.channels = expand(value, format.bits, format.channel_bits);
    }
    if (format.source == .pins and command.control & (1 << 23) != 0) {
        if (format.bits < 8) {
            const offset = if (format.alternate) 8 - format.bits - active.capture_bits else active.capture_bits;
            active.capture |= value << @as(u5, @intCast(offset));
            active.capture_bits += @intCast(format.bits);
            if (active.capture_bits == 8) {
                if (!cog.fifo_write_in_use()) {
                    _ = cog.write_fifo(active.capture, 1);
                }
                active.capture = 0;
                active.capture_bits = 0;
            }
        } else {
            if (!cog.fifo_write_in_use()) {
                _ = cog.write_fifo(value, @intCast(format.bits / 8));
            }
        }
    }
    if (format.source != .pins) {
        if (format.source == .fifo or active.samples_left > 1) {
            if (format.bits == 32) {
                active.shift = 0;
            } else {
                active.shift >>= @intCast(format.bits);
            }
            active.samples_left -= 1;
        }
    }
    if (command.control & (1 << 23) != 0 and format.source != .pins) {
        streamer.pin_data = std.math.rotl(u64, value, command.pinBase());
    } else {
        streamer.pin_data = 0;
    }
    if (videoMode(cog) == .forward or videoMode(cog) == .reverse) {
        streamer.tmds.load(value);
    }
    streamer.publishDacs(cog);
}

/// Reverse sample order within each byte without reversing the bits inside a sample.
fn reorder(value: u32, width: u6) u32 {
    var result: u32 = 0;
    for (0..4) |byte| {
        var bit: u6 = 0;
        while (bit < 8) : (bit += width) {
            const mask = (@as(u32, 1) << @as(u5, @intCast(width))) - 1;
            const sample_bits = (value >> @intCast(byte * 8 + bit)) & mask;
            result |= sample_bits << @intCast(byte * 8 + 8 - width - bit);
        }
    }
    return result;
}

/// Split an architectural word into DAC0..DAC3 independently of host byte order.
fn dacBytes(value: u32) [4]u8 {
    return .{
        @truncate(value),
        @truncate(value >> 8),
        @truncate(value >> 16),
        @truncate(value >> 24),
    };
}

/// Replicate each 1/2/4/8-bit component across one complete DAC byte.
fn expand(value: u32, width: u6, channel_bits: u4) [4]u8 {
    var channels: [4]u8 = @splat(0);
    const mask: u32 = (@as(u32, 1) << @as(u5, channel_bits)) - 1;
    for (&channels, 0..) |*channel, index| {
        const offset = index * channel_bits;
        if (offset < width) {
            channel.* = @intCast(((value >> @intCast(offset)) & mask) * (255 / mask));
        }
    }
    return channels;
}

/// Expand the streamer's digital pixel formats; this is independent of the colorspace converter.
fn pixel(mode: Pixel, value: u32, data: u32) u32 {
    var red: u32 = 0;
    var green: u32 = 0;
    var blue: u32 = 0;
    switch (mode) {
        .luma8, .rgbi8 => {
            const intensity = if (mode == .luma8) value else ((value & 31) << 3) | ((value & 31) >> 2);
            const color: u3 = @truncate(if (mode == .luma8) data else value >> 5);
            red = if (color & 4 != 0 or color == 0) intensity else 0;
            green = if (color & 2 != 0) intensity else if (color == 0) intensity >> 1 else 0;
            blue = if (color & 1 != 0) intensity else 0;
        },
        .rgb8 => {
            const r = value >> 5;
            const g = (value >> 2) & 7;
            red = (r << 5) | (r << 2) | (r >> 1);
            green = (g << 5) | (g << 2) | (g >> 1);
            blue = (value & 3) * 85;
        },
        .rgb16 => {
            const r = value >> 11;
            const g = (value >> 5) & 63;
            const b = value & 31;
            red = (r << 3) | (r >> 2);
            green = (g << 2) | (g >> 4);
            blue = (b << 3) | (b >> 2);
        },
        .rgb24 => return value & 0xffff_ff00,
        .none => unreachable,
    }
    return (red << 24) | (green << 16) | (blue << 8);
}

/// Three independent TMDS running disparities and ten-bit serializers.
pub const Tmds = struct {
    disparity: [3]i8 = @splat(0),
    symbols: [3]u10 = @splat(0),
    position: u4 = 0,

    /// Encode pixel bytes or load literal ten-bit channel symbols, starting a new character.
    pub fn load(tmds: *Tmds, value: u32) void {
        if (value & 2 != 0) {
            for (&tmds.symbols, 0..) |*symbol, index| {
                symbol.* = @truncate(value >> @intCast(2 + index * 10));
            }
            tmds.disparity = @splat(0);
        } else {
            for (&tmds.symbols, &tmds.disparity, 0..) |*symbol, *disparity, index| {
                symbol.* = encode(@truncate(value >> @intCast(8 + index * 8)), disparity);
            }
        }
        tmds.position = 0;
    }

    /// Emit CLK, blue, green and red differential pairs, optionally reversing the eight pins.
    pub fn step(tmds: *Tmds, video: Video) u8 {
        var pins: u8 = if (tmds.position < 5) 2 else 1;
        for (tmds.symbols, 0..) |symbol, index| {
            const bit = symbol & (@as(u10, 1) << @as(u4, tmds.position)) != 0;
            pins |= @as(u8, if (bit) 2 else 1) << @intCast(2 + index * 2);
        }
        tmds.position = if (tmds.position == 9) 0 else tmds.position + 1;
        return if (video == .reverse) @bitReverse(pins) else pins;
    }

    /// Encode the DVI transition-minimized byte followed by DC-balancing inversion.
    pub fn encode(value: u8, disparity: *i8) u10 {
        const ones = @popCount(value);
        const xnor = ones > 4 or (ones == 4 and value & 1 == 0);
        var q: u8 = value & 1;
        for (1..8) |index| {
            const shift: u3 = @intCast(index);
            const bit = ((q >> (shift - 1)) ^ (value >> shift)) & 1;
            q |= (bit ^ @as(u8, @intFromBool(xnor))) << shift;
        }
        const balance: i8 = 2 * @as(i8, @intCast(@popCount(q))) - 8;
        const bit8 = !xnor;
        if (disparity.* == 0 or balance == 0) {
            disparity.* += if (bit8) balance else -balance;
            return (@as(u10, @intFromBool(!bit8)) << 9) |
                (@as(u10, @intFromBool(bit8)) << 8) | @as(u10, if (bit8) q else ~q);
        }
        const invert = (disparity.* > 0 and balance > 0) or (disparity.* < 0 and balance < 0);
        if (invert) {
            disparity.* += @as(i8, if (bit8) 2 else 0) - balance;
        } else {
            disparity.* += balance - @as(i8, if (bit8) 0 else 2);
        }
        return (@as(u10, @intFromBool(invert)) << 9) |
            (@as(u10, @intFromBool(bit8)) << 8) | @as(u10, if (invert) ~q else q);
    }
};

test "TMDS all bytes round-trip, minimize transitions and account for disparity" {
    for (0..256) |byte| {
        var initial: i8 = -8;
        while (initial <= 8) : (initial += 2) {
            var disparity = initial;
            const symbol = Tmds.encode(@intCast(byte), &disparity);
            const q: u8 = @as(u8, @truncate(symbol)) ^ @as(u8, if (symbol & 0x200 != 0) 255 else 0);
            var decoded: u8 = q & 1;
            var transitions: u4 = 0;
            for (1..8) |index| {
                const shift: u3 = @intCast(index);
                const transition: u8 = ((q >> shift) ^ (q >> (shift - 1))) & 1;
                transitions += @intCast(transition);
                const bit = transition ^ @as(u8, @intFromBool(symbol & 0x100 == 0));
                decoded |= bit << shift;
            }
            try std.testing.expectEqual(@as(u8, @intCast(byte)), decoded);
            try std.testing.expect(transitions <= 4);
            try std.testing.expectEqual(initial + 2 * @as(i8, @intCast(@popCount(symbol))) - 10, disparity);
            try std.testing.expect(disparity >= -8 and disparity <= 8);
        }
    }
    var disparity: i8 = 0;
    try std.testing.expectEqual(@as(u10, 0x100), Tmds.encode(0, &disparity));
    disparity = 0;
    try std.testing.expectEqual(@as(u10, 0x200), Tmds.encode(255, &disparity));
}

test "TMDS literal symbols and reverse pin order serialize without touching DAC state" {
    var forward: Tmds = .{};
    forward.load(0b1101010100_0010101011_0101010100_10);
    var reverse = forward;
    for (0..10) |index| {
        const pins = forward.step(.forward);
        try std.testing.expectEqual(@bitReverse(pins), reverse.step(.reverse));
        try std.testing.expectEqual(@as(u8, 0x55), (pins ^ (pins >> 1)) & 0x55);
        try std.testing.expectEqual(@as(u8, if (index < 5) 2 else 1), pins & 3);
    }
}

test "immediate data repeats its last sample, restores background, and resets per cog" {
    const Hub = @import("Hub.zig");
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    hub.init();
    const cog = &hub.cogs[0];
    cog.streamer.apply(cog, .{ .dacs = 0x78563412 });
    const command = try Command.init(0x6f0e0006, 0x04030201, null, .immediate, .{});
    cog.streamer.apply(cog, .{ .command = command });
    cog.streamer.step(cog);
    for ([_]u8{ 1, 2, 3, 4, 4, 4 }) |expected| {
        try std.testing.expectEqual(expected, cog.dacs[0]);
        cog.streamer.step(cog);
    }
    try std.testing.expect(cog.events.contains(.XFI));
    try std.testing.expectEqualSlices(u8, &.{ 0x12, 0x34, 0x56, 0x78 }, &cog.dacs);
    try std.testing.expectEqualSlices(u8, &.{ 0, 0, 0, 0 }, &hub.cogs[1].dacs);
    cog.reset();
    try std.testing.expectEqualDeep(Streamer{}, cog.streamer);
    try std.testing.expectEqualSlices(u8, &.{ 0, 0, 0, 0 }, &cog.dacs);
}

test "LUT address wraps, raises XRL and selectable sensors, and shared writes win" {
    const Hub = @import("Hub.zig");
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    hub.init();
    const cog = &hub.cogs[0];
    cog.lut[0x1ff] = 0x12345678;
    cog.lut[0] = 0xabcdef01;
    cog.selectable_events[0] = .{ .lut_lock = 3 };
    const command = try Command.init(0x3f0f0002, 0x201f, null, .immediate, .{});
    cog.streamer.apply(cog, .{ .command = command });
    try std.testing.expect(cog.events.contains(.XRL));
    try std.testing.expect(cog.events.contains(.SE1));
    try std.testing.expectEqualSlices(u8, &.{ 0x78, 0x56, 0x34, 0x12 }, &cog.dacs);
    cog.streamer.step(cog);
    cog.streamer.step(cog);
    try std.testing.expectEqualSlices(u8, &.{ 1, 0xef, 0xcd, 0xab }, &cog.dacs);
    cog.events = .empty;
    cog.streamer.lut_write = 100;
    const colliding = try Command.init(0x3f0f0002, 0x1f20, null, .immediate, .{});
    cog.streamer.apply(cog, .{ .command = colliding });
    cog.streamer.step(cog);
    cog.streamer.step(cog);
    try std.testing.expect(!cog.events.contains(.XRL));
    try std.testing.expectEqualSlices(u8, &.{ 1, 0xef, 0xcd, 0xab }, &cog.dacs);
}

test "alternate ordering reverses samples within bytes, preserving sample bit order" {
    try std.testing.expectEqual(@as(u32, 0x80c4a2e6), reorder(0x01234567, 1));
    try std.testing.expectEqual(@as(u32, 0x40c851d9), reorder(0x01234567, 2));
    try std.testing.expectEqual(@as(u32, 0x10325476), reorder(0x01234567, 4));
}
