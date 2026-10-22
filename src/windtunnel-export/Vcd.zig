//! Convert native Scope recordings into VCD without participating in simulation.
const std = @import("std");
const windtunnel = @import("windtunnel");
const Capture = windtunnel.Capture;
const DigitalSample = Capture.DigitalSample;
const AnalogSample = Capture.AnalogSample;
const ResetSample = Capture.ResetSample;

/// Export a retained recording, merging changes at equal timestamps without dropping their order.
pub fn write(capture: *const Capture, writer: *std.Io.Writer) !void {
    try writer.print("$version Windtunnel Scope $end\n$comment Original start {d} fs; clipping 0=none 1=low 2=high. $end\n$timescale 1fs $end\n$scope module scope $end\n", .{capture.metadataConst().start});
    for (0..64) |pin| {
        try writer.print("$var wire 1 d{d} P{d} $end\n$var real 64 a{d} P{d}_voltage $end\n$var wire 2 c{d} P{d}_clipped $end\n", .{ pin, pin, pin, pin, pin, pin });
    }
    try writer.writeAll("$var wire 64 r reset_count $end\n$upscope $end\n$enddefinitions $end\n#0\n$dumpvars\n");
    var digital: [64]Capture.Iterator(DigitalSample) = undefined;
    var analog: [64]Capture.Iterator(AnalogSample) = undefined;
    var next_d: [64]?DigitalSample = undefined;
    var next_a: [64]?AnalogSample = undefined;
    for (&digital, &analog, &next_d, &next_a, 0..) |*di, *ai, *nd, *na, pin| {
        di.* = capture.iterator(DigitalSample, capture.digitalChannelsConst()[pin]);
        ai.* = capture.iterator(AnalogSample, capture.analogChannelsConst()[pin]);
        di.seek(capture.metadataConst().start);
        ai.seek(capture.metadataConst().start);
        var d: ?DigitalSample = null;
        nd.* = di.next();
        while (nd.*) |sample| {
            if (sample.timestamp >= capture.metadataConst().start)
                break;
            d = sample;
            nd.* = di.next();
        }
        if (d == null and nd.* != null and nd.*.?.timestamp == capture.metadataConst().start) {
            d = nd.*;
            nd.* = di.next();
        }
        var a: ?AnalogSample = null;
        na.* = ai.next();
        while (na.*) |sample| {
            if (sample.timestamp >= capture.metadataConst().start)
                break;
            a = sample;
            na.* = ai.next();
        }
        if (a == null and na.* != null and na.*.?.timestamp == capture.metadataConst().start) {
            a = na.*;
            na.* = ai.next();
        }
        if (d) |sample| {
            try writer.print("{d}d{d}\n", .{ @intFromEnum(sample.value), pin });
        }
        if (a) |sample| {
            try analogVcd(writer, pin, sample.value);
        }
    }
    var resets = capture.iterator(ResetSample, capture.resetChannelConst().*);
    var next_reset = resets.next();
    var reset_count: u64 = 0;
    while (next_reset) |sample| {
        if (sample.timestamp >= capture.metadataConst().start)
            break;
        reset_count += 1;
        next_reset = resets.next();
    }
    try writer.print("b{b} r\n$end\n", .{reset_count});
    while (true) {
        var timestamp: u128 = std.math.maxInt(u128);
        for (next_d) |sample| {
            if (sample) |value| {
                timestamp = @min(timestamp, value.timestamp);
            }
        }
        for (next_a) |sample| {
            if (sample) |value| {
                timestamp = @min(timestamp, value.timestamp);
            }
        }
        if (next_reset) |sample| {
            timestamp = @min(timestamp, sample.timestamp);
        }
        if (timestamp == std.math.maxInt(u128) or timestamp > capture.metadataConst().end)
            break;
        try writer.print("#{d}\n", .{timestamp - capture.metadataConst().start});
        for (&digital, &next_d, 0..) |*di, *next, pin| {
            while (next.*) |sample| {
                if (sample.timestamp != timestamp)
                    break;
                try writer.print("{d}d{d}\n", .{ @intFromEnum(sample.value), pin });
                next.* = di.next();
            }
        }
        for (&analog, &next_a, 0..) |*ai, *next, pin| {
            while (next.*) |sample| {
                if (sample.timestamp != timestamp)
                    break;
                try analogVcd(writer, pin, sample.value);
                next.* = ai.next();
            }
        }
        while (next_reset) |sample| {
            if (sample.timestamp != timestamp)
                break;
            reset_count += 1;
            try writer.print("b{b} r\n", .{reset_count});
            next_reset = resets.next();
        }
    }
    try writer.print("#{d}\n", .{capture.metadataConst().end - capture.metadataConst().start});
    try writer.flush();
}

/// Emit voltage and its independently visible clipping state.
fn analogVcd(writer: *std.Io.Writer, pin: usize, value: u16) !void {
    const clipped: u2 = switch (value) {
        0 => 1,
        std.math.maxInt(u16) => 2,
        else => 0,
    };
    try writer.print("r{d:.4} a{d}\nb{b} c{d}\n", .{ Capture.volts(value), pin, clipped, pin });
}

const test_initial: Capture.InitialState = .{
    .digital = @splat(.low),
    .analog = @splat(20001),
};

test "VCD preserves ordered coincident changes, clipping and quiet pin baselines" {
    var capture = try Capture.init(std.testing.allocator, Capture.minimum_memory, .stop, test_initial);
    defer capture.deinit();

    try std.testing.expect(capture.recordDigital(&capture.digitalChannels()[0], 1, .high));
    try std.testing.expect(capture.recordDigital(&capture.digitalChannels()[0], 1, .low));
    try std.testing.expect(capture.recordAnalog(&capture.analogChannels()[0], 1, 0));
    try std.testing.expect(capture.recordAnalog(&capture.analogChannels()[0], 1, 65535));
    capture.reset(.firmware);
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();

    try write(&capture, &output.writer);
    const text = output.written();
    try std.testing.expect(std.mem.indexOf(u8, text, "#1\n1d0\n0d0\n") != null);
    try std.testing.expect(std.mem.indexOf(u8, text, "r-2.0000 a0\nb1 c0\nr4.5000 a0\nb10 c0\n") != null);
    try std.testing.expect(std.mem.indexOf(u8, text, "0d63\nr0.0000 a63\nb0 c63\n") != null);
    try std.testing.expect(std.mem.indexOf(u8, text, "b1 r\n") != null);
}

test "VCD keeps time-zero transitions and holds unchanged values to the recording end" {
    var capture = try Capture.init(std.testing.allocator, Capture.minimum_memory, .stop, test_initial);
    defer capture.deinit();

    try std.testing.expect(capture.recordDigital(&capture.digitalChannels()[0], 0, .high));
    try std.testing.expect(capture.recordDigital(&capture.digitalChannels()[0], 0, .low));
    try std.testing.expect(capture.recordDigital(&capture.digitalChannels()[0], 100, .low));
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();

    try write(&capture, &output.writer);
    try std.testing.expect(std.mem.indexOf(u8, output.written(), "#0\n1d0\n0d0\n") != null);
    try std.testing.expect(std.mem.endsWith(u8, output.written(), "#100\n"));
}

test "native reload preserves retained VCD, quiet clipped baselines and reset markers" {
    var initial = test_initial;
    initial.digital[63] = .high;
    initial.analog[63] = 65535;
    var original = try Capture.init(std.testing.allocator, Capture.minimum_memory, .rolling, initial);
    defer original.deinit();

    try std.testing.expect(original.recordDigital(&original.digitalChannels()[0], 1, .high));
    try std.testing.expect(original.recordDigital(&original.digitalChannels()[0], 1, .low));
    original.reset(.reinitialize);
    try std.testing.expect(original.recordAnalog(&original.analogChannels()[0], 2, Capture.voltage(3.3)));
    original.metadata().start = 1;
    var native = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer native.deinit();

    try original.writeNative(&native.writer);
    var reader = std.Io.Reader.fixed(native.written());
    var loaded = try Capture.readNative(std.testing.allocator, &reader, native.written().len);
    defer loaded.deinit();
    var expected = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer expected.deinit();
    var actual = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer actual.deinit();

    try write(&original, &expected.writer);
    try write(&loaded, &actual.writer);
    try std.testing.expectEqualStrings(expected.written(), actual.written());
    try std.testing.expect(std.mem.indexOf(u8, actual.written(), "Original start 1 fs") != null);
    try std.testing.expect(std.mem.indexOf(u8, actual.written(), "1d63\nr4.5000 a63\nb10 c63\n") != null);
    try std.testing.expect(std.mem.indexOf(u8, actual.written(), "#0\n1d0\n0d0\n") != null);
}

test "VCD writer failures propagate and an exhausted merge terminates at maximum end time" {
    var capture = try Capture.init(std.testing.allocator, Capture.minimum_memory, .stop, test_initial);
    defer capture.deinit();

    var tiny_buffer: [1]u8 = undefined;
    var tiny = std.Io.Writer.fixed(&tiny_buffer);
    try std.testing.expectError(error.WriteFailed, write(&capture, &tiny));
    capture.metadata().time.femtoseconds = std.math.maxInt(u128);
    capture.metadata().end = std.math.maxInt(u128);
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();

    try write(&capture, &output.writer);
    try std.testing.expect(std.mem.endsWith(u8, output.written(), "#340282366920938463463374607431768211455\n"));
}
