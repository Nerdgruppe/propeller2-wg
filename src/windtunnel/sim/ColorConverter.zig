//! Cog-local digital colorspace matrix, five-stage chroma rotator and final DAC selection.
//! Integer widths and transport boundaries are characterized on Rev B/C silicon; see docs/accumulated.md.
const std = @import("std");
const ColorConverter = @This();

pub const Mode = enum(u2) { bypass, component, svideo, composite };
pub const Control = packed struct(u9) {
    invert_sync: bool = false,
    add_q: bool = false,
    add_i: bool = false,
    add_y: bool = false,
    signed: bool = false,
    mode: Mode = .bypass,
    reverse_tmds: bool = false,
    tmds: bool = false,
};
pub const Action = union(enum) { cy: u32, ci: u32, cq: u32, frequency: u32, control: u9 };

pub const Matrix = struct {
    y: i32 = 0,
    i: i32 = 0,
    q: i32 = 0,
    dac0: u8 = 0,
};

pub const Stage = struct {
    x: i12 = 0,
    y: i12 = 0,
    angle: i8 = 0,
    luma: u8 = 0,
    i: u8 = 0,
    q: u8 = 0,
    dac0: u8 = 0,
};

cy: u32 = 0,
ci: u32 = 0,
cq: u32 = 0,
frequency: u32 = 0,
phase: u32 = 0,
control: Control = .{},
input: [4]u8 = @splat(0),
matrix_latch: Matrix = .{},
/// Equivalent register partition: two rotations, two rotations, one rotation, output.
/// External input/coefficient/phase delays are measured; internal cell placement is inferred.
stages: [4]Stage = @splat(.{}),

/// Write a parameter; mode transitions preserve the arithmetic pipeline but bypass clears phase.
pub fn apply(converter: *ColorConverter, action: Action) void {
    switch (action) {
        .cy => |value| converter.cy = value,
        .ci => |value| converter.ci = value,
        .cq => |value| converter.cq = value,
        .frequency => |value| converter.frequency = value,
        .control => |value| {
            converter.control = @bitCast(value);
            if (!converter.active()) {
                converter.phase = 0;
            }
        },
    }
}

/// Report conversion independently of the CMOD bits controlling the streamer's TMDS path.
pub fn active(converter: *const ColorConverter) bool {
    return converter.control.mode != .bypass;
}

/// Advance the gated arithmetic once, using the preceding DAC latch and live coefficient words.
pub fn step(converter: *ColorConverter, raw: [4]u8) [4]u8 {
    if (!converter.active()) {
        converter.phase = 0;
        converter.input = raw;
        return raw;
    }
    converter.phase -%= converter.frequency;
    converter.stages[3] = converter.stages[2];
    converter.stages[2] = rotate(converter.stages[1], 4);
    converter.stages[1] = rotate(rotate(converter.stages[0], 2), 3);
    converter.stages[0] = rotate(rotate(converter.firstStage(), 0), 1);
    converter.matrix_latch = .{
        .y = matrix(converter.input, converter.cy, converter.control.signed),
        .i = matrix(converter.input, converter.ci, converter.control.signed),
        .q = matrix(converter.input, converter.cq, converter.control.signed),
        .dac0 = converter.input[0],
    };
    converter.input = raw;
    return converter.output(raw);
}

/// Select final bytes using delayed arithmetic/DAC0 and live mode, offset and addition controls.
pub fn output(converter: *const ColorConverter, raw: [4]u8) [4]u8 {
    const stage = converter.stages[3];
    const cy: u8 = @truncate(converter.cy);
    const ci: u8 = @truncate(converter.ci);
    const cq: u8 = @truncate(converter.cq);
    const iq: u8 = @truncate(@as(u12, @bitCast(stage.y >> 4)));
    const luma = if (stage.dac0 & 2 != 0) @as(u8, 0) else if (stage.dac0 & 1 != 0) ci else cy +% stage.luma;
    const composite = luma +% iq;
    return switch (converter.control.mode) {
        .bypass => raw,
        .component => .{
            if ((stage.dac0 & 1 != 0) != converter.control.invert_sync) 255 else 0,
            cq +% stage.q +% (if (converter.control.add_q) stage.dac0 else @as(u8, 0)),
            ci +% stage.i +% (if (converter.control.add_i) stage.dac0 else @as(u8, 0)),
            cy +% stage.luma +% (if (converter.control.add_y) stage.dac0 else @as(u8, 0)),
        },
        .svideo => .{ luma, cq +% iq, composite, composite },
        .composite => @splat(composite),
    };
}

/// Prepare a quadrant-normalized coordinate pair from the retained three fractional bits.
fn firstStage(converter: *const ColorConverter) Stage {
    const y = converter.matrix_latch.y;
    const i = converter.matrix_latch.i;
    const q = converter.matrix_latch.q;
    const x_fixed: i11 = @truncate(i >> 4);
    const y_fixed: i11 = @truncate(q >> 4);
    var stage: Stage = .{
        .x = x_fixed,
        .y = y_fixed,
        .angle = @bitCast(@as(u8, @truncate(converter.phase >> 24))),
        .luma = @truncate(@as(u32, @bitCast(y >> 7))),
        .i = @truncate(@as(u32, @bitCast(i >> 7))),
        .q = @truncate(@as(u32, @bitCast(q >> 7))),
        .dac0 = converter.matrix_latch.dac0,
    };
    if (stage.angle >= 64 or stage.angle < -64) {
        stage.x = -stage.x;
        stage.y = -stage.y;
        stage.angle +%= @as(i8, -128);
    }
    return stage;
}

/// Accumulate unsigned DAC samples against signed or zero-extended coefficient bytes.
fn matrix(raw: [4]u8, coefficients: u32, signed: bool) i32 {
    var result: i32 = 0;
    for (1..4) |channel| {
        const byte: u8 = @truncate(coefficients >> @as(u5, @intCast(channel * 8)));
        const coefficient: i32 = if (signed) @as(i8, @bitCast(byte)) else byte;
        result += @as(i32, raw[channel]) * coefficient;
    }
    return result;
}

/// Rotate one cell with arithmetic shifts, explicit twelve-bit wrapping and quantized angles.
fn rotate(previous: Stage, index: u3) Stage {
    const angles = [5]i8{ 32, 19, 10, 5, 3 };
    var next = previous;
    if (previous.angle >= 0) {
        next.x = previous.x -% (previous.y >> index);
        next.y = previous.y +% (previous.x >> index);
        next.angle = previous.angle - angles[index];
    } else {
        next.x = previous.x +% (previous.y >> index);
        next.y = previous.y -% (previous.x >> index);
        next.angle = previous.angle + angles[index];
    }
    return next;
}

/// Clock enough edges to observe a constant matrix/phase input.
fn settle(converter: *ColorConverter, raw: [4]u8) [4]u8 {
    var result: [4]u8 = undefined;
    for (0..6) |_| {
        result = converter.step(raw);
    }
    return result;
}

test "matrix signed coefficients offsets additions and byte wrapping" {
    var converter: ColorConverter = .{};
    converter.apply(.{ .cy = 0x8000_0001 });
    converter.apply(.{ .ci = 0x0080_0002 });
    converter.apply(.{ .cq = 0x0000_8003 });
    converter.apply(.{ .control = 0x20 });
    try std.testing.expectEqual([4]u8{ 0, 55, 88, 121 }, settle(&converter, .{ 18, 52, 86, 120 }));
    converter.apply(.{ .control = 0x3e });
    try std.testing.expectEqual([4]u8{ 0, 225, 190, 159 }, settle(&converter, .{ 22, 56, 90, 120 }));
}

test "DAC coefficient phase and offset changes have measured five four three and zero clock delays" {
    var converter: ColorConverter = .{};
    converter.apply(.{ .cy = 0x8000_0000 });
    converter.apply(.{ .ci = 0x8000_0000 });
    converter.apply(.{ .control = 0x20 });
    _ = settle(&converter, .{ 0, 0, 0, 64 });
    for (0..5) |_| {
        try std.testing.expectEqual(@as(u8, 64), converter.step(.{ 0, 0, 0, 128 })[3]);
    }
    try std.testing.expectEqual(@as(u8, 128), converter.step(.{ 0, 0, 0, 128 })[3]);
    converter.apply(.{ .cy = 0x4000_0000 });
    for (0..4) |_| {
        try std.testing.expectEqual(@as(u8, 128), converter.step(.{ 0, 0, 0, 128 })[3]);
    }
    try std.testing.expectEqual(@as(u8, 64), converter.step(.{ 0, 0, 0, 128 })[3]);
    converter.apply(.{ .cy = 0x4000_0007 });
    try std.testing.expectEqual(@as(u8, 71), converter.output(.{ 0, 0, 0, 128 })[3]);
    converter.apply(.{ .cy = 128 });
    converter.apply(.{ .control = 0x60 });
    _ = settle(&converter, .{ 0, 0, 0, 128 });
    converter.apply(.{ .frequency = 0x4000_0000 });
    for (0..3) |_| {
        try std.testing.expectEqual(@as(u8, 126), converter.step(.{ 0, 0, 0, 128 })[3]);
    }
    try std.testing.expectEqual(@as(u8, 233), converter.step(.{ 0, 0, 0, 128 })[3]);
}

test "bypass clears phase freezes arithmetic and keeps TMDS controls independent" {
    var converter: ColorConverter = .{};
    converter.apply(.{ .control = 0x60 });
    converter.apply(.{ .frequency = 0x1234_5678 });
    _ = settle(&converter, .{ 1, 2, 3, 4 });
    const stages = converter.stages;
    converter.apply(.{ .control = 0x180 });
    try std.testing.expect(!converter.active());
    try std.testing.expect(converter.control.tmds and converter.control.reverse_tmds);
    try std.testing.expectEqual([4]u8{ 4, 3, 2, 1 }, converter.step(.{ 4, 3, 2, 1 }));
    try std.testing.expectEqual(@as(u32, 0), converter.phase);
    try std.testing.expectEqualDeep(stages, converter.stages);
    try std.testing.expectEqual(@as(u32, 0x1234_5678), converter.frequency);
}

test "silicon chroma vectors retain product fractions and wrap twelve-bit coordinates" {
    const Vector = struct { dacs: [4]u8, ci: u32, cq: u32, phase: u8, expected: u8 };
    const vectors = [_]Vector{
        .{ .dacs = .{ 0, 0, 0, 1 }, .ci = 0x4000_0000, .cq = 0, .phase = 255, .expected = 127 },
        .{ .dacs = .{ 0, 0, 0, 255 }, .ci = 0xff00_0000, .cq = 0, .phase = 208, .expected = 131 },
        .{ .dacs = .{ 0, 0, 127, 127 }, .ci = 0x8000_0000, .cq = 0x0080_0000, .phase = 240, .expected = 138 },
        .{ .dacs = .{ 0, 0, 128, 128 }, .ci = 0x8000_0000, .cq = 0x0080_0000, .phase = 240, .expected = 117 },
    };
    for (vectors) |vector| {
        var converter: ColorConverter = .{};
        converter.apply(.{ .cy = 128 });
        converter.apply(.{ .ci = vector.ci });
        converter.apply(.{ .cq = vector.cq });
        converter.apply(.{ .control = 0x60 });
        converter.phase = @as(u32, vector.phase) << 24;
        try std.testing.expectEqual(@as([4]u8, @splat(vector.expected)), settle(&converter, vector.dacs));
    }
}
