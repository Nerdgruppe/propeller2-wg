//! One shared hub CORDIC: 54 clocked arithmetic stages followed by cog read transport.
//! Every occupied stage carries its issuer and live arithmetic state; results are never precomputed.

const std = @import("std");
const Cordic = @This();

pub const CogId = enum(u3) { _ };
pub const Operation = enum { mul, div, frac, sqrt, rotate, vector, log, exp };
pub const Axis = enum { x, y };
pub const Result = struct { x: u32, y: u32 = 0 };
pub const Operands = struct { d: u32, s: u32, q: u32 };

/// Cog-local result interface. All arithmetic and physical in-flight state lives in the hub.
pub const Port = struct {
    result: Result = .{ .x = 0 },
    ready: std.EnumArray(Axis, bool) = .initFill(false),
    pending: u4 = 0,

    /// Invalidate both unread words and track a newly submitted command.
    pub fn begin(port: *Port) void {
        port.ready = .initFill(false);
        port.pending += 1;
    }

    /// Replace both latches, independently of whether reset abandoned this command.
    pub fn accept(port: *Port, delivery: Delivery) void {
        port.result = delivery.result;
        port.ready = .initFill(true);
        if (delivery.tracked) port.pending -= 1;
    }

    /// Clear read readiness/tracking while preserving the two physical result words.
    pub fn reset(port: *Port) void {
        port.ready = .initFill(false);
        port.pending = 0;
    }

    /// Report whether an unready read should wait for a locally submitted command.
    pub fn busy(port: *const Port) bool {
        return port.pending != 0;
    }
};

pub const Multiply = struct { product: u64 = 0, multiplicand: u64, multiplier: u32 };
pub const Divide = struct { remainder: i33, quotient: u32, divisor: u32 };
pub const SquareRoot = struct { radicand: u64, remainder: u64 = 0, root: u32 = 0 };
pub const Circular = struct { x: i64, y: i64, z: i64, normalization: u5 = 0, zero: bool = false };
pub const Hyperbolic = struct { input: u32, x: i64 = 0, y: i64 = 0, z: i64 = 0, magnitude: u5 = 0 };
pub const Arithmetic = union(enum) {
    operands: Operands,
    multiply: Multiply,
    divide: Divide,
    square_root: SquareRoot,
    circular: Circular,
    hyperbolic: Hyperbolic,
    result: Result,
};
pub const Entry = struct { cog: CogId, tracked: bool = true, operation: Operation, data: Arithmetic };
pub const Delivery = struct { cog: CogId, tracked: bool, result: Result };
pub const Stage = union(enum) { decode, normalize, quadrant, iterate: u5, scale: u5, repeat: u5, finish };

/// The official depth is 54. The allocation of arithmetic work to individual cells is inferred,
/// not published RTL: three input stages, 50 iteration/correction cells, and final rounding.
pub const stage_layout: [54]Stage = blk: {
    var layout: [54]Stage = undefined;
    layout[0..3].* = .{ .decode, .normalize, .quadrant };
    var next: usize = 3;
    for (0..32) |i| {
        if (adjusts(i)) {
            layout[next] = .{ .scale = @intCast(i) };
            next += 1;
        }
        layout[next] = .{ .iterate = @intCast(i) };
        next += 1;
        if (i == 4 or i == 13) {
            layout[next] = .{ .repeat = @intCast(i) };
            next += 1;
        }
    }
    std.debug.assert(next == 53);
    layout[next] = .finish;
    break :blk layout;
};

input: ?Entry = null,
pipeline: [stage_layout.len]?Entry = @splat(null),
active: usize = 0,
/// Read-side transport is separate from the 54 arithmetic cells. It calibrates GETQ timing.
returns: [3]?Delivery = @splat(null),

/// Accept at most one hub-slot command on this edge, retaining operands for the first stage.
pub fn submit(cordic: *Cordic, cog: CogId, operation: Operation, d: u32, s: u32, q: u32) void {
    std.debug.assert(cordic.input == null);
    cordic.input = .{ .cog = cog, .operation = operation, .data = .{ .operands = .{ .d = d, .s = s, .q = q } } };
    cordic.active += 1;
}

/// Clock every occupied arithmetic stage once, then return one tagged result to its cog.
/// A grant enters stage 0 on the next edge, exits the solver at +55, and is readable at +58.
pub fn step(cordic: *Cordic) ?Delivery {
    if (cordic.active == 0) return null;
    const delivery = cordic.returns[cordic.returns.len - 1];
    if (delivery != null) cordic.active -= 1;
    std.mem.copyBackwards(?Delivery, cordic.returns[1..], cordic.returns[0 .. cordic.returns.len - 1]);
    cordic.returns[0] = if (cordic.pipeline[stage_layout.len - 1]) |last|
        .{ .cog = last.cog, .tracked = last.tracked, .result = last.data.result }
    else
        null;
    var index: usize = stage_layout.len;
    while (index != 0) {
        index -= 1;
        cordic.pipeline[index] = if (index == 0) cordic.input else cordic.pipeline[index - 1];
        if (cordic.pipeline[index]) |*entry| advance(entry, stage_layout[index]);
    }
    cordic.input = null;
    return delivery;
}

/// Abandon one cog's logical tracking without removing its physical arithmetic or return traffic.
pub fn forget(cordic: *Cordic, cog: CogId) void {
    if (cordic.input) |*entry| if (entry.cog == cog) {
        entry.tracked = false;
    };
    for (&cordic.pipeline) |*slot| if (slot.*) |*entry| {
        if (entry.cog == cog) entry.tracked = false;
    };
    for (&cordic.returns) |*slot| if (slot.*) |*delivery| {
        if (delivery.cog == cog) delivery.tracked = false;
    };
}

/// Prevent idle fast-forwarding from skipping an occupied arithmetic or transport stage.
pub fn in_flight(cordic: *const Cordic) bool {
    return cordic.active != 0;
}

/// Apply just this cell's work to its incoming entry, leaving other operations as pass-throughs.
fn advance(entry: *Entry, stage: Stage) void {
    switch (stage) {
        .decode => {
            const args = entry.data.operands;
            entry.data = switch (entry.operation) {
                .mul => .{ .multiply = .{ .multiplicand = args.d, .multiplier = args.s } },
                .div => .{ .divide = .{ .remainder = args.q, .quotient = args.d, .divisor = args.s } },
                .frac => .{ .divide = .{ .remainder = args.d, .quotient = args.q, .divisor = args.s } },
                .sqrt => .{ .square_root = .{ .radicand = (@as(u64, args.s) << 32) | args.d } },
                .rotate, .vector => .{ .circular = .{
                    .x = @as(i32, @bitCast(args.d)),
                    .y = @as(i32, @bitCast(if (entry.operation == .rotate) args.q else args.s)),
                    .z = if (entry.operation == .rotate) @as(i64, @as(i32, @bitCast(args.s))) << 8 else 0,
                } },
                .log, .exp => .{ .hyperbolic = .{ .input = args.d } },
            };
        },
        .normalize => switch (entry.data) {
            .circular => |*value| {
                const x: i32 = @intCast(value.x);
                const y: i32 = @intCast(value.y);
                const magnitude = (@abs(x) | @abs(y)) & 0x7fffffff;
                value.zero = magnitude == 0;
                if (value.zero) return;
                value.normalization = @intCast(@clz(magnitude) - 1);
                value.x = @as(i32, @bitCast(@as(u32, @bitCast(x)) << value.normalization));
                value.y = @as(i32, @bitCast(@as(u32, @bitCast(y)) << value.normalization));
            },
            .hyperbolic => |*value| {
                const d = value.input;
                const exponent = entry.operation == .exp;
                value.magnitude = if (exponent) 31 - @as(u5, @truncate(d >> 27)) else @intCast(@min(@clz(d), 31));
                value.y = if (exponent) 0 else @as(i64, (d << value.magnitude) & 0x7fffffff) << 6;
                value.x = if (exponent) 0x7f42e61c5a else value.y + (@as(i64, 1) << 38);
                value.z = if (exponent) @as(i64, d & 0x7ffffff) << 11 else 0;
            },
            else => {},
        },
        .quadrant => if (entry.data == .circular) {
            const value = &entry.data.circular;
            if (value.zero) return;
            var x: i32 = @intCast(value.x);
            var y: i32 = @intCast(value.y);
            const vector = entry.operation == .vector;
            const angle: u32 = @bitCast(@as(i32, @intCast(value.z >> 8)));
            if (if (vector) x < 0 else (angle ^ (angle << 1)) & 0x80000000 != 0) {
                x = -%x;
                y = -%y;
                value.z += if (vector or value.z < 0) @as(i64, 1) << 39 else -(@as(i64, 1) << 39);
            }
            value.x = @as(i64, x) << 7;
            value.y = @as(i64, y) << 7;
        },
        .scale => |index| switch (entry.data) {
            .circular => |*value| {
                value.x -= value.x >> index;
                value.y -= value.y >> index;
            },
            .hyperbolic => |*value| {
                value.x -= value.x >> index;
                value.y -= value.y >> index;
            },
            else => {},
        },
        .iterate => |index| iterate(entry, index),
        .repeat => |index| if (entry.data == .hyperbolic) {
            hyperbolicStep(&entry.data.hyperbolic, index, entry.operation == .exp);
        },
        .finish => {
            const result = finish(entry.*);
            entry.data = .{ .result = result };
        },
    }
}

/// Advance one binary multiply/divide/root step or one circular/hyperbolic microrotation.
fn iterate(entry: *Entry, index: u5) void {
    switch (entry.data) {
        .multiply => |*value| {
            if (value.multiplier & 1 != 0) value.product += value.multiplicand;
            value.multiplicand <<= 1;
            value.multiplier >>= 1;
        },
        .divide => |*value| {
            const negative = value.remainder < 0;
            value.remainder = (value.remainder << 1) | @as(i33, value.quotient >> 31);
            value.remainder = if (negative) value.remainder +% @as(i33, value.divisor) else value.remainder -% @as(i33, value.divisor);
            value.quotient = (value.quotient << 1) | @intFromBool(value.remainder >= 0);
        },
        .square_root => |*value| {
            value.remainder = (value.remainder << 2) | (value.radicand >> 62);
            value.radicand <<= 2;
            const trial = (@as(u64, value.root) << 2) | 1;
            value.root <<= 1;
            if (value.remainder >= trial) {
                value.remainder -= trial;
                value.root |= 1;
            }
        },
        .circular => |*value| {
            const sx = value.y >> index;
            const sy = value.x >> index;
            const delta = circular_angles[index];
            if (if (entry.operation == .vector) value.y < 0 else value.z >= 0) {
                value.x -= sx;
                value.y += sy;
                value.z -= delta;
            } else {
                value.x += sx;
                value.y -= sy;
                value.z += delta;
            }
        },
        .hyperbolic => |*value| if (index != 0) {
            hyperbolicStep(value, index, entry.operation == .exp);
        },
        else => unreachable,
    }
}

/// Perform one PNut hyperbolic step, with arithmetic shifts and mode-specific steering.
fn hyperbolicStep(value: *Hyperbolic, index: u5, exponent: bool) void {
    const sx = value.y >> index;
    const sy = value.x >> index;
    const delta = hyperbolic_angles[index - 1];
    if (if (exponent) value.z >= 0 else value.y < 0) {
        value.x += sx;
        value.y += sy;
        value.z -= delta;
    } else {
        value.x -= sx;
        value.y -= sy;
        value.z += delta;
    }
}

/// Correct the remainder or round/pack the final live state into two 32-bit result words.
fn finish(entry: Entry) Result {
    return switch (entry.data) {
        .multiply => |value| .{ .x = @truncate(value.product), .y = @truncate(value.product >> 32) },
        .divide => |value| blk: {
            const remainder = if (value.remainder < 0) value.remainder +% @as(i33, value.divisor) else value.remainder;
            break :blk .{ .x = value.quotient, .y = @truncate(@as(u33, @bitCast(remainder))) };
        },
        .square_root => |value| .{ .x = value.root },
        .circular => |value| blk: {
            if (value.zero) break :blk .{ .x = 0 };
            const shift: u6 = @as(u6, value.normalization) + 7;
            const half = @as(i64, 1) << (shift - 1);
            break :blk .{
                .x = @truncate(@as(u64, @bitCast((value.x + half) >> shift))),
                .y = @truncate(@as(u64, @bitCast(if (entry.operation == .vector) (value.z + 0x20) >> 8 else (value.y + half) >> shift))),
            };
        },
        .hyperbolic => |value| blk: {
            const m = value.magnitude;
            if (entry.operation == .exp) break :blk .{ .x = @intCast(((value.x >> m) + (value.y >> m) + 0x40) >> 7) };
            const z = ((value.z >> 2) & ~@as(i64, 0x7f)) + 0x80 + (@as(i64, 31 - m) << 35);
            break :blk .{ .x = if (m == 0 and z & (@as(i64, 1) << 39) == 0) 0xffffffff else @truncate(@as(u64, @bitCast(z >> 8))) };
        },
        else => unreachable,
    };
}

/// Shared gain corrections precede these numbered circular/hyperbolic iterations.
fn adjusts(index: usize) bool {
    return switch (index) {
        2, 3, 4, 7, 8, 10, 12, 14, 16, 19, 20, 22, 23, 24, 25, 30 => true,
        else => false,
    };
}

// atan(2^-i) in turns with 40 fractional bits, rounded to nearest.
const circular_angles = [_]i64{
    0x2000000000,
    0x12e4051d9e,
    0x09fb385b5f,
    0x051111d41e,
    0x028b0d430e,
    0x0145d7e159,
    0x00a2f61e5c,
    0x00517c5512,
    0x0028be5347,
    0x00145f2ebb,
    0x000a2f9801,
    0x000517cc15,
    0x00028be60d,
    0x000145f307,
    0x0000a2f983,
    0x0000517cc2,
    0x000028be61,
    0x0000145f30,
    0x00000a2f98,
    0x00000517cc,
    0x0000028be6,
    0x00000145f3,
    0x000000a2fa,
    0x000000517d,
    0x00000028be,
    0x000000145f,
    0x0000000a30,
    0x0000000518,
    0x000000028c,
    0x0000000146,
    0x00000000a3,
    0x0000000051,
};

// PNut's hyperbolic deltas: atanh(2^-i) / ln(2), scaled by 2^38.
const hyperbolic_angles = [_]i64{
    0x32b803473f,
    0x179538dea7,
    0x0b9a2c912f,
    0x05c73f7233,
    0x02e2e683f7,
    0x01715c285f,
    0x00b8ab3164,
    0x005c553c5c,
    0x002e2a92a3,
    0x00171547e0,
    0x000b8aa3c2,
    0x0005c551db,
    0x0002e2a8ed,
    0x0001715476,
    0x0000b8aa3b,
    0x00005c551e,
    0x00002e2a8f,
    0x0000171547,
    0x00000b8aa4,
    0x000005c552,
    0x000002e2a9,
    0x0000017154,
    0x000000b8aa,
    0x0000005c55,
    0x0000002e2b,
    0x0000001715,
    0x0000000b8b,
    0x00000005c5,
    0x00000002e3,
    0x0000000171,
    0x00000000b9,
};

test "all 54 stages carry live arithmetic before the result reaches cog transport" {
    var cordic: Cordic = .{};
    cordic.submit(@enumFromInt(3), .mul, 6, 7, 0);
    for (stage_layout, 0..) |stage, index| {
        try std.testing.expect(cordic.step() == null);
        for (cordic.pipeline, 0..) |slot, position| try std.testing.expectEqual(position == index, slot != null);
        const entry = &cordic.pipeline[index].?;
        try std.testing.expectEqual(@as(CogId, @enumFromInt(3)), entry.cog);
        if (stage == .iterate and stage.iterate == 0) {
            try std.testing.expectEqual(@as(u64, 6), entry.data.multiply.product);
            try std.testing.expectEqual(@as(u32, 3), entry.data.multiply.multiplier);
            // A downstream result must depend on the live state, rather than a precomputed answer.
            entry.data.multiply.product += 1;
        }
    }
    try std.testing.expectEqual(@as(u32, 43), cordic.pipeline[53].?.data.result.x);
    try std.testing.expect(cordic.step() == null);
    try std.testing.expectEqual(@as(u32, 43), cordic.returns[0].?.result.x);
    try std.testing.expect(cordic.step() == null);
    try std.testing.expect(cordic.step() == null);
    const delivery = cordic.step().?;
    try std.testing.expectEqual(@as(CogId, @enumFromInt(3)), delivery.cog);
    try std.testing.expectEqual(@as(u32, 43), delivery.result.x);
    try std.testing.expect(!cordic.in_flight());
}

test "one shared pipeline accepts consecutive cog slots at full depth and routes every result" {
    var cordic: Cordic = .{};
    var delivered: usize = 0;
    for (0..122) |clock| {
        const delivery = cordic.step();
        if (delivery) |value| {
            try std.testing.expectEqual(delivered + 58, clock);
            try std.testing.expectEqual(@as(CogId, @enumFromInt(@as(u3, @truncate(delivered)))), value.cog);
            try std.testing.expectEqual(@as(u32, 0xffffffff) - @as(u32, @intCast(delivered)), value.result.x);
            try std.testing.expectEqual(@as(u32, @intCast(delivered)), value.result.y);
            delivered += 1;
        }
        if (clock == 54) for (cordic.pipeline) |slot| try std.testing.expect(slot != null);
        if (clock < 64) cordic.submit(@enumFromInt(@as(u3, @truncate(clock))), .mul, @intCast(clock + 1), 0xffffffff, 0);
    }
    try std.testing.expectEqual(@as(usize, 64), delivered);
    try std.testing.expect(!cordic.in_flight());
}

test "reset abandons only one cog's tracking across input arithmetic and return stages" {
    for ([_]usize{ 0, 1, 20, 54, 55, 56, 57 }) |age| {
        var cordic: Cordic = .{};
        var ports: [2]Port = @splat(.{});
        cordic.submit(@enumFromInt(0), .mul, 6, 7, 0);
        ports[0].begin();
        for (0..age) |clock| {
            try std.testing.expect(cordic.step() == null);
            if (clock == 0) {
                cordic.submit(@enumFromInt(1), .mul, 11, 17, 0);
                ports[1].begin();
            }
        }
        cordic.forget(@enumFromInt(0));
        ports[0].reset();
        try std.testing.expect(!ports[0].busy());
        try std.testing.expect(cordic.in_flight());
        if (age == 0) {
            // Make room for another cog when reset happened immediately after submission.
            try std.testing.expect(cordic.step() == null);
            cordic.submit(@enumFromInt(1), .mul, 11, 17, 0);
            ports[1].begin();
        }
        while (cordic.in_flight()) {
            if (cordic.step()) |delivery| ports[@intFromEnum(delivery.cog)].accept(delivery);
        }
        try std.testing.expectEqual(@as(u32, 42), ports[0].result.x);
        try std.testing.expect(ports[0].ready.get(.x));
        try std.testing.expectEqual(@as(u32, 187), ports[1].result.x);
        try std.testing.expect(!ports[1].busy());
        ports[0].reset();
        try std.testing.expect(!ports[0].ready.get(.x));
        try std.testing.expectEqual(@as(u32, 42), ports[0].result.x);
    }
}
