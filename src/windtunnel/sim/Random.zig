//! One shared, clocked xoroshiro128** source for all cog random consumers.
//! The production output-bit numbering, absolute polarities and seed lane remain
//! explicitly open. Anonymous tap indices and relative inversions are measured;
//! assigning them to the same numbered output bits is an approximation.

const std = @import("std");
const Random = @This();

s0: u64 = 1,
s1: u64 = 0,
multiply5: u64 = 0,
output: u64 = 0,

/// Advance the [24,16,37] transition and the two-stage [5,7,9] scrambler once.
/// Pipeline registers start cleared; boot entropy and ROM seeding are not modeled.
pub fn step(random: *Random) void {
    random.output = std.math.rotl(u64, random.multiply5, 7) *% 9;
    random.multiply5 = random.s0 *% 5;
    const mixed = random.s1 ^ random.s0;
    random.s0 = std.math.rotl(u64, random.s0, 24) ^ mixed ^ (mixed << 16);
    random.s1 = std.math.rotl(u64, mixed, 37);
}

/// Replace one 32-bit lane, retaining the remaining 96 bits and scrambler state.
/// ponytail: upper S1 follows the historical designer description; production
/// lane and seed-edge ordering remain open pending hardware characterization.
pub fn seed(random: *Random, value: u32) void {
    random.s1 = (random.s1 & 0xffffffff) | (@as(u64, value | 0x80000000) << 32);
}

/// Project the current shared output without consuming or advancing any state.
pub fn word(random: *const Random, cog: u3) u32 {
    var result: u32 = inverted[cog];
    for (taps[cog], 0..) |tap, bit| result ^= @as(u32, @truncate((random.output >> tap) & 1)) << @intCast(bit);
    return result;
}

// Calibrated from data/random/taps.json. Indices identify anonymous observed
// signals, not recovered silicon xoroshiro bit numbers. Each appears in four cogs.
const taps: [8][32]u6 = .{
    .{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 },
    .{ 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63 },
    .{ 2, 44, 54, 36, 60, 11, 33, 53, 26, 13, 20, 32, 40, 61, 24, 41, 58, 6, 12, 17, 29, 37, 48, 50, 18, 3, 35, 9, 38, 16, 49, 56 },
    .{ 42, 8, 55, 5, 57, 63, 59, 34, 23, 46, 39, 47, 51, 10, 43, 21, 30, 1, 4, 22, 27, 15, 0, 62, 45, 19, 52, 31, 7, 25, 14, 28 },
    .{ 12, 43, 42, 21, 62, 6, 14, 8, 22, 10, 32, 25, 49, 4, 35, 47, 31, 51, 23, 1, 45, 20, 13, 56, 41, 38, 59, 34, 9, 24, 37, 26 },
    .{ 48, 36, 50, 46, 52, 58, 5, 3, 19, 55, 54, 29, 17, 7, 16, 0, 27, 61, 44, 39, 18, 11, 63, 53, 40, 15, 28, 2, 30, 33, 60, 57 },
    .{ 62, 53, 1, 19, 51, 40, 56, 48, 10, 21, 47, 31, 0, 18, 14, 55, 42, 43, 8, 33, 9, 16, 20, 30, 17, 49, 7, 61, 45, 32, 27, 41 },
    .{ 13, 60, 50, 63, 28, 23, 3, 11, 12, 36, 25, 46, 34, 29, 59, 44, 58, 54, 5, 38, 39, 37, 4, 35, 57, 22, 52, 26, 2, 24, 15, 6 },
};
const inverted: [8]u32 = .{ 0x76bf1fe0, 0xf616321e, 0xe00b02ce, 0x4f32a1a9, 0x462dda5c, 0x91145fdb, 0x42a69862, 0x3b317f92 };

test "xoroshiro reference sequence and partial seed preserve the output pipeline" {
    // Published P2 collaborator vectors for S0=1, S1=0, after pipeline fill.
    const expected = [_]u64{
        0x1680,             0x1696801680,       0xe682e68000001680, 0x800016f099c09692,
        0xdb9ce96699c368c0, 0xacd4a897e816f3bc, 0x736225bd8540f37d, 0xd5ef4bc8db65564a,
        0xe6844f92c4467f20, 0xecbfdd2089c19276, 0x3e3c8f32277dc824, 0x6135641f1dadd2ca,
        0x0695119b0de23cfa, 0x8837ded221b7094d, 0x99f5fdc2c04a92d6, 0x57262a94b712228c,
    };
    var random: Random = .{};
    try std.testing.expectEqual(@as(u64, 0), random.output);
    random.step();
    try std.testing.expectEqual(@as(u64, 0), random.output);
    for (expected) |value| {
        random.step();
        try std.testing.expectEqual(value, random.output);
    }
    const before = random;
    random.seed(0x12345678);
    try std.testing.expectEqual(before.s0, random.s0);
    try std.testing.expectEqual((before.s1 & 0xffffffff) | 0x9234567800000000, random.s1);
    try std.testing.expectEqual(before.multiply5, random.multiply5);
    try std.testing.expectEqual(before.output, random.output);
    var unseeded = before;
    // Two pipeline outputs remain unaffected; then the overwritten lane reaches S0's scrambler.
    for (0..2) |_| {
        random.step();
        unseeded.step();
        try std.testing.expectEqual(unseeded.output, random.output);
    }
    random.step();
    unseeded.step();
    try std.testing.expect(random.output != unseeded.output);
}

test "cog projections reproduce measured relationships and inversions" {
    // Independent raw calibration words, reconstructed in anonymous class order.
    const observations = [_][9]u64{
        .{ 0xf7639957422ac562, 0x3495da82, 0x175ab49, 0x30403a10, 0x2d914913, 0x4ca544a6, 0x62d24ab8, 0x6928df8d, 0xfa3ee998 },
        .{ 0x65e8ab89673e5b88, 0x11814468, 0x93fe9997, 0x6f16ff6a, 0x3cb37daf, 0xf79f1687, 0x80ae607b, 0xff707e19, 0x13a25a52 },
        .{ 0x956b3df1fa31e460, 0x8c8efb80, 0x637d0fef, 0x10781c50, 0xa6035580, 0x4dec432, 0xc6d913b8, 0x3045d390, 0xfb0ed989 },
        .{ 0xa8a106d9045fc6ec, 0x72e0d90c, 0x5eb734c7, 0xdf41af47, 0x1d1a85c4, 0xd10d9d38, 0x9bce2c18, 0xefd779e8, 0xe1ad3dda },
        .{ 0x37a8809c877c8a86, 0xf1c39566, 0xc1beb282, 0x6d0a6777, 0x75183d3d, 0xfe861354, 0x5bae7cf9, 0x4ef6363c, 0x40a06910 },
    };
    for (observations) |observation| {
        const random: Random = .{ .output = observation[0] };
        for (0..8) |cog| try std.testing.expectEqual(@as(u32, @intCast(observation[cog + 1])), random.word(@intCast(cog)));
    }
    var incidence: [64]u8 = @splat(0);
    for (taps, 0..) |row, cog| for (row) |tap| {
        const mask = @as(u8, 1) << @intCast(cog);
        try std.testing.expect(incidence[tap] & mask == 0);
        incidence[tap] |= mask;
    };
    for (incidence) |mask| {
        try std.testing.expectEqual(@as(u4, 4), @popCount(mask));
        for (0..4) |pair| try std.testing.expectEqual(@as(u4, 1), @popCount(mask & (@as(u8, 3) << @intCast(pair * 2))));
    }
}
