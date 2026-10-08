//!
//! This file implements the basic types used by the Propeller 2.
//!
const std = @import("std");

pub const Flag = enum(u1) {
    unset = 0,
    set = 1,

    pub fn from_bool(value: bool) Flag {
        return @enumFromInt(@intFromBool(value));
    }

    pub fn from_lsb(value: u32) Flag {
        return @enumFromInt(Decomposition.from(value).view.lsb);
    }

    pub fn from_msb(value: u32) Flag {
        return @enumFromInt(Decomposition.from(value).view.msb);
    }

    pub fn from_zero(value: u32) Flag {
        return from_bool(value == 0);
    }

    pub fn from_parity(value: u32) Flag {
        return @enumFromInt(@popCount(value) & 1);
    }

    pub fn as_int(flag: Flag, comptime T: type) T {
        return @intFromEnum(flag);
    }

    pub fn as_mask(flag: Flag, comptime T: type) T {
        return switch (flag) {
            .unset => 0,
            .set => ~@as(T, 0),
        };
    }

    pub fn not(flag: Flag) Flag {
        return switch (flag) {
            .unset => .set,
            .set => .unset,
        };
    }
};

/// A 32-bit wide array of `T` elements.
pub fn BitArray(comptime T: type) type {
    return packed struct(u32) {
        const Array = @This();

        pub const Index = std.math.IntFittingRange(0, len - 1);

        pub const len = @divExact(32, @bitSizeOf(T));
        pub const zero: Array = .{ .raw = 0 };

        raw: u32,

        pub fn from_array(arr: [len]T) Array {
            var out: Array = .zero;
            for (arr, 0..) |v, i| {
                out.set(@intCast(i), v);
            }
            return out;
        }

        pub fn to_array(arr: Array) [len]T {
            var out: [len]T = @splat(0);
            for (&out, 0..) |*d, i| {
                d.* = arr.get(@intCast(i));
            }
            return out;
        }

        pub fn set(arr: *Array, index: Index, value: T) void {
            const shift = @bitSizeOf(T) * @as(u5, index);

            const mask = @as(u32, ~@as(T, 0)) << shift;

            arr.raw &= ~mask;
            arr.raw |= @as(u32, value) << shift;
        }

        pub fn get(arr: Array, index: Index) T {
            const shift = @bitSizeOf(T) * @as(u5, index);
            return @truncate(arr.raw >> shift);
        }
    };
}

/// A struct that allows viewing a `u32` as multiple different types.
pub const Decomposition = packed union(u32) {
    u32: u32,
    i32: i32,
    u16: BitArray(u16),
    u8: BitArray(u8),
    u4: BitArray(u4),
    u2: BitArray(u2),
    u1: BitArray(u1),
    view: packed struct(u32) {
        lsb: u1,
        center: u30,
        msb: u1,
    },

    pub fn from(value: u32) Decomposition {
        return .{ .u32 = value };
    }
};

pub const RGBA8888 = packed struct(u32) {
    r: u8,
    g: u8,
    b: u8,
    a: u8,
};

pub const RGBA565 = packed struct(u16) {
    r: u5,
    g: u6,
    b: u5,
};

/// D[S[9:5]+S[4:0]:S[4:0]]
pub const BitIndexAndCount = packed struct(u32) {
    index: u5,
    count: u5,
    unused: u22,
};

/// D[10:6]+D[5:0]..D[5:0]
pub const PinIndexAndCount = packed struct(u32) {
    index: u6,
    count: u5,
    unused: u21,
};

/// The instruction layout used by SETD/SETS/SETR
pub const InstructionFields = packed struct(u32) {
    s: u9,
    d: u9,
    r: u9,
    unused: u5,
};
