//! Fixed-capacity delay lines that retain their complete history when the active delay changes.
const std = @import("std");

/// Build an allocation-free delay line supporting zero through max_len elapsed steps of delay.
pub fn Pipeline(comptime T: type, comptime max_len: usize) type {
    return struct {
        const Self = @This();

        values: [max_len + 1]T,
        length: usize,

        /// Fill every retained age and select the maximum delay.
        pub fn init(initial: T) Self {
            return .{ .values = @splat(initial), .length = max_len };
        }

        /// Read the selected output without advancing the pipeline.
        pub fn current(self: *const Self) T {
            return self.values[self.length];
        }

        /// Append one value, advance all retained ages, and return the new selected output.
        pub fn step(self: *Self, value: T) T {
            std.mem.copyBackwards(T, self.values[1..], self.values[0..max_len]);
            self.values[0] = value;
            return self.current();
        }

        /// Select another retained age immediately; an invalid length leaves all state unchanged.
        pub fn resize(self: *Self, length: usize) error{InvalidLength}!void {
            if (length > max_len)
                return error.InvalidLength;
            self.length = length;
        }

        /// Read a retained age, with zero identifying the latest append, even beyond the active delay.
        pub fn tap(self: *const Self, age: usize) T {
            std.debug.assert(age <= max_len);
            return self.values[age];
        }

        /// Inspect or edit active pending stages, newest first, excluding the current output.
        pub fn pending(self: *Self) []T {
            return self.values[0..self.length];
        }
    };
}

test "initialized delays zero one and two expose the latest step through current" {
    const expected = [3][4]u32{
        .{ 1, 2, 3, 4 },
        .{ 9, 1, 2, 3 },
        .{ 9, 9, 1, 2 },
    };
    for (expected, 0..) |sequence, delay| {
        var pipe = Pipeline(u32, 2).init(9);
        try std.testing.expectEqual(@as(u32, 9), pipe.current());
        try pipe.resize(delay);
        for (sequence, 1..) |value, input| {
            try std.testing.expectEqual(value, pipe.step(@intCast(input)));
            try std.testing.expectEqual(value, pipe.current());
            try std.testing.expectEqual(@as(u32, @intCast(input)), pipe.tap(0));
        }
    }
}

test "zero capacity passes values through and has no pending stages" {
    var pipe = Pipeline(bool, 0).init(false);
    try std.testing.expect(!pipe.current());
    try pipe.resize(0);
    try std.testing.expect(pipe.step(true));
    try std.testing.expect(pipe.current());
    try std.testing.expect(!pipe.step(false));
    try std.testing.expectEqual(@as(usize, 0), pipe.pending().len);
    try std.testing.expectError(error.InvalidLength, pipe.resize(1));
}

test "resizing retaps the complete history without advancing or flushing" {
    var pipe = Pipeline(u32, 3).init(0);
    for (1..5) |value| {
        _ = pipe.step(@intCast(value));
    }
    try std.testing.expectEqual(@as(u32, 1), pipe.current());
    try pipe.resize(1);
    try std.testing.expectEqual(@as(u32, 3), pipe.current());
    try std.testing.expectEqual(@as(u32, 4), pipe.step(5));
    try std.testing.expectEqual(@as(u32, 2), pipe.tap(3));
    try pipe.resize(3);
    try std.testing.expectEqual(@as(u32, 2), pipe.current());
    try pipe.resize(0);
    try std.testing.expectEqual(@as(u32, 5), pipe.current());
    const before = pipe;
    try std.testing.expectError(error.InvalidLength, pipe.resize(4));
    try std.testing.expectEqualDeep(before, pipe);
}

test "optional aggregate pending stages can be edited without changing the current output" {
    const Payload = struct { id: u32, tracked: bool };
    var pipe = Pipeline(?Payload, 2).init(null);
    try std.testing.expectEqual(@as(?Payload, null), pipe.step(.{ .id = 7, .tracked = true }));
    try std.testing.expectEqual(@as(usize, 2), pipe.pending().len);
    pipe.pending()[0].?.tracked = false;
    try std.testing.expectEqual(@as(?Payload, null), pipe.current());
    try std.testing.expectEqual(@as(?Payload, null), pipe.step(null));
    try std.testing.expectEqualDeep(@as(?Payload, .{ .id = 7, .tracked = false }), pipe.step(null));
}
