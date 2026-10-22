//! Shared human-readable time formatting for capture inspection and Scope.
const std = @import("std");
const CaptureTime = @This();

femtoseconds: u128,

/// Round femtoseconds to the selected display quantum without overflowing large timestamps.
fn roundTime(femtoseconds: u128, quantum: u128) u128 {
    return femtoseconds / quantum + @intFromBool(femtoseconds % quantum >= (quantum + 1) / 2);
}

/// Select ns/us/ms/s and format exactly six decimal places using integer arithmetic throughout.
pub fn format(time: CaptureTime, writer: *std.Io.Writer) std.Io.Writer.Error!void {
    const femtoseconds = time.femtoseconds;
    const Unit = struct { label: []const u8, quantum: u128 };
    const units = [_]Unit{
        .{ .label = "ns", .quantum = 1 },
        .{ .label = "us", .quantum = 1_000 },
        .{ .label = "ms", .quantum = 1_000_000 },
        .{ .label = "s", .quantum = 1_000_000_000 },
    };
    var unit: usize = 0;
    for (units, 0..) |candidate, index| {
        if (femtoseconds >= candidate.quantum * 1_000_000) {
            unit = index;
        }
    }
    var rounded = roundTime(femtoseconds, units[unit].quantum);
    if (rounded >= 1_000_000_000 and unit + 1 < units.len) {
        unit += 1;
        rounded = roundTime(femtoseconds, units[unit].quantum);
    }
    try writer.print("{d}.{d:0>6} {s}", .{
        rounded / 1_000_000, rounded % 1_000_000, units[unit].label,
    });
}

test "capture time units have six decimals, nanosecond minimum, and exact integer rounding" {
    const Case = struct { femtoseconds: u128, text: []const u8 };
    const cases = [_]Case{
        .{ .femtoseconds = 0, .text = "0.000000 ns" },
        .{ .femtoseconds = 1, .text = "0.000001 ns" },
        .{ .femtoseconds = 999_999_999, .text = "999.999999 ns" },
        .{ .femtoseconds = 1_000_000_000, .text = "1.000000 us" },
        .{ .femtoseconds = 23_083_333_333, .text = "23.083333 us" },
        .{ .femtoseconds = 23_083_333_500, .text = "23.083334 us" },
        .{ .femtoseconds = 999_999_999_500, .text = "1.000000 ms" },
        .{ .femtoseconds = 1_000_000_000_000_000, .text = "1.000000 s" },
        .{ .femtoseconds = std.math.maxInt(u128), .text = "340282366920938463463374.607432 s" },
    };
    for (cases) |case| {
        var buffer: [64]u8 = undefined;
        try std.testing.expectEqualStrings(case.text, try std.fmt.bufPrint(&buffer, "{f}", .{CaptureTime{ .femtoseconds = case.femtoseconds }}));
    }
}
