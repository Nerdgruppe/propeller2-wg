//! Integer-origin navigation for the Scope's femtosecond time axis.
const std = @import("std");
const Viewport = @This();

start: u128 = 0,
width: u128 = 1,

/// Scale a duration by a normalized screen coordinate without rounding large absolute times.
pub fn fraction(duration: u128, coordinate: f64) u128 {
    const denominator: u64 = 1 << 32;
    const numerator: u64 = @intFromFloat(std.math.clamp(coordinate, 0, 1) * denominator);
    return @intCast((@as(u256, duration) * numerator) / denominator);
}

/// Fit retained data with a minimum span, padding short recordings and optionally following their end.
pub fn constrain(view: *Viewport, first: u128, end: u128, fit: bool, follow: bool, minimum: u128) void {
    const floor = @max(@as(u128, 1), minimum);
    const extent = @max(floor, end - first);
    view.width = if (fit)
        extent
    else
        std.math.clamp(view.width, floor, extent);
    const latest = @max(first, end -| view.width);
    view.start = if (follow)
        latest
    else
        std.math.clamp(view.start, first, latest);
}

/// Zoom smoothly around the pointer's timestamp, clamping only at the recording boundaries.
pub fn zoom(view: *Viewport, first: u128, end: u128, anchor: f64, wheel: f32, minimum: u128) void {
    view.constrain(first, end, false, false, minimum);
    const timestamp = view.start +| fraction(view.width, anchor);
    const floor = @max(@as(u128, 1), minimum);
    const extent = @max(floor, end - first);
    const scaled = @as(f64, @floatFromInt(view.width)) * @exp2(-@as(f64, wheel) / 4);
    view.width = if (scaled >= @as(f64, @floatFromInt(extent)))
        extent
    else if (scaled <= @as(f64, @floatFromInt(floor)))
        floor
    else
        @intFromFloat(scaled);
    view.start = timestamp -| fraction(view.width, anchor);
    view.constrain(first, end, false, false, minimum);
}

/// Pan from the gesture's original position so small motion never accumulates rounding error.
pub fn pan(view: *Viewport, first: u128, end: u128, origin: u128, displacement: f64, minimum: u128) void {
    const distance = @as(f64, @floatFromInt(view.width)) * @abs(displacement);
    const shift: u128 = if (distance >= @as(f64, @floatFromInt(std.math.maxInt(u128))))
        std.math.maxInt(u128)
    else
        @intFromFloat(distance);
    view.start = if (displacement > 0)
        origin -| shift
    else
        origin +| shift;
    view.constrain(first, end, false, false, minimum);
}

/// Convert a timestamp to lane pixels after subtracting the integer viewport origin.
pub fn position(view: Viewport, timestamp: u128, pixels: f32) f32 {
    const relative = std.math.clamp(timestamp, view.start, view.start +| view.width) - view.start;
    return @as(f32, @floatFromInt(relative)) / @as(f32, @floatFromInt(view.width)) * pixels;
}

pub const Cursor = struct {
    timestamp: u128,
    distance: f32 = 3,

    /// Snap to the nearest actual edge strictly within three pixels; stable ties prefer the earlier edge.
    pub fn consider(cursor: *Cursor, view: Viewport, edge: u128, mouse_x: f32, pixels: f32) void {
        if (edge < view.start or edge > view.start +| view.width)
            return;
        const distance = @abs(view.position(edge, pixels) - mouse_x);
        if (distance < cursor.distance or
            (distance == cursor.distance and distance < 3 and edge < cursor.timestamp))
        {
            cursor.timestamp = edge;
            cursor.distance = distance;
        }
    }
};

pub const Sampling = struct {
    step: u128,

    /// Require ten minimum substeps, rounding upward to the capture timestamp's whole-femtosecond units.
    pub fn minimumWidth(sampling: Sampling) u128 {
        return @max(1, @as(u128, @intCast((@as(u256, sampling.step) * 10 + std.math.maxInt(u64)) >> 64)));
    }

    /// Enumerate a global reference lattice only when adjacent minimum substeps are more than five pixels apart.
    pub fn grid(sampling: Sampling, view: Viewport, pixels: f32) ?Grid {
        if (sampling.step == 0)
            return null;
        const pixel_spacing = @as(f64, @floatFromInt(sampling.step)) / 18446744073709551616.0 /
            @as(f64, @floatFromInt(view.width)) * pixels;
        if (pixel_spacing <= 5)
            return null;
        const first: u256 = @as(u256, view.start) << 64;
        return .{
            .step = sampling.step,
            .index = (first + sampling.step - 1) / sampling.step,
            .end = view.start +| view.width,
        };
    }
};

pub const Grid = struct {
    step: u128,
    index: u256,
    end: u128,

    /// Yield potential sample times without converting the large absolute origin to floating point.
    pub fn next(grid: *Grid) ?u128 {
        const timestamp = grid.index * grid.step >> 64;
        if (timestamp > grid.end)
            return null;
        grid.index += 1;
        return @intCast(timestamp);
    }
};

test "pointer zoom and drag preserve the time axis at large origins and retained boundaries" {
    const first = std.math.maxInt(u128) - 100_000;
    const end = first + 90_000;
    var view: Viewport = .{ .start = first + 20_000, .width = 40_000 };
    const anchor = view.start + fraction(view.width, 0.25);
    view.zoom(first, end, 0.25, 4, 1);
    try std.testing.expectEqual(@as(u128, 20_000), view.width);
    try std.testing.expectEqual(anchor, view.start + fraction(view.width, 0.25));
    view.zoom(first, end, 0.25, -4, 1);
    try std.testing.expectEqual(@as(u128, 40_000), view.width);
    try std.testing.expectEqual(anchor, view.start + fraction(view.width, 0.25));
    const origin = view.start;
    view.pan(first, end, origin, 0.125, 1);
    try std.testing.expectEqual(origin - 5000, view.start);
    view.pan(first, end, origin, -0.125, 1);
    try std.testing.expectEqual(origin + 5000, view.start);
    view.pan(first, end, origin, 10, 1);
    try std.testing.expectEqual(first, view.start);
    view.pan(first, end, origin, -10, 1);
    try std.testing.expectEqual(end - view.width, view.start);
    view.zoom(first, end, 1, -1000, 1);
    try std.testing.expectEqual(first, view.start);
    try std.testing.expectEqual(end - first, view.width);
    view.zoom(first, end, 0, 1000, 1);
    try std.testing.expectEqual(@as(u128, 1), view.width);
    view = .{ .start = 0, .width = std.math.maxInt(u128) };
    view.zoom(0, std.math.maxInt(u128), 1, -1, 1);
    try std.testing.expectEqual(std.math.maxInt(u128), view.width);
    view.constrain(end, end, true, true, 1);
    try std.testing.expectEqual(end, view.start);
    try std.testing.expectEqual(@as(u128, 1), view.width);
}

test "edge snapping is strict, nearest, stable across traces, and relative to large timestamps" {
    const origin = std.math.maxInt(u128) - 1000;
    const view: Viewport = .{ .start = origin, .width = 1000 };
    var cursor: Cursor = .{ .timestamp = origin + 500 };
    cursor.consider(view, origin + 503, 500, 1000);
    try std.testing.expectEqual(origin + 500, cursor.timestamp);
    cursor.consider(view, origin + 502, 500, 1000);
    try std.testing.expectEqual(origin + 502, cursor.timestamp);
    cursor.consider(view, origin + 499, 500, 1000);
    try std.testing.expectEqual(origin + 499, cursor.timestamp);
    cursor.consider(view, origin + 501, 500, 1000);
    try std.testing.expectEqual(origin + 499, cursor.timestamp);
    cursor.consider(view, origin - 1, 0, 1000);
    try std.testing.expectEqual(origin + 499, cursor.timestamp);
}

test "minimum substep sets ten-slot zoom floor and globally aligned grid above five pixels" {
    const sampling: Sampling = .{ .step = @as(u128, 10) << 64 };
    try std.testing.expectEqual(@as(u128, 100), sampling.minimumWidth());
    var view: Viewport = .{ .start = 27, .width = 1000 };
    const anchor = view.start + fraction(view.width, 0.5);
    view.zoom(0, 2000, 0.5, 1000, sampling.minimumWidth());
    try std.testing.expectEqual(@as(u128, 100), view.width);
    try std.testing.expectEqual(anchor, view.start + fraction(view.width, 0.5));
    view.start = 27;
    try std.testing.expect(sampling.grid(view, 50) == null);
    var grid = sampling.grid(view, 51).?;
    try std.testing.expectEqual(@as(u128, 30), grid.next().?);
    var last: u128 = 0;
    var count: usize = 1;
    while (grid.next()) |timestamp| {
        last = timestamp;
        count += 1;
    }
    try std.testing.expectEqual(@as(usize, 10), count);
    try std.testing.expectEqual(@as(u128, 120), last);
    view.constrain(0, 3, true, true, sampling.minimumWidth());
    try std.testing.expectEqual(@as(u128, 100), view.width);
    try std.testing.expectEqual(@as(u128, 0), view.start);
    const fractional: Sampling = .{ .step = @as(u128, 3) << 63 };
    try std.testing.expectEqual(@as(u128, 15), fractional.minimumWidth());
    view = .{ .start = 1, .width = 15 };
    grid = fractional.grid(view, 120).?;
    try std.testing.expectEqual(@as(u128, 1), grid.next().?);
    try std.testing.expectEqual(@as(u128, 3), grid.next().?);
    try std.testing.expectEqual(@as(u128, 4), grid.next().?);
    const unknown: Sampling = .{ .step = 0 };
    try std.testing.expect(unknown.grid(view, 120) == null);
    try std.testing.expectEqual(@as(u128, 1), unknown.minimumWidth());
}
