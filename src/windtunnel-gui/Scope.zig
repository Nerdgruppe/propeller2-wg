//! Host-owned Scope controls and bounded waveform drawing over the core block chains.
const std = @import("std");
const zgui = @import("zgui");
const windtunnel = @import("windtunnel");
const Capture = windtunnel.Capture;
const CaptureTime = windtunnel.CaptureTime;
const Scope = @This();
const Viewport = @import("Viewport.zig");

/// Read ImGui wheel input through the small binding extension.
extern fn windtunnelScopeMouseWheel() f32;

visible: bool = false,
digital_visible: [64]bool = @splat(false),
analog_visible: [64]bool = @splat(false),
recording: ?Capture = null,
memory_mib: usize = Capture.default_memory / (1024 * 1024),
overflow: Capture.Overflow = .rolling,
failure: ?anyerror = null,
path: [1024:0]u8 = @splat(0),
follow: bool = true,
fit_all: bool = true,
view: Viewport = .{ .width = 0 },
drag_origin: u128 = 0,
dragged: bool = false,
cursors: [2]?u128 = .{ null, null },

/// Detach before releasing storage; a hidden Scope continues recording until explicitly frozen.
pub fn deinit(scope: *Scope, hub: *windtunnel.Hub) void {
    if (scope.recording) |*capture| {
        hub.capture = null;
        capture.deinit();
        scope.recording = null;
    }
}

/// Allocate first so a failed new capture preserves the existing recording and attachment.
pub fn start(scope: *Scope, hub: *windtunnel.Hub, allocator: std.mem.Allocator) !void {
    const bytes = try std.math.mul(usize, scope.memory_mib, 1024 * 1024);
    const capture = try Capture.init(allocator, bytes, scope.overflow, hub.io.captureState(hub.io.pads));
    scope.deinit(hub);
    scope.recording = capture;
    hub.attachCapture(&scope.recording.?);
    scope.failure = null;
    scope.view.start = 0;
    scope.view.width = 0;
    scope.cursors = .{ null, null };
    scope.follow = true;
    scope.fit_all = true;
}

/// Freeze the retained data and release the simulation's full-stop policy attachment.
pub fn freeze(scope: *Scope, hub: *windtunnel.Hub) void {
    hub.capture = null;
    if (scope.recording) |*capture| {
        if (capture.metadata().status == .recording) {
            capture.metadata().status = .frozen;
        }
    }
}

/// Choose traces for the selected I/O pin without adding controls to the pad grid.
pub fn pinControls(scope: *Scope, pin: usize) void {
    zgui.pushIntId(@intCast(pin));
    defer zgui.popId();

    _ = zgui.checkbox("Analog##scope", .{ .v = &scope.analog_visible[pin] });
    zgui.sameLine(.{});
    _ = zgui.checkbox("Digital##scope", .{ .v = &scope.digital_visible[pin] });
}

/// Keep view arithmetic in integers and subtract its origin before any floating-point conversion.
fn constrain(scope: *Scope, capture: *const Capture) void {
    const sampling: Viewport.Sampling = .{ .step = capture.metadataConst().minimum_substep };
    scope.view.constrain(
        capture.metadataConst().start,
        capture.metadataConst().end,
        scope.fit_all,
        scope.follow,
        sampling.minimumWidth(),
    );
}

/// Draw capture controls and one shared time axis for every visible pin lane.
pub fn drawContents(scope: *Scope, hub: *windtunnel.Hub, allocator: std.mem.Allocator, io: ?std.Io) void {
    const active = if (scope.recording) |*capture|
        capture.metadata().status == .recording
    else
        false;
    zgui.beginDisabled(.{ .disabled = active });
    _ = zgui.inputScalar("Memory (MiB)", usize, .{ .v = &scope.memory_mib });
    const overflow_label = switch (scope.overflow) {
        .rolling => "Overflow: rolling",
        .stop => "Overflow: pause simulation",
    };
    if (zgui.button(overflow_label, .{})) {
        scope.overflow = switch (scope.overflow) {
            .rolling => .stop,
            .stop => .rolling,
        };
    }
    zgui.sameLine(.{});
    if (zgui.button("New capture", .{})) {
        scope.start(hub, allocator) catch |err| {
            scope.failure = err;
        };
    }
    zgui.endDisabled();
    zgui.sameLine(.{});
    if (zgui.button("Freeze", .{})) {
        scope.freeze(hub);
    }
    if (scope.failure) |err| {
        zgui.textColored(.{ 1, 0.3, 0.2, 1 }, "Scope: {t}", .{err});
    }
    const capture = if (scope.recording) |*recording|
        recording
    else
        return;
    zgui.text("{t} | {Bi:.2} / {Bi:.2} | {f} .. {f}", .{
        capture.metadataConst().status,
        capture.memoryUsed(),
        capture.memoryCapacity(),
        CaptureTime{ .femtoseconds = capture.metadataConst().start },
        CaptureTime{ .femtoseconds = capture.metadataConst().end },
    });
    _ = zgui.inputText("Export path", .{ .buf = &scope.path });
    const path = std.mem.sliceTo(&scope.path, 0);
    zgui.beginDisabled(.{ .disabled = io == null or path.len == 0 });
    if (zgui.button("Save capture", .{})) {
        windtunnel.saveCapture(capture, io.?, path) catch |err| {
            scope.failure = err;
        };
    }
    zgui.endDisabled();
    if (scope.view.width == 0) {
        scope.view.width = @max(@as(u128, 1), capture.metadataConst().end - capture.metadataConst().start);
    }
    if (zgui.button("Fit", .{})) {
        scope.fit_all = true;
        scope.view.start = capture.metadataConst().start;
        scope.view.width = @max(@as(u128, 1), capture.metadataConst().end - capture.metadataConst().start);
    }
    zgui.sameLine(.{});
    _ = zgui.checkbox("Follow latest", .{ .v = &scope.follow });
    scope.constrain(capture);
    zgui.text("{f} .. {f}", .{
        CaptureTime{ .femtoseconds = scope.view.start },
        CaptureTime{ .femtoseconds = scope.view.start +| scope.view.width },
    });
    if (scope.cursors[0]) |a| {
        zgui.text("A: {f}", .{CaptureTime{ .femtoseconds = a }});
    }
    if (scope.cursors[1]) |b| {
        zgui.text("B: {f}", .{CaptureTime{ .femtoseconds = b }});
    }
    if (scope.cursors[0] != null and scope.cursors[1] != null) {
        const a = scope.cursors[0].?;
        const b = scope.cursors[1].?;
        timeDelta("B-A", a, b);
    }
    const child = zgui.beginChild("Scope lanes", .{});
    defer zgui.endChild();

    if (!child)
        return;
    const sampling: Viewport.Sampling = .{ .step = capture.metadataConst().minimum_substep };
    var next_view: ?Viewport = null;
    for (0..64) |pin| {
        zgui.pushIntId(@intCast(pin));
        defer zgui.popId();

        if (scope.analog_visible[pin] or scope.digital_visible[pin]) {
            if (scope.lane(capture, pin, sampling)) |view| {
                next_view = view;
            }
        }
    }
    if (next_view) |view| {
        scope.view = view;
        scope.follow = false;
        scope.fit_all = false;
        scope.constrain(capture);
    }
}

const Bin = struct {
    min: f32 = std.math.inf(f32),
    max: f32 = -std.math.inf(f32),
    first: f32 = 0,
    last: f32 = 0,
    clipped: bool = false,
};

/// Decode only the displayed level; clipping remains a separate bin property.
fn sampleValue(comptime Sample: type, sample: Sample) f32 {
    return if (Sample == Capture.DigitalSample)
        switch (sample.value) {
            .low => 0,
            .high => 3.3,
        }
    else
        @floatCast(Capture.volts(sample.value));
}

/// Accumulate held intervals into pixels, retaining excursions even when both edges share one pixel.
fn accumulate(bins: []Bin, first: usize, last: usize, value: f32, clipped: bool) void {
    for (bins[first .. last + 1]) |*bin| {
        if (bin.min == std.math.inf(f32)) {
            bin.first = value;
        }
        bin.min = @min(bin.min, value);
        bin.max = @max(bin.max, value);
        bin.last = value;
        bin.clipped = bin.clipped or clipped;
    }
}

const LaneGeometry = struct {
    origin: [2]f32,
    width: f32,
    height: f32,
};

const analog_color: u32 = 0xffff8050;
const digital_color: u32 = 0xff60df60;

/// Draw one voltage-scaled lane per pin, with analog first and digital overlaid at 0 or 3.3 V.
fn lane(scope: *Scope, capture: *const Capture, pin: usize, sampling: Viewport.Sampling) ?Viewport {
    zgui.text("P{d} (-2 .. +4.5 V)", .{pin});
    const width = @max(@as(f32, 1), zgui.getContentRegionAvail()[0]);
    const origin = zgui.getCursorScreenPos();
    const height: f32 = 70;
    const clicked = zgui.invisibleButton("waveform", .{
        .w = width,
        .h = height,
        .flags = .{ .mouse_button_left = true, .mouse_button_middle = true },
    });
    const canvas = zgui.getWindowDrawList();
    const clip_min = canvas.getClipRectMin();
    const clip_max = canvas.getClipRectMax();
    if (origin[1] + height < clip_min[1] or origin[1] > clip_max[1])
        return null;
    canvas.addRectFilled(.{ .pmin = origin, .pmax = .{ origin[0] + width, origin[1] + height }, .col = 0xff201a16 });
    if (sampling.grid(scope.view, width)) |lattice| {
        var grid = lattice;
        while (grid.next()) |timestamp| {
            const x = origin[0] + scope.view.position(timestamp, width);
            canvas.addLine(.{
                .p1 = .{ x, origin[1] },
                .p2 = .{ x, origin[1] + height },
                .col = 0x183f7f9f,
            });
        }
    }
    const geometry: LaneGeometry = .{ .origin = origin, .width = width, .height = height };
    if (scope.analog_visible[pin]) {
        scope.trace(Capture.AnalogSample, capture, capture.analogChannelsConst()[pin], geometry, analog_color);
    }
    if (scope.digital_visible[pin]) {
        scope.trace(Capture.DigitalSample, capture, capture.digitalChannelsConst()[pin], geometry, digital_color);
    }
    var next_view: ?Viewport = null;
    if (zgui.isItemActivated()) {
        scope.drag_origin = scope.view.start;
        scope.dragged = false;
    }
    if (zgui.isItemActive()) {
        const button: zgui.MouseButton = if (zgui.isMouseDown(.middle))
            .middle
        else
            .left;
        if (zgui.isMouseDragging(button, -1)) {
            scope.dragged = true;
            var view = scope.view;
            view.pan(
                capture.metadataConst().start,
                capture.metadataConst().end,
                scope.drag_origin,
                @as(f64, zgui.getMouseDragDelta(button, .{})[0]) / width,
                sampling.minimumWidth(),
            );
            next_view = view;
        }
    }
    if (zgui.isItemHovered(.{})) {
        zgui.setItemKeyOwner(.mouse_wheel_y);
        const fraction = std.math.clamp((zgui.getMousePos()[0] - origin[0]) / width, 0, 1);
        var cursor: Viewport.Cursor = .{
            .timestamp = scope.view.start +| Viewport.fraction(scope.view.width, fraction),
        };
        const mouse_x = fraction * width;
        if (scope.analog_visible[pin]) {
            scope.snapTrace(Capture.AnalogSample, capture, capture.analogChannelsConst()[pin], &cursor, mouse_x, width);
        }
        if (scope.digital_visible[pin]) {
            scope.snapTrace(Capture.DigitalSample, capture, capture.digitalChannelsConst()[pin], &cursor, mouse_x, width);
        }
        const timestamp = cursor.timestamp;
        if (clicked and zgui.isMouseReleased(.left) and !scope.dragged) {
            scope.cursors[0] = timestamp;
        }
        if (zgui.isMouseClicked(.right)) {
            scope.cursors[1] = timestamp;
        }
        const wheel = windtunnelScopeMouseWheel();
        if (wheel != 0) {
            var view = next_view orelse scope.view;
            view.zoom(capture.metadataConst().start, capture.metadataConst().end, fraction, wheel, sampling.minimumWidth());
            next_view = view;
        }
        const x = origin[0] + scope.view.position(timestamp, width);
        canvas.addLine(.{ .p1 = .{ x, origin[1] }, .p2 = .{ x, origin[1] + height }, .col = 0xff808080 });
        if (zgui.beginTooltip()) {
            defer zgui.endTooltip();

            zgui.text("P{d}  {f}", .{ pin, CaptureTime{ .femtoseconds = timestamp } });
            for (scope.cursors, 0..) |persistent, index| {
                if (persistent) |time| {
                    const label = if (index == 0)
                        "t-A"
                    else
                        "t-B";
                    timeDelta(label, time, timestamp);
                }
            }
            if (scope.analog_visible[pin]) {
                hoverValue(Capture.AnalogSample, capture, capture.analogChannelsConst()[pin], timestamp);
            }
            if (scope.digital_visible[pin]) {
                hoverValue(Capture.DigitalSample, capture, capture.digitalChannelsConst()[pin], timestamp);
            }
        }
    }
    for (scope.cursors) |timestamp| {
        if (timestamp) |time| {
            if (time >= scope.view.start and time <= scope.view.start +| scope.view.width) {
                const x = origin[0] + scope.view.position(time, width);
                canvas.addLine(.{ .p1 = .{ x, origin[1] }, .p2 = .{ x, origin[1] + height }, .col = 0xfff0e0e0 });
            }
        }
    }
    if (scope.analog_visible[pin]) {
        scope.cursorValues(Capture.AnalogSample, capture, capture.analogChannelsConst()[pin], geometry, analog_color, 0);
    }
    if (scope.digital_visible[pin]) {
        const offset: f32 = if (scope.analog_visible[pin])
            16
        else
            0;
        scope.cursorValues(Capture.DigitalSample, capture, capture.digitalChannelsConst()[pin], geometry, digital_color, offset);
    }
    var resets = capture.iterator(Capture.ResetSample, capture.resetChannelConst().*);
    resets.seek(scope.view.start);
    while (resets.next()) |sample| {
        if (sample.timestamp > scope.view.start +| scope.view.width)
            break;
        if (sample.timestamp < scope.view.start)
            continue;
        const x = origin[0] + scope.view.position(sample.timestamp, width);
        canvas.addLine(.{ .p1 = .{ x, origin[1] }, .p2 = .{ x, origin[1] + height }, .col = 0xff40a0ff });
        canvas.addText(.{ x + 2, origin[1] + height - 16 }, 0xff40a0ff, "reset:{t}", .{sample.value});
    }
    return next_view;
}

/// Rasterize a trace on the shared voltage scale, preserving per-pixel extrema and clipping markers.
fn trace(scope: *const Scope, comptime Sample: type, capture: *const Capture, channel: anytype, geometry: LaneGeometry, color: u32) void {
    const digital = Sample == Capture.DigitalSample;
    const origin = geometry.origin;
    const width = geometry.width;
    const height = geometry.height;
    const canvas = zgui.getWindowDrawList();
    const columns: usize = @intFromFloat(@min(@as(f32, 2048), @max(@as(f32, 1), @ceil(width))));
    var bins: [2048]Bin = @splat(.{});
    var cursor = capture.iterator(Sample, channel);
    cursor.seek(scope.view.start);
    var current = cursor.next() orelse return;
    var next = cursor.next();
    while (next) |sample| {
        if (sample.timestamp > scope.view.start)
            break;
        current = sample;
        next = cursor.next();
    }
    const until = @min(capture.metadataConst().end, scope.view.start +| scope.view.width);
    while (current.timestamp <= until) {
        const end = if (next) |sample|
            @min(until, sample.timestamp)
        else
            until;
        const x0: usize = @min(columns - 1, @as(usize, @intFromFloat(scope.view.position(current.timestamp, @floatFromInt(columns)))));
        const x1: usize = @min(columns - 1, @as(usize, @intFromFloat(scope.view.position(end, @floatFromInt(columns)))));
        const clipped = if (digital)
            false
        else
            current.value == 0 or current.value == 65535;
        accumulate(bins[0..columns], x0, x1, sampleValue(Sample, current), clipped);
        current = next orelse break;
        next = cursor.next();
    }
    const low: f32 = -2;
    const scale: f32 = 6.5;
    var previous_y: ?f32 = null;
    for (bins[0..columns], 0..) |bin, index| {
        if (bin.min == std.math.inf(f32))
            continue;
        const x = origin[0] + @as(f32, @floatFromInt(index)) * width / @as(f32, @floatFromInt(columns));
        const bottom = origin[1] + height - 5;
        const amplitude = height - 10;
        const first_y = bottom - (bin.first - low) / scale * amplitude;
        const last_y = bottom - (bin.last - low) / scale * amplitude;
        if (bin.clipped) {
            canvas.addRectFilled(.{
                .pmin = .{ x, origin[1] },
                .pmax = .{ x + width / @as(f32, @floatFromInt(columns)), origin[1] + 3 },
                .col = 0xff4050ff,
            });
        }
        if (previous_y) |y| {
            canvas.addLine(.{ .p1 = .{ x - width / @as(f32, @floatFromInt(columns)), y }, .p2 = .{ x, first_y }, .col = color });
        }
        canvas.addLine(.{
            .p1 = .{ x, bottom - (bin.min - low) / scale * amplitude },
            .p2 = .{ x, bottom - (bin.max - low) / scale * amplitude },
            .col = color,
        });
        previous_y = last_y;
    }
}

/// Show each enabled trace's cursor value in its own color and text row.
fn cursorValues(scope: *const Scope, comptime Sample: type, capture: *const Capture, channel: anytype, geometry: LaneGeometry, color: u32, y_offset: f32) void {
    const digital = Sample == Capture.DigitalSample;
    const origin = geometry.origin;
    const width = geometry.width;
    const canvas = zgui.getWindowDrawList();
    for (scope.cursors, 0..) |timestamp, index| {
        if (timestamp) |time| {
            if (time >= scope.view.start and time <= scope.view.start +| scope.view.width) {
                const x = origin[0] + scope.view.position(time, width);
                const value = valueAt(Sample, capture, channel, time);
                if (value) |sample| {
                    const name = if (index == 0)
                        "A"
                    else
                        "B";
                    if (digital) {
                        canvas.addText(.{ x + 2, origin[1] + y_offset }, color, "{s}:{t}", .{ name, sample.value });
                    } else {
                        const clipped = if (sample.value == 0 or sample.value == 65535)
                            " CLAMPED"
                        else
                            "";
                        canvas.addText(.{ x + 2, origin[1] + y_offset }, color, "{s}:{d:.4}V{s}", .{
                            name, Capture.volts(sample.value), clipped,
                        });
                    }
                }
            }
        }
    }
}

/// Read the held sample at a timestamp, including the last transition at coincident times.
fn valueAt(comptime Sample: type, capture: *const Capture, channel: anytype, time: u128) ?Sample {
    var at = capture.iterator(Sample, channel);
    at.seek(time);
    var value: ?Sample = null;
    while (at.next()) |sample| {
        if (sample.timestamp > time)
            break;
        value = sample;
    }
    return value;
}

/// Show the actual held voltage of each enabled trace without changing either measurement cursor.
fn hoverValue(comptime Sample: type, capture: *const Capture, channel: anytype, time: u128) void {
    const sample = valueAt(Sample, capture, channel, time) orelse return;
    if (Sample == Capture.DigitalSample) {
        zgui.textColored(.{ 0.38, 0.87, 0.38, 1 }, "Digital: {d:.1} V ({t})", .{
            sampleValue(Sample, sample), sample.value,
        });
    } else {
        const clipped = if (sample.value == 0 or sample.value == 65535)
            " CLAMPED"
        else
            "";
        zgui.textColored(.{ 0.31, 0.5, 1, 1 }, "Analog: {d:.4} V{s}", .{
            Capture.volts(sample.value), clipped,
        });
    }
}

/// Show a signed cursor difference with the shared capture time formatter.
fn timeDelta(label: []const u8, from: u128, to: u128) void {
    const difference = if (to >= from)
        to - from
    else
        from - to;
    const sign = if (to >= from)
        ""
    else
        "-";
    zgui.text("{s}: {s}{f}", .{ label, sign, CaptureTime{ .femtoseconds = difference } });
}

/// Search only the pointer's three-pixel neighborhood for actual transitions of an enabled trace.
fn snapTrace(scope: *const Scope, comptime Sample: type, capture: *const Capture, channel: anytype, cursor: *Viewport.Cursor, mouse_x: f32, width: f32) void {
    const target = scope.view.start +| Viewport.fraction(scope.view.width, mouse_x / width);
    const radius = Viewport.fraction(scope.view.width, @min(@as(f64, 1), 3.0 / width)) +| 1;
    const first = target -| radius;
    const end = target +| radius;
    var at = capture.iterator(Sample, channel);
    at.seek(first);
    var previous = at.next() orelse return;
    while (at.next()) |sample| {
        if (sample.timestamp > end)
            break;
        if (sample.timestamp >= first and sample.value != previous.value) {
            cursor.consider(scope.view, sample.timestamp, mouse_x, width);
        }
        previous = sample;
    }
}
