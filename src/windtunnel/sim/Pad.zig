//! Independent RC pad with two current-limited voltage sources and capacitance to ground.
const std = @import("std");
const p2 = @import("p2");
const signal = @import("signal.zig");
const Pad = @This();

// Provisional electrical assumptions, not hardware-calibrated parameters.
pub const supply_voltage: f64 = 3.3;
pub const digital_threshold: f64 = supply_voltage / 2;
pub const internal_capacitance: f64 = 5e-12;
pub const fast_resistance: f64 = 30;

pub const Source = struct {
    voltage: f64,
    resistance: f64,
    max_current: f64 = std.math.inf(f64),
    drive_enabled: bool = true,
    capacitance: f64 = 0,

    pub const float: Source = .{ .voltage = 0, .resistance = 1, .drive_enabled = false };
    pub const high: Source = .{ .voltage = supply_voltage, .resistance = 1 };
    pub const low: Source = .{ .voltage = 0, .resistance = 1 };

    /// Validate a source in volts, ohms, amperes and farads, including disabled sources.
    pub fn validate(source: Source) error{InvalidSource}!void {
        if (!std.math.isFinite(source.voltage) or
            !std.math.isFinite(source.resistance) or source.resistance <= 0 or
            std.math.isNan(source.max_current) or source.max_current < 0 or
            !std.math.isFinite(source.capacitance) or source.capacitance < 0)
        {
            return error.InvalidSource;
        }
    }

    /// Return signed instantaneous current from this source into the pad.
    pub fn current(source: Source, voltage: f64) f64 {
        if (!source.drive_enabled or source.max_current == 0)
            return 0;
        return std.math.clamp(
            (source.voltage - voltage) / source.resistance,
            -source.max_current,
            source.max_current,
        );
    }
};

pub const State = struct {
    voltage: f64,
    /// Positive from the external source into the pad.
    current: f64,
    /// Positive from the P2 driver into the pad.
    internal_current: f64,

    /// Digitize the physical voltage, including retained charge on an undriven pad.
    pub fn digital(state: State) signal.Signal {
        return signal.Signal.from(state.voltage >= digital_threshold);
    }
};

state: ?State = null,
last_internal: Source = .float,
last_external: Source = .float,
last_dt: f64 = 0,
settled: bool = false,

/// Translate the selected digital output and WRPIN drive fields into an internal source.
pub fn internalSource(configuration: p2.types.PinConfiguration, level: signal.Signal, enabled: bool) Source {
    if (!enabled or configuration.isDac())
        return .float;
    const drive = switch (level) {
        .low => configuration.low_drive,
        .high => configuration.high_drive,
    };
    const resistance: f64 = switch (drive) {
        .fast, .current_1ma, .current_100ua, .current_10ua => fast_resistance,
        .resistor_1k5 => 1_500,
        .resistor_15k => 15_000,
        .resistor_150k => 150_000,
        .float => return .float,
    };
    const max_current: f64 = switch (drive) {
        .current_1ma => 1e-3,
        .current_100ua => 100e-6,
        .current_10ua => 10e-6,
        else => std.math.inf(f64),
    };
    return .{
        .voltage = switch (level) {
            .low => 0,
            .high => supply_voltage,
        },
        .resistance = resistance,
        .max_current = max_current,
    };
}

/// Advance one clock using backward Euler; replacing sources or capacitance preserves voltage.
pub fn step(pad: *Pad, internal: Source, external: Source, dt: f64) State {
    std.debug.assert(std.math.isFinite(dt) and dt > 0);
    if (pad.settled and pad.last_dt == dt and
        std.meta.eql(pad.last_internal, internal) and std.meta.eql(pad.last_external, external))
    {
        return pad.state.?;
    }
    const previous = if (pad.state) |state|
        state.voltage
    else
        0;
    const capacitance = internal_capacitance + internal.capacitance + external.capacitance;
    const voltage = solve(previous, capacitance / dt, internal, external);
    const state: State = .{
        .voltage = voltage,
        .current = external.current(voltage),
        .internal_current = internal.current(voltage),
    };
    pad.state = state;
    pad.last_internal = internal;
    pad.last_external = external;
    pad.last_dt = dt;
    pad.settled = voltage == previous;
    return state;
}

/// Solve the monotone capacitor/source equation over at most five affine current regions.
fn solve(previous: f64, capacitor_conductance: f64, internal: Source, external: Source) f64 {
    if ((!internal.drive_enabled or internal.max_current == 0) and
        (!external.drive_enabled or external.max_current == 0))
    {
        return previous;
    }
    const sources = [_]Source{ internal, external };
    var boundaries: [4]f64 = undefined;
    var count: usize = 0;
    for (sources) |source| {
        if (!source.drive_enabled or source.max_current == 0 or !std.math.isFinite(source.max_current))
            continue;
        const span = source.max_current * source.resistance;
        for ([_]f64{ source.voltage - span, source.voltage + span }) |boundary| {
            if (std.math.isFinite(boundary)) {
                boundaries[count] = boundary;
                count += 1;
            }
        }
    }
    std.mem.sort(f64, boundaries[0..count], {}, std.sort.asc(f64));
    var lower = -std.math.inf(f64);
    for (0..count + 1) |region| {
        const upper = if (region < count)
            boundaries[region]
        else
            std.math.inf(f64);
        var numerator = capacitor_conductance * previous;
        var denominator = capacitor_conductance;
        for (sources) |source| {
            if (!source.drive_enabled or source.max_current == 0)
                continue;
            const span = source.max_current * source.resistance;
            if (std.math.isFinite(source.max_current) and upper <= source.voltage - span) {
                numerator += source.max_current;
            } else if (std.math.isFinite(source.max_current) and lower >= source.voltage + span) {
                numerator -= source.max_current;
            } else {
                numerator += source.voltage / source.resistance;
                denominator += 1 / source.resistance;
            }
        }
        const voltage = numerator / denominator;
        if (voltage >= lower and voltage <= upper)
            return voltage;
        lower = upper;
    }
    unreachable; // A positive capacitor conductance guarantees exactly one root.
}

test "resistive divider and signed branch currents conserve charge" {
    var pad: Pad = .{};
    const internal: Source = .{ .voltage = 3.3, .resistance = 1_500 };
    const external: Source = .{ .voltage = 0, .resistance = 1_500, .capacitance = 995e-12 };
    const dt = 1e-6;
    var previous: f64 = 0;
    for (0..100) |_| {
        const state = pad.step(internal, external, dt);
        try std.testing.expect(state.voltage >= previous - 1e-14 and state.voltage <= 1.65 + 1e-14);
        try std.testing.expect(state.internal_current > 0 and state.current < 0);
        try std.testing.expectApproxEqAbs(
            1e-9 * (state.voltage - previous) / dt,
            state.internal_current + state.current,
            1e-15,
        );
        previous = state.voltage;
    }
    try std.testing.expectApproxEqAbs(@as(f64, 1.65), previous, 1e-12);
}

test "current limiting crosses regions and sources can sink current" {
    var pad: Pad = .{};
    const source: Source = .{ .voltage = 3.3, .resistance = 30, .max_current = 10e-6 };
    const dt = 50e-9;
    var previous: f64 = 0;
    for (0..40) |_| {
        const state = pad.step(source, .float, dt);
        try std.testing.expect(state.voltage >= previous and state.voltage <= 3.3);
        try std.testing.expect(state.internal_current >= 0 and state.internal_current <= source.max_current);
        try std.testing.expectApproxEqAbs(
            internal_capacitance * (state.voltage - previous) / dt,
            state.internal_current,
            1e-14,
        );
        previous = state.voltage;
    }
    try std.testing.expectApproxEqAbs(@as(f64, 3.3), previous, 1e-10);
    const sink: Source = .{ .voltage = 0, .resistance = 30, .max_current = 100e-6 };
    const discharged = pad.step(sink, .float, dt);
    try std.testing.expectApproxEqAbs(@as(f64, -100e-6), discharged.internal_current, 1e-14);
    try std.testing.expect(discharged.voltage < previous);
}

test "floating retains charge and changing capacitance preserves voltage" {
    var pad: Pad = .{};
    try std.testing.expect(pad.state == null);
    _ = pad.step(.float, .high, 50e-9);
    const charged = pad.state.?.voltage;
    var load: Source = .float;
    load.capacitance = 1e-6;
    for (0..10) |_| {
        const state = pad.step(.float, load, 1e-3);
        try std.testing.expectApproxEqAbs(charged, state.voltage, 1e-15);
        try std.testing.expectEqual(@as(f64, 0), state.current);
        try std.testing.expectEqual(@as(f64, 0), state.internal_current);
        try std.testing.expectEqual(signal.Signal.high, state.digital());
    }
    const discharged = pad.step(.float, .low, 1);
    try std.testing.expect(std.math.isFinite(discharged.voltage));
    try std.testing.expect(discharged.voltage >= 0 and discharged.voltage < charged);
}

test "RC response converges toward continuous time with smaller clock steps" {
    const source: Source = .{ .voltage = 3.3, .resistance = 1_000, .capacitance = 995e-12 };
    const expected = 3.3 * (1 - @exp(@as(f64, -1)));
    var previous_error: f64 = std.math.inf(f64);
    for ([_]usize{ 1, 10, 100, 1_000 }) |steps| {
        var pad: Pad = .{};
        for (0..steps) |_| {
            _ = pad.step(.float, source, 1e-6 / @as(f64, @floatFromInt(steps)));
        }
        const err = @abs(pad.state.?.voltage - expected);
        try std.testing.expect(err < previous_error);
        previous_error = err;
    }
    try std.testing.expect(previous_error < 0.001);
}

test "source descriptors validate all electrical fields" {
    try Source.high.validate();
    try Source.low.validate();
    try Source.float.validate();
    for ([_]Source{
        .{ .voltage = std.math.nan(f64), .resistance = 1 },
        .{ .voltage = std.math.inf(f64), .resistance = 1 },
        .{ .voltage = 0, .resistance = 0 },
        .{ .voltage = 0, .resistance = -1 },
        .{ .voltage = 0, .resistance = std.math.inf(f64) },
        .{ .voltage = 0, .resistance = std.math.nan(f64) },
        .{ .voltage = 0, .resistance = 1, .max_current = -1 },
        .{ .voltage = 0, .resistance = 1, .max_current = std.math.nan(f64) },
        .{ .voltage = 0, .resistance = 1, .capacitance = -1 },
        .{ .voltage = 0, .resistance = 1, .capacitance = std.math.inf(f64) },
        .{ .voltage = 0, .resistance = 1, .capacitance = std.math.nan(f64) },
    }) |source| {
        try std.testing.expectError(error.InvalidSource, source.validate());
    }
}

test "WRPIN independently selects rail resistance and current limit" {
    const Drive = p2.types.PinConfiguration.Drive;
    for (std.enums.values(Drive)) |drive| {
        const configuration: p2.types.PinConfiguration = .{ .high_drive = drive, .low_drive = .resistor_150k };
        const high = internalSource(configuration, .high, true);
        const low = internalSource(configuration, .low, true);
        try std.testing.expectEqual(@as(f64, 150_000), low.resistance);
        try std.testing.expectEqual(@as(f64, 0), low.voltage);
        try std.testing.expectEqual(drive != .float, high.drive_enabled);
        try std.testing.expect(!internalSource(configuration, .high, false).drive_enabled);
        if (drive != .float) {
            try std.testing.expectEqual(supply_voltage, high.voltage);
            const expected_resistance: f64 = switch (drive) {
                .resistor_1k5 => 1_500,
                .resistor_15k => 15_000,
                .resistor_150k => 150_000,
                else => 30,
            };
            const expected_limit: f64 = switch (drive) {
                .current_1ma => 1e-3,
                .current_100ua => 100e-6,
                .current_10ua => 10e-6,
                else => std.math.inf(f64),
            };
            try std.testing.expectEqual(expected_resistance, high.resistance);
            try std.testing.expectEqual(expected_limit, high.max_current);
        }
    }
    try std.testing.expect(!internalSource(.{ .pad_mode = .dac_124r_75r }, .high, true).drive_enabled);
}

test "two current-limited branches conserve charge through all limiting combinations" {
    for ([_]f64{ 1e-6, 100e-6, std.math.inf(f64) }) |internal_limit| {
        for ([_]f64{ 0, 1e-6, 100e-6, std.math.inf(f64) }) |external_limit| {
            var pad: Pad = .{};
            const internal: Source = .{ .voltage = 3.3, .resistance = 30, .max_current = internal_limit };
            const external: Source = .{ .voltage = 0, .resistance = 100, .max_current = external_limit };
            var previous: f64 = 0;
            for (0..100) |_| {
                const state = pad.step(internal, external, 50e-9);
                try std.testing.expect(std.math.isFinite(state.voltage));
                try std.testing.expect(state.voltage >= 0 and state.voltage <= 3.3 + 1e-14);
                try std.testing.expect(@abs(state.current) <= external_limit);
                try std.testing.expect(@abs(state.internal_current) <= internal_limit);
                try std.testing.expectApproxEqAbs(
                    internal_capacitance * (state.voltage - previous) / 50e-9,
                    state.current + state.internal_current,
                    1e-14,
                );
                previous = state.voltage;
            }
        }
    }
}
