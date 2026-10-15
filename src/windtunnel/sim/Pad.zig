//! Independent RC pad with configurable linear/piecewise drivers, leakage and rating diagnostics.
const std = @import("std");
const p2 = @import("p2");
const signal = @import("signal.zig");
const SystemConfiguration = @import("SystemConfiguration.zig");
const Pad = @This();

pub const Source = struct {
    voltage: f64,
    resistance: f64,
    max_current: f64 = std.math.inf(f64),
    drive_enabled: bool = true,
    capacitance: f64 = 0,

    pub const float: Source = .{ .voltage = 0, .resistance = 1, .drive_enabled = false };
    pub const high: Source = .{ .voltage = 3.3, .resistance = 1 };
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

    /// Describe this source's affine current on the region containing voltage.
    fn affine(source: Source, voltage: f64) Affine {
        if (!source.drive_enabled or source.max_current == 0)
            return .{ .offset = 0, .conductance = 0 };
        const raw = (source.voltage - voltage) / source.resistance;
        if (raw > source.max_current)
            return .{ .offset = source.max_current, .conductance = 0 };
        if (raw < -source.max_current)
            return .{ .offset = -source.max_current, .conductance = 0 };
        return .{ .offset = source.voltage / source.resistance, .conductance = 1 / source.resistance };
    }

    /// Add finite current-limit transition voltages to the solver's region boundaries.
    fn boundaries(source: Source, regions: *Regions) void {
        if (!source.drive_enabled or source.max_current == 0 or !std.math.isFinite(source.max_current))
            return;
        const span = source.max_current * source.resistance;
        regions.append(source.voltage - span);
        regions.append(source.voltage + span);
    }
};

const Affine = struct { offset: f64, conductance: f64 };

pub const Driver = union(enum) {
    linear: Source,
    fast: signal.Signal,

    pub const float: Driver = .{ .linear = .float };

    /// Return the internal branch capacitance, if a synthetic linear source supplies one.
    fn capacitance(driver: Driver) f64 {
        return switch (driver) {
            .linear => |source| source.capacitance,
            .fast => 0,
        };
    }

    /// Determine whether the internal source is electrically disconnected.
    fn floating(driver: Driver) bool {
        return switch (driver) {
            .linear => |source| !source.drive_enabled or source.max_current == 0,
            .fast => false,
        };
    }

    /// Describe current into the pad on one affine region of the configured output curve.
    fn affine(driver: Driver, configuration: *const SystemConfiguration, voltage: f64) Affine {
        switch (driver) {
            .linear => |source| return source.affine(voltage),
            .fast => |level| {
                const high = level == .high;
                const drop = if (high)
                    configuration.vio_voltage - voltage
                else
                    voltage;
                var previous_current: f64 = 0;
                var previous_drop: f64 = 0;
                for (configuration.curve, 0..) |point, index| {
                    const point_drop = if (high)
                        point.high_drop
                    else
                        point.low_drop;
                    if (@abs(drop) <= point_drop or index == configuration.curve.len - 1) {
                        const conductance = (point.current - previous_current) / (point_drop - previous_drop);
                        const intercept = previous_current - conductance * previous_drop;
                        const signed_intercept = if (drop < 0)
                            -intercept
                        else
                            intercept;
                        return .{
                            .offset = if (high)
                                signed_intercept + conductance * configuration.vio_voltage
                            else
                                -signed_intercept,
                            .conductance = conductance,
                        };
                    }
                    previous_current = point.current;
                    previous_drop = point_drop;
                }
                unreachable;
            },
        }
    }

    /// Read signed current into the pad without advancing the electrical state.
    pub fn current(driver: Driver, configuration: *const SystemConfiguration, voltage: f64) f64 {
        return switch (driver) {
            .linear => |source| source.current(voltage),
            .fast => blk: {
                const coefficients = driver.affine(configuration, voltage);
                break :blk coefficients.offset - coefficients.conductance * voltage;
            },
        };
    }

    /// Add the mirrored curve or current-limit transition voltages.
    fn boundaries(driver: Driver, configuration: *const SystemConfiguration, regions: *Regions) void {
        switch (driver) {
            .linear => |source| source.boundaries(regions),
            .fast => |level| {
                const high = level == .high;
                const rail = if (high)
                    configuration.vio_voltage
                else
                    0;
                for (configuration.curve) |point| {
                    const drop = if (high)
                        point.high_drop
                    else
                        point.low_drop;
                    regions.append(rail - drop);
                    regions.append(rail + drop);
                }
            },
        }
    }
};

pub const Violations = struct {
    undervoltage: bool,
    overvoltage: bool,
    overcurrent: bool,

    /// Report whether the completed snapshot exceeds any configured absolute rating.
    pub fn any(violations: Violations) bool {
        return violations.undervoltage or violations.overvoltage or violations.overcurrent;
    }

    /// Compare completed electrical quantities with absolute ratings, including equality within the limits.
    fn check(configuration: *const SystemConfiguration, voltage: f64, pin_current: f64) Violations {
        return .{
            .undervoltage = voltage < configuration.min_pad_voltage,
            .overvoltage = voltage > configuration.vio_voltage + configuration.max_pad_voltage_margin,
            .overcurrent = @abs(pin_current) > configuration.max_pin_current,
        };
    }
};

pub const State = struct {
    voltage: f64,
    /// Positive from the external source into the pad node, including external load capacitance.
    current: f64,
    /// Positive from the P2 output driver into the pad.
    internal_current: f64,
    /// Positive from the pad into the chip; excludes external capacitance current.
    pin_current: f64,
    violations: Violations,

    /// Digitize the physical voltage using the initialized chip's threshold ratio.
    pub fn digital(state: State, configuration: *const SystemConfiguration) signal.Signal {
        return signal.Signal.from(state.voltage >= configuration.vio_voltage * configuration.threshold_ratio);
    }
};

state: ?State = null,
last_internal: Driver = .float,
last_external: Source = .float,
last_dt: f64 = 0,
settled: bool = false,

/// Translate selected digital output and WRPIN fields into the initialized chip's internal driver.
pub fn internalDriver(
    system: *const SystemConfiguration,
    configuration: p2.types.PinConfiguration,
    level: signal.Signal,
    enabled: bool,
) Driver {
    if (!enabled or configuration.isDac())
        return .float;
    const drive = switch (level) {
        .low => configuration.low_drive,
        .high => configuration.high_drive,
    };
    if (drive == .fast and system.fast_model == .piecewise)
        return .{ .fast = level };
    const resistance: f64 = switch (drive) {
        .fast => switch (level) {
            .low => system.fast_low_resistance,
            .high => system.fast_high_resistance,
        },
        .current_1ma, .current_100ua, .current_10ua => system.current_drive_resistance,
        .resistor_1k5 => system.resistor_1k5,
        .resistor_15k => system.resistor_15k,
        .resistor_150k => system.resistor_150k,
        .float => return .float,
    };
    const max_current: f64 = switch (drive) {
        .current_1ma => system.current_1ma,
        .current_100ua => system.current_100ua,
        .current_10ua => system.current_10ua,
        else => std.math.inf(f64),
    };
    return .{ .linear = .{
        .voltage = switch (level) {
            .low => 0,
            .high => system.vio_voltage,
        },
        .resistance = resistance,
        .max_current = max_current,
    } };
}

/// Advance one clock; configuration is fixed for this Pad's entire initialized lifetime.
pub fn step(pad: *Pad, configuration: *const SystemConfiguration, internal: Driver, external: Source, dt: f64) State {
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
    const chip_capacitance = configuration.pad_capacitance + internal.capacitance();
    const capacitance = chip_capacitance + external.capacitance;
    const voltage = solve(configuration, previous, capacitance / dt, internal, external);
    const internal_current = internal.current(configuration, voltage);
    const leakage = if (configuration.leakage_enabled)
        voltage / configuration.leakage_resistance
    else
        0;
    const pin_current = chip_capacitance * (voltage - previous) / dt + leakage - internal_current;
    const state: State = .{
        .voltage = voltage,
        .current = external.current(voltage),
        .internal_current = internal_current,
        .pin_current = pin_current,
        .violations = Violations.check(configuration, voltage, pin_current),
    };
    pad.state = state;
    pad.last_internal = internal;
    pad.last_external = external;
    pad.last_dt = dt;
    // A cached equilibrium has no capacitor current on subsequent clocks.
    pad.settled = voltage == previous;
    return state;
}

const Regions = struct {
    values: [8]f64 = undefined,
    count: usize = 0,

    /// Append one finite transition; fast curves need six and an external limiter needs two.
    fn append(regions: *Regions, voltage: f64) void {
        if (std.math.isFinite(voltage)) {
            regions.values[regions.count] = voltage;
            regions.count += 1;
        }
    }
};

/// Solve the monotone capacitor, output-curve, external-source and leakage equation.
fn solve(
    configuration: *const SystemConfiguration,
    previous: f64,
    capacitor_conductance: f64,
    internal: Driver,
    external: Source,
) f64 {
    if (!configuration.leakage_enabled and internal.floating() and
        (!external.drive_enabled or external.max_current == 0))
    {
        return previous;
    }
    var regions: Regions = .{};
    internal.boundaries(configuration, &regions);
    external.boundaries(&regions);
    std.mem.sort(f64, regions.values[0..regions.count], {}, std.sort.asc(f64));
    const leakage_conductance = if (configuration.leakage_enabled)
        1 / configuration.leakage_resistance
    else
        0;
    var lower = -std.math.inf(f64);
    for (0..regions.count + 1) |region| {
        const upper = if (region < regions.count)
            regions.values[region]
        else
            std.math.inf(f64);
        if (lower == upper)
            continue;
        const sample = if (!std.math.isFinite(lower)) blk: {
            break :blk if (std.math.isFinite(upper))
                upper - @max(1, @abs(upper))
            else
                0;
        } else if (!std.math.isFinite(upper))
            lower + @max(1, @abs(lower))
        else
            lower / 2 + upper / 2;
        const a = internal.affine(configuration, sample);
        const b = external.affine(sample);
        const numerator = capacitor_conductance * previous + a.offset + b.offset;
        const denominator = capacitor_conductance + leakage_conductance + a.conductance + b.conductance;
        const voltage = numerator / denominator;
        if (voltage >= lower and voltage <= upper)
            return voltage;
        lower = upper;
    }
    unreachable; // Positive capacitance and monotone branch currents guarantee a unique root.
}

test "resistive divider and signed branch currents conserve charge" {
    var pad: Pad = .{};
    const internal: Source = .{ .voltage = 3.3, .resistance = 1_500 };
    const external: Source = .{ .voltage = 0, .resistance = 1_500, .capacitance = 995e-12 };
    const dt = 1e-6;
    var previous: f64 = 0;
    for (0..100) |_| {
        const state = pad.step(&SystemConfiguration.ideal, .{ .linear = internal }, external, dt);
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
        const state = pad.step(&SystemConfiguration.ideal, .{ .linear = source }, .float, dt);
        try std.testing.expect(state.voltage >= previous and state.voltage <= 3.3);
        try std.testing.expect(state.internal_current >= 0 and state.internal_current <= source.max_current);
        try std.testing.expectApproxEqAbs(
            SystemConfiguration.ideal.pad_capacitance * (state.voltage - previous) / dt,
            state.internal_current,
            1e-14,
        );
        previous = state.voltage;
    }
    try std.testing.expectApproxEqAbs(@as(f64, 3.3), previous, 1e-10);
    const sink: Source = .{ .voltage = 0, .resistance = 30, .max_current = 100e-6 };
    const discharged = pad.step(&SystemConfiguration.ideal, .{ .linear = sink }, .float, dt);
    try std.testing.expectApproxEqAbs(@as(f64, -100e-6), discharged.internal_current, 1e-14);
    try std.testing.expect(discharged.voltage < previous);
}

test "floating retains charge and changing capacitance preserves voltage" {
    var pad: Pad = .{};
    try std.testing.expect(pad.state == null);
    _ = pad.step(&SystemConfiguration.ideal, .float, .high, 50e-9);
    const charged = pad.state.?.voltage;
    var load: Source = .float;
    load.capacitance = 1e-6;
    for (0..10) |_| {
        const state = pad.step(&SystemConfiguration.ideal, .float, load, 1e-3);
        try std.testing.expectApproxEqAbs(charged, state.voltage, 1e-15);
        try std.testing.expectEqual(@as(f64, 0), state.current);
        try std.testing.expectEqual(@as(f64, 0), state.internal_current);
        try std.testing.expectEqual(signal.Signal.high, state.digital(&SystemConfiguration.ideal));
    }
    const discharged = pad.step(&SystemConfiguration.ideal, .float, .low, 1);
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
            _ = pad.step(&SystemConfiguration.ideal, .float, source, 1e-6 / @as(f64, @floatFromInt(steps)));
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
        const high = internalDriver(&SystemConfiguration.ideal, configuration, .high, true).linear;
        const low = internalDriver(&SystemConfiguration.ideal, configuration, .low, true).linear;
        try std.testing.expectEqual(@as(f64, 150_000), low.resistance);
        try std.testing.expectEqual(@as(f64, 0), low.voltage);
        try std.testing.expectEqual(drive != .float, high.drive_enabled);
        try std.testing.expect(!internalDriver(&SystemConfiguration.ideal, configuration, .high, false).linear.drive_enabled);
        if (drive != .float) {
            try std.testing.expectEqual(SystemConfiguration.ideal.vio_voltage, high.voltage);
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
    try std.testing.expect(!internalDriver(&SystemConfiguration.ideal, .{ .pad_mode = .dac_124r_75r }, .high, true).linear.drive_enabled);
}

test "two current-limited branches conserve charge through all limiting combinations" {
    for ([_]f64{ 1e-6, 100e-6, std.math.inf(f64) }) |internal_limit| {
        for ([_]f64{ 0, 1e-6, 100e-6, std.math.inf(f64) }) |external_limit| {
            var pad: Pad = .{};
            const internal: Source = .{ .voltage = 3.3, .resistance = 30, .max_current = internal_limit };
            const external: Source = .{ .voltage = 0, .resistance = 100, .max_current = external_limit };
            var previous: f64 = 0;
            for (0..100) |_| {
                const state = pad.step(&SystemConfiguration.ideal, .{ .linear = internal }, external, 50e-9);
                try std.testing.expect(std.math.isFinite(state.voltage));
                try std.testing.expect(state.voltage >= 0 and state.voltage <= 3.3 + 1e-14);
                try std.testing.expect(@abs(state.current) <= external_limit);
                try std.testing.expect(@abs(state.internal_current) <= internal_limit);
                try std.testing.expectApproxEqAbs(
                    SystemConfiguration.ideal.pad_capacitance * (state.voltage - previous) / 50e-9,
                    state.current + state.internal_current,
                    1e-14,
                );
                previous = state.voltage;
            }
        }
    }
}

test "datasheet fast curves match points, interpolate, extrapolate and reverse continuously" {
    const configuration = SystemConfiguration.realistic;
    for ([_]signal.Signal{ .low, .high }) |level| {
        const driver = internalDriver(&configuration, .{}, level, true);
        const high = level == .high;
        for (configuration.curve) |point| {
            const drop = if (high)
                point.high_drop
            else
                point.low_drop;
            const voltage = if (high)
                configuration.vio_voltage - drop
            else
                drop;
            const expected = if (high)
                point.current
            else
                -point.current;
            try std.testing.expectApproxEqAbs(expected, driver.current(&configuration, voltage), 1e-15);
            const reverse = if (high)
                configuration.vio_voltage + drop
            else
                -drop;
            try std.testing.expectApproxEqAbs(-expected, driver.current(&configuration, reverse), 1e-15);
            const below = driver.current(&configuration, voltage - 1e-9);
            const above = driver.current(&configuration, voltage + 1e-9);
            try std.testing.expectApproxEqAbs(below, above, 1e-9);
        }
        const middle = if (high)
            configuration.vio_voltage - 88e-3
        else
            87.5e-3;
        const extended = if (high)
            configuration.vio_voltage - 785e-3
        else
            685e-3;
        const direction: f64 = if (high)
            1
        else
            -1;
        try std.testing.expectApproxEqAbs(direction * 5.5e-3, driver.current(&configuration, middle), 1e-15);
        try std.testing.expectApproxEqAbs(direction * 40e-3, driver.current(&configuration, extended), 1e-15);
    }
}

test "realistic solver reaches loaded curve points and conserves capacitor and leakage current" {
    const configuration = SystemConfiguration.realistic;
    for ([_]signal.Signal{ .low, .high }) |level| {
        const driver = internalDriver(&configuration, .{}, level, true);
        const high = level == .high;
        for (configuration.curve) |point| {
            var pad: Pad = .{};
            const target = if (high)
                configuration.vio_voltage - point.high_drop
            else
                point.low_drop;
            const leakage = target / configuration.leakage_resistance;
            const external: Source = .{
                .voltage = if (high)
                    0
                else
                    configuration.vio_voltage,
                .resistance = 1,
                .max_current = if (high)
                    point.current - leakage
                else
                    point.current + leakage,
            };
            var previous: f64 = 0;
            for (0..100) |_| {
                const state = pad.step(&configuration, driver, external, 50e-9);
                try std.testing.expectApproxEqAbs(
                    configuration.pad_capacitance * (state.voltage - previous) / 50e-9,
                    state.internal_current + state.current - state.voltage / configuration.leakage_resistance,
                    1e-14,
                );
                try std.testing.expect(std.math.isFinite(state.voltage));
                previous = state.voltage;
            }
            try std.testing.expectApproxEqAbs(target, previous, 1e-10);
        }
    }
}

test "realistic floating charge decays through leakage without external terminal current" {
    const configuration = SystemConfiguration.realistic;
    var pad: Pad = .{};
    _ = pad.step(&configuration, .float, .high, 50e-9);
    const charged = pad.state.?.voltage;
    const dt = 1e-6;
    var previous = charged;
    for (0..100) |_| {
        const state = pad.step(&configuration, .float, .float, dt);
        try std.testing.expect(state.voltage > 0 and state.voltage < previous);
        try std.testing.expectEqual(@as(f64, 0), state.current);
        try std.testing.expectApproxEqAbs(@as(f64, 0), state.pin_current, 1e-18);
        previous = state.voltage;
    }
    const factor = 1 + dt / (configuration.pad_capacitance * configuration.leakage_resistance);
    try std.testing.expectApproxEqAbs(charged / std.math.pow(f64, factor, 100), previous, 1e-12);
}

test "rating boundaries are inclusive and overcurrent excludes external capacitor charging" {
    const configuration = SystemConfiguration.ideal;
    const maximum = configuration.vio_voltage + configuration.max_pad_voltage_margin;
    try std.testing.expect(!Violations.check(&configuration, configuration.min_pad_voltage, configuration.max_pin_current).any());
    try std.testing.expect(!Violations.check(&configuration, maximum, -configuration.max_pin_current).any());
    try std.testing.expect(Violations.check(&configuration, configuration.min_pad_voltage - 1e-9, 0).undervoltage);
    try std.testing.expect(Violations.check(&configuration, maximum + 1e-9, 0).overvoltage);
    try std.testing.expect(Violations.check(&configuration, 0, configuration.max_pin_current + 1e-9).overcurrent);
    try std.testing.expect(Violations.check(&configuration, 0, -configuration.max_pin_current - 1e-9).overcurrent);
    var pad: Pad = .{};
    var external: Source = .high;
    external.capacitance = 1e-6;
    const state = pad.step(&configuration, .float, external, 50e-9);
    try std.testing.expect(state.current > 1);
    try std.testing.expect(state.pin_current > 0 and state.pin_current < 1e-3);
    try std.testing.expect(!state.violations.any());
    try std.testing.expectApproxEqAbs(
        state.current - external.capacitance * state.voltage / 50e-9,
        state.pin_current,
        1e-14,
    );
}

test "absolute rating violations report, recover and never clamp voltage or driver current" {
    const configuration = SystemConfiguration.ideal;
    var pad: Pad = .{};
    const driven = internalDriver(&configuration, .{}, .low, true);
    const contested = pad.step(&configuration, driven, .high, 50e-9);
    try std.testing.expect(contested.violations.overcurrent);
    try std.testing.expect(contested.pin_current > configuration.max_pin_current);
    const above = pad.step(&configuration, .float, .{ .voltage = 5, .resistance = 1 }, 50e-9);
    try std.testing.expect(above.voltage > configuration.vio_voltage + configuration.max_pad_voltage_margin);
    try std.testing.expect(above.violations.overvoltage);
    const below = pad.step(&configuration, .float, .{ .voltage = -1, .resistance = 1 }, 50e-9);
    try std.testing.expect(below.voltage < configuration.min_pad_voltage);
    try std.testing.expect(below.violations.undervoltage);
    const recovered = pad.step(&configuration, .float, .low, 50e-9);
    try std.testing.expect(!recovered.violations.any());
}
