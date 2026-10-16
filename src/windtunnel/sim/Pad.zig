//! Scalar electrical descriptors, readings, and WRPIN drive mapping for the SIMD Pads bank.
const std = @import("std");
const p2 = @import("p2");
const signal = @import("signal.zig");
const SystemConfiguration = @import("SystemConfiguration.zig");

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
};

const Affine = struct { offset: f64, conductance: f64 };

pub const Driver = union(enum) {
    linear: Source,
    fast: signal.Signal,

    pub const float: Driver = .{ .linear = .float };

    /// Describe current into the pad on one affine region of the configured output curve.
    fn affine(driver: Driver, configuration: *const SystemConfiguration, voltage: f64) Affine {
        switch (driver) {
            .linear => unreachable, // Linear currents use Source.current directly.
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
    pub fn check(configuration: *const SystemConfiguration, voltage: f64, pin_current: f64) Violations {
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

/// Describe a constant-impedance DAC: changing its code changes voltage, never resistance.
pub fn dacDriver(system: *const SystemConfiguration, range: p2.types.PinConfiguration.DacRange, code: u8) Driver {
    const scale: f64 = switch (range) {
        .ohm_990_3v, .ohm_124_3v => 1,
        .ohm_600_2v, .ohm_75_2v => 2.0 / 3.3,
    };
    return .{ .linear = .{
        .voltage = system.vio_voltage * scale * @as(f64, @floatFromInt(code)) / 256,
        .resistance = system.dac_resistance[@intFromEnum(range)],
    } };
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

test "DAC resistance is constant across every code in each physical range" {
    const system = SystemConfiguration.ideal;
    for (0..4) |range| {
        for (0..256) |code| {
            const source = dacDriver(&system, @enumFromInt(range), @intCast(code)).linear;
            try std.testing.expectEqual(system.dac_resistance[range], source.resistance);
            try std.testing.expect(source.drive_enabled);
            try std.testing.expectEqual(@as(f64, 0), source.current(source.voltage));
        }
    }
}
