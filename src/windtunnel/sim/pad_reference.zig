//! Test-only scalar reference: independent RC pad with configurable linear/piecewise drivers, leakage and rating diagnostics.
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

/// Report committed driver connectivity; capacitance and leakage do not constitute drive.
pub fn isDriven(pad: *const Pad) bool {
    return pad.state != null and (!pad.last_internal.floating() or
        (pad.last_external.drive_enabled and pad.last_external.max_current != 0));
}

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
    return pad.advance(configuration, internal, external, dt, false);
}

/// Benchmark the same scalar equations with either the original or tiny-array sort.
pub fn advance(pad: *Pad, configuration: *const SystemConfiguration, internal: Driver, external: Source, dt: f64, comptime small_sort: bool) State {
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
    const voltage = solve(configuration, previous, capacitance / dt, internal, external, small_sort);
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
    comptime small_sort: bool,
) f64 {
    if (!configuration.leakage_enabled and internal.floating() and
        (!external.drive_enabled or external.max_current == 0))
    {
        return previous;
    }
    var regions: Regions = .{};
    internal.boundaries(configuration, &regions);
    external.boundaries(&regions);
    if (small_sort) {
        std.sort.insertion(f64, regions.values[0..regions.count], {}, std.sort.asc(f64));
    } else {
        std.mem.sort(f64, regions.values[0..regions.count], {}, std.sort.asc(f64));
    }
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
