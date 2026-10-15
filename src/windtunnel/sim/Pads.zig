//! Whole-bank SIMD electrical integration; routing remains synchronous in IO.
const std = @import("std");
const Pad = @import("Pad.zig");
const SystemConfiguration = @import("SystemConfiguration.zig");
const signal = @import("signal.zig");
const Pads = @This();

pub const Vector = @Vector(64, f64);
pub const Mask = @Vector(64, bool);
const zero: Vector = @splat(0);
const one: Vector = @splat(1);
const infinity: Vector = @splat(std.math.inf(f64));
const no: Mask = @splat(false);
const yes: Mask = @splat(true);

/// Numeric source planes; scalar writes stage descriptors without creating per-pad objects.
const Sources = struct {
    voltage: Vector = zero,
    resistance: Vector = one,
    max_current: Vector = infinity,
    capacitance: Vector = zero,
    enabled: Mask = no,

    /// Stage one source into its numeric lanes.
    fn set(sources: *Sources, index: usize, source: Pad.Source) void {
        lane(&sources.voltage, index).* = source.voltage;
        lane(&sources.resistance, index).* = source.resistance;
        lane(&sources.max_current, index).* = source.max_current;
        lane(&sources.capacitance, index).* = source.capacitance;
        setBit(&sources.enabled, index, source.drive_enabled and source.max_current != 0);
    }

    /// Identify lanes with changed descriptors, including disconnected capacitance.
    fn changed(a: Sources, b: Sources) Mask {
        return (a.voltage != b.voltage) | (a.resistance != b.resistance) |
            (a.max_current != b.max_current) | (a.capacitance != b.capacitance) | (a.enabled != b.enabled);
    }

    /// Evaluate endpoint source current with the descriptor's exact clamp convention.
    fn current(sources: Sources, voltage: Vector) Vector {
        const raw = (sources.voltage - voltage) / sources.resistance;
        return @select(f64, sources.enabled, @max(-sources.max_current, @min(raw, sources.max_current)), zero);
    }

    /// Prepare three affine regions; unlimited and floating sources use only the middle one.
    fn segments(sources: Sources) [3]Segment {
        const limited = sources.enabled & (sources.max_current != infinity);
        const limit = @select(f64, limited, sources.max_current, zero);
        const span = limit * sources.resistance;
        const lower = @select(f64, limited, sources.voltage - span, -infinity);
        const upper = @select(f64, limited, sources.voltage + span, infinity);
        const g = @select(f64, sources.enabled, one / sources.resistance, zero);
        return .{
            .{ .lower = -infinity, .upper = lower, .offset = limit, .conductance = zero, .valid = limited },
            .{ .lower = lower, .upper = upper, .offset = sources.voltage * g, .conductance = g, .valid = yes },
            .{ .lower = upper, .upper = infinity, .offset = -limit, .conductance = zero, .valid = limited },
        };
    }
};

const Segment = struct {
    lower: Vector = -infinity,
    upper: Vector = infinity,
    offset: Vector = zero,
    conductance: Vector = zero,
    valid: Mask = no,
};
const CurveSegment = struct { lower: f64, upper: f64, offset: f64, conductance: f64 };

voltage: Vector = zero,
current: Vector = zero,
internal_current: Vector = zero,
pin_current: Vector = zero,
undervoltage: Mask = no,
overvoltage: Mask = no,
overcurrent: Mask = no,
settled: Mask = no,
connected: Mask = no,
valid: bool = false,
internal: Sources = .{},
external: Sources = .{},
last_internal: Sources = .{},
fast: Mask = no,
high: Mask = no,
last_fast: Mask = no,
last_high: Mask = no,
external_dirty: bool = true,
internal_regions: [5]Segment = @splat(.{}),
external_regions: [3]Segment = @splat(.{}),
curve_low: [5]CurveSegment = undefined,
curve_high: [5]CurveSegment = undefined,
last_dt: f64 = 0,

/// Broadcast immutable coefficients without ambiguous nested @splat result types.
fn broadcast(value: f64) Vector {
    return @splat(value);
}

/// Read a scalar snapshot without requiring dynamic vector extraction from the backend.
fn read(vector: *const Vector, index: usize) f64 {
    const array: *const [64]f64 = @ptrCast(vector);
    return array[index];
}

/// Read one bit of a packed vector mask.
fn bit(mask: Mask, index: usize) bool {
    const bits: u64 = @bitCast(mask);
    return bits & (@as(u64, 1) << @intCast(index)) != 0;
}

/// Stage one bit without dynamic vector insertion or read-modify-write of numeric planes.
fn setBit(mask: *Mask, index: usize, value: bool) void {
    const flag = @as(u64, 1) << @intCast(index);
    const bits: u64 = @bitCast(mask.*);
    mask.* = @bitCast(if (value)
        bits | flag
    else
        bits & ~flag);
}

/// Obtain a scalar lane pointer for staging; the owned field remains a full 64-lane vector.
fn lane(vector: *Vector, index: usize) *f64 {
    const array: *[64]f64 = @ptrCast(vector);
    return &array[index];
}

/// Initialize immutable curve plans only after Hub has validated its configuration.
pub fn init(pads: *Pads, configuration: *const SystemConfiguration) void {
    pads.* = .{};
    pads.curve_low = curve(configuration, false);
    pads.curve_high = curve(configuration, true);
}

/// Prepare the five mirrored affine regions; the third point defines the extended final slope.
fn curve(configuration: *const SystemConfiguration, high: bool) [5]CurveSegment {
    var drops: [3]f64 = @splat(0);
    var slopes: [3]f64 = @splat(0);
    var offsets: [3]f64 = @splat(0);
    var old_i: f64 = 0;
    var old_v: f64 = 0;
    for (configuration.curve, 0..) |point, i| {
        drops[i] = if (high)
            point.high_drop
        else
            point.low_drop;
        slopes[i] = (point.current - old_i) / (drops[i] - old_v);
        offsets[i] = old_i - slopes[i] * old_v;
        old_i = point.current;
        old_v = drops[i];
    }
    const rail = if (high)
        configuration.vio_voltage
    else
        0;
    const bounds = [_]f64{ -std.math.inf(f64), rail - drops[1], rail - drops[0], rail + drops[0], rail + drops[1], std.math.inf(f64) };
    var result: [5]CurveSegment = undefined;
    for (&result, 0..) |*region, i| {
        const part = [_]usize{ 2, 1, 0, 1, 2 };
        const g = slopes[part[i]];
        const b = offsets[part[i]];
        region.* = .{
            .lower = bounds[i],
            .upper = bounds[i + 1],
            .offset = g * rail + if (i < 2)
                b
            else
                -b,
            .conductance = g,
        };
    }
    return result;
}

/// Stage a validated external source without changing any committed electrical reading.
pub fn setExternal(pads: *Pads, index: usize, source: Pad.Source) void {
    pads.external.set(index, source);
    pads.external_dirty = true;
    setBit(&pads.settled, index, false);
}

/// Collect the already routed internal driver; every pad is collected before any voltage advances.
pub fn setInternal(pads: *Pads, index: usize, driver: Pad.Driver) void {
    switch (driver) {
        .linear => |source| {
            pads.internal.set(index, source);
            setBit(&pads.fast, index, false);
            setBit(&pads.high, index, false);
        },
        .fast => |level| {
            if (!bit(pads.fast, index)) {
                pads.internal.set(index, .float);
            }
            setBit(&pads.fast, index, true);
            setBit(&pads.high, index, level == .high);
        },
    }
}

/// Assemble the scalar public snapshot; only the entire pre-clock snapshot is optional.
pub fn get(pads: *const Pads, index: usize) ?Pad.State {
    if (!pads.valid)
        return null;
    return .{
        .voltage = read(&pads.voltage, index),
        .current = read(&pads.current, index),
        .internal_current = read(&pads.internal_current, index),
        .pin_current = read(&pads.pin_current, index),
        .violations = .{
            .undervoltage = bit(pads.undervoltage, index),
            .overvoltage = bit(pads.overvoltage, index),
            .overcurrent = bit(pads.overcurrent, index),
        },
    };
}

/// Return committed connectivity, excluding capacitive loads and leakage.
pub fn isDriven(pads: *const Pads, index: usize) bool {
    return pads.valid and bit(pads.connected, index);
}

/// Prepare internal linear/curve coefficients without allocating region-search scratch buffers.
fn prepareInternal(pads: *Pads) void {
    const linear = pads.internal.segments();
    for (&pads.internal_regions, 0..) |*target, i| {
        const base: Segment = if (i < linear.len)
            linear[i]
        else
            .{};
        const low = pads.curve_low[i];
        const high = pads.curve_high[i];
        target.* = .{
            .lower = @select(f64, pads.fast, @select(f64, pads.high, broadcast(high.lower), broadcast(low.lower)), base.lower),
            .upper = @select(f64, pads.fast, @select(f64, pads.high, broadcast(high.upper), broadcast(low.upper)), base.upper),
            .offset = @select(f64, pads.fast, @select(f64, pads.high, broadcast(high.offset), broadcast(low.offset)), base.offset),
            .conductance = @select(f64, pads.fast, @select(f64, pads.high, broadcast(high.conductance), broadcast(low.conductance)), base.conductance),
            .valid = pads.fast | base.valid,
        };
    }
}

/// Solve all active lanes with either a direct linear expression or fifteen affine candidates.
fn solve(pads: *const Pads, a: Vector, leakage: Vector, active: Mask, comptime linear_shortcut: bool) Vector {
    const internal_limited = pads.internal.enabled & (pads.internal.max_current != infinity);
    const external_limited = pads.external.enabled & (pads.external.max_current != infinity);
    if (linear_shortcut and !@reduce(.Or, active & (pads.fast | internal_limited | external_limited))) {
        const i = pads.internal_regions[1];
        const e = pads.external_regions[1];
        return (a * pads.voltage + i.offset + e.offset) / (a + leakage + i.conductance + e.conductance);
    }
    var best = pads.voltage;
    var error_distance = infinity;
    const history = a * pads.voltage;
    for (&pads.internal_regions) |*i| {
        for (&pads.external_regions) |*e| {
            const lower = @max(i.lower, e.lower);
            const upper = @min(i.upper, e.upper);
            const candidate = (history + i.offset + e.offset) / (a + leakage + i.conductance + e.conductance);
            const distance = @max(zero, @max(lower - candidate, candidate - upper));
            const choose = i.valid & e.valid & (lower <= upper) & (distance < error_distance);
            best = @select(f64, choose, candidate, best);
            error_distance = @select(f64, choose, distance, error_distance);
        }
    }
    const tolerance = @as(Vector, @splat(32 * std.math.floatEps(f64))) * @max(one, @abs(best));
    std.debug.assert(@reduce(.And, !active | (error_distance <= tolerance)));
    return best;
}

/// Evaluate the committed fast curve at the selected endpoint, including reverse current.
fn fastCurrent(pads: *const Pads, voltage: Vector) Vector {
    var result = zero;
    var found = no;
    for (&pads.internal_regions) |*region| {
        const choose = region.valid & !found & (voltage >= region.lower) & (voltage <= region.upper);
        result = @select(f64, choose, region.offset - region.conductance * voltage, result);
        found |= choose;
    }
    return result;
}

/// Advance one completed hub clock with the bank-wide direct-linear shortcut enabled.
pub fn step(pads: *Pads, configuration: *const SystemConfiguration, dt: f64) void {
    pads.advance(configuration, dt, true);
}

/// Advance the same model with a compile-time kernel choice for tests and benchmarks.
pub fn advance(pads: *Pads, configuration: *const SystemConfiguration, dt: f64, comptime linear_shortcut: bool) void {
    @setFloatMode(.strict);
    std.debug.assert(std.math.isFinite(dt) and dt > 0);
    const changed = pads.internal.changed(pads.last_internal) | (pads.fast != pads.last_fast) |
        (pads.high != pads.last_high);
    const active = !pads.settled | changed | @as(Mask, @splat(!pads.valid or pads.last_dt != dt));
    if (!@reduce(.Or, active))
        return;
    if (!pads.valid or @reduce(.Or, changed)) {
        pads.prepareInternal();
    }
    if (pads.external_dirty) {
        pads.external_regions = pads.external.segments();
    }
    const previous = pads.voltage;
    const chip_capacitance = @as(Vector, @splat(configuration.pad_capacitance)) + pads.internal.capacitance;
    const capacitance = chip_capacitance + pads.external.capacitance;
    const a = capacitance / @as(Vector, @splat(dt));
    const leakage: Vector = @splat(if (configuration.leakage_enabled)
        1 / configuration.leakage_resistance
    else
        0);
    const floating = !(pads.fast | pads.internal.enabled | pads.external.enabled);
    const solved = pads.solve(a, leakage, active, linear_shortcut);
    const retained = floating & @as(Mask, @splat(!configuration.leakage_enabled));
    const voltage = @select(f64, active & !retained, solved, previous);
    const linear_current = pads.internal.current(voltage);
    const internal_current = if (@reduce(.Or, pads.fast & active))
        @select(f64, pads.fast, pads.fastCurrent(voltage), linear_current)
    else
        linear_current;
    const pin_current = chip_capacitance * (voltage - previous) / @as(Vector, @splat(dt)) +
        voltage * leakage - internal_current;
    pads.voltage = voltage;
    pads.current = @select(f64, active, pads.external.current(voltage), pads.current);
    pads.internal_current = @select(f64, active, internal_current, pads.internal_current);
    pads.pin_current = @select(f64, active, pin_current, pads.pin_current);
    pads.undervoltage = voltage < @as(Vector, @splat(configuration.min_pad_voltage));
    pads.overvoltage = voltage > @as(Vector, @splat(configuration.vio_voltage + configuration.max_pad_voltage_margin));
    pads.overcurrent = @abs(pads.pin_current) > @as(Vector, @splat(configuration.max_pin_current));
    pads.connected = pads.fast | pads.internal.enabled | pads.external.enabled;
    pads.settled = (voltage == previous) | !active;
    pads.last_internal = pads.internal;
    pads.last_fast = pads.fast;
    pads.last_high = pads.high;
    pads.last_dt = dt;
    pads.external_dirty = false;
    pads.valid = true;
}

/// Digitize the complete vector bank for the existing digital sampling pipelines.
pub fn digital(pads: *const Pads, configuration: *const SystemConfiguration) [64]signal.Signal {
    const high = pads.voltage >= @as(Vector, @splat(configuration.vio_voltage * configuration.threshold_ratio));
    var result: [64]signal.Signal = @splat(.low);
    for (&result, 0..) |*state, i| {
        state.* = signal.Signal.from(bit(high, i));
    }
    return result;
}

const Source = Pad.Source;
const Violations = Pad.Violations;
const internalDriver = Pad.internalDriver;

/// Exercise the public single-pad test scenarios through the actual 64-lane kernel.
const TestPad = struct {
    bank: Pads = .{},
    state: ?Pad.State = null,

    /// Advance lane zero, keeping all other lanes floating.
    fn step(pad: *TestPad, configuration: *const SystemConfiguration, internal: Pad.Driver, external: Source, dt: f64) Pad.State {
        if (pad.state == null) {
            pad.bank.init(configuration);
        }
        pad.bank.setInternal(0, internal);
        pad.bank.setExternal(0, external);
        pad.bank.step(configuration, dt);
        pad.state = pad.bank.get(0);
        return pad.state.?;
    }
};
test "resistive divider and signed branch currents conserve charge" {
    var pad: TestPad = .{};
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
    var pad: TestPad = .{};
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
    var pad: TestPad = .{};
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
        var pad: TestPad = .{};
        for (0..steps) |_| {
            _ = pad.step(&SystemConfiguration.ideal, .float, source, 1e-6 / @as(f64, @floatFromInt(steps)));
        }
        const err = @abs(pad.state.?.voltage - expected);
        try std.testing.expect(err < previous_error);
        previous_error = err;
    }
    try std.testing.expect(previous_error < 0.001);
}

test "two current-limited branches conserve charge through all limiting combinations" {
    for ([_]f64{ 1e-6, 100e-6, std.math.inf(f64) }) |internal_limit| {
        for ([_]f64{ 0, 1e-6, 100e-6, std.math.inf(f64) }) |external_limit| {
            var pad: TestPad = .{};
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

test "realistic solver reaches loaded curve points and conserves capacitor and leakage current" {
    const configuration = SystemConfiguration.realistic;
    for ([_]signal.Signal{ .low, .high }) |level| {
        const driver = internalDriver(&configuration, .{}, level, true);
        const high = level == .high;
        for (configuration.curve) |point| {
            var pad: TestPad = .{};
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
    var pad: TestPad = .{};
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
    var pad: TestPad = .{};
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
    var pad: TestPad = .{};
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

/// Copy a descriptor into the independently retained scalar reference type.
fn referenceSource(source: Source) Reference.Source {
    return .{
        .voltage = source.voltage,
        .resistance = source.resistance,
        .max_current = source.max_current,
        .drive_enabled = source.drive_enabled,
        .capacitance = source.capacitance,
    };
}
const Reference = @import("pad_reference.zig");

/// Compare a bank against scalar lanes, including conservation and endpoint currents.
fn compareReference(bank: *const Pads, references: *const [64]Reference, configuration: *const SystemConfiguration) !void {
    for (references, 0..) |reference, index| {
        const actual = bank.get(index).?;
        const expected = reference.state.?;
        try std.testing.expectApproxEqAbs(expected.voltage, actual.voltage, 1e-11 * @max(1, @abs(expected.voltage)));
        try std.testing.expectApproxEqAbs(expected.current, actual.current, 1e-11 * @max(1e-3, @abs(expected.current)));
        try std.testing.expectApproxEqAbs(expected.internal_current, actual.internal_current, 1e-11 * @max(1e-3, @abs(expected.internal_current)));
        try std.testing.expectApproxEqAbs(expected.pin_current, actual.pin_current, 1e-11 * @max(1e-3, @abs(expected.pin_current)));
        try std.testing.expectEqual(expected.digital(configuration), actual.digital(configuration));
        try std.testing.expectEqual(expected.violations.undervoltage, actual.violations.undervoltage);
        try std.testing.expectEqual(expected.violations.overvoltage, actual.violations.overvoltage);
        try std.testing.expectEqual(expected.violations.overcurrent, actual.violations.overcurrent);
    }
}

test "heterogeneous vector lanes match scalar reference and uniform kernel through source and clock changes" {
    var random = std.Random.DefaultPrng.init(0x501df64);
    const rng = random.random();
    var custom: SystemConfiguration = .realistic;
    custom.vio_voltage = 4.2;
    custom.threshold_ratio = 0.6;
    custom.pad_capacitance = 13e-12;
    custom.leakage_resistance = 7.5e6;
    for (&custom.curve) |*point| {
        point.current *= 1.2;
        point.low_drop *= 1.7;
        point.high_drop *= 1.3;
    }
    try custom.validate();
    for ([_]SystemConfiguration{ .ideal, .realistic, custom }) |configuration| {
        var bank: Pads = .{};
        bank.init(&configuration);
        var uniform: Pads = bank;
        var references: [64]Reference = @splat(.{});
        var sources: [64]Source = @splat(.float);
        var drivers: [64]Pad.Driver = @splat(.float);
        for (0..64) |i| {
            sources[i] = .{
                .voltage = rng.float(f64) * 6 - 1,
                .resistance = @exp(rng.float(f64) * 12),
                .max_current = if (i % 3 == 0)
                    std.math.inf(f64)
                else
                    rng.float(f64) * 0.03,
                .drive_enabled = i % 7 != 0,
                .capacitance = rng.float(f64) * 1e-9,
            };
            drivers[i] = switch (i % 4) {
                0 => .float,
                1 => .{ .fast = .high },
                2 => .{ .fast = .low },
                else => .{ .linear = .{
                    .voltage = 3.3,
                    .resistance = 30,
                    .max_current = 100e-6,
                } },
            };
            bank.setExternal(i, sources[i]);
            uniform.setExternal(i, sources[i]);
        }
        for (0..80) |clock| {
            const dt: f64 = if (clock < 40)
                50e-9
            else
                5e-9;
            if (clock == 20) {
                sources[17] = .float;
                sources[17].capacitance = 1e-6;
                bank.setExternal(17, sources[17]);
                uniform.setExternal(17, sources[17]);
            }
            if (clock == 30) {
                drivers[29] = .float;
            }
            for (drivers, sources, &references, 0..) |driver, source, *reference, i| {
                bank.setInternal(i, driver);
                uniform.setInternal(i, driver);
                const scalar_driver: Reference.Driver = switch (driver) {
                    .linear => |linear| .{ .linear = referenceSource(linear) },
                    .fast => |level| .{ .fast = level },
                };
                _ = reference.step(&configuration, scalar_driver, referenceSource(source), dt);
            }
            bank.step(&configuration, dt);
            uniform.advance(&configuration, dt, false);
            try compareReference(&bank, &references, &configuration);
            for (0..64) |i| {
                try std.testing.expectApproxEqAbs(read(&bank.voltage, i), read(&uniform.voltage, i), 1e-11);
            }
        }
    }
}

test "settled masks preserve neighboring lanes and staged connectivity" {
    const configuration = SystemConfiguration.ideal;
    var bank: Pads = .{};
    bank.init(&configuration);
    try std.testing.expect(bank.get(63) == null);
    bank.step(&configuration, 50e-9);
    const idle = bank;
    bank.step(&configuration, 50e-9);
    try std.testing.expectEqualDeep(idle, bank);
    bank.setExternal(63, .high);
    try std.testing.expect(!bank.isDriven(63));
    try std.testing.expectEqual(@as(f64, 0), bank.get(63).?.voltage);
    bank.step(&configuration, 50e-9);
    try std.testing.expect(bank.isDriven(63));
    try std.testing.expect(bank.voltage[63] > 1.65);
    for (0..63) |i| {
        try std.testing.expectEqualDeep(idle.get(i), bank.get(i));
    }
    const charged = bank.voltage[63];
    bank.setExternal(63, .float);
    bank.step(&configuration, 50e-9);
    try std.testing.expect(!bank.isDriven(63));
    try std.testing.expectEqual(charged, bank.voltage[63]);
    bank.step(&configuration, 5e-9);
    try std.testing.expectEqual(charged, bank.voltage[63]);
}

test "curve breakpoints and adjacent floats have finite bounded-residual solutions" {
    const configuration = SystemConfiguration.realistic;
    for ([_]signal.Signal{ .low, .high }) |level| {
        var bank: Pads = .{};
        bank.init(&configuration);
        const driver: Pad.Driver = .{ .fast = level };
        const dt = 50e-9;
        const a = configuration.pad_capacitance / dt;
        for (0..64) |i| {
            const point = configuration.curve[i % 3];
            const drop = if (level == .high)
                point.high_drop
            else
                point.low_drop;
            const boundary = if (level == .high)
                configuration.vio_voltage - drop
            else
                drop;
            const v = switch (i % 3) {
                0 => boundary,
                1 => std.math.nextAfter(f64, boundary, std.math.inf(f64)),
                else => std.math.nextAfter(f64, boundary, -std.math.inf(f64)),
            };
            lane(&bank.voltage, i).* = (a * v + v / configuration.leakage_resistance - driver.current(&configuration, v)) / a;
            bank.setInternal(i, driver);
        }
        const previous = bank.voltage;
        bank.step(&configuration, dt);
        for (0..64) |i| {
            const v = read(&bank.voltage, i);
            try std.testing.expect(std.math.isFinite(v));
            const residual = a * (v - read(&previous, i)) + v / configuration.leakage_resistance - driver.current(&configuration, v);
            try std.testing.expectApproxEqAbs(@as(f64, 0), residual, 1e-14);
        }
    }
}
