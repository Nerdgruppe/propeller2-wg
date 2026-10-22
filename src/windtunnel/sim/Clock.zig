//! Stateful oscillator clocks, measured PLL phase profiles and one shared physical-time interval.
const std = @import("std");
const p2 = @import("p2");
pub const Configuration = @import("ClockConfiguration.zig");
const profiles = @import("clock_profiles.zig");
const Clock = @This();
const Mode = p2.types.ClockMode;

pub const Coverage = enum { ideal, measured, uncharacterized, nontracking };
pub const Interval = struct { quiet: u128, period: u128 };
pub const Time = struct {
    femtoseconds: u128 = 0,
    fraction: u64 = 0,

    /// Carry fractional femtoseconds without overflowing long-running physical timestamps.
    pub fn add(time: *Time, q64: u128) void {
        const sum = @addWithOverflow(time.fraction, @as(u64, @truncate(q64)));
        time.fraction = sum[0];
        time.femtoseconds += (q64 >> 64) + sum[1];
    }
};
const Oscillator = struct {
    specification: Configuration.Oscillator,
    offset: f64,

    /// Draw a reproducible initial value inside asymmetric uncertainty bounds.
    fn init(specification: Configuration.Oscillator, random: std.Random) Oscillator {
        return .{
            .specification = specification,
            .offset = if (specification.isIdeal())
                0
            else
                random.float(f64) * (specification.below + specification.above) - specification.below,
        };
    }

    /// Reflect a small random walk at the bounds; this is a simulation assumption, not measured jitter.
    fn step(oscillator: *Oscillator, random: std.Random) void {
        const spec = oscillator.specification;
        const span = spec.below + spec.above;
        if (spec.isIdeal())
            return;
        oscillator.offset += (random.float(f64) * 2 - 1) * span / 4096;
        if (oscillator.offset > spec.above) {
            oscillator.offset = 2 * spec.above - oscillator.offset;
        } else if (oscillator.offset < -spec.below) {
            oscillator.offset = -2 * spec.below - oscillator.offset;
        }
    }

    /// Read the current physical oscillator frequency in Hz.
    fn frequency(oscillator: Oscillator) f64 {
        return oscillator.specification.frequency + oscillator.offset;
    }
};
const Response = struct { curve: *const profiles.Curve, started: f64, initial: f64 };
const Sample = struct { frequency: f64, boundary: f64 };
const Factors = struct { d: f64, m: f64, p: f64 };

configuration: Configuration,
random: std.Random.DefaultPrng,
rcfast: Oscillator,
rcslow: Oscillator,
crystal: Oscillator,
mode: Mode,
coverage: Coverage,
unsafe_postdivider_up: bool = false,
xi_started: ?f64 = null,
response: ?Response = null,
nontracking_vco: ?f64 = null,
seconds: f64 = 0,
elapsed: Time = .{},
last_interval: Interval = .{ .quiet = 0, .period = 0 },
cached_frequency: f64 = 0,
cached_period: u128 = 0,

/// Initialize a reproducible independent clock RNG; the P2's own RNG is never consumed.
pub fn init(configuration: Configuration) Clock {
    var random = std.Random.DefaultPrng.init(configuration.seed);
    const rng = random.random();
    const rcfast = Oscillator.init(configuration.rcfast, rng);
    const rcslow = Oscillator.init(configuration.rcslow, rng);
    const crystal = Oscillator.init(configuration.crystal, rng);
    return .{
        .configuration = configuration,
        .random = random,
        .rcfast = rcfast,
        .rcslow = rcslow,
        .crystal = crystal,
        .mode = @bitCast(@as(u32, 0xf0)),
        .coverage = if (configuration.isIdeal()) .ideal else .measured,
    };
}

/// Return decoded PLL input, feedback and output divisors without arithmetic magic encodings.
fn factors(mode: Mode) Factors {
    return .{
        .d = @as(f64, @floatFromInt(mode.xi_div)) + 1,
        .m = @as(f64, @floatFromInt(mode.vco_mul)) + 1,
        .p = if (mode.vco_div == 15) 1 else 2 * (@as(f64, @floatFromInt(mode.vco_div)) + 1),
    };
}

/// Compute the requested VCO rate, distinct from an empirical nontracking output.
pub fn requestedVco(clock: *const Clock) f64 {
    const values = factors(clock.mode);
    return clock.crystal.frequency() * values.m / values.d;
}

/// Evaluate a normalized integrated-phase profile and the next physical interpolation boundary.
fn sampleResponse(response: Response, now: f64, target: f64) Sample {
    const age = @max(0, now - response.started);
    const curve = response.curve;
    if (age < curve.ready) {
        return .{ .frequency = response.initial, .boundary = response.started + curve.ready };
    }
    const index: usize = @intFromFloat(age / curve.step);
    if (index + 1 >= curve.phase.len)
        return .{ .frequency = target, .boundary = std.math.inf(f64) };
    const start = @max(@as(f64, @floatFromInt(index)) * curve.step, curve.ready);
    const end = @as(f64, @floatFromInt(index + 1)) * curve.step;
    const response_fraction = (curve.phase[index + 1] - curve.phase[index]) / (end - start);
    return .{
        .frequency = @max(0, response.initial + (target - response.initial) * response_fraction),
        .boundary = response.started + end,
    };
}

/// Evaluate the VCO independently of which oscillator currently clocks the CPU.
fn vco(clock: *const Clock, now: f64) Sample {
    const target = if (clock.nontracking_vco) |rate|
        rate * clock.crystal.frequency() / 20e6
    else
        clock.requestedVco();
    if (clock.response) |response|
        return sampleResponse(response, now, target);
    return .{ .frequency = target, .boundary = std.math.inf(f64) };
}

/// Report whether the empirical PLL response still has acquisition samples remaining.
pub fn acquiring(clock: *const Clock) bool {
    if (clock.response) |response|
        return clock.seconds - response.started < @as(f64, @floatFromInt(response.curve.phase.len - 1)) * response.curve.step;
    return false;
}

/// Apply a valid clock command, preserving PLL state across source and postdivider-only changes.
pub fn configure(clock: *Clock, raw: u32) bool {
    const next: Mode = @bitCast(raw);
    _ = next.get_frequency(@intFromFloat(clock.configuration.crystal.frequency)) catch return false;
    const old = clock.mode;
    const previous_vco = clock.vco(clock.seconds).frequency;
    const old_active = old.pll_on and old.crystal != .float;
    const active = next.pll_on and next.crystal != .float;
    const changed = old.xi_div != next.xi_div or old.vco_mul != next.vco_mul;
    if (next.crystal == .float) {
        clock.xi_started = null;
    } else if (old.crystal == .float or clock.xi_started == null) {
        clock.xi_started = clock.seconds;
    }
    clock.mode = next;
    if (!active) {
        clock.response = null;
        clock.nontracking_vco = null;
        clock.coverage = if (clock.configuration.isIdeal())
            .ideal
        else if (next.clock_src == .xi and !clock.calibratedReference(next))
            .uncharacterized
        else
            .measured;
        return true;
    }
    if (old_active and !changed and old.crystal == next.crystal) {
        if (old.clock_src == .pll and next.clock_src == .pll and old.vco_div == 0 and next.vco_div == 15 and
            next.xi_div == 0 and next.vco_mul == 15)
            clock.unsafe_postdivider_up = true;
        return true;
    }
    clock.response = null;
    clock.nontracking_vco = null;
    if (clock.configuration.isIdeal()) {
        clock.coverage = .ideal;
        return true;
    }
    clock.coverage = .uncharacterized;
    if (!clock.calibratedReference(next) or (old_active and !clock.calibratedReference(old)))
        return true;
    // These are specimen tail observations, not a universal VCO clamp or temperature model.
    if (next.xi_div == 0 and next.vco_mul == 19) {
        clock.nontracking_vco = 385713862.92561675;
    } else if (next.xi_div == 1 and next.vco_mul == 38) {
        clock.nontracking_vco = 385642449.0929911;
    } else if (next.xi_div == 7 and next.vco_mul == 159) {
        clock.nontracking_vco = 385745482.758443;
    } else if (next.xi_div == 31 and next.vco_mul == 639) {
        clock.nontracking_vco = 385674803.2839147;
    }
    if (clock.nontracking_vco != null) {
        clock.coverage = .nontracking;
        return true;
    }
    const curve: ?*const profiles.Curve = if (!old_active and next.xi_div == 0 and next.vco_mul == 7)
        if (clock.seconds - clock.xi_started.? >= 0.001) &profiles.warm else &profiles.cold
    else if (old_active and old.xi_div == 0 and old.vco_mul == 7 and next.xi_div == 0 and next.vco_mul == 9)
        &profiles.m_up
    else if (old_active and old.xi_div == 0 and old.vco_mul == 9 and next.xi_div == 0 and next.vco_mul == 7)
        &profiles.m_down
    else if (old_active and old.xi_div == 0 and old.vco_mul == 7 and next.xi_div == 1 and next.vco_mul == 7)
        &profiles.d_down
    else
        null;
    if (curve) |selected| {
        clock.response = .{ .curve = selected, .started = clock.seconds, .initial = if (old_active) previous_vco else 0 };
        clock.coverage = .measured;
    } else if (old_active and factors(old).m / factors(old).d == factors(next).m / factors(next).d) {
        clock.coverage = .measured;
    }
    return true;
}

/// The measured profiles used a nominal 20 MHz board crystal with the 15 pF loading selection.
fn calibratedReference(clock: *const Clock, mode: Mode) bool {
    return clock.configuration.crystal.frequency == 20e6 and mode.crystal == .@"15pF";
}

/// Read selected-source rate and any upcoming empirical segment boundary.
fn source(clock: *const Clock, now: f64) Sample {
    return switch (clock.mode.clock_src) {
        .rcfast => .{ .frequency = clock.rcfast.frequency(), .boundary = std.math.inf(f64) },
        .rcslow => .{ .frequency = clock.rcslow.frequency(), .boundary = std.math.inf(f64) },
        .xi => if (!clock.configuration.isIdeal() and clock.calibratedReference(clock.mode))
            sampleResponse(.{ .curve = &profiles.xi, .started = clock.xi_started orelse now, .initial = 0 }, now, clock.crystal.frequency())
        else
            .{ .frequency = clock.crystal.frequency(), .boundary = std.math.inf(f64) },
        .pll => blk: {
            var value = clock.vco(now);
            value.frequency /= factors(clock.mode).p;
            break :blk value;
        },
    };
}

/// Read the current selected clock; zero denotes an empirically unready source.
pub fn frequency(clock: *const Clock) f64 {
    return clock.source(clock.seconds).frequency;
}

/// Convert seconds to Q64 femtoseconds with enough range for slow-clock or quiet intervals.
pub fn duration(seconds: f64) u128 {
    return @intFromFloat(@max(0, seconds) * 1e15 * 18446744073709551616.0);
}

/// Convert Q64 femtoseconds back to seconds for the electrical solver.
pub fn secondsOf(q64: u128) f64 {
    return @as(f64, @floatFromInt(q64)) / (1e15 * 18446744073709551616.0);
}

/// Consume exactly one system cycle, advancing acquisition and physical time through quiet intervals.
pub fn step(clock: *Clock) Interval {
    const rng = clock.random.random();
    clock.rcfast.step(rng);
    clock.rcslow.step(rng);
    clock.crystal.step(rng);
    const first = clock.source(clock.seconds);
    if (first.frequency <= 0) {
        const pause = @min(@max(first.boundary - clock.seconds, 1e-15), 100e-9);
        clock.seconds += pause;
        clock.last_interval = .{ .quiet = duration(pause), .period = 0 };
        clock.elapsed.add(clock.last_interval.quiet);
        return clock.last_interval;
    }
    var remaining: f64 = 1;
    var period: f64 = 0;
    var quiet: f64 = 0;
    while (remaining > 1e-12) {
        const value = clock.source(clock.seconds);
        if (value.frequency <= 0) {
            const pause = @max(value.boundary - clock.seconds, 1e-15);
            quiet += pause;
            clock.seconds += pause;
            continue;
        }
        const until = @max(value.boundary - clock.seconds, 1e-15);
        const dt = @min(remaining / value.frequency, until);
        remaining = @max(0, remaining - dt * value.frequency);
        period += dt;
        clock.seconds += dt;
    }
    const rate = 1 / period;
    if (rate != clock.cached_frequency) {
        clock.cached_frequency = rate;
        // Preserve exact rational periods for integer-Hz ideal clocks and fractional carry in physical time.
        if (rate >= 1 and rate <= 1e12 and rate == @floor(rate)) {
            const hz: u64 = @intFromFloat(rate);
            clock.cached_period = (((@as(u128, 500_000_000_000_000) << 64) + hz / 2) / hz) * 2;
        } else {
            clock.cached_period = duration(period);
        }
    }
    clock.last_interval = .{ .quiet = duration(quiet), .period = clock.cached_period };
    clock.elapsed.add(clock.last_interval.quiet);
    clock.elapsed.add(clock.last_interval.period);
    return clock.last_interval;
}

test "bounded clocks are reproducible, independent of silicon RNG and use typical ideal values" {
    var a = Clock.init(.realistic);
    var b = Clock.init(.realistic);
    for (0..10000) |_| {
        try std.testing.expectEqualDeep(a.step(), b.step());
        try std.testing.expect(a.rcfast.frequency() >= 20e6 and a.rcfast.frequency() <= 30e6);
        try std.testing.expect(a.rcslow.frequency() >= 12e3 and a.rcslow.frequency() <= 30e3);
    }
    var different = Configuration.realistic;
    different.seed = 1234;
    var c = Clock.init(different);
    try std.testing.expect(a.rcfast.frequency() != c.rcfast.frequency());
    var ideal = Clock.init(.ideal);
    try std.testing.expectEqual(@as(f64, 24e6), ideal.frequency());
    for (0..10) |_| {
        _ = ideal.step();
        try std.testing.expectEqual(@as(f64, 24e6), ideal.frequency());
    }
}

test "zero variation bounds select ideal clock behavior without a profile setting" {
    var fixed = Configuration.realistic;
    fixed.rcfast = Configuration.ideal.rcfast;
    fixed.rcslow = Configuration.ideal.rcslow;
    fixed.rcfast.frequency = 25.23e6;
    fixed.seed = 42;
    try std.testing.expect(fixed.isIdeal());
    var ideal = Clock.init(fixed);
    try std.testing.expect(ideal.configure(0x010007fb));
    try std.testing.expectEqual(Coverage.ideal, ideal.coverage);
    try std.testing.expectEqual(@as(f64, 160e6), ideal.frequency());
    try std.testing.expect(!ideal.acquiring());

    inline for (.{ "rcfast", "rcslow", "crystal" }) |name| {
        inline for (.{ "below", "above" }) |bound| {
            var ranged = fixed;
            @field(@field(ranged, name), bound) = 1;
            try std.testing.expect(!ranged.isIdeal());
            var measured = Clock.init(ranged);
            try std.testing.expect(measured.configure(0x010007fb));
            try std.testing.expectEqual(Coverage.measured, measured.coverage);
            try std.testing.expect(measured.acquiring());
            try std.testing.expectEqual(@as(f64, 0), measured.frequency());
        }
    }
}

test "PLL acquisition advances on RCFAST and postdivider does not restart a measured response" {
    var config = Configuration.realistic;
    config.rcfast = Configuration.ideal.rcfast;
    var clock = Clock.init(config);
    try std.testing.expect(clock.configure(0x010007f8));
    for (0..32768) |_| _ = clock.step();
    try std.testing.expect(clock.configure(0x010007fb));
    try std.testing.expect(!clock.acquiring());
    try std.testing.expectApproxEqAbs(@as(f64, 160e6), clock.frequency(), 1);
    try std.testing.expect(clock.configure(0x010009fb));
    const started = clock.response.?.started;
    try std.testing.expect(clock.configure(0x0100090b));
    try std.testing.expectEqual(started, clock.response.?.started);
    try std.testing.expectEqual(Coverage.measured, clock.coverage);
}

test "frozen empirical profiles predict independent 100 MS/s capture phase without refitting" {
    const Vector = struct {
        case: []const u8,
        curve: []const u8,
        divider: f64,
        initial: f64,
        final: f64,
        edge_ns: []const i64,
        csv_sha256: []const u8,
    };
    const vectors = try std.json.parseFromSlice([]Vector, std.testing.allocator, @embedFile("clock-vectors"), .{});
    defer vectors.deinit();
    const Kind = enum { m_up, m_down, d_down };
    for (vectors.value) |vector| {
        const kind = std.meta.stringToEnum(Kind, vector.curve).?;
        const curve: *const profiles.Curve = switch (kind) {
            .m_up => &profiles.m_up,
            .m_down => &profiles.m_down,
            .d_down => &profiles.d_down,
        };
        const raw: u32 = switch (kind) {
            .m_up => 0x010009fb,
            .m_down => 0x010007fb,
            .d_down => 0x010407fb,
        };
        var config = Configuration.ideal;
        const decoded: Mode = @bitCast(raw);
        const ratio = factors(decoded);
        // Measured initial/tail rates remove the common instrument/crystal scale, not transient shape.
        config.crystal.frequency = vector.final * ratio.d / ratio.m;
        var clock = Clock.init(config);
        clock.mode = decoded;
        clock.response = .{ .curve = curve, .started = 0, .initial = vector.initial };
        var errors: std.ArrayList(f64) = .empty;
        defer errors.deinit(std.testing.allocator);
        var cycles: u64 = 0;
        var previous_seconds: f64 = 0;
        var sum: f64 = 0;
        for (vector.edge_ns, 0..) |ns, index| {
            const stamp = @as(f64, @floatFromInt(ns)) * 1e-9;
            var phase: f64 = vector.initial * stamp;
            if (stamp > 0) {
                while (clock.seconds < stamp) {
                    previous_seconds = clock.seconds;
                    _ = clock.step();
                    cycles += 1;
                }
                phase = @as(f64, @floatFromInt(cycles - 1)) + (stamp - previous_seconds) / (clock.seconds - previous_seconds);
            }
            const residual = (@as(f64, @floatFromInt(index)) * vector.divider - phase) / vector.final * 1e9;
            try errors.append(std.testing.allocator, residual);
            sum += residual;
        }
        const offset = sum / @as(f64, @floatFromInt(errors.items.len));
        var squares: f64 = 0;
        var peak: f64 = 0;
        for (errors.items) |value| {
            squares += (value - offset) * (value - offset);
            peak = @max(peak, @abs(value - offset));
        }
        const rms = @sqrt(squares / @as(f64, @floatFromInt(errors.items.len)));
        if (rms > 10 or peak > 30) {
            std.debug.print("clock profile {s}: RMS {d:.3} ns, peak {d:.3} ns\n", .{ vector.case, rms, peak });
        }
        try std.testing.expect(rms <= 10 and peak <= 30);
    }
}

test "all source overrides drift inside their bounds and crystal drift feeds the PLL ratio" {
    var configuration = Configuration.ideal;
    configuration.seed = 42;
    configuration.rcfast = try Configuration.Oscillator.parse("25.23+3.2ppm");
    configuration.rcslow = try Configuration.Oscillator.parse("20kHz+1%");
    configuration.crystal = try Configuration.Oscillator.parse("20+30ppm");
    var clock = Clock.init(configuration);
    const first = clock.crystal.frequency();
    try std.testing.expect(clock.configure(0x010009fb));
    for (0..10000) |_| {
        _ = clock.step();
        try std.testing.expect(clock.rcfast.frequency() >= 25_229_919.264 and clock.rcfast.frequency() <= 25_230_080.736);
        try std.testing.expect(clock.rcslow.frequency() >= 19_800 and clock.rcslow.frequency() <= 20_200);
        try std.testing.expect(clock.crystal.frequency() >= 19_999_400 and clock.crystal.frequency() <= 20_000_600);
        try std.testing.expectApproxEqAbs(clock.crystal.frequency() * 10, clock.frequency(), 1e-6);
    }
    try std.testing.expect(first != clock.crystal.frequency());
    try std.testing.expect(clock.configure(0xf1));
    try std.testing.expectEqual(clock.rcslow.frequency(), clock.frequency());
}
