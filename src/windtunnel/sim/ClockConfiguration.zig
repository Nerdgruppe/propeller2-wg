//! Oscillator ranges in Hz and CLI clock syntax; bounds describe uncertainty, not measured noise spectra.
const std = @import("std");
const Configuration = @This();

pub const Oscillator = struct {
    frequency: f64,
    below: f64,
    above: f64,

    /// Report an exact oscillator with no configured frequency variation.
    pub fn isIdeal(value: Oscillator) bool {
        return value.below == 0 and value.above == 0;
    }

    /// Reject nonfinite, nonpositive or excessive oscillator endpoints before simulation starts.
    pub fn validate(value: Oscillator) error{InvalidConfiguration}!void {
        if (!std.math.isFinite(value.frequency) or !std.math.isFinite(value.below) or
            !std.math.isFinite(value.above) or value.below < 0 or value.above < 0 or
            value.frequency - value.below < 1 or value.frequency + value.above > 1e9)
            return error.InvalidConfiguration;
    }

    /// Parse MHz by default, explicit Hz/kHz/MHz, symmetric +range, or asymmetric -low+high ranges.
    pub fn parse(text: []const u8) error{InvalidClock}!Oscillator {
        const at = delimiter(text, 0) orelse text.len;
        const base = text[0..at];
        var value: Oscillator = .{ .frequency = try amount(base, null), .below = 0, .above = 0 };
        if (at != text.len) {
            const rest = text[at + 1 ..];
            if (text[at] == '+') {
                value.above = try amount(rest, value.frequency);
                value.below = value.above;
            } else {
                const plus = delimiter(rest, 0) orelse return error.InvalidClock;
                if (rest[plus] != '+')
                    return error.InvalidClock;
                value.below = try lowerAmount(rest[0..plus], rest[plus + 1 ..], value.frequency);
                value.above = try amount(rest[plus + 1 ..], value.frequency);
            }
        }
        value.validate() catch return error.InvalidClock;
        return value;
    }
};

rcfast: Oscillator,
rcslow: Oscillator,
crystal: Oscillator,
seed: u64,

pub const ideal: Configuration = .{
    .rcfast = .{ .frequency = 24e6, .below = 0, .above = 0 },
    .rcslow = .{ .frequency = 20e3, .below = 0, .above = 0 },
    .crystal = .{ .frequency = 20e6, .below = 0, .above = 0 },
    .seed = 0,
};

pub const realistic: Configuration = .{
    // Datasheet min/typ/max: RCFAST 20/24/30 MHz; RCSLOW 12/20/30 kHz.
    .rcfast = .{ .frequency = 24e6, .below = 4e6, .above = 6e6 },
    .rcslow = .{ .frequency = 20e3, .below = 8e3, .above = 10e3 },
    // The attached crystal's tolerance is unknown: no invented default tolerance.
    .crystal = .{ .frequency = 20e6, .below = 0, .above = 0 },
    .seed = 0,
};

/// Use ideal clock behavior only when every oscillator has zero variation bounds.
pub fn isIdeal(configuration: Configuration) bool {
    return configuration.rcfast.isIdeal() and configuration.rcslow.isIdeal() and configuration.crystal.isIdeal();
}

/// Validate all three oscillators, including sources not currently selected by HUBSET.
pub fn validate(configuration: Configuration) error{InvalidConfiguration}!void {
    try configuration.rcfast.validate();
    try configuration.rcslow.validate();
    try configuration.crystal.validate();
}

/// Locate range punctuation without mistaking a scientific exponent's sign for a separator.
fn delimiter(text: []const u8, start: usize) ?usize {
    for (text[start..], start..) |character, index| {
        if ((character == '+' or character == '-') and index != 0 and
            text[index - 1] != 'e' and text[index - 1] != 'E')
            return index;
    }
    return null;
}

/// Inherit a trailing relative unit for a bare lower bound, e.g. 24-2+3ppm.
fn lowerAmount(lower: []const u8, upper: []const u8, reference: f64) error{InvalidClock}!f64 {
    const parsed = std.fmt.parseFloat(f64, lower) catch return amount(lower, reference);
    if (!std.math.isFinite(parsed) or parsed < 0)
        return error.InvalidClock;
    const Unit = struct { suffix: []const u8, factor: f64 };
    for ([_]Unit{
        .{ .suffix = "ppm", .factor = 1e-6 },
        .{ .suffix = "ppb", .factor = 1e-9 },
        .{ .suffix = "%", .factor = 1e-2 },
    }) |unit| {
        if (std.mem.endsWith(u8, upper, unit.suffix))
            return parsed * reference * unit.factor;
    }
    return amount(lower, reference);
}

/// Parse an absolute frequency or a relative range; bare numbers and 'unit' mean MHz.
fn amount(text: []const u8, reference: ?f64) error{InvalidClock}!f64 {
    var number = text;
    var scale: f64 = 1e6;
    const Suffix = struct { name: []const u8, factor: f64, relative: bool };
    for ([_]Suffix{
        .{ .name = "ppm", .factor = 1e-6, .relative = true },
        .{ .name = "ppb", .factor = 1e-9, .relative = true },
        .{ .name = "%", .factor = 1e-2, .relative = true },
        .{ .name = "MHz", .factor = 1e6, .relative = false },
        .{ .name = "kHz", .factor = 1e3, .relative = false },
        .{ .name = "Hz", .factor = 1, .relative = false },
        .{ .name = "unit", .factor = 1e6, .relative = false },
    }) |suffix| {
        if (std.mem.endsWith(u8, text, suffix.name)) {
            number = text[0 .. text.len - suffix.name.len];
            scale = suffix.factor;
            if (suffix.relative) {
                scale *= reference orelse return error.InvalidClock;
            }
            break;
        }
    }
    const parsed = std.fmt.parseFloat(f64, number) catch return error.InvalidClock;
    if (!std.math.isFinite(parsed) or parsed < 0)
        return error.InvalidClock;
    return parsed * scale;
}

test "oscillator units, asymmetric limits and invalid ranges" {
    const value = try Oscillator.parse("25.23+3.2ppm");
    try std.testing.expectApproxEqAbs(@as(f64, 25_230_000), value.frequency, 1e-6);
    try std.testing.expectApproxEqAbs(@as(f64, 80.736), value.above, 1e-9);
    try std.testing.expectApproxEqAbs(@as(f64, 48), (try Oscillator.parse("24-2+3ppm")).below, 1e-10);
    try std.testing.expectEqualDeep(realistic.rcfast, try Oscillator.parse("24-4+6"));
    try std.testing.expectEqualDeep(realistic.rcslow, try Oscillator.parse("20kHz-8kHz+10kHz"));
    try std.testing.expectApproxEqAbs(@as(f64, 0.08), (try Oscillator.parse("20+4ppb")).below, 1e-12);
    try std.testing.expectEqual(@as(f64, 200_000), (try Oscillator.parse("20+1%")).above);
    try std.testing.expectEqual(@as(f64, 3_300_000), (try Oscillator.parse("25+3.3")).above);
    try std.testing.expectEqual(@as(f64, 20_000), (try Oscillator.parse("2e-2")).frequency);
    for ([_][]const u8{ "datasheet", "datasheet+3ppm", "", "0", "nan", "inf", "24+", "24-4", "24+30", "24+-2", "24-25+6", "24+1ppm+2ppm", "-24" }) |invalid| {
        try std.testing.expectError(error.InvalidClock, Oscillator.parse(invalid));
    }
}
