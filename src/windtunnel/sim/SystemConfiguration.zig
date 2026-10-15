//! Host-selected electrical parameters, fixed from Hub initialization until the next initialization.
const std = @import("std");
const SystemConfiguration = @This();

pub const FastDriveModel = enum { linear, piecewise };
pub const CurvePoint = struct {
    current: f64,
    low_drop: f64,
    high_drop: f64,
};

// All electrical quantities use SI units. Curve drops are measured from the selected rail.
vio_voltage: f64,
threshold_ratio: f64,
pad_capacitance: f64,
fast_model: FastDriveModel,
fast_low_resistance: f64,
fast_high_resistance: f64,
curve: [3]CurvePoint,
resistor_1k5: f64,
resistor_15k: f64,
resistor_150k: f64,
current_drive_resistance: f64,
current_1ma: f64,
current_100ua: f64,
current_10ua: f64,
leakage_enabled: bool,
leakage_resistance: f64,
min_pad_voltage: f64,
max_pad_voltage_margin: f64,
max_pin_current: f64,

pub const ideal: SystemConfiguration = .{
    .vio_voltage = 3.3,
    .threshold_ratio = 0.5,
    .pad_capacitance = 5e-12,
    .fast_model = .linear,
    .fast_low_resistance = 30,
    .fast_high_resistance = 30,
    // docs/p2/datasheet.md: DC output voltage drops at 1, 10 and 30 mA.
    .curve = .{
        .{ .current = 1e-3, .low_drop = 15e-3, .high_drop = 6e-3 },
        .{ .current = 10e-3, .low_drop = 160e-3, .high_drop = 170e-3 },
        .{ .current = 30e-3, .low_drop = 510e-3, .high_drop = 580e-3 },
    },
    .resistor_1k5 = 1_500,
    .resistor_15k = 15_000,
    .resistor_150k = 150_000,
    .current_drive_resistance = 30,
    .current_1ma = 1e-3,
    .current_100ua = 100e-6,
    .current_10ua = 10e-6,
    .leakage_enabled = false,
    .leakage_resistance = 33e6,
    .min_pad_voltage = -0.3,
    .max_pad_voltage_margin = 0.3,
    .max_pin_current = 30e-3,
};

pub const realistic: SystemConfiguration = blk: {
    var configuration = ideal;
    configuration.fast_model = .piecewise;
    configuration.leakage_enabled = true;
    break :blk configuration;
};

/// Reject invalid parameters before replacing a simulation, including currently inactive settings.
pub fn validate(configuration: SystemConfiguration) error{InvalidConfiguration}!void {
    inline for (std.meta.fields(SystemConfiguration)) |field| {
        if (field.type == f64) {
            if (!std.math.isFinite(@field(configuration, field.name)))
                return error.InvalidConfiguration;
        }
    }
    for ([_]f64{
        configuration.vio_voltage,
        configuration.pad_capacitance,
        configuration.fast_low_resistance,
        configuration.fast_high_resistance,
        configuration.resistor_1k5,
        configuration.resistor_15k,
        configuration.resistor_150k,
        configuration.current_drive_resistance,
        configuration.current_1ma,
        configuration.current_100ua,
        configuration.current_10ua,
        configuration.leakage_resistance,
        configuration.max_pin_current,
    }) |value| {
        if (value <= 0)
            return error.InvalidConfiguration;
    }
    const maximum = configuration.vio_voltage + configuration.max_pad_voltage_margin;
    if (configuration.threshold_ratio < 0 or configuration.threshold_ratio > 1 or
        configuration.max_pad_voltage_margin < 0 or !std.math.isFinite(maximum) or
        configuration.min_pad_voltage >= maximum)
    {
        return error.InvalidConfiguration;
    }
    var previous: CurvePoint = .{ .current = 0, .low_drop = 0, .high_drop = 0 };
    for (configuration.curve) |point| {
        if (!std.math.isFinite(point.current) or !std.math.isFinite(point.low_drop) or
            !std.math.isFinite(point.high_drop) or point.current <= previous.current or
            point.low_drop <= previous.low_drop or point.high_drop <= previous.high_drop)
        {
            return error.InvalidConfiguration;
        }
        previous = point;
    }
}

/// Identify unchanged presets for display; customized configurations have no hidden preset behavior.
pub fn name(configuration: SystemConfiguration) [:0]const u8 {
    if (std.meta.eql(configuration, ideal))
        return "Ideal";
    if (std.meta.eql(configuration, realistic))
        return "Realistic";
    return "Custom";
}

test "presets and every configurable quantity are validated" {
    try ideal.validate();
    try realistic.validate();
    inline for (std.meta.fields(SystemConfiguration)) |field| {
        if (field.type == f64) {
            var configuration = ideal;
            @field(configuration, field.name) = std.math.nan(f64);
            try std.testing.expectError(error.InvalidConfiguration, configuration.validate());
            @field(configuration, field.name) = std.math.inf(f64);
            try std.testing.expectError(error.InvalidConfiguration, configuration.validate());
        }
    }
    var configuration = realistic;
    configuration.curve[1].current = configuration.curve[0].current;
    try std.testing.expectError(error.InvalidConfiguration, configuration.validate());
    configuration = ideal;
    configuration.pad_capacitance = 0;
    try std.testing.expectError(error.InvalidConfiguration, configuration.validate());
    configuration = ideal;
    configuration.threshold_ratio = 1.1;
    try std.testing.expectError(error.InvalidConfiguration, configuration.validate());
    configuration = ideal;
    configuration.min_pad_voltage = 4;
    try std.testing.expectError(error.InvalidConfiguration, configuration.validate());
    try std.testing.expectEqualStrings("Ideal", ideal.name());
    try std.testing.expectEqualStrings("Realistic", realistic.name());
    configuration = ideal;
    configuration.vio_voltage = 3.2;
    try std.testing.expectEqualStrings("Custom", configuration.name());
}
