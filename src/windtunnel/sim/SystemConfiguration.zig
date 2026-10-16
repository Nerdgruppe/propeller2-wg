//! Host-selected electrical parameters, fixed from Hub initialization until the next initialization.
const std = @import("std");
const SystemConfiguration = @This();

/// Select constant-resistance fast GPIO drivers or the measured piecewise I–V curve.
pub const FastDriveModel = enum { linear, piecewise };
/// One positive-current point on the fast GPIO sink/source curves.
pub const CurvePoint = struct {
    /// Current magnitude in amperes, increasing across the curve.
    current: f64,
    /// Pad voltage above GIO in volts while sinking this current.
    low_drop: f64,
    /// Pad voltage below VIO in volts while sourcing this current.
    high_drop: f64,
};

/// Number of analog integration substeps per completed system clock; fixed at initialization.
oversample: u32,

/// I/O supply voltage in volts; sets GPIO high rail and ADC/comparator references.
/// Pad.dacDriver uses VIO for the nominal 3.3 V ranges and VIO * 2.0 / 3.3 for the 2.0 V ranges.
vio_voltage: f64,
/// Ordinary logic threshold as a fraction of VIO; values at or above it read high.
threshold_ratio: f64,
/// Schmitt falling threshold as a fraction of VIO; values at or below it latch low.
schmitt_low_ratio: f64,
/// Schmitt rising threshold as a fraction of VIO; values at or above it latch high.
schmitt_high_ratio: f64,
/// Logic/Schmitt propagation delay in seconds, quantized to analog substeps.
/// Subclock delays are model assumptions bounded, not isolated, by RCFAST measurements.
receiver_delay: f64,
/// Pin-to-pin and level-comparator propagation delay in seconds, quantized to analog substeps.
comparator_delay: f64,
/// Level-comparator reference's first-order settling time constant in seconds; zero is instantaneous.
reference_settle_time: f64,
/// Constant DAC source resistances in ohms, indexed by PinConfiguration.DacRange:
/// [ohm_990_3v, ohm_600_2v, ohm_124_3v, ohm_75_2v]. Code never changes the resistance.
/// Voltage ranges are defined in Pad.dacDriver: VIO or VIO * 2.0 / 3.3, then multiplied by code/256.
dac_resistance: [4]f64,
/// ADC one-bit density for the internal GIO calibration source, in [0, 1].
adc_gio_density: f64,
/// ADC one-bit density for the internal VIO calibration source, in [0, 1].
adc_vio_density: f64,
/// ADC one-bit density for the internal floating source, independent of pad voltage.
adc_float_density: f64,
/// ADC gain pivot voltage as a fraction of VIO; density there interpolates between GIO/VIO calibration.
adc_center_ratio: f64,
/// Internal pad capacitance in farads, added to any external source capacitance.
pad_capacitance: f64,
/// Electrical model used only for the fast GPIO drive setting.
fast_model: FastDriveModel,
/// Fast GPIO low-side resistance in ohms when fast_model is linear.
fast_low_resistance: f64,
/// Fast GPIO high-side resistance in ohms when fast_model is linear.
fast_high_resistance: f64,
/// Fast GPIO I–V points for the piecewise model; interpolated from zero and extrapolated at the ends.
curve: [3]CurvePoint,
/// Resistance in ohms for the nominal 1.5 kOhm drive setting and level-comparator drives.
resistor_1k5: f64,
/// Resistance in ohms for the nominal 15 kOhm GPIO drive setting.
resistor_15k: f64,
/// Resistance in ohms for the nominal 150 kOhm GPIO drive setting.
resistor_150k: f64,
/// Series resistance in ohms for current-limited GPIO drivers near their selected rail.
current_drive_resistance: f64,
/// Source/sink current limit in amperes for the nominal 1 mA GPIO drive setting.
current_1ma: f64,
/// Source/sink current limit in amperes for the nominal 100 µA GPIO drive setting.
current_100ua: f64,
/// Source/sink current limit in amperes for the nominal 10 µA GPIO drive setting.
current_10ua: f64,
/// Enable the pad's resistive leakage path to GIO, including when its output floats.
leakage_enabled: bool,
/// Leakage resistance to GIO in ohms when leakage_enabled is true.
leakage_resistance: f64,
/// Pad voltage in volts below which the undervoltage diagnostic is asserted; does not clamp voltage.
min_pad_voltage: f64,
/// Allowed voltage above VIO in volts; exceeding VIO + margin diagnoses overvoltage without clamping.
max_pad_voltage_margin: f64,
/// Absolute chip-pin current diagnostic limit in amperes; does not limit current or model damage.
max_pin_current: f64,

pub const ideal: SystemConfiguration = .{
    .oversample = 1,
    .vio_voltage = 3.3,
    .threshold_ratio = 0.5,
    .schmitt_low_ratio = 0.35,
    .schmitt_high_ratio = 0.62,
    .receiver_delay = 0,
    .comparator_delay = 0,
    .reference_settle_time = 0,
    .dac_resistance = .{ 990, 600, 123.75, 75 },
    .adc_gio_density = 0.2,
    .adc_vio_density = 0.85,
    .adc_float_density = 0.525,
    .adc_center_ratio = 0.5,
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
    configuration.receiver_delay = 3e-9;
    configuration.comparator_delay = 10e-9;
    configuration.reference_settle_time = 50e-9;
    break :blk configuration;
};

/// Reject invalid parameters before replacing a simulation, including currently inactive settings.
pub fn validate(configuration: SystemConfiguration) error{InvalidConfiguration}!void {
    if (configuration.oversample == 0)
        return error.InvalidConfiguration;
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
    for (configuration.dac_resistance) |resistance| {
        if (!std.math.isFinite(resistance) or resistance <= 0)
            return error.InvalidConfiguration;
    }
    if (configuration.schmitt_low_ratio < 0 or
        configuration.schmitt_high_ratio > 1 or
        configuration.schmitt_low_ratio >= configuration.schmitt_high_ratio or
        configuration.receiver_delay < 0 or configuration.comparator_delay < 0 or
        configuration.reference_settle_time < 0 or
        configuration.adc_gio_density < 0 or configuration.adc_vio_density > 1 or
        configuration.adc_gio_density >= configuration.adc_vio_density or
        configuration.adc_float_density < 0 or configuration.adc_float_density > 1 or
        configuration.adc_center_ratio < 0 or configuration.adc_center_ratio > 1)
    {
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
    configuration.oversample = 0;
    try std.testing.expectError(error.InvalidConfiguration, configuration.validate());
    configuration.oversample = 8;
    try configuration.validate();
    try std.testing.expectEqualStrings("Custom", configuration.name());
    configuration = ideal;
    configuration.vio_voltage = 3.2;
    try std.testing.expectEqualStrings("Custom", configuration.name());
}

test "DAC range resistance and analog receiver bounds reject invalid configuration" {
    for (0..4) |range| {
        for ([_]f64{ 0, -1, std.math.nan(f64), std.math.inf(f64) }) |value| {
            var configuration = ideal;
            configuration.dac_resistance[range] = value;
            try std.testing.expectError(error.InvalidConfiguration, configuration.validate());
        }
    }
    var configuration = ideal;
    configuration.schmitt_low_ratio = configuration.schmitt_high_ratio;
    try std.testing.expectError(error.InvalidConfiguration, configuration.validate());
    configuration = ideal;
    configuration.adc_gio_density = configuration.adc_vio_density;
    try std.testing.expectError(error.InvalidConfiguration, configuration.validate());
    configuration = ideal;
    configuration.comparator_delay = -1;
    try std.testing.expectError(error.InvalidConfiguration, configuration.validate());
}
