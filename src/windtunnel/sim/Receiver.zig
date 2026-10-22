//! Physical input receivers and local feedback, separate from architectural sampling.
const std = @import("std");
const p2 = @import("p2");
const signal = @import("signal.zig");
const SystemConfiguration = @import("SystemConfiguration.zig");
const Configuration = p2.types.PinConfiguration;
const Receiver = @This();

output: signal.Signal = .low,
schmitt: signal.Signal = .low,
pending: signal.Signal = .low,
remaining: f64 = 0,
reference: ?f64 = null,

/// Evaluate the local receiver from a simultaneous pair of physical pad voltages.
pub fn step(
    receiver: *Receiver,
    configuration: Configuration,
    system: *const SystemConfiguration,
    voltage: f64,
    adjacent: f64,
    dt: f64,
) void {
    const mode = configuration.pad_mode;
    const input = if (mode.uses_adjacent_input())
        adjacent
    else
        voltage;
    const desired = switch (mode) {
        .level,
        .level_negative_feedback,
        .level_adjacent_positive_feedback,
        .level_adjacent_negative_feedback,
        => blk: {
            const target = system.vio_voltage * @as(f64, @floatFromInt(configuration.dacCode())) / 256;
            const previous = receiver.reference orelse target;
            receiver.reference = previous + (target - previous) * dt / (system.reference_settle_time + dt);
            break :blk signal.Signal.from(input >= receiver.reference.?);
        },
        .compare, .compare_feedback => signal.Signal.from(voltage >= adjacent),
        .schmitt, .schmitt_feedback, .schmitt_adjacent_feedback => blk: {
            if (input >= system.vio_voltage * system.schmitt_high_ratio) {
                receiver.schmitt = .high;
            } else if (input <= system.vio_voltage * system.schmitt_low_ratio) {
                receiver.schmitt = .low;
            }
            break :blk receiver.schmitt;
        },
        else => signal.Signal.from(input >= system.vio_voltage * system.threshold_ratio),
    };
    const delay = if (mode.is_level() or mode.is_compare())
        system.comparator_delay
    else
        system.receiver_delay;
    if (desired != receiver.pending) {
        receiver.pending = desired;
        receiver.remaining = delay;
    }
    receiver.remaining = @max(0, receiver.remaining - dt);
    if (receiver.remaining == 0) {
        receiver.output = receiver.pending;
    }
}

/// Convert physical voltage or the selected internal calibration source to ADC bit density.
pub fn adcDensity(configuration: Configuration, system: *const SystemConfiguration, voltage: f64) f64 {
    const source = if (configuration.isDac())
        Configuration.AdcSource.gain_1
    else
        configuration.adcSource();
    const gain: f64 = switch (source) {
        .gio => return system.adc_gio_density,
        .vio => return system.adc_vio_density,
        .floating => return system.adc_float_density,
        .gain_1 => 1,
        .gain_3 => std.math.sqrt(@as(f64, 10)),
        .gain_10 => 10,
        .gain_30 => std.math.sqrt(@as(f64, 1000)),
        .gain_100 => 100,
    };
    const span = system.adc_vio_density - system.adc_gio_density;
    const center = system.adc_gio_density + span * system.adc_center_ratio;
    return std.math.clamp(center + span * gain * (voltage / system.vio_voltage - system.adc_center_ratio), 0, 1);
}

test "Schmitt history and ADC internal sources are independent of physical voltage" {
    var receiver: Receiver = .{};
    const system = SystemConfiguration.ideal;
    const config: Configuration = .{ .pad_mode = .schmitt };
    receiver.step(config, &system, 3.3, 0, 50e-9);
    receiver.step(config, &system, 1.65, 0, 50e-9);
    try std.testing.expectEqual(signal.Signal.high, receiver.output);
    receiver.step(config, &system, 0, 0, 50e-9);
    receiver.step(config, &system, 1.65, 0, 50e-9);
    try std.testing.expectEqual(signal.Signal.low, receiver.output);
    try std.testing.expectEqual(system.adc_float_density, adcDensity(@bitCast(@as(u32, 0x110000)), &system, 3.3));
    try std.testing.expectEqual(system.adc_gio_density, adcDensity(@bitCast(@as(u32, 0x100000)), &system, 3.3));
    try std.testing.expectEqual(@as(f64, 1), adcDensity(@bitCast(@as(u32, 0x138000)), &system, 3.3));
}

test "calibrated ADC gain transfer agrees with held-out two-board measurements" {
    // P8..P11 in each board direction. Internal calibration and 1x observations determine
    // transfer and source voltage; the 96 gain observations are independent validation data.
    const Calibration = struct {
        gio: f64,
        vio: f64,
        floating: f64,
        voltages: [3]f64,
        readings: [4][3]f64,
    };
    const measured = [_]Calibration{
        .{ .gio = 0.20159312656947545, .vio = 0.85420935494559147, .floating = 0.53061267307826454, .voltages = .{ 0.8691910649689869, 1.702734151889348, 2.5366051566690828 }, .readings = .{ .{ 0.037781715393066406, 0.55474281311035156, 1 }, .{ 0, 0.6088714599609375, 1 }, .{ 0, 0.76327991485595703, 1 }, .{ 0, 1, 1 } } },
        .{ .gio = 0.17714541299002512, .vio = 0.831573486328125, .floating = 0.50576918465750553, .voltages = .{ 0.86121721494827852, 1.6987234577621466, 2.5394276659027133 }, .readings = .{ .{ 0.0075750350952148438, 0.53177356719970703, 1 }, .{ 0, 0.58723354339599609, 1 }, .{ 0, 0.75267314910888672, 1 }, .{ 0, 1, 1 } } },
        .{ .gio = 0.17348521096365793, .vio = 0.83482374463762554, .floating = 0.50476782662527897, .voltages = .{ 0.85754319002374413, 1.696329543249353, 2.5378711956501578 }, .readings = .{ .{ 0.0030202865600585938, 0.53259849548339844, 1 }, .{ 0, 0.58841037750244141, 1 }, .{ 0, 0.766845703125, 1 }, .{ 0, 1, 1 } } },
        .{ .gio = 0.17852074759347097, .vio = 0.83052103860037663, .floating = 0.50541673387799946, .voltages = .{ 0.85740023983937164, 1.6965190276120354, 2.5352757996056177 }, .readings = .{ .{ 0.010260581970214844, 0.53194999694824219, 1 }, .{ 0, 0.58989238739013672, 1 }, .{ 0, 0.75397777557373047, 1 }, .{ 0, 1, 1 } } },
        .{ .gio = 0.20161124638148717, .vio = 0.849597522190639, .floating = 0.52691350664411274, .voltages = .{ 0.80638674667384314, 1.6096346273526985, 2.418564937850066 }, .readings = .{ .{ 0, 0.49968051910400391, 0.99857902526855469 }, .{ 0, 0.44199180603027344, 1 }, .{ 0, 0.25960826873779297, 1 }, .{ 0, 0, 1 } } },
        .{ .gio = 0.17910208020891463, .vio = 0.82885510580880306, .floating = 0.50659561157226562, .voltages = .{ 0.8071805004101309, 1.6128601855905633, 2.4280187402185014 }, .readings = .{ .{ 0, 0.4779815673828125, 0.9795074462890625 }, .{ 0, 0.41490936279296875, 1 }, .{ 0, 0.22533988952636719, 1 }, .{ 0, 0, 1 } } },
        .{ .gio = 0.15910366603306361, .vio = 0.8153042112077985, .floating = 0.48552635737827848, .voltages = .{ 0.80590565869682607, 1.6153233456513223, 2.4213742540536938 }, .readings = .{ .{ 0, 0.46846485137939453, 0.974395751953125 }, .{ 0, 0.42981719970703125, 1 }, .{ 0, 0.30435562133789062, 1 }, .{ 0, 0.0024347305297851562, 1 } } },
        .{ .gio = 0.16382666996547154, .vio = 0.82284191676548546, .floating = 0.49422863551548551, .voltages = .{ 0.80646339726366101, 1.6141813857747664, 2.4245688770326095 }, .readings = .{ .{ 0, 0.46903038024902344, 0.97725677490234375 }, .{ 0, 0.41222858428955078, 1 }, .{ 0, 0.24637317657470703, 1 }, .{ 0, 0, 1 } } },
    };
    for (measured) |calibration| {
        var system = SystemConfiguration.ideal;
        system.adc_gio_density = calibration.gio;
        system.adc_vio_density = calibration.vio;
        system.adc_float_density = calibration.floating;
        system.adc_center_ratio = (calibration.floating - calibration.gio) / (calibration.vio - calibration.gio);
        try system.validate();
        for (calibration.readings, 0..) |readings, gain| {
            const raw = @as(u32, 0x100000) | (@as(u32, @intCast(gain + 4)) << 15);
            for (readings, calibration.voltages) |expected, voltage| {
                try std.testing.expectApproxEqAbs(expected, adcDensity(@bitCast(raw), &system, voltage), 0.04);
            }
        }
    }
}
