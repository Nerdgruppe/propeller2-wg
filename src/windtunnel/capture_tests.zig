//! End-to-end electrical observation invariants, independent of the recording implementation.
const std = @import("std");
const Hub = @import("sim/Hub.zig");
const IO = @import("sim/IO.zig");
const Capture = @import("sim/Capture.zig");
const Clock = @import("sim/Clock.zig");
const Pad = @import("sim/Pad.zig");

test "capture observes every RC substep without affecting electrical execution" {
    const observed = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(observed);
    const reference = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(reference);

    var config: Hub.SystemConfiguration = .ideal;
    config.clocks.rcfast.frequency = 20e6;
    config.oversample = 7;
    try observed.init(config);
    try reference.init(config);
    var capture = try Capture.init(std.testing.allocator, Capture.minimum_memory, .stop, observed.io.captureState(observed.io.pads));
    defer capture.deinit();
    observed.attachCapture(&capture);
    try std.testing.expectEqual(Capture.descriptor_blocks * Capture.block_size, capture.memoryUsed());
    const source: Pad.Source = .{ .voltage = 3.3, .resistance = 1000, .capacitance = 95e-12 };
    try observed.io.setExternal(@enumFromInt(0), source);
    try reference.io.setExternal(@enumFromInt(0), source);
    _ = observed.step();
    _ = reference.step();
    var samples = capture.iterator(Capture.AnalogSample, capture.analogChannelsConst()[0]);
    try std.testing.expectEqual(@as(u112, 0), samples.next().?.timestamp);
    for (1..8) |substep| {
        const sample = samples.next().?;
        try std.testing.expectEqual(@as(u112, @intCast(50_000_000 * substep / 7)), sample.timestamp);
    }
    try std.testing.expect(samples.next() == null);
    // Changing frequency, CT rollover and PLL quiet periods must not derive capture time from CT.
    observed.counter = std.math.maxInt(u64);
    reference.counter = std.math.maxInt(u64);
    observed.io.clock.rcfast.specification.frequency = 20_000_001;
    reference.io.clock.rcfast.specification.frequency = 20_000_001;
    for (0..10) |_| {
        _ = observed.step();
        _ = reference.step();
        try std.testing.expectEqualDeep(reference.io.getExternal(@enumFromInt(0)), observed.io.getExternal(@enumFromInt(0)));
        try std.testing.expectEqualDeep(reference.io.pads, observed.io.pads);
        try std.testing.expectEqual(reference.counter, observed.counter);
    }
    try std.testing.expectEqual(observed.io.clock.elapsed.femtoseconds, capture.metadataConst().end);
    try std.testing.expectEqual(observed.io.clock.elapsed.fraction, capture.metadataConst().time.fraction);
    const before_reset = capture.metadataConst().end;
    observed.configure(0x1000_0000);
    try std.testing.expectEqual(Hub.StepResult.reset, observed.step());
    try std.testing.expect(observed.capture == &capture);
    try std.testing.expectEqual(before_reset, capture.metadataConst().end);
    var resets = capture.iterator(Capture.ResetSample, capture.resetChannelConst().*);
    try std.testing.expectEqual(before_reset, resets.next().?.timestamp);
    _ = observed.step();
    try std.testing.expect(capture.metadataConst().end > before_reset);
    try std.testing.expectEqual(@as(u64, 1), observed.counter);
}

test "quiet integration samples the same physical substeps without clocking CT" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    var config: Hub.SystemConfiguration = .ideal;
    config.oversample = 4;
    try hub.init(config);
    var capture = try Capture.init(std.testing.allocator, Capture.minimum_memory, .stop, hub.io.captureState(hub.io.pads));
    defer capture.deinit();

    hub.attachCapture(&capture);
    try hub.io.setExternal(@enumFromInt(1), .{ .voltage = 3.3, .resistance = 1000, .capacitance = 95e-12 });
    hub.io.advanceQuiet(hub, Clock.duration(50e-9));
    try std.testing.expectEqual(@as(u64, 0), hub.counter);
    var it = capture.iterator(Capture.AnalogSample, capture.analogChannelsConst()[1]);
    _ = it.next();
    var count: usize = 0;
    while (it.next()) |sample| {
        count += 1;
        try std.testing.expect(sample.timestamp > 0);
    }
    try std.testing.expectEqual(@as(usize, 4), count);
    try std.testing.expectApproxEqAbs(@as(f64, 50_000_000), @as(f64, @floatFromInt(capture.metadataConst().end)), 1);
}

test "physical pad state follows committed drive while voltage remains independent and unclipped" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    for ([_]Hub.SystemConfiguration{ .ideal, .realistic }) |configuration| {
        try hub.init(configuration);
        const initial_voltage = if (hub.io.analog_pads.get(0)) |pad|
            pad.voltage
        else
            0;
        try std.testing.expectEqual(@as(f64, 0), initial_voltage);
        var source = IO.Pad.Source.high;
        source.voltage = 5;
        try hub.io.setExternal(@enumFromInt(0), source);
        const staged_voltage = if (hub.io.analog_pads.get(0)) |pad|
            pad.voltage
        else
            0;
        try std.testing.expectEqual(initial_voltage, staged_voltage);
        _ = hub.step();
        try std.testing.expectEqual(IO.Signal.high, hub.io.pads[0]);
        try std.testing.expect(hub.io.analog_pads.isDriven(0));
        try std.testing.expect(hub.io.analog_pads.get(0).?.voltage > 3.3);

        try hub.io.setExternal(@enumFromInt(0), .float);
        try std.testing.expect(hub.io.analog_pads.isDriven(0));
        _ = hub.step();
        try std.testing.expect(!hub.io.analog_pads.isDriven(0));
        const released_voltage = hub.io.analog_pads.get(0).?.voltage;
        _ = hub.step();
        try std.testing.expect(!hub.io.analog_pads.isDriven(0));
        if (configuration.leakage_enabled) {
            try std.testing.expect(hub.io.analog_pads.get(0).?.voltage < released_voltage);
        } else {
            try std.testing.expectEqual(released_voltage, hub.io.analog_pads.get(0).?.voltage);
        }
        source.voltage = -1;
        try hub.io.setExternal(@enumFromInt(0), source);
        _ = hub.step();
        try std.testing.expectEqual(IO.Signal.low, hub.io.pads[0]);
        try std.testing.expect(hub.io.analog_pads.get(0).?.voltage < 0);
        source.max_current = 0;
        source.capacitance = 1e-9;
        try hub.io.setExternal(@enumFromInt(0), source);
        _ = hub.step();
        try std.testing.expect(!hub.io.analog_pads.isDriven(0));
    }
}

test "physical drive follows delayed enable and independent high and low drive modes" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.realistic);
    hub.cogs[0].write_reg(.DIRA, 1);
    hub.cogs[0].write_reg(.OUTA, 1);
    for (0..3) |_| {
        _ = hub.step();
        try std.testing.expect(!hub.io.analog_pads.isDriven(0));
    }
    _ = hub.step();
    try std.testing.expect(hub.io.analog_pads.isDriven(0));
    try std.testing.expectEqual(IO.Signal.high, hub.io.pads[0]);
    var configuration = hub.io.pins[0].configuration;
    configuration.high_drive = .float;
    hub.io.editConfiguration(@enumFromInt(0), @bitCast(configuration));
    _ = hub.step();
    try std.testing.expect(!hub.io.analog_pads.isDriven(0));
    try std.testing.expect(hub.io.analog_pads.get(0).?.voltage > 1.65);
    hub.cogs[0].write_reg(.OUTA, 0);
    for (0..4) |_| {
        _ = hub.step();
    }
    try std.testing.expect(hub.io.analog_pads.isDriven(0));
    try std.testing.expectEqual(IO.Signal.low, hub.io.pads[0]);
    hub.cogs[0].write_reg(.DIRA, 0);
    for (0..3) |_| {
        _ = hub.step();
        try std.testing.expect(hub.io.analog_pads.isDriven(0));
        try std.testing.expectEqual(IO.Signal.low, hub.io.pads[0]);
    }
    _ = hub.step();
    try std.testing.expect(!hub.io.analog_pads.isDriven(0));
}

test "capture does not change shared command timing or electrical evolution under either preset" {
    const plain = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(plain);

    const traced = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(traced);

    for ([_]Hub.SystemConfiguration{ .ideal, .realistic }) |preset| {
        for ([_]u32{ 1, 3, 8 }) |count| {
            var configuration = preset;
            configuration.oversample = count;
            try plain.init(configuration);
            try traced.init(configuration);
            var capture = try Capture.init(std.testing.allocator, Capture.minimum_memory, .rolling, traced.io.captureState(traced.io.pads));
            defer capture.deinit();

            traced.attachCapture(&capture);
            for ([_]*Hub{ plain, traced }) |hub| {
                hub.cogs[0].write_reg(.DIRA, 1 << 8);
                try hub.io.setExternal(@enumFromInt(8), .high);
                hub.io.enqueue(hub, .{ .mask = 1 << 8, .kind = .configure, .value = 2, .cog = 0, .pc = 0, .instruction = 0 });
                for (0..IO.command_delay + 1) |_| {
                    _ = hub.step();
                }
                hub.io.enqueue(hub, .{ .mask = 1 << 8, .kind = .write_x, .value = 123, .cog = 0, .pc = 0, .instruction = 0 });
            }
            for (0..10) |_| {
                try std.testing.expectEqual(plain.step(), traced.step());
                try std.testing.expectEqual(plain.counter, traced.counter);
                try std.testing.expectEqualDeep(plain.random, traced.random);
                try std.testing.expectEqual(plain.io.get_in(), traced.io.get_in());
                try std.testing.expectEqualDeep(plain.io.analog_pads, traced.io.analog_pads);
                try std.testing.expectEqual(plain.io.pins[8].smart.registers.result, traced.io.pins[8].smart.registers.result);
            }
        }
    }
}

test "physical pad thresholds remain separate from Schmitt receiver history and raw ADC bits" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    var configuration = Hub.SystemConfiguration.ideal;
    configuration.oversample = 8;
    try hub.init(configuration);
    hub.io.editConfiguration(@enumFromInt(0), 0x60000);
    hub.io.editConfiguration(@enumFromInt(1), 0x118000);
    try hub.io.setAdcOverride(@enumFromInt(1), 0.5);
    var saw_low = false;
    var saw_high = false;
    try hub.io.setExternal(@enumFromInt(0), .high);
    for (0..8) |_| {
        _ = hub.step();
        saw_low = saw_low or hub.io.adc_bits[1] == .low;
        saw_high = saw_high or hub.io.adc_bits[1] == .high;
    }
    try hub.io.setExternal(@enumFromInt(0), .{ .voltage = 1.4, .resistance = 1 });
    for (0..8) |_| {
        _ = hub.step();
        saw_low = saw_low or hub.io.adc_bits[1] == .low;
        saw_high = saw_high or hub.io.adc_bits[1] == .high;
    }
    try std.testing.expectEqual(IO.Signal.low, hub.io.pads[0]);
    try std.testing.expectEqual(IO.Signal.high, hub.io.receiver_inputs[0]);
    try std.testing.expect(saw_low and saw_high);
}

test "USB pair isolation and turnaround preserve neighboring physical drivers" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    for ([_]Hub.SystemConfiguration{ .ideal, .realistic }) |system| {
        try hub.init(system);
        // Two adjacent pairs run independently; an ordinary neighboring GPIO keeps its drive.
        for (8..12) |pin| {
            hub.io.editConfiguration(@enumFromInt(pin), 0x76);
        }
        hub.io.editSmartRegister(@enumFromInt(8), .x, 4096);
        hub.io.editSmartRegister(@enumFromInt(10), .x, 0xc000 | 4096);
        hub.cogs[0].write_reg(.DIRA, 0x1f00);
        hub.cogs[0].write_reg(.OUTA, 1 << 12);
        // Queue state changes after both controllers have left reset.
        for (0..16) |_| {
            _ = hub.step();
        }
        hub.io.editSmartRegister(@enumFromInt(8), .y, 2);
        hub.io.editSmartRegister(@enumFromInt(10), .y, 1);
        for (0..256) |_| {
            _ = hub.step();
        }
        const levels = hub.io.pads;
        const receivers = hub.io.receiver_inputs;
        const adc = hub.io.adc_bits;
        var voltages: [3]f64 = undefined;
        var driven: [3]bool = undefined;
        for (10..13, &voltages, &driven) |pin, *voltage, *enabled| {
            voltage.* = hub.io.analog_pads.get(pin).?.voltage;
            enabled.* = hub.io.analog_pads.isDriven(pin);
        }
        try std.testing.expectEqual(IO.Signal.low, hub.io.pads[8]);
        try std.testing.expectEqual(IO.Signal.high, hub.io.pads[9]);
        try std.testing.expectEqual(IO.Signal.low, hub.io.pads[10]);
        try std.testing.expectEqual(IO.Signal.low, hub.io.pads[11]);
        try std.testing.expectEqual(IO.Signal.high, hub.io.pads[12]);
        // A live sniffer transition releases only the selected physical pair.
        hub.io.editConfiguration(@enumFromInt(8), 0x36);
        hub.io.editConfiguration(@enumFromInt(9), 0x36);
        for (0..32) |_| {
            _ = hub.step();
        }
        for (10..13, voltages, driven) |pin, voltage, enabled| {
            try std.testing.expectEqual(enabled, hub.io.analog_pads.isDriven(pin));
            try std.testing.expectEqual(levels[pin], hub.io.pads[pin]);
            try std.testing.expectEqual(receivers[pin], hub.io.receiver_inputs[pin]);
            try std.testing.expectEqual(adc[pin], hub.io.adc_bits[pin]);
            try std.testing.expectApproxEqAbs(voltage, hub.io.analog_pads.get(pin).?.voltage, 1e-12);
        }
        try std.testing.expect(!hub.io.analog_pads.isDriven(8));
        try std.testing.expect(!hub.io.analog_pads.isDriven(9));
    }
}

test "capture records odd and even RC substeps at their shared physical times" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    for ([_]u32{ 1, 3, 8 }) |count| {
        var configuration: Hub.SystemConfiguration = .ideal;
        configuration.oversample = count;
        configuration.clocks.rcfast.frequency = 20e6;
        try hub.init(configuration);
        var capture = try Capture.init(std.testing.allocator, Capture.minimum_memory, .stop, hub.io.captureState(hub.io.pads));
        defer capture.deinit();

        hub.attachCapture(&capture);
        try hub.io.setExternal(@enumFromInt(0), .{ .voltage = 3.3, .resistance = 500, .capacitance = 95e-12 });
        try std.testing.expectEqual(Hub.StepResult.normal, hub.step());
        try std.testing.expectEqual(@as(u64, 1), hub.counter);
        try std.testing.expectEqual(IO.Signal.high, hub.io.pads[0]);
        var samples = capture.iterator(Capture.AnalogSample, capture.analogChannelsConst()[0]);
        for (0..count + 1) |substep| {
            const sample = samples.next().?;
            try std.testing.expectEqual(@as(u112, @intCast(50_000_000 * substep / count)), sample.timestamp);
            const rate = 1 + 1 / @as(f64, @floatFromInt(count));
            const expected = 3.3 * (1 - std.math.pow(f64, rate, -@as(f64, @floatFromInt(substep))));
            try std.testing.expectEqual(Capture.voltage(expected), sample.value);
        }
        try std.testing.expect(samples.next() == null);
        try std.testing.expectEqual(@as(u128, 50_000_000), capture.metadataConst().end);
    }
}

test "oversampled captures retain fractional time across frequency changes and counter rollover" {
    const normal = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(normal);
    const sampled = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(sampled);

    for ([_]u32{ 3, 8 }) |count| {
        try normal.init(.ideal);
        var configuration: Hub.SystemConfiguration = .ideal;
        configuration.oversample = count;
        try sampled.init(configuration);
        normal.counter = std.math.maxInt(u64) - 1;
        sampled.counter = normal.counter;
        var capture = try Capture.init(std.testing.allocator, Capture.minimum_memory, .stop, sampled.io.captureState(sampled.io.pads));
        defer capture.deinit();

        sampled.attachCapture(&capture);
        var minimum: u128 = 0;
        for (0..100) |clock| {
            const frequency: u64 = switch (clock % 3) {
                0 => 20_000_000,
                1 => 200_000_001,
                else => 3,
            };
            normal.io.clock.rcfast.specification.frequency = @floatFromInt(frequency);
            sampled.io.clock.rcfast.specification.frequency = @floatFromInt(frequency);
            try std.testing.expectEqual(normal.step(), sampled.step());
            const substep = sampled.io.clock.last_interval.period / count;
            minimum = if (minimum == 0)
                substep
            else
                @min(minimum, substep);
            try std.testing.expectEqual(minimum, capture.metadataConst().minimum_substep);
            try std.testing.expectEqual(normal.counter, sampled.counter);
            try std.testing.expectEqualDeep(normal.io.clock.elapsed, sampled.io.clock.elapsed);
            try std.testing.expectEqual(sampled.io.clock.elapsed.femtoseconds, capture.metadataConst().end);
            try std.testing.expectEqual(sampled.io.clock.elapsed.fraction, capture.metadataConst().time.fraction);
        }
        try std.testing.expectEqual(Capture.descriptor_blocks * Capture.block_size, capture.memoryUsed());
    }
}
