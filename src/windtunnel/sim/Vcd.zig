//! Physical-time traces of digital registers, driver connectivity, and analog pad voltages.
const std = @import("std");
const Hub = @import("Hub.zig");
const Debugger = @import("Debugger.zig");
const IO = @import("IO.zig");
const SystemConfiguration = @import("SystemConfiguration.zig");
const Vcd = @This();

writer: *std.Io.Writer,
previous: ?Snapshot = null,
last_clock: u64 = 0,
/// Whole femtoseconds and a Q64 fractional remainder, independent of CT rollover.
elapsed: u128 = 0,
fraction: u64 = 0,
/// Streaming pad samples stay within this completed-clock interval; no per-factor buffers.
clock_open: bool = false,
clock_period: u128 = 0,
midpoint_emitted: bool = false,
last_substep: u32 = 0,
/// Mid-clock export errors are reported by capture after the hub clock completes.
pending_error: ?anyerror = null,

const names = [_][]const u8{ "OUTA", "OUTB", "DIRA", "DIRB", "INA", "INB" };
const PadSnapshot = struct {
    state: enum { low, high, floating },
    voltage: f64,
    receiver: IO.Signal,
    adc: IO.Signal,
};
const Snapshot = struct {
    cog: [8][6]u32,
    alive: [8]bool,
    starting: [8]bool,
    combined: [6]u32,
    pads: [64]PadSnapshot,
    ct: u64,
};

/// Capture cog registers, liveness, combined I/O, committed pads, and the system counter.
fn snapshot(hub: *const Hub) Snapshot {
    const input = hub.io.get_in();
    var state: Snapshot = .{
        .cog = undefined,
        .alive = undefined,
        .starting = undefined,
        .combined = .{
            @truncate(hub.io.outputs),
            @truncate(hub.io.outputs >> 32),
            @truncate(hub.io.directions),
            @truncate(hub.io.directions >> 32),
            @truncate(input),
            @truncate(input >> 32),
        },
        .pads = undefined,
        .ct = hub.counter,
    };
    for (&hub.cogs, 0..) |*cog, id| {
        state.cog[id] = .{
            cog.registers.get(.OUTA),
            cog.registers.get(.OUTB),
            cog.registers.get(.DIRA),
            cog.registers.get(.DIRB),
            @truncate(input),
            @truncate(input >> 32),
        };
        state.alive[id] = cog.exec_mode != .stopped;
        state.starting[id] = cog.startup != null;
    }
    state.pads = padSnapshot(&hub.io, hub.io.pads);
    return state;
}

/// Read only the electrical observation planes, using the appropriate completed or substep levels.
fn padSnapshot(io: *const IO, levels: [64]IO.Signal) [64]PadSnapshot {
    const pads = &io.analog_pads;
    var result: [64]PadSnapshot = undefined;
    for (levels, &result, 0..) |digital, *reading, index| {
        reading.* = .{
            .receiver = io.receiver_inputs[index],
            .adc = io.adc_bits[index],
            .state = if (pads.isDriven(index))
                switch (digital) {
                    .low => .low,
                    .high => .high,
                }
            else
                .floating,
            .voltage = if (pads.get(index)) |electrical|
                electrical.voltage
            else
                0,
        };
    }
    return result;
}

/// Emit pad-only differences independently from architectural registers and CT.
fn emitPads(vcd: *Vcd, current: *const [64]PadSnapshot, previous: ?*const [64]PadSnapshot) !void {
    for (current, 0..) |value, pin| {
        if (previous == null or value.state != previous.?[pin].state) {
            const bit: u8 = switch (value.state) {
                .low => '0',
                .high => '1',
                .floating => 'z',
            };
            try vcd.writer.print("{c}p{d}s\n", .{ bit, pin });
        }
        if (previous == null or value.receiver != previous.?[pin].receiver) {
            try vcd.writer.print("{d}p{d}r\n", .{ @intFromEnum(value.receiver), pin });
        }
        if (previous == null or value.adc != previous.?[pin].adc) {
            try vcd.writer.print("{d}p{d}a\n", .{ @intFromEnum(value.adc), pin });
        }
        if (previous == null or value.voltage != previous.?[pin].voltage) {
            try vcd.writer.print("r{e} p{d}v\n", .{ value.voltage, pin });
        }
    }
}

/// Declare a vector and its independently selectable bits in a same-name child scope.
fn declareRegister(vcd: *Vcd, id: usize, name: []const u8) !void {
    try vcd.writer.print("$var wire 32 v{d} {s} $end\n$scope module {s} $end\n", .{ id, name, name });
    for (0..32) |bit| {
        try vcd.writer.print("$var wire 1 b{d}_{d} {s} [{d}] $end\n", .{ id, bit, name, bit });
    }
    try vcd.writer.writeAll("$upscope $end\n");
}

/// Write VCD signal declarations and the initial dump header.
fn header(vcd: *Vcd) !void {
    const writer = vcd.writer;
    try writer.writeAll(
        "$version Windtunnel $end\n" ++
            "$comment Physical simulation time; pad voltage is in volts. $end\n" ++
            "$timescale 1fs $end\n" ++
            "$scope module windtunnel $end\n" ++
            "$var wire 64 ct CT $end\n" ++
            "$var wire 1 clk clk $end\n",
    );
    for (0..8) |cog| {
        try writer.print("$scope module cog{d} $end\n", .{cog});
        try writer.print("$var wire 1 a{d} alive $end\n$var wire 1 s{d} starting $end\n", .{ cog, cog });
        for (names, 0..) |name, reg| {
            try vcd.declareRegister(cog * 6 + reg, name);
        }
        try writer.writeAll("$upscope $end\n");
    }
    try writer.writeAll("$scope module io $end\n");
    for (names, 0..) |name, reg| {
        try vcd.declareRegister(48 + reg, name);
    }
    try writer.writeAll("$scope module pads $end\n");
    for (0..64) |pin| {
        try writer.print(
            "$scope module p{d} $end\n" ++
                "$var wire 1 p{d}s state $end\n" ++
                "$var real 64 p{d}v voltage $end\n" ++
                "$var wire 1 p{d}r receiver $end\n" ++
                "$var wire 1 p{d}a adc $end\n$upscope $end\n",
            .{ pin, pin, pin, pin, pin },
        );
    }
    try writer.writeAll("$upscope $end\n$upscope $end\n$upscope $end\n$enddefinitions $end\n#0\n$dumpvars\n1clk\n");
}

/// Emit a changed vector and only the individual bits that changed.
fn emitRegister(vcd: *Vcd, id: usize, value: u32, previous: ?u32) !void {
    if (previous != null and previous.? == value)
        return;
    try vcd.writer.print("b{b} v{d}\n", .{ value, id });
    var changed = if (previous) |old|
        old ^ value
    else
        std.math.maxInt(u32);
    while (changed != 0) {
        const bit: u5 = @intCast(@ctz(changed));
        try vcd.writer.print("{d}b{d}_{d}\n", .{ (value >> bit) & 1, id, bit });
        changed &= changed - 1;
    }
}

const Time = struct { elapsed: u128, fraction: u64 };

/// Compute a rounded Q64 half-period, preserving the original one-sample clock timing.
fn halfPeriod(frequency: u64) u128 {
    const numerator = @as(u128, 500_000_000_000_000) << 64;
    return (numerator + frequency / 2) / frequency;
}

/// Add an interval offset without shifting the large whole-femtosecond timestamp into Q64.
fn timeAt(vcd: *const Vcd, offset: u128) Time {
    const sum = @addWithOverflow(vcd.fraction, @as(u64, @truncate(offset)));
    return .{ .elapsed = vcd.elapsed + (offset >> 64) + sum[1], .fraction = sum[0] };
}

/// Advance one half-period, carrying fractional femtoseconds across frequency changes.
fn halfCycle(vcd: *Vcd, frequency: u64, high: bool) !void {
    const next = vcd.timeAt(halfPeriod(frequency));
    vcd.elapsed = next.elapsed;
    vcd.fraction = next.fraction;
    try vcd.writer.print("#{d}\n{d}clk\n", .{ vcd.elapsed, @intFromBool(high) });
}

/// Open an oversampled interval; failures are deferred until the completed-clock capture.
pub fn beginClock(vcd: *Vcd, counter: u64, frequency: u64) void {
    if (vcd.pending_error != null)
        return;
    if (frequency == 0) {
        vcd.pending_error = error.InvalidClockFrequency;
        return;
    }
    if (vcd.previous == null or vcd.last_clock != counter or vcd.clock_open) {
        vcd.pending_error = error.NonConsecutiveClock;
        return;
    }
    vcd.clock_open = true;
    vcd.clock_period = halfPeriod(frequency) * 2;
    vcd.midpoint_emitted = false;
    vcd.last_substep = 0;
}

/// Emit the falling edge exactly halfway through the interval, including odd sampling factors.
fn midpoint(vcd: *Vcd) !void {
    const time = vcd.timeAt(vcd.clock_period / 2);
    try vcd.writer.print("#{d}\n0clk\n", .{time.elapsed});
    vcd.midpoint_emitted = true;
}

/// Stream one intermediate pad sample, retaining electrical execution on writer failure.
pub fn capturePads(vcd: *Vcd, io: *const IO, configuration: *const SystemConfiguration, substep: u32) void {
    if (vcd.pending_error != null)
        return;
    vcd.intermediate(io, configuration, substep) catch |err| {
        vcd.pending_error = err;
    };
}

/// Merge a fractional sample with the physical clock without advancing architectural snapshots.
fn intermediate(vcd: *Vcd, io: *const IO, configuration: *const SystemConfiguration, substep: u32) !void {
    const count = configuration.oversample;
    if (!vcd.clock_open or substep == 0 or substep >= count or substep != vcd.last_substep + 1)
        return error.NonConsecutiveSample;
    const at_midpoint = @as(u64, substep) * 2 == count;
    if (!vcd.midpoint_emitted and @as(u64, substep) * 2 >= count) {
        try vcd.midpoint();
    }
    const current = padSnapshot(io, io.analog_pads.digital(configuration));
    const previous = &vcd.previous.?.pads;
    if (!std.meta.eql(previous.*, current)) {
        // Divide before multiplying to keep even the largest supported factor/frequency in u128.
        const offset = (vcd.clock_period / count) * substep +
            ((vcd.clock_period % count) * substep) / count;
        const time = vcd.timeAt(offset);
        if (!at_midpoint) {
            try vcd.writer.print("#{d}\n", .{time.elapsed});
        }
        try vcd.emitPads(&current, previous);
        vcd.previous.?.pads = current;
    }
    vcd.last_substep = substep;
}

/// Complete a streamed interval, committing exactly one period of Q64 elapsed time.
fn endClock(vcd: *Vcd) !void {
    if (!vcd.midpoint_emitted) {
        try vcd.midpoint();
    }
    const next = vcd.timeAt(vcd.clock_period);
    vcd.elapsed = next.elapsed;
    vcd.fraction = next.fraction;
    try vcd.writer.print("#{d}\n1clk\n", .{vcd.elapsed});
    vcd.clock_open = false;
}

/// Emit the initial snapshot or the next completed clock using the frequency consumed by I/O.
/// Unchanged duplicate snapshots are ignored; skipped clocks are rejected. Host-only changes
/// at the same counter are recorded at the existing timestamp without inventing clock edges.
pub fn capture(vcd: *Vcd, hub: *const Hub, clock_frequency: u64) !void {
    if (vcd.pending_error) |err|
        return err;
    if (clock_frequency == 0)
        return error.InvalidClockFrequency;
    const current = snapshot(hub);
    const previous = vcd.previous;
    if (previous == null) {
        try vcd.header();
    } else {
        const delta = hub.counter -% vcd.last_clock;
        if (delta > 1)
            return error.NonConsecutiveClock;
        if (delta == 1) {
            if (vcd.clock_open) {
                try vcd.endClock();
            } else {
                try vcd.halfCycle(clock_frequency, false);
                try vcd.halfCycle(clock_frequency, true);
            }
        } else {
            if (std.meta.eql(previous.?, current))
                return;
            try vcd.writer.print("#{d}\n", .{vcd.elapsed});
        }
    }
    if (previous == null or current.ct != previous.?.ct) {
        try vcd.writer.print("b{b} ct\n", .{current.ct});
    }
    for (current.cog, 0..) |registers, cog| {
        if (previous == null or current.alive[cog] != previous.?.alive[cog]) {
            try vcd.writer.print("{d}a{d}\n", .{ @intFromBool(current.alive[cog]), cog });
        }
        if (previous == null or current.starting[cog] != previous.?.starting[cog]) {
            try vcd.writer.print("{d}s{d}\n", .{ @intFromBool(current.starting[cog]), cog });
        }
        for (registers, 0..) |value, reg| {
            const old_value = if (previous) |old|
                old.cog[cog][reg]
            else
                null;
            try vcd.emitRegister(cog * 6 + reg, value, old_value);
        }
    }
    for (current.combined, 0..) |value, reg| {
        const old_value = if (previous) |old|
            old.combined[reg]
        else
            null;
        try vcd.emitRegister(48 + reg, value, old_value);
    }
    const old_pads = if (vcd.previous) |*old|
        &old.pads
    else
        null;
    try vcd.emitPads(&current.pads, old_pads);
    if (previous == null) {
        try vcd.writer.writeAll("$end\n");
    }
    vcd.previous = current;
    vcd.last_clock = hub.counter;
}

/// Flush the VCD output stream without advancing simulation time.
pub fn finish(vcd: *Vcd) !void {
    try vcd.writer.flush();
}

/// Read the last real assignment for an identifier, independently of the emitter's formatting.
fn lastReal(text: []const u8, identifier: []const u8) !f64 {
    var result: ?f64 = null;
    var lines = std.mem.splitScalar(u8, text, '\n');
    while (lines.next()) |line| {
        if (line.len == 0 or line[0] != 'r')
            continue;
        const space = std.mem.indexOfScalar(u8, line, ' ') orelse continue;
        if (std.mem.eql(u8, line[space + 1 ..], identifier)) {
            result = try std.fmt.parseFloat(f64, line[1..space]);
        }
    }
    return result orelse error.MissingReal;
}

test "hierarchy, physical clock phases, consecutive capture and CT rollover" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();

    var vcd: Vcd = .{ .writer = &output.writer };
    hub.counter = std.math.maxInt(u64) - 1;
    try hub.attachVcd(&vcd);
    const initial_length = output.written().len;
    try vcd.capture(hub, hub.io.clock_frequency);
    try std.testing.expectEqual(initial_length, output.written().len);
    try hub.io.setExternal(@enumFromInt(63), .high);
    try std.testing.expectEqual(Hub.StepResult.normal, hub.step());
    try std.testing.expectEqual(Hub.StepResult.normal, hub.step());
    try std.testing.expectEqual(@as(u64, 0), hub.counter);
    const text = output.written();
    try std.testing.expect(std.mem.indexOf(u8, text, "$timescale 1fs $end") != null);
    try std.testing.expect(std.mem.indexOf(u8, text, "$scope module cog7 $end") != null);
    try std.testing.expect(std.mem.indexOf(u8, text, "$var wire 1 b47_31 INB [31] $end") != null);
    try std.testing.expect(std.mem.indexOf(u8, text, "$scope module p63 $end\n$var wire 1 p63s state $end") != null);
    try std.testing.expect(std.mem.indexOf(u8, text, "$var real 64 p63v voltage $end") != null);
    try std.testing.expect(std.mem.indexOf(u8, text, "$dumpvars\n1clk\n") != null);
    try std.testing.expect(std.mem.indexOf(u8, text, "zp63s\n") != null);
    try std.testing.expect(std.mem.indexOf(u8, text, "1p63s\n") != null);
    try std.testing.expect(std.mem.indexOf(u8, text, "#25000000\n0clk\n#50000000\n1clk\n") != null);
    try std.testing.expect(std.mem.indexOf(u8, text, "#100000000\n1clk\nb0 ct\n") != null);
    try std.testing.expectEqual(@as(u128, 100_000_000), vcd.elapsed);

    const length = output.written().len;
    hub.counter = 2;
    try std.testing.expectError(error.NonConsecutiveClock, vcd.capture(hub, hub.io.clock_frequency));
    try std.testing.expectError(error.InvalidClockFrequency, vcd.capture(hub, 0));
    try std.testing.expectEqual(length, output.written().len);
    try vcd.finish();
    try std.testing.expectEqual(length, output.written().len);
    hub.configure(0x1000_0000);
    try std.testing.expectEqual(Hub.StepResult.reset, hub.step());
    try std.testing.expect(hub.vcd == null);
    try std.testing.expectEqual(length, output.written().len);
}

test "first completed clock, frequency changes and fractional time accumulation" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();

    var vcd: Vcd = .{ .writer = &output.writer };
    try hub.attachVcd(&vcd);
    _ = hub.step();
    try std.testing.expect(std.mem.endsWith(u8, output.written(), "#50000000\n1clk\nb1 ct\n"));
    hub.io.clock_frequency = 200_000_000;
    _ = hub.step();
    try std.testing.expect(std.mem.endsWith(u8, output.written(), "#52500000\n0clk\n#55000000\n1clk\nb10 ct\n"));
    hub.io.clock_frequency = 200_000_001;
    for (0..1000) |_| {
        _ = hub.step();
    }
    const expected = 55_000_000 + @as(u128, 1000) * 1_000_000_000_000_000 / 200_000_001;
    try std.testing.expectEqual(expected, vcd.elapsed);
    try std.testing.expect(vcd.fraction != 0);
    // Carry the existing remainder into a new, very different frequency.
    hub.io.clock_frequency = 3;
    for (0..3) |_| {
        _ = hub.step();
    }
    try std.testing.expectEqual(expected + 1_000_000_000_000_000, vcd.elapsed);
}

test "register bits have independent identifiers and emit only changed bits" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    hub.cogs[0].write_reg(.OUTA, 1 << 31);
    hub.cogs[0].write_reg(.DIRB, 1);
    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();

    var vcd: Vcd = .{ .writer = &output.writer };
    try hub.attachVcd(&vcd);
    try std.testing.expect(std.mem.indexOf(u8, output.written(), "$var wire 32 v2 DIRA $end\n$scope module DIRA $end") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.written(), "1b0_31\n") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.written(), "1b3_0\n") != null);
    const start = output.written().len;
    hub.cogs[0].write_reg(.DIRA, 1 << 10);
    try vcd.capture(hub, hub.io.clock_frequency);
    try std.testing.expectEqualStrings("#0\nb10000000000 v2\n1b2_10\n", output.written()[start..]);
    for (0..8) |_| {
        _ = hub.step();
    }
    try std.testing.expect(std.mem.indexOf(u8, output.written()[start..], "1b50_10\n") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.written()[start..], "1b51_0\n") != null);
}

test "pad state follows committed drive while voltage is independent and unclipped" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    for ([_]SystemConfiguration{ .ideal, .realistic }) |configuration| {
        try hub.init(configuration);
        var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
        defer output.deinit();

        var vcd: Vcd = .{ .writer = &output.writer };
        try hub.attachVcd(&vcd);
        try std.testing.expectEqual(@as(f64, 0), try lastReal(output.written(), "p0v"));
        const initial_length = output.written().len;
        var source = IO.Pad.Source.high;
        source.voltage = 5;
        try hub.io.setExternal(@enumFromInt(0), source);
        try vcd.capture(hub, hub.io.clock_frequency);
        try std.testing.expectEqual(initial_length, output.written().len);
        _ = hub.step();
        try std.testing.expect(std.mem.indexOf(u8, output.written(), "1p0s\n") != null);
        try std.testing.expect((try lastReal(output.written(), "p0v")) > 3.3);
        try std.testing.expectEqual(hub.io.analog_pads.get(0).?.voltage, try lastReal(output.written(), "p0v"));

        const driven_length = output.written().len;
        try hub.io.setExternal(@enumFromInt(0), .float);
        try vcd.capture(hub, hub.io.clock_frequency);
        try std.testing.expectEqual(driven_length, output.written().len);
        _ = hub.step();
        try std.testing.expect(std.mem.indexOf(u8, output.written()[driven_length..], "zp0s\n") != null);
        const released_voltage = hub.io.analog_pads.get(0).?.voltage;
        const release_length = output.written().len;
        _ = hub.step();
        try std.testing.expect(std.mem.indexOf(u8, output.written()[release_length..], "p0s\n") == null);
        if (configuration.leakage_enabled) {
            try std.testing.expect((try lastReal(output.written()[release_length..], "p0v")) < released_voltage);
        } else {
            try std.testing.expect(std.mem.indexOf(u8, output.written()[release_length..], " p0v\n") == null);
            try std.testing.expectEqual(released_voltage, hub.io.analog_pads.get(0).?.voltage);
        }
        source.voltage = -1;
        try hub.io.setExternal(@enumFromInt(0), source);
        _ = hub.step();
        try std.testing.expect(std.mem.indexOf(u8, output.written(), "0p0s\n") != null);
        try std.testing.expect((try lastReal(output.written(), "p0v")) < 0);
        source.max_current = 0;
        source.capacitance = 1e-9;
        try hub.io.setExternal(@enumFromInt(0), source);
        _ = hub.step();
        try std.testing.expect(!hub.io.analog_pads.isDriven(0));
        try std.testing.expectEqual(.floating, vcd.previous.?.pads[0].state);
    }
}

test "pad Z follows physical enable and independent high and low drive modes" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.realistic);
    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();

    var vcd: Vcd = .{ .writer = &output.writer };
    try hub.attachVcd(&vcd);
    hub.cogs[0].write_reg(.DIRA, 1);
    hub.cogs[0].write_reg(.OUTA, 1);
    for (0..3) |_| {
        _ = hub.step();
        try std.testing.expectEqual(.floating, vcd.previous.?.pads[0].state);
    }
    _ = hub.step();
    try std.testing.expectEqual(.high, vcd.previous.?.pads[0].state);
    var configuration = hub.io.pins[0].configuration;
    configuration.high_drive = .float;
    hub.io.editConfiguration(@enumFromInt(0), @bitCast(configuration));
    _ = hub.step();
    try std.testing.expectEqual(.floating, vcd.previous.?.pads[0].state);
    try std.testing.expect(hub.io.analog_pads.get(0).?.voltage > 1.65);
    hub.cogs[0].write_reg(.OUTA, 0);
    for (0..4) |_| {
        _ = hub.step();
    }
    try std.testing.expectEqual(.low, vcd.previous.?.pads[0].state);
    hub.cogs[0].write_reg(.DIRA, 0);
    for (0..3) |_| {
        _ = hub.step();
        try std.testing.expectEqual(.low, vcd.previous.?.pads[0].state);
    }
    _ = hub.step();
    try std.testing.expectEqual(.floating, vcd.previous.?.pads[0].state);
}

test "cog liveness covers startup and waits, with no fabricated paused clocks" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    try hub.start_cog(0, .{ .load_image = false });
    hub.pending_starts[1] = .{
        .provenance = .{ .cog = 0 },
        .at = 2,
        .address = 0,
        .ptra = 0,
        .load = false,
        .command_clock = 2,
    };
    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();

    var vcd: Vcd = .{ .writer = &output.writer };
    try hub.attachVcd(&vcd);
    try std.testing.expect(std.mem.indexOf(u8, output.written(), "1a0\n1s0\n") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.written(), "0a1\n0s1\n") != null);
    for (0..2) |_| {
        _ = hub.step();
        try std.testing.expect(!vcd.previous.?.alive[1]);
    }
    _ = hub.step();
    try std.testing.expect(vcd.previous.?.alive[1]);
    try std.testing.expect(vcd.previous.?.starting[1]);
    for (0..17) |_| {
        _ = hub.step();
    }
    try std.testing.expect(vcd.previous.?.alive[0]);
    try std.testing.expect(!vcd.previous.?.starting[0]);
    hub.cogs[0].wait_until = hub.counter + 100;
    _ = hub.step();
    try std.testing.expect(vcd.previous.?.alive[0]);
    const length = output.written().len;
    var debugger: Debugger = .{ .allocator = std.testing.allocator, .stop = .execution };
    defer debugger.deinit();

    hub.debugger = &debugger;
    const counter = hub.counter;
    try std.testing.expectEqual(Hub.StepResult.breakpoint, hub.step());
    try std.testing.expectEqual(counter, hub.counter);
    try std.testing.expect(vcd.previous.?.alive[0]);
    try vcd.capture(hub, hub.io.clock_frequency);
    try std.testing.expectEqual(length, output.written().len);
    hub.debugger = null;
    hub.cogs[0].reset();
    _ = hub.step();
    try std.testing.expect(!vcd.previous.?.alive[0]);
    try std.testing.expect(std.mem.indexOf(u8, output.written()[length..], "0a0\n") != null);
}

test "writer failures reject attachment and propagate from subsequent capture" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    var tiny_buffer: [1]u8 = undefined;
    var tiny = std.Io.Writer.fixed(&tiny_buffer);
    var rejected: Vcd = .{ .writer = &tiny };
    try std.testing.expectError(error.WriteFailed, hub.attachVcd(&rejected));
    try std.testing.expect(hub.vcd == null);

    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();

    var vcd: Vcd = .{ .writer = &output.writer };
    try hub.attachVcd(&vcd);
    hub.vcd = null;
    _ = hub.step();
    vcd.writer = &tiny;
    try std.testing.expectError(error.WriteFailed, vcd.capture(hub, hub.io.clock_frequency));
}

test "tracing does not change shared command timing or electrical evolution under either preset" {
    const plain = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(plain);

    const traced = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(traced);

    for ([_]SystemConfiguration{ .ideal, .realistic }) |preset| {
        for ([_]u32{ 1, 3, 8 }) |count| {
            var configuration = preset;
            configuration.oversample = count;
            try plain.init(configuration);
            try traced.init(configuration);
            var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
            defer output.deinit();

            var vcd: Vcd = .{ .writer = &output.writer };
            try traced.attachVcd(&vcd);
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

test "oversampling emits charging samples and merges odd and even clock midpoints" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    for ([_]u32{ 1, 3, 8 }) |count| {
        var configuration: SystemConfiguration = .ideal;
        configuration.oversample = count;
        try hub.init(configuration);
        var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
        defer output.deinit();

        var vcd: Vcd = .{ .writer = &output.writer };
        try hub.attachVcd(&vcd);
        try hub.io.setExternal(@enumFromInt(0), .{ .voltage = 3.3, .resistance = 500, .capacitance = 95e-12 });
        try std.testing.expectEqual(Hub.StepResult.normal, hub.step());
        try std.testing.expectEqual(@as(u64, 1), hub.counter);
        try std.testing.expectEqual(IO.Signal.high, hub.io.pads[0]);
        var timestamp: u128 = 0;
        var samples: u32 = 0;
        var counter_changes: usize = 0;
        var falling_edges: usize = 0;
        var rising_edges: usize = 0;
        var last_voltage: f64 = -1;
        var lines = std.mem.splitScalar(u8, output.written(), '\n');
        while (lines.next()) |line| {
            if (line.len == 0)
                continue;
            if (line[0] == '#') {
                const time = try std.fmt.parseInt(u128, line[1..], 10);
                try std.testing.expect(time >= timestamp);
                timestamp = time;
            } else if (line[0] == 'r' and std.mem.endsWith(u8, line, " p0v")) {
                const voltage = try std.fmt.parseFloat(f64, line[1 .. line.len - 4]);
                try std.testing.expect(voltage > last_voltage);
                last_voltage = voltage;
                try std.testing.expectEqual(@as(u128, 50_000_000) * samples / count, timestamp);
                const rate = 1 + 1 / @as(f64, @floatFromInt(count));
                const expected = 3.3 * (1 - std.math.pow(f64, rate, -@as(f64, @floatFromInt(samples))));
                try std.testing.expectApproxEqAbs(expected, voltage, 1e-12);
                samples += 1;
            } else if (std.mem.eql(u8, line, "0clk")) {
                falling_edges += 1;
                try std.testing.expectEqual(@as(u128, 25_000_000), timestamp);
            } else if (std.mem.eql(u8, line, "1clk")) {
                rising_edges += 1;
                try std.testing.expect(timestamp == 0 or timestamp == 50_000_000);
            } else if (line[0] == 'b' and std.mem.endsWith(u8, line, " ct")) {
                counter_changes += 1;
                try std.testing.expect(timestamp == 0 or timestamp == 50_000_000);
            }
        }
        try std.testing.expectEqual(count + 1, samples);
        try std.testing.expectEqual(@as(usize, 1), falling_edges);
        try std.testing.expectEqual(@as(usize, 2), rising_edges);
        try std.testing.expectEqual(@as(usize, 2), counter_changes);
        try std.testing.expectEqual(hub.io.getExternal(@enumFromInt(0)).?.voltage, last_voltage);
        if (count == 8) {
            try std.testing.expect(std.mem.indexOf(u8, output.written(), "#37500000\n1p0s\n") != null);
        }
        const length = output.written().len;
        try vcd.capture(hub, hub.io.clock_frequency);
        try vcd.finish();
        try std.testing.expectEqual(length, output.written().len);
    }
}

test "oversampled time carries fractional periods across frequency changes and counter rollover" {
    const normal = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(normal);
    const sampled = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(sampled);

    for ([_]u32{ 3, 8 }) |count| {
        try normal.init(.ideal);
        var configuration: SystemConfiguration = .ideal;
        configuration.oversample = count;
        try sampled.init(configuration);
        normal.counter = std.math.maxInt(u64) - 1;
        sampled.counter = normal.counter;
        var normal_output: std.Io.Writer.Allocating = .init(std.testing.allocator);
        defer normal_output.deinit();
        var sampled_output: std.Io.Writer.Allocating = .init(std.testing.allocator);
        defer sampled_output.deinit();

        var normal_vcd: Vcd = .{ .writer = &normal_output.writer };
        var sampled_vcd: Vcd = .{ .writer = &sampled_output.writer };
        try normal.attachVcd(&normal_vcd);
        try sampled.attachVcd(&sampled_vcd);
        for (0..100) |clock| {
            const frequency: u64 = switch (clock % 3) {
                0 => 20_000_000,
                1 => 200_000_001,
                else => 3,
            };
            normal.io.clock_frequency = frequency;
            sampled.io.clock_frequency = frequency;
            try std.testing.expectEqual(normal.step(), sampled.step());
            try std.testing.expectEqual(normal.counter, sampled.counter);
            try std.testing.expectEqual(normal_vcd.elapsed, sampled_vcd.elapsed);
            try std.testing.expectEqual(normal_vcd.fraction, sampled_vcd.fraction);
        }
        // Settled pads produce no redundant sample timestamps, even at odd factors.
        try std.testing.expectEqualStrings(normal_output.written(), sampled_output.written());
    }
}

test "intermediate writer errors are deferred without stopping analog substeps" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    var configuration: SystemConfiguration = .ideal;
    configuration.oversample = 8;
    try hub.init(configuration);
    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();
    var vcd: Vcd = .{ .writer = &output.writer };
    try hub.attachVcd(&vcd);
    try hub.io.setExternal(@enumFromInt(0), .high);
    vcd.beginClock(hub.counter, hub.io.clock_frequency);
    hub.io.analog_pads.step(&configuration, 50e-9 / 8.0);
    var bytes: [1]u8 = undefined;
    var broken = std.Io.Writer.fixed(&bytes);
    vcd.writer = &broken;
    vcd.capturePads(&hub.io, &configuration, 1);
    try std.testing.expectEqual(error.WriteFailed, vcd.pending_error.?);
    hub.vcd = null; // Check deferred propagation directly without an intentional error log in the test runner.
    try std.testing.expectEqual(Hub.StepResult.normal, hub.step());
    try std.testing.expectEqual(@as(u64, 1), hub.counter);
    try std.testing.expect(hub.io.getExternal(@enumFromInt(0)).?.voltage > 3.29);
    try std.testing.expectError(error.WriteFailed, vcd.capture(hub, hub.io.clock_frequency));
}

test "trace distinguishes Schmitt receiver history from voltage and raw ADC bits" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    var configuration = SystemConfiguration.ideal;
    configuration.oversample = 8;
    try hub.init(configuration);
    hub.io.editConfiguration(@enumFromInt(0), 0x60000);
    hub.io.editConfiguration(@enumFromInt(1), 0x118000);
    try hub.io.setAdcOverride(@enumFromInt(1), 0.5);
    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();

    var vcd: Vcd = .{ .writer = &output.writer };
    try hub.attachVcd(&vcd);
    try hub.io.setExternal(@enumFromInt(0), .high);
    for (0..8) |_| {
        _ = hub.step();
    }
    try hub.io.setExternal(@enumFromInt(0), .{ .voltage = 1.4, .resistance = 1 });
    for (0..8) |_| {
        _ = hub.step();
    }
    try std.testing.expectEqual(IO.Signal.low, hub.io.pads[0]);
    try std.testing.expectEqual(IO.Signal.high, hub.io.receiver_inputs[0]);
    try std.testing.expect(std.mem.indexOf(u8, output.written(), "1p0r\n") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.written(), "0p1a\n") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.written(), "1p1a\n") != null);
    try vcd.finish();
}

test "USB pair isolation and turnaround preserve physical drivers and VCD electrical snapshots" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    for ([_]SystemConfiguration{ .ideal, .realistic }) |system| {
        try hub.init(system);
        var trace: std.Io.Writer.Allocating = .init(std.testing.allocator);
        defer trace.deinit();

        var vcd: Vcd = .{ .writer = &trace.writer };
        try hub.attachVcd(&vcd);
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
        const before = snapshot(hub);
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
        const after = snapshot(hub);
        try std.testing.expectEqualDeep(before.pads[10], after.pads[10]);
        try std.testing.expectEqualDeep(before.pads[11], after.pads[11]);
        try std.testing.expectEqualDeep(before.pads[12], after.pads[12]);
        try std.testing.expectEqual(.floating, after.pads[8].state);
        try std.testing.expectEqual(.floating, after.pads[9].state);
        try vcd.finish();
        try std.testing.expect(std.mem.indexOf(u8, trace.written(), "$enddefinitions") != null);
    }
}
