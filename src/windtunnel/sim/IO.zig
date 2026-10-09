//! Shared digital pads, routing, and simultaneous cog-to-smart-pin commands.
const std = @import("std");
const Hub = @import("Hub.zig");
const smart_pin = @import("smart_pin.zig");
pub const SmartPin = smart_pin.SmartPin;
const p2 = @import("p2");
const types = p2.types;
const signal = @import("signal.zig");
pub const Signal = signal.Signal;
pub const PinIndex = types.PinIndex;
pub const PadIndex = types.PadIndex;
const PinConfiguration = types.PinConfiguration;
pub const DataSource = smart_pin.DataSource;
pub const DataSink = smart_pin.DataSink;
const IO = @This();

/// Surrounding pin logic: WRPIN routing, the smart engine and its published output snapshot.
pub const Pin = struct {
    /// Full WRPIN word, including routing, pad drive and smart-mode selection.
    configuration: PinConfiguration = .{},
    /// Shared smart registers and mode-specific logic, independent of pad configuration.
    smart: SmartPin = .{},
    /// Logic outputs published after every pin has advanced for the frame.
    outputs: smart_pin.Outputs = .{},

    const OutputSource = enum { out, other, smart };

    /// Apply WRPIN to the routing configuration and replace the smart engine's logic.
    fn configure(pin: *Pin, configuration: PinConfiguration) void {
        pin.configuration = configuration;
        pin.smart.configure(engineMode(configuration));
    }

    /// Decode the silicon's SMART override and OUT/OTHER selector from WRPIN.
    /// See docs/p2docs.github.io/page/mirror/p2silicon.md, "pin DIR/OUT control".
    fn outputSource(pin: *const Pin) OutputSource {
        return switch (pin.configuration.mode) {
            .pulse, .transition, .nco_frequency, .nco_duty, .pwm_triangle, .pwm_sawtooth, .pwm_smps, .usb_pair, .sync_tx, .uart_tx => .smart,
            else => switch (pin.configuration.output_source) {
                .out => .out,
                .other => .other,
            },
        };
    }

    /// Decode physical output enable: DIR without smart logic, otherwise WRPIN's TT low bit.
    /// This is pad control; DIR independently controls reset inside the smart engine.
    fn outputEnabled(pin: *const Pin, dir: bool) bool {
        return switch (pin.configuration.mode) {
            .gpio => dir,
            else => pin.configuration.output_enable,
        };
    }

    /// Identify modes that consume an attached data source, for idle-clock scheduling.
    fn receivesData(pin: *const Pin) bool {
        return switch (pin.configuration.mode) {
            .sync_rx, .uart_rx => true,
            else => false,
        };
    }
};

/// Hardware probes in data/timing-data/smart-pin-{cases,results}.yaml:
/// The command arrives at +2; the return bus crosses two sampling stages.
/// Z/ack are observable at +4 and repository IN at +5.
pub const command_delay = 2;
pub const ReadResult = struct { value: u32 = 0, flag: bool = false };
pub const CommandKind = enum { write_x, write_y, configure, acknowledge };
/// Hardware command selectors share an OR-combined two-bit bus.
const CommandBus = enum(u2) { write_x = 1, write_y = 2, configure = 3 };

/// Encode a semantic command for simultaneous bus-driver combination.
fn commandBus(kind: CommandKind) CommandBus {
    return switch (kind) {
        .write_x => .write_x,
        .write_y => .write_y,
        .configure, .acknowledge => .configure,
    };
}

/// Decode the combined selector and low acknowledgement bit into an operation.
fn commandKind(bus: CommandBus, value: u32) CommandKind {
    return switch (bus) {
        .write_x => .write_x,
        .write_y => .write_y,
        .configure => if (value & 1 != 0) .acknowledge else .configure,
    };
}
pub const Command = struct {
    mask: u64,
    kind: CommandKind,
    value: u32,
    cog: u3,
    pc: u20,
    instruction: u32,
};

pins: [64]Pin = @splat(.{}),
smart_a: [64]Signal = @splat(.z),
smart_b: [64]Signal = @splat(.z),
pads: [64]Signal = @splat(.z),
external: [64]Signal = @splat(.z),
pad_reads: [64]Signal = @splat(.z),
sync_out: [64]Signal = @splat(.zero),
sync_in: [64]Signal = @splat(.z),
directions: u64 = 0,
physical_directions: u64 = 0,
physical_outputs: u64 = 0,
gpio_effects: [8]?struct { directions: u64, outputs: u64 } = @splat(null),
pad_history: [4][64]Signal = @splat(@splat(.z)),
input_history: [4]u64 = @splat(0),
result_history: [4][64]ReadResult = @splat(@splat(.{})),
input_registers: u64 = 0,
routed_inputs: u64 = 0,
dynamic_pads: bool = false,
command_count: u8 = 0,
gpio_effect_count: u4 = 0,
sampling: bool = false,
retime_until: ?u64 = null,
outputs: u64 = 0,
drives: u64 = 0,
dirty: bool = true,
clock_mode: u32 = 0,
clock_frequency: u64 = 20_000_000,
crystal_frequency: u64 = 20_000_000,
random_seed: u64 = 0,
last_clock: u64 = 0,
commands: [8][8]?Command = @splat(@splat(null)),
owners: [64]struct { cog: u3 = 0, pc: u20 = 0, instruction: u32 = 0 } = @splat(.{}),
collision: ?struct { clock: u64, pin: PinIndex, cogs: u8 } = null,
failure: ?struct { pin: PinIndex, err: anyerror } = null,

/// Select the smart engine, including digital long-repository aliases.
fn engineMode(configuration: PinConfiguration) smart_pin.Mode {
    const mode = configuration.mode;
    // In the supported digital pad configurations, all three encodings are repositories.
    return switch (mode) {
        .dac_dither_rnd, .dac_dither_pwm => .repository,
        else => mode,
    };
}

/// Check digital pad routing; smart-mode support is diagnosed by its engine.
pub fn supported(value: u32) bool {
    if (value & 1 != 0) return true; // WRPIN #1 / AKPIN acknowledges only.
    const configuration: PinConfiguration = @bitCast(value);
    return @intFromEnum(configuration.pad_mode) <= @intFromEnum(PinConfiguration.PadMode.schmitt_adjacent_feedback) and @intFromEnum(configuration.input_logic) < @intFromEnum(PinConfiguration.InputLogic.filter0);
}

/// Apply a supported HUBSET clock or random-seed operation.
pub fn setClock(io: *IO, mode: u32) bool {
    if (mode & 0x80000000 != 0) {
        io.random_seed = mode;
        return true;
    }
    const config: types.ClockMode = @bitCast(mode);
    io.clock_frequency = config.get_frequency(io.crystal_frequency) catch return false;
    io.clock_mode = mode;
    return true;
}

/// ponytail: clock-indexed deterministic noise, not the silicon's undocumented
/// cog bit permutation; replace with the hardware PRNG mapping when that is characterized.
/// Indexing CT keeps idle-clock skips equivalent to individual clock steps.
pub fn randomWord(io: *const IO, cog: u3, clock: u64) u32 {
    return io.noiseWord(cog, clock);
}

/// Produce deterministic clock-indexed noise for a cog or physical pad stream.
fn noiseWord(io: *const IO, stream: u64, clock: u64) u32 {
    var rng = std.Random.SplitMix64.init(io.random_seed ^ (clock *% 0x9e3779b97f4a7c15) ^ (stream *% 0xbf58476d1ce4e5b9));
    return @truncate(rng.next());
}

/// Queue a cog command for its measured delivery edge, including calls outside Hub.step.
pub fn enqueue(io: *IO, hub: *Hub, command: Command) void {
    const slot = &io.commands[(hub.counter +% command_delay) % io.commands.len][command.cog];
    std.debug.assert(slot.* == null);
    slot.* = command;
    io.command_count += 1;
}

/// Prepare one combined command without advancing its smart logic.
fn apply(io: *IO, index: PinIndex, command: Command, inputs: *smart_pin.Inputs) !void {
    const i = @intFromEnum(index);
    io.owners[i] = .{ .cog = command.cog, .pc = command.pc, .instruction = command.instruction };
    switch (command.kind) {
        .configure => {
            if (!supported(command.value)) return error.UnsupportedPinConfiguration;
            const configuration: PinConfiguration = @bitCast(command.value);
            io.pins[i].configure(configuration);
            inputs.ack = true;
            io.dirty = true;
        },
        .write_x => inputs.new_x = command.value,
        .write_y => inputs.new_y = command.value,
        .acknowledge => inputs.ack = true,
    }
}

/// Collect one frame, advance every smart pin once, then publish pads and sampled registers.
pub fn step(io: *IO, hub: *Hub) void {
    io.last_clock = hub.counter;
    io.updateDirections(hub);
    if (!io.sampling) {
        io.pad_history = @splat(io.pads);
        io.input_history = @splat(io.routed_inputs);
        for (&io.result_history) |*edge| for (edge, &io.pins) |*result, *pin| {
            result.* = .{ .value = pin.outputs.y, .flag = pin.outputs.flag };
        };
        io.sampling = true;
    }
    if (io.gpio_effects[hub.counter % io.gpio_effects.len]) |effect| {
        io.applyState(effect.directions, effect.outputs);
        io.gpio_effects[hub.counter % io.gpio_effects.len] = null;
        io.gpio_effect_count -= 1;
    }
    var inputs: [64]smart_pin.Inputs = undefined;
    for (&inputs, 0..) |*sample, i| sample.* = io.pinInputs(@enumFromInt(i));
    const slot = &io.commands[hub.counter % io.commands.len];
    var touched: u64 = 0;
    if (io.command_count != 0) for (slot) |command| if (command) |c| {
        touched |= c.mask;
        io.command_count -= 1;
    };
    while (touched != 0) {
        const index: PinIndex = @enumFromInt(@ctz(touched));
        const mask = @as(u64, 1) << @intFromEnum(index);
        touched &= touched - 1;
        var combined: ?Command = null;
        var cogs: u8 = 0;
        for (slot) |command| if (command) |c| {
            if (c.mask & mask == 0) continue;
            cogs |= @as(u8, 1) << c.cog;
            if (combined) |*merged| {
                const bus: CommandBus = @enumFromInt(@intFromEnum(commandBus(merged.kind)) | @intFromEnum(commandBus(c.kind)));
                merged.value |= c.value | @as(u32, @intFromBool(c.kind == .acknowledge));
                merged.kind = commandKind(bus, merged.value);
            } else {
                combined = c;
                combined.?.value |= @intFromBool(c.kind == .acknowledge);
                combined.?.kind = commandKind(commandBus(c.kind), combined.?.value);
            }
        };
        if (@popCount(cogs) > 1) {
            io.collision = .{ .clock = hub.counter, .pin = index, .cogs = cogs };
            std.log.warn("smart pin {d} command collision at clock {d}, cog mask 0x{x}", .{ @intFromEnum(index), hub.counter, cogs });
            for (slot) |command| if (command) |c| {
                if (c.mask & mask != 0) std.log.warn("  cog {d}, pc 0x{x}: {t} value 0x{x:0>8}", .{ c.cog, c.pc, c.kind, c.value });
            };
        }
        io.apply(index, combined.?, &inputs[@intFromEnum(index)]) catch |err| {
            io.fail(hub, index, err);
            return;
        };
    }
    slot.* = @splat(null);
    io.routeInputs();
    for (&inputs, io.smart_a, io.smart_b) |*sample, a, b| {
        sample.a = a;
        sample.b = b;
    }
    // Keep all outputs from the previous frame visible until every pin has advanced.
    var next: [64]smart_pin.Outputs = undefined;
    for (&io.pins, inputs, &next, 0..) |*pin, sample, *state, i| {
        state.* = pin.smart.step(sample) catch |err| {
            io.fail(hub, @enumFromInt(i), err);
            return;
        };
    }
    io.routed_inputs = 0;
    io.dynamic_pads = false;
    for (&io.pins, next, 0..) |*pin, state, i| {
        const before = pin.outputs;
        const configuration = pin.configuration;
        io.routed_inputs |= @as(u64, state.in) << @intCast(i);
        if (state.in != before.in or state.y != before.y or state.flag != before.flag) io.retime_until = hub.counter +% 4;
        if (state.smart_out != before.smart_out) io.dirty = true;
        if (configuration.synchronous or configuration.output_source == .other or configuration.pad_mode != .logic) io.dynamic_pads = true;
        pin.outputs = state;
    }
    if (io.dirty or io.dynamic_pads) io.resolvePads();
    io.pad_history[hub.counter % io.pad_history.len] = io.pads;
    const input = io.routed_inputs;
    if (input != io.input_history[(hub.counter -% 1) % io.input_history.len]) io.retime_until = hub.counter +% 4;
    io.input_history[hub.counter % io.input_history.len] = input;
    io.input_registers = io.input_history[(hub.counter -% 2) % io.input_history.len];
    const results = &io.result_history[hub.counter % io.result_history.len];
    const prior = &io.result_history[(hub.counter -% 1) % io.result_history.len];
    for (results, prior, &io.pins) |*result, previous, *pin| {
        result.* = .{ .value = pin.outputs.y, .flag = pin.outputs.flag };
        if (!std.meta.eql(result.*, previous)) io.retime_until = hub.counter +% 4;
    }
    if (io.retime_until) |until| if (Hub.clock_reached(hub.counter, until)) {
        io.retime_until = null;
    };
}

/// Read the smart result through the two-stage return-bus pipeline.
pub fn readResult(io: *const IO, index: PinIndex, clock: u64) ReadResult {
    if (!io.sampling) {
        const state = io.pins[@intFromEnum(index)].outputs;
        return .{ .value = state.y, .flag = state.flag };
    }
    return io.result_history[(clock -% 2) % io.result_history.len][@intFromEnum(index)];
}

/// Record a pin I/O failure and fault the issuing cog.
fn fail(io: *IO, hub: *Hub, index: PinIndex, err: anyerror) void {
    io.failure = .{ .pin = index, .err = err };
    const owner = io.owners[@intFromEnum(index)];
    hub.fault = .{ .cog = owner.cog, .pc = owner.pc, .instruction = owner.instruction, .result = if (err == error.UnsupportedSmartPinMode) .not_implemented else .{ .illegal = "smart-pin I/O failure (see pin diagnostic)" } };
    std.log.warn("smart pin {d}: {t}", .{ @intFromEnum(index), err });
}

/// Aggregate all cog OUT/DIR registers once after the edge's writebacks.
fn updateDirections(io: *IO, hub: *Hub) void {
    var directions: u64 = 0;
    var outputs: u64 = 0;
    var drives: u64 = 0;
    for (&hub.cogs) |*cog| {
        const dir = @as(u64, cog.registers.get(.DIRA)) | (@as(u64, cog.registers.get(.DIRB)) << 32);
        const out = @as(u64, cog.registers.get(.OUTA)) | (@as(u64, cog.registers.get(.OUTB)) << 32);
        directions |= dir;
        outputs |= out;
        drives |= dir & out;
    }
    if (directions != io.directions or outputs != io.outputs or drives != io.drives) {
        io.directions = directions;
        io.outputs = outputs;
        io.drives = drives;
        // Register writeback is execution+1. The pad drive changes at +5.
        const effect = &io.gpio_effects[(hub.counter +% 4) % io.gpio_effects.len];
        if (effect.* == null) io.gpio_effect_count += 1;
        effect.* = .{ .directions = directions, .outputs = drives };
    }
}

/// Commit delayed cog OUT/DIR signals to the physical output logic.
fn applyState(io: *IO, directions: u64, outputs: u64) void {
    io.physical_directions = directions;
    io.physical_outputs = outputs;
    io.dirty = true;
}

/// Stage an external physical pad driver for resolution during the next frame.
pub fn setExternal(io: *IO, index: PadIndex, state: Signal) void {
    io.external[@intFromEnum(index)] = state;
    io.dirty = true;
}

/// Build one smart-logic input sample from the aggregate cog registers and routed pads.
fn pinInputs(io: *const IO, index: PinIndex) smart_pin.Inputs {
    const i = @intFromEnum(index);
    const mask = @as(u64, 1) << i;
    return .{
        .a = io.smart_a[i],
        .b = io.smart_b[i],
        .out = @intFromBool(io.outputs & mask != 0),
        .dir = @intFromBool(io.directions & mask != 0),
        .clock = io.last_clock,
    };
}

/// Obtain the neighboring engine's output for the physical OTHER selector.
fn smartOutput(io: *const IO, index: PinIndex) Signal {
    const i = @intFromEnum(index);
    return if (io.pins[i].outputSource() == .smart) io.pins[i].outputs.smart_out else Signal.from(io.physical_outputs & (@as(u64, 1) << i) != 0);
}

/// Resolve the output mux, feedback selection and output inversion for a pad.
fn outputState(io: *const IO, index: PadIndex) Signal {
    const i = @intFromEnum(index);
    const pin = &io.pins[i];
    const configuration = pin.configuration;
    var state: Signal = switch (pin.outputSource()) {
        .out => Signal.from(io.physical_outputs & (@as(u64, 1) << i) != 0),
        .smart => pin.outputs.smart_out,
        .other => if (i & 1 != 0) io.smartOutput(@enumFromInt(i ^ 1)).invert() else Signal.from(io.noiseWord(i, io.last_clock) & 1 != 0),
    };
    switch (configuration.pad_mode) {
        .logic_feedback, .schmitt_feedback => state = io.pads[i],
        .logic_adjacent_feedback, .schmitt_adjacent_feedback => state = io.pads[i ^ 1],
        else => {},
    }
    if (configuration.invert_output) state = state.invert();
    return state;
}

/// Apply WRPIN output enable and the physical high/low drive configuration.
fn drive(io: *const IO, index: PadIndex) Signal {
    const i = @intFromEnum(index);
    const configuration = io.pins[i].configuration;
    if (!io.pins[i].outputEnabled(io.physical_directions & (@as(u64, 1) << i) != 0)) return .z;
    const state = if (configuration.synchronous) io.sync_out[i] else io.outputState(index);
    const high_float = configuration.high_drive == .float;
    const low_float = configuration.low_drive == .float;
    return switch (state) {
        .one => if (high_float) .z else .one,
        .zero => if (low_float) .z else .zero,
        else => if (high_float and low_float) .z else .x,
    };
}

/// Select a relative physical pad or the cog OUT bit for one smart input.
fn selected(io: *const IO, index: PinIndex, selector: PinConfiguration.Selector) Signal {
    const i = @intFromEnum(index);
    const offset: u6 = switch (selector.source) {
        .local, .out => 0,
        .plus1 => 1,
        .plus2 => 2,
        .plus3 => 3,
        .minus3 => 61,
        .minus2 => 62,
        .minus1 => 63,
    };
    const pad: PadIndex = @enumFromInt(i +% offset);
    const state = if (selector.source == .out) Signal.from(io.drives & (@as(u64, 1) << i) != 0) else io.pad_reads[@intFromEnum(pad)];
    return if (selector.invert) state.invert() else state;
}

/// Settle the physical drive network from published outputs without advancing smart logic.
fn resolvePads(io: *IO) void {
    io.dirty = false;
    const previous = io.pads;
    for (&io.sync_out, 0..) |*state, i| {
        state.* = io.outputState(@enumFromInt(i));
    }
    // Settle digital feedback together. Oscillating networks have unknown pad levels.
    var settled = false;
    for (0..65) |_| {
        var next: [64]Signal = undefined;
        for (&next, 0..) |*state, i| state.* = Signal.resolve(io.drive(@enumFromInt(i)), io.external[i]);
        if (std.mem.eql(Signal, &next, &io.pads)) {
            settled = true;
            break;
        }
        io.pads = next;
    }
    if (!settled) for (&io.pads, 0..) |*state, i| {
        switch (io.pins[i].configuration.pad_mode) {
            .logic_feedback, .logic_adjacent_feedback, .schmitt_feedback, .schmitt_adjacent_feedback => state.* = .x,
            else => {},
        }
    };
    if (io.sampling and !std.mem.eql(Signal, &previous, &io.pads)) io.retime_until = io.last_clock +% 4;
    for (0..64) |i| {
        const internal = io.drive(@enumFromInt(i));
        const conflict = (internal == .one and io.external[i] == .zero) or (internal == .zero and io.external[i] == .one);
        if (conflict and previous[i] != .x) std.log.warn("digital contention on pin {d} at clock {d}", .{ i, io.last_clock });
    }
    io.sync_in = io.pads;
}

/// Compute SmartA/SmartB from the previous pad samples without advancing smart logic.
fn routeInputs(io: *IO) void {
    const pads = if (io.sampling) io.pad_history[(io.last_clock -% 2) % io.pad_history.len] else io.pads;
    for (&io.pad_reads, 0..) |*state, i| {
        const input = if (io.pins[i].configuration.synchronous) io.sync_in[i] else pads[i];
        state.* = if (io.pins[i].configuration.invert_input) input.invert() else input;
    }
    for (0..io.pins.len) |i| {
        const configuration = io.pins[i].configuration;
        const a = io.selected(@enumFromInt(i), configuration.a);
        const b = io.selected(@enumFromInt(i), configuration.b);
        const operation: signal.InputLogic = @enumFromInt(@as(u2, @intCast(@intFromEnum(configuration.input_logic))));
        io.smart_a[i] = Signal.logic(a, b, operation);
        io.smart_b[i] = b;
    }
}

/// Return the cog-visible input register sample.
pub fn get_in(io: *const IO) u64 {
    return if (io.sampling) io.input_registers else io.routed_inputs;
}

/// Report finite effects already scheduled, excluding sources waiting for new input.
pub fn scheduled(io: *const IO) bool {
    if (io.retime_until != null or io.gpio_effect_count != 0 or io.command_count != 0) return true;
    for (&io.pins) |*pin| if (pin.outputs.scheduled) return true;
    return false;
}

/// Report scheduled effects or input sources that still need clock service.
pub fn pending(io: *const IO) bool {
    if (io.scheduled() or io.dynamic_pads) return true;
    for (&io.pins) |*pin| {
        if (!pin.receivesData() or !pin.smart.registers.enabled) continue;
        if (pin.smart.registers.source) |source| {
            if (source.* != .reader or source.reader.bufferedLen() != 0) return true;
        }
    }
    return false;
}

/// Deliver one test command through the real bus delay and advance to its delivery frame.
fn testCommand(hub: *Hub, index: PinIndex, kind: CommandKind, value: u32) void {
    hub.io.enqueue(hub, .{ .mask = @as(u64, 1) << @intFromEnum(index), .kind = kind, .value = value, .cog = 0, .pc = 0, .instruction = 0 });
    for (0..command_delay + 1) |_| hub.step();
}

test "clock fields and reader-backed UART input are independent of terminal pin numbers" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);
    hub.init();
    const io = &hub.io;
    try std.testing.expect(io.setClock(0x0100_09fb));
    try std.testing.expectEqual(@as(u64, 200_000_000), io.clock_frequency);
    try std.testing.expect(!io.setClock(3));
    try std.testing.expectEqual(@as(u64, 200_000_000), io.clock_frequency);
    try std.testing.expectEqual(@as(u32, 0x0100_09fb), io.clock_mode);

    var reader = std.Io.Reader.fixed("ab");
    var source: DataSource = .{ .reader = &reader };
    const pin = &io.pins[9].smart;
    pin.registers.source = &source;
    testCommand(hub, @enumFromInt(9), .configure, 0x3e);
    testCommand(hub, @enumFromInt(9), .write_x, (2 << 16) | 7); // 20 clocks per byte.
    try std.testing.expectEqual(@as(usize, 2), reader.bufferedLen());
    hub.cogs[0].write_reg(.DIRA, 1 << 9);
    hub.step();
    try std.testing.expectEqual(@as(usize, 1), reader.bufferedLen());
    try std.testing.expect(io.pending());
    for (0..19) |_| hub.step();
    try std.testing.expect(!pin.registers.ready);
    hub.step();
    try std.testing.expectEqual(@as(u32, 'a') << 24, pin.registers.result);
    try std.testing.expectEqual(@as(usize, 0), reader.bufferedLen());
    try std.testing.expect(io.scheduled());
    // DIR reset discards the in-flight second frame before it becomes readable.
    hub.cogs[0].write_reg(.DIRA, 0);
    hub.step();
    for (0..20) |_| hub.step();
    try std.testing.expect(!pin.registers.ready);
    try std.testing.expectEqual(@as(u32, 0), pin.registers.result);
    try std.testing.expect(!io.pending());
    try std.testing.expect(io.pins[63].smart.registers.source == null);
}

test "hardware repository command timing and OR-combined mixed command buses" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);
    hub.init();
    hub.cogs[0].write_reg(.DIRB, 1 << 8);
    const pin = &hub.io.pins[40].smart;
    testCommand(hub, @enumFromInt(40), .configure, 2);
    testCommand(hub, @enumFromInt(40), .write_x, 0);
    for (0..6) |_| hub.step();
    hub.counter = 100;
    hub.io.enqueue(hub, .{ .mask = @as(u64, 1) << 40, .kind = .write_x, .value = 0x155, .cog = 0, .pc = 1, .instruction = 0 });
    hub.io.enqueue(hub, .{ .mask = @as(u64, 1) << 40, .kind = .write_x, .value = 0xaa, .cog = 7, .pc = 2, .instruction = 0 });
    try std.testing.expect(hub.next_idle_clock() == null);
    for (0..4) |_| {
        hub.step();
        try std.testing.expectEqual(@as(u32, 0), hub.io.readResult(@enumFromInt(40), hub.counter -% 1).value);
        try std.testing.expect(hub.io.get_in() & (@as(u64, 1) << 40) != 0);
    }
    hub.step();
    try std.testing.expectEqual(@as(u32, 0x1ff), pin.registers.result);
    try std.testing.expect(hub.io.get_in() & (@as(u64, 1) << 40) == 0);
    try std.testing.expectEqual(@as(u8, 0x81), hub.io.collision.?.cogs);
    hub.step();
    try std.testing.expect(hub.io.get_in() & (@as(u64, 1) << 40) != 0);
    hub.cogs[0].write_reg(.DIRB, 0);
    testCommand(hub, @enumFromInt(40), .write_x, 0x12345678);
    try std.testing.expectEqual(@as(u32, 0x1ff), pin.registers.result);
    try std.testing.expect(!pin.registers.ready);
    hub.cogs[0].write_reg(.DIRB, 1 << 8);
    // X | Y selects WRPIN; an odd combined value becomes ACK, leaving Z alone.
    hub.io.enqueue(hub, .{ .mask = @as(u64, 1) << 40, .kind = .write_x, .value = 0x100, .cog = 0, .pc = 1, .instruction = 0 });
    hub.io.enqueue(hub, .{ .mask = @as(u64, 1) << 40, .kind = .write_y, .value = 1, .cog = 1, .pc = 2, .instruction = 0 });
    for (0..5) |_| hub.step();
    try std.testing.expectEqual(@as(u32, 0x1ff), pin.registers.result);
    try std.testing.expectEqual(@as(u32, 2), @as(u32, @bitCast(hub.io.pins[40].configuration)));
}

test "GPIO cog OR, A/B routing, float, feedback and contention remain separate from IN" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);
    hub.init();
    hub.cogs[0].write_reg(.DIRA, 1);
    hub.cogs[1].write_reg(.OUTA, 1);
    for (0..9) |_| hub.step();
    try std.testing.expectEqual(Signal.zero, hub.io.pads[0]);
    hub.cogs[1].write_reg(.DIRA, 1);
    for (0..9) |_| hub.step();
    try std.testing.expectEqual(Signal.one, hub.io.pads[0]);
    try std.testing.expectEqual(@as(u64, 1), hub.io.get_in());
    hub.io.setExternal(@enumFromInt(0), .zero);
    for (0..5) |_| hub.step();
    try std.testing.expectEqual(Signal.x, hub.io.pads[0]);
    try std.testing.expectEqual(@as(u64, 0), hub.io.get_in());
    hub.io.setExternal(@enumFromInt(0), .z);
    hub.cogs[0].write_reg(.DIRA, 0);
    hub.cogs[1].write_reg(.DIRA, 0);
    for (0..9) |_| hub.step();
    try std.testing.expectEqual(Signal.z, hub.io.pads[0]);
    hub.io.setExternal(@enumFromInt(63), .one);
    testCommand(hub, @enumFromInt(0), .configure, 0x70000000); // A = relative -1, wrapping all 64 pins.
    try std.testing.expect(hub.io.smart_a[0].sample());
    testCommand(hub, @enumFromInt(0), .configure, 0xf0000000);
    try std.testing.expect(!hub.io.smart_a[0].sample());
    testCommand(hub, @enumFromInt(0), .configure, 0x3800); // High drive floats.
    hub.cogs[0].write_reg(.OUTA, 1);
    hub.cogs[0].write_reg(.DIRA, 1);
    for (0..5) |_| hub.step();
    try std.testing.expectEqual(Signal.z, hub.io.pads[0]);
    // Adjacent-pin feedback drives pin 0 from externally driven pin 1.
    hub.io.setExternal(@enumFromInt(1), .one);
    testCommand(hub, @enumFromInt(0), .configure, 0x40000);
    try std.testing.expectEqual(Signal.one, hub.io.pads[0]);
    testCommand(hub, @enumFromInt(2), .configure, 2);
    hub.cogs[0].write_reg(.DIRA, 5);
    testCommand(hub, @enumFromInt(2), .write_x, 0);
    for (0..3) |_| hub.step();
    try std.testing.expectEqual(Signal.z, hub.io.pads[2]);
    try std.testing.expect(hub.io.get_in() & 4 != 0);
}

test "independent UART pins and endpoint errors identify their pin and issuing cog" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);
    hub.init();
    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();
    var sink: DataSink = .{ .writer = &output.writer };
    for ([_]PinIndex{ @enumFromInt(4), @enumFromInt(33) }) |index| {
        hub.io.pins[@intFromEnum(index)].smart.registers.sink = &sink;
        testCommand(hub, index, .configure, 0x7c);
        testCommand(hub, index, .write_x, (1 << 16) | 7);
    }
    hub.cogs[0].write_reg(.DIRA, 1 << 4);
    hub.cogs[0].write_reg(.DIRB, 1 << 1);
    hub.io.enqueue(hub, .{ .mask = 1 << 4, .kind = .write_y, .value = 'a', .cog = 0, .pc = 0, .instruction = 0 });
    hub.io.enqueue(hub, .{ .mask = @as(u64, 1) << 33, .kind = .write_y, .value = 'b', .cog = 1, .pc = 0, .instruction = 0 });
    for (0..command_delay + 11) |_| hub.step();
    try std.testing.expectEqualStrings("ab", output.written());
    const Broken = struct {
        /// Fail a test transmission to exercise instruction provenance.
        fn write(_: *anyopaque, _: u32) anyerror!void {
            return error.TestSinkFailure;
        }
    };
    var context: u8 = 0;
    var broken: DataSink = .{ .callback = .{ .context = &context, .write = Broken.write } };
    hub.io.pins[33].smart.registers.sink = &broken;
    hub.io.enqueue(hub, .{ .mask = @as(u64, 1) << 33, .kind = .write_y, .value = 'c', .cog = 6, .pc = 123, .instruction = 456 });
    for (0..command_delay + 11) |_| hub.step();
    try std.testing.expectEqual(@as(PinIndex, @enumFromInt(33)), hub.io.failure.?.pin);
    try std.testing.expectEqual(error.TestSinkFailure, hub.io.failure.?.err);
    try std.testing.expectEqual(@as(u3, 6), hub.fault.?.cog);
    try std.testing.expectEqual(@as(u20, 123), hub.fault.?.pc);
}

test "a primed USART held in reset permits idle skipping and runner shutdown" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);
    hub.init();
    testCommand(hub, @enumFromInt(8), .configure, 56);
    testCommand(hub, @enumFromInt(8), .write_y, 123);
    try std.testing.expect(hub.io.pins[8].smart.logic.sync_tx.buffer != null);
    try std.testing.expect(!hub.io.pins[8].outputs.scheduled);
    try std.testing.expect(!hub.io.scheduled());
    try std.testing.expect(!hub.io.pending());
    hub.cogs[0].write_reg(.DIRA, 1 << 8);
    try std.testing.expect(!hub.io.pins[8].smart.registers.enabled);
    hub.step();
    try std.testing.expect(hub.io.pins[8].smart.logic.sync_tx.buffer == null);
    try std.testing.expect(hub.io.pins[8].smart.registers.ready);
}

test "digital repository aliases select the repository engine while preserving WRPIN configuration" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);
    hub.init();
    hub.cogs[0].write_reg(.DIRA, 1 << 8);
    for ([_]u32{ 2, 4, 6 }) |configuration| {
        testCommand(hub, @enumFromInt(8), .configure, configuration);
        const pin = &hub.io.pins[8].smart;
        try std.testing.expectEqual(configuration, @as(u32, @bitCast(hub.io.pins[8].configuration)));
        try std.testing.expect(pin.logic == .repository);
        testCommand(hub, @enumFromInt(8), .write_x, 0x89abcdef);
        hub.step();
        try std.testing.expect(pin.registers.ready);
        try std.testing.expectEqual(@as(u32, 0x89abcdef), pin.registers.result);
        hub.cogs[0].write_reg(.DIRA, 0);
        testCommand(hub, @enumFromInt(8), .write_x, 0);
        try std.testing.expectEqual(@as(u32, 0x89abcdef), pin.registers.result);
        hub.cogs[0].write_reg(.DIRA, 1 << 8);
        hub.step();
    }
}

test "dummy smart modes fault at command delivery with DIR low and retain issuing instruction" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);
    hub.init();
    const index: PinIndex = @enumFromInt(9);
    hub.io.enqueue(hub, .{
        .mask = @as(u64, 1) << @intFromEnum(index),
        .kind = .configure,
        .value = @as(u32, @intFromEnum(smart_pin.Mode.usb_pair)) << 1,
        .cog = 5,
        .pc = 0x1234,
        .instruction = 0xfc000009,
    });
    for (0..command_delay) |_| {
        hub.step();
        try std.testing.expect(hub.fault == null);
    }
    hub.step();
    try std.testing.expect(hub.io.pins[9].smart.logic == .usb_pair);
    try std.testing.expectEqual(@as(u64, 0), hub.io.directions);
    try std.testing.expectEqual(index, hub.io.failure.?.pin);
    try std.testing.expectEqual(error.UnsupportedSmartPinMode, hub.io.failure.?.err);
    try std.testing.expectEqual(@as(u3, 5), hub.fault.?.cog);
    try std.testing.expectEqual(@as(u20, 0x1234), hub.fault.?.pc);
    try std.testing.expectEqual(@as(u32, 0xfc000009), hub.fault.?.instruction);
    try std.testing.expect(hub.fault.?.result == .not_implemented);
}

test "each pin advances once per frame and outputs publish together after fixed input sampling" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);
    hub.init();
    const Device = struct {
        hub: *Hub,
        calls: usize = 0,
        published_in: u1 = 0,
        /// Observe the previous output snapshot while counting frame service calls.
        fn read(context: *anyopaque) anyerror!?u32 {
            const device: *@This() = @ptrCast(@alignCast(context));
            for (&device.hub.io.pins) |*pin| try std.testing.expectEqual(device.published_in, pin.outputs.in);
            try std.testing.expectEqual(std.math.maxInt(u64), device.hub.io.directions);
            device.calls += 1;
            return 1;
        }
    };
    var device: Device = .{ .hub = hub };
    var source: DataSource = .{ .callback = .{ .context = &device, .read = Device.read } };
    for (&hub.io.pins) |*pin| pin.smart.registers.source = &source;
    hub.cogs[0].write_reg(.DIRA, std.math.maxInt(u32));
    hub.cogs[0].write_reg(.DIRB, std.math.maxInt(u32));
    hub.io.enqueue(hub, .{ .mask = std.math.maxInt(u64), .kind = .configure, .value = 58, .cog = 0, .pc = 0, .instruction = 0 });
    hub.io.setExternal(@enumFromInt(0), .one);
    try std.testing.expectEqual(@as(usize, 0), device.calls);
    try std.testing.expect(hub.io.pins[0].smart.logic == .gpio);
    for (0..command_delay) |_| {
        hub.step();
        try std.testing.expect(hub.fault == null);
        try std.testing.expectEqual(@as(usize, 0), device.calls);
        // Even ordinary GPIO pins receive the aggregate DIR in their one frame update.
        for (&hub.io.pins) |*pin| try std.testing.expect(pin.smart.registers.enabled);
    }
    hub.step();
    try std.testing.expect(hub.fault == null);
    try std.testing.expectEqual(@as(usize, 64), device.calls);
    for (&hub.io.pins) |*pin| try std.testing.expectEqual(@as(u1, 1), pin.outputs.in);
    device.published_in = 1;
    // A receiver holds its word until the shared ACK arrives; that frame reads once again.
    hub.io.enqueue(hub, .{ .mask = std.math.maxInt(u64), .kind = .acknowledge, .value = 0, .cog = 0, .pc = 0, .instruction = 0 });
    for (0..command_delay) |_| {
        hub.step();
        try std.testing.expectEqual(@as(usize, 64), device.calls);
    }
    hub.step();
    try std.testing.expect(hub.fault == null);
    try std.testing.expectEqual(@as(usize, 128), device.calls);
    // External updates and command queueing cannot invoke a pin between frames.
    hub.io.setExternal(@enumFromInt(0), .zero);
    hub.io.enqueue(hub, .{ .mask = 1, .kind = .write_x, .value = 7, .cog = 0, .pc = 0, .instruction = 0 });
    try std.testing.expectEqual(@as(usize, 128), device.calls);
    try std.testing.expectEqual(@as(u32, 0), hub.io.pins[0].smart.registers.x);
}

test "WRPIN owns output routing and enable while smart outputs remain independent" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);
    hub.init();
    // Pin 0 drives high, so the odd pin's OTHER input is low.
    hub.cogs[0].write_reg(.DIRA, 1);
    hub.cogs[0].write_reg(.OUTA, 1);
    for (0..5) |_| hub.step();
    try std.testing.expectEqual(Signal.one, hub.io.pads[0]);
    const pin = &hub.io.pins[1];
    var reader = std.Io.Reader.fixed("x");
    var source: DataSource = .{ .reader = &reader };
    pin.smart.registers.source = &source;
    testCommand(hub, @enumFromInt(1), .configure, 0x7c); // UART TX, TT enables output with DIR low.
    try std.testing.expect(!pin.smart.registers.enabled);
    try std.testing.expectEqual(Signal.one, pin.outputs.smart_out);
    try std.testing.expectEqual(Signal.one, hub.io.pads[1]);
    hub.cogs[0].write_reg(.DIRA, 3);
    for (0..9) |_| hub.step();
    try std.testing.expectEqual(@as(usize, 1), reader.bufferedLen());
    try std.testing.expect(!hub.io.pending()); // A transmitter cannot consume its attached source.
    testCommand(hub, @enumFromInt(1), .configure, 0xfc); // SMART overrides OTHER in UART TX.
    try std.testing.expectEqual(Signal.one, hub.io.pads[1]);
    testCommand(hub, @enumFromInt(1), .configure, 0xbc); // TT disables output despite DIR high.
    try std.testing.expect(pin.smart.registers.enabled);
    try std.testing.expectEqual(Signal.one, pin.outputs.smart_out);
    try std.testing.expectEqual(Signal.z, hub.io.pads[1]);
    // RX leaves the output mux to OTHER, independent of the receiver's DIR reset.
    pin.smart.registers.source = null;
    hub.cogs[0].write_reg(.DIRA, 1);
    testCommand(hub, @enumFromInt(1), .configure, 0xfa);
    try std.testing.expect(!pin.smart.registers.enabled);
    try std.testing.expectEqual(Signal.zero, hub.io.pads[1]);
    testCommand(hub, @enumFromInt(1), .configure, 0xc0); // Normal mode uses DIR regardless of TT.
    for (0..2) |_| hub.step();
    try std.testing.expectEqual(Signal.z, hub.io.pads[1]);
    hub.cogs[0].write_reg(.DIRA, 3);
    for (0..5) |_| hub.step();
    try std.testing.expectEqual(Signal.zero, hub.io.pads[1]);
}
