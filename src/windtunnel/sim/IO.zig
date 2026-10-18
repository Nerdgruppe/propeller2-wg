//! Electrical pads, physical receivers, routing, and simultaneous cog-to-smart-pin commands.
const std = @import("std");
const Hub = @import("Hub.zig");
const smart_pin = @import("smart_pin.zig");
pub const Pad = @import("Pad.zig");
const Pads = @import("Pads.zig");
const Receiver = @import("Receiver.zig");
const pipeline = @import("pipeline.zig");
const Pipeline = pipeline.Pipeline;
pub const SmartPin = smart_pin.SmartPin;
const p2 = @import("p2");
const types = p2.types;
const signal = @import("signal.zig");
pub const Signal = signal.Signal;
pub const PinIndex = types.PinIndex;
pub const PadIndex = types.PadIndex;
const PinConfiguration = types.PinConfiguration;
const DriveSignal = smart_pin.DriveSignal;
pub const DataSource = smart_pin.DataSource;
pub const DataSink = smart_pin.DataSink;
const IO = @This();
pub const Clock = @import("Clock.zig");

/// Deterministic first-order delta/sigma source; density is a fraction of one bits, not a voltage.
pub const DeltaSigma = struct {
    density: f64 = 0.5,
    phase: f64 = 0,

    /// Emit one raw ADC bit while retaining fractional error for the next system clock.
    pub fn step(source: *DeltaSigma) bool {
        source.phase += source.density;
        const bit = source.phase >= 1;
        if (bit) {
            source.phase -= 1;
        }
        return bit;
    }
};

/// One global unanimous-filter setting, selected independently by each pin.
pub const FilterConfiguration = struct {
    length: enum(u2) {
        two,
        three,
        five,
        eight,

        /// Return the history bits participating in the unanimity comparison.
        fn mask(length: @This()) u8 {
            return switch (length) {
                .two => 0x03,
                .three => 0x07,
                .five => 0x1f,
                .eight => 0xff,
            };
        }
    },
    tap: u5,

    /// Detect either transition of the sampled CT tap, half a period ahead of GETCT's phase.
    /// CT-tap phase follows the silicon measurements summarized in docs/accumulated.md.
    fn samples(config: FilterConfiguration, clock: u64, previous: u64) bool {
        const phase = if (config.tap == 0) @as(u64, 0) else @as(u64, 1) << (config.tap - 1);
        const changed = (clock +% phase) ^ (previous +% phase);
        return changed & (@as(u64, 1) << config.tap) != 0;
    }
};

/// Each pin retains separate A/B histories and outputs across configuration and DIR changes.
const InputFilter = struct {
    history: u8 = 0,
    output_pipeline: Pipeline(bool, 2) = .init(false),

    /// Clock unanimity and its two output stages from the old history, then shift a CT-tap sample.
    fn step(filter: *InputFilter, input: Signal, mask: u8, sample: bool) Signal {
        const history = filter.history & mask;
        const agreed = if (history == 0 or history == mask) history != 0 else filter.output_pipeline.tap(0);
        const output = filter.output_pipeline.step(agreed);
        if (sample) {
            filter.history = (filter.history << 1) | @intFromBool(input.sample());
        }
        return Signal.from(output);
    }
};

/// Surrounding pin logic: WRPIN routing, the smart engine and its published output snapshot.
pub const Pin = struct {
    /// Full WRPIN word, including routing, pad drive and smart-mode selection.
    configuration: PinConfiguration = .{},
    /// Shared smart registers and mode-specific logic, independent of pad configuration.
    smart: SmartPin = .{},
    /// Logic outputs published after every pin has advanced for the frame.
    outputs: smart_pin.Outputs = .{},
    /// Registered output-mux path, clocked in every mode without configuration-time resets.
    output_pipeline: Pipeline(DriveSignal, 1) = .init(.{}),
    /// Optional output synchronization retains its history while deselected.
    sync_output_pipeline: Pipeline(DriveSignal, 3) = .init(.{}),
    /// Selected OUT/OTHER/SmartOut after the common propagation delay.
    output: Signal = .low,
    /// Engine drive override, transported alongside the selected output level.
    strength: ?PinConfiguration.Drive = null,
    /// Host architectural edits deliver their engine strobes on the next complete hub edge.
    edits: struct { x: ?u32 = null, y: ?u32 = null } = .{},
    filter_a: InputFilter = .{},
    filter_b: InputFilter = .{},

    const OutputSource = enum { other, smart };

    /// Apply WRPIN routing and delegate engine configuration and state retention to the smart pin.
    pub fn configure(pin: *Pin, configuration: PinConfiguration) void {
        pin.configuration = configuration;
        pin.smart.configure(configuration.mode);
    }

    /// Decode the silicon's SMART override and OUT/OTHER selector from WRPIN.
    /// See docs/p2docs.github.io/page/mirror/p2silicon.md, "pin DIR/OUT control".
    fn outputSource(pin: *const Pin) OutputSource {
        const configuration = pin.configuration;
        if (configuration.isDac() and configuration.mode == .gpio and
            configuration.output_source == .other and !configuration.output_enable)
        {
            return .smart;
        }
        return if (pin.smart.overridesOutput() or configuration.output_source == .out)
            .smart
        else
            .other;
    }

    /// Decode physical output enable: DIR without smart logic, otherwise WRPIN's TT low bit.
    /// This is pad control; DIR independently controls reset inside the smart engine.
    pub fn outputEnabled(pin: *const Pin, dir: bool) bool {
        return switch (pin.configuration.mode) {
            .gpio => dir,
            else => pin.configuration.output_enable,
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
        .configure => if (value & 1 != 0)
            .acknowledge
        else
            .configure,
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

/// Cog DIR transport to the physical output gate, independent of output state propagation.
const DirectionPipeline = struct {
    directions: [8]?u64 = @splat(null),
    head: usize = 0,
};

/// One command slot per cog at each delayed delivery edge.
const CommandPipeline = struct {
    slots: [8][8]?Command = @splat(@splat(null)),
    head: usize = 0,
};

/// Pad, OUT, input, and smart-result samples from the same four-clock window.
const Samples = struct {
    pads: [4][64]Signal = @splat(@splat(.low)),
    receivers: [4][64]Signal = @splat(@splat(.low)),
    /// OUT history supplies the mode-specific two/three-clock input selector delay.
    out: [4]u64 = @splat(0),
    directions: [4]u64 = @splat(0),
    inputs: [4]u64 = @splat(0),
    results: [8][64]ReadResult = @splat(@splat(.{})),
    input_registers: u64 = 0,
    active: bool = false,
};

pins: [64]Pin = @splat(.{}),
/// Shared physical USB hardware, separate from the 64 per-pin smart-engine payloads.
usb: [32]smart_pin.Usb = @splat(.{}),
/// Voltage-derived deterministic ADC modulators; explicit test overrides retain their phase.
adc_inputs: [64]DeltaSigma = @splat(.{}),
adc_overrides: [64]?f64 = @splat(null),
receivers: [64]Receiver = @splat(.{}),
/// Ordinary receivers can publish the SIMD threshold plane without touching per-pin analog state.
simple_receivers: bool = true,
receiver_inputs: [64]Signal = @splat(.low),
adc_bits: [64]Signal = @splat(.low),
clocked_feedback: [64]Signal = @splat(.low),
/// Reused frame storage keeps ReleaseSafe from poisoning large scratch arrays every clock.
frame_inputs: [64]smart_pin.Inputs = @splat(.{}),
frame_outputs: [64]smart_pin.Outputs = @splat(.{}),
filters: [4]FilterConfiguration = .{
    .{ .length = .two, .tap = 0 },
    .{ .length = .three, .tap = 5 },
    .{ .length = .five, .tap = 19 },
    .{ .length = .eight, .tap = 22 },
},
smart_a: [64]Signal = @splat(.low),
smart_b: [64]Signal = @splat(.low),
pads: [64]Signal = @splat(.low),
external: [64]Pad.Source = @splat(.float),
analog_pads: Pads = .{},
last_driver_keys: [64]DriveKey = @splat(.floating),
pad_reads: [64]Signal = @splat(.low),
sync_out: [64]DriveSignal = @splat(.{}),
sync_in: [64]Signal = @splat(.low),
directions: u64 = 0,
physical_directions: u64 = 0,
direction_pipeline: DirectionPipeline = .{},
samples: Samples = .{},
routed_inputs: u64 = 0,
outputs: u64 = 0,
drives: u64 = 0,
clock_mode: u32 = 0,
clock: Clock = .init(.ideal),
/// Current shared output, latched by step; physical pad tap numbering is approximate.
random_bits: u64 = 0,
last_clock: u64 = 0,
/// Last input-routing edge, including the initial CT=0 reset state.
input_clock: u64 = 0,
commands: CommandPipeline = .{},
owners: [64]struct { cog: u3 = 0, pc: u20 = 0, instruction: u32 = 0 } = @splat(.{}),
collision: ?struct { clock: u64, pin: PinIndex, cogs: u8 } = null,
failure: ?struct { pin: PinIndex, err: anyerror } = null,

/// Bind each smart-pin USB forwarder to the bank after IO reaches its final address.
pub fn connect(io: *IO) void {
    for (&io.pins, 0..) |*pin, index| {
        pin.smart.connectUsb(&io.usb[index / 2].ports[index % 2]);
    }
}

/// Decode HUBSET's global filter selector, history length, and CT timing tap.
pub fn configureFilter(io: *IO, value: u32) void {
    const index: u2 = @truncate(value >> 7);
    io.filters[index] = .{
        .length = @enumFromInt(@as(u2, @truncate(value >> 5))),
        .tap = @truncate(value),
    };
}

/// Apply a supported HUBSET clock operation; the hub owns random seeding.
pub fn setClock(io: *IO, mode: u32) bool {
    if (!io.clock.configure(mode))
        return false;
    io.clock_mode = mode;
    return true;
}

/// Queue a cog command for its measured delivery edge, including calls outside Hub.step.
pub fn enqueue(io: *IO, hub: *Hub, command: Command) void {
    _ = hub;
    const slot = &io.commands.slots[(io.commands.head + command_delay) % io.commands.slots.len][command.cog];
    std.debug.assert(slot.* == null);
    slot.* = command;
}

/// Prepare one combined command without advancing its smart logic.
fn apply(io: *IO, index: PinIndex, command: Command, inputs: *smart_pin.Inputs) !void {
    const i = @intFromEnum(index);
    io.owners[i] = .{ .cog = command.cog, .pc = command.pc, .instruction = command.instruction };
    switch (command.kind) {
        .configure => {
            const configuration: PinConfiguration = @bitCast(command.value);
            io.pins[i].configure(configuration);
            inputs.ack = true;
        },
        .write_x => inputs.new_x = command.value,
        .write_y => inputs.new_y = command.value,
        .acknowledge => inputs.ack = true,
    }
}

/// Collect one frame, advance every smart pin once, then publish pads and sampled registers.
pub fn step(io: *IO, hub: *Hub) void {
    io.last_clock = hub.counter;
    io.random_bits = hub.random.output;
    io.updateDirections(hub);
    if (!io.samples.active) {
        io.samples.pads = @splat(io.pads);
        io.samples.receivers = @splat(io.receiver_inputs);
        io.samples.out = @splat(io.drives);
        io.samples.directions = @splat(io.directions);
        io.samples.inputs = @splat(io.routed_inputs);
        for (&io.samples.results) |*edge| {
            for (edge, &io.pins) |*result, *pin| {
                result.* = .{ .value = pin.outputs.y, .flag = pin.outputs.flag };
            }
        }
        io.samples.active = true;
    }
    if (io.direction_pipeline.directions[io.direction_pipeline.head]) |directions| {
        io.physical_directions = directions;
        io.direction_pipeline.directions[io.direction_pipeline.head] = null;
    }
    io.clocked_feedback = io.receiver_inputs;
    const inputs = &io.frame_inputs;
    for (inputs, 0..) |*sample, i| {
        sample.* = io.pinInputs(@enumFromInt(i));
        sample.new_x = io.pins[i].edits.x;
        sample.new_y = io.pins[i].edits.y;
        io.pins[i].edits = .{};
    }
    const slot = &io.commands.slots[io.commands.head];
    var touched: u64 = 0;
    for (slot) |command| {
        if (command) |c| {
            touched |= c.mask;
        }
    }
    while (touched != 0) {
        const index: PinIndex = @enumFromInt(@ctz(touched));
        const mask = @as(u64, 1) << @intFromEnum(index);
        touched &= touched - 1;
        var combined: ?Command = null;
        var cogs: u8 = 0;
        for (slot) |command| {
            if (command) |c| {
                if (c.mask & mask == 0)
                    continue;
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
            }
        }
        if (@popCount(cogs) > 1) {
            io.collision = .{ .clock = hub.counter, .pin = index, .cogs = cogs };
            std.log.warn("smart pin {d} command collision at clock {d}, cog mask 0x{x}", .{ @intFromEnum(index), hub.counter, cogs });
            for (slot) |command| {
                if (command) |c| {
                    if (c.mask & mask != 0) {
                        std.log.warn("  cog {d}, pc 0x{x}: {t} value 0x{x:0>8}", .{ c.cog, c.pc, c.kind, c.value });
                    }
                }
            }
        }
        io.apply(index, combined.?, &inputs[@intFromEnum(index)]) catch |err| {
            io.fail(hub, index, err);
            return;
        };
    }
    slot.* = @splat(null);
    io.routeInputs(&hub.system_configuration);
    for (inputs, &io.pins, io.smart_a, io.smart_b, io.pad_reads) |*sample, *pin, a, b, physical| {
        sample.physical = physical;
        sample.a = a;
        sample.b = b;
        sample.dac_mode = pin.configuration.pad_mode.is_dac();
    }
    // Keep all outputs from the previous frame visible until every pin has advanced.
    const next = &io.frame_outputs;
    for (&io.pins, inputs) |*pin, sample| {
        pin.smart.sample(sample);
    }
    for (&io.usb) |*usb| {
        usb.clock();
    }
    for (&io.pins, next, 0..) |*pin, *state, i| {
        state.* = pin.smart.clock() catch |err| {
            io.fail(hub, @enumFromInt(i), err);
            return;
        };
    }
    io.routed_inputs = 0;
    for (&io.pins, next, 0..) |*pin, next_state, i| {
        const state = next_state;
        io.routed_inputs |= @as(u64, state.in) << @intCast(i);
        pin.outputs = state;
    }
    for (&hub.cogs) |*cog| {
        if (!cog.scope_configuration.enabled)
            continue;
        const base = @as(usize, cog.scope_configuration.pin_base) * 4;
        var word: u32 = 0;
        for (0..4) |channel| {
            const value = io.readResult(@enumFromInt(base + channel), hub.counter).value;
            word |= (value & 255) << @as(u5, @intCast(channel * 8));
        }
        cog.scope = word;
    }
    // Delay the selected output in every mode, including GPIO and WRPIN mux changes.
    for (&io.pins, 0..) |*pin, i| {
        const mux_signal = io.selectedOutput(@enumFromInt(i));
        const output = pin.output_pipeline.step(mux_signal);
        pin.output = output.level;
        pin.strength = output.strength;
    }
    for (&io.sync_out, 0..) |*state, i| {
        const configuration = io.pins[i].configuration;
        if (configuration.isAdc() or configuration.hasFeedback() or !configuration.synchronous)
            continue;
        const sync_pipeline = &io.pins[i].sync_output_pipeline;
        state.* = sync_pipeline.step(.{ .level = io.outputState(@enumFromInt(i)), .strength = io.pins[i].strength });
    }
    io.resolvePads(hub, Clock.secondsOf(io.clock.last_interval.period), true);
    io.samples.pads[hub.counter % io.samples.pads.len] = io.pads;
    io.samples.receivers[hub.counter % io.samples.receivers.len] = io.receiver_inputs;
    io.samples.out[hub.counter % io.samples.out.len] = io.drives;
    io.samples.directions[hub.counter % io.samples.directions.len] = io.directions;
    io.samples.inputs[hub.counter % io.samples.inputs.len] = io.routed_inputs;
    io.samples.input_registers = io.samples.inputs[(hub.counter -% 2) % io.samples.inputs.len];
    const results = &io.samples.results[hub.counter % io.samples.results.len];
    for (results, &io.pins) |*result, *pin| {
        result.* = .{ .value = pin.outputs.y, .flag = pin.outputs.flag };
    }
}

/// Read the mode-specific return-bus stage measured by the smart-pin boundary probes.
pub fn readResult(io: *const IO, index: PinIndex, clock: u64) ReadResult {
    if (!io.samples.active) {
        const state = io.pins[@intFromEnum(index)].outputs;
        return .{ .value = state.y, .flag = state.flag };
    }
    const delay = io.pins[@intFromEnum(index)].smart.resultLatency();
    return io.samples.results[(clock -% delay) % io.samples.results.len][@intFromEnum(index)];
}

/// Advance fixed-delay command and GPIO slots after all producers have run.
pub fn advance_delays(io: *IO) void {
    io.commands.head = (io.commands.head + 1) % io.commands.slots.len;
    io.direction_pipeline.head = (io.direction_pipeline.head + 1) % io.direction_pipeline.directions.len;
}

/// Record a pin I/O failure and fault the issuing cog.
fn fail(io: *IO, hub: *Hub, index: PinIndex, err: anyerror) void {
    io.failure = .{ .pin = index, .err = err };
    const owner = io.owners[@intFromEnum(index)];
    hub.fault = .{
        .cog = owner.cog,
        .pc = owner.pc,
        .instruction = owner.instruction,
        .result = .{ .illegal = "smart-pin I/O failure (see pin diagnostic)" },
    };
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
        drives |= dir & (out | cog.streamer.pins);
    }
    if (directions != io.directions or outputs != io.outputs or drives != io.drives) {
        io.directions = directions;
        io.outputs = outputs;
        io.drives = drives;
        // Optimistic digital transport excludes electrical pad slew and threshold-crossing delays.
        const delay = 3;
        io.direction_pipeline.directions[(io.direction_pipeline.head + delay) % io.direction_pipeline.directions.len] = directions;
    }
}

/// Report the physical output gate used by pad resolution, including delayed GPIO DIR and smart TT.
pub fn outputEnabled(io: *const IO, index: PadIndex) bool {
    const i = @intFromEnum(index);
    return io.pins[i].outputEnabled(io.physical_directions & (@as(u64, 1) << i) != 0);
}

/// Apply a host WRPIN edit through the same configuration decoder as delayed commands.
pub fn editConfiguration(io: *IO, index: PinIndex, value: u32) void {
    io.pins[@intFromEnum(index)].configure(@bitCast(value));
}

pub const SmartRegister = enum { x, y };

/// Update a paused architectural register and retain the write strobe for the next engine clock.
/// A simulated command delivered on that edge has priority over the host edit.
pub fn editSmartRegister(io: *IO, index: PinIndex, register: SmartRegister, value: u32) void {
    const pin = &io.pins[@intFromEnum(index)];
    switch (register) {
        .x => {
            pin.smart.registers.x = value;
            pin.edits.x = value;
        },
        .y => {
            pin.smart.registers.y = value;
            pin.edits.y = value;
        },
    }
    pin.smart.registers.ready = false;
}

/// Validate and stage an external source; replacing it preserves the capacitor voltage.
pub fn setExternal(io: *IO, index: PadIndex, source: Pad.Source) error{InvalidSource}!void {
    try source.validate();
    io.external[@intFromEnum(index)] = source;
    io.analog_pads.setExternal(@intFromEnum(index), source);
}

/// Return the last completed electrical snapshot, or null before the first clock after reset.
pub fn getExternal(io: *const IO, index: PadIndex) ?Pad.State {
    return io.analog_pads.get(@intFromEnum(index));
}

/// Override a normalized ADC density for tests, or restore electrical conversion with null.
pub fn setAdcOverride(io: *IO, index: PinIndex, density: ?f64) error{InvalidDensity}!void {
    if (density) |value| {
        if (!std.math.isFinite(value) or value < 0 or value > 1)
            return error.InvalidDensity;
        io.adc_inputs[@intFromEnum(index)].density = value;
    }
    io.adc_overrides[@intFromEnum(index)] = density;
}

/// Build one smart-logic input sample from the aggregate cog registers and routed pads.
fn pinInputs(io: *const IO, index: PinIndex) smart_pin.Inputs {
    const i = @intFromEnum(index);
    const mask = @as(u64, 1) << i;
    const direction = io.directions & mask != 0;
    const registered_direction = if (io.samples.active)
        io.samples.directions[(io.last_clock -% 2) % io.samples.directions.len] & mask != 0
    else
        direction;
    return .{
        .a = io.smart_a[i],
        .b = io.smart_b[i],
        .out = @intFromBool(io.drives & mask != 0),
        .dir = io.pins[i].smart.directionInput(direction, registered_direction),
        .clock = io.last_clock,
        .random = @truncate(std.math.rotr(u64, io.random_bits, @as(u6, @intCast(i)))),
    };
}

/// Obtain the neighboring engine's output for the physical OTHER selector.
fn smartOutput(io: *const IO, index: PinIndex) Signal {
    return io.pins[@intFromEnum(index)].outputs.smart_out;
}

/// Select the current OUT/OTHER/SmartOut signal before the common output pipeline.
fn selectedOutput(io: *const IO, index: PadIndex) DriveSignal {
    const i = @intFromEnum(index);
    const pin = &io.pins[i];
    return switch (pin.outputSource()) {
        .smart => .{ .level = pin.outputs.smart_out, .strength = pin.outputs.strength },
        .other => .{ .level = if (i & 1 != 0)
            io.smartOutput(@enumFromInt(i ^ 1)).invert()
        else
            Signal.from((io.random_bits >> @intCast(i)) & 1 != 0) },
    };
}

/// Resolve feedback selection and output inversion after registered output selection.
fn outputState(io: *const IO, index: PadIndex) Signal {
    const i = @intFromEnum(index);
    const configuration = io.pins[i].configuration;
    var state = io.pins[i].output;
    if (configuration.hasFeedback()) {
        state = if (configuration.synchronous)
            io.clocked_feedback[i]
        else
            io.receivers[i].output;
        if (configuration.pad_mode.has_negative_feedback()) {
            state = state.invert();
        }
    }
    if (!configuration.pad_mode.is_level() and configuration.invert_output) {
        state = state.invert();
    }
    return state;
}

/// Cached electrical identity: zero floats, other values encode selected WRPIN drive and rail.
const DriveKey = enum(u16) { floating, _ };

/// Select registered drive characteristics without rebuilding unchanged numeric descriptors.
fn driveKey(io: *const IO, hub: *const Hub, index: PadIndex) DriveKey {
    const i = @intFromEnum(index);
    const configuration = io.pins[i].configuration;
    if (!io.outputEnabled(index))
        return .floating;
    if (configuration.isDac()) {
        const code = if (configuration.isSmartDac())
            io.pins[i].outputs.dac orelse 0
        else if (configuration.mode != .gpio or configuration.output_source == .other) blk: {
            const high = io.pins[i].output == .high;
            const nibble: u8 = if (high)
                configuration.dacCode() >> 4
            else
                configuration.dacCode() & 15;
            break :blk nibble * 17;
        } else if (configuration.output_enable) blk: {
            const cog = configuration.dacCode() & 15;
            break :blk if (cog < hub.cogs.len)
                hub.cogs[cog].dacs[i % 4]
            else
                0;
        } else configuration.dacCode();
        return @enumFromInt(17 + @as(u16, @intFromEnum(configuration.dacRange())) * 256 + code);
    }
    const drive = if (!configuration.isAdc() and !configuration.hasFeedback() and configuration.synchronous)
        io.sync_out[i]
    else
        DriveSignal{ .level = io.outputState(index), .strength = io.pins[i].strength };
    const state = drive.level;
    const mode: PinConfiguration.Drive = drive.strength orelse if (configuration.pad_mode.is_level())
        .resistor_1k5
    else switch (state) {
        .low => configuration.low_drive,
        .high => configuration.high_drive,
    };
    if (mode == .float)
        return .floating;
    return @enumFromInt(1 + @as(u16, @intFromEnum(mode)) * 2 + @intFromEnum(state));
}

/// Expand a changed physical drive identity using the immutable initialized system parameters.
fn driver(key: DriveKey, system: *const Hub.SystemConfiguration) Pad.Driver {
    if (key == .floating)
        return .float;
    if (@intFromEnum(key) >= 17) {
        const encoded = @intFromEnum(key) - 17;
        const range: PinConfiguration.DacRange = @enumFromInt(@as(u2, @intCast(encoded / 256)));
        return Pad.dacDriver(system, range, @truncate(encoded));
    }
    const encoded = @intFromEnum(key) - 1;
    const mode: PinConfiguration.Drive = @enumFromInt(encoded >> 1);
    return Pad.internalDriver(system, .{ .low_drive = mode, .high_drive = mode }, Signal.from(encoded & 1 != 0), true);
}

/// Inspect the actual committed electrical driver without decoding private cache identities.
pub fn resolvedDriver(io: *const IO, index: PadIndex, system: *const Hub.SystemConfiguration) Pad.Driver {
    return driver(io.last_driver_keys[@intFromEnum(index)], system);
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
    const out_delay: u64 = switch (io.pins[i].configuration.mode) {
        .adc, .adc_ext, .adc_scope => 3,
        else => 2,
    };
    const out = if (io.samples.active)
        io.samples.out[(io.last_clock -% out_delay) % io.samples.out.len]
    else
        io.drives;
    const state = if (selector.source == .out)
        Signal.from(out & (@as(u64, 1) << i) != 0)
    else
        io.pad_reads[@intFromEnum(pad)];
    return if (selector.invert)
        state.invert()
    else
        state;
}

/// Advance simultaneous RC substeps, updating asynchronous feedback between electrical solves.
fn resolvePads(io: *IO, hub: *Hub, seconds: f64, clocked: bool) void {
    const system = &hub.system_configuration;
    const dt = seconds / @as(f64, @floatFromInt(system.oversample));
    if (hub.vcd) |vcd| {
        if (clocked and system.oversample > 1) {
            vcd.beginClock(hub.counter, io.clock.last_interval.period);
        }
    }
    var all_logic = true;
    for (&io.pins) |*pin| {
        if (pin.configuration.pad_mode != .logic) {
            all_logic = false;
            break;
        }
    }
    const simple = all_logic and dt >= system.receiver_delay;
    if (!simple and io.simple_receivers) {
        for (&io.receivers, io.receiver_inputs) |*receiver, input| {
            receiver.output = input;
            receiver.pending = input;
            receiver.remaining = 0;
        }
    }
    io.simple_receivers = simple;
    var asynchronous_feedback = false;
    for (0..system.oversample) |substep| {
        if (substep == 0 or asynchronous_feedback) {
            for (0..64) |i| {
                const configuration = io.pins[i].configuration;
                asynchronous_feedback = asynchronous_feedback or
                    (configuration.hasFeedback() and !configuration.synchronous);
                const key = io.driveKey(hub, @enumFromInt(i));
                if (key != io.last_driver_keys[i]) {
                    io.analog_pads.setInternal(i, driver(key, system));
                    io.last_driver_keys[i] = key;
                }
            }
        }
        io.analog_pads.step(system, dt);
        const levels = io.analog_pads.digital(system);
        if (simple) {
            io.receiver_inputs = levels;
        } else {
            for (&io.receivers, &io.pins, &io.receiver_inputs, levels, 0..) |*receiver, *pin, *input, level, i| {
                if (pin.configuration.pad_mode == .logic and dt >= system.receiver_delay) {
                    receiver.output = level;
                    receiver.pending = level;
                    receiver.remaining = 0;
                } else {
                    const voltage = io.analog_pads.get(i).?.voltage;
                    const adjacent = io.analog_pads.get(i ^ 1).?.voltage;
                    receiver.step(pin.configuration, system, voltage, adjacent, dt);
                }
                input.* = receiver.output;
            }
        }
        if (hub.vcd) |vcd| {
            if (clocked and substep + 1 < system.oversample) {
                vcd.capturePads(io, system, @intCast(substep + 1));
            }
        }
    }
    const next = io.analog_pads.digital(system);
    io.pads = next;
    if (clocked) {
        io.sync_in = io.receiver_inputs;
    }
}

/// Integrate asynchronous electrical state during a clock-not-ready interval without clocking ADCs or smart pins.
pub fn advanceQuiet(io: *IO, hub: *Hub, seconds: f64) void {
    io.resolvePads(hub, seconds, false);
}

/// Compute SmartA/SmartB from the previous pad samples without advancing smart logic.
fn routeInputs(io: *IO, system: *const Hub.SystemConfiguration) void {
    const previous_clock = io.input_clock;
    io.input_clock = io.last_clock;
    const pads = if (io.samples.active)
        io.samples.receivers[(io.last_clock -% 2) % io.samples.receivers.len]
    else
        io.receiver_inputs;
    for (&io.pad_reads, 0..) |*state, i| {
        const configuration = io.pins[i].configuration;
        const adc = configuration.isAdc();
        const input = if (adc) blk: {
            if (configuration.isDac()) {
                if (!configuration.isSmartDac() and
                    (configuration.mode != .gpio or configuration.output_source == .other))
                    break :blk Signal.low;
                const enabled = if (configuration.isSmartDac() and configuration.output_source == .other)
                    io.selectedOutput(@enumFromInt(i)).level == .high
                else
                    io.outputs & (@as(u64, 1) << @intCast(i)) != 0;
                if (!enabled)
                    break :blk Signal.low;
            }
            const voltage = if (io.analog_pads.get(i)) |pad|
                pad.voltage
            else
                0;
            io.adc_inputs[i].density = io.adc_overrides[i] orelse Receiver.adcDensity(configuration, system, voltage);
            break :blk Signal.from(io.adc_inputs[i].step());
        } else if (configuration.synchronous and configuration.pad_mode.is_level())
            io.samples.receivers[(io.last_clock -% 3) % io.samples.receivers.len][i]
        else if (configuration.synchronous)
            io.sync_in[i]
        else
            pads[i];
        io.adc_bits[i] = if (adc)
            input
        else
            .low;
        // ADC source/gain bits alias the GPIO inversion and synchronization fields.
        state.* = if (!adc and !configuration.pad_mode.is_level() and configuration.invert_input)
            input.invert()
        else
            input;
    }
    for (0..io.pins.len) |i| {
        const configuration = io.pins[i].configuration;
        const a = io.selected(@enumFromInt(i), configuration.a);
        const b = io.selected(@enumFromInt(i), configuration.b);
        switch (configuration.input_logic) {
            .a, .a_and_b, .a_or_b, .a_xor_b => {
                const operation: signal.InputLogic = @enumFromInt(@intFromEnum(configuration.input_logic));
                io.smart_a[i] = Signal.logic(a, b, operation);
                io.smart_b[i] = b;
            },
            .filter0, .filter1, .filter2, .filter3 => {
                const index = @intFromEnum(configuration.input_logic) - @intFromEnum(PinConfiguration.InputLogic.filter0);
                const filter = io.filters[index];
                const pin = &io.pins[i];
                const sample = filter.samples(io.last_clock, previous_clock);
                io.smart_a[i] = pin.filter_a.step(a, filter.length.mask(), sample);
                io.smart_b[i] = pin.filter_b.step(b, filter.length.mask(), sample);
            },
        }
    }
}

/// Return the cog-visible input register sample.
pub fn get_in(io: *const IO) u64 {
    return if (io.samples.active)
        io.samples.input_registers
    else
        io.routed_inputs;
}

/// Goertzel and selectable sensors observe IN one pipeline stage before the cog-visible TESTP sample.
pub fn eventInput(io: *const IO, index: PinIndex) bool {
    const inputs = if (io.samples.active)
        io.samples.inputs[(io.last_clock -% 1) % io.samples.inputs.len]
    else
        io.routed_inputs;
    return inputs & (@as(u64, 1) << @intFromEnum(index)) != 0;
}

/// Deliver one test command through the real bus delay and advance to its delivery frame.
fn testCommand(hub: *Hub, index: PinIndex, kind: CommandKind, value: u32) void {
    hub.io.enqueue(hub, .{
        .mask = @as(u64, 1) << @intFromEnum(index),
        .kind = kind,
        .value = value,
        .cog = 0,
        .pc = 0,
        .instruction = 0,
    });
    for (0..command_delay + 1) |_| {
        _ = hub.step();
    }
}

test "external RC snapshots are staged, clocked and sampled through the existing input pipeline" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    var configuration: Hub.SystemConfiguration = .ideal;
    configuration.clocks.rcfast.frequency = 20e6;
    try hub.init(configuration);
    const pad: PadIndex = @enumFromInt(10);
    try std.testing.expect(hub.io.getExternal(pad) == null);
    const source: Pad.Source = .{ .voltage = 3.3, .resistance = 1_000, .capacitance = 95e-12 };
    try hub.io.setExternal(pad, source);
    try std.testing.expect(hub.io.getExternal(pad) == null);
    try std.testing.expectEqual(Signal.low, hub.io.pads[10]);
    try std.testing.expectEqual(Hub.StepResult.normal, hub.step());
    const first = hub.io.getExternal(pad).?;
    try std.testing.expectApproxEqAbs(@as(f64, 1.1), first.voltage, 1e-12);
    try std.testing.expect(first.current > 0 and first.internal_current == 0);
    for (0..64) |index| {
        try std.testing.expect(hub.io.getExternal(@enumFromInt(index)) != null);
    }
    hub.io.clock.rcfast.specification.frequency = @floatFromInt(10_000_000);
    _ = hub.step();
    const second = hub.io.getExternal(pad).?;
    try std.testing.expectApproxEqAbs(@as(f64, 2.2), second.voltage, 1e-12);
    try std.testing.expectEqual(Signal.high, hub.io.pads[10]);
    try std.testing.expect(hub.io.get_in() & (1 << 10) == 0);
    try hub.io.setExternal(pad, .float);
    try std.testing.expectEqualDeep(second, hub.io.getExternal(pad).?);
    for (0..6) |_| {
        _ = hub.step();
    }
    const held = hub.io.getExternal(pad).?;
    try std.testing.expectEqual(second.voltage, held.voltage);
    try std.testing.expectEqual(@as(f64, 0), held.current);
    try std.testing.expect(hub.io.get_in() & (1 << 10) != 0);
    try std.testing.expectError(error.InvalidSource, hub.io.setExternal(pad, .{ .voltage = 0, .resistance = 0 }));
    try std.testing.expectEqualDeep(Pad.Source.float, hub.io.external[10]);
    try std.testing.expectEqualDeep(held, hub.io.getExternal(pad).?);
    try hub.init(.ideal);
    try std.testing.expect(hub.io.getExternal(pad) == null);
    _ = hub.step();
    try std.testing.expectEqualDeep(Pad.State{
        .voltage = 0,
        .current = 0,
        .internal_current = 0,
        .pin_current = 0,
        .violations = .{ .undervoltage = false, .overvoltage = false, .overcurrent = false },
    }, hub.io.getExternal(pad).?);
}

test "adjacent feedback uses the previous complete snapshot and advances each pad once" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    var configuration: Hub.SystemConfiguration = .ideal;
    configuration.clocks.rcfast.frequency = 20e6;
    try hub.init(configuration);
    try hub.io.setExternal(@enumFromInt(0), .high);
    _ = hub.step();
    try hub.io.setExternal(@enumFromInt(0), .float);
    hub.io.editConfiguration(@enumFromInt(0), 0x40000);
    hub.io.editConfiguration(@enumFromInt(1), 0x40000);
    hub.cogs[0].write_reg(.DIRA, 3);
    for (0..3) |_| {
        _ = hub.step();
        try std.testing.expectEqual(Signal.high, hub.io.pads[0]);
        try std.testing.expectEqual(Signal.low, hub.io.pads[1]);
    }
    _ = hub.step();
    try std.testing.expectEqual(Signal.low, hub.io.pads[0]);
    try std.testing.expectEqual(Signal.high, hub.io.pads[1]);
    const falling = hub.io.getExternal(@enumFromInt(0)).?;
    const rising = hub.io.getExternal(@enumFromInt(1)).?;
    try std.testing.expect(falling.voltage > 0.009 and falling.voltage < 0.01);
    try std.testing.expect(rising.voltage > 3.29 and rising.voltage < 3.3);
    _ = hub.step();
    try std.testing.expectEqual(Signal.high, hub.io.pads[0]);
    try std.testing.expectEqual(Signal.low, hub.io.pads[1]);
}

test "clock fields and reader-backed UART input are independent of terminal pin numbers" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    const io = &hub.io;
    try std.testing.expect(io.setClock(0x0100_09fb));
    try std.testing.expectEqual(@as(f64, 200_000_000), io.clock.frequency());
    try std.testing.expect(!io.setClock(3));
    try std.testing.expectEqual(@as(f64, 200_000_000), io.clock.frequency());
    try std.testing.expectEqual(@as(u32, 0x0100_09fb), io.clock_mode);

    var reader = std.Io.Reader.fixed("ab");
    var source: DataSource = .{ .reader = &reader };
    const pin = &io.pins[9].smart;
    pin.rx_mode = .{ .emulated = &source };
    testCommand(hub, @enumFromInt(9), .configure, 0x3e);
    testCommand(hub, @enumFromInt(9), .write_x, (8 << 16) | 7); // Eight-clock bits, including UART start and stop timing.
    try std.testing.expectEqual(@as(usize, 2), reader.bufferedLen());
    hub.cogs[0].write_reg(.DIRA, 1 << 9);
    _ = hub.step();
    try std.testing.expectEqual(@as(usize, 1), reader.bufferedLen());
    for (0..78) |_| {
        _ = hub.step();
    }
    try std.testing.expectEqual(@as(u32, 'a') << 24, pin.registers.result);
    try std.testing.expectEqual(@as(usize, 0), reader.bufferedLen());
    // DIR reset discards the in-flight second frame before it becomes readable.
    hub.cogs[0].write_reg(.DIRA, 0);
    _ = hub.step();
    for (0..20) |_| {
        _ = hub.step();
    }
    try std.testing.expect(!pin.registers.ready);
    try std.testing.expectEqual(@as(u32, 'a') << 24, pin.registers.result);
    try std.testing.expect(io.pins[63].smart.rx_mode == .exact);
}

test "hardware repository command timing and OR-combined mixed command buses" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    hub.cogs[0].write_reg(.DIRB, 1 << 8);
    const pin = &hub.io.pins[40].smart;
    testCommand(hub, @enumFromInt(40), .configure, 2);
    testCommand(hub, @enumFromInt(40), .write_x, 0);
    for (0..6) |_| {
        _ = hub.step();
    }
    hub.counter = 100;
    hub.io.enqueue(hub, .{ .mask = @as(u64, 1) << 40, .kind = .write_x, .value = 0x155, .cog = 0, .pc = 1, .instruction = 0 });
    hub.io.enqueue(hub, .{ .mask = @as(u64, 1) << 40, .kind = .write_x, .value = 0xaa, .cog = 7, .pc = 2, .instruction = 0 });
    for (0..4) |_| {
        _ = hub.step();
        try std.testing.expectEqual(@as(u32, 0), hub.io.readResult(@enumFromInt(40), hub.counter -% 1).value);
        try std.testing.expect(hub.io.get_in() & (@as(u64, 1) << 40) != 0);
    }
    _ = hub.step();
    try std.testing.expectEqual(@as(u32, 0x1ff), pin.registers.result);
    try std.testing.expect(hub.io.get_in() & (@as(u64, 1) << 40) == 0);
    try std.testing.expectEqual(@as(u8, 0x81), hub.io.collision.?.cogs);
    _ = hub.step();
    try std.testing.expect(hub.io.get_in() & (@as(u64, 1) << 40) != 0);
    hub.cogs[0].write_reg(.DIRB, 0);
    testCommand(hub, @enumFromInt(40), .write_x, 0x12345678);
    try std.testing.expectEqual(@as(u32, 0x1ff), pin.registers.result);
    try std.testing.expect(!pin.registers.ready);
    hub.cogs[0].write_reg(.DIRB, 1 << 8);
    // X | Y selects WRPIN; an odd combined value becomes ACK, leaving Z alone.
    hub.io.enqueue(hub, .{ .mask = @as(u64, 1) << 40, .kind = .write_x, .value = 0x100, .cog = 0, .pc = 1, .instruction = 0 });
    hub.io.enqueue(hub, .{ .mask = @as(u64, 1) << 40, .kind = .write_y, .value = 1, .cog = 1, .pc = 2, .instruction = 0 });
    for (0..5) |_| {
        _ = hub.step();
    }
    try std.testing.expectEqual(@as(u32, 0x1ff), pin.registers.result);
    try std.testing.expectEqual(@as(u32, 2), @as(u32, @bitCast(hub.io.pins[40].configuration)));
}

test "GPIO pad pipeline has fixed cycle timing at every clock frequency" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    for ([_]u64{ 20_000_000, 167_000_000, 170_000_000, 200_000_000 }) |frequency| {
        try hub.init(.ideal);
        hub.io.clock.rcfast.specification.frequency = @floatFromInt(frequency);
        hub.cogs[0].registers.set(.DIRA, 1);
        hub.cogs[0].registers.set(.OUTA, 1);
        for (0..3) |_| {
            _ = hub.step();
            try std.testing.expectEqual(Signal.low, hub.io.pads[0]);
        }
        _ = hub.step();
        try std.testing.expectEqual(Signal.high, hub.io.pads[0]);

        hub.cogs[0].registers.set(.OUTA, 0);
        for (0..3) |_| {
            _ = hub.step();
            try std.testing.expectEqual(Signal.high, hub.io.pads[0]);
        }
        _ = hub.step();
        try std.testing.expectEqual(Signal.low, hub.io.pads[0]);
    }
}

test "frequency changes preserve in-flight GPIO timing and history" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    hub.cogs[0].registers.set(.DIRA, 1 << 4);
    for (0..8) |_| {
        _ = hub.step();
    }
    // Clock changes on the publication edges must neither delay nor advance queued transitions.
    const expected = [_]Signal{ .low, .low, .low, .high, .high, .high, .high, .high, .low, .low };
    for (expected, 0..) |level, clock| {
        switch (clock) {
            0 => hub.cogs[0].registers.set(.OUTA, 1 << 4),
            3 => hub.io.clock.rcfast.specification.frequency = 200_000_000,
            5 => hub.cogs[0].registers.set(.OUTA, 0),
            8 => hub.io.clock.rcfast.specification.frequency = 20_000_000,
            else => {},
        }
        _ = hub.step();
        try std.testing.expectEqual(level, hub.io.pads[4]);
    }
}

test "synchronous output retains pending transitions while deselected across clock changes" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    hub.cogs[0].registers.set(.DIRA, 1 << 4);
    for (0..8) |_| {
        _ = hub.step();
    }
    hub.io.editConfiguration(@enumFromInt(4), 1 << 16);
    const pin = &hub.io.pins[4];
    // The first high enters synchronization at clock 3; deselection pauses it until clock 7.
    const expected = [_]Signal{ .low, .low, .low, .low, .high, .high, .high, .low, .low, .high, .low };
    var frozen = pin.sync_output_pipeline;
    for (expected, 0..) |level, clock| {
        switch (clock) {
            0 => hub.cogs[0].registers.set(.OUTA, 1 << 4),
            4 => {
                frozen = pin.sync_output_pipeline;
                hub.io.editConfiguration(@enumFromInt(4), 0);
                hub.cogs[0].registers.set(.OUTA, 0);
            },
            7 => {
                hub.io.clock.rcfast.specification.frequency = @floatFromInt(200_000_000);
                hub.io.editConfiguration(@enumFromInt(4), 1 << 16);
            },
            else => {},
        }
        _ = hub.step();
        try std.testing.expectEqual(level, hub.io.pads[4]);
        if (clock >= 4 and clock < 7) {
            try std.testing.expectEqualDeep(frozen, pin.sync_output_pipeline);
        }
    }
}

test "GPIO cog OR, A/B routing, float, feedback and contention remain separate from IN" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    hub.cogs[0].write_reg(.DIRA, 1);
    hub.cogs[1].write_reg(.OUTA, 1);
    for (0..9) |_| {
        _ = hub.step();
    }
    try std.testing.expectEqual(Signal.low, hub.io.pads[0]);
    hub.cogs[1].write_reg(.DIRA, 1);
    for (0..9) |_| {
        _ = hub.step();
    }
    try std.testing.expectEqual(Signal.high, hub.io.pads[0]);
    try std.testing.expectEqual(@as(u64, 1), hub.io.get_in());
    try hub.io.setExternal(@enumFromInt(0), .low);
    for (0..5) |_| {
        _ = hub.step();
    }
    try std.testing.expectEqual(Signal.low, hub.io.pads[0]);
    const contested = hub.io.getExternal(@enumFromInt(0)).?;
    try std.testing.expectApproxEqAbs(@as(f64, 3.3 / 31.0), contested.voltage, 1e-10);
    try std.testing.expect(contested.internal_current > 0 and contested.current < 0);
    try std.testing.expectEqual(@as(u64, 0), hub.io.get_in());
    try hub.io.setExternal(@enumFromInt(0), .float);
    hub.cogs[0].write_reg(.DIRA, 0);
    hub.cogs[1].write_reg(.DIRA, 0);
    for (0..9) |_| {
        _ = hub.step();
    }
    try std.testing.expectEqual(Signal.high, hub.io.pads[0]);
    try std.testing.expect(!hub.io.outputEnabled(@enumFromInt(0)));
    try hub.io.setExternal(@enumFromInt(63), .high);
    testCommand(hub, @enumFromInt(0), .configure, 0x70000000); // A = relative -1, wrapping all 64 pins.
    try std.testing.expect(hub.io.smart_a[0].sample());
    testCommand(hub, @enumFromInt(0), .configure, 0xf0000000);
    try std.testing.expect(!hub.io.smart_a[0].sample());
    testCommand(hub, @enumFromInt(0), .configure, 0x3800); // High drive floats.
    hub.cogs[0].write_reg(.OUTA, 1);
    hub.cogs[0].write_reg(.DIRA, 1);
    for (0..5) |_| {
        _ = hub.step();
    }
    try std.testing.expectEqual(Signal.high, hub.io.pads[0]);
    try std.testing.expectEqual(@as(f64, 0), hub.io.getExternal(@enumFromInt(0)).?.internal_current);
    // Adjacent-pin feedback drives pin 0 from externally driven pin 1.
    try hub.io.setExternal(@enumFromInt(1), .high);
    testCommand(hub, @enumFromInt(0), .configure, 0x40000);
    try std.testing.expectEqual(Signal.high, hub.io.pads[0]);
    testCommand(hub, @enumFromInt(2), .configure, 2);
    hub.cogs[0].write_reg(.DIRA, 5);
    testCommand(hub, @enumFromInt(2), .write_x, 0);
    for (0..3) |_| {
        _ = hub.step();
    }
    try std.testing.expectEqual(Signal.low, hub.io.pads[2]);
    try std.testing.expect(hub.io.get_in() & 4 != 0);
}

test "independent UART pins and endpoint errors identify their pin and issuing cog" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();

    var sink: DataSink = .{ .writer = &output.writer };
    for ([_]PinIndex{ @enumFromInt(4), @enumFromInt(33) }) |index| {
        hub.io.pins[@intFromEnum(index)].smart.tx_mode = .{ .virtual = &sink };
        testCommand(hub, index, .configure, 0x7c);
        testCommand(hub, index, .write_x, (1 << 16) | 7);
    }
    hub.cogs[0].write_reg(.DIRA, 1 << 4);
    hub.cogs[0].write_reg(.DIRB, 1 << 1);
    hub.io.enqueue(hub, .{ .mask = 1 << 4, .kind = .write_y, .value = 'a', .cog = 0, .pc = 0, .instruction = 0 });
    hub.io.enqueue(hub, .{ .mask = @as(u64, 1) << 33, .kind = .write_y, .value = 'b', .cog = 1, .pc = 0, .instruction = 0 });
    for (0..command_delay + 11) |_| {
        _ = hub.step();
    }
    try std.testing.expectEqualStrings("ab", output.written());
    const Broken = struct {
        /// Fail a test transmission to exercise instruction provenance.
        fn write(_: *anyopaque, _: u32) anyerror!void {
            return error.TestSinkFailure;
        }
    };
    var context: u8 = 0;
    var broken: DataSink = .{ .callback = .{ .context = &context, .write = Broken.write } };
    hub.io.pins[33].smart.tx_mode = .{ .virtual = &broken };
    hub.io.enqueue(hub, .{ .mask = @as(u64, 1) << 33, .kind = .write_y, .value = 'c', .cog = 6, .pc = 123, .instruction = 456 });
    for (0..command_delay + 11) |_| {
        _ = hub.step();
    }
    try std.testing.expectEqual(@as(PinIndex, @enumFromInt(33)), hub.io.failure.?.pin);
    try std.testing.expectEqual(error.TestSinkFailure, hub.io.failure.?.err);
    try std.testing.expectEqual(@as(u3, 6), hub.fault.?.cog);
    try std.testing.expectEqual(@as(u20, 123), hub.fault.?.pc);
}

test "a primed USART exposes its first bit on enable and waits for B clocks" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    testCommand(hub, @enumFromInt(8), .configure, 56);
    testCommand(hub, @enumFromInt(8), .write_y, 123);
    try std.testing.expect(hub.io.pins[8].smart.logic.sync_tx.buffer != null);
    try std.testing.expect(!hub.io.pins[8].outputs.scheduled);
    hub.cogs[0].write_reg(.DIRA, 1 << 8);
    try std.testing.expect(!hub.io.pins[8].smart.registers.enabled);
    _ = hub.step();
    try std.testing.expect(hub.io.pins[8].smart.logic.sync_tx.buffer == null);
    try std.testing.expect(hub.io.pins[8].smart.logic.sync_tx.shifter != null);
    try std.testing.expect(!hub.io.pins[8].smart.registers.ready);
}

test "all DAC encodings use repository semantics without DAC_MODE" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    hub.cogs[0].write_reg(.DIRA, 1 << 8);
    for ([_]u32{ 2, 4, 6 }) |configuration| {
        testCommand(hub, @enumFromInt(8), .configure, configuration);
        const pin = &hub.io.pins[8].smart;
        try std.testing.expectEqual(configuration, @as(u32, @bitCast(hub.io.pins[8].configuration)));
        try std.testing.expectEqual(hub.io.pins[8].configuration.mode, std.meta.activeTag(pin.logic));
        testCommand(hub, @enumFromInt(8), .write_x, 0x89abcdef);
        _ = hub.step();
        try std.testing.expect(pin.registers.ready);
        try std.testing.expect(hub.io.pins[8].outputs.dac == null);
        try std.testing.expectEqual(@as(u32, 0x89abcdef), pin.registers.result);
        hub.cogs[0].write_reg(.DIRA, 0);
        testCommand(hub, @enumFromInt(8), .write_x, 0);
        try std.testing.expectEqual(@as(u32, 0x89abcdef), pin.registers.result);
        hub.cogs[0].write_reg(.DIRA, 1 << 8);
        _ = hub.step();
    }
}

test "a partially configured USB pair starts with DIR low without a configuration fault" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    const index: PinIndex = @enumFromInt(9);
    hub.io.enqueue(hub, .{
        .mask = @as(u64, 1) << @intFromEnum(index),
        .kind = .configure,
        .value = @as(u32, @intFromEnum(smart_pin.Mode.usb)) << 1,
        .cog = 5,
        .pc = 0x1234,
        .instruction = 0xfc000009,
    });
    for (0..command_delay) |_| {
        _ = hub.step();
        try std.testing.expect(hub.fault == null);
    }
    _ = hub.step();
    try std.testing.expect(hub.io.pins[9].smart.logic == .usb);
    try std.testing.expectEqual(@as(u64, 0), hub.io.directions);
    try std.testing.expect(hub.io.failure == null);
    try std.testing.expect(hub.fault == null);
    try std.testing.expectEqual(@as(u32, 0), hub.io.pins[9].outputs.y);
}

test "each pin advances once per frame and outputs publish together after fixed input sampling" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    const Device = struct {
        hub: *Hub,
        calls: usize = 0,
        published_in: u1 = 0,
        /// Observe the previous output snapshot while counting frame service calls.
        fn read(context: *anyopaque) anyerror!?u32 {
            const device: *@This() = @ptrCast(@alignCast(context));
            for (&device.hub.io.pins) |*pin| {
                try std.testing.expectEqual(device.published_in, pin.outputs.in);
            }
            try std.testing.expectEqual(std.math.maxInt(u64), device.hub.io.directions);
            device.calls += 1;
            return 1;
        }
    };
    var device: Device = .{ .hub = hub };
    var source: DataSource = .{ .callback = .{ .context = &device, .read = Device.read } };
    for (&hub.io.pins) |*pin| {
        pin.smart.rx_mode = .{ .virtual = &source };
    }
    hub.cogs[0].write_reg(.DIRA, std.math.maxInt(u32));
    hub.cogs[0].write_reg(.DIRB, std.math.maxInt(u32));
    hub.io.enqueue(hub, .{ .mask = std.math.maxInt(u64), .kind = .configure, .value = 58, .cog = 0, .pc = 0, .instruction = 0 });
    try hub.io.setExternal(@enumFromInt(0), .high);
    try std.testing.expectEqual(@as(usize, 0), device.calls);
    try std.testing.expect(hub.io.pins[0].smart.logic == .gpio);
    for (0..command_delay) |_| {
        _ = hub.step();
        try std.testing.expect(hub.fault == null);
        try std.testing.expectEqual(@as(usize, 0), device.calls);
        // Even ordinary GPIO pins receive the aggregate DIR in their one frame update.
        for (&hub.io.pins) |*pin| {
            try std.testing.expect(pin.smart.registers.enabled);
        }
    }
    _ = hub.step();
    try std.testing.expect(hub.fault == null);
    try std.testing.expectEqual(@as(usize, 64), device.calls);
    for (&hub.io.pins) |*pin| {
        try std.testing.expectEqual(@as(u1, 1), pin.outputs.in);
    }
    device.published_in = 1;
    // A receiver holds its word until the shared ACK arrives; that frame reads once again.
    hub.io.enqueue(hub, .{ .mask = std.math.maxInt(u64), .kind = .acknowledge, .value = 0, .cog = 0, .pc = 0, .instruction = 0 });
    for (0..command_delay) |_| {
        _ = hub.step();
        try std.testing.expectEqual(@as(usize, 64), device.calls);
    }
    _ = hub.step();
    try std.testing.expect(hub.fault == null);
    try std.testing.expectEqual(@as(usize, 128), device.calls);
    // External updates and command queueing cannot invoke a pin between frames.
    try hub.io.setExternal(@enumFromInt(0), .low);
    hub.io.enqueue(hub, .{ .mask = 1, .kind = .write_x, .value = 7, .cog = 0, .pc = 0, .instruction = 0 });
    try std.testing.expectEqual(@as(usize, 128), device.calls);
    try std.testing.expectEqual(@as(u32, 0), hub.io.pins[0].smart.registers.x);
}

test "reused frame inputs do not replay writes or acknowledgements" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    var bytes: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer bytes.deinit();

    var sink: DataSink = .{ .writer = &bytes.writer };
    hub.io.pins[10].smart.tx_mode = .{ .virtual = &sink };
    hub.cogs[0].write_reg(.DIRA, 1 << 10);
    const configuration: PinConfiguration = .{ .mode = .sync_tx };
    testCommand(hub, @enumFromInt(10), .configure, @bitCast(configuration));
    testCommand(hub, @enumFromInt(10), .write_x, 7);
    testCommand(hub, @enumFromInt(10), .write_y, 'A');
    _ = hub.step();
    try std.testing.expectEqualStrings("A", bytes.written());
    for (0..4) |_| {
        _ = hub.step();
        try std.testing.expect(hub.io.frame_inputs[10].new_x == null);
        try std.testing.expect(hub.io.frame_inputs[10].new_y == null);
        try std.testing.expect(!hub.io.frame_inputs[10].ack);
    }
    try std.testing.expectEqualStrings("A", bytes.written());
    try std.testing.expect(hub.io.pins[10].smart.registers.ready);
    testCommand(hub, @enumFromInt(10), .acknowledge, 0);
    _ = hub.step();
    try std.testing.expect(!hub.io.frame_inputs[10].ack);
    try std.testing.expect(!hub.io.pins[10].smart.registers.ready);
}

test "WRPIN owns output routing and enable while smart outputs remain independent" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    // Pin 0 drives high, so the odd pin's OTHER input is low.
    hub.cogs[0].write_reg(.DIRA, 1);
    hub.cogs[0].write_reg(.OUTA, 1);
    for (0..5) |_| {
        _ = hub.step();
    }
    try std.testing.expectEqual(Signal.high, hub.io.pads[0]);
    const pin = &hub.io.pins[1];
    var reader = std.Io.Reader.fixed("x");
    var source: DataSource = .{ .reader = &reader };
    pin.smart.rx_mode = .{ .virtual = &source };
    testCommand(hub, @enumFromInt(1), .configure, 0x7c); // UART TX, TT enables output with DIR low.
    try std.testing.expect(!pin.smart.registers.enabled);
    // WRPIN changes the mux without filling its output pipeline with UART's idle level.
    for (0..2) |_| {
        _ = hub.step();
    }
    try std.testing.expectEqual(Signal.high, pin.outputs.smart_out);
    try std.testing.expectEqual(Signal.high, hub.io.pads[1]);
    hub.cogs[0].write_reg(.DIRA, 3);
    for (0..9) |_| {
        _ = hub.step();
    }
    try std.testing.expectEqual(@as(usize, 1), reader.bufferedLen());
    testCommand(hub, @enumFromInt(1), .configure, 0xfc); // SMART overrides OTHER in UART TX.
    try std.testing.expectEqual(Signal.high, hub.io.pads[1]);
    testCommand(hub, @enumFromInt(1), .configure, 0xbc); // TT disables output despite DIR high.
    try std.testing.expect(pin.smart.registers.enabled);
    try std.testing.expectEqual(Signal.high, pin.outputs.smart_out);
    try std.testing.expectEqual(Signal.high, hub.io.pads[1]);
    try std.testing.expect(!hub.io.outputEnabled(@enumFromInt(1)));
    // RX leaves the output mux to OTHER, independent of the receiver's DIR reset.
    pin.smart.rx_mode = .exact;
    hub.cogs[0].write_reg(.DIRA, 1);
    testCommand(hub, @enumFromInt(1), .configure, 0xfa);
    try std.testing.expect(!pin.smart.registers.enabled);
    for (0..2) |_| {
        _ = hub.step();
    }
    try std.testing.expectEqual(Signal.low, hub.io.pads[1]);
    testCommand(hub, @enumFromInt(1), .configure, 0xc0); // Normal mode uses DIR regardless of TT.
    for (0..2) |_| {
        _ = hub.step();
    }
    try std.testing.expectEqual(Signal.low, hub.io.pads[1]);
    hub.cogs[0].write_reg(.DIRA, 3);
    for (0..5) |_| {
        _ = hub.step();
    }
    try std.testing.expectEqual(Signal.low, hub.io.pads[1]);
}

test "unanimous histories reject glitches and register agreement before publishing" {
    const lengths = [_]u4{ 2, 3, 5, 8 };
    for (lengths) |length| {
        const mask: u8 = @intCast((@as(u16, 1) << length) - 1);
        var filter: InputFilter = .{};
        for (0..length - 1) |_| {
            try std.testing.expectEqual(Signal.low, filter.step(.high, mask, true));
        }
        _ = filter.step(.low, mask, true);
        try std.testing.expect(!filter.output_pipeline.tap(0));
        // History comparison and output sampling use the preceding edge's values.
        for (0..length) |_| {
            try std.testing.expectEqual(Signal.low, filter.step(.high, mask, true));
        }
        try std.testing.expectEqual(Signal.low, filter.step(.low, mask, false));
        try std.testing.expectEqual(Signal.low, filter.step(.low, mask, false));
        try std.testing.expectEqual(Signal.high, filter.step(.low, mask, false));
        for (0..length - 1) |_| {
            try std.testing.expectEqual(Signal.high, filter.step(.low, mask, true));
        }
        try std.testing.expectEqual(Signal.high, filter.step(.low, mask, true));
        try std.testing.expectEqual(Signal.high, filter.step(.low, mask, false));
        try std.testing.expectEqual(Signal.high, filter.step(.low, mask, false));
        try std.testing.expectEqual(Signal.low, filter.step(.low, mask, false));
        try std.testing.expectEqual(Signal.low, filter.step(.low, mask, true));
    }
}

test "all global filters independently sample selected inverted A and B on both CT edges" {
    var io: IO = .{};
    for (0..4) |index| {
        io.configureFilter(0x4000_0000 | (@as(u32, @intCast(index)) << 7) | (3 << 5) | 2);
        io.pins[index].configure(.{
            .input_logic = @enumFromInt(@intFromEnum(PinConfiguration.InputLogic.filter0) + index),
            .a = .{ .source = .plus1, .invert = false },
            .b = .{ .source = .plus2, .invert = true },
        });
    }
    // Every selected A is high and each inverted B is low.
    io.receiver_inputs = @splat(.high);
    for (0..31) |clock| {
        io.last_clock = clock;
        io.routeInputs(&Hub.SystemConfiguration.ideal);
        for (io.pins[0..4]) |pin| {
            try std.testing.expectEqual(@as(u8, 0), pin.filter_b.history);
        }
    }
    for (io.pins[0..4]) |pin| {
        try std.testing.expectEqual(@as(u8, 0xff), pin.filter_a.history);
        try std.testing.expect(!pin.filter_a.output_pipeline.tap(0));
    }
    for (31..34) |clock| {
        io.last_clock = clock;
        io.routeInputs(&Hub.SystemConfiguration.ideal);
    }
    for (0..4) |i| {
        try std.testing.expectEqual(Signal.high, io.smart_a[i]);
        try std.testing.expectEqual(Signal.low, io.smart_b[i]);
    }
    // Reconfiguration must use the retained history even without a new sample.
    io.configureFilter(0x4000_001f);
    io.pins[0].filter_a.history = 3;
    io.last_clock = 34;
    io.routeInputs(&Hub.SystemConfiguration.ideal);
    try std.testing.expectEqual(Signal.high, io.smart_a[0]);
    // The sampled CT tap is half a period ahead: test both transitions and rollover.
    io.pins[0].filter_a.history = 0;
    io.input_clock = (@as(u64, 1) << 30) - 1;
    io.last_clock = @as(u64, 1) << 30;
    io.routeInputs(&Hub.SystemConfiguration.ideal);
    try std.testing.expectEqual(@as(u8, 1), io.pins[0].filter_a.history);
    io.input_clock = (@as(u64, 3) << 30) - 1;
    io.last_clock = @as(u64, 3) << 30;
    io.routeInputs(&Hub.SystemConfiguration.ideal);
    try std.testing.expectEqual(@as(u8, 3), io.pins[0].filter_a.history);
    io.input_clock = std.math.maxInt(u64);
    io.last_clock = 0;
    io.routeInputs(&Hub.SystemConfiguration.ideal);
    try std.testing.expectEqual(@as(u8, 3), io.pins[0].filter_a.history);
}

test "adjacent pad input precedes inversion and selectors even with output disabled" {
    var io: IO = .{};
    io.receiver_inputs[0] = .high;
    io.receiver_inputs[1] = .low;
    io.sync_in = io.receiver_inputs;
    for ([_]PinConfiguration.PadMode{ .logic_adjacent_feedback, .schmitt_adjacent_feedback }) |mode| {
        for ([_]bool{ false, true }) |synchronous| {
            for ([_]bool{ false, true }) |invert| {
                io.pins[0].configure(.{ .pad_mode = mode, .synchronous = synchronous, .invert_input = invert });
                io.pins[1].configure(.{ .pad_mode = mode, .synchronous = synchronous, .invert_input = invert });
                io.pins[63].configure(.{ .a = .{ .source = .plus1, .invert = false } });
                io.receivers[0].step(io.pins[0].configuration, &Hub.SystemConfiguration.ideal, 3.3, 0, 50e-9);
                io.receivers[1].step(io.pins[1].configuration, &Hub.SystemConfiguration.ideal, 0, 3.3, 50e-9);
                io.receiver_inputs[0] = io.receivers[0].output;
                io.receiver_inputs[1] = io.receivers[1].output;
                io.sync_in = io.receiver_inputs;
                io.routeInputs(&Hub.SystemConfiguration.ideal);
                try std.testing.expectEqual(Signal.from(invert), io.smart_a[0]);
                try std.testing.expectEqual(Signal.from(!invert), io.smart_a[1]);
                try std.testing.expectEqual(io.smart_a[0], io.smart_a[63]);
                try std.testing.expect(!io.outputEnabled(@enumFromInt(0)));
            }
        }
    }
}

test "Boolean routing changes A only and preserves B" {
    var io: IO = .{};
    for ([_]Signal{ .low, .high }) |a| {
        for ([_]Signal{ .low, .high }) |b| {
            io.receiver_inputs[0] = a;
            io.receiver_inputs[1] = b;
            inline for ([_]PinConfiguration.InputLogic{ .a, .a_and_b, .a_or_b, .a_xor_b }) |logic| {
                io.pins[0].configure(.{ .input_logic = logic, .b = .{ .source = .plus1, .invert = false } });
                io.routeInputs(&Hub.SystemConfiguration.ideal);
                try std.testing.expectEqual(Signal.logic(a, b, @enumFromInt(@intFromEnum(logic))), io.smart_a[0]);
                try std.testing.expectEqual(b, io.smart_b[0]);
            }
        }
    }
}

test "GPIO and smart modes receive filtered A and B through the common frame routing" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    for ([_]smart_pin.Mode{ .gpio, .dac_noise }) |mode| {
        try hub.init(.ideal);
        try std.testing.expectEqual(@as(u5, 0), hub.io.filters[0].tap);
        try std.testing.expectEqual(@as(u5, 5), hub.io.filters[1].tap);
        try std.testing.expectEqual(@as(u5, 19), hub.io.filters[2].tap);
        try std.testing.expectEqual(@as(u5, 22), hub.io.filters[3].tap);
        try hub.io.setExternal(@enumFromInt(0), .high);
        try hub.io.setExternal(@enumFromInt(1), .low);
        hub.io.pins[2].configure(.{
            .mode = mode,
            .input_logic = .filter0,
            .a = .{ .source = .minus2, .invert = false },
            .b = .{ .source = .minus1, .invert = false },
        });
        hub.cogs[0].write_reg(.DIRA, 4);
        for (0..20) |_| {
            _ = hub.step();
        }
        const inputs = hub.io.pinInputs(@enumFromInt(2));
        try std.testing.expectEqual(Signal.high, inputs.a);
        try std.testing.expectEqual(Signal.low, inputs.b);
        // GPIO publishes A; a smart mode publishes its own completion flag instead.
        try std.testing.expectEqual(if (mode == .gpio) @as(u64, 4) else 0, hub.io.get_in() & 4);
    }
}

test "switching to virtual TX preserves the ordinary output propagation delay" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    const pin = &hub.io.pins[4];
    testCommand(hub, @enumFromInt(4), .configure, 0x7c);
    testCommand(hub, @enumFromInt(4), .write_x, (8 << 16) | 7);
    hub.cogs[0].write_reg(.DIRA, 1 << 4);
    testCommand(hub, @enumFromInt(4), .write_y, 0);
    for (0..5) |_| {
        _ = hub.step();
    }
    try std.testing.expectEqual(Signal.low, hub.io.pads[4]);
    const discard: DataSink = .discard;
    pin.smart.tx_mode = .{ .virtual = &discard };
    for (0..2) |clock| {
        _ = hub.step();
        try std.testing.expectEqual(Signal.low, hub.io.pads[4]);
        try std.testing.expectEqual(if (clock == 0) Signal.low else Signal.high, pin.outputs.smart_out);
    }
    for (0..20) |_| {
        _ = hub.step();
        try std.testing.expectEqual(Signal.high, hub.io.pads[4]);
        try std.testing.expectEqual(Signal.high, pin.outputs.smart_out);
    }
}

test "raw ADC retains GPIO drive enable inversion and float legs without a synchronization alias" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    for (0..8) |source| {
        for ([_]bool{ false, true }) |inverted| {
            for ([_]PinConfiguration.Drive{ .fast, .resistor_1k5, .float }) |high_drive| {
                for ([_]PinConfiguration.Drive{ .fast, .resistor_1k5, .float }) |low_drive| {
                    try hub.init(.ideal);
                    const mode = @as(u32, 0x0010_0000) | (@as(u32, @intCast(source)) << 15);
                    var adc: PinConfiguration = @bitCast(mode);
                    adc.invert_output = inverted;
                    adc.high_drive = high_drive;
                    adc.low_drive = low_drive;
                    hub.io.editConfiguration(@enumFromInt(0), @bitCast(adc));
                    // Compare each clock with a GPIO output using the same physical driver settings.
                    var gpio = adc;
                    gpio.pad_mode = .logic;
                    gpio.synchronous = false;
                    gpio.invert_input = false;
                    hub.io.editConfiguration(@enumFromInt(1), @bitCast(gpio));
                    try hub.io.setAdcOverride(@enumFromInt(0), 1);
                    hub.cogs[0].write_reg(.DIRA, 3);
                    for ([_]bool{ false, true, false }) |out| {
                        hub.cogs[0].write_reg(.OUTA, if (out)
                            3
                        else
                            0);
                        for (0..6) |_| {
                            _ = hub.step();
                            try std.testing.expectEqual(hub.io.pads[1], hub.io.pads[0]);
                        }
                        const level = out != inverted;
                        const drive_mode = if (level)
                            high_drive
                        else
                            low_drive;
                        if (drive_mode == .float) {
                            try std.testing.expectEqual(
                                @as(f64, 0),
                                hub.io.getExternal(@enumFromInt(0)).?.internal_current,
                            );
                        } else {
                            try std.testing.expectEqual(Signal.from(level), hub.io.pads[0]);
                        }
                        // The host-supplied ADC stream stays independent of the driven pad level.
                        try std.testing.expect(hub.io.get_in() & 1 != 0);
                    }
                    const retained = hub.io.pads[0];
                    hub.cogs[0].write_reg(.DIRA, 0);
                    for (0..6) |_| {
                        _ = hub.step();
                    }
                    try std.testing.expectEqual(retained, hub.io.pads[0]);
                    try std.testing.expect(!hub.io.outputEnabled(@enumFromInt(0)));
                }
            }
        }
    }
}

test "DAC pad modes gate the independent raw ADC stream with OUT and TT instead of DIR" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    for ([_]u32{ 0x0014_0000, 0x0015_0000, 0x0016_0000, 0x0017_0000 }) |mode| {
        for (0..4) |tt| {
            for ([_]bool{ false, true }) |dir| {
                try hub.init(.ideal);
                hub.io.editConfiguration(@enumFromInt(0), mode | 0x8800 | (@as(u32, @intCast(tt)) << 6));
                try hub.io.setAdcOverride(@enumFromInt(0), 0.25);
                hub.cogs[0].write_reg(.DIRA, @intFromBool(dir));
                for ([_]bool{ false, true, false }) |out| {
                    hub.cogs[0].write_reg(.OUTA, @intFromBool(out));
                    for (0..6) |_| {
                        _ = hub.step();
                    }
                    var count: u32 = 0;
                    for (0..16) |_| {
                        _ = hub.step();
                        count += @intFromBool(hub.io.get_in() & 1 != 0);
                    }
                    const expected: u32 = if (tt < 2 and out)
                        4
                    else
                        0;
                    try std.testing.expectEqual(expected, count);
                    const expected_pad = dir and tt != 1 and (mode == 0x140000 or mode == 0x160000);
                    try std.testing.expectEqual(Signal.from(expected_pad), hub.io.pads[0]);
                    try std.testing.expectEqualSlices(u8, &.{ 0, 0, 0, 0 }, &hub.cogs[0].dacs);
                }
            }
        }
    }
}

test "raw ADC densities emit bounded digital streams and reject nonfinite inputs" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    const io = &hub.io;
    const configurations = [_]u32{ 0x0010_0000, 0x0010_8000, 0x0011_0000, 0x0011_8000, 0x0012_0000 };
    for ([_]f64{ 0, 0.125, 0.5, 0.875, 1 }, 0..) |density, i| {
        io.editConfiguration(@enumFromInt(i), configurations[i]);
        try io.setAdcOverride(@enumFromInt(i), density);
    }
    var counts: [5]usize = @splat(0);
    for (0..1024) |_| {
        _ = hub.step();
        for (&counts, 0..) |*count, i| {
            count.* += @intFromBool(io.routed_inputs & (@as(u64, 1) << @intCast(i)) != 0);
        }
    }
    try std.testing.expectEqualDeep([5]usize{ 0, 128, 512, 896, 1024 }, counts);
    for ([_]f64{ -0.01, 1.01, std.math.nan(f64), std.math.inf(f64) }) |density| {
        try std.testing.expectError(error.InvalidDensity, io.setAdcOverride(@enumFromInt(0), density));
    }
    try std.testing.expectEqual(@as(f64, 0), io.adc_inputs[0].density);
    try hub.init(.ideal);
    try std.testing.expectEqualDeep(DeltaSigma{}, hub.io.adc_inputs[0]);
}

test "analog oversampling converges toward RC behavior while clocked feedback remains synchronous" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    const continuous = 3.3 * (1 - @exp(@as(f64, -0.5)));
    var old_error: f64 = std.math.inf(f64);
    for ([_]u32{ 1, 3, 8 }) |count| {
        var configuration: Hub.SystemConfiguration = .ideal;
        configuration.oversample = count;
        configuration.clocks.rcfast.frequency = 20e6;
        try hub.init(configuration);
        try hub.io.setExternal(@enumFromInt(0), .{ .voltage = 3.3, .resistance = 1_000, .capacitance = 95e-12 });
        var expected = hub.io.analog_pads;
        for (0..count) |_| {
            expected.step(&configuration, 50e-9 / @as(f64, @floatFromInt(count)));
        }
        try std.testing.expectEqual(Hub.StepResult.normal, hub.step());
        try std.testing.expectEqual(@as(u64, 1), hub.counter);
        try std.testing.expectEqualDeep(expected, hub.io.analog_pads);
        const error_now = @abs(continuous - hub.io.getExternal(@enumFromInt(0)).?.voltage);
        try std.testing.expect(error_now < old_error);
        old_error = error_now;

        try hub.init(configuration);
        try hub.io.setExternal(@enumFromInt(0), .high);
        _ = hub.step();
        try hub.io.setExternal(@enumFromInt(0), .float);
        hub.io.editConfiguration(@enumFromInt(0), 0x50000);
        hub.io.editConfiguration(@enumFromInt(1), 0x50000);
        hub.cogs[0].write_reg(.DIRA, 3);
        for (0..3) |_| {
            _ = hub.step();
            try std.testing.expectEqual(Signal.high, hub.io.pads[0]);
            try std.testing.expectEqual(Signal.low, hub.io.pads[1]);
        }
        _ = hub.step();
        try std.testing.expectEqual(Signal.low, hub.io.pads[0]);
        try std.testing.expectEqual(Signal.high, hub.io.pads[1]);
        _ = hub.step();
        try std.testing.expectEqual(Signal.high, hub.io.pads[0]);
        try std.testing.expectEqual(Signal.low, hub.io.pads[1]);
    }
}

test "DAC loading and same-pad ADC conversion share the solved physical voltage" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    for (0..4) |range| {
        for ([_]u8{ 0, 32, 128, 255 }) |code| {
            try hub.init(.ideal);
            const configuration: PinConfiguration = @bitCast(@as(u32, 0x140000) |
                (@as(u32, @intCast(range)) << 16) | (@as(u32, code) << 8));
            hub.io.editConfiguration(@enumFromInt(0), @bitCast(configuration));
            try hub.io.setExternal(@enumFromInt(0), .{ .voltage = 0, .resistance = 1500 });
            hub.cogs[0].write_reg(.DIRA, 1);
            hub.cogs[0].write_reg(.OUTA, 1);
            for (0..32) |_| {
                _ = hub.step();
            }
            const source = Pad.dacDriver(&hub.system_configuration, configuration.dacRange(), code).linear;
            const expected = source.voltage * 1500 / (1500 + source.resistance);
            const snapshot = hub.io.getExternal(@enumFromInt(0)).?;
            try std.testing.expectApproxEqAbs(expected, snapshot.voltage, 1e-10);
            try std.testing.expectApproxEqAbs(
                Receiver.adcDensity(configuration, &hub.system_configuration, expected),
                hub.io.adc_inputs[0].density,
                1e-10,
            );
            try std.testing.expectEqual(source.resistance, hub.io.resolvedDriver(@enumFromInt(0), &hub.system_configuration).linear.resistance);
            try hub.io.setAdcOverride(@enumFromInt(0), 0.125);
            _ = hub.step();
            try std.testing.expectEqual(@as(f64, 0.125), hub.io.adc_inputs[0].density);
            try hub.io.setAdcOverride(@enumFromInt(0), null);
            _ = hub.step();
            try std.testing.expectApproxEqAbs(Receiver.adcDensity(configuration, &hub.system_configuration, expected), hub.io.adc_inputs[0].density, 1e-10);
        }
    }
}

test "asynchronous level feedback resolves within substeps and converges around its reference" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    var coarse_span: f64 = 0;
    for ([_]u32{ 1, 8, 32, 128 }) |factor| {
        var configuration = Hub.SystemConfiguration.realistic;
        configuration.oversample = factor;
        try hub.init(configuration);
        try hub.io.setExternal(@enumFromInt(0), .{ .voltage = 0, .resistance = 1, .drive_enabled = false, .capacitance = 100e-12 });
        hub.io.editConfiguration(@enumFromInt(0), 0x1a8000);
        hub.cogs[0].write_reg(.DIRA, 1);
        for (0..256) |_| {
            _ = hub.step();
        }
        var minimum: f64 = configuration.vio_voltage;
        var maximum: f64 = 0;
        var sum: f64 = 0;
        for (0..256) |_| {
            _ = hub.step();
            const voltage = hub.io.getExternal(@enumFromInt(0)).?.voltage;
            minimum = @min(minimum, voltage);
            maximum = @max(maximum, voltage);
            sum += voltage;
        }
        const span = maximum - minimum;
        try std.testing.expect(minimum < 1.65 and maximum > 1.65);
        try std.testing.expectApproxEqAbs(@as(f64, 1.65), sum / 256, 0.08);
        if (factor == 1) {
            coarse_span = span;
        } else {
            // Endpoint sampling aliases the oscillation; its span need not decrease at every factor.
            try std.testing.expect(span < coarse_span);
            try std.testing.expect(span < 0.25);
        }
    }
}

test "USB idle pulls, active fast drives and sniffer gating resolve through both electrical profiles" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    for ([_]Hub.SystemConfiguration{ .ideal, .realistic }) |system| {
        for ([_]bool{ false, true }) |host| {
            for ([_]bool{ false, true }) |full_speed| {
                try hub.init(system);
                for (8..10) |i| {
                    hub.io.editConfiguration(@enumFromInt(i), @bitCast(PinConfiguration{
                        .mode = .usb,
                        .output_enable = true,
                        .high_drive = .float,
                        .low_drive = .float,
                    }));
                    const load = if (host)
                        system.vio_voltage
                    else
                        0;
                    try hub.io.setExternal(@enumFromInt(i), .{ .voltage = load, .resistance = 1500 });
                }
                const x: u32 = 4096 | (@as(u32, @intFromBool(host)) << 15) |
                    (@as(u32, @intFromBool(full_speed)) << 14);
                hub.io.editSmartRegister(@enumFromInt(8), .x, x);
                // Lower DIR enables both physical drives; upper DIR may remain clear.
                hub.cogs[0].write_reg(.DIRA, 1 << 8);
                for (0..256) |_| {
                    _ = hub.step();
                }
                for (8..10) |i| {
                    const pullup = (i == 9) == full_speed;
                    const expected = if (host)
                        system.vio_voltage * system.resistor_15k / (1500 + system.resistor_15k)
                    else if (pullup)
                        system.vio_voltage * 1500 / (1500 + system.resistor_1k5)
                    else
                        0;
                    try std.testing.expectApproxEqAbs(expected, hub.io.getExternal(@enumFromInt(i)).?.voltage, 0.003);
                }
                hub.io.editSmartRegister(@enumFromInt(8), .y, 2);
                for (0..64) |_| {
                    _ = hub.step();
                }
                for (8..10) |i| {
                    const high = (i == 9) != full_speed;
                    const voltage = hub.io.getExternal(@enumFromInt(i)).?.voltage;
                    const driven = if (high)
                        voltage > 3.2
                    else
                        voltage < 0.1;
                    try std.testing.expect(driven);
                    // TT disables each pad independently while the serializer keeps running.
                    var configuration = hub.io.pins[i].configuration;
                    configuration.output_enable = false;
                    hub.io.editConfiguration(@enumFromInt(i), @bitCast(configuration));
                }
                for (0..64) |_| {
                    _ = hub.step();
                }
                for (8..10) |i| {
                    const driver_state = hub.io.resolvedDriver(@enumFromInt(i), &system);
                    try std.testing.expect(driver_state == .linear and !driver_state.linear.drive_enabled);
                }
            }
        }
    }
}
