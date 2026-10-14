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
    /// CT-aligned silicon traces are retained in data/timing-data/digital-inputs.
    fn samples(config: FilterConfiguration, clock: u64, previous: u64) bool {
        const phase = if (config.tap == 0) @as(u64, 0) else @as(u64, 1) << (config.tap - 1);
        const changed = (clock +% phase) ^ (previous +% phase);
        return changed & (@as(u64, 1) << config.tap) != 0;
    }
};

/// Each pin retains separate A/B histories and outputs across configuration and DIR changes.
const InputFilter = struct {
    history: u8 = 0,
    output: bool = false,
    sampled_output: bool = false,

    /// Clock unanimity and its two output stages from the old history, then shift a CT-tap sample.
    fn step(filter: *InputFilter, input: Signal, mask: u8, sample: bool) Signal {
        const output = filter.sampled_output;
        filter.sampled_output = filter.output;
        const history = filter.history & mask;
        if (history == 0 or history == mask) {
            filter.output = history != 0;
        }
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
    output_pipeline: [3]Signal = @splat(.zero),
    /// Selected OUT/OTHER/SmartOut after the common propagation delay.
    output: Signal = .zero,
    /// Host architectural edits deliver their engine strobes on the next complete hub edge.
    edits: struct { x: ?u32 = null, y: ?u32 = null } = .{},
    filter_a: InputFilter = .{},
    filter_b: InputFilter = .{},

    const OutputSource = enum { other, smart };

    /// Apply WRPIN routing and select the smart engine, preserving same-mode digital state.
    pub fn configure(pin: *Pin, configuration: PinConfiguration) void {
        pin.configuration = configuration;
        pin.smart.configure(engineMode(configuration));
    }

    /// Decode the silicon's SMART override and OUT/OTHER selector from WRPIN.
    /// See docs/p2docs.github.io/page/mirror/p2silicon.md, "pin DIR/OUT control".
    fn outputSource(pin: *const Pin) OutputSource {
        return if (pin.smart.overridesOutput() or pin.configuration.output_source == .out) .smart else .other;
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
    pads: [4][64]Signal = @splat(@splat(.z)),
    /// OUT selectors cross the same two input sampling stages as physical pad reads.
    out: [4]u64 = @splat(0),
    directions: [4]u64 = @splat(0),
    inputs: [4]u64 = @splat(0),
    results: [8][64]ReadResult = @splat(@splat(.{})),
    input_registers: u64 = 0,
    active: bool = false,
};

pins: [64]Pin = @splat(.{}),
/// Reused frame storage keeps ReleaseSafe from poisoning large scratch arrays every clock.
frame_inputs: [64]smart_pin.Inputs = @splat(.{}),
frame_outputs: [64]smart_pin.Outputs = @splat(.{}),
filters: [4]FilterConfiguration = .{
    .{ .length = .two, .tap = 0 },
    .{ .length = .three, .tap = 5 },
    .{ .length = .five, .tap = 19 },
    .{ .length = .eight, .tap = 22 },
},
smart_a: [64]Signal = @splat(.z),
smart_b: [64]Signal = @splat(.z),
pads: [64]Signal = @splat(.z),
external: [64]Signal = @splat(.z),
pad_reads: [64]Signal = @splat(.z),
sync_out: [64]Signal = @splat(.zero),
sync_output_pipeline: [3][64]Signal = @splat(@splat(.zero)),
sync_in: [64]Signal = @splat(.z),
directions: u64 = 0,
physical_directions: u64 = 0,
direction_pipeline: DirectionPipeline = .{},
samples: Samples = .{},
routed_inputs: u64 = 0,
outputs: u64 = 0,
drives: u64 = 0,
dirty: bool = true,
clock_mode: u32 = 0,
clock_frequency: u64 = 20_000_000,
crystal_frequency: u64 = 20_000_000,
/// Current shared output, latched by step; physical pad tap numbering is approximate.
random_bits: u64 = 0,
last_clock: u64 = 0,
/// Last input-routing edge, including the initial CT=0 reset state.
input_clock: u64 = 0,
commands: CommandPipeline = .{},
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
    if (value & 1 != 0)
        return true; // WRPIN #1 / AKPIN acknowledges only.
    const configuration: PinConfiguration = @bitCast(value);
    return @intFromEnum(configuration.pad_mode) <= @intFromEnum(PinConfiguration.PadMode.schmitt_adjacent_feedback);
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
    const config: types.ClockMode = @bitCast(mode);
    io.clock_frequency = config.get_frequency(io.crystal_frequency) catch return false;
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
            if (!supported(command.value))
                return error.UnsupportedPinConfiguration;
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
    io.random_bits = hub.random.output;
    io.updateDirections(hub);
    if (!io.samples.active) {
        io.samples.pads = @splat(io.pads);
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
        io.dirty = true;
    }
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
    io.routeInputs();
    for (inputs, io.smart_a, io.smart_b) |*sample, a, b| {
        sample.a = a;
        sample.b = b;
    }
    // Keep all outputs from the previous frame visible until every pin has advanced.
    const next = &io.frame_outputs;
    for (&io.pins, inputs, next, 0..) |*pin, sample, *state, i| {
        state.* = pin.smart.step(sample) catch |err| {
            io.fail(hub, @enumFromInt(i), err);
            return;
        };
    }
    io.routed_inputs = 0;
    var dynamic_pads = false;
    for (&io.pins, next, 0..) |*pin, next_state, i| {
        const state = next_state;
        const before = pin.outputs;
        const configuration = pin.configuration;
        io.routed_inputs |= @as(u64, state.in) << @intCast(i);
        if (state.smart_out != before.smart_out) {
            io.dirty = true;
        }
        if (configuration.synchronous or configuration.output_source == .other or configuration.pad_mode != .logic) {
            dynamic_pads = true;
        }
        pin.outputs = state;
    }
    // Delay the selected output in every mode, including GPIO and WRPIN mux changes.
    for (&io.pins, 0..) |*pin, i| {
        const mux_signal = io.selectedOutput(@enumFromInt(i));
        const output = pin.output_pipeline[if (io.clock_frequency >= 170_000_000) 1 else 2];
        pin.output_pipeline[0] = pin.output_pipeline[1];
        pin.output_pipeline[1] = pin.output_pipeline[2];
        pin.output_pipeline[2] = mux_signal;
        if (pin.output != output) {
            pin.output = output;
            io.dirty = true;
        }
    }
    for (&io.sync_out, 0..) |*state, i| {
        if (!io.pins[i].configuration.synchronous)
            continue;
        state.* = io.sync_output_pipeline[if (io.clock_frequency >= 170_000_000) 1 else 0][i];
        io.sync_output_pipeline[0][i] = io.sync_output_pipeline[1][i];
        io.sync_output_pipeline[1][i] = io.sync_output_pipeline[2][i];
        io.sync_output_pipeline[2][i] = io.outputState(@enumFromInt(i));
    }
    if (io.dirty or dynamic_pads) {
        io.resolvePads();
    }
    io.samples.pads[hub.counter % io.samples.pads.len] = io.pads;
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
        .result = if (err == error.UnsupportedSmartPinMode)
            .not_implemented
        else
            .{ .illegal = "smart-pin I/O failure (see pin diagnostic)" },
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
        drives |= dir & out;
    }
    if (directions != io.directions or outputs != io.outputs or drives != io.drives) {
        io.directions = directions;
        io.outputs = outputs;
        io.drives = drives;
        // DIR independently reaches the output gate in three/four clocks after writeback.
        // ponytail: frequency-only propagation; add an electrical timing model for intermediate-band fidelity.
        const delay: usize = if (io.clock_frequency >= 170_000_000) 4 else 3;
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
    io.dirty = true;
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
    };
}

/// Obtain the neighboring engine's output for the physical OTHER selector.
fn smartOutput(io: *const IO, index: PinIndex) Signal {
    return io.pins[@intFromEnum(index)].outputs.smart_out;
}

/// Select the current OUT/OTHER/SmartOut signal before the common output pipeline.
fn selectedOutput(io: *const IO, index: PadIndex) Signal {
    const i = @intFromEnum(index);
    const pin = &io.pins[i];
    return switch (pin.outputSource()) {
        .smart => pin.outputs.smart_out,
        .other => if (i & 1 != 0)
            io.smartOutput(@enumFromInt(i ^ 1)).invert()
        else
            Signal.from((io.random_bits >> @intCast(i)) & 1 != 0),
    };
}

/// Resolve feedback selection and output inversion after registered output selection.
fn outputState(io: *const IO, index: PadIndex) Signal {
    const i = @intFromEnum(index);
    const configuration = io.pins[i].configuration;
    var state = io.pins[i].output;
    switch (configuration.pad_mode) {
        .logic_feedback, .schmitt_feedback => state = io.pads[i],
        .logic_adjacent_feedback, .schmitt_adjacent_feedback => state = io.pads[i ^ 1],
        else => {},
    }
    if (configuration.invert_output) {
        state = state.invert();
    }
    return state;
}

/// Apply WRPIN output enable and the physical high/low drive configuration.
fn drive(io: *const IO, index: PadIndex) Signal {
    const i = @intFromEnum(index);
    const configuration = io.pins[i].configuration;
    if (!io.outputEnabled(index))
        return .z;
    const state = if (configuration.synchronous)
        io.sync_out[i]
    else
        io.outputState(index);
    const high_float = configuration.high_drive == .float;
    const low_float = configuration.low_drive == .float;
    return switch (state) {
        .one => if (high_float)
            .z
        else
            .one,
        .zero => if (low_float)
            .z
        else
            .zero,
        else => if (high_float and low_float)
            .z
        else
            .x,
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
    const out = if (io.samples.active)
        io.samples.out[(io.last_clock -% 2) % io.samples.out.len]
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

/// Settle the physical drive network from published outputs without advancing smart logic.
fn resolvePads(io: *IO) void {
    io.dirty = false;
    const previous = io.pads;
    // Settle digital feedback together. Oscillating networks have unknown pad levels.
    var settled = false;
    for (0..65) |_| {
        var next: [64]Signal = undefined;
        for (&next, 0..) |*state, i| {
            state.* = Signal.resolve(io.drive(@enumFromInt(i)), io.external[i]);
        }
        if (std.mem.eql(Signal, &next, &io.pads)) {
            settled = true;
            break;
        }
        io.pads = next;
    }
    if (!settled)
        for (&io.pads, 0..) |*state, i| {
            switch (io.pins[i].configuration.pad_mode) {
                .logic_feedback, .logic_adjacent_feedback, .schmitt_feedback, .schmitt_adjacent_feedback => state.* = .x,
                else => {},
            }
        };
    for (0..64) |i| {
        const internal = io.drive(@enumFromInt(i));
        const conflict = (internal == .one and
            io.external[i] == .zero) or
            (internal == .zero and
                io.external[i] == .one);
        if (conflict and previous[i] != .x) {
            std.log.warn("digital contention on pin {d} at clock {d}", .{ i, io.last_clock });
        }
    }
    io.sync_in = io.pads;
}

/// Compute SmartA/SmartB from the previous pad samples without advancing smart logic.
fn routeInputs(io: *IO) void {
    const previous_clock = io.input_clock;
    io.input_clock = io.last_clock;
    const pads = if (io.samples.active)
        io.samples.pads[(io.last_clock -% 2) % io.samples.pads.len]
    else
        io.pads;
    for (&io.pad_reads, 0..) |*state, i| {
        const configuration = io.pins[i].configuration;
        const source = switch (configuration.pad_mode) {
            .logic_adjacent_feedback, .schmitt_adjacent_feedback => i ^ 1,
            else => i,
        };
        const input = if (configuration.synchronous)
            io.sync_in[source]
        else
            pads[source];
        state.* = if (configuration.invert_input)
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

/// Selectable sensors observe IN one pipeline stage before the cog-visible TESTP sample.
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
        hub.step();
    }
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
    pin.rx_mode = .{ .emulated = &source };
    testCommand(hub, @enumFromInt(9), .configure, 0x3e);
    testCommand(hub, @enumFromInt(9), .write_x, (8 << 16) | 7); // Eight-clock bits, including UART start and stop timing.
    try std.testing.expectEqual(@as(usize, 2), reader.bufferedLen());
    hub.cogs[0].write_reg(.DIRA, 1 << 9);
    hub.step();
    try std.testing.expectEqual(@as(usize, 1), reader.bufferedLen());
    for (0..78) |_| {
        hub.step();
    }
    try std.testing.expectEqual(@as(u32, 'a') << 24, pin.registers.result);
    try std.testing.expectEqual(@as(usize, 0), reader.bufferedLen());
    // DIR reset discards the in-flight second frame before it becomes readable.
    hub.cogs[0].write_reg(.DIRA, 0);
    hub.step();
    for (0..20) |_| {
        hub.step();
    }
    try std.testing.expect(!pin.registers.ready);
    try std.testing.expectEqual(@as(u32, 'a') << 24, pin.registers.result);
    try std.testing.expect(io.pins[63].smart.rx_mode == .exact);
}

test "hardware repository command timing and OR-combined mixed command buses" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    hub.init();
    hub.cogs[0].write_reg(.DIRB, 1 << 8);
    const pin = &hub.io.pins[40].smart;
    testCommand(hub, @enumFromInt(40), .configure, 2);
    testCommand(hub, @enumFromInt(40), .write_x, 0);
    for (0..6) |_| {
        hub.step();
    }
    hub.counter = 100;
    hub.io.enqueue(hub, .{ .mask = @as(u64, 1) << 40, .kind = .write_x, .value = 0x155, .cog = 0, .pc = 1, .instruction = 0 });
    hub.io.enqueue(hub, .{ .mask = @as(u64, 1) << 40, .kind = .write_x, .value = 0xaa, .cog = 7, .pc = 2, .instruction = 0 });
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
    for (0..5) |_| {
        hub.step();
    }
    try std.testing.expectEqual(@as(u32, 0x1ff), pin.registers.result);
    try std.testing.expectEqual(@as(u32, 2), @as(u32, @bitCast(hub.io.pins[40].configuration)));
}

test "GPIO pad pipeline follows the measured low and high frequency timing bands" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    const TimingCase = struct { frequency: u64, delay: usize };
    const cases = [_]TimingCase{
        .{ .frequency = 20_000_000, .delay = 3 },
        .{ .frequency = 167_000_000, .delay = 3 },
        .{ .frequency = 170_000_000, .delay = 4 },
        .{ .frequency = 200_000_000, .delay = 4 },
    };
    for (cases) |case| {
        hub.init();
        hub.io.clock_frequency = case.frequency;
        hub.cogs[0].registers.set(.DIRA, 1);
        hub.cogs[0].registers.set(.OUTA, 1);
        for (0..case.delay) |_| {
            hub.step();
            try std.testing.expectEqual(Signal.z, hub.io.pads[0]);
        }
        hub.step();
        try std.testing.expectEqual(Signal.one, hub.io.pads[0]);

        hub.cogs[0].registers.set(.OUTA, 0);
        for (0..case.delay) |_| {
            hub.step();
            try std.testing.expectEqual(Signal.one, hub.io.pads[0]);
        }
        hub.step();
        try std.testing.expectEqual(Signal.zero, hub.io.pads[0]);
    }
}

test "GPIO cog OR, A/B routing, float, feedback and contention remain separate from IN" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    hub.init();
    hub.cogs[0].write_reg(.DIRA, 1);
    hub.cogs[1].write_reg(.OUTA, 1);
    for (0..9) |_| {
        hub.step();
    }
    try std.testing.expectEqual(Signal.zero, hub.io.pads[0]);
    hub.cogs[1].write_reg(.DIRA, 1);
    for (0..9) |_| {
        hub.step();
    }
    try std.testing.expectEqual(Signal.one, hub.io.pads[0]);
    try std.testing.expectEqual(@as(u64, 1), hub.io.get_in());
    hub.io.setExternal(@enumFromInt(0), .zero);
    for (0..5) |_| {
        hub.step();
    }
    try std.testing.expectEqual(Signal.x, hub.io.pads[0]);
    try std.testing.expectEqual(@as(u64, 0), hub.io.get_in());
    hub.io.setExternal(@enumFromInt(0), .z);
    hub.cogs[0].write_reg(.DIRA, 0);
    hub.cogs[1].write_reg(.DIRA, 0);
    for (0..9) |_| {
        hub.step();
    }
    try std.testing.expectEqual(Signal.z, hub.io.pads[0]);
    hub.io.setExternal(@enumFromInt(63), .one);
    testCommand(hub, @enumFromInt(0), .configure, 0x70000000); // A = relative -1, wrapping all 64 pins.
    try std.testing.expect(hub.io.smart_a[0].sample());
    testCommand(hub, @enumFromInt(0), .configure, 0xf0000000);
    try std.testing.expect(!hub.io.smart_a[0].sample());
    testCommand(hub, @enumFromInt(0), .configure, 0x3800); // High drive floats.
    hub.cogs[0].write_reg(.OUTA, 1);
    hub.cogs[0].write_reg(.DIRA, 1);
    for (0..5) |_| {
        hub.step();
    }
    try std.testing.expectEqual(Signal.z, hub.io.pads[0]);
    // Adjacent-pin feedback drives pin 0 from externally driven pin 1.
    hub.io.setExternal(@enumFromInt(1), .one);
    testCommand(hub, @enumFromInt(0), .configure, 0x40000);
    try std.testing.expectEqual(Signal.one, hub.io.pads[0]);
    testCommand(hub, @enumFromInt(2), .configure, 2);
    hub.cogs[0].write_reg(.DIRA, 5);
    testCommand(hub, @enumFromInt(2), .write_x, 0);
    for (0..3) |_| {
        hub.step();
    }
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
        hub.io.pins[@intFromEnum(index)].smart.tx_mode = .{ .virtual = &sink };
        testCommand(hub, index, .configure, 0x7c);
        testCommand(hub, index, .write_x, (1 << 16) | 7);
    }
    hub.cogs[0].write_reg(.DIRA, 1 << 4);
    hub.cogs[0].write_reg(.DIRB, 1 << 1);
    hub.io.enqueue(hub, .{ .mask = 1 << 4, .kind = .write_y, .value = 'a', .cog = 0, .pc = 0, .instruction = 0 });
    hub.io.enqueue(hub, .{ .mask = @as(u64, 1) << 33, .kind = .write_y, .value = 'b', .cog = 1, .pc = 0, .instruction = 0 });
    for (0..command_delay + 11) |_| {
        hub.step();
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
        hub.step();
    }
    try std.testing.expectEqual(@as(PinIndex, @enumFromInt(33)), hub.io.failure.?.pin);
    try std.testing.expectEqual(error.TestSinkFailure, hub.io.failure.?.err);
    try std.testing.expectEqual(@as(u3, 6), hub.fault.?.cog);
    try std.testing.expectEqual(@as(u20, 123), hub.fault.?.pc);
}

test "a primed USART exposes its first bit on enable and waits for B clocks" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    hub.init();
    testCommand(hub, @enumFromInt(8), .configure, 56);
    testCommand(hub, @enumFromInt(8), .write_y, 123);
    try std.testing.expect(hub.io.pins[8].smart.logic.sync_tx.buffer != null);
    try std.testing.expect(!hub.io.pins[8].outputs.scheduled);
    hub.cogs[0].write_reg(.DIRA, 1 << 8);
    try std.testing.expect(!hub.io.pins[8].smart.registers.enabled);
    hub.step();
    try std.testing.expect(hub.io.pins[8].smart.logic.sync_tx.buffer == null);
    try std.testing.expect(hub.io.pins[8].smart.logic.sync_tx.shifter != null);
    try std.testing.expect(!hub.io.pins[8].smart.registers.ready);
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
    hub.io.setExternal(@enumFromInt(0), .one);
    try std.testing.expectEqual(@as(usize, 0), device.calls);
    try std.testing.expect(hub.io.pins[0].smart.logic == .gpio);
    for (0..command_delay) |_| {
        hub.step();
        try std.testing.expect(hub.fault == null);
        try std.testing.expectEqual(@as(usize, 0), device.calls);
        // Even ordinary GPIO pins receive the aggregate DIR in their one frame update.
        for (&hub.io.pins) |*pin| {
            try std.testing.expect(pin.smart.registers.enabled);
        }
    }
    hub.step();
    try std.testing.expect(hub.fault == null);
    try std.testing.expectEqual(@as(usize, 64), device.calls);
    for (&hub.io.pins) |*pin| {
        try std.testing.expectEqual(@as(u1, 1), pin.outputs.in);
    }
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

test "reused frame inputs do not replay writes or acknowledgements" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    hub.init();
    var bytes: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer bytes.deinit();

    var sink: DataSink = .{ .writer = &bytes.writer };
    hub.io.pins[10].smart.tx_mode = .{ .virtual = &sink };
    hub.cogs[0].write_reg(.DIRA, 1 << 10);
    const configuration: PinConfiguration = .{ .mode = .sync_tx };
    testCommand(hub, @enumFromInt(10), .configure, @bitCast(configuration));
    testCommand(hub, @enumFromInt(10), .write_x, 7);
    testCommand(hub, @enumFromInt(10), .write_y, 'A');
    hub.step();
    try std.testing.expectEqualStrings("A", bytes.written());
    for (0..4) |_| {
        hub.step();
        try std.testing.expect(hub.io.frame_inputs[10].new_x == null);
        try std.testing.expect(hub.io.frame_inputs[10].new_y == null);
        try std.testing.expect(!hub.io.frame_inputs[10].ack);
    }
    try std.testing.expectEqualStrings("A", bytes.written());
    try std.testing.expect(hub.io.pins[10].smart.registers.ready);
    testCommand(hub, @enumFromInt(10), .acknowledge, 0);
    hub.step();
    try std.testing.expect(!hub.io.frame_inputs[10].ack);
    try std.testing.expect(!hub.io.pins[10].smart.registers.ready);
}

test "WRPIN owns output routing and enable while smart outputs remain independent" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    hub.init();
    // Pin 0 drives high, so the odd pin's OTHER input is low.
    hub.cogs[0].write_reg(.DIRA, 1);
    hub.cogs[0].write_reg(.OUTA, 1);
    for (0..5) |_| {
        hub.step();
    }
    try std.testing.expectEqual(Signal.one, hub.io.pads[0]);
    const pin = &hub.io.pins[1];
    var reader = std.Io.Reader.fixed("x");
    var source: DataSource = .{ .reader = &reader };
    pin.smart.rx_mode = .{ .virtual = &source };
    testCommand(hub, @enumFromInt(1), .configure, 0x7c); // UART TX, TT enables output with DIR low.
    try std.testing.expect(!pin.smart.registers.enabled);
    // WRPIN changes the mux without filling its output pipeline with UART's idle level.
    for (0..2) |_| {
        hub.step();
    }
    try std.testing.expectEqual(Signal.one, pin.outputs.smart_out);
    try std.testing.expectEqual(Signal.one, hub.io.pads[1]);
    hub.cogs[0].write_reg(.DIRA, 3);
    for (0..9) |_| {
        hub.step();
    }
    try std.testing.expectEqual(@as(usize, 1), reader.bufferedLen());
    testCommand(hub, @enumFromInt(1), .configure, 0xfc); // SMART overrides OTHER in UART TX.
    try std.testing.expectEqual(Signal.one, hub.io.pads[1]);
    testCommand(hub, @enumFromInt(1), .configure, 0xbc); // TT disables output despite DIR high.
    try std.testing.expect(pin.smart.registers.enabled);
    try std.testing.expectEqual(Signal.one, pin.outputs.smart_out);
    try std.testing.expectEqual(Signal.z, hub.io.pads[1]);
    // RX leaves the output mux to OTHER, independent of the receiver's DIR reset.
    pin.smart.rx_mode = .exact;
    hub.cogs[0].write_reg(.DIRA, 1);
    testCommand(hub, @enumFromInt(1), .configure, 0xfa);
    try std.testing.expect(!pin.smart.registers.enabled);
    for (0..2) |_| {
        hub.step();
    }
    try std.testing.expectEqual(Signal.zero, hub.io.pads[1]);
    testCommand(hub, @enumFromInt(1), .configure, 0xc0); // Normal mode uses DIR regardless of TT.
    for (0..2) |_| {
        hub.step();
    }
    try std.testing.expectEqual(Signal.z, hub.io.pads[1]);
    hub.cogs[0].write_reg(.DIRA, 3);
    for (0..5) |_| {
        hub.step();
    }
    try std.testing.expectEqual(Signal.zero, hub.io.pads[1]);
}

test "unanimous histories reject glitches and register agreement before publishing" {
    const lengths = [_]u4{ 2, 3, 5, 8 };
    for (lengths) |length| {
        const mask: u8 = @intCast((@as(u16, 1) << length) - 1);
        var filter: InputFilter = .{};
        for (0..length - 1) |_| {
            try std.testing.expectEqual(Signal.zero, filter.step(.one, mask, true));
        }
        _ = filter.step(.zero, mask, true);
        try std.testing.expect(!filter.output);
        // History comparison and output sampling use the preceding edge's values.
        for (0..length) |_| {
            try std.testing.expectEqual(Signal.zero, filter.step(.one, mask, true));
        }
        try std.testing.expectEqual(Signal.zero, filter.step(.zero, mask, false));
        try std.testing.expectEqual(Signal.zero, filter.step(.zero, mask, false));
        try std.testing.expectEqual(Signal.one, filter.step(.zero, mask, false));
        for (0..length - 1) |_| {
            try std.testing.expectEqual(Signal.one, filter.step(.zero, mask, true));
        }
        try std.testing.expectEqual(Signal.one, filter.step(.z, mask, true));
        try std.testing.expectEqual(Signal.one, filter.step(.x, mask, false));
        try std.testing.expectEqual(Signal.one, filter.step(.x, mask, false));
        try std.testing.expectEqual(Signal.zero, filter.step(.x, mask, false));
        try std.testing.expectEqual(Signal.zero, filter.step(.x, mask, true));
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
    io.pads = @splat(.one);
    for (0..31) |clock| {
        io.last_clock = clock;
        io.routeInputs();
        for (io.pins[0..4]) |pin| {
            try std.testing.expectEqual(@as(u8, 0), pin.filter_b.history);
        }
    }
    for (io.pins[0..4]) |pin| {
        try std.testing.expectEqual(@as(u8, 0xff), pin.filter_a.history);
        try std.testing.expect(!pin.filter_a.output);
    }
    for (31..34) |clock| {
        io.last_clock = clock;
        io.routeInputs();
    }
    for (0..4) |i| {
        try std.testing.expectEqual(Signal.one, io.smart_a[i]);
        try std.testing.expectEqual(Signal.zero, io.smart_b[i]);
    }
    // Reconfiguration must use the retained history even without a new sample.
    io.configureFilter(0x4000_001f);
    io.pins[0].filter_a.history = 3;
    io.last_clock = 34;
    io.routeInputs();
    try std.testing.expectEqual(Signal.one, io.smart_a[0]);
    // The sampled CT tap is half a period ahead: test both transitions and rollover.
    io.pins[0].filter_a.history = 0;
    io.input_clock = (@as(u64, 1) << 30) - 1;
    io.last_clock = @as(u64, 1) << 30;
    io.routeInputs();
    try std.testing.expectEqual(@as(u8, 1), io.pins[0].filter_a.history);
    io.input_clock = (@as(u64, 3) << 30) - 1;
    io.last_clock = @as(u64, 3) << 30;
    io.routeInputs();
    try std.testing.expectEqual(@as(u8, 3), io.pins[0].filter_a.history);
    io.input_clock = std.math.maxInt(u64);
    io.last_clock = 0;
    io.routeInputs();
    try std.testing.expectEqual(@as(u8, 3), io.pins[0].filter_a.history);
}

test "adjacent pad input precedes inversion and selectors even with output disabled" {
    var io: IO = .{};
    io.pads[0] = .one;
    io.pads[1] = .zero;
    io.sync_in = io.pads;
    for ([_]PinConfiguration.PadMode{ .logic_adjacent_feedback, .schmitt_adjacent_feedback }) |mode| {
        for ([_]bool{ false, true }) |synchronous| {
            for ([_]bool{ false, true }) |invert| {
                io.pins[0].configure(.{ .pad_mode = mode, .synchronous = synchronous, .invert_input = invert });
                io.pins[1].configure(.{ .pad_mode = mode, .synchronous = synchronous, .invert_input = invert });
                io.pins[63].configure(.{ .a = .{ .source = .plus1, .invert = false } });
                io.routeInputs();
                try std.testing.expectEqual(Signal.from(invert), io.smart_a[0]);
                try std.testing.expectEqual(Signal.from(!invert), io.smart_a[1]);
                try std.testing.expectEqual(io.smart_a[0], io.smart_a[63]);
                try std.testing.expect(!io.outputEnabled(@enumFromInt(0)));
            }
        }
    }
}

test "Boolean routing changes A only and preserves four-state B" {
    var io: IO = .{};
    for ([_]Signal{ .zero, .one, .x, .z }) |a| {
        for ([_]Signal{ .zero, .one, .x, .z }) |b| {
            io.pads[0] = a;
            io.pads[1] = b;
            inline for ([_]PinConfiguration.InputLogic{ .a, .a_and_b, .a_or_b, .a_xor_b }) |logic| {
                io.pins[0].configure(.{ .input_logic = logic, .b = .{ .source = .plus1, .invert = false } });
                io.routeInputs();
                try std.testing.expectEqual(Signal.logic(a, b, @enumFromInt(@intFromEnum(logic))), io.smart_a[0]);
                try std.testing.expectEqual(b, io.smart_b[0]);
            }
        }
    }
}

test "GPIO and smart modes receive filtered A and B through the common frame routing" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    for ([_]smart_pin.Mode{ .gpio, .repository }) |mode| {
        hub.init();
        try std.testing.expectEqual(@as(u5, 0), hub.io.filters[0].tap);
        try std.testing.expectEqual(@as(u5, 5), hub.io.filters[1].tap);
        try std.testing.expectEqual(@as(u5, 19), hub.io.filters[2].tap);
        try std.testing.expectEqual(@as(u5, 22), hub.io.filters[3].tap);
        hub.io.setExternal(@enumFromInt(0), .one);
        hub.io.setExternal(@enumFromInt(1), .zero);
        hub.io.pins[2].configure(.{
            .mode = mode,
            .input_logic = .filter0,
            .a = .{ .source = .minus2, .invert = false },
            .b = .{ .source = .minus1, .invert = false },
        });
        hub.cogs[0].write_reg(.DIRA, 4);
        for (0..20) |_| {
            hub.step();
        }
        const inputs = hub.io.pinInputs(@enumFromInt(2));
        try std.testing.expectEqual(Signal.one, inputs.a);
        try std.testing.expectEqual(Signal.zero, inputs.b);
        // GPIO publishes A; a smart mode publishes its own completion flag instead.
        try std.testing.expectEqual(if (mode == .gpio) @as(u64, 4) else 0, hub.io.get_in() & 4);
    }
}

test "switching to virtual TX preserves the ordinary output propagation delay" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    hub.init();
    const pin = &hub.io.pins[4];
    testCommand(hub, @enumFromInt(4), .configure, 0x7c);
    testCommand(hub, @enumFromInt(4), .write_x, (8 << 16) | 7);
    hub.cogs[0].write_reg(.DIRA, 1 << 4);
    testCommand(hub, @enumFromInt(4), .write_y, 0);
    for (0..5) |_| {
        hub.step();
    }
    try std.testing.expectEqual(Signal.zero, hub.io.pads[4]);
    const discard: DataSink = .discard;
    pin.smart.tx_mode = .{ .virtual = &discard };
    for (0..2) |clock| {
        hub.step();
        try std.testing.expectEqual(Signal.zero, hub.io.pads[4]);
        try std.testing.expectEqual(if (clock == 0) Signal.zero else Signal.one, pin.outputs.smart_out);
    }
    for (0..20) |_| {
        hub.step();
        try std.testing.expectEqual(Signal.one, hub.io.pads[4]);
        try std.testing.expectEqual(Signal.one, pin.outputs.smart_out);
    }
}
