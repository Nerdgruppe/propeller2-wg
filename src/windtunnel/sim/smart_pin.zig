//! Smart engines and their configuration/state retention; IO owns routing and electrical pad state.
const std = @import("std");
const p2 = @import("p2");
const signal = @import("signal.zig");
const pipeline = @import("pipeline.zig");
const Pipeline = pipeline.Pipeline;
pub const Mode = p2.types.SmartPinMode;
const Signal = signal.Signal;
const Drive = p2.types.PinConfiguration.Drive;

/// A registered logic level and its optional engine-selected electrical drive.
pub const DriveSignal = struct {
    level: Signal = .low,
    strength: ?Drive = null,
};

pub const DataSource = union(enum) {
    reader: *std.Io.Reader,
    callback: struct {
        context: *anyopaque,
        read: *const fn (*anyopaque) anyerror!?u32,
    },

    /// Never asks a Reader to refill; the host owns refill and EOF handling.
    pub fn read(source: *DataSource) anyerror!?u32 {
        return switch (source.*) {
            .reader => |reader| blk: {
                const bytes = reader.buffered();
                if (bytes.len == 0)
                    break :blk null;
                const byte = bytes[0];
                reader.toss(1);
                break :blk byte;
            },
            .callback => |cb| cb.read(cb.context),
        };
    }
};

pub const DataSink = union(enum) {
    discard,
    writer: *std.Io.Writer,
    live_writer: *std.Io.Writer,
    callback: struct { context: *anyopaque, write: *const fn (*anyopaque, u32) anyerror!void },

    /// Deliver a word to the endpoint, truncating writer-backed words to a byte.
    pub fn write(sink: *const DataSink, value: u32) anyerror!void {
        switch (sink.*) {
            .discard => {},
            .writer => |writer| try writer.writeByte(@truncate(value)),
            .live_writer => |writer| {
                try writer.writeByte(@truncate(value));
                try writer.flush();
            },
            .callback => |cb| try cb.write(cb.context, value),
        }
    }
};

/// Per-pin receive fidelity and its endpoint, independent of WRPIN hardware configuration.
pub const RxMode = union(enum) {
    exact,
    emulated: *DataSource,
    virtual: *DataSource,
};

/// Transmit fidelity; both modes report completed words to a sink.
pub const TxMode = union(enum) {
    emulated: *const DataSink,
    virtual: *const DataSink,
};

const discard_sink: DataSink = .discard;

/// Registers shared by every smart-pin mode.
pub const Registers = struct {
    x: u32 = 0,
    y: u32 = 0,
    result: u32 = 0,
    enabled: bool = false,
    ready: bool = false,
};

/// Serial word width and 16.16 bit period decoded from the live X register.
pub const SerialTiming = struct {
    bits: u6,
    period: u32,

    /// Decode the word width and the integer or fractional UART divider.
    pub fn init(x: u32) SerialTiming {
        return .{ .bits = @as(u6, @intCast(x & 31)) + 1, .period = BaudTimer.period(x) };
    }
};

/// Fixed inputs for one smart-pin update in a simulation frame.
/// IO combines commands and routes input signals before sampling the smart pins.
pub const Inputs = struct {
    /// Mode-selected DIR level. NCOs receive raw cog DIR for their private pipelines;
    /// IO supplies the sampled enable/reset level to the other digital engines.
    dir: u1 = 0,
    /// Physical DAC_MODE for this update; all three DAC encodings become repositories when false.
    dac_mode: bool = false,
    /// Approximate projection of the shared RNG, sampled without advancing it.
    random: u8 = 0,
    /// OR of the cogs' OUT bits, before the physical pad's drive gating and delays.
    out: u1 = 0,
    /// SmartA after input selection, inversion, A/B logic and applicable sampling delays.
    a: Signal = .low,
    /// SmartB after input selection, inversion and applicable sampling delays.
    b: Signal = .low,
    /// Local physical receiver before A/B selection; USB samples the upper endpoint here.
    physical: Signal = .low,
    /// WXPIN value delivered this frame; null means no write, including no implicit ACK.
    /// SmartPin.sample stores a present value in Registers.x before running the logic.
    new_x: ?u32 = null,
    /// WYPIN value delivered this frame; null means no write, including no implicit ACK.
    /// SmartPin.sample stores a present value in Registers.y before running the logic.
    new_y: ?u32 = null,
    /// Acknowledgement delivered by AKPIN, RDPIN or WRPIN this frame.
    /// Clears ready before running the logic; X/Y writes also acknowledge implicitly.
    ack: bool = false,
    /// Current wrapping Hub counter identifying this simulation frame.
    clock: u64 = 0,
    /// Falling DIR edge derived by SmartPin.sample from Registers.enabled.
    /// Caller-supplied values are overwritten before dispatch to the active logic.
    reset: bool = false,
};

/// Signals and scheduler state produced by one smart-pin update.
/// SmartPin.clock latches y into Registers.result and IN into Registers.ready.
/// IO publishes every pin's outputs together, then applies pad routing and register delays.
pub const Outputs = struct {
    /// Logic drive before pad routing, inversion and electrical resolution.
    /// Emulated serial transmitters generate this level; virtual transmitters hold their reset level.
    smart_out: Signal = .low,
    /// Engine-selected drive; null selects the WRPIN high/low drive fields.
    strength: ?Drive = null,
    /// Smart-DAC code overrides the physical mode register's code for this clock.
    dac: ?u8 = null,
    /// Signal sent toward the cog IN registers: routed SmartA for GPIO, ready for smart modes.
    in: u1 = 0,
    /// Current result (Z) returned through RDPIN/RQPIN, distinct from the WYPIN register.
    y: u32 = 0,
    /// Modal carry: UART TX busy, previous state for state timing, otherwise result bit 31.
    flag: bool = false,
    /// Enabled clocking can still change an output, result, or completion indication.
    scheduled: bool = false,
};

/// Endpoint callbacks propagate their I/O errors through the simulator fault path.
pub const SmartPinError = anyerror;

/// Construct the ordinary result and ready signals of a smart engine.
fn outputs(regs: *const Registers, inputs: Inputs) Outputs {
    return .{
        .smart_out = Signal.from(inputs.out != 0),
        .in = @intFromBool(regs.ready),
        .y = regs.result,
        .flag = regs.result >> 31 != 0,
    };
}

/// Limit a transmitted word to the configured serial width.
fn wordMask(x: u32) u32 {
    return @as(u32, std.math.maxInt(u32)) >> @as(u5, @intCast(32 - SerialTiming.init(x).bits));
}

/// Publish a received word, MSB justified as required by serial RDPIN.
fn receive(state: *Outputs, x: u32, value: u32) void {
    state.y = (value & wordMask(x)) << @as(u5, @intCast(32 - SerialTiming.init(x).bits));
    state.flag = state.y >> 31 != 0;
    state.in = 1;
}

pub const Normal = struct {
    /// Route SmartA to IN and the cog OUT signal to SmartOut.
    fn step(_: *Normal, regs: *Registers, inputs: Inputs) SmartPinError!Outputs {
        return .{
            .in = @intFromBool(inputs.a.sample()),
            .smart_out = Signal.from(inputs.out != 0),
            .y = regs.result,
            .flag = regs.result >> 31 != 0,
        };
    }
};

pub const Repository = struct {
    raise_next: bool = false,

    /// Retain Z through DIR reset and publish X with its measured completion delay.
    fn step(logic: *Repository, regs: *Registers, inputs: Inputs) SmartPinError!Outputs {
        var state = outputs(regs, inputs);
        const raise_now = logic.raise_next and
            !inputs.ack and
            inputs.new_x == null and
            inputs.new_y == null and
            !inputs.reset;
        logic.raise_next = false;
        if (inputs.new_x != null and regs.enabled) {
            state.y = regs.x;
            state.flag = state.y >> 31 != 0;
            logic.raise_next = true;
        }
        if (raise_now) {
            state.in = @intFromBool(regs.enabled);
        }
        state.scheduled = logic.raise_next;
        return state;
    }
};

/// DAC noise/dither with a shared long-repository fallback when DAC_MODE is disabled.
fn Dac(comptime mode: Mode) type {
    return struct {
        repository: Repository = .{},
        /// DAC_MODE observed during the preceding engine update.
        dac_mode: bool = false,
        /// Resolve same-mode WRPIN retention when the next update supplies physical DAC_MODE.
        reconfigure: bool = false,
        count: u16 = 0,
        accumulation: u16 = 0,
        value: u16 = 0,

        /// Use repository semantics without DAC_MODE; otherwise emit a DAC code and accumulate A.
        fn step(logic: *@This(), regs: *Registers, inputs: Inputs) SmartPinError!Outputs {
            if (logic.reconfigure and !(logic.dac_mode and inputs.dac_mode)) {
                logic.* = .{};
            }
            logic.reconfigure = false;
            logic.dac_mode = inputs.dac_mode;
            if (!inputs.dac_mode)
                return logic.repository.step(regs, inputs);

            var state = outputs(regs, inputs);
            if (!regs.enabled) {
                logic.count = 0;
                logic.accumulation = 0;
                logic.value = @min(@as(u16, @truncate(regs.y)), 0xff00);
                state.in = 0;
            } else {
                logic.accumulation +%= @intFromBool(inputs.a.sample());
                logic.count +%= 1;
                if (logic.count == @as(u16, @truncate(regs.x))) {
                    state.y = logic.accumulation;
                    state.in = 1;
                    logic.count = 0;
                    logic.accumulation = 0;
                    logic.value = @min(@as(u16, @truncate(regs.y)), 0xff00);
                }
            }
            state.dac = if (mode == .dac_noise)
                inputs.random
            else blk: {
                const fraction: u8 = @truncate(logic.value);
                const phase: u8 = if (mode == .dac_dither_rnd)
                    inputs.random
                else
                    @truncate(logic.count);
                break :blk @as(u8, @intCast(logic.value >> 8)) + @as(u8, @intFromBool(phase < fraction));
            };
            state.flag = false;
            state.scheduled = regs.enabled;
            return state;
        }
    };
}

/// Three wrapping silicon-width integrators, period differences and raw ADC snapshots.
pub fn Adc(comptime external_clock: bool) type {
    return struct {
        const SamplingMode = enum(u2) { automatic, sinc2, sinc3, raw };

        integrators: [3]u27 = @splat(0),
        differences: [2]u27 = @splat(0),
        raw: u32 = 0,
        elapsed: u16 = 0,
        period_override: ?u14 = null,
        pending_period: ?u14 = null,
        ready: bool = false,
        previous_b: bool = false,

        /// Apply X/Y strobes, integrate one selected A bit and publish completed sample periods.
        fn step(logic: *@This(), regs: *Registers, inputs: Inputs) SmartPinError!Outputs {
            var state = outputs(regs, inputs);
            state.flag = false;
            if (inputs.ack or inputs.new_x != null or inputs.new_y != null or !regs.enabled) {
                logic.ready = false;
            }
            state.in = @intFromBool(logic.ready);
            if (logic.pending_period) |period| {
                logic.period_override = period;
                logic.pending_period = null;
                logic.elapsed = 0;
            }
            const b = inputs.b.sample();
            const rising = b and !logic.previous_b;
            logic.previous_b = b;
            if (inputs.new_x != null) {
                logic.period_override = null;
            }
            const mode: SamplingMode = @enumFromInt(@as(u2, @truncate(regs.x >> 4)));
            if (inputs.new_y) |y| {
                if (mode != .automatic) {
                    logic.pending_period = @truncate(y);
                }
            }
            state.scheduled = regs.enabled;
            if (!regs.enabled) {
                logic.elapsed = 0;
                state.in = 0;
            }
            if (external_clock and !rising)
                return state;
            const bit: u27 = @intFromBool(inputs.a.sample());
            const old = logic.integrators;
            logic.integrators[0] +%= bit;
            logic.integrators[1] +%= old[0];
            logic.integrators[2] +%= old[1];
            logic.raw = (logic.raw >> 1) | (@as(u32, @intCast(bit)) << 31);
            if (!regs.enabled)
                return state;
            const period: u16 = if (logic.period_override) |value|
                if (value == 0)
                    0x4000
                else
                    value
            else
                @as(u16, 1) << @as(u4, @truncate(regs.x));
            logic.elapsed +%= 1;
            if (logic.elapsed == period) {
                logic.elapsed = 0;
                const stage: usize = if (mode == .sinc3)
                    2
                else
                    1;
                const accumulator = if (external_clock)
                    old[stage]
                else
                    logic.integrators[stage];
                const difference = accumulator -% logic.differences[0];
                logic.differences[0] = accumulator;
                state.y = switch (mode) {
                    .automatic => blk: {
                        const sample = difference -% logic.differences[1];
                        logic.differences[1] = difference;
                        const exponent: u4 = @truncate(regs.x);
                        const rounding: u27 = if (exponent < 2)
                            0
                        else
                            @as(u27, 1) << (exponent - 2);
                        const rounded = sample +% rounding;
                        break :blk if (exponent == 0)
                            @as(u32, rounded) << 1
                        else
                            @as(u32, rounded) >> (exponent - 1);
                    },
                    .sinc2, .sinc3 => difference,
                    .raw => logic.raw,
                };
                logic.ready = true;
                state.in = 1;
            }
            state.flag = false;
            return state;
        }
    };
}

/// Measured symmetric integer FIRs, normalized to 255 for a continuously high input.
pub const Scope = struct {
    pub const Filter = enum(u2) { tukey68, tukey45, hann28, hann28_alias };

    history: u128 = 0,
    armed: bool = false,

    pub const tukey68 = [_]u8{
        1,  2,  4,  6,  8,  11, 14, 17, 20, 23, 26, 29, 32, 34, 36, 38, 39,
        40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
        40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
        39, 38, 36, 34, 32, 29, 26, 23, 20, 17, 14, 11, 8,  6,  4,  2,  1,
    };
    pub const tukey45 = [_]u8{
        1,  2,  4,  6,  8,  11, 14, 17, 20, 23, 26, 28, 30, 32, 33,
        34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
        33, 32, 30, 28, 26, 23, 20, 17, 14, 11, 8,  6,  4,  2,  1,
    };
    pub const hann28 = [_]u8{
        1,  3,  5,  8,  11, 14, 17, 20, 23, 26, 29, 31, 33, 34,
        34, 33, 31, 29, 26, 23, 20, 17, 14, 11, 8,  5,  3,  1,
    };

    /// Sum the selected filter with exact measured fixed-point truncation.
    pub fn sample(history: u128, filter: Filter) u8 {
        const weights: []const u8 = switch (filter) {
            .tukey68 => &tukey68,
            .tukey45 => &tukey45,
            .hann28, .hann28_alias => &hann28,
        };
        var sum: u16 = 0;
        for (weights, 0..) |weight, index| {
            if (history & (@as(u128, 1) << @intCast(index)) != 0) {
                sum += weight;
            }
        }
        const shift: u4 = switch (filter) {
            .tukey68 => 3,
            .tukey45 => 2,
            .hann28, .hann28_alias => 1,
        };
        return @intCast(sum >> shift);
    }

    /// Publish each clock's filtered sample and apply the strict/inclusive hysteretic trigger.
    fn step(logic: *Scope, regs: *Registers, inputs: Inputs) SmartPinError!Outputs {
        var state = outputs(regs, inputs);
        if (!regs.enabled) {
            logic.history = 0;
            logic.armed = false;
            state.in = 0;
            state.y = 0;
            state.flag = false;
            return state;
        }
        const value = sample(logic.history, @enumFromInt(@as(u2, @truncate(regs.x))));
        logic.history = (logic.history << 1) | @intFromBool(inputs.a.sample());
        const a: u6 = @truncate(regs.x >> 2);
        const b: u6 = @truncate(regs.x >> 10);
        const compared = value >> 2;
        if (logic.armed) {
            const triggered = if (a > b)
                compared < b
            else
                compared >= b;
            if (triggered) {
                logic.armed = false;
                state.in = 1;
            }
        } else {
            logic.armed = if (a > b)
                compared >= a
            else
                compared < a;
        }
        state.y = value;
        state.flag = logic.armed;
        state.scheduled = regs.enabled;
        return state;
    }
};

/// UART divider with a live reload value and a retained fractional remainder.
const BaudTimer = struct {
    remaining: i64 = 0,

    /// Decode X's documented 16.6 fractional or 16-bit integer period.
    fn period(x: u32) u32 {
        return if (x >> 26 == 0) x & 0xffff_fc00 else x & 0xffff_0000;
    }

    /// Begin a full or half bit without rounding away fractional phase.
    fn start(timer: *BaudTimer, x: u32, half: bool) void {
        const interval = @as(i64, period(x)) >> @intFromBool(half);
        timer.remaining = if (half) interval else interval - 0xffff;
    }

    /// Clock the current interval; changes to X affect the next reload, not the elapsed interval.
    fn tick(timer: *BaudTimer, x: u32) bool {
        timer.remaining -= 0x10000;
        if (timer.remaining <= 0) {
            timer.remaining += period(x);
            return true;
        }
        return false;
    }
};

pub const UartTx = struct {
    buffer: ?u32 = null,
    shifter: ?u32 = null,
    shift: u32 = 0,
    position: u6 = 0,
    timer: BaudTimer = .{},
    line: Signal = .high,
    a: Pipeline(bool, 1) = .init(false),

    /// Buffer writes, serialize start/data/stop, and deliver completed words to the current sink.
    fn step(logic: *UartTx, regs: *Registers, inputs: Inputs, mode: TxMode) SmartPinError!Outputs {
        if (mode == .virtual) {
            logic.* = .{};
            var state = try transmitImmediate(regs, inputs, mode.virtual);
            state.smart_out = .high;
            state.flag = false;
            if (regs.enabled and inputs.new_y != null) {
                state.y = regs.y;
            }
            return state;
        }
        var state = outputs(regs, inputs);
        const a = logic.a.step(inputs.a.sample());
        if (!regs.enabled) {
            logic.* = .{};
            state.smart_out = .high;
            state.flag = false;
            return state;
        }
        const active = logic.shifter != null;
        const buffered = logic.buffer != null;
        if (inputs.new_y != null) {
            logic.buffer = regs.y;
        }
        if (logic.shifter) |word| {
            if (logic.timer.tick(regs.x)) {
                if (logic.position == SerialTiming.init(regs.x).bits + 1) {
                    logic.shift = (logic.shift >> 1) | (@as(u32, @intFromBool(a)) << 31);
                    state.y = logic.shift;
                    try mode.emulated.write(word & wordMask(regs.x));
                    logic.shifter = null;
                    logic.line = .high;
                } else {
                    if (logic.position != 0) {
                        logic.shift = (logic.shift >> 1) | (@as(u32, @intFromBool(a)) << 31);
                    }
                    state.y = logic.shift;
                    if (logic.position < 63) {
                        logic.position += 1;
                    }
                    logic.line = if (logic.position == SerialTiming.init(regs.x).bits + 1)
                        .high
                    else
                        Signal.from(logic.shift & 1 != 0);
                }
            }
        }
        if (logic.shifter == null and buffered and BaudTimer.period(regs.x) != 0) {
            if (logic.buffer) |word| {
                logic.shifter = word;
                logic.shift = word;
                logic.buffer = null;
                logic.position = 0;
                if (!active) {
                    logic.timer.start(regs.x, false);
                }
                logic.line = .low;
                state.y = word;
                state.in = 1;
            }
        }
        state.flag = logic.buffer != null or logic.shifter != null;
        state.scheduled = state.flag;
        state.smart_out = logic.line;
        return state;
    }
};

pub const UsartTx = struct {
    buffer: ?u32 = null,
    shifter: ?u32 = null,
    shift: u32 = 0,
    count: u5 = 0,
    b: bool = false,
    line: Signal = .low,
    a: Pipeline(bool, 1) = .init(false),
    primed: bool = false,

    /// Prime in reset and shift data on routed B edges, delaying each output update two clocks.
    fn step(logic: *UsartTx, regs: *Registers, inputs: Inputs, mode: TxMode) SmartPinError!Outputs {
        if (mode == .virtual) {
            logic.* = .{ .a = .init(inputs.a.sample()), .b = inputs.b.sample() };
            var state = try transmitImmediate(regs, inputs, mode.virtual);
            state.smart_out = .low;
            return state;
        }
        var state = outputs(regs, inputs);
        const a = logic.a.step(inputs.a.sample());
        const b = inputs.b.sample();
        const rising = b and !logic.b;
        logic.b = b;
        if (!regs.enabled) {
            if (inputs.reset) {
                logic.* = .{ .b = b };
            }
            if (inputs.new_y != null) {
                logic.buffer = regs.y;
            }
            logic.shifter = regs.y;
            logic.shift = regs.y;
            logic.count = 0;
            logic.primed = true;
            state.smart_out = .low;
            return state;
        }
        if (logic.primed) {
            logic.buffer = null;
            logic.primed = false;
        }
        if (inputs.new_y != null) {
            logic.buffer = regs.y;
            if (regs.x & 32 != 0 and logic.count == 0) {
                logic.shifter = regs.y;
                logic.shift = regs.y;
                logic.buffer = null;
                state.in = 1;
            }
        }
        if (rising) {
            if (logic.count == @as(u5, @truncate(regs.x))) {
                if (logic.shifter) |word| {
                    try mode.emulated.write(word & wordMask(regs.x));
                }
                state.y = logic.shift;
                logic.shifter = regs.y;
                logic.shift = regs.y;
                logic.count = 0;
                if (logic.buffer != null) {
                    state.in = 1;
                    logic.buffer = null;
                }
            } else {
                logic.shift = (logic.shift >> 1) | (@as(u32, @intFromBool(a)) << 31);
                logic.count +%= 1;
            }
        }
        logic.line = Signal.from(logic.shift & 1 != 0);
        state.flag = state.y >> 31 != 0;
        state.smart_out = logic.line;
        state.scheduled = true;
        return state;
    }
};

pub const UartRx = struct {
    const Phase = enum { idle, start, validate, data, finish };
    phase: Phase = .idle,
    shift: u32 = 0,
    count: u6 = 0,
    source_word: ?u32 = null,
    timer: BaudTimer = .{},
    idle_delay: bool = false,
    received: ?u32 = null,

    /// Shift one bit; the following boundary rechecks the live terminal count before capturing Z.
    fn sample(logic: *UartRx, x: u32, a: bool) void {
        logic.shift = (logic.shift >> 1) | (@as(u32, @intFromBool(a)) << 31);
        if (logic.count == @as(u5, @truncate(x))) {
            logic.phase = .finish;
        } else {
            logic.phase = .data;
            logic.count = @min(logic.count + 1, 32);
        }
    }

    /// Recover UART samples with live word length and divider reloads in both timed modes.
    fn step(logic: *UartRx, regs: *Registers, inputs: Inputs, mode: RxMode) SmartPinError!Outputs {
        if (mode == .virtual) {
            logic.* = .{};
            return receiveImmediate(regs, inputs, mode.virtual);
        }
        var state = outputs(regs, inputs);
        if (!regs.enabled) {
            logic.* = .{ .shift = logic.shift };
            return state;
        }
        if (logic.received) |word| {
            state.y = word;
            state.in = 1;
            state.flag = word >> 31 != 0;
            logic.received = null;
        }
        if (mode == .emulated and logic.source_word == null) {
            logic.source_word = try mode.emulated.read();
            if (logic.source_word == null and logic.phase != .idle and logic.phase != .start) {
                state.scheduled = true;
                return state;
            }
        }
        const a = if (mode == .exact)
            inputs.a.sample()
        else if (logic.source_word == null)
            true
        else switch (logic.phase) {
            .idle => !(logic.idle_delay and logic.timer.remaining <= 0x10000),
            .finish => if (logic.count == @as(u5, @truncate(regs.x)))
                true
            else
                logic.source_word.? >> @as(u5, @truncate(@min(logic.count + 1, 32))) & 1 != 0,
            .start, .validate => false,
            .data => logic.source_word.? >> @as(u5, @truncate(logic.count)) & 1 != 0,
        };
        // Minimum-divider probes expose idle and start-search samples in unused receive bits.
        if (BaudTimer.period(regs.x) == 3 << 16 and (logic.phase == .idle or logic.phase == .start)) {
            logic.shift = (logic.shift >> 1) | (@as(u32, @intFromBool(a)) << 31);
        }
        switch (logic.phase) {
            .idle => {
                if (mode == .emulated and logic.idle_delay) {
                    if (logic.timer.tick(regs.x)) {
                        logic.idle_delay = false;
                        if (logic.source_word != null) {
                            logic.phase = .validate;
                            logic.timer.remaining -= @as(i64, BaudTimer.period(regs.x)) >> 1;
                        } else {
                            logic.phase = .start;
                        }
                    }
                } else if (a) {
                    logic.phase = .start;
                }
            },
            .start => {
                if (!a and BaudTimer.period(regs.x) != 0) {
                    logic.phase = .validate;
                    logic.timer.start(regs.x, true);
                }
            },
            .validate => {
                if (logic.timer.tick(regs.x)) {
                    logic.phase = if (a) .start else .data;
                    logic.shift = (logic.shift >> 1) | (@as(u32, @intFromBool(a)) << 31);
                }
            },
            .data => {
                if (logic.timer.tick(regs.x)) {
                    logic.sample(regs.x, a);
                }
            },
            .finish => {
                if (logic.timer.tick(regs.x)) {
                    if (logic.count == @as(u5, @truncate(regs.x))) {
                        logic.received = logic.shift;
                        logic.shift = (logic.shift >> 1) | (@as(u32, @intFromBool(a)) << 31);
                        logic.count = 0;
                        logic.phase = .idle;
                        logic.source_word = null;
                        logic.idle_delay = true;
                        logic.timer.remaining -= @as(i64, BaudTimer.period(regs.x)) >> 1;
                    } else {
                        logic.count = @min(logic.count + 1, 32);
                        logic.sample(regs.x, a);
                    }
                }
            },
        }
        state.flag = state.y >> 31 != 0;
        state.scheduled = true;
        return state;
    }
};

pub const UsartRx = struct {
    shift: u32 = 0,
    count: u6 = 0,
    a: Pipeline(bool, 1) = .init(false),
    b: bool = false,
    source_word: ?u32 = null,

    /// Shift the previous or coincident A sample on routed B edges using the live terminal count.
    fn step(logic: *UsartRx, regs: *Registers, inputs: Inputs, mode: RxMode) SmartPinError!Outputs {
        if (mode == .virtual) {
            logic.* = .{ .a = .init(inputs.a.sample()), .b = inputs.b.sample() };
            return receiveImmediate(regs, inputs, mode.virtual);
        }
        var state = outputs(regs, inputs);
        const a = inputs.a.sample();
        const b = inputs.b.sample();
        const previous_a = logic.a.step(a);
        const sample = if (regs.x & 32 != 0) a else previous_a;
        const rising = b and !logic.b;
        logic.b = b;
        if (!regs.enabled) {
            logic.* = .{ .shift = logic.shift, .a = .init(a), .b = b };
        } else if (rising) {
            if (mode == .emulated and logic.source_word == null) {
                logic.source_word = try mode.emulated.read();
                if (logic.source_word == null) {
                    state.scheduled = true;
                    return state;
                }
            }
            const bit = if (mode == .exact) sample else logic.source_word.? >> @as(u5, @truncate(logic.count)) & 1 != 0;
            logic.shift = (logic.shift >> 1) | (@as(u32, @intFromBool(bit)) << 31);
            if (logic.count == @as(u5, @truncate(regs.x))) {
                state.y = logic.shift;
                state.in = 1;
                logic.count = 0;
                logic.source_word = null;
            } else {
                logic.count = @min(logic.count + 1, 32);
            }
        }
        state.flag = state.y >> 31 != 0;
        state.scheduled = regs.enabled;
        return state;
    }
};

/// Deliver an enabled virtual WYPIN immediately without retaining any timed transmission state.
fn transmitImmediate(regs: *const Registers, inputs: Inputs, sink: *const DataSink) SmartPinError!Outputs {
    var state = outputs(regs, inputs);
    if (regs.enabled and inputs.new_y != null) {
        try sink.write(regs.y & wordMask(regs.x));
        state.in = 1;
    }
    return state;
}

/// Receive at most one endpoint word per clock, preserving unread data until acknowledgement.
fn receiveImmediate(regs: *Registers, inputs: Inputs, source: *DataSource) SmartPinError!Outputs {
    var state = outputs(regs, inputs);
    if (regs.enabled and !regs.ready) {
        if (try source.read()) |word| {
            receive(&state, regs.x, word);
        }
    }
    state.scheduled = regs.enabled;
    return state;
}

/// A 16-bit base-period divider; zero encodes 65536 clocks.
const Divider = struct {
    remaining: u32 = 0,

    /// Hold the initial reload, including the measured extra startup clock, during reset.
    fn reset(divider: *Divider, x: u32) void {
        divider.remaining = period(x) + 1;
    }

    /// Advance one clock and report the base-period boundary.
    fn tick(divider: *Divider, x: u32) bool {
        if (divider.remaining <= 1) {
            divider.remaining = period(x);
            return true;
        }
        divider.remaining -= 1;
        return false;
    }

    /// Decode the wrapping 16-bit reload field.
    fn period(x: u32) u32 {
        const value = x & 0xffff;
        return if (value == 0) 65536 else value;
    }
};

pub const Pulse = struct {
    divider: Divider = .{},
    remaining: u32 = 0,
    active: bool = false,

    /// Start pulse trains at a base boundary and signal their finite completion.
    fn step(logic: *Pulse, regs: *Registers, inputs: Inputs) SmartPinError!Outputs {
        var state = outputs(regs, inputs);
        state.smart_out = .low;
        if (!regs.enabled) {
            logic.divider.reset(regs.x);
            logic.remaining = 0;
            logic.active = false;
            state.in = 0;
            return state;
        }
        if (logic.divider.tick(regs.x)) {
            if (logic.active and logic.remaining != 0) {
                logic.remaining -= 1;
                if (logic.remaining == 0) {
                    state.in = 1;
                }
            }
            logic.active = logic.remaining != 0;
        }
        if (inputs.new_y != null) {
            logic.remaining = regs.y;
        }
        state.smart_out = Signal.from(logic.active and logic.divider.remaining > regs.x >> 16);
        state.scheduled = logic.remaining != 0;
        return state;
    }
};

pub const Transition = struct {
    divider: Divider = .{},
    remaining: u32 = 0,
    level: bool = false,

    /// Toggle at free-running base boundaries and hold the final state until reset.
    fn step(logic: *Transition, regs: *Registers, inputs: Inputs) SmartPinError!Outputs {
        var state = outputs(regs, inputs);
        state.smart_out = .low;
        if (!regs.enabled) {
            logic.divider.reset(regs.x);
            logic.remaining = 0;
            logic.level = false;
            state.in = 0;
            return state;
        }
        if (logic.divider.tick(regs.x) and logic.remaining != 0) {
            logic.level = !logic.level;
            logic.remaining -= 1;
            if (logic.remaining == 0) {
                state.in = 1;
                state.y = regs.y & 1;
            }
        }
        state.flag = false;
        if (inputs.new_y != null) {
            logic.remaining = regs.y;
        }
        state.smart_out = Signal.from(logic.level);
        state.scheduled = logic.remaining != 0;
        return state;
    }
};

/// Frequency and duty NCOs share the accumulator, divider, and overflow indication.
pub fn Nco(comptime duty: bool) type {
    return struct {
        divider: Divider = .{},
        accumulator: u32 = 0,
        direction_pipeline: Pipeline(u1, 2) = .init(0),
        tick_pipeline: Pipeline(bool, 1) = .init(false),

        /// Keep the base counter running until the sampled DIR has also entered reset.
        fn enabled(logic: *const @This(), dir: u1) bool {
            return dir != 0 or logic.direction_pipeline.tap(1) != 0;
        }

        /// Advance the divider, registered accumulator and independently gated publication bus.
        fn step(logic: *@This(), regs: *Registers, inputs: Inputs) SmartPinError!Outputs {
            var state = outputs(regs, inputs);
            const phase_enabled = logic.direction_pipeline.step(inputs.dir) != 0;
            const publish_enabled = logic.direction_pipeline.tap(1) != 0 and phase_enabled;

            if (inputs.new_x != null) {
                logic.accumulator = (logic.accumulator & 0xffff) | (regs.x & 0xffff0000);
            }
            const sum = @addWithOverflow(logic.accumulator, regs.y);
            var tick = false;
            if (regs.enabled) {
                tick = logic.divider.tick(regs.x);
            } else {
                logic.divider.remaining = 0;
                state.in = 0;
            }
            const tick_previous = logic.tick_pipeline.step(tick);

            // The accumulator consumes the preceding divider boundary. The physical drive
            // sees the adder output, while Z captures the registered accumulator on a carry.
            if (!phase_enabled) {
                logic.accumulator = regs.x & 0xffff0000;
            } else if (tick_previous) {
                logic.accumulator = sum[0];
            }
            if (tick and sum[1] != 0 and publish_enabled) {
                state.y = logic.accumulator;
                state.in = 1;
            }
            state.flag = state.y >> 31 != 0;
            state.smart_out = Signal.from(if (duty)
                regs.enabled and sum[1] != 0
            else
                phase_enabled and sum[0] >> 31 != 0);
            state.scheduled = regs.enabled;
            return state;
        }
    };
}

/// PWM variants share a base divider, frame counter, and frame-latched duty value.
pub fn Pwm(comptime mode: Mode) type {
    return struct {
        divider: Divider = .{},
        position: u32 = 1,
        descending: bool = mode == .pwm_triangle,
        duty: u16 = 0,
        inhibited: bool = false,
        waiting: bool = false,

        /// Advance PWM frames, applying the SMPS digital voltage/current gates when selected.
        fn step(logic: *@This(), regs: *Registers, inputs: Inputs) SmartPinError!Outputs {
            var state = outputs(regs, inputs);
            const frame = if (regs.x >> 16 == 0) @as(u32, 65536) else regs.x >> 16;
            state.smart_out = .low;
            state.flag = false;
            if (!regs.enabled) {
                logic.divider.reset(regs.x);
                logic.position = if (mode == .pwm_triangle) frame else 1;
                logic.descending = mode == .pwm_triangle;
                logic.duty = @truncate(regs.y);
                logic.inhibited = false;
                logic.waiting = false;
                state.in = 0;
                return state;
            }
            if (mode == .pwm_smps and inputs.b.sample()) {
                logic.inhibited = true;
            }
            if (logic.divider.tick(regs.x)) {
                var boundary = false;
                if (mode == .pwm_triangle) {
                    if (logic.descending) {
                        if (logic.position == 1) {
                            logic.descending = false;
                        } else {
                            logic.position -= 1;
                        }
                    } else if (logic.position == frame) {
                        logic.descending = true;
                        boundary = true;
                    } else {
                        logic.position = if (logic.position == 65536) 1 else logic.position + 1;
                    }
                } else if (logic.position == frame or logic.waiting) {
                    if (mode == .pwm_smps and inputs.a.sample()) {
                        logic.waiting = true;
                    } else {
                        logic.position = 1;
                        logic.waiting = false;
                        boundary = true;
                    }
                } else {
                    logic.position = if (logic.position == 65536) 1 else logic.position + 1;
                }
                if (boundary) {
                    state.y = (regs.x & 0xffff0000) | (regs.x >> 16);
                    logic.duty = @truncate(regs.y);
                    logic.inhibited = mode == .pwm_smps and inputs.b.sample();
                    state.in = 1;
                }
            }
            state.smart_out = Signal.from(!logic.waiting and !logic.inhibited and logic.duty >= logic.position);
            state.scheduled = true;
            return state;
        }
    };
}

/// Limit elapsed-time measurements to the hardware's unsigned saturation value.
fn saturatingTick(value: u32) u32 {
    return if (value < 0x80000000) value + 1 else value;
}

/// Input counters share periodic snapshot timing and boundary-event carryover.
pub fn Counter(comptime mode: Mode) type {
    return struct {
        a: bool = false,
        b: bool = false,
        accumulator: u32 = 0,
        remaining: u32 = 0,

        /// Decode a signed per-clock increment from the current and previous A/B inputs.
        fn increment(logic: *const @This(), regs: *const Registers, a: bool, b: bool) i32 {
            const rise_a = a and !logic.a;
            const rise_b = b and !logic.b;
            return switch (mode) {
                .quadrature => if (a != logic.a and b == logic.b)
                    (if (a != b) @as(i32, 1) else -1)
                else if (b != logic.b and a == logic.a)
                    (if (a == b) @as(i32, 1) else -1)
                else
                    0,
                .reg_up => @intFromBool(rise_a and logic.b),
                .reg_up_down => if (rise_a) (if (logic.b) @as(i32, 1) else -1) else 0,
                .count_rises => @as(i32, @intFromBool(rise_a)) -
                    @as(i32, @intFromBool(regs.y & 1 != 0 and rise_b)),
                .count_highs => @as(i32, @intFromBool(a)) -
                    @as(i32, @intFromBool(regs.y & 1 != 0 and b)),
                else => unreachable,
            };
        }

        /// Accumulate continuously or publish completed windows without losing the boundary event.
        fn step(logic: *@This(), regs: *Registers, inputs: Inputs) SmartPinError!Outputs {
            var state = outputs(regs, inputs);
            const a = inputs.a.sample();
            const b = inputs.b.sample();
            const delta: u32 = @bitCast(logic.increment(regs, a, b));
            logic.a = a;
            logic.b = b;
            if (!regs.enabled) {
                logic.accumulator = delta;
                logic.remaining = regs.x -% 1;
                state.y = delta;
                state.in = 0;
            } else if (regs.x == 0) {
                state.y = logic.accumulator;
                logic.accumulator +%= delta;
            } else if (logic.remaining <= 1) {
                state.y = logic.accumulator;
                logic.accumulator = delta;
                logic.remaining = regs.x;
                state.in = 1;
            } else {
                logic.remaining -= 1;
                logic.accumulator +%= delta;
            }
            state.flag = state.y >> 31 != 0;
            state.scheduled = regs.enabled and regs.x != 0;
            return state;
        }
    };
}

/// State and high-state duration modes share a saturated per-state counter.
pub fn Duration(comptime high_only: bool) type {
    return struct {
        a: bool = false,
        ticks: u32 = 1,
        result_state: bool = false,

        /// Publish completed durations and, for state timing, the previous input state in C.
        fn step(logic: *@This(), regs: *Registers, inputs: Inputs) SmartPinError!Outputs {
            var state = outputs(regs, inputs);
            const a = inputs.a.sample();
            const changed = a != logic.a;
            if (!regs.enabled) {
                logic.ticks = 1;
                logic.result_state = false;
                state.y = 1;
                state.in = 0;
            } else if (changed) {
                if (!high_only or logic.a) {
                    state.y = logic.ticks;
                    logic.result_state = logic.a;
                    state.in = 1;
                }
                logic.ticks = 1;
            } else if (!high_only or a) {
                logic.ticks = saturatingTick(logic.ticks);
            }
            logic.a = a;
            state.flag = if (high_only) state.y >> 31 != 0 else logic.result_state;
            return state;
        }
    };
}

pub const Events = struct {
    a: bool = false,
    ticks: u32 = 1,
    remaining: u32 = 0,
    timeout: u32 = 0,

    /// Measure event groups or continuously report elapsed clocks with periodic missing-event alarms.
    fn step(logic: *Events, regs: *Registers, inputs: Inputs) SmartPinError!Outputs {
        var state = outputs(regs, inputs);
        const a = inputs.a.sample();
        const event = switch (@as(u2, @truncate(regs.y))) {
            0 => a,
            1 => a and !logic.a,
            2, 3 => a != logic.a,
        };
        logic.a = a;
        if (!regs.enabled) {
            logic.ticks = if (regs.y & 7 == 0) 0 else 1;
            logic.remaining = regs.x;
            logic.timeout = regs.x;
            state.y = 1;
            state.in = 0;
        } else if (regs.y & 4 != 0) {
            state.y = logic.ticks;
            if (event) {
                logic.ticks = 1;
                logic.timeout = regs.x;
            } else {
                logic.ticks = saturatingTick(logic.ticks);
                logic.timeout -%= 1;
                if (logic.timeout == 0) {
                    logic.timeout = regs.x;
                    state.in = 1;
                }
            }
        } else {
            if (regs.y & 3 == 0) {
                logic.ticks = saturatingTick(logic.ticks);
            }
            if (event) {
                logic.remaining -%= 1;
                if (logic.remaining == 0) {
                    state.y = logic.ticks;
                    logic.ticks = 0;
                    logic.remaining = regs.x;
                    state.in = 1;
                }
            }
            if (regs.y & 3 != 0) {
                logic.ticks = saturatingTick(logic.ticks);
            }
        }
        state.flag = state.y >> 31 != 0;
        state.scheduled = regs.enabled;
        return state;
    }
};

/// Fixed-period and minimum-window measurements share A-start/B-end detection.
pub fn Periods(comptime mode: Mode) type {
    return struct {
        a: bool = false,
        b: bool = false,
        active: bool = false,
        level: bool = false,
        started: bool = false,
        elapsed: u64 = 0,
        accumulated: u32 = 0,
        periods: u64 = 0,

        /// Accumulate complete periods, retaining coincident starts for the next period.
        fn step(logic: *@This(), regs: *Registers, inputs: Inputs) SmartPinError!Outputs {
            var state = outputs(regs, inputs);
            const a = inputs.a.sample();
            const b = inputs.b.sample();
            const previous_a = logic.a;
            const start = a != logic.a and (regs.y & 2 != 0 or a);
            const end = b != logic.b and (regs.y & 1 != 0 or b);
            logic.a = a;
            logic.b = b;
            if (!regs.enabled) {
                logic.active = false;
                logic.started = false;
                logic.elapsed = 0;
                logic.accumulated = 0;
                logic.periods = 0;
                state.y = 0;
                state.in = 0;
            } else {
                if (logic.started) {
                    logic.elapsed = @min(logic.elapsed + 1, @as(u64, 1) << 32);
                }
                if (logic.active) {
                    if (mode == .periods_ticks or mode == .counter_ticks or
                        ((mode == .periods_highs or mode == .counter_highs) and previous_a == logic.level))
                    {
                        logic.accumulated = saturatingTick(logic.accumulated);
                    }
                    if (end) {
                        logic.periods = @min(logic.periods + 1, @as(u64, 1) << 32);
                        const done = if (mode == .periods_ticks or mode == .periods_highs)
                            logic.periods == (if (regs.x == 0) @as(u64, 1) << 32 else regs.x)
                        else
                            logic.elapsed >= (if (regs.x == 0) @as(u64, 1) << 32 else regs.x);
                        if (done) {
                            state.y = if (mode == .counter_periods)
                                @intCast(@min(logic.periods, 0x80000000))
                            else
                                logic.accumulated;
                            state.in = 1;
                            logic.accumulated = if (mode == .periods_highs or mode == .counter_highs)
                                @intFromBool(!start and a == logic.level)
                            else
                                0;
                            logic.periods = 0;
                            logic.elapsed = 0;
                            logic.started = false;
                        } else if (mode == .periods_highs or mode == .counter_highs) {
                            logic.accumulated +%= @intFromBool(!start and a == logic.level);
                        }
                        logic.active = false;
                    }
                }
                if (start and !logic.active) {
                    logic.active = true;
                    logic.level = a;
                    logic.started = true;
                }
            }
            state.flag = state.y >> 31 != 0;
            state.scheduled = regs.enabled and logic.started;
            return state;
        }
    };
}

/// One physical USB pair with two sampled ports and one shared hardware state.
pub const Usb = struct {
    /// Input refers to a pin's sampled frame; output is produced once by the shared block.
    pub const Port = struct {
        frame: ?*const SmartPin.Frame = null,
        registers: ?*const Registers = null,
        output: Outputs = .{},
    };

    /// USB smart-pin logic only forwards the selected pair endpoint's published signals.
    pub const Forwarder = struct {
        port: *const Port,

        /// Read the pair output without owning or advancing any USB hardware state.
        fn step(endpoint: *Forwarder, _: *Registers, _: Inputs) SmartPinError!Outputs {
            return endpoint.port.output;
        }
    };

    ports: [2]Port = @splat(.{}),
    state: State = .{},

    /// Clock the shared hardware after both ports have sampled, without changing pin registers.
    pub fn clock(usb: *Usb) void {
        const lower = usb.ports[0].frame.?;
        const upper = usb.ports[1].frame.?;
        if (lower.mode != .usb and upper.mode != .usb) {
            if (lower.mode_changed) {
                usb.state = .{};
            }
            return;
        }
        if (lower.mode_changed or lower.mode != .usb) {
            usb.state = .{};
        }
        var lower_registers = usb.ports[0].registers.?.*;
        var upper_registers = usb.ports[1].registers.?.*;
        const next = usb.state.advance(
            &lower_registers,
            &upper_registers,
            .{ lower.signals, upper.signals },
        );
        usb.ports[0].output = next[0];
        usb.ports[1].output = next[1];
        if (!upper_registers.enabled) {
            usb.ports[1].output.in = 0;
        }
    }

    /// Shared serializer/decoder state; configuration comes from the lower port.
    const State = struct {
        const Speed = enum(u1) { low, full };
        const Role = enum(u1) { device, host };
        const StateCommand = enum(u2) { idle, se0, k, j };
        const Wire = struct { dm: bool = false, dp: bool = false };

        const Configuration = packed struct(u16) {
            increment: u14,
            speed: Speed,
            role: Role,
        };

        /// Silicon receiver result, low flags followed by the last completed byte.
        pub const Status = packed struct(u16) {
            j: bool = false,
            k: bool = false,
            se0: bool = false,
            steady: bool = false,
            sop: bool = false,
            eop: bool = false,
            err: bool = false,
            toggle: bool = false,
            byte: u8 = 0,
        };

        const Line = enum { se0, j, k, se1 };
        const Tx = enum { idle, sync, data, eop_second, eop_j, release };

        phase: u16 = 0,
        tx_speed: Speed = .low,
        rx_speed: Speed = .low,
        /// Live speed and physical samples traverse the measured receiver input stages.
        speed_pipeline: Pipeline(Speed, 2) = .init(.low),
        receiver_input: Pipeline(Wire, 2) = .init(.{}),
        /// Receiver reset visibility is independent of the transmitter's immediate DIR gate.
        rx_enable: Pipeline(bool, 6) = .init(false),
        pending: ?u8 = null,
        tx: Tx = .idle,
        drive: ?Line = null,
        shift: u8 = 0,
        bits: u4 = 0,
        ones: u3 = 0,
        previous: Line = .se0,
        rx_phase: u16 = 0,
        steady: u20 = 0,
        se0: u20 = 0,
        rx_last: Line = .j,
        rx_shift: u8 = 0xff,
        rx_bits: u4 = 0,
        rx_ones: u3 = 0,
        packet: bool = false,
        eop: bool = false,
        status: Status = .{},

        /// Convert the measured wire polarity into normalized USB line states.
        fn line(dm: bool, dp: bool, speed: Speed) Line {
            if (dm == dp)
                return if (dm) .se1 else .se0;
            return if (dp == (speed == .full)) .j else .k;
        }

        /// Toggle an NRZI differential symbol; transitions encode zero bits.
        fn invert(value: Line) Line {
            return switch (value) {
                .j => .k,
                .k => .j,
                else => .k,
            };
        }

        /// Consume the one pending FIFO entry and latch the upper endpoint's notification.
        fn take(usb: *State, upper: *Registers) ?u8 {
            const value = usb.pending;
            if (value != null) {
                usb.pending = null;
                if (upper.enabled) {
                    upper.ready = true;
                }
            }
            return value;
        }

        /// Emit one baud interval, retaining stuffing state across byte boundaries.
        fn transmit(usb: *State, upper: *Registers) void {
            switch (usb.tx) {
                .idle => {
                    if (usb.take(upper)) |command| {
                        if (command & 0x80 != 0) {
                            usb.tx = .sync;
                            usb.shift = command;
                            usb.bits = 8;
                            usb.drive = .j;
                            usb.transmit(upper);
                        } else if (command & 4 != 0) {
                            usb.drive = .se0;
                            usb.tx = .eop_second;
                        } else switch (@as(StateCommand, @enumFromInt(@as(u2, @truncate(command))))) {
                            .idle => usb.drive = null,
                            .se0 => usb.drive = .se0,
                            .k => usb.drive = .k,
                            .j => usb.drive = .j,
                        }
                    }
                },
                .sync => {
                    if (usb.bits == 0) {
                        usb.tx = .data;
                        usb.ones = 1;
                        usb.transmit(upper);
                    } else {
                        if (usb.shift & 1 == 0) {
                            usb.drive = invert(usb.drive orelse .j);
                        }
                        usb.shift >>= 1;
                        usb.bits -= 1;
                    }
                },
                .data => {
                    if (usb.ones == 6) {
                        usb.drive = invert(usb.drive orelse .j);
                        usb.ones = 0;
                    } else {
                        if (usb.bits == 0) {
                            if (usb.take(upper)) |byte| {
                                usb.shift = byte;
                                usb.bits = 8;
                            } else {
                                usb.drive = .se0;
                                usb.tx = .eop_second;
                                return;
                            }
                        }
                        if (usb.shift & 1 == 0) {
                            usb.drive = invert(usb.drive orelse .j);
                            usb.ones = 0;
                        } else {
                            usb.ones += 1;
                        }
                        usb.shift >>= 1;
                        usb.bits -= 1;
                    }
                },
                .eop_second => {
                    usb.drive = .se0;
                    usb.tx = .eop_j;
                },
                .release => {
                    usb.drive = null;
                    usb.tx = .idle;
                },
                .eop_j => {
                    usb.drive = .j;
                    usb.tx = .release;
                },
            }
        }

        /// Decode one centered differential sample, including SYNC and stuffed-zero validation.
        fn sample(usb: *State, value: Line) void {
            if (value != .j and value != .k)
                return;
            const bit = value == usb.rx_last;
            usb.rx_last = value;
            if (!usb.packet) {
                usb.rx_shift = (usb.rx_shift >> 1) | (@as(u8, @intFromBool(bit)) << 7);
                if (usb.rx_shift == 0x80) {
                    usb.packet = true;
                    usb.status.toggle = false;
                    usb.status.err = false;
                    usb.status.eop = false;
                    usb.status.sop = true;
                    usb.rx_bits = 0;
                    usb.rx_ones = 1;
                }
                return;
            }
            if (usb.rx_ones == 6) {
                usb.rx_ones = 0;
                if (bit) {
                    usb.status.err = true;
                    usb.packet = false;
                    usb.rx_shift = 0xff;
                }
                return;
            }
            usb.rx_ones = if (bit)
                usb.rx_ones + 1
            else
                0;
            usb.rx_shift = (usb.rx_shift >> 1) | (@as(u8, @intFromBool(bit)) << 7);
            usb.rx_bits += 1;
            if (usb.rx_bits == 8) {
                usb.rx_bits = 0;
                usb.status.byte = usb.rx_shift;
                usb.status.toggle = !usb.status.toggle;
            }
        }

        /// Qualify line status at centered baud samples; short SE1 skew is not an illegal symbol.
        fn receive(usb: *State, value: Line, increment: u16) void {
            if (value != usb.previous) {
                usb.steady = 0;
                usb.rx_phase = 0;
                usb.previous = value;
            }
            usb.steady = @min(usb.steady + @as(u20, increment), 8 * 65536);
            const old = usb.rx_phase;
            usb.rx_phase +%= increment;
            if (!(old < 0x8000 and usb.rx_phase >= 0x8000))
                return;
            if (value != .se0 and usb.se0 != 0) {
                usb.eop = false;
                usb.se0 = 0;
            }
            usb.status.j = value == .j;
            usb.status.k = value == .k;
            usb.status.se0 = value == .se0;
            usb.status.steady = usb.steady >= 7 * 65536;
            if (usb.status.steady) {
                usb.status.sop = false;
                usb.status.eop = false;
            }
            switch (value) {
                .se1 => {
                    usb.status.err = true;
                    usb.packet = false;
                    usb.rx_shift = 0xff;
                },
                .se0 => {
                    usb.se0 = @min(usb.se0 + 1, 8);
                    if (usb.packet) {
                        usb.packet = false;
                        usb.eop = true;
                        usb.status.sop = false;
                        usb.status.eop = true;
                    }
                    if (usb.eop and usb.se0 > 3) {
                        usb.status.err = true;
                    }
                    usb.rx_last = .j;
                    usb.rx_shift = 0xff;
                },
                .j, .k => usb.sample(value),
            }
        }

        /// Advance independent TX/RX machines using the lower endpoint's shared fractional baud rate.
        fn advance(usb: *State, lower: *Registers, upper: *Registers, inputs: [2]Inputs) [2]Outputs {
            const old_status = usb.status;
            const configuration: Configuration = @bitCast(@as(u16, @truncate(lower.x)));
            const increment: u16 = configuration.increment;
            const rx_enabled = usb.rx_enable.step(lower.enabled);
            const physical = usb.receiver_input.step(.{
                .dm = inputs[0].a.sample(),
                .dp = inputs[1].physical.sample(),
            });
            const full_speed = configuration.speed == .full;
            const rx_full_speed = usb.speed_pipeline.step(configuration.speed);
            if (usb.rx_speed != rx_full_speed) {
                if (usb.status.steady) {
                    const j = usb.status.j;
                    usb.status.j = usb.status.k;
                    usb.status.k = j;
                }
                if (usb.previous == .j or usb.previous == .k) {
                    usb.previous = invert(usb.previous);
                }
                usb.rx_last = invert(usb.rx_last);
                usb.rx_speed = rx_full_speed;
            }
            if (!lower.enabled) {
                usb.phase = increment;
                usb.pending = null;
                usb.tx = .idle;
                usb.drive = null;
                lower.ready = false;
            } else {
                const next = @as(u17, usb.phase) + increment;
                usb.phase = @truncate(next);
                if (next > 0xffff) {
                    usb.tx_speed = configuration.speed;
                    usb.transmit(upper);
                }
                if (inputs[0].new_y) |value| {
                    usb.pending = @truncate(value);
                }
            }
            if (rx_enabled) {
                usb.receive(line(physical.dm, physical.dp, rx_full_speed), increment);
                if (lower.enabled and !std.meta.eql(usb.status, old_status)) {
                    lower.ready = true;
                }
            } else {
                usb.status = .{};
                usb.packet = false;
                usb.eop = false;
                usb.rx_shift = 0xff;
                usb.rx_bits = 0;
                usb.rx_ones = 0;
                usb.previous = .se0;
                usb.rx_phase = 0;
                usb.steady = 0;
                usb.se0 = 0;
                usb.rx_last = .j;
            }
            // Upper acknowledgement wins a coincident FIFO-transfer notification.
            if (inputs[1].ack or inputs[1].new_x != null or inputs[1].new_y != null) {
                upper.ready = false;
            }
            var result = [2]Outputs{ outputs(lower, inputs[0]), outputs(upper, inputs[1]) };
            result[0].y = @as(u16, @bitCast(usb.status));
            result[0].flag = usb.status.err;
            result[1].y = 0;
            result[1].flag = false;
            for (&result, 0..) |*state, index| {
                state.scheduled = lower.enabled;
                state.strength = .float;
                if (!lower.enabled)
                    continue;
                const j_high = (index == 1) == full_speed;
                if (usb.drive) |value| {
                    const driven_j_high = (index == 1) == (usb.tx_speed == .full);
                    state.smart_out = Signal.from(switch (value) {
                        .j => driven_j_high,
                        .k => !driven_j_high,
                        .se0 => false,
                        .se1 => true,
                    });
                    state.strength = .fast;
                } else if (configuration.role == .host) {
                    state.smart_out = .low;
                    state.strength = .resistor_15k;
                } else if (j_high) {
                    state.smart_out = .high;
                    state.strength = .resistor_1k5;
                }
            }
            return result;
        }
    };
};

/// Shared registers and a mode-specific payload containing only private logic state.
pub const SmartPin = struct {
    registers: Registers = .{},
    logic: Logic = .{ .gpio = .{} },
    rx_mode: RxMode = .exact,
    tx_mode: TxMode = .{ .emulated = &discard_sink },
    /// Cog OUT transport remains clocked while an engine overrides the output.
    out_pipeline: Pipeline(u1, 2) = .init(0),
    /// Output engines publish through a register; GPIO passes the registered cog OUT bus through.
    output_register: Pipeline(DriveSignal, 1) = .init(.{}),
    adc_ready_pipeline: Pipeline(u1, 3) = .init(0),
    /// Stable attachment to a pair-bank port, retained through smart-mode changes.
    usb_port: ?*Usb.Port = null,
    frame: Frame = .{},
    mode_changed: bool = false,

    pub const Logic = union(Mode) {
        gpio: Normal,
        dac_noise: Dac(.dac_noise),
        dac_dither_rnd: Dac(.dac_dither_rnd),
        dac_dither_pwm: Dac(.dac_dither_pwm),
        pulse: Pulse,
        transition: Transition,
        nco_frequency: Nco(false),
        nco_duty: Nco(true),
        pwm_triangle: Pwm(.pwm_triangle),
        pwm_sawtooth: Pwm(.pwm_sawtooth),
        pwm_smps: Pwm(.pwm_smps),
        quadrature: Counter(.quadrature),
        reg_up: Counter(.reg_up),
        reg_up_down: Counter(.reg_up_down),
        count_rises: Counter(.count_rises),
        count_highs: Counter(.count_highs),
        state_ticks: Duration(false),
        high_ticks: Duration(true),
        events_ticks: Events,
        periods_ticks: Periods(.periods_ticks),
        periods_highs: Periods(.periods_highs),
        counter_ticks: Periods(.counter_ticks),
        counter_highs: Periods(.counter_highs),
        counter_periods: Periods(.counter_periods),
        adc: Adc(false),
        adc_ext: Adc(true),
        adc_scope: Scope,
        usb: Usb.Forwarder,
        sync_tx: UsartTx,
        sync_rx: UsartRx,
        uart_tx: UartTx,
        uart_rx: UartRx,
    };

    /// Decode the documented engine override of WRPIN's OUT/OTHER selector.
    pub fn overridesOutput(pin: *const SmartPin) bool {
        return std.meta.activeTag(pin.logic).overridesOutput();
    }

    /// Return the measured engine-to-cog result latency, including the shared two-clock return path.
    pub fn resultLatency(pin: *const SmartPin) u64 {
        return switch (pin.logic) {
            .adc, .adc_ext => 4,
            .adc_scope => 3,
            else => if (pin.digitalLogic() and pin.logic != .events_ticks)
                3
            else
                2,
        };
    }

    /// Apply the digital engine's reset-release sampling; NCOs maintain their own internal DIR stages.
    pub fn directionInput(pin: *const SmartPin, raw: bool, registered: bool) u1 {
        if (pin.logic == .adc_scope)
            return @intFromBool(registered);
        const nco = pin.logic == .nco_frequency or pin.logic == .nco_duty;
        return @intFromBool(raw and (nco or !pin.digitalLogic() or registered));
    }

    /// Identify digital engines with measured reset-release and result stages.
    fn digitalLogic(pin: *const SmartPin) bool {
        return std.meta.activeTag(pin.logic).digitalLogic();
    }

    /// Configure the engine mode; connectUsb must bind a physical port before selecting USB.
    /// DAC aliases resolve retention using DAC_MODE in the next sample.
    pub fn configure(pin: *SmartPin, mode: Mode) void {
        if (std.meta.activeTag(pin.logic) == mode) {
            switch (pin.logic) {
                inline .dac_noise, .dac_dither_rnd, .dac_dither_pwm => |*logic| {
                    logic.reconfigure = true;
                    return;
                },
                else => {
                    if (mode.retainsState())
                        return;
                },
            }
        }
        pin.mode_changed = std.meta.activeTag(pin.logic) != mode;
        pin.adc_ready_pipeline = .init(0);
        pin.logic = switch (mode) {
            .usb => .{ .usb = .{ .port = pin.usb_port.? } },
            inline else => |tag| @unionInit(Logic, @tagName(tag), .{}),
        };
    }

    const Frame = struct {
        signals: Inputs = .{},
        out: u1 = 0,
        mode: Mode = .gpio,
        mode_changed: bool = false,
    };

    /// Bind this pin's USB forwarder to its fixed physical endpoint in the pair bank.
    pub fn connectUsb(pin: *SmartPin, port: *Usb.Port) void {
        pin.usb_port = port;
        port.frame = &pin.frame;
        port.registers = &pin.registers;
        if (pin.logic == .usb) {
            pin.logic.usb.port = port;
        }
    }

    /// Sample fixed inputs and commands for this edge, before any smart engine is clocked.
    pub fn sample(pin: *SmartPin, inputs: Inputs) void {
        var signals = inputs;
        signals.out = pin.out_pipeline.tap(0);
        const out = pin.out_pipeline.step(inputs.out);
        const enabled = switch (pin.logic) {
            inline .nco_frequency, .nco_duty => |*logic| logic.enabled(inputs.dir),
            else => inputs.dir != 0,
        };
        signals.reset = pin.registers.enabled and !enabled;
        pin.registers.enabled = enabled;
        if (inputs.new_x) |value| {
            pin.registers.x = value;
        }
        if (inputs.new_y) |value| {
            pin.registers.y = value;
        }
        if (inputs.ack or inputs.new_x != null or inputs.new_y != null or signals.reset) {
            pin.registers.ready = false;
        }
        pin.frame = .{
            .signals = signals,
            .out = out,
            .mode = std.meta.activeTag(pin.logic),
            .mode_changed = pin.mode_changed,
        };
        pin.mode_changed = false;
    }

    /// Run an individual engine without publishing its registered output yet.
    fn dispatch(pin: *SmartPin, signals: Inputs) SmartPinError!Outputs {
        return switch (pin.logic) {
            .uart_rx => |*logic| logic.step(&pin.registers, signals, pin.rx_mode),
            .sync_rx => |*logic| logic.step(&pin.registers, signals, pin.rx_mode),
            .uart_tx => |*logic| logic.step(&pin.registers, signals, pin.tx_mode),
            .sync_tx => |*logic| logic.step(&pin.registers, signals, pin.tx_mode),
            inline else => |*logic| logic.step(&pin.registers, signals),
        };
    }

    /// Register the output drive together with its level and latch result/readiness.
    fn publish(pin: *SmartPin, frame: Frame, next: Outputs) Outputs {
        var state = next;
        if (pin.logic == .adc or pin.logic == .adc_ext) {
            state.in = pin.adc_ready_pipeline.step(state.in);
        }
        const output = pin.output_register.step(.{ .level = state.smart_out, .strength = state.strength });
        if (pin.overridesOutput()) {
            state.smart_out = output.level;
            state.strength = output.strength;
        } else {
            state.smart_out = Signal.from(frame.out != 0);
            state.strength = null;
        }
        pin.registers.result = state.y;
        pin.registers.ready = state.in != 0;
        return state;
    }

    /// Clock the engine from sampled inputs and latch its output/result registers.
    /// Shared USB pairs must be clocked between sample() and this operation.
    pub fn clock(pin: *SmartPin) SmartPinError!Outputs {
        return pin.publish(pin.frame, try pin.dispatch(pin.frame.signals));
    }
};

/// A word endpoint records full-width transfers and never refills host Readers.
const SerialDevice = struct {
    values: []const u32,
    reads: usize = 0,
    written: [128]u32 = @splat(0),
    writes: usize = 0,

    /// Consume the next available word once.
    fn read(context: *anyopaque) anyerror!?u32 {
        const device: *SerialDevice = @ptrCast(@alignCast(context));
        if (device.reads == device.values.len) {
            return null;
        }
        const value = device.values[device.reads];
        device.reads += 1;
        return value;
    }

    /// Retain completed words without truncating their high bits.
    fn write(context: *anyopaque, value: u32) anyerror!void {
        const device: *SerialDevice = @ptrCast(@alignCast(context));
        device.written[device.writes] = value;
        device.writes += 1;
    }

    /// Create the receive callback descriptor used by a test pin.
    fn source(device: *SerialDevice) DataSource {
        return .{ .callback = .{ .context = device, .read = read } };
    }

    /// Create the completion callback descriptor used by a test pin.
    fn sink(device: *SerialDevice) DataSink {
        return .{ .callback = .{ .context = device, .write = write } };
    }
};

/// Clock one independent test pin using the same sample/evaluate boundary as IO.
fn clockPin(pin: *SmartPin, inputs: Inputs) SmartPinError!Outputs {
    pin.sample(inputs);
    return pin.clock();
}

/// Clock two attached test endpoints with shared hardware between sampling and publication.
fn clockUsb(lower: *SmartPin, upper: *SmartPin, usb: *Usb, inputs: [2]Inputs) [2]SmartPinError!Outputs {
    lower.sample(inputs[0]);
    upper.sample(inputs[1]);
    usb.clock();
    return .{ lower.clock(), upper.clock() };
}

test "virtual serial modes acknowledge RX and deliver TX writes immediately without transitions" {
    for ([_]Mode{ .uart_rx, .sync_rx }) |mode| {
        var device: SerialDevice = .{ .values = &.{ 0x89abcdef, 0x12345678 } };
        var source = device.source();
        var pin: SmartPin = .{ .rx_mode = .{ .virtual = &source } };
        pin.configure(mode);
        _ = try clockPin(&pin, .{ .dir = 1, .new_x = 31 });
        try std.testing.expectEqual(@as(u32, 0x89abcdef), pin.registers.result);
        for (0..32) |_| {
            _ = try clockPin(&pin, .{ .dir = 1 });
        }
        try std.testing.expectEqual(@as(usize, 1), device.reads);
        _ = try clockPin(&pin, .{ .dir = 1, .ack = true });
        try std.testing.expectEqual(@as(u32, 0x12345678), pin.registers.result);
        _ = try clockPin(&pin, .{ .dir = 1, .ack = true });
        try std.testing.expect(!pin.registers.ready);
    }
    for ([_]Mode{ .uart_tx, .sync_tx }) |mode| {
        var device: SerialDevice = .{ .values = &.{} };
        const sink = device.sink();
        var pin: SmartPin = .{ .tx_mode = .{ .virtual = &sink } };
        pin.configure(mode);
        // Let the output register publish the protocol's reset level before sending words.
        _ = try clockPin(&pin, .{});
        for (0..32) |i| {
            const state = try clockPin(&pin, .{ .dir = 1, .new_x = 31, .new_y = @as(u32, 0x80000000) + @as(u32, @intCast(i)) });
            try std.testing.expectEqual(if (mode == .uart_tx) Signal.high else Signal.low, state.smart_out);
            try std.testing.expectEqual(i + 1, device.writes);
        }
        _ = try clockPin(&pin, .{ .dir = 1 });
        try std.testing.expectEqual(@as(usize, 32), device.writes);
        try std.testing.expectEqual(@as(u32, 0x8000001f), device.written[31]);
        _ = try clockPin(&pin, .{ .dir = 1, .new_y = 0xffffffff });
        _ = try clockPin(&pin, .{});
        try std.testing.expectEqual(@as(usize, 33), device.writes);
        try std.testing.expectEqual(@as(u32, 0xffffffff), device.written[32]);
        // Writes while reset do not prime a virtual or subsequent timed transfer.
        _ = try clockPin(&pin, .{ .new_y = 123 });
        _ = try clockPin(&pin, .{ .dir = 1 });
        try std.testing.expectEqual(@as(usize, 33), device.writes);
    }
}

test "virtual reception discards partial words and returns to a complete fresh timed word" {
    for ([_]Mode{ .uart_rx, .sync_rx }) |mode| {
        var device: SerialDevice = .{ .values = &.{ 0xff, 0x55, 0xa5 } };
        var source = device.source();
        var pin: SmartPin = .{ .rx_mode = .{ .emulated = &source } };
        pin.configure(mode);
        _ = try clockPin(&pin, .{ .dir = 1, .new_x = (8 << 16) | 7 });
        if (mode == .uart_rx) {
            for (0..30) |_| {
                _ = try clockPin(&pin, .{ .dir = 1 });
            }
        } else {
            for (0..4) |_| {
                _ = try clockPin(&pin, .{ .dir = 1, .b = .high });
                _ = try clockPin(&pin, .{ .dir = 1 });
            }
        }
        try std.testing.expectEqual(@as(usize, 1), device.reads);
        try std.testing.expect(!pin.registers.ready);
        pin.rx_mode = .{ .virtual = &source };
        for (0..8) |clock| {
            _ = try clockPin(&pin, .{ .dir = 1, .a = .high, .b = Signal.from(clock % 2 != 0) });
        }
        try std.testing.expectEqual(@as(usize, 2), device.reads);
        try std.testing.expectEqual(@as(u32, 0x55000000), pin.registers.result);
        pin.rx_mode = .{ .emulated = &source };
        _ = try clockPin(&pin, .{ .dir = 1, .ack = true, .b = .high });
        if (mode == .uart_rx) {
            // A complete new UART frame must elapse; the discarded prefix cannot shorten it.
            for (0..70) |_| {
                _ = try clockPin(&pin, .{ .dir = 1 });
                try std.testing.expect(!pin.registers.ready);
            }
            for (0..20) |_| {
                _ = try clockPin(&pin, .{ .dir = 1 });
            }
        } else {
            // The held-high clock is not an edge, and none of the old four bits survive.
            try std.testing.expectEqual(@as(usize, 2), device.reads);
            for (0..8) |bit| {
                _ = try clockPin(&pin, .{ .dir = 1 });
                _ = try clockPin(&pin, .{ .dir = 1, .b = .high });
                try std.testing.expectEqual(bit == 7, pin.registers.ready);
            }
        }
        try std.testing.expect(pin.registers.ready);
        try std.testing.expectEqual(@as(usize, 3), device.reads);
        try std.testing.expectEqual(@as(u32, 0xa5), pin.registers.result >> 24);
    }
}

test "virtual transmission abandons timed state and leaves no word to resume" {
    for ([_]Mode{ .uart_tx, .sync_tx }) |mode| {
        var device: SerialDevice = .{ .values = &.{} };
        const sink = device.sink();
        var pin: SmartPin = .{ .tx_mode = .{ .emulated = &sink } };
        pin.configure(mode);
        _ = try clockPin(&pin, .{ .dir = 1, .new_x = (8 << 16) | 39, .new_y = 0xff });
        for (0..3) |_| {
            _ = try clockPin(&pin, .{ .dir = 1 });
            _ = try clockPin(&pin, .{ .dir = 1, .b = .high });
        }
        pin.tx_mode = .{ .virtual = &sink };
        _ = try clockPin(&pin, .{ .dir = 1, .new_y = 0xa5, .a = .high, .b = .high });
        try std.testing.expectEqual(@as(usize, 1), device.writes);
        try std.testing.expectEqual(@as(u32, 0xa5), device.written[0]);
        for (0..20) |_| {
            _ = try clockPin(&pin, .{ .dir = 1, .a = .high, .b = .high });
        }
        pin.tx_mode = .{ .emulated = &sink };
        for (0..100) |_| {
            _ = try clockPin(&pin, .{ .dir = 1, .a = .high, .b = .high });
        }
        try std.testing.expectEqual(@as(usize, 1), device.writes);
        _ = try clockPin(&pin, .{ .dir = 1, .new_y = 0x3c, .b = .high });
        if (mode == .uart_tx) {
            for (0..100) |_| {
                _ = try clockPin(&pin, .{ .dir = 1 });
            }
        } else {
            for (0..8) |_| {
                _ = try clockPin(&pin, .{ .dir = 1 });
                _ = try clockPin(&pin, .{ .dir = 1, .b = .high });
            }
        }
        try std.testing.expectEqual(@as(usize, 2), device.writes);
        try std.testing.expectEqual(@as(u32, 0x3c), device.written[1]);
    }
}

test "USART exact and emulated receivers agree for every width and retain old shift bits" {
    for (1..33) |width| {
        const word: u32 = 0x89abcdef;
        var device: SerialDevice = .{ .values = &.{ word, word } };
        var source = device.source();
        var exact: SmartPin = .{};
        var emulated: SmartPin = .{ .rx_mode = .{ .emulated = &source } };
        exact.configure(.sync_rx);
        emulated.configure(.sync_rx);
        const x: u32 = @intCast(width - 1);
        _ = try clockPin(&exact, .{ .new_x = x });
        _ = try clockPin(&emulated, .{ .new_x = x });
        for (0..width * 2) |bit| {
            const a = Signal.from(word >> @as(u5, @intCast(bit % width)) & 1 != 0);
            const low: Inputs = .{ .dir = 1, .a = a, .b = .low, .ack = true };
            _ = try clockPin(&exact, low);
            _ = try clockPin(&emulated, low);
            const high: Inputs = .{ .dir = 1, .a = a, .b = .high };
            try std.testing.expectEqualDeep(try clockPin(&exact, high), try clockPin(&emulated, high));
            try std.testing.expectEqual((bit + 1) % width == 0, exact.registers.ready);
        }
        try std.testing.expectEqual(@as(usize, 2), device.reads);
        try std.testing.expectEqual(exact.registers.result, emulated.registers.result);
    }
}

test "USART sample selection, live length changes, and handoff align the whole source word" {
    var pin: SmartPin = .{};
    pin.configure(.sync_rx);
    _ = try clockPin(&pin, .{ .dir = 1, .new_x = 7, .a = .low });
    _ = try clockPin(&pin, .{ .dir = 1, .a = .high, .b = .high });
    try std.testing.expectEqual(@as(u32, 0), pin.logic.sync_rx.shift);
    _ = try clockPin(&pin, .{ .dir = 1, .a = .high, .b = .low, .new_x = 39 });
    _ = try clockPin(&pin, .{ .dir = 1, .a = .low, .b = .high });
    try std.testing.expectEqual(@as(u32, 0), pin.logic.sync_rx.shift);

    var device: SerialDevice = .{ .values = &.{0x89abcdef} };
    var source = device.source();
    pin.rx_mode = .{ .emulated = &source };
    // Two physical zero bits are retained; the source supplies bits 2 through 31.
    for (2..32) |bit| {
        const input: Inputs = .{ .dir = 1, .b = .low, .new_x = if (bit == 4) 31 else null };
        _ = try clockPin(&pin, input);
        _ = try clockPin(&pin, .{ .dir = 1, .b = .high });
    }
    try std.testing.expectEqual(@as(u32, 0x89abcdec), pin.registers.result);
    try std.testing.expectEqual(@as(usize, 1), device.reads);
    try std.testing.expect(pin.registers.ready);
}

test "UART receivers validate starts and agree with emulated data across all word widths" {
    for (1..33) |width| {
        const word: u32 = 0x89abcdef;
        var device: SerialDevice = .{ .values = &.{word} };
        var source = device.source();
        var exact: SmartPin = .{};
        var emulated: SmartPin = .{ .rx_mode = .{ .emulated = &source } };
        exact.configure(.uart_rx);
        emulated.configure(.uart_rx);
        const x: u32 = (8 << 16) | @as(u32, @intCast(width - 1));
        _ = try clockPin(&exact, .{ .new_x = x, .a = .high });
        _ = try clockPin(&emulated, .{ .new_x = x });
        for (0..(width + 2) * 8) |clock| {
            const data_bit = if (clock >= 9 and clock < (width + 1) * 8 + 1)
                Signal.from(word >> @as(u5, @intCast((clock - 9) / 8)) & 1 != 0)
            else if (clock >= 1 and clock < 9)
                Signal.low
            else
                Signal.high;
            const actual = try clockPin(&exact, .{ .dir = 1, .a = data_bit });
            const synthesized = try clockPin(&emulated, .{ .dir = 1 });
            try std.testing.expectEqualDeep(actual, synthesized);
        }
        try std.testing.expectEqual((word & wordMask(x)) << @as(u5, @intCast(32 - width)), exact.registers.result);
        try std.testing.expect(exact.registers.ready);
    }
    var pin: SmartPin = .{};
    pin.configure(.uart_rx);
    _ = try clockPin(&pin, .{ .dir = 1, .new_x = (8 << 16) | 7, .a = .high });
    _ = try clockPin(&pin, .{ .dir = 1, .a = .low });
    for (0..100) |_| {
        _ = try clockPin(&pin, .{ .dir = 1, .a = .high });
    }
    try std.testing.expect(!pin.registers.ready);
}

test "entering virtual aborts partial RX and TX but retains completed RX until acknowledgement" {
    var device: SerialDevice = .{ .values = &.{0x12345678} };
    var source = device.source();
    const sink = device.sink();
    for ([_]Mode{ .sync_rx, .uart_rx }) |mode| {
        var pin: SmartPin = .{};
        pin.configure(mode);
        pin.registers.result = 0x89abcdef;
        pin.registers.ready = true;
        pin.rx_mode = .{ .virtual = &source };
        _ = try clockPin(&pin, .{ .dir = 1 });
        try std.testing.expectEqual(@as(u32, 0x89abcdef), pin.registers.result);
        try std.testing.expectEqual(@as(usize, 0), device.reads);
        pin.rx_mode = .exact;
        _ = try clockPin(&pin, .{ .dir = 1, .a = .high });
        try std.testing.expect(pin.registers.ready);
    }
    for ([_]Mode{ .sync_tx, .uart_tx }) |mode| {
        var pin: SmartPin = .{ .tx_mode = .{ .emulated = &sink } };
        pin.configure(mode);
        _ = try clockPin(&pin, .{ .new_x = (8 << 16) | 31, .new_y = 0xffffffff });
        _ = try clockPin(&pin, .{ .dir = 1, .new_y = 0xffffffff });
        _ = try clockPin(&pin, .{ .dir = 1 });
        pin.tx_mode = .{ .virtual = &sink };
        _ = try clockPin(&pin, .{ .dir = 1 });
        try std.testing.expectEqual(@as(usize, 0), device.writes);
    }
}

test "emulated TX generates full UART waveforms and completes into the current sink" {
    var first: SerialDevice = .{ .values = &.{} };
    var second: SerialDevice = .{ .values = &.{} };
    const first_sink = first.sink();
    const second_sink = second.sink();
    var pin: SmartPin = .{ .tx_mode = .{ .emulated = &first_sink } };
    pin.configure(.uart_tx);
    _ = try clockPin(&pin, .{ .new_x = (8 << 16) | 7 });
    _ = try clockPin(&pin, .{ .dir = 1, .new_y = 0xa5 });
    for (0..81) |clock| {
        const state = try clockPin(&pin, .{ .dir = 1 });
        const expected = if (clock == 0)
            Signal.high
        else if (clock < 9)
            Signal.low
        else if (clock < 73)
            Signal.from(@as(u32, 0xa5) >> @as(u5, @intCast((clock - 9) / 8)) & 1 != 0)
        else
            Signal.high;
        try std.testing.expectEqual(expected, state.smart_out);
        if (clock == 40) {
            pin.tx_mode = .{ .emulated = &second_sink };
        }
    }
    try std.testing.expectEqual(@as(usize, 0), first.writes);
    try std.testing.expectEqual(@as(usize, 1), second.writes);
    try std.testing.expectEqual(@as(u32, 0xa5), second.written[0]);
}

test "UART in-flight handoffs retain staged 32-bit data, timing and physical prefix" {
    var device: SerialDevice = .{ .values = &.{0x89abcdef} };
    var source = device.source();
    var pin: SmartPin = .{};
    pin.configure(.uart_rx);
    _ = try clockPin(&pin, .{ .new_x = (8 << 16) | 7 });
    for (0..280) |clock| {
        if (clock == 31 or clock == 96) {
            pin.rx_mode = .{ .emulated = &source };
        } else if (clock == 80) {
            pin.rx_mode = .exact;
        }
        const word: u32 = if (clock < 31) 1 else 0x89abcdef;
        const a = if (clock >= 9 and clock < 265)
            Signal.from(word >> @as(u5, @intCast((clock - 9) / 8)) & 1 != 0)
        else if (clock >= 1 and clock < 9)
            Signal.low
        else
            Signal.high;
        _ = try clockPin(&pin, .{
            .dir = 1,
            .a = a,
            .new_x = if (clock == 31) (8 << 16) | 31 else null,
        });
    }
    try std.testing.expectEqual(@as(usize, 1), device.reads);
    try std.testing.expectEqual(@as(u32, 0x89abcde9), pin.registers.result);
    try std.testing.expect(pin.registers.ready);
}

test "shortened USART words stall past 64 edges and reset recovers without clearing Z" {
    var pin: SmartPin = .{};
    pin.configure(.sync_rx);
    pin.registers.result = 0x12345678;
    for (0..80) |bit| {
        _ = try clockPin(&pin, .{ .dir = 1, .a = .high, .new_x = if (bit == 0) 7 else if (bit == 5) 1 else null });
        _ = try clockPin(&pin, .{ .dir = 1, .a = .high, .b = .high });
    }
    try std.testing.expect(!pin.registers.ready);
    try std.testing.expectEqual(@as(u32, 0x12345678), pin.registers.result);
    _ = try clockPin(&pin, .{ .a = .high });
    try std.testing.expectEqual(@as(u32, 0x12345678), pin.registers.result);
    for (0..2) |_| {
        _ = try clockPin(&pin, .{ .dir = 1, .a = .high });
        _ = try clockPin(&pin, .{ .dir = 1, .a = .high, .b = .high });
    }
    try std.testing.expectEqual(@as(u32, 0xffffffff), pin.registers.result);
    try std.testing.expect(pin.registers.ready);
}

test "emulated USART clocks, source starvation and endpoint replacement preserve partial words" {
    var empty: SerialDevice = .{ .values = &.{} };
    var available: SerialDevice = .{ .values = &.{ 0x89abcdef, 0x12345678 } };
    var later: SerialDevice = .{ .values = &.{0xffffffff} };
    var empty_source = empty.source();
    var available_source = available.source();
    var later_source = later.source();
    var pin: SmartPin = .{ .rx_mode = .{ .emulated = &empty_source } };
    pin.configure(.sync_rx);
    _ = try clockPin(&pin, .{ .dir = 1, .new_x = 31 });
    for (0..8) |_| {
        _ = try clockPin(&pin, .{ .dir = 1 });
        _ = try clockPin(&pin, .{ .dir = 1, .b = .high });
    }
    try std.testing.expectEqual(@as(u6, 0), pin.logic.sync_rx.count);
    try std.testing.expect(!pin.registers.ready);
    pin.rx_mode = .{ .emulated = &available_source };
    for (0..32) |bit| {
        if (bit == 4) {
            pin.rx_mode = .{ .emulated = &later_source };
        }
        _ = try clockPin(&pin, .{ .dir = 1 });
        _ = try clockPin(&pin, .{ .dir = 1, .b = .high });
    }
    try std.testing.expectEqual(@as(u32, 0x89abcdef), pin.registers.result);
    try std.testing.expectEqual(@as(usize, 1), available.reads);
    try std.testing.expectEqual(@as(usize, 0), later.reads);
    // Timed reception overwrites unread Z. Virtual reception retains it until acknowledgement.
    for (0..32) |_| {
        _ = try clockPin(&pin, .{ .dir = 1 });
        _ = try clockPin(&pin, .{ .dir = 1, .b = .high });
    }
    try std.testing.expectEqual(@as(u32, 0xffffffff), pin.registers.result);
    pin.rx_mode = .{ .virtual = &available_source };
    _ = try clockPin(&pin, .{ .dir = 1 });
    try std.testing.expectEqual(@as(usize, 1), available.reads);
    _ = try clockPin(&pin, .{ .dir = 1, .ack = true });
    try std.testing.expectEqual(@as(u32, 0x12345678), pin.registers.result);
}

test "USART TX start-stop buffers after the first edge and reports completed words" {
    var device: SerialDevice = .{ .values = &.{} };
    const sink = device.sink();
    var pin: SmartPin = .{ .tx_mode = .{ .emulated = &sink } };
    pin.configure(.sync_tx);
    _ = try clockPin(&pin, .{ .new_x = 35, .new_y = 0x12345678 });
    var state = try clockPin(&pin, .{ .dir = 1, .new_y = 0x89abcdef });
    try std.testing.expectEqual(Signal.low, state.smart_out);
    state = try clockPin(&pin, .{ .dir = 1, .b = .high });
    try std.testing.expectEqual(Signal.high, state.smart_out);
    _ = try clockPin(&pin, .{ .dir = 1, .new_y = 0x76543210, .ack = true });
    for (0..3) |_| {
        _ = try clockPin(&pin, .{ .dir = 1, .b = .high });
        _ = try clockPin(&pin, .{ .dir = 1 });
    }
    try std.testing.expectEqual(@as(usize, 1), device.writes);
    try std.testing.expectEqual(@as(u32, 15), device.written[0]);
    try std.testing.expect(pin.registers.ready);
    for (0..4) |_| {
        _ = try clockPin(&pin, .{ .dir = 1, .b = .high });
        state = try clockPin(&pin, .{ .dir = 1 });
    }
    try std.testing.expectEqual(@as(usize, 2), device.writes);
    try std.testing.expectEqual(@as(u32, 0), device.written[1]);
    _ = try clockPin(&pin, .{});
    try std.testing.expectEqual(@as(usize, 2), device.writes);
}

test "WRPIN retains host modes and endpoint errors propagate through all serial engines" {
    const Broken = struct {
        /// Exercise callback receive failures without converting them to idle data.
        fn read(_: *anyopaque) anyerror!?u32 {
            return error.TestSourceFailure;
        }
        /// Exercise completion failures without discarding them.
        fn write(_: *anyopaque, _: u32) anyerror!void {
            return error.TestSinkFailure;
        }
    };
    var context: u8 = 0;
    var source: DataSource = .{ .callback = .{ .context = &context, .read = Broken.read } };
    const sink: DataSink = .{ .callback = .{ .context = &context, .write = Broken.write } };
    var pin: SmartPin = .{ .rx_mode = .{ .emulated = &source }, .tx_mode = .{ .virtual = &sink } };
    pin.configure(.uart_rx);
    try std.testing.expectError(error.TestSourceFailure, clockPin(&pin, .{ .dir = 1, .new_x = (8 << 16) | 7 }));
    pin.configure(.sync_rx);
    _ = try clockPin(&pin, .{ .dir = 1 });
    try std.testing.expectError(error.TestSourceFailure, clockPin(&pin, .{ .dir = 1, .b = .high }));
    try std.testing.expect(pin.rx_mode.emulated == &source);
    for ([_]Mode{ .uart_tx, .sync_tx }) |mode| {
        pin.configure(mode);
        try std.testing.expectError(error.TestSinkFailure, clockPin(&pin, .{ .dir = 1, .new_y = 0x12345678 }));
        _ = try clockPin(&pin, .{ .dir = 1 });
        try std.testing.expect(pin.tx_mode.virtual == &sink);
    }
}

test "buffered UART preserves fractional baud phase across odd-length frames" {
    var device: SerialDevice = .{ .values = &.{} };
    const sink = device.sink();
    var pin: SmartPin = .{ .tx_mode = .{ .emulated = &sink } };
    pin.configure(.uart_tx);
    _ = try clockPin(&pin, .{ .new_x = (7 << 16) | 0x4000 });
    _ = try clockPin(&pin, .{ .dir = 1, .new_y = 0 });
    for (0..45) |clock| {
        _ = try clockPin(&pin, .{ .dir = 1, .new_y = if (clock == 1) 1 else null });
        const expected: usize = @as(usize, @intFromBool(clock >= 21)) + @as(usize, @intFromBool(clock >= 43));
        try std.testing.expectEqual(expected, device.writes);
    }
    try std.testing.expectEqualSlices(u32, &.{ 0, 1 }, device.written[0..device.writes]);
}

test "returning from virtual USART waits for a new B edge" {
    var device: SerialDevice = .{ .values = &.{ 0x89abcdef, 0x12345678 } };
    var source = device.source();
    var pin: SmartPin = .{ .rx_mode = .{ .virtual = &source } };
    pin.configure(.sync_rx);
    _ = try clockPin(&pin, .{ .dir = 1, .new_x = 7, .b = .high });
    pin.rx_mode = .{ .emulated = &source };
    for (0..8) |_| {
        _ = try clockPin(&pin, .{ .dir = 1, .b = .high });
    }
    try std.testing.expectEqual(@as(usize, 1), device.reads);
    try std.testing.expectEqual(@as(u6, 0), pin.logic.sync_rx.count);
    _ = try clockPin(&pin, .{ .dir = 1, .b = .low });
    _ = try clockPin(&pin, .{ .dir = 1, .b = .high });
    try std.testing.expectEqual(@as(usize, 2), device.reads);
    try std.testing.expectEqual(@as(u6, 1), pin.logic.sync_rx.count);
    // A real low-to-high transition on the first timed step must not be suppressed either.
    pin.rx_mode = .{ .virtual = &source };
    _ = try clockPin(&pin, .{ .dir = 1, .a = .high });
    pin.rx_mode = .exact;
    _ = try clockPin(&pin, .{ .dir = 1, .a = .high, .b = .high });
    try std.testing.expectEqual(@as(u6, 1), pin.logic.sync_rx.count);
    try std.testing.expectEqual(@as(u32, 0x80000000), pin.logic.sync_rx.shift);
}

test "virtual transmitters do not invent words when enabled without WYPIN" {
    for ([_]Mode{ .uart_tx, .sync_tx }) |mode| {
        var device: SerialDevice = .{ .values = &.{} };
        const sink = device.sink();
        var pin: SmartPin = .{ .tx_mode = .{ .virtual = &sink } };
        pin.configure(mode);
        _ = try clockPin(&pin, .{ .new_x = 7 });
        for (0..8) |_| {
            _ = try clockPin(&pin, .{ .dir = 1 });
        }
        try std.testing.expectEqual(@as(usize, 0), device.writes);
        _ = try clockPin(&pin, .{ .dir = 1, .new_y = 0xa5 });
        _ = try clockPin(&pin, .{ .dir = 1 });
        try std.testing.expectEqual(@as(usize, 1), device.writes);
        _ = try clockPin(&pin, .{});
        _ = try clockPin(&pin, .{ .dir = 1 });
        try std.testing.expectEqual(@as(usize, 1), device.writes);
    }
}

test "emulated UART keeps fractional frame cadence and arms while its source is empty" {
    var device: SerialDevice = .{ .values = &.{ 0, 1, 0 } };
    var source = device.source();
    var pin: SmartPin = .{ .rx_mode = .{ .emulated = &source } };
    pin.configure(.uart_rx);
    _ = try clockPin(&pin, .{ .new_x = (7 << 16) | 0x4000 });
    var completions: [3]usize = undefined;
    var count: usize = 0;
    for (0..68) |clock| {
        const state = try clockPin(&pin, .{ .dir = 1, .ack = true });
        if (state.in != 0) {
            completions[count] = clock;
            count += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 3), count);
    try std.testing.expectEqualSlices(usize, &.{ 21, 42, 64 }, &completions);
    device.values = &.{ 0, 1, 0, 1 };
    try std.testing.expect(pin.logic.uart_rx.phase == .start);
    _ = try clockPin(&pin, .{ .dir = 1 });
    try std.testing.expect(pin.logic.uart_rx.phase == .validate);
    try std.testing.expectEqual(@as(usize, 4), device.reads);
}

test "UART width can extend after the nominal last sample and before capture" {
    var device: SerialDevice = .{ .values = &.{0x89ab12ef} };
    var source = device.source();
    var pin: SmartPin = .{ .rx_mode = .{ .emulated = &source } };
    pin.configure(.uart_rx);
    _ = try clockPin(&pin, .{ .new_x = (8 << 16) | 7 });
    for (0..145) |clock| {
        if (clock == 72) {
            try std.testing.expect(pin.logic.uart_rx.phase == .finish);
        }
        _ = try clockPin(&pin, .{ .dir = 1, .new_x = if (clock == 72) (8 << 16) | 15 else null });
    }
    try std.testing.expectEqual(@as(u32, 0x12ef0000), pin.registers.result);
    try std.testing.expect(pin.registers.ready);
    try std.testing.expectEqual(@as(usize, 1), device.reads);
}

test "step updates shared registers and latches repository outputs" {
    var pin: SmartPin = .{};
    pin.configure(.dac_noise);
    const written = try clockPin(&pin, .{ .dir = 1, .new_x = 0x89abcdef, .new_y = 123, .clock = 10 });
    try std.testing.expectEqual(@as(u32, 0x89abcdef), pin.registers.x);
    try std.testing.expectEqual(@as(u32, 123), pin.registers.y);
    try std.testing.expectEqual(written.y, pin.registers.result);
    try std.testing.expectEqual(written.in != 0, pin.registers.ready);
    try std.testing.expect(pin.registers.enabled and !pin.registers.ready);
    const complete = try clockPin(&pin, .{ .dir = 1, .clock = 11 });
    try std.testing.expect(complete.in != 0 and pin.registers.ready);
    _ = try clockPin(&pin, .{ .dir = 1, .ack = true, .clock = 12 });
    try std.testing.expect(!pin.registers.ready);
    _ = try clockPin(&pin, .{ .new_x = 456, .clock = 13 });
    try std.testing.expectEqual(@as(u32, 456), pin.registers.x);
    try std.testing.expectEqual(@as(u32, 0x89abcdef), pin.registers.result);
    try std.testing.expect(!pin.registers.enabled);
}

test "DAC configuration owns repository reset and running sample-period retention" {
    for ([_]Mode{ .dac_noise, .dac_dither_rnd, .dac_dither_pwm }) |mode| {
        var pin: SmartPin = .{};
        pin.configure(mode);
        _ = try clockPin(&pin, .{ .dir = 1, .new_x = 123 });
        pin.configure(mode);
        const reset = try clockPin(&pin, .{ .dir = 1 });
        try std.testing.expectEqual(@as(u1, 0), reset.in);
        try std.testing.expect(reset.dac == null);

        pin.configure(mode);
        _ = try clockPin(&pin, .{ .dac_mode = true, .new_x = 4, .new_y = 0x8000 });
        for (0..2) |_| {
            _ = try clockPin(&pin, .{ .dac_mode = true, .dir = 1, .a = .high });
        }
        try std.testing.expect(!pin.registers.ready);
        pin.configure(mode);
        for (0..2) |_| {
            _ = try clockPin(&pin, .{ .dac_mode = true, .dir = 1, .a = .high });
        }
        try std.testing.expect(pin.registers.ready);
        try std.testing.expectEqual(@as(u32, 4), pin.registers.result);

        pin.configure(mode);
        const restored = try clockPin(&pin, .{ .dir = 1, .new_x = 456 });
        try std.testing.expect(restored.dac == null);
        try std.testing.expectEqual(@as(u32, 456), restored.y);
    }
}

test "configuration replaces only logic and preserves GPIO transport" {
    var pin: SmartPin = .{};
    _ = try clockPin(&pin, .{ .dir = 1, .out = 1, .a = .high, .new_x = 12, .new_y = 34 });
    const registers = pin.registers;
    var usb: Usb = .{};
    pin.connectUsb(&usb.ports[0]);
    pin.configure(.usb);
    try std.testing.expectEqualDeep(registers, pin.registers);
    _ = try clockPin(&pin, .{ .dir = 1, .clock = 1 });
    pin.configure(.gpio);
    const state = try clockPin(&pin, .{ .dir = 1, .out = 1, .a = .high, .clock = 2 });
    try std.testing.expectEqual(Signal.high, state.smart_out);
    try std.testing.expectEqual(@as(u1, 1), state.in);
    const registered = try clockPin(&pin, .{ .dir = 1, .out = 1, .a = .high, .clock = 3 });
    try std.testing.expectEqual(Signal.low, registered.smart_out);
    const settled = try clockPin(&pin, .{ .dir = 1, .out = 1, .a = .high, .clock = 4 });
    try std.testing.expectEqual(Signal.high, settled.smart_out);
}

test "digital engines ignore fast serial mode and absolute clock rollover" {
    for (std.enums.values(Mode)) |mode| {
        if (!mode.digitalLogic()) {
            continue;
        }
        var ordinary: SmartPin = .{};
        ordinary.configure(mode);
        _ = try clockPin(&ordinary, .{ .new_x = 0x70003, .new_y = 3 });
        var accelerated = ordinary;
        for (0..256) |clock| {
            const input: Inputs = .{
                .dir = @intFromBool(clock < 240),
                .a = Signal.from(clock % 11 < 5),
                .b = Signal.from(clock % 17 < 8),
                .ack = clock % 13 == 0,
                .clock = clock,
            };
            var fast = input;
            fast.clock = std.math.maxInt(u64) -% 20 +% clock;
            accelerated.rx_mode = .exact;
            accelerated.tx_mode = .{ .virtual = &discard_sink };
            try std.testing.expectEqualDeep(try clockPin(&ordinary, input), try clockPin(&accelerated, fast));
        }
    }
}

test "continuous count wraps and held low inputs do not invent rising edges" {
    var pin: SmartPin = .{};
    pin.configure(.count_rises);
    _ = try clockPin(&pin, .{});
    pin.logic.count_rises.accumulator = 0xffffffff;
    var state = try clockPin(&pin, .{ .dir = 1, .a = .high });
    try std.testing.expectEqual(@as(u32, 0xffffffff), state.y);
    try std.testing.expect(state.flag);
    state = try clockPin(&pin, .{ .dir = 1, .a = .high });
    try std.testing.expectEqual(@as(u32, 0), state.y);
    try std.testing.expect(!state.flag);
    _ = try clockPin(&pin, .{ .dir = 1, .a = .low });
    state = try clockPin(&pin, .{ .dir = 1, .a = .low });
    try std.testing.expectEqual(@as(u32, 0), state.y);
}

test "duration saturation preserves the previous-state carry instead of Z's sign" {
    var pin: SmartPin = .{};
    pin.configure(.state_ticks);
    pin.registers.enabled = true;
    pin.logic.state_ticks.ticks = 0x7fffffff;
    _ = try clockPin(&pin, .{ .dir = 1, .a = .low });
    _ = try clockPin(&pin, .{ .dir = 1, .a = .low });
    const state = try clockPin(&pin, .{ .dir = 1, .a = .high });
    try std.testing.expectEqual(@as(u32, 0x80000000), state.y);
    try std.testing.expect(!state.flag);
    try std.testing.expectEqual(@as(u1, 1), state.in);
}

test "zero minimum window and fixed-period count encode the full 32-bit range" {
    var pin: SmartPin = .{};
    pin.configure(.counter_ticks);
    pin.registers.enabled = true;
    pin.logic.counter_ticks = .{
        .active = true,
        .started = true,
        .elapsed = 0xffffffff,
        .accumulated = 0x7fffffff,
    };
    var state = try clockPin(&pin, .{ .dir = 1, .b = .high });
    try std.testing.expectEqual(@as(u32, 0x80000000), state.y);
    try std.testing.expectEqual(@as(u1, 1), state.in);
    pin.configure(.periods_ticks);
    pin.logic.periods_ticks = .{
        .active = true,
        .started = true,
        .periods = 0xffffffff,
        .accumulated = 123,
    };
    state = try clockPin(&pin, .{ .dir = 1, .b = .high });
    try std.testing.expectEqual(@as(u32, 124), state.y);
    try std.testing.expectEqual(@as(u1, 1), state.in);
}

test "scope FIR normalization and smart DAC clipping preserve measured full scales" {
    for (0..4) |filter| {
        try std.testing.expectEqual(@as(u8, 255), Scope.sample(std.math.maxInt(u128), @enumFromInt(filter)));
        try std.testing.expectEqual(@as(u8, 0), Scope.sample(0, @enumFromInt(filter)));
    }
    var pin: SmartPin = .{};
    pin.configure(.dac_dither_pwm);
    _ = try clockPin(&pin, .{ .dac_mode = true, .new_x = 256, .new_y = 0xffff });
    for (0..512) |_| {
        const state = try clockPin(&pin, .{ .dac_mode = true, .dir = 1, .a = .high });
        try std.testing.expectEqual(@as(?u8, 255), state.dac);
    }
    try std.testing.expectEqual(@as(u32, 256), pin.registers.result);
}

test "USB decodes independent two-board wire captures at held-out phases and both speed polarities" {
    const Vector = struct {
        name: []const u8,
        rate: u16,
        speed: bool,
        wire: []const [2]u16,
        bytes: []const u16,
        final: u16,
    };
    const parsed = try std.json.parseFromSlice([]const Vector, std.testing.allocator, @embedFile("usb-vectors"), .{});
    defer parsed.deinit();

    for (parsed.value) |vector| {
        const speed: Usb.State.Speed = if (vector.speed)
            .full
        else
            .low;

        for ([_]usize{ 0, 1, 3, 7, 11 }) |phase| {
            var usb: Usb.State = .{};
            var event: usize = 0;
            for (vector.wire, 0..) |run, index| {
                const extra = if (index == 0)
                    phase
                else
                    0;
                const duration = run[0] + extra;
                const value = Usb.State.line(run[1] & 1 != 0, run[1] & 2 != 0, speed);
                for (0..duration) |_| {
                    usb.receive(value, vector.rate);
                    if (event < vector.bytes.len and @as(u16, @bitCast(usb.status)) & 0xff80 == vector.bytes[event]) {
                        event += 1;
                    }
                }
            }
            const last = vector.wire[vector.wire.len - 1][1];
            for (0..8192) |_| {
                usb.receive(Usb.State.line(last & 1 != 0, last & 2 != 0, speed), vector.rate);
            }
            std.testing.expectEqual(vector.final, @as(u16, @bitCast(usb.status))) catch |err| {
                std.debug.print("USB captured vector {s}, phase {d}\n", .{ vector.name, phase });
                return err;
            };
            try std.testing.expectEqual(vector.bytes.len, event);
        }
    }
}

test "USB upper acknowledgement is latched independently of lower reset and same-mode configuration" {
    var lower: SmartPin = .{};
    var upper: SmartPin = .{};
    var usb: Usb = .{};
    lower.connectUsb(&usb.ports[0]);
    upper.connectUsb(&usb.ports[1]);
    lower.configure(.usb);
    upper.configure(.usb);
    var input = [2]Inputs{ .{ .dir = 1, .new_x = 4096, .new_y = 3, .a = .high }, .{ .dir = 1 } };
    for (0..256) |_| {
        const states = clockUsb(&lower, &upper, &usb, input);
        _ = try states[0];
        _ = try states[1];
        input[0].new_x = null;
        input[0].new_y = null;
    }
    try std.testing.expect(upper.registers.ready);
    try std.testing.expectEqual(@as(u32, 9), lower.registers.result);
    lower.configure(.usb);
    upper.configure(.usb);
    input[1].ack = true;
    for (0..128) |_| {
        const states = clockUsb(&lower, &upper, &usb, input);
        _ = try states[0];
        _ = try states[1];
        input[1].ack = false;
        try std.testing.expect(!upper.registers.ready);
    }
    input[0].new_y = 2;
    for (0..64) |_| {
        const states = clockUsb(&lower, &upper, &usb, input);
        _ = try states[0];
        _ = try states[1];
        input[0].new_y = null;
    }
    try std.testing.expect(upper.registers.ready);
    input[0].dir = 0;
    for (0..16) |_| {
        const states = clockUsb(&lower, &upper, &usb, input);
        _ = try states[0];
        _ = try states[1];
    }
    try std.testing.expect(upper.registers.ready);
    try std.testing.expectEqual(@as(u32, 0), lower.registers.result);
    input[1].dir = 0;
    const states = clockUsb(&lower, &upper, &usb, input);
    _ = try states[0];
    _ = try states[1];
    try std.testing.expect(!upper.registers.ready);
}

test "USB serializer matches independently captured wire symbols including cross-byte stuffing" {
    const Vector = struct {
        name: []const u8,
        speed: bool,
        data: []const u8,
        symbols: []const u2,
    };
    const parsed = try std.json.parseFromSlice(
        []const Vector,
        std.testing.allocator,
        @embedFile("usb-transmit-vectors"),
        .{},
    );
    defer parsed.deinit();

    for (parsed.value) |vector| {
        const speed: Usb.State.Speed = if (vector.speed)
            .full
        else
            .low;

        var usb: Usb.State = .{ .pending = 0x80 };
        var upper: Registers = .{ .enabled = true };
        var next: usize = 0;
        for (vector.symbols) |symbol| {
            usb.transmit(&upper);
            const observed = Usb.State.line(symbol & 1 != 0, symbol & 2 != 0, speed);
            std.testing.expectEqual(observed, usb.drive.?) catch |err| {
                std.debug.print("USB transmitter vector {s}\n", .{vector.name});
                return err;
            };
            if (upper.ready) {
                upper.ready = false;
                if (next < vector.data.len) {
                    usb.pending = vector.data[next];
                    next += 1;
                }
            }
        }
        try std.testing.expectEqual(vector.data.len, next);
        usb.transmit(&upper);
        try std.testing.expectEqual(@as(?Usb.State.Line, null), usb.drive);
    }
}
