//! Smart engines only. WRPIN routing and pad configuration belong to IO.zig.
const std = @import("std");
const p2 = @import("p2");
const signal = @import("signal.zig");
pub const Mode = p2.types.SmartPinMode;
const Signal = signal.Signal;

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
                if (bytes.len == 0) break :blk null;
                const byte = bytes[0];
                reader.toss(1);
                break :blk byte;
            },
            .callback => |cb| cb.read(cb.context),
        };
    }
};

pub const DataSink = union(enum) {
    writer: *std.Io.Writer,
    callback: struct { context: *anyopaque, write: *const fn (*anyopaque, u32) anyerror!void },

    /// Deliver a word to the endpoint, truncating writer-backed words to a byte.
    pub fn write(sink: *DataSink, value: u32) anyerror!void {
        switch (sink.*) {
            .writer => |writer| try writer.writeByte(@truncate(value)),
            .callback => |cb| try cb.write(cb.context, value),
        }
    }
};

/// Registers and endpoints shared by every smart-pin mode.
pub const Registers = struct {
    x: u32 = 0,
    y: u32 = 0,
    result: u32 = 0,
    enabled: bool = false,
    ready: bool = false,
    source: ?*DataSource = null,
    sink: ?*DataSink = null,
};

/// UART word width and frame duration decoded from X; no routing state is involved.
pub const SerialTiming = struct {
    bits: u6,
    frame_clocks: u64,

    /// Decode the fixed-point baud period and the start/data/stop frame length.
    pub fn init(x: u32) SerialTiming {
        const width: u6 = @as(u6, @intCast(x & 31)) + 1;
        const period: u64 = if (x >> 26 == 0) x & 0xffff_fc00 else x & 0xffff_0000;
        return .{ .bits = width, .frame_clocks = (period * (width + @as(u64, 2)) + 0xffff) >> 16 };
    }
};

/// Fixed inputs for one smart-pin update in a simulation frame.
/// IO combines commands and routes input signals before invoking SmartPin.step.
pub const Inputs = struct {
    /// OR of the cogs' DIR bits. One enables the smart logic; a falling edge resets it.
    dir: u1 = 0,
    /// OR of the cogs' OUT bits, before the physical pad's drive gating and delays.
    out: u1 = 0,
    /// SmartA after input selection, inversion, A/B logic and applicable sampling delays.
    a: Signal = .z,
    /// SmartB after input selection, inversion and applicable sampling delays.
    b: Signal = .z,
    /// WXPIN value delivered this frame; null means no write, including no implicit ACK.
    /// SmartPin.step stores a present value in Registers.x before running the logic.
    new_x: ?u32 = null,
    /// WYPIN value delivered this frame; null means no write, including no implicit ACK.
    /// SmartPin.step stores a present value in Registers.y before running the logic.
    new_y: ?u32 = null,
    /// Acknowledgement delivered by AKPIN, RDPIN or WRPIN this frame.
    /// Clears ready before running the logic; X/Y writes also acknowledge implicitly.
    ack: bool = false,
    /// Current wrapping Hub counter, in system clocks, used for transfer deadlines.
    clock: u64 = 0,
    /// Falling DIR edge derived by SmartPin.step from Registers.enabled.
    /// Caller-supplied values are overwritten before dispatch to the active logic.
    reset: bool = false,
};

/// Signals and scheduler state produced by one smart-pin update.
/// SmartPin.step latches y into Registers.result and IN into Registers.ready.
/// IO publishes every pin's outputs together, then applies pad routing and register delays.
pub const Outputs = struct {
    /// Logic drive before pad routing, inversion and electrical resolution.
    /// Serial modes may return x because endpoint transfers bypass waveform encoding.
    smart_out: Signal = .zero,
    /// Signal sent toward the cog IN registers: routed SmartA for GPIO, ready for smart modes.
    in: u1 = 0,
    /// Current result (Z) returned through RDPIN/RQPIN, distinct from the WYPIN register.
    y: u32 = 0,
    /// Modal carry flag returned through RDPIN/RQPIN: UART TX busy, otherwise result bit 31.
    flag: bool = false,
    /// Finite work is underway, such as a completion deadline or enabled serial transfer.
    /// Sources waiting for new input are handled separately by IO.pending.
    scheduled: bool = false,
};

/// Endpoint callbacks may report their own errors, in addition to unsupported modes.
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

/// Reset serial result state on a falling DIR edge.
fn serialOutputs(regs: *const Registers, inputs: Inputs) Outputs {
    var state = outputs(regs, inputs);
    if (inputs.reset) {
        state.y = 0;
        state.flag = false;
    }
    return state;
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

/// Compare wrapping clock deadlines within half the counter range.
fn reached(clock: u64, deadline: u64) bool {
    return clock -% deadline < (@as(u64, 1) << 63);
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
    raise_at: ?u64 = null,

    /// Retain Z through DIR reset and publish X with its measured completion delay.
    fn step(logic: *Repository, regs: *Registers, inputs: Inputs) SmartPinError!Outputs {
        var state = outputs(regs, inputs);
        if (inputs.ack or inputs.new_x != null or inputs.new_y != null or inputs.reset) logic.raise_at = null;
        if (inputs.new_x != null and regs.enabled) {
            state.y = regs.x;
            state.flag = state.y >> 31 != 0;
            logic.raise_at = inputs.clock +% 1;
        }
        if (logic.raise_at) |at| if (reached(inputs.clock, at)) {
            state.in = @intFromBool(regs.enabled);
            logic.raise_at = null;
        };
        state.scheduled = logic.raise_at != null;
        return state;
    }
};

pub const UartTx = struct {
    buffer: ?u32 = null,
    shifter: ?u32 = null,
    end: u64 = 0,

    /// Accept Y writes, abort on DIR reset, and advance the buffered UART transmitter.
    fn step(logic: *UartTx, regs: *Registers, inputs: Inputs) SmartPinError!Outputs {
        var state = serialOutputs(regs, inputs);
        if (inputs.reset) {
            logic.buffer = null;
            logic.shifter = null;
        }
        if (inputs.new_y != null and regs.enabled) logic.buffer = regs.y & wordMask(regs.x);
        if (regs.enabled) {
            if (logic.shifter != null and reached(inputs.clock, logic.end)) {
                if (regs.sink) |sink| try sink.write(logic.shifter.?);
                logic.shifter = null;
            }
            const timing = SerialTiming.init(regs.x);
            if (logic.shifter == null and timing.frame_clocks != 0) if (logic.buffer) |word| {
                logic.shifter = word;
                logic.buffer = null;
                logic.end = inputs.clock +% timing.frame_clocks;
                state.in = 1;
            };
        }
        state.flag = logic.buffer != null or logic.shifter != null;
        state.scheduled = regs.enabled and state.flag;
        state.smart_out = if (state.scheduled) .x else .one;
        return state;
    }
};

pub const UsartTx = struct {
    buffer: ?u32 = null,

    /// Prime Y even in reset, then deliver a whole word on clock service when enabled.
    fn step(logic: *UsartTx, regs: *Registers, inputs: Inputs) SmartPinError!Outputs {
        var state = serialOutputs(regs, inputs);
        if (inputs.reset) logic.buffer = null;
        if (inputs.new_y != null) logic.buffer = regs.y & wordMask(regs.x);
        if (regs.enabled) if (logic.buffer) |word| {
            if (regs.sink) |sink| try sink.write(word);
            logic.buffer = null;
            state.in = 1;
        };
        state.scheduled = regs.enabled and logic.buffer != null;
        state.smart_out = if (regs.enabled) .x else .zero;
        return state;
    }
};

pub const UartRx = struct {
    shifter: ?u32 = null,
    end: u64 = 0,

    /// Receive buffered source words with UART frame timing, aborting a frame on DIR reset.
    fn step(logic: *UartRx, regs: *Registers, inputs: Inputs) SmartPinError!Outputs {
        var state = serialOutputs(regs, inputs);
        if (inputs.reset) logic.shifter = null;
        if (regs.enabled) {
            if (logic.shifter != null and reached(inputs.clock, logic.end)) {
                receive(&state, regs.x, logic.shifter.?);
                logic.shifter = null;
            }
            const timing = SerialTiming.init(regs.x);
            if (logic.shifter == null and timing.frame_clocks != 0) if (regs.source) |source| {
                if (try source.read()) |word| {
                    logic.shifter = word;
                    logic.end = inputs.clock +% timing.frame_clocks;
                }
            };
        }
        state.scheduled = regs.enabled and logic.shifter != null;
        return state;
    }
};

pub const UsartRx = struct {
    /// Receive whole words, retaining the result until an acknowledgement clears ready.
    fn step(_: *UsartRx, regs: *Registers, inputs: Inputs) SmartPinError!Outputs {
        var state = serialOutputs(regs, inputs);
        if (regs.enabled and !regs.ready) {
            if (regs.source) |source| if (try source.read()) |word| receive(&state, regs.x, word);
        }
        return state;
    }
};

pub const DummyMode = struct {
    /// Reject unsupported smart logic through the simulator's ordinary fault path.
    fn step(_: *DummyMode, _: *Registers, _: Inputs) SmartPinError!Outputs {
        return error.UnsupportedSmartPinMode;
    }
};

/// Shared registers and a mode-specific payload containing only private logic state.
pub const SmartPin = struct {
    registers: Registers = .{},
    logic: Logic = .{ .gpio = .{} },

    pub const Logic = union(Mode) {
        gpio: Normal,
        repository: Repository,
        dac_dither_rnd: DummyMode,
        dac_dither_pwm: DummyMode,
        pulse: DummyMode,
        transition: DummyMode,
        nco_frequency: DummyMode,
        nco_duty: DummyMode,
        pwm_triangle: DummyMode,
        pwm_sawtooth: DummyMode,
        pwm_smps: DummyMode,
        quadrature: DummyMode,
        reg_up: DummyMode,
        reg_up_down: DummyMode,
        count_rises: DummyMode,
        count_highs: DummyMode,
        state_ticks: DummyMode,
        high_ticks: DummyMode,
        events_ticks: DummyMode,
        periods_ticks: DummyMode,
        periods_highs: DummyMode,
        counter_ticks: DummyMode,
        counter_highs: DummyMode,
        counter_periods: DummyMode,
        adc: DummyMode,
        adc_ext: DummyMode,
        adc_scope: DummyMode,
        usb_pair: DummyMode,
        sync_tx: UsartTx,
        sync_rx: UsartRx,
        uart_tx: UartTx,
        uart_rx: UartRx,
    };

    /// Replace the logic payload, retaining the shared registers and attached endpoints.
    pub fn configure(pin: *SmartPin, mode: Mode) void {
        pin.logic = switch (mode) {
            inline else => |tag| @unionInit(Logic, @tagName(tag), .{}),
        };
    }

    /// Apply command inputs, dispatch the active logic, then latch its result and IN signal.
    pub fn step(pin: *SmartPin, inputs: Inputs) SmartPinError!Outputs {
        var signals = inputs;
        signals.reset = pin.registers.enabled and inputs.dir == 0;
        pin.registers.enabled = inputs.dir != 0;
        if (inputs.new_x) |value| pin.registers.x = value;
        if (inputs.new_y) |value| pin.registers.y = value;
        if (inputs.ack or inputs.new_x != null or inputs.new_y != null or signals.reset) pin.registers.ready = false;
        const state = try switch (pin.logic) {
            inline else => |*logic| logic.step(&pin.registers, signals),
        };
        pin.registers.result = state.y;
        pin.registers.ready = state.in != 0;
        return state;
    }
};

test "buffered endpoints truncate writer words and untimed RX waits for acknowledgement" {
    var reader = std.Io.Reader.fixed(&.{ 0x80, 0xff });
    var source: DataSource = .{ .reader = &reader };
    var pin: SmartPin = .{};
    pin.configure(.sync_rx);
    pin.registers.source = &source;
    _ = try pin.step(.{ .dir = 1, .new_x = 7, .clock = 0 });
    try std.testing.expectEqual(@as(u32, 0x80000000), pin.registers.result);
    _ = try pin.step(.{ .dir = 1, .clock = 1 });
    try std.testing.expectEqual(@as(usize, 1), reader.bufferedLen());
    _ = try pin.step(.{ .dir = 1, .ack = true, .clock = 2 });
    try std.testing.expectEqual(@as(u32, 0xff000000), pin.registers.result);
    _ = try pin.step(.{ .dir = 1, .ack = true, .clock = 3 });
    try std.testing.expect(!pin.registers.ready);

    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();
    var sink: DataSink = .{ .writer = &output.writer };
    pin = .{};
    pin.configure(.sync_tx);
    pin.registers.sink = &sink;
    _ = try pin.step(.{ .dir = 1, .new_x = 31, .new_y = 0x123456ab, .clock = 4 });
    try std.testing.expectEqualSlices(u8, &.{0xab}, output.written());
    try std.testing.expect(pin.registers.ready);
}

test "UART fractional frame timing, holding register, widths and counter rollover" {
    var pin: SmartPin = .{};
    pin.configure(.uart_tx);
    _ = try pin.step(.{ .dir = 1, .new_x = (7 << 16) | 0x8000 | 7, .clock = std.math.maxInt(u64) - 6 });
    try std.testing.expectEqual(@as(u64, 75), SerialTiming.init(pin.registers.x).frame_clocks);
    _ = try pin.step(.{ .dir = 1, .new_x = (1 << 16) | 31, .clock = std.math.maxInt(u64) - 5 });
    try std.testing.expectEqual(@as(u64, 34), SerialTiming.init(pin.registers.x).frame_clocks);
    _ = try pin.step(.{ .dir = 1, .new_y = 0x89abcdef, .clock = std.math.maxInt(u64) - 4 });
    try std.testing.expectEqual(@as(?u32, 0x89abcdef), pin.logic.uart_tx.shifter);
    _ = try pin.step(.{ .dir = 1, .new_y = 0x12345678, .clock = std.math.maxInt(u64) - 3 });
    _ = try pin.step(.{ .dir = 1, .clock = 28 });
    try std.testing.expectEqual(@as(?u32, 0x89abcdef), pin.logic.uart_tx.shifter);
    _ = try pin.step(.{ .dir = 1, .clock = 29 });
    try std.testing.expectEqual(@as(?u32, 0x12345678), pin.logic.uart_tx.shifter);
    _ = try pin.step(.{ .clock = 30 });
    try std.testing.expect(pin.logic.uart_tx.buffer == null and pin.logic.uart_tx.shifter == null);
}

test "whole-word callbacks and timed UART RX preserve all 32 bits" {
    const Device = struct {
        value: ?u32 = 0x89abcdef,
        received: ?u32 = null,
        /// Consume one word from the test input device.
        fn read(context: *anyopaque) anyerror!?u32 {
            const device: *@This() = @ptrCast(@alignCast(context));
            const value = device.value;
            device.value = null;
            return value;
        }
        /// Record the full word delivered to the test device.
        fn write(context: *anyopaque, value: u32) anyerror!void {
            const device: *@This() = @ptrCast(@alignCast(context));
            device.received = value;
        }
    };
    var device: Device = .{};
    var source: DataSource = .{ .callback = .{ .context = &device, .read = Device.read } };
    var sink: DataSink = .{ .callback = .{ .context = &device, .write = Device.write } };
    var pin: SmartPin = .{};
    pin.configure(.uart_rx);
    pin.registers.source = &source;
    _ = try pin.step(.{ .dir = 1, .new_x = (3 << 16) | 31, .clock = 10 });
    _ = try pin.step(.{ .dir = 1, .clock = 111 });
    try std.testing.expect(!pin.registers.ready);
    _ = try pin.step(.{ .dir = 1, .clock = 112 });
    try std.testing.expectEqual(@as(u32, 0x89abcdef), pin.registers.result);
    try std.testing.expect(pin.registers.ready);
    pin = .{};
    pin.configure(.sync_tx);
    pin.registers.sink = &sink;
    _ = try pin.step(.{ .dir = 1, .new_x = 31, .new_y = 0x12345678, .clock = 113 });
    try std.testing.expectEqual(@as(?u32, 0x12345678), device.received);
}

test "reconfiguration retains endpoints and registers but discards mode-specific transfers" {
    var reader = std.Io.Reader.fixed(&.{0x80});
    var source: DataSource = .{ .reader = &reader };
    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();
    var sink: DataSink = .{ .writer = &output.writer };
    var pin: SmartPin = .{};
    pin.registers.source = &source;
    pin.registers.sink = &sink;
    _ = try pin.step(.{ .dir = 1, .new_x = (1 << 16) | 7, .clock = 0 });
    pin.configure(.uart_tx);
    _ = try pin.step(.{ .dir = 1, .new_y = 'a', .clock = 1 });
    try std.testing.expect(pin.logic.uart_tx.shifter != null);
    pin.configure(.sync_tx);
    try std.testing.expect(pin.logic == .sync_tx and pin.logic.sync_tx.buffer == null);
    try std.testing.expect(pin.registers.source == &source);
    _ = try pin.step(.{ .dir = 1, .new_y = 'b', .clock = 2 });
    try std.testing.expectEqualStrings("b", output.written());
    pin.configure(.uart_rx);
    _ = try pin.step(.{ .dir = 1, .clock = 3 });
    _ = try pin.step(.{ .dir = 1, .clock = 13 });
    try std.testing.expectEqual(@as(u32, 0x80000000), pin.registers.result);
}

test "step updates shared registers and latches repository outputs" {
    var pin: SmartPin = .{};
    pin.configure(.repository);
    const written = try pin.step(.{ .dir = 1, .new_x = 0x89abcdef, .new_y = 123, .clock = 10 });
    try std.testing.expectEqual(@as(u32, 0x89abcdef), pin.registers.x);
    try std.testing.expectEqual(@as(u32, 123), pin.registers.y);
    try std.testing.expectEqual(written.y, pin.registers.result);
    try std.testing.expectEqual(written.in != 0, pin.registers.ready);
    try std.testing.expect(pin.registers.enabled and !pin.registers.ready);
    const complete = try pin.step(.{ .dir = 1, .clock = 11 });
    try std.testing.expect(complete.in != 0 and pin.registers.ready);
    _ = try pin.step(.{ .dir = 1, .ack = true, .clock = 12 });
    try std.testing.expect(!pin.registers.ready);
    _ = try pin.step(.{ .new_x = 456, .clock = 13 });
    try std.testing.expectEqual(@as(u32, 456), pin.registers.x);
    try std.testing.expectEqual(@as(u32, 0x89abcdef), pin.registers.result);
    try std.testing.expect(!pin.registers.enabled);
}

test "configuration replaces only logic and unsupported logic fails through step" {
    var pin: SmartPin = .{};
    _ = try pin.step(.{ .dir = 1, .out = 1, .a = .one, .new_x = 12, .new_y = 34 });
    const registers = pin.registers;
    pin.configure(.usb_pair);
    try std.testing.expectEqualDeep(registers, pin.registers);
    try std.testing.expectError(error.UnsupportedSmartPinMode, pin.step(.{ .dir = 1, .clock = 1 }));
    pin.configure(.gpio);
    const state = try pin.step(.{ .dir = 1, .out = 1, .a = .one, .clock = 2 });
    try std.testing.expectEqual(Signal.one, state.smart_out);
    try std.testing.expectEqual(@as(u1, 1), state.in);
}
