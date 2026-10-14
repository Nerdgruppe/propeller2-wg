//! Cog/LUT RAM and the five-stage execution pipeline.
//! Hub services clock shared requests; the cog captures operands, stalls and redirects fetch.

const std = @import("std");
const logger = std.log.scoped(.cog);

const Cog = @This();
const Debugger = @import("Debugger.zig");
const Hub = @import("Hub.zig");
const Cordic = @import("Cordic.zig");

const decode = @import("decode.zig");
const encoding = @import("encoding.zig");
const execute = @import("execute.zig");
const p2 = @import("p2");
const types = p2.types;
const EventId = types.EventId;
const PinIndex = types.PinIndex;

pub const Register = types.Register;
pub const EventSet = std.EnumSet(EventId);

/// Execute-only register ROM, transcribed from the silicon instruction words.
pub const debug_rom = [8]u32{
    0b1111_1101011_001_000001111_000101000,
    0b1111_1100011_000_000000000_111111000,
    0b1111_1101011_001_000001111_000101000,
    0b1111_1011000_000_000000000_111111001,
    0b1111_1101100_000_000000000_000000000,
    0b1111_1101011_001_000001111_000101000,
    0b1111_1011000_000_000000000_111111000,
    0b1111_1011001_110_111111111_111111111,
};

pub const ExecMode = enum {
    stopped,
    cog,
    hub,
};

hub: *Hub,
id: u3,

registers: std.EnumArray(Register, u32) = .initFill(0),
/// Underlying RAM at $1F8..$1FF, bypassed by ordinary special-register access.
ram_tail: [8]u32 = @splat(0),
lut: [512]u32 = @splat(0),

pc: u20 = 0,
q: u32 = 0,
cordic_result: Cordic.Port = .{},
setq_pending: bool = false,
block_pointer_delta: bool = false,
z: bool = false,
c: bool = false,
q2: bool = false,

augs: u32 = 0,
augs_pending: bool = false,
augd: u32 = 0,
augd_pending: bool = false,

lut_sharing: bool = false,
interrupts: Interrupts = .{},
debug: Debug = .{},
repeat: ?Repeat = null,
skip: Skip = .{},
xbyte_mode: XbyteMode = @enumFromInt(0),
xbyte_default_mode: XbyteMode = @enumFromInt(0),
xbyte: ?Xbyte = null,
started_no_load: bool = false,

exec_mode: ExecMode = .stopped,

/// Slots being processed on this edge: fetch, read/decode, latch, execute, store.
pipeline: [5]?PipelineState = @splat(null),
issue_phase: bool = true,
collecting_writes: bool = false,
writeback: Writeback = .{},
retired: ?PipelineState = null,
ram_collision: ?struct { clock: u64, address: u9, value: u32 } = null,
lut_collision: ?struct { clock: u64, address: u9 } = null,
dispatch_pc: u20 = 0,
branched: bool = false,
stack: [8]u32 = @splat(0),
wait_until: ?u64 = null,
fifo_state: FifoState = .{},
read_bus: ReadBus = .{},
ct_targets: [3]?u32 = @splat(null),
events: EventSet = .empty,
selectable_events: [4]?SelectableEvent = @splat(null),
pattern: ?Pattern = null,
pixel_pivot: u8 = 0,
pixel_mode: u6 = 0,
memory_transfer: ?MemoryTransfer = null,
hub_writeback: ?struct { provenance: Debugger.Provenance, address: u32, value: u32, size: u3, masked: bool, origin: Hub.WriteOrigin } = null,
startup: ?Startup = null,

pub const Pattern = struct {
    input: Register,
    mask: u32,
    value: u32,
    equal: bool,
    matched: bool,
};

pub const SelectableEvent = union(enum) {
    lut_lock: u6,
    pin: struct {
        index: PinIndex,
        mode: enum { rising, falling, change, low, high },
        previous: bool,

        /// Test an edge against the last sample, or a continuously asserted input level.
        fn triggered(pin: @This(), value: bool) bool {
            return switch (pin.mode) {
                .rising => value and !pin.previous,
                .falling => !value and pin.previous,
                .change => value != pin.previous,
                .low => !value,
                .high => value,
            };
        }
    },
};

pub const InterruptLevel = enum { int1, int2, int3 };
pub const InjectedInterrupt = enum { int0, int1, int2, int3 };

/// The hidden interrupt controller and its two overlaid registers.
pub const Debug = struct {
    state: InterruptState = .idle,
    control: u32 = 0,
    code: u8 = 0,
    coginit: bool = false,
    ijmp: u32 = 0x1f8,
    iret: u32 = 0,
    pending_since: u64 = 0,
    return_pc: u20 = 0,
    return_mode: ExecMode = .cog,
    fifo_untouched: bool = true,
};
/// One hardware repeat context counts pipeline encounters, not elapsed clocks.
pub const Repeat = struct {
    start: u20,
    length: u9,
    left: u9,
    iterations: u32,
};

/// The skip shifter is suspended by hardware-stack calls and interrupt service.
pub const Skip = struct {
    mode: enum { fast, cancel } = .fast,
    pattern: u32 = 0,
    depth: u4 = 0,
};

/// All nine-bit hardware configurations are valid, including aliases and compressed-table modes.
const XbyteMode = enum(u9) { _ };

/// Latches used only while the phantom bytecode dispatch owns the pipeline.
const Xbyte = struct {
    provenance: Debugger.Provenance,
    phase: enum { lookup, latch, execute, branch } = .lookup,
    address: u9 = 0,
    entry: u32 = 0,
};

/// Select a bytecode's LUT entry, including compressed sixteen-byte groups and upper-bit modes.
fn xbyte_address(configuration: XbyteMode, byte: u8) u9 {
    const mode = @intFromEnum(configuration);
    const width: u4 = switch (@as(u2, @truncate(mode >> 2))) {
        0 => {
            const threshold: u4 = @truncate(mode >> 4);
            const group: u4 = @truncate(byte >> 4);
            return (mode & 0x100) | if (threshold != 0 and group >= threshold)
                (@as(u9, threshold) << 4) | (group - threshold)
            else
                byte;
        },
        1 => if (mode & 0x10 == 0) 7 else 6,
        2 => 5,
        3 => 4,
    };
    const mask: u9 = (@as(u9, 1) << width) - 1;
    const shift: u3 = if (mode & 2 != 0) @intCast(8 - width) else 0;
    return (mode & ~mask) | ((@as(u9, byte) >> shift) & mask);
}

pub const InterruptState = enum(u2) { idle = 0, pending = 2, active = 3 };
pub const Interrupts = struct {
    stalled: bool = false,
    levels: std.EnumArray(InterruptLevel, struct {
        source: ?EventId = null,
        state: InterruptState = .idle,
        pending_since: u64 = 0,
    }) = .initFill(.{}),
};

pub const Startup = struct {
    provenance: Debugger.Provenance,
    address: u32,
    grant_at: u64,
    remaining: u10,
    reg: u9 = 0,
    release_at: ?u64 = null,
    responses: [8]?struct { reg: u9, value: u32 } = @splat(null),
};

/// Shared hub read-response phase and the value last driven on that bus.
pub const ReadBus = struct {
    head: usize = 0,
    latch: u32 = 0,
};

pub const MemoryTransfer = struct {
    /// State needed only while returning hub reads.
    pub const Read = struct {
        no_result: bool = false,
        grant_address: u32 = 0,
        grants_left: u64 = 0,
        responses: [8]?u32 = @splat(null),
        value: ?u32 = null,
        partial: ?u32 = null,
    };

    /// State needed only while sending hub writes.
    pub const Write = struct {
        immediate: ?u32 = null,
        ready_at: u64 = 0,
        masked: bool = false,
        origin: Hub.WriteOrigin = .application,
        source_value: ?u32 = null,
    };

    /// Exactly one direction owns the transfer-specific latches.
    pub const Direction = union(enum) { read: Read, write: Write };

    provenance: Debugger.Provenance,
    address: u32,
    remaining: u64,
    reg: u9,
    lut: bool,
    block: bool = false,
    timed: bool = false,
    size: u3 = 0,
    grant_at: u64 = 0,
    direction: Direction = .{ .read = .{} },

    /// Whether this transfer uses the write side of the hub interface.
    pub fn isWrite(transfer: *const MemoryTransfer) bool {
        return switch (transfer.direction) {
            .read => false,
            .write => true,
        };
    }
};

/// The shared sequential interface has one transfer direction at a time.
pub const FifoMode = enum { read, write };

/// A reload descriptor counts aligned longs; zero in the instruction means a full address-map span.
pub const FifoBlock = struct {
    address: u20,
    longs: u19,

    /// Decode only the architectural address and fourteen-bit block count fields.
    fn init(config: u32, address: u32) FifoBlock {
        const count: u14 = @truncate(config);
        return .{ .address = @truncate(address), .longs = if (count == 0)
            0x40000
        else
            @as(u19, count) << 4 };
    }
};

/// A requested configuration waits for the preceding write stream to drain.
pub const FifoRequest = struct {
    provenance: Debugger.Provenance,
    mode: FifoMode,
    block: FifoBlock,
    setup: u4,
    software: bool = true,
    accepted_at: u64 = 0,
};

/// The RAM return path holds five requests; the FIFO holds nineteen longs.
/// Stop issuing at fifteen occupied words so returning reads cannot overflow.
pub const HubFifo = struct {
    provenance: Debugger.Provenance = .{},
    mode: FifoMode = .read,
    initializing: bool = false,
    block: ?FifoBlock = null,
    remaining: u19 = 0x40000,
    write_bytes: [76]u8 = undefined,
    write_provenance: [76]Debugger.Provenance = @splat(.{}),
    /// Byte counts awaiting the fixed two-clock eligibility stage.
    write_pending: [2]usize = @splat(0),
    write_pending_head: usize = 0,
    write_ready_count: usize = 0,
    write_head: usize = 0,
    write_count: usize = 0,
    words: [19]u32 = undefined,
    /// Physical source and readable lanes travel with each returned long through block reloads.
    word_addresses: [19]u20 = @splat(0),
    word_valid: [19]u4 = @splat(0),
    response_addresses: [8]u20 = @splat(0),
    response_valid: [8]u4 = @splat(0),
    head: usize = 0,
    count: usize = 0,
    byte_offset: u2,
    address: u20,
    ready_at: ?u64,
    responses: [8]?u32 = @splat(null),

    /// Consume the next instruction slice; read/decode joins the adjacent slice for an unaligned word.
    fn read(fifo: *HubFifo) ?u32 {
        if (fifo.count == 0)
            return null;
        const low = fifo.words[fifo.head];
        const value = low >> (@as(u5, fifo.byte_offset) * 8);
        fifo.head = (fifo.head + 1) % fifo.words.len;
        fifo.count -= 1;
        return value;
    }

    /// Consume one software-FIFO byte, releasing its buffered long after the fourth byte.
    pub fn read_byte(fifo: *HubFifo) ?u8 {
        if (fifo.count == 0)
            return null;
        const value: u8 = @truncate(fifo.words[fifo.head] >> (@as(u5, fifo.byte_offset) * 8));
        fifo.byte_offset +%= 1;
        if (fifo.byte_offset == 0) {
            fifo.head = (fifo.head + 1) % fifo.words.len;
            fifo.count -= 1;
        }
        return value;
    }
};

/// Cog-local FIFO configuration, architectural pointer and per-edge grant signals.
pub const FifoState = struct {
    address: ?u20 = null,
    engine: ?HubFifo = null,
    granted_at: ?u64 = null,
    request: ?FifoRequest = null,
    initial_response: bool = false,
};

const Writeback = struct {
    provenance: Debugger.Provenance = .{},
    writes: [3]struct { reg: Register, value: u32, raw: bool = false, debug_overlay: bool = false } = undefined,
    count: u2 = 0,
    c: ?bool = null,
    z: ?bool = null,
    lut: ?struct { address: u9, value: u32 } = null,
};

/// Create a stopped cog with cleared execution state and a link to its shared hub.
pub fn init(hub: *Hub, id: u3) Cog {
    return .{
        .id = id,
        .hub = hub,
    };
}

/// Stop and clear the execution engine, release owned locks and cancel pending startup.
/// Retain cog/LUT RAM while clearing the special pointer, direction and output registers.
pub fn reset(cog: *Cog) void {
    const id = cog.id;
    const hub = cog.hub;
    hub.pending_starts[id] = null;
    const registers = cog.registers;
    const ram_tail = cog.ram_tail;
    const lut = cog.lut;
    hub.cordic.forget(@enumFromInt(id));
    cog.cordic_result.reset();
    const cordic_result = cog.cordic_result;
    for (&hub.locks) |*lock| {
        if (lock.taken and lock.owner == id) {
            hub.release_lock(lock);
        }
    }
    for (&hub.pending_events) |*edge| {
        edge[id] = .empty;
    }
    cog.* = .init(hub, id);
    // Cog and LUT RAM survive COGSTOP and no-load COGINIT.
    cog.registers = registers;
    cog.ram_tail = ram_tail;
    cog.lut = lut;
    cog.cordic_result = cordic_result;
    for ([_]Register{ .PTRA, .PTRB, .DIRA, .DIRB, .OUTA, .OUTB }) |reg| {
        cog.registers.set(reg, 0);
    }
}

/// Advance instruction stages for one clock after the hub has serviced shared resources.
/// An execute-stage wait freezes younger stages; completed results are committed on the next edge.
pub fn step(cog: *Cog) void {
    if (cog.exec_mode == .stopped or cog.startup != null) {
        return;
    }

    for (&cog.ct_targets, [3]EventId{ .CT1, .CT2, .CT3 }) |*target, event| {
        if (target.*) |value| {
            if (@as(u32, @truncate(cog.hub.counter)) -% value < 0x8000_0000) {
                cog.raise_event(event);
            }
        }
    }
    for (&cog.selectable_events, [4]EventId{ .SE1, .SE2, .SE3, .SE4 }) |*selection, event| {
        if (selection.*) |*sensor|
            switch (sensor.*) {
                .lut_lock => {},
                .pin => |*pin| {
                    const value = cog.read_input_bit(pin.index);
                    if (pin.triggered(value)) {
                        cog.hub.signal_event(cog.id, event);
                    }
                    pin.previous = value;
                },
            };
    }
    if (cog.pattern) |*pattern| {
        const matched = cog.pattern_matches();
        if (matched and !pattern.matched) {
            cog.raise_event(.PAT);
        }
        pattern.matched = matched;
    }

    if (cog.xbyte != null) {
        cog.clock_xbyte();
        return;
    }

    // Unaligned fetch begins with the low word. The adjacent slice returns
    // on the next clock, completing the instruction at its read/decode stage.
    // Complete it before an older ALTx can alter this younger instruction.
    if (cog.pipeline[1]) |*state| {
        if (state.fifo_tail) {
            const fifo = &cog.fifo_state.engine.?;
            if (fifo.count == 0)
                return;
            state.instr |= fifo.words[fifo.head] << @intCast(32 - @as(u6, fifo.byte_offset) * 8);
            state.fifo_tail = false;
        }
    }

    // Q clearing is decoded before branch cancellation, but follows the older
    // instruction's Q reads. Keep the decoded word even if that branch flushes it.
    const decoded_word: ?u32 = if (cog.pipeline[1]) |state|
        state.instr
    else
        null;
    var late_q_write = false;
    if (cog.pipeline[3]) |*state| {
        state.debugger_attempted = true;
        const enabled = !state.cancelled and
            (state.instr == 0 or
                cog.is_condition_met(@enumFromInt(@as(u4, @truncate(state.instr >> 28)))));
        if (enabled and !state.execution_ready and state.ready_at == null) {
            const extra: u64 = switch (decode.decode(state.instr)) {
                .rdlut => 1,
                .addpix, .mulpix, .blnpix, .mixpix => 5,
                else => 0,
            };
            state.ready_at = cog.hub.counter +% extra;
        }
        if (enabled and !state.execution_ready and !Hub.clock_reached(cog.hub.counter, state.ready_at.?)) {
            cog.trace(.stall, state.*);
            return;
        }
        state.execution_ready = true;
        const instr = state.*;
        late_q_write = enabled and switch (decode.decode(instr.instr)) {
            .setq, .setq2, .crcnib => true,
            else => false,
        };
        cog.trace(.execute, instr);
        cog.dispatch_pc = instr.pc;
        cog.branched = false;
        const old_c = cog.c;
        const old_z = cog.z;
        cog.collecting_writes = true;
        if (instr.interrupt) |level| {
            if (level == .int0) {
                cog.debug.state = .active;
                cog.debug.control = 0;
                cog.debug.return_pc = instr.pc;
                cog.debug.return_mode = cog.exec_mode;
                cog.debug.fifo_untouched = true;
            } else {
                const ordinary: InterruptLevel = @enumFromInt(@intFromEnum(level) - 1);
                cog.interrupts.levels.getPtr(ordinary).state = .active;
                cog.raise_event(.INT);
                const index: u5 = @intFromEnum(ordinary);
                if (cog.debug.control & ((@as(u32, 1) << (5 + index)) | (@as(u32, 1) << (1 + index))) != 0) {
                    cog.request_debug(false);
                }
            }
        }
        var result = if (instr.cancelled)
            cog.cancel_instruction()
        else if (instr.command_result) |result| blk: {
            cog.writeback = instr.command_writeback;
            break :blk result;
        } else execute.execute_instruction(cog, instr);
        cog.collecting_writes = false;
        // COGSTOP/self-restart can clear the entire pipeline during execution.
        if (cog.exec_mode == .stopped or cog.pipeline[3] == null)
            return;
        if (cog.c != old_c) {
            cog.writeback.c = cog.c;
        }
        if (cog.z != old_z) {
            cog.writeback.z = cog.z;
        }
        cog.c = old_c;
        cog.z = old_z;
        if (std.meta.activeTag(result) == .next and instr.instr != 0 and instr.instr >> 28 == 0 and !cog.branched) {
            result = cog.return_stack();
        }
        switch (result) {
            .wait => {
                cog.trace(.stall, instr);
                return;
            },
            .next, .skip => {},
            .trap, .not_implemented, .illegal => {
                cog.hub.fault = .{
                    .cog = cog.id,
                    .pc = instr.pc,
                    .instruction = instr.instr,
                    .result = result,
                    .debug_rom = instr.debug_rom,
                };
                cog.exec_mode = .stopped;
                cog.pipeline = @splat(null);
                cog.writeback = .{};
                return;
            },
        }
        if (!cog.branched) {
            if (instr.repeat_target) |target| {
                cog.jump_repeat(target);
            }
        }
    }

    if (cog.pipeline[1]) |*state| {
        cog.prepare_flow(state);
    }

    // ALTx may have changed the next word; a taken branch has discarded it.
    const next_word = if (cog.branched)
        decoded_word
    else if (cog.pipeline[1]) |state|
        state.instr
    else
        null;
    if (next_word) |word|
        switch (decode.decode(word)) {
            .coginit, .qdiv, .qfrac, .qrotate => {
                if (!late_q_write and !(cog.setq_pending and !cog.q2)) {
                    cog.q = 0;
                }
            },
            else => {},
        };

    if (cog.pipeline[2]) |*state| {
        cog.capture_operands(state);
        cog.trace(.latch, state.*);
    }
    if (cog.pipeline[1]) |state| {
        cog.trace(.read, state);
    }
    var fetched = false;
    if (cog.inject_interrupt()) |state| {
        cog.pipeline[0] = state;
        cog.trace(.fetch, state);
        fetched = true;
    } else if (cog.issue_phase and cog.xbyte == null and !cog.debug_queued()) {
        cog.pipeline[0] = cog.fetch_instruction();
        if (cog.pipeline[0]) |state| {
            cog.trace(.fetch, state);
            fetched = true;
        }
    }
    std.mem.copyBackwards(?PipelineState, cog.pipeline[1..], cog.pipeline[0..4]);
    cog.pipeline[0] = null;
    // An empty FIFO has not consumed an issue slot. Retry on the next edge.
    cog.issue_phase = !cog.issue_phase or !fetched;
}

/// Receive timed image-load beats and release startup after the final load-to-fetch handoff.
pub fn clock_startup(cog: *Cog) void {
    const startup = if (cog.startup) |*value|
        value
    else
        return;
    cog.hub.access_context = startup.provenance;
    if (startup.release_at) |clock| {
        if (cog.hub.counter == clock) {
            cog.startup = null;
        }
        return;
    }
    const response = &startup.responses[cog.read_bus.head];
    if (response.*) |beat| {
        cog.write_ram(beat.reg, beat.value);
        cog.read_bus.latch = beat.value;
        response.* = null;
        // The completed load passes through the startup/fetch handoff.
        if (beat.reg == 503) {
            startup.release_at = cog.hub.counter +% 3;
        }
    }
    if (startup.remaining == 0 or cog.hub.counter != startup.grant_at)
        return;
    startup.responses[(cog.read_bus.head + 5) % startup.responses.len] = .{
        .reg = startup.reg,
        .value = cog.hub.read_memory(startup.address, 4),
    };
    startup.reg += 1;
    startup.address +%= 4;
    startup.remaining -= 1;
    startup.grant_at +%= 1;
}

/// Hub commands have a separate eight-clock round robin, whose slot is the
/// cog ID (unlike the rotating RAM slices). Evaluate before any cog executes,
/// so a stop, start, or lock event has the same edge for every observer.
pub fn clock_command(cog: *Cog) void {
    if (cog.exec_mode == .stopped or cog.startup != null)
        return;
    const state = if (cog.pipeline[3]) |*value|
        value
    else
        return;
    if (state.cancelled or
        state.command_result != null or
        !cog.is_condition_met(@enumFromInt(@as(u4, @truncate(state.instr >> 28)))))
        return;
    const opcode = decode.decode(state.instr);
    const extra: u64 = switch (opcode) {
        .cogid => if (state.instr & (1 << 20) != 0 or state.instr & (1 << 18) == 0) 2 else 0,
        .locknew => 2,
        .coginit, .locktry, .lockrel => if (state.instr & (1 << 20) != 0)
            2
        else
            0,
        .cogstop, .lockret, .hubset, .qmul, .qdiv, .qfrac, .qsqrt, .qrotate, .qvector, .qlog, .qexp => 0,
        else => return,
    };
    state.debugger_attempted = true;
    if (state.command_at == null) {
        state.command_at = cog.hub.counter +% ((@as(u64, cog.id) -% cog.hub.counter) & 7);
        state.ready_at = state.command_at.? +% extra;
    }
    if (cog.hub.counter != state.command_at.?)
        return;
    const old_c = cog.c;
    const old_z = cog.z;
    cog.collecting_writes = true;
    const result = execute.execute_instruction(cog, state.*);
    cog.collecting_writes = false;
    // A self-stop/restart discarded this command along with its old pipeline.
    if (cog.pipeline[3] == null)
        return;
    if (cog.c != old_c) {
        cog.writeback.c = cog.c;
    }
    if (cog.z != old_z) {
        cog.writeback.z = cog.z;
    }
    cog.c = old_c;
    cog.z = old_z;
    state.command_writeback = cog.writeback;
    cog.writeback = .{};
    state.command_result = result;
}

/// Deliver RAM responses and service the shared FIFO before any cog executes this edge.
pub fn clock_fifo(cog: *Cog) void {
    cog.fifo_state.initial_response = false;
    if (cog.fifo_state.request) |request| {
        if (cog.fifo_state.engine == null or cog.fifo_state.engine.?.write_count == 0) {
            cog.apply_fifo(request);
        }
    }
    const fifo = if (cog.fifo_state.engine) |*value|
        value
    else
        return;
    if (fifo.ready_at) |deadline| {
        if (Hub.clock_reached(cog.hub.counter, deadline)) {
            fifo.ready_at = null;
        }
    }
    if (fifo.mode == .read) {
        const response = &fifo.responses[cog.read_bus.head];
        if (response.*) |value| {
            if (fifo.initializing) {
                cog.fifo_state.initial_response = true;
                fifo.initializing = false;
            }
            std.debug.assert(fifo.count < fifo.words.len);
            const slot = (fifo.head + fifo.count) % fifo.words.len;
            fifo.words[slot] = value;
            fifo.word_addresses[slot] = fifo.response_addresses[cog.read_bus.head];
            fifo.word_valid[slot] = fifo.response_valid[cog.read_bus.head];
            fifo.count += 1;
            response.* = null;
        }
    }
    if (!cog.fifo_grants(cog.hub.counter))
        return;
    cog.fifo_state.granted_at = cog.hub.counter;
    if (fifo.mode == .read) {
        cog.hub.read_class = .physical;
        cog.hub.access_context = fifo.provenance;
        if (cog.hub.access_context.cog == null) {
            cog.hub.access_context = .{ .cog = cog.id };
        }
        const response = (cog.read_bus.head + 5) % fifo.responses.len;
        fifo.responses[response] = cog.hub.read_memory(fifo.address, 4);
        fifo.response_addresses[response] = fifo.address;
        fifo.response_valid[response] = 0;
        for (0..4) |i| {
            if (cog.hub.readBackingAddress(fifo.address +% @as(u20, @intCast(i))) != null) {
                fifo.response_valid[response] |= @as(u4, 1) << @intCast(i);
            }
        }
        cog.hub.read_class = .data;
        fifo.address +%= 4;
        cog.advance_fifo_block();
    } else {
        // Drain only ready bytes in this physical long; later lanes can use a subsequent slice visit.
        const size = @min(@as(usize, 4 - @as(u3, @as(u2, @truncate(fifo.address)))), fifo.write_count);
        var consumed: usize = 0;
        while (consumed < size and consumed < fifo.write_ready_count) : (consumed += 1) {
            const index = (fifo.write_head + consumed) % fifo.write_bytes.len;
            cog.hub.access_context = fifo.write_provenance[index];
            cog.hub.write_memory(fifo.address, fifo.write_bytes[index], 1, false);
            fifo.address +%= 1;
        }
        fifo.write_head = (fifo.write_head + consumed) % fifo.write_bytes.len;
        fifo.write_count -= consumed;
        fifo.write_ready_count -= consumed;
        if (fifo.address & 3 == 0) {
            cog.advance_fifo_block();
        }
        if (fifo.write_count == 0) {
            if (cog.fifo_state.request) |request| {
                cog.apply_fifo(request);
            }
        }
    }
}

/// Reload the configured block at a transport boundary and strobe FBW independently of software reads.
fn advance_fifo_block(cog: *Cog) void {
    const fifo = &cog.fifo_state.engine.?;
    fifo.remaining -= 1;
    if (fifo.remaining != 0)
        return;
    if (fifo.block) |block| {
        fifo.address = block.address & 0xffffc;
        fifo.remaining = block.longs;
        cog.raise_event(.FBW);
    } else {
        fifo.remaining = 0x40000;
    }
}

/// Test current/next-clock grants, including returning reads and queued byte readiness.
pub fn fifo_grants(cog: *const Cog, clock: u64) bool {
    const fifo = cog.fifo_state.engine orelse return false;
    if (fifo.ready_at) |deadline| {
        if (!Hub.clock_reached(clock, deadline))
            return false;
    }
    if (((fifo.address >> 2) & 7) != ((clock +% cog.id) & 7))
        return false;
    if (fifo.mode == .write)
        return fifo.write_ready_count != 0 or
            (clock != cog.hub.counter and
                fifo.write_pending[fifo.write_pending_head] != 0);
    const returning: usize = @intFromBool(clock != cog.hub.counter and
        fifo.responses[(cog.read_bus.head + 1) % fifo.responses.len] != null);
    return fifo.count + returning < 15;
}

/// Record possible cog fetch/write collisions and retire the completed store-stage instruction.
pub fn commit_results(cog: *Cog) void {
    cog.retired = null;
    // This edge completes the previous ALU result, including streaming beats
    // while an instruction holds the execute stage.
    for (cog.writeback.writes[0..cog.writeback.count]) |write| {
        if (cog.issue_phase and cog.exec_mode == .cog and
            cog.local_fetch_address() < 504 and cog.local_fetch_address() == @intFromEnum(write.reg))
        {
            cog.ram_collision = .{
                .clock = cog.hub.counter,
                .address = @intFromEnum(write.reg),
                .value = cog.read_reg(write.reg),
            };
            cog.trace_collision(@intFromEnum(write.reg), .cog_ram);
        }
        if (write.raw) {
            cog.write_ram(@intFromEnum(write.reg), write.value);
        } else if (write.debug_overlay) {
            cog.hub.observe(.registers, cog.id, @intFromEnum(write.reg), .write, write.value, .data, .architectural);
            if (write.reg == .INA) {
                cog.debug.ijmp = write.value;
            } else {
                cog.debug.iret = write.value;
            }
        } else {
            cog.write_reg(write.reg, write.value);
        }
    }
    if (cog.writeback.lut) |write| {
        cog.write_lut(write.address, write.value);
    }
    if (cog.writeback.c) |value| {
        cog.c = value;
    }
    if (cog.writeback.z) |value| {
        cog.z = value;
    }
    cog.writeback = .{};
    cog.retired = cog.pipeline[4];
    if (cog.retired) |state| {
        cog.trace(.store, state);
    }
    cog.pipeline[4] = null;
}

/// Hub read grants continue while the cog pipeline is stalled. Each aligned
/// grant produces a response five clocks later; SETQ streams one per clock.
pub fn clock_memory(cog: *Cog) void {
    const transfer = if (cog.memory_transfer) |*value|
        value
    else
        return;
    cog.hub.access_context = transfer.provenance;
    cog.hub.read_class = .physical;
    defer cog.hub.read_class = .data;
    // FIFO initialization shares the ordinary hub-read completion strobe. A pending
    // command completes with the retained read bus rather than issuing its own read.
    if (cog.fifo_state.initial_response and !transfer.isWrite()) {
        transfer.direction.read.value = cog.read_bus.latch;
        transfer.direction.read.grants_left = 0;
        transfer.direction.read.responses = @splat(null);
        return;
    }
    if (transfer.timed and transfer.isWrite()) {
        if (cog.hub.counter == transfer.grant_at and cog.fifo_state.granted_at == cog.hub.counter) {
            transfer.grant_at +%= 8;
            transfer.direction.write.ready_at +%= 8;
            return;
        }
        if (cog.hub.counter == transfer.grant_at and (transfer.address & 3) + transfer.size > 4) {
            const low_size: u3 = @intCast(4 - (transfer.address & 3));
            const value = cog.transfer_source(transfer);
            cog.hub.write_memory_from(
                transfer.address,
                value,
                low_size,
                transfer.direction.write.masked,
                transfer.direction.write.origin,
            );
        }
        return;
    }
    if (!transfer.timed or transfer.direction.read.grants_left == 0) {
        if (transfer.timed) {
            const slot = &transfer.direction.read.responses[cog.read_bus.head];
            transfer.direction.read.value = slot.*;
            if (slot.*) |value| {
                cog.read_bus.latch = value;
            }
            slot.* = null;
        }
        return;
    }
    const slot = &transfer.direction.read.responses[cog.read_bus.head];
    transfer.direction.read.value = slot.*;
    if (slot.*) |value| {
        cog.read_bus.latch = value;
    }
    slot.* = null;
    if (cog.hub.counter != transfer.grant_at)
        return;
    if (cog.fifo_state.granted_at == cog.hub.counter) {
        transfer.grant_at +%= 8;
        return;
    }
    const low_size: u3 = @intCast(@min(transfer.size, 4 - (transfer.direction.read.grant_address & 3)));
    const crossing = low_size < transfer.size;
    if (crossing and transfer.direction.read.partial == null) {
        transfer.direction.read.partial = cog.hub.read_memory(transfer.direction.read.grant_address, low_size);
        transfer.grant_at +%= 1;
        return;
    }
    const high_word = if (crossing)
        cog.hub.read_memory(transfer.direction.read.grant_address +% low_size, 4)
    else
        0;
    const high_size = transfer.size - low_size;
    const high_mask: u32 = if (crossing)
        (@as(u32, 1) << @intCast(@as(u6, high_size) * 8)) - 1
    else
        0;
    const value = if (crossing)
        transfer.direction.read.partial.? | ((high_word & high_mask) << @intCast(@as(u6, low_size) * 8))
    else
        cog.hub.read_memory(transfer.direction.read.grant_address, transfer.size);
    if (crossing) {
        transfer.direction.read.partial = high_word >> @intCast(@as(u6, high_size) * 8);
    }
    for (0..transfer.size) |i| {
        const address = (transfer.direction.read.grant_address +% @as(u32, @intCast(i))) & 0xfffff;
        if (address < (if (cog.hub.debug.protected) @as(u32, 0x7c000) else 0x80000) or address >= 0xfc000) {
            cog.hub.observe(.hub, null, address & 0x7ffff, .read, @as(u8, @truncate(value >> @intCast(i * 8))), .consumed, .ram);
        }
    }
    const response = &transfer.direction.read.responses[(cog.read_bus.head + 5) % transfer.direction.read.responses.len];
    std.debug.assert(response.* == null);
    response.* = value;
    transfer.direction.read.grants_left -= 1;
    transfer.direction.read.grant_address +%= transfer.size;
    transfer.grant_at +%= 1;
}

/// Capture a block-write source once per beat, bypassing architectural LUT-read event generation.
pub fn transfer_source(cog: *Cog, transfer: *MemoryTransfer) u32 {
    if (transfer.direction.write.source_value) |value|
        return value;
    const value = transfer.direction.write.immediate orelse if (transfer.lut)
        cog.read_lut(transfer.reg)
    else if (transfer.block and
        transfer.reg >= 504)
        cog.ram_tail[transfer.reg - 504]
    else
        cog.registers.values[transfer.reg];
    if (transfer.direction.write.immediate == null and !transfer.lut) {
        cog.hub.observe(.registers, cog.id, transfer.reg, .read, value, .data, .ram);
    }
    transfer.direction.write.source_value = value;
    return value;
}

/// The access window begins two clocks after the execution-stage request.
/// Grant timing is shared by reads and writes; their return paths differ.
pub fn memory_grant(cog: *Cog, address: u32) u64 {
    return cog.memory_grant_after(address, 2);
}

/// Find the first rotating RAM-slice window after the requested number of setup clocks.
pub fn memory_grant_after(cog: *Cog, address: u32, setup: u64) u64 {
    const earliest = cog.hub.counter +% setup;
    const bank = (address >> 2) & 7;
    const delay = (bank -% @as(u32, @truncate(earliest +% cog.id))) & 7;
    return earliest +% delay;
}

/// Test the ordinary interrupt busy latches without confusing STALLI with ISR execution.
fn in_isr(cog: *const Cog) bool {
    if (cog.debug.state == .active)
        return true;
    for (cog.interrupts.levels.values) |level| {
        if (level.state == .active)
            return true;
    }
    return false;
}

/// Cancel one pipeline slot using the same SETQ lifetime as conditional cancellation.
fn cancel_instruction(cog: *Cog) ExecResult {
    cog.setq_pending = false;
    cog.q2 = false;
    cog.block_pointer_delta = false;
    return .skip;
}

/// Install the single repeat context; zero length also cancels an older repeat.
pub fn begin_repeat(cog: *Cog, length: u32, iterations: u32) void {
    const count: u9 = @truncate(length);
    cog.repeat = if (count == 0)
        null
    else
        .{
            .start = cog.next_pc(),
            .length = count,
            .left = count,
            .iterations = iterations,
        };
}

/// Replace the skip shifter and reset its call suspension counter.
pub fn begin_skip(cog: *Cog, mode: @FieldType(Skip, "mode"), pattern: u32) void {
    cog.skip = .{ .mode = mode, .pattern = pattern };
}

/// Consume one encounter and prepare its fallthrough before any branch can replace the PC.
/// Fast stepping changes fetched addresses, never the hub clock or instruction issue cadence.
fn prepare_flow(cog: *Cog, state: *PipelineState) void {
    if (state.next_pc != null or state.interrupt != null)
        return;
    const skipping = !cog.in_isr() and cog.skip.depth == 0;
    if (skipping) {
        state.cancelled = cog.skip.pattern & 1 != 0;
        cog.skip.pattern >>= 1;
    }
    if (skipping and cog.skip.mode == .fast and cog.exec_mode == .cog) {
        var count: u4 = 0;
        while (count < 7 and cog.skip.pattern & 1 != 0) : (count += 1) {
            cog.pc +%= 1;
            cog.skip.pattern >>= 1;
        }
    }
    if (cog.repeat) |*rep| {
        rep.left -= 1;
        if (rep.left == 0) {
            if (rep.iterations == 1) {
                cog.repeat = null;
            } else {
                if (rep.iterations != 0) {
                    rep.iterations -= 1;
                }
                rep.left = rep.length;
                if (cog.exec_mode == .hub) {
                    state.repeat_target = rep.start;
                } else {
                    cog.pc = rep.start;
                }
            }
        }
    }
    state.next_pc = state.repeat_target orelse cog.pc;
    if (!state.cancelled and cog.debug.state != .active) {
        if (decode.decode(state.instr) == .brk and cog.debug.control & (1 << 4) != 0) {
            cog.request_debug(false);
        }
        var step_bit: u5 = 0;
        for (std.enums.values(InterruptLevel)) |level| {
            if (cog.interrupts.levels.get(level).state == .active) {
                step_bit = @intCast(@intFromEnum(level) + 1);
                break;
            }
        }
        if (cog.debug.control & (@as(u32, 1) << step_bit) != 0 or
            (cog.debug.control & (1 << 10) != 0 and state.pc == cog.debug.control >> 12))
            cog.request_debug(false);
    }
}

/// Return the prepared nonlinear fallthrough, or sequential PC for direct semantic callers.
pub fn next_pc(cog: *const Cog) u20 {
    if (cog.pipeline[3]) |state| {
        if (state.next_pc) |pc|
            return pc;
    }
    return cog.dispatch_pc +% @as(u20, if (cog.exec_mode == .hub)
        4
    else
        1);
}

/// Take the implicit REP branch without canceling its context, selecting the target's domain.
fn jump_repeat(cog: *Cog, target: u20) void {
    cog.redirect_fetch(target, if (target < 0x400)
        .cog
    else
        .hub);
}

/// Resume a suspended skip shifter on RET/_RET_; ordinary PUSH/POP do not participate.
pub fn return_skip(cog: *Cog) void {
    if (cog.skip.depth != 0) {
        cog.skip.depth -%= 1;
    }
}

/// Return normally, or retain the exact $001FF stack sentinel and start a phantom bytecode dispatch.
pub fn return_stack(cog: *Cog) ExecResult {
    if (cog.stack[0] != 0x1ff) {
        cog.return_skip();
        cog.jump(@truncate(cog.pop()));
        return .next;
    }
    if (cog.fifo_state.address == null or cog.fifo_state.engine == null or
        cog.fifo_state.engine.?.mode != .read)
        return .{ .illegal = "XBYTE requires a read FIFO initialized by RDFAST" };

    cog.xbyte_mode = cog.xbyte_default_mode;
    if (cog.pipeline[3]) |state| {
        if (state.instr >> 28 == 0) {
            switch (decode.decode(state.instr)) {
                .setq => {
                    cog.xbyte_mode = @enumFromInt(@as(u9, @truncate(cog.q)));
                    cog.xbyte_default_mode = cog.xbyte_mode;
                },
                .setq2 => cog.xbyte_mode = @enumFromInt(@as(u9, @truncate(cog.q))),
                else => {},
            }
        }
    }
    cog.xbyte = .{ .provenance = cog.provenance() };
    cog.begin_skip(.fast, 0);
    cog.repeat = null;
    cog.setq_pending = false;
    cog.q2 = false;
    cog.block_pointer_delta = false;
    @memset(cog.pipeline[0..3], null);
    cog.branched = true;
    return .next;
}

/// Clock the hidden lookup and EXECF while hub services and event sensors continue independently.
fn clock_xbyte(cog: *Cog) void {
    const state = &cog.xbyte.?;
    cog.hub.access_context = state.provenance;
    switch (state.phase) {
        .lookup => {
            const byte: u8 = @truncate(cog.read_fifo(1).?);
            cog.write_reg(.PA, byte);
            state.address = xbyte_address(cog.xbyte_mode, byte);
            state.entry = cog.read_lut(state.address);
            state.phase = .latch;
        },
        .latch => state.phase = .execute,
        .execute => state.phase = .branch,
        .branch => {
            const entry = state.entry;
            const address = state.address;
            cog.write_reg(.PB, cog.fifo_state.address.?);
            if (@intFromEnum(cog.xbyte_mode) & 1 != 0) {
                cog.c = address & 2 != 0;
                cog.z = address & 1 != 0;
            }
            cog.begin_skip(.fast, entry >> 10);
            cog.jump(@as(u10, @truncate(entry)));
            cog.xbyte = null;
            // The ordinary branch fetch starts on the following edge.
            cog.issue_phase = true;
        },
    }
}

/// Redirect fetch and discard younger stages; entering hub execution restarts its instruction FIFO.
/// Leaving hub execution keeps the existing FIFO filling, so it may still contend with data transfers.
pub fn jump(cog: *Cog, target: u20) void {
    cog.repeat = null;
    if (cog.pipeline[3]) |state| {
        if (state.interrupt == .int0) {
            cog.pc = target;
            cog.exec_mode = if (target < 0x400)
                .cog
            else
                .hub;
            if (cog.exec_mode == .cog) {
                @memset(cog.pipeline[0..3], null);
                cog.issue_phase = false;
                cog.branched = true;
                return;
            }
        }
    }
    if (cog.debug.state == .active and target >= 0x400) {
        cog.debug.fifo_untouched = false;
    }
    cog.redirect_fetch(target, if (target < 0x400)
        .cog
    else
        .hub);
}

/// Flush and refill a branch target in its selected execution domain.
fn redirect_fetch(cog: *Cog, target: u20, mode: ExecMode) void {
    if (cog.pipeline[3]) |state| {
        cog.trace(.flush, state);
    }
    const was_hub = cog.exec_mode == .hub;
    cog.pc = target;
    cog.exec_mode = mode;
    if (cog.exec_mode == .hub) {
        cog.fifo_state.address = null;
        const request: FifoRequest = .{
            .provenance = cog.provenance(),
            .mode = .read,
            .block = .init(0, target),
            .setup = 5,
            .software = false,
            .accepted_at = cog.hub.counter,
        };
        if (cog.fifo_state.engine != null and
            cog.fifo_state.engine.?.mode == .write and
            cog.fifo_state.engine.?.write_count != 0)
        {
            cog.fifo_state.request = request;
        } else {
            cog.apply_fifo(request);
        }
    } else if (was_hub) {
        // The read FIFO keeps filling after leaving hubexec. Cog fetch no
        // longer consumes it, so it soon fills and stops granting RAM reads.
        cog.fifo_state.address = cog.dispatch_pc +% 8;
    }
    @memset(cog.pipeline[0..3], null);
    // The target's fetch overlaps the branch's store on the next clock.
    cog.issue_phase = false;
    cog.branched = true;
}

/// Write a bounded instruction-stage event when pipeline tracing is enabled.
fn trace(cog: *Cog, stage: enum { fetch, read, latch, execute, store, stall, flush }, state: PipelineState) void {
    const writer = cog.hub.trace_writer orelse return;
    if (cog.hub.trace_lines_left == 0)
        return;
    cog.hub.trace_lines_left -= 1;
    writer.print("CT={d} cog={d} stage={t} pc=0x{x} instruction=0x{x}", .{ cog.hub.counter, cog.id, stage, state.pc, state.instr }) catch {};
    if (state.local_address) |address| {
        writer.print(" local=0x{x}", .{address}) catch {};
    }
    if (state.interrupt) |level| {
        writer.print(" injected={t}", .{level}) catch {};
    }
    writer.writeByte('\n') catch {};
    if (cog.hub.trace_lines_left == 0) {
        writer.writeAll("pipeline trace truncated after 10000 events\n") catch {};
    }
}

/// Write a bounded collision diagnostic without turning ambiguous RAM data into an execution fault.
pub fn trace_collision(cog: *Cog, address: u9, kind: enum { cog_ram, lut_write }) void {
    const writer = cog.hub.trace_writer orelse return;
    if (cog.hub.trace_lines_left == 0)
        return;
    cog.hub.trace_lines_left -= 1;
    writer.print("CT={d} cog={d} stage=collision kind={t} address=0x{x}\n", .{ cog.hub.counter, cog.id, kind, address }) catch {};
}

/// Push a hardware-stack value, discarding the oldest of the eight entries.
pub fn push(cog: *Cog, value: u32) void {
    std.mem.copyBackwards(u32, cog.stack[1..], cog.stack[0..7]);
    cog.stack[0] = value;
}

/// Pop the hardware stack; its bottom entry remains sticky after repeated pops.
pub fn pop(cog: *Cog) u32 {
    const value = cog.stack[0];
    // Entry 7 stays unchanged, so repeated pops eventually repeat that value.
    std.mem.copyForwards(u32, cog.stack[0..7], cog.stack[1..]);
    return value;
}

/// Push the current return address and flags, then redirect instruction fetch.
pub fn call(cog: *Cog, target: u20) void {
    cog.push(cog.return_address());
    if (cog.skip.pattern != 0) {
        cog.skip.depth +%= 1;
    }
    cog.jump(target);
}

/// Pack C/Z and the next execution address; local PCs advance by longs and hub PCs by bytes.
pub fn return_address(cog: *Cog) u32 {
    const injected = if (cog.pipeline[3]) |state|
        state.interrupt != null
    else
        false;
    const pc = if (injected)
        cog.dispatch_pc
    else
        cog.next_pc();
    return (@as(u32, @intFromBool(cog.c)) << 31) | (@as(u32, @intFromBool(cog.z)) << 30) | pc;
}

/// Latch a sampled event and notify idle interrupt levels selecting that source.
pub fn raise_event(cog: *Cog, event: EventId) void {
    cog.events.insert(event);
    if (cog.debug.control & (1 << 9) != 0 and @as(u4, @truncate(cog.debug.control >> 12)) == @intFromEnum(event)) {
        cog.request_debug(false);
    }
    for (&cog.interrupts.levels.values) |*level| {
        if (level.source == event and level.state == .idle) {
            level.state = .pending;
            level.pending_since = cog.hub.counter;
        }
    }
}

/// WAITINT samples pending requests, independently of its pollable INT latch and STALLI mask.
/// Other waits sample their ordinary event latch. This distinction is measured on silicon.
pub fn event_for_wait(cog: *const Cog, event: EventId) bool {
    if (event == .INT) {
        for (cog.interrupts.levels.values) |level| {
            if (level.state == .pending)
                return true;
        }
        return false;
    }
    return cog.events.contains(event);
}

/// Return the measured stalled-wait resume delay; FBW has a direct FIFO wake path.
pub fn event_wait_resume_clocks(_: *const Cog, event: EventId, occurred: bool) u2 {
    return if (event == .FBW)
        (if (occurred)
            @as(u2, 0)
        else
            1)
    else
        2;
}

/// Select the low-four-bit event ID; zero disables the source without selecting INT.
pub fn configure_interrupt(cog: *Cog, level: InterruptLevel, value: u32) void {
    const source: EventId = @enumFromInt(@as(u4, @truncate(value)));
    cog.interrupts.levels.getPtr(level).source = if (source == .INT)
        null
    else
        source;
}

/// Force an idle interrupt into its pending state, retaining STALLI's entry mask.
pub fn trigger_interrupt(cog: *Cog, level: InterruptLevel) void {
    const slot = cog.interrupts.levels.getPtr(level);
    if (slot.state == .idle) {
        slot.state = .pending;
        slot.pending_since = cog.hub.counter;
    }
}

/// Clear the interrupt state, including an executing level, without changing control flow.
pub fn cancel_interrupt(cog: *Cog, level: InterruptLevel) void {
    const slot = cog.interrupts.levels.getPtr(level);
    slot.state = .idle;
}

/// Recognize RETI/RESI and equivalent successful register-source CALLD WCZ instructions.
pub fn complete_interrupt(cog: *Cog, source: Register, immediate: bool, wcz: bool) void {
    if (immediate or !wcz)
        return;
    const level: InterruptLevel = switch (source) {
        .IRET1 => .int1,
        .IRET2 => .int2,
        .IRET3 => .int3,
        else => return,
    };
    cog.interrupts.levels.getPtr(level).state = .idle;
}

/// Report ordinary cog interrupt status; startup mode survives subsequent execution-mode changes.
pub fn interrupt_status(cog: *const Cog) u32 {
    var value: u32 = @intFromBool(cog.started_no_load);
    value |= @as(u32, @intFromBool(cog.interrupts.stalled)) << 1;
    if (cog.fifo_state.engine) |fifo| {
        value |= @as(u32, @intFromBool(fifo.mode == .write)) << 20;
    }
    for (cog.interrupts.levels.values, 0..) |level, index| {
        value |= @as(u32, @intFromEnum(level.state)) << @intCast(2 + 2 * index);
        if (level.source) |source| {
            value |= @as(u32, @intFromEnum(source)) << @intCast(8 + 4 * index);
        }
    }
    if (cog.debug.state == .active) {
        value |= (@as(u32, cog.debug.code) << 24) | (@as(u32, @intFromBool(cog.debug.coginit)) << 23);
    }
    return value;
}

/// Read event traps, skip mode/depth and LUT/stack status without consuming latches.
pub fn event_status(cog: *const Cog) u32 {
    var value: u32 = (@as(u32, cog.skip.depth) << 28) | (@as(u32, @intFromEnum(cog.xbyte_mode)) << 16);
    if (cog.skip.mode == .cancel) {
        value |= 1 << 27;
    }
    for (std.enums.values(EventId)) |event| {
        if (cog.events.contains(event)) {
            value |= @as(u32, 1) << @intFromEnum(event);
        }
    }
    if (cog.lut_sharing) {
        value |= 1 << 26;
    }
    if (cog.stack[0] == 0x1ff) {
        value |= 1 << 25;
    }
    return value;
}

/// Protect prefix/forwarding instructions until the following instruction can consume their state.
fn shields_interrupt(cog: *Cog, state: PipelineState) bool {
    if (state.cancelled)
        return false;
    if (state.instr != 0 and !cog.is_condition_met(@enumFromInt(@as(u4, @truncate(state.instr >> 28)))))
        return false;
    return switch (decode.decode(state.instr)) {
        .altsn,
        .altgn,
        .altsb,
        .altgb,
        .altsw,
        .altgw,
        .altr,
        .altd,
        .alts,
        .altb,
        .alti,
        .crcnib,
        .sca,
        .scas,
        .getxacc,
        .setq,
        .setq2,
        .xoro32,
        .augs,
        .augd,
        .rep,
        .stalli,
        => true,
        .getct => state.instr & (1 << 20) != 0,
        else => false,
    };
}

const PrefixConsumers = struct { s: bool = false, d: bool = false, q: bool = false };

/// Find augmentation consumers already ahead of an injected CALLD, using the decoder's field metadata.
fn augmentation_consumers(cog: *Cog) PrefixConsumers {
    const masks = comptime blk: {
        @setEvalBranchQuota(20_000);
        var result: std.EnumArray(decode.OpCode, struct { s: u32 = 0, d: u32 = 0 }) = .initFill(.{});
        for (std.enums.values(decode.OpCode)) |opcode| {
            const field = decode.instruction_type.get(opcode);
            if (field.len == 0)
                continue;
            const Args = @FieldType(encoding.Instruction, field);
            if (@hasField(Args, "s_imm")) {
                result.getPtr(opcode).s = 1 << @bitOffsetOf(Args, "s_imm");
            }
            if (@hasField(Args, "d_imm")) {
                result.getPtr(opcode).d = 1 << @bitOffsetOf(Args, "d_imm");
            }
        }
        break :blk result;
    };
    var consumed: PrefixConsumers = .{};
    for (cog.pipeline[1..4]) |instruction| {
        if (instruction) |state| {
            consumed.q = consumed.q or !cog.shields_interrupt(state);
            if (state.instr != 0 and
                !cog.is_condition_met(@enumFromInt(@as(u4, @truncate(state.instr >> 28)))))
                continue;
            const mask = masks.get(decode.decode(state.instr));
            consumed.s = consumed.s or state.instr & mask.s != 0;
            consumed.d = consumed.d or state.instr & mask.d != 0;
        }
    }
    return consumed;
}

/// Insert an ordinary CALLD in an available pipeline slot while holding fetch PC and hub FIFO bytes.
fn inject_interrupt(cog: *Cog) ?PipelineState {
    if (!cog.issue_phase or cog.repeat != null or cog.xbyte != null or cog.debug.state == .active)
        return null;
    const is_debug = cog.debug.state == .pending;
    if (!is_debug and cog.interrupts.stalled)
        return null;
    const level: InterruptLevel = pending: {
        if (is_debug)
            break :pending .int1;
        for (std.enums.values(InterruptLevel)) |level| {
            const slot = cog.interrupts.levels.get(level);
            if (slot.state == .active)
                return null;
            if (slot.state != .pending)
                continue;
            if (slot.pending_since == cog.hub.counter)
                return null;
            break :pending level;
        }
        return null;
    };
    for (cog.pipeline) |instruction| {
        if (instruction) |state| {
            if (state.interrupt != null or
                (cog.shields_interrupt(state) and
                    !(is_debug and
                        decode.decode(state.instr) == .stalli)))
                return null;
        }
    }
    const consumed = cog.augmentation_consumers();
    if ((cog.augs_pending and !consumed.s) or (cog.augd_pending and !consumed.d) or
        (cog.setq_pending and !consumed.q))
        return null;
    const source: Register = if (is_debug)
        .INA
    else switch (level) {
        .int1 => .IJMP1,
        .int2 => .IJMP2,
        .int3 => .IJMP3,
    };
    const dest: Register = if (is_debug)
        .INB
    else switch (level) {
        .int1 => .IRET1,
        .int2 => .IRET2,
        .int3 => .IRET3,
    };
    return .{
        .pc = cog.pc,
        .instr = 0xFB38_0000 | (@as(u32, @intFromEnum(dest)) << 9) | @intFromEnum(source),
        .interrupt = if (is_debug)
            .int0
        else
            @enumFromInt(@intFromEnum(level) + 1),
    };
}

/// Coalesce requests until entry; the configuration is cleared only when INT0 executes.
pub fn request_debug(cog: *Cog, coginit: bool) void {
    if (cog.hub.debug.enables & (@as(u8, 1) << cog.id) == 0 or cog.debug.state != .idle)
        return;
    cog.debug.state = .pending;
    cog.debug.coginit = coginit;
    cog.debug.pending_since = cog.hub.counter;
}

/// Hold fetch while the injected debug CALLD drains behind older instructions.
fn debug_queued(cog: *const Cog) bool {
    for (cog.pipeline) |slot| {
        if (slot) |state| {
            if (state.interrupt == .int0)
                return true;
        }
    }
    return false;
}

/// Configure the next debug entry, or update the application's break code.
pub fn breakpoint(cog: *Cog, value: u32, condition: bool) void {
    if (cog.debug.state == .active) {
        if (condition) {
            cog.debug.control = value;
        }
    } else {
        if (condition) {
            cog.debug.code = @truncate(value);
        }
        if (cog.debug.control & (1 << 4) != 0) {
            cog.request_debug(false);
        }
    }
}

/// Send an asynchronous break only from a debug ISR to a valid enabled target.
pub fn break_cog(cog: *Cog, value: u32) void {
    const index: u4 = @truncate(value);
    if (cog.debug.state != .active or index >= cog.hub.cogs.len)
        return;
    const target = &cog.hub.cogs[index];
    if (target.debug.control & (1 << 8) != 0) {
        target.request_debug(false);
    }
}

/// Finish a successful register-source CALLD through IRET0, preserving an untouched hub FIFO.
pub fn return_debug(cog: *Cog, source: Register, immediate: bool, wcz: bool, target: u20) bool {
    if (cog.debug.state != .active or
        cog.debug.control & (1 << 11) != 0 or
        source != .INB or
        immediate or
        !wcz)
        return false;
    cog.debug.state = .idle;
    if (target == cog.debug.return_pc and cog.debug.return_mode == .hub and cog.debug.fifo_untouched) {
        cog.pc = target;
        cog.exec_mode = .hub;
        @memset(cog.pipeline[0..3], null);
        cog.issue_phase = false;
        cog.branched = true;
    } else {
        cog.jump(target);
    }
    return true;
}

/// Apply ALTR/ALTI result redirection or cancellation before scheduling a register write.
pub fn write_result(cog: *Cog, reg: Register, value: u32) void {
    if (cog.pipeline[3]) |state| {
        if (state.no_result)
            return;
        cog.write_reg(state.alt_r orelse reg, value);
    } else {
        cog.write_reg(reg, value);
    }
}

/// Arm a counter sensor and clear its event until the comparison becomes true again.
pub fn set_counter_target(cog: *Cog, event: EventId, value: u32) void {
    std.debug.assert(event == .CT1 or event == .CT2 or event == .CT3);
    cog.ct_targets[@intFromEnum(event) - @intFromEnum(EventId.CT1)] = value;
    cog.events.remove(event);
}

/// Capture the operands and C/Z-selected comparison, clear PAT, and establish its edge baseline.
pub fn configure_pattern(cog: *Cog, mask: u32, value: u32) void {
    cog.pattern = .{
        .input = if (cog.c)
            .INB
        else
            .INA,
        .mask = mask,
        .value = value,
        .equal = cog.z,
        .matched = false,
    };
    cog.pattern.?.matched = cog.pattern_matches();
    cog.events.remove(.PAT);
}

/// Test the live architectural input register; comparison bits outside the mask remain significant.
pub fn pattern_matches(cog: *Cog) bool {
    const pattern = cog.pattern orelse return false;
    return ((cog.read_reg(pattern.input) & pattern.mask) == pattern.value) == pattern.equal;
}

/// Select a LUT, lock or architectural input sensor and clear only its event latch.
/// Pin reconfiguration establishes an edge baseline without canceling notifications in flight.
pub fn configure_selectable(cog: *Cog, event: EventId, value: u32) void {
    std.debug.assert(event == .SE1 or event == .SE2 or event == .SE3 or event == .SE4);
    const selection = value & 0x1ff;
    const pin: PinIndex = @enumFromInt(@as(u6, @truncate(selection)));
    cog.selectable_events[@intFromEnum(event) - @intFromEnum(EventId.SE1)] = if (selection < 0x40)
        .{ .lut_lock = @intCast(selection) }
    else
        .{ .pin = .{
            .index = pin,
            .mode = if (selection < 0x80)
                .rising
            else if (selection < 0xc0)
                .falling
            else if (selection < 0x100)
                .change
            else if (selection < 0x180)
                .low
            else
                .high,
            .previous = cog.read_input_bit(pin),
        } };
    cog.events.remove(event);
}

/// Read the selected event-bus input, one sample ahead of the architectural IN registers.
fn read_input_bit(cog: *Cog, pin: PinIndex) bool {
    return cog.hub.io.eventInput(pin);
}

/// Consume one CORDIC word, wait for pending work, or signal an empty read while retaining its value.
pub fn read_cordic_result(cog: *Cog, axis: Cordic.Axis) ?u32 {
    if (!cog.cordic_result.ready.get(axis)) {
        if (cog.cordic_result.busy())
            return null;
        cog.raise_event(.QMT);
    }
    cog.cordic_result.ready.set(axis, false);
    return switch (axis) {
        .x => cog.cordic_result.result.x,
        .y => cog.cordic_result.result.y,
    };
}

/// Accept software configuration once, clear FBW, and retain previous writes until they drain.
pub fn begin_fifo(cog: *Cog, mode: FifoMode, config: u32, address: u32) void {
    cog.events.remove(.FBW);
    cog.fifo_state.address = @truncate(address);
    const request: FifoRequest = .{
        .provenance = cog.provenance(),
        .mode = mode,
        .block = .init(config, address),
        .setup = if (mode == .read)
            3
        else
            1,
        .accepted_at = cog.hub.counter,
    };
    if (cog.fifo_state.engine != null and
        cog.fifo_state.engine.?.mode == .write and
        cog.fifo_state.engine.?.write_count != 0)
    {
        cog.fifo_state.request = request;
    } else {
        cog.apply_fifo(request);
    }
}

/// Compatibility entry for the unbounded read configuration used by semantic tests.
pub fn begin_fifo_read(cog: *Cog, address: u32) void {
    cog.begin_fifo(.read, 0, address);
}

/// Install a configuration after the previous write stream has completed its physical transfers.
fn apply_fifo(cog: *Cog, request: FifoRequest) void {
    const initial_ready = request.accepted_at +% request.setup;
    const drained_ready = cog.hub.counter +% 2;
    const ready = if (request.mode == .write)
        initial_ready
    else if (request.software)
        cog.hub.counter +% request.setup
    else if (Hub.clock_reached(drained_ready, initial_ready))
        drained_ready
    else
        initial_ready;
    cog.fifo_state.engine = .{
        .provenance = request.provenance,
        .mode = request.mode,
        .initializing = request.software and request.mode == .read,
        .block = if (request.software)
            request.block
        else
            null,
        .remaining = request.block.longs,
        .byte_offset = @truncate(request.block.address),
        .address = if (request.mode == .read)
            request.block.address & 0xffffc
        else
            request.block.address,
        .ready_at = if (Hub.clock_reached(cog.hub.counter, ready))
            null
        else
            ready,
    };
    cog.fifo_state.request = null;
}

/// Set the descriptor used for the next and subsequent reloads without disturbing buffered data.
pub fn fifo_block(cog: *Cog, config: u32, address: u32) void {
    cog.events.remove(.FBW);
    if (cog.fifo_state.request) |*request| {
        request.block = .init(config, address);
    } else if (cog.fifo_state.engine) |*fifo| {
        fifo.block = .init(config, address);
    }
}

/// Inspect a buffered byte without advancing the architectural pointer or releasing a long.
pub fn peek_fifo(cog: *const Cog, offset: usize) ?u8 {
    const fifo = cog.fifo_state.engine orelse return null;
    const position = @as(usize, fifo.byte_offset) + offset;
    if (position / 4 >= fifo.count)
        return null;
    return @truncate(fifo.words[(fifo.head + position / 4) % fifo.words.len] >> @intCast((position % 4) * 8));
}

/// Consume one to four buffered bytes atomically; direct semantic execution uses the same pointer rules.
pub fn read_fifo(cog: *Cog, size: u3) ?u32 {
    const address = cog.fifo_state.address orelse return null;
    var value: u32 = 0;
    if (cog.fifo_state.request != null or cog.peek_fifo(size - 1) == null)
        return 0;
    const fifo = &cog.fifo_state.engine.?;
    for (0..size) |index| {
        const source = fifo.word_addresses[fifo.head] +% fifo.byte_offset;
        const valid = fifo.word_valid[fifo.head] & (@as(u4, 1) << fifo.byte_offset) != 0;
        const byte = fifo.read_byte().?;
        value |= @as(u32, byte) << @intCast(index * 8);
        if (valid) {
            cog.hub.observe(.hub, null, source & 0x7ffff, .read, byte, .consumed, .ram);
        }
    }
    cog.fifo_state.address = address +% size;
    return value;
}

/// Enqueue a software write; modeled eligibility precedes the actual hub RAM-slice grant.
pub fn write_fifo(cog: *Cog, value: u32, size: u3) bool {
    const address = cog.fifo_state.address orelse return false;
    const fifo = &cog.fifo_state.engine.?;
    if (fifo.write_count + size > fifo.write_bytes.len)
        return false;
    for (0..size) |index| {
        const slot = (fifo.write_head + fifo.write_count + index) % fifo.write_bytes.len;
        fifo.write_provenance[slot] = cog.provenance();
        fifo.write_bytes[slot] = @truncate(value >> @intCast(index * 8));
    }
    fifo.write_pending[(fifo.write_pending_head + 1) % fifo.write_pending.len] += size;
    fifo.write_count += size;
    cog.fifo_state.address = address +% size;
    return true;
}

/// Move one fixed write-eligibility stage after the edge's instructions have enqueued bytes.
pub fn advance_fifo_write_ready(cog: *Cog) void {
    const fifo = if (cog.fifo_state.engine) |*value|
        value
    else
        return;
    if (fifo.mode != .write)
        return;
    fifo.write_ready_count += fifo.write_pending[fifo.write_pending_head];
    fifo.write_pending[fifo.write_pending_head] = 0;
    fifo.write_pending_head = (fifo.write_pending_head + 1) % fifo.write_pending.len;
}

/// Advance the common five-clock hub response arrays after every hub edge.
pub fn advance_read_bus(cog: *Cog) void {
    cog.read_bus.head = (cog.read_bus.head + 1) % 8;
}

/// Raise each selectable LUT/lock event matching this notification, leaving pin sensors independent.
pub fn signal_selectable(cog: *Cog, config: u6) void {
    for (cog.selectable_events, [4]EventId{ .SE1, .SE2, .SE3, .SE4 }) |selection, event| {
        if (selection) |sensor|
            switch (sensor) {
                .lut_lock => |source| if (source == config) {
                    cog.hub.signal_event(cog.id, event);
                },
                .pin => {},
            };
    }
}

/// Return the adjacent even/odd companion cog used by LUT sharing and LUT event sensors.
pub fn other(cog: *Cog) *Cog {
    return &cog.hub.cogs[cog.id ^ 1];
}

/// Return the RAM slice currently available to this cog in the rotating eight-clock hub schedule.
pub fn ram_slice(cog: *Cog) u3 {
    return (cog.hub.counter +% cog.id) % 8;
}

/// Queue an instruction write during execution, or update the live register.
/// The hub forwards the combined cog I/O state once at the next clock boundary.
pub fn write_reg(cog: *Cog, reg: Register, value: u32) void {
    if (cog.collecting_writes) {
        std.debug.assert(cog.writeback.count < cog.writeback.writes.len);
        cog.writeback.writes[cog.writeback.count] = .{
            .reg = reg,
            .value = value,
            .debug_overlay = cog.debug.state == .active and
                cog.debug.control & (1 << 11) == 0 and
                (reg == .INA or
                    reg == .INB),
        };
        cog.writeback.provenance = cog.provenance();
        cog.writeback.count += 1;
        return;
    }
    cog.hub.observe(.registers, cog.id, @intFromEnum(reg), .write, value, .data, .architectural);
    if (cog.debug.state == .active and cog.debug.control & (1 << 11) == 0 and (reg == .INA or reg == .INB)) {
        if (reg == .INA) {
            cog.debug.ijmp = value;
        } else {
            cog.debug.iret = value;
        }
    } else {
        cog.registers.set(reg, value);
    }
}

/// Capture the issuing instruction, or identify a cog-only service such as startup.
pub fn provenance(cog: *const Cog) Debugger.Provenance {
    if (cog.pipeline[3]) |state| {
        return .{ .cog = cog.id, .pc = state.pc, .instruction = state.instr };
    }
    return .{ .cog = cog.id };
}

/// Write underlying cog RAM, including the tail hidden by ordinary special-register access.
pub fn write_ram(cog: *Cog, address: u9, value: u32) void {
    if (cog.collecting_writes) {
        std.debug.assert(cog.writeback.count < cog.writeback.writes.len);
        cog.writeback.writes[cog.writeback.count] = .{ .reg = @enumFromInt(address), .value = value, .raw = true };
        cog.writeback.provenance = cog.provenance();
        cog.writeback.count += 1;
    } else if (address >= 504) {
        cog.hub.observe(.registers, cog.id, address, .write, value, .data, .ram);
        cog.ram_tail[address - 504] = value;
    } else {
        cog.hub.observe(.registers, cog.id, address, .write, value, .data, .ram);
        cog.registers.values[address] = value;
    }
}

/// Read live architectural state; INA/INB are supplied by the pin model rather than stored RAM.
pub fn read_reg(cog: *Cog, reg: Register) u32 {
    if (cog.debug.state == .active and cog.debug.control & (1 << 11) == 0)
        switch (reg) {
            .INA => return cog.debug.ijmp,
            .INB => return cog.debug.iret,
            else => {},
        };
    return switch (reg) {
        .INA => @truncate(cog.hub.io.get_in() >> 0),
        .INB => @truncate(cog.hub.io.get_in() >> 32),
        else => cog.registers.get(reg),
    };
}

/// Only execution reads use latched data; debugger/test reads see live state.
pub fn read_operand(cog: *Cog, reg: Register) u32 {
    const value = cog.operand_value(reg);
    const class: Debugger.ReadClass = if (cog.pipeline[3] != null and cog.pipeline[3].?.operands != null) .consumed else .data;
    cog.hub.observe(.registers, cog.id, @intFromEnum(reg), .read, value, class, .architectural);
    return value;
}

/// Resolve captured operand values without reporting an inspection read.
fn operand_value(cog: *Cog, reg: Register) u32 {
    if (cog.pipeline[3]) |state| {
        if (state.operands) |values| {
            const d: Register = state.alt_d orelse @enumFromInt(@as(u9, @truncate(state.instr >> 9)));
            const s: Register = state.alt_s orelse @enumFromInt(@as(u9, @truncate(state.instr)));
            if (reg == d)
                return values.d;
            if (reg == s)
                return values.s;
            return switch (reg) {
                .PTRA => values.ptra,
                .PTRB => values.ptrb,
                .PA => values.pa,
                .PB => values.pb,
                else => cog.read_reg(reg),
            };
        }
    }
    return cog.read_reg(reg);
}

/// Latch D/S and pointer/parameter operands after earlier result writes, honoring alternate D/S addresses.
fn capture_operands(cog: *Cog, state: *PipelineState) void {
    state.operands = .{
        .d = cog.read_reg(state.alt_d orelse @enumFromInt(@as(u9, @truncate(state.instr >> 9)))),
        .s = cog.read_reg(state.alt_s orelse @enumFromInt(@as(u9, @truncate(state.instr)))),
        .ptra = cog.read_reg(.PTRA),
        .ptrb = cog.read_reg(.PTRB),
        .pa = cog.read_reg(.PA),
        .pb = cog.read_reg(.PB),
    };
    const addresses = [_]Register{
        state.alt_d orelse @enumFromInt(@as(u9, @truncate(state.instr >> 9))),
        state.alt_s orelse @enumFromInt(@as(u9, @truncate(state.instr))),
        .PTRA,
        .PTRB,
        .PA,
        .PB,
    };
    const values = state.operands.?;
    const saved = cog.hub.access_context;
    cog.hub.access_context = .{ .cog = cog.id, .pc = state.pc, .instruction = state.instr };
    for (addresses, [_]u32{ values.d, values.s, values.ptra, values.ptrb, values.pa, values.pb }) |reg, value| {
        cog.hub.observe(.registers, cog.id, @intFromEnum(reg), .read, value, .physical, .architectural);
    }
    cog.hub.access_context = saved;
    if (state.interrupt == .int0) {
        state.operands.?.s = cog.debug.ijmp;
        state.operands.?.d = cog.debug.iret;
    }
    if (state.debug_rom) {
        const base: u32 = 0xff800 + ((15 ^ @as(u32, cog.id)) << 7);
        state.operands.?.ptra = base;
        state.operands.?.ptrb = base + 0x40;
        const d: Register = @enumFromInt(@as(u9, @truncate(state.instr >> 9)));
        const s: Register = @enumFromInt(@as(u9, @truncate(state.instr)));
        if (d == .PTRA) {
            state.operands.?.d = base;
        }
        if (d == .PTRB) {
            state.operands.?.d = base + 0x40;
        }
        if (s == .PTRA) {
            state.operands.?.s = base;
        }
        if (s == .PTRB) {
            state.operands.?.s = base + 0x40;
        }
    }
}

/// Find the next instruction behind execution for ALTx modification and next-source forwarding.
pub fn following_instruction(cog: *Cog) ?*PipelineState {
    var i: usize = 3;
    while (i > 0) {
        i -= 1;
        if (cog.pipeline[i]) |*state|
            return state;
    }
    return null;
}

/// Queue a local LUT write and its sensor event, or apply an untimed write with receiver-controlled sharing.
pub fn write_lut(cog: *Cog, addr: u9, value: u32) void {
    if (cog.collecting_writes) {
        std.debug.assert(cog.writeback.lut == null);
        cog.writeback.provenance = cog.provenance();
        cog.writeback.lut = .{ .address = addr, .value = value };
        if (addr >= 0x1fc) {
            cog.signal_selectable(4 | (@as(u6, @truncate(addr)) & 3));
        }
        return;
    }
    cog.lut[addr] = value;
    cog.hub.observe(.lut, cog.id, addr, .write, value, .data, .ram);
    if (!cog.hub.clocking and cog.other().lut_sharing)
        cog.other().lut[addr] = value;
    if (addr >= 0x1fc) {
        // Own writes signal from the execution request; a companion sees the
        // physical RAM commit a clock later. Direct semantic calls are untimed.
        if (!cog.hub.clocking) {
            cog.signal_selectable(4 | (@as(u6, @truncate(addr)) & 3));
        }
        cog.other().signal_selectable(12 | (@as(u6, @truncate(addr)) & 3));
    }
}

/// Read local LUT RAM and notify local/companion selectable sensors for the four monitored addresses.
pub fn read_lut(cog: *Cog, addr: u9) u32 {
    if (addr >= 0x1fc) {
        cog.signal_selectable(@as(u6, @truncate(addr)) & 3);
        cog.other().signal_selectable(8 | (@as(u6, @truncate(addr)) & 3));
    }
    const value = cog.lut[addr];
    cog.hub.observe(.lut, cog.id, addr, .read, value, .data, .ram);
    return value;
}

/// Checks if `cond` would currently apply to this cog or not.
pub fn is_condition_met(cog: *Cog, cond: types.Condition) bool {
    return switch (cond) {
        ._RET_ => true,
        .IF_NC_AND_NZ => !cog.c and !cog.z,
        .IF_NC_AND_Z => !cog.c and cog.z,
        .IF_NC => !cog.c,
        .IF_C_AND_NZ => cog.c and !cog.z,
        .IF_NZ => !cog.z,
        .IF_C_NE_Z => (cog.c != cog.z),
        .IF_NC_OR_NZ => !cog.c or !cog.z,
        .IF_C_AND_Z => cog.c and cog.z,
        .IF_C_EQ_Z => (cog.c == cog.z),
        .IF_Z => cog.z,
        .IF_NC_OR_Z => !cog.c or cog.z,
        .IF_C => cog.c,
        .IF_C_OR_NZ => cog.c or !cog.z,
        .IF_C_OR_Z => cog.c or cog.z,
        .IF_ALWAYS => true,
    };
}

/// Resolve a nine-bit immediate or live register for direct semantic callers.
pub fn resolve_operand(cog: *Cog, reg: Register, imm: bool) u32 {
    return if (imm)
        @intFromEnum(reg)
    else
        return cog.read_reg(reg);
}

pub const PipelineState = struct {
    pc: u20,
    instr: u32,
    /// One encounter survives all execute stalls and command scheduling retries.
    debugger_attempted: bool = false,
    /// Physical local instruction source; null for hub fetches and injected instructions.
    local_address: ?u10 = null,
    /// Fallthrough after repeat wrap and fast skip stepping, captured at read/decode.
    next_pc: ?u20 = null,
    cancelled: bool = false,
    repeat_target: ?u20 = null,
    interrupt: ?InjectedInterrupt = null,
    fifo_tail: bool = false,
    debug_rom: bool = false,
    fifo_started: bool = false,
    fifo_nonblocking: bool = false,
    fifo_write_value: ?u32 = null,

    alt_r: ?Register = null,
    alt_s: ?Register = null,
    alt_d: ?Register = null,
    /// Full source value forwarded by SCA, SCAS or XORO32 to this instruction.
    s_value: ?u32 = null,
    no_result: bool = false,
    operands: ?struct { d: u32, s: u32, ptra: u32, ptrb: u32, pa: u32, pb: u32 } = null,
    ready_at: ?u64 = null,
    execution_ready: bool = false,
    waited_for_event: bool = false,
    event_resume_at: ?u64 = null,
    event_result: bool = false,
    command_at: ?u64 = null,
    command_result: ?ExecResult = null,
    command_writeback: Writeback = .{},
};

/// Fetches the last value set up by 'AUGS' and resets the value.
pub fn fetch_augs(cog: *Cog) u32 {
    const augs = cog.augs;
    cog.augs = 0;
    cog.augs_pending = false;
    return augs;
}

/// Fetches the last value set up by 'AUGD' and resets the value.
pub fn fetch_augd(cog: *Cog) u32 {
    const augd = cog.augd;
    cog.augd = 0;
    cog.augd_pending = false;
    return augd;
}

/// Select the local RAM read port: overflow leaves it on the preceding instruction's S address.
pub fn local_fetch_address(cog: *const Cog) u10 {
    if (cog.pc & 0x200 != 0)
        return @as(u10, @truncate(cog.pc));
    if (cog.pc < 0x200)
        return @intCast(cog.pc);
    if (cog.pipeline[2]) |state| {
        if (state.alt_s) |reg|
            return @intFromEnum(reg);
        return @as(u9, @truncate(state.instr));
    }
    return 0;
}

/// Overflow reuses the S read port, which returns old RAM data on a same-edge result write.
fn overflow_instruction(cog: *Cog, address: u9) u32 {
    if (cog.ram_collision) |collision| {
        if (collision.clock == cog.hub.counter and collision.address == address)
            return collision.value;
    }
    return cog.read_reg(@enumFromInt(address));
}

/// Fetch local cog/LUT code or consume a hub FIFO slice without changing mode on sequential PC wrap.
/// Mark execute-only debug-ROM fetches so only instructions that reach execution report missing support.
fn fetch_instruction(cog: *Cog) ?PipelineState {
    switch (cog.exec_mode) {
        .stopped => unreachable,
        .cog => {
            const address = cog.local_fetch_address();
            const rom = cog.pc >= 504 and cog.pc < 512;
            const instr: PipelineState = .{
                .pc = cog.pc,
                .local_address = address,
                .debug_rom = rom,
                .instr = if (address >= 0x200)
                    cog.lut[address - 0x200]
                else if (rom)
                    debug_rom[address - 504]
                else if (cog.pc >= 0x400)
                    cog.overflow_instruction(@intCast(address))
                else
                    cog.registers.values[address],
            };
            const saved = cog.hub.access_context;
            cog.hub.access_context = .{ .cog = cog.id, .pc = instr.pc, .instruction = instr.instr };
            cog.hub.observe(if (address < 0x200) .registers else .lut, cog.id, address & 0x1ff, .read, instr.instr, .physical, if (rom) .execute_rom else .ram);
            cog.hub.access_context = saved;
            cog.pc +%= 1;
            return instr;
        },
        .hub => {
            if (cog.fifo_state.request != null or
                (cog.fifo_state.engine != null and
                    cog.fifo_state.engine.?.mode != .read))
                return null;
            if (cog.fifo_state.engine == null) {
                cog.fifo_state.engine = .{
                    .byte_offset = @truncate(cog.pc),
                    .address = cog.pc & 0xffffc,
                    .ready_at = cog.hub.counter +% 5,
                };
                return null;
            }
            const word = cog.fifo_state.engine.?.read() orelse return null;
            const instr: PipelineState = .{
                .pc = cog.pc,
                .instr = word,
                .fifo_tail = cog.fifo_state.engine.?.byte_offset != 0,
            };
            cog.pc +%= 4;
            return instr;
        },
    }
}

pub const ExecResult = union(enum) {
    wait,
    next,
    trap,
    skip,
    not_implemented,
    illegal: []const u8,
};

pub const Fault = struct {
    cog: u3,
    pc: u20,
    instruction: u32,
    result: ExecResult,
    debug_rom: bool = false,

    /// Return the diagnostic text for a trap, missing implementation or reason-bearing illegal execution.
    pub fn reason(fault: Fault) []const u8 {
        return switch (fault.result) {
            .trap => "execution trap",
            .not_implemented => if (fault.debug_rom)
                "debug ROM execution is not implemented"
            else
                "instruction is not implemented",
            .illegal => |message| message,
            else => unreachable,
        };
    }
};

test "XBYTE sentinel is exact, POP consumes it, and direct jumps still fetch debug ROM" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    hub.init();
    const cog = &hub.cogs[0];
    cog.exec_mode = .cog;
    for ([_]u32{ 0x8000_01ff, 0x4000_01ff, 0x0010_01ff }) |value| {
        cog.stack[0] = value;
        cog.stack[1] = 0x123;
        try std.testing.expectEqual(ExecResult.next, cog.return_stack());
        try std.testing.expect(cog.xbyte == null);
        try std.testing.expectEqual(@as(u32, 0x123), cog.stack[0]);
    }
    cog.stack[0] = 0x1ff;
    try std.testing.expectEqual(@as(u32, 0x1ff), cog.pop());
    cog.jump(0x1ff);
    const instruction = cog.fetch_instruction().?;
    try std.testing.expect(instruction.debug_rom);
    try std.testing.expectEqual(debug_rom[7], instruction.instr);
    try std.testing.expect(cog.xbyte == null);
}

test "reset cancels every XBYTE phase and mode while retaining LUT RAM" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    for (0..4) |clocks| {
        hub.init();
        const cog = &hub.cogs[0];
        cog.exec_mode = .cog;
        cog.xbyte_mode = @enumFromInt(0x101);
        cog.xbyte_default_mode = cog.xbyte_mode;
        cog.begin_fifo_read(0x1000);
        cog.stack[0] = 0x1ff;
        cog.lut[0x100] = 0x20;
        try std.testing.expectEqual(ExecResult.next, cog.return_stack());
        for (0..clocks) |_| {
            _ = hub.step();
        }
        try std.testing.expect(cog.xbyte != null);
        cog.reset();
        try std.testing.expect(cog.xbyte == null);
        try std.testing.expectEqual(@as(u9, 0), @intFromEnum(cog.xbyte_mode));
        try std.testing.expectEqual(@as(u9, 0), @intFromEnum(cog.xbyte_default_mode));
        try std.testing.expectEqual(@as(u32, 0x20), cog.lut[0x100]);
        try std.testing.expectEqual(ExecMode.stopped, cog.exec_mode);
    }
}

test "five clock passage delays writeback and forwards to the following operand latch" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    hub.init();
    const cog = &hub.cogs[0];
    cog.exec_mode = .cog;
    // MOV r20,#7; ADD r20,#1.
    cog.registers.values[0] = 0xF604_2807;
    cog.registers.values[1] = 0xF104_2801;
    for (1..5) |stage| {
        _ = hub.step();
        try std.testing.expectEqual(@as(u20, 0), cog.pipeline[stage].?.pc);
        try std.testing.expectEqual(@as(u32, 0), cog.registers.values[20]);
    }
    _ = hub.step();
    try std.testing.expectEqual(@as(u32, 7), cog.registers.values[20]);
    try std.testing.expectEqual(@as(u20, 0), cog.retired.?.pc);
    try std.testing.expectEqual(@as(u20, 1), cog.pipeline[3].?.pc);
    try std.testing.expectEqual(@as(u32, 7), cog.pipeline[3].?.operands.?.d);
    _ = hub.step();
    try std.testing.expectEqual(@as(u32, 7), cog.registers.values[20]);
    _ = hub.step();
    try std.testing.expectEqual(@as(u32, 8), cog.registers.values[20]);
}

test "execute-only ROM fetch differs from underlying tail RAM and discarded fetches are harmless" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    hub.init();
    const cog = &hub.cogs[0];
    cog.exec_mode = .cog;
    cog.pc = 504;
    cog.write_ram(504, 0xdead_beef);
    const instruction = cog.fetch_instruction().?;
    try std.testing.expect(instruction.debug_rom);
    try std.testing.expectEqual(debug_rom[0], instruction.instr);
    try std.testing.expectEqual(@as(u32, 0xdead_beef), cog.ram_tail[0]);
    cog.pc = 502;
    cog.registers.values[502] = 0xFD80_0000; // JMP #0, younger ROM fetch discarded.
    for (0..12) |_| {
        _ = hub.step();
    }
    try std.testing.expect(hub.fault == null);
    try std.testing.expectEqual(ExecMode.cog, cog.exec_mode);
}

test "illegal execution stops the cog and discards pending pipeline writes" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    hub.init();
    const cog = &hub.cogs[0];
    cog.exec_mode = .hub;
    // RFBYTE r20: the instruction FIFO cannot also serve software reads.
    cog.pipeline[3] = .{ .pc = 0x1000, .instr = 0xFD60_2810 };
    cog.pipeline[2] = .{ .pc = 0x1004, .instr = 0xF604_2807 };
    _ = hub.step();
    const fault = hub.fault.?;
    try std.testing.expectEqualStrings("hub execution uses the FIFO for instruction fetching", fault.result.illegal);
    try std.testing.expectEqual(@as(u20, 0x1000), fault.pc);
    try std.testing.expectEqual(ExecMode.stopped, cog.exec_mode);
    for (cog.pipeline) |stage| {
        try std.testing.expect(stage == null);
    }
    try std.testing.expectEqual(@as(usize, 0), cog.writeback.count);
    for (0..8) |_| {
        _ = hub.step();
    }
    try std.testing.expectEqual(@as(u32, 0), cog.registers.values[20]);
}

test "stop discards startup, FIFO, transfers and queued results but preserves RAM" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    hub.init();
    try hub.start_cog(1, .{});
    const cog = &hub.cogs[1];
    cog.registers.values[100] = 77;
    cog.lut[100] = 88;
    cog.pipeline[3] = .{ .pc = 0, .instr = 0 };
    cog.writeback = .{ .count = 1 };
    cog.fifo_state.engine = .{ .address = 0, .ready_at = 0, .byte_offset = 0 };
    cog.repeat = .{ .start = 10, .length = 3, .left = 2, .iterations = 0 };
    cog.skip = .{ .mode = .cancel, .pattern = 0xf0, .depth = 3 };
    cog.reset();
    try std.testing.expect(cog.startup == null);
    try std.testing.expect(cog.fifo_state.engine == null);
    try std.testing.expect(cog.memory_transfer == null);
    try std.testing.expect(cog.pipeline[3] == null);
    try std.testing.expect(cog.repeat == null);
    try std.testing.expectEqual(Skip{}, cog.skip);
    try std.testing.expectEqual(@as(u2, 0), cog.writeback.count);
    for (0..32) |_| {
        _ = hub.step();
    }
    try std.testing.expectEqual(@as(u32, 77), cog.registers.values[100]);
    try std.testing.expectEqual(@as(u32, 88), cog.lut[100]);
}

test "COGINIT allocates free cogs and pairs, reports failure, and retains RAM on restart" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    hub.init();
    const cog = &hub.cogs[0];
    cog.exec_mode = .cog;
    cog.write_reg(@enumFromInt(20), 0x30); // Start a free cog without loading RAM.
    cog.write_reg(@enumFromInt(21), 0x4000_0210);
    hub.cogs[1].write_reg(@enumFromInt(100), 0x1234);
    hub.cogs[1].lut[16] = 0x5678;
    hub.cogs[1].repeat = .{ .start = 10, .length = 3, .left = 2, .iterations = 0 };
    hub.cogs[1].skip = .{ .mode = .cancel, .pattern = 0xf0, .depth = 3 };
    cog.q = 0x8765_4321;
    cog.setq_pending = true;
    const start = 0xFCE0_0000 | (1 << 20) | (20 << 9) | 21; // COGINIT r20, r21 WC
    try std.testing.expectEqual(Cog.ExecResult.next, execute.execute_instruction(cog, .{ .pc = 0, .instr = start }));
    try std.testing.expectEqual(@as(u32, 1), cog.read_reg(@enumFromInt(20)));
    try std.testing.expect(!cog.c);
    try std.testing.expectEqual(Cog.ExecMode.cog, hub.cogs[1].exec_mode);
    try std.testing.expectEqual(@as(u20, 0x210), hub.cogs[1].pc);
    try std.testing.expectEqual(@as(u32, 0x4000_0210), hub.cogs[1].read_reg(.PTRB));
    try std.testing.expectEqual(@as(u32, 0x8765_4321), hub.cogs[1].read_reg(.PTRA));
    try std.testing.expectEqual(@as(u32, 0x1234), hub.cogs[1].read_reg(@enumFromInt(100)));
    try std.testing.expectEqual(@as(u32, 0x5678), hub.cogs[1].lut[16]);
    try std.testing.expect(hub.cogs[1].repeat == null);
    try std.testing.expectEqual(Skip{}, hub.cogs[1].skip);
    cog.write_reg(@enumFromInt(20), 0x31); // First available even/odd pair is 2,3.
    // This direct semantic call bypasses the pipeline's unprefixed Q clear.
    cog.q = 0;
    try std.testing.expectEqual(Cog.ExecResult.next, execute.execute_instruction(cog, .{ .pc = 0, .instr = start }));
    try std.testing.expectEqual(@as(u32, 2), cog.read_reg(@enumFromInt(20)));
    try std.testing.expectEqual(Cog.ExecMode.cog, hub.cogs[3].exec_mode);
    try std.testing.expectEqual(@as(u32, 0), cog.q);
    for (&hub.cogs) |*active| {
        active.exec_mode = .cog;
    }
    cog.write_reg(@enumFromInt(20), 0x30);
    try std.testing.expectEqual(Cog.ExecResult.next, execute.execute_instruction(cog, .{ .pc = 0, .instr = start }));
    try std.testing.expect(cog.c);
    try std.testing.expectEqual(@as(u32, 15), cog.read_reg(@enumFromInt(20)));

    hub.locks[4] = .{ .id = 4, .allocated = true, .taken = true, .owner = 1 };
    cog.selectable_events[0] = .{ .lut_lock = 0x24 };
    hub.cogs[1].reset();
    try std.testing.expect(!hub.locks[4].taken);
    try std.testing.expect(hub.locks[4].allocated);
    try std.testing.expectEqual(@as(u3, 1), hub.locks[4].owner);
    try std.testing.expect(cog.events.contains(.SE1));
    try std.testing.expectEqual(@as(u32, 0x1234), hub.cogs[1].read_reg(@enumFromInt(100)));
    try std.testing.expectEqual(@as(u32, 0x5678), hub.cogs[1].lut[16]);

    // Loading an image at the end of hub RAM wraps through the physical RAM.
    hub.write_memory(0xffffc, 0x1122_3344, 4, false);
    hub.write_memory(0, 0x5566_7788, 4, false);
    try hub.start_cog(1, .{ .hub_address = 0xffffc, .clocked = false });
    try std.testing.expectEqual(@as(u32, 0x1122_3344), hub.cogs[1].read_reg(@enumFromInt(0)));
    try std.testing.expectEqual(@as(u32, 0x5566_7788), hub.cogs[1].read_reg(@enumFromInt(1)));
}

test "LUT sharing receives companion writes and selectable events track both cogs" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    hub.init();
    const even = &hub.cogs[2];
    const odd = even.other();
    even.lut_sharing = true;
    even.selectable_events = .{ .{ .lut_lock = 12 }, .{ .lut_lock = 8 }, .{ .lut_lock = 7 }, null };
    odd.selectable_events[0] = .{ .lut_lock = 4 };
    odd.write_lut(508, 0x8000_0000);
    try std.testing.expectEqual(@as(u32, 0x8000_0000), even.lut[508]);
    try std.testing.expect(even.events.contains(.SE1));
    try std.testing.expect(odd.events.contains(.SE1));
    _ = odd.read_lut(508);
    try std.testing.expect(even.events.contains(.SE2));
    even.write_lut(511, 99);
    try std.testing.expect(even.events.contains(.SE3));
    try std.testing.expectEqual(@as(u32, 0), odd.lut[511]);
    even.lut_sharing = false;
    odd.write_lut(508, 1);
    try std.testing.expectEqual(@as(u32, 0x8000_0000), even.lut[508]);
}

test "GETCT reads both halves and counter waits cross the 32-bit wrap" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    hub.init();
    const cog = &hub.cogs[0];
    hub.counter = 0x1234_5678_ffff_ffff;
    cog.c = false;
    const high = 0xFD70_001A | (20 << 9); // GETCT r20 WC
    const low = 0xFD60_001A | (21 << 9); // GETCT r21
    try std.testing.expectEqual(Cog.ExecResult.next, execute.execute_instruction(cog, .{ .pc = 0, .instr = high }));
    hub.counter += 1;
    try std.testing.expectEqual(Cog.ExecResult.next, execute.execute_instruction(cog, .{ .pc = 1, .instr = low }));
    try std.testing.expectEqual(@as(u32, 0x1234_5678), cog.read_reg(@enumFromInt(20)));
    try std.testing.expectEqual(@as(u32, 0), cog.read_reg(@enumFromInt(21)));
    try std.testing.expect(!cog.c);
    try std.testing.expectEqual(Cog.ExecResult.next, execute.execute_instruction(cog, .{ .pc = 2, .instr = low }));
    try std.testing.expectEqual(@as(u32, 0), cog.read_reg(@enumFromInt(21)));

    hub.counter = 0xffff_fffe;
    cog.exec_mode = .cog;
    cog.ct_targets[2] = 0;
    _ = hub.step();
    _ = hub.step();
    try std.testing.expect(!cog.events.contains(.CT3));
    _ = hub.step();
    try std.testing.expect(cog.events.contains(.CT3));
    cog.q = 4;
    cog.setq_pending = true;
    const wait = 0xFD70_3C24; // WAITATN WC
    try std.testing.expectEqual(Cog.ExecResult.wait, execute.execute_instruction(cog, .{ .pc = 3, .instr = wait }));
    try std.testing.expect(cog.setq_pending);
    hub.counter = 0x1_0000_0004;
    try std.testing.expectEqual(Cog.ExecResult.next, execute.execute_instruction(cog, .{ .pc = 3, .instr = wait }));
    try std.testing.expect(cog.c);
    try std.testing.expect(!cog.setq_pending);
}

test "counter configuration clears each latch and the reached condition reasserts across rollover" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    for ([_]EventId{ .CT1, .CT2, .CT3 }) |event| {
        hub.init();
        const cog = &hub.cogs[0];
        cog.exec_mode = .cog;
        hub.counter = 0xffff_fffe;
        cog.events = .full;
        cog.set_counter_target(event, 1);
        var remaining = EventSet.full;
        remaining.remove(event);
        try std.testing.expect(cog.events.eql(remaining));
        for (0..3) |_| {
            _ = hub.step();
        }
        try std.testing.expect(!cog.events.contains(event));
        _ = hub.step();
        try std.testing.expect(cog.events.contains(event));
        cog.events.remove(event);
        _ = hub.step();
        try std.testing.expect(cog.events.contains(event));
    }
}

test "selectable configuration masks nine bits and clears only the configured event" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    hub.init();
    const cog = &hub.cogs[0];
    for ([_]EventId{ .SE1, .SE2, .SE3, .SE4 }, 0..) |event, index| {
        cog.events = .full;
        cog.configure_selectable(event, 0xffff_fe24);
        try std.testing.expectEqual(@as(u6, 0x24), cog.selectable_events[index].?.lut_lock);
        var remaining = EventSet.full;
        remaining.remove(event);
        try std.testing.expect(cog.events.eql(remaining));
        cog.events.insert(event);
        cog.configure_selectable(event, 0xffff_ffc0);
        try std.testing.expectEqual(@as(PinIndex, @enumFromInt(0)), cog.selectable_events[index].?.pin.index);
        try std.testing.expect(cog.events.eql(remaining));
    }
}

test "selectable pin sensors sample every architectural input with independent edge and level latches" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    const cases = [_]struct { config: u9, expected: [6]bool }{
        .{ .config = 0x40, .expected = .{ false, true, false, false, false, true } },
        .{ .config = 0x80, .expected = .{ false, false, false, true, false, false } },
        .{ .config = 0xc0, .expected = .{ false, true, false, true, false, true } },
        .{ .config = 0x100, .expected = .{ true, false, false, true, true, false } },
        .{ .config = 0x140, .expected = .{ true, false, false, true, true, false } },
        .{ .config = 0x180, .expected = .{ false, true, true, false, false, true } },
        .{ .config = 0x1c0, .expected = .{ false, true, true, false, false, true } },
    };
    for (0..64) |index| {
        for (cases) |case| {
            hub.init();
            const cog = &hub.cogs[3];
            cog.exec_mode = .cog;
            hub.io.samples.active = true;
            const mask = @as(u64, 1) << @intCast(index);
            for ([_]EventId{ .SE1, .SE2, .SE3, .SE4 }) |event| {
                cog.configure_selectable(event, case.config | @as(u9, @intCast(index)));
            }
            for ([_]bool{ false, true, true, false, false, true }, case.expected) |value, expected| {
                // Every unselected input is opposite: the sensor must read its INA/INB bit.
                hub.io.samples.input_registers = if (value)
                    mask
                else
                    ~mask;
                // Hold the synthetic level through both the event and architectural sampling stages.
                hub.io.samples.inputs = @splat(hub.io.samples.input_registers);
                cog.step();
                for ([_]EventId{ .SE1, .SE2, .SE3, .SE4 }) |event| {
                    try std.testing.expectEqual(expected, cog.events.contains(event));
                    cog.events.remove(event);
                }
            }
            // Selecting another, already-high input establishes a fresh edge baseline.
            hub.io.samples.input_registers = ~mask;
            hub.io.samples.inputs = @splat(~mask);
            const neighbor = @as(u6, @intCast(index)) +% 1;
            cog.configure_selectable(.SE1, 0x40 | @as(u9, neighbor));
            cog.step();
            try std.testing.expect(!cog.events.contains(.SE1));
            // LUT/lock strobes cannot accidentally notify any pin selector.
            cog.events = .empty;
            for (0..64) |source| {
                cog.signal_selectable(@intCast(source));
            }
            try std.testing.expectEqual(@as(usize, 0), cog.events.count());
            cog.reset();
            for (cog.selectable_events) |selection| {
                try std.testing.expect(selection == null);
            }
        }
    }
}

test "RDFAST clears block-wrap only on accepted initialization and not on stalled retries" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    hub.init();
    const cog = &hub.cogs[0];
    cog.exec_mode = .cog;
    const read: PipelineState = .{ .pc = 0, .instr = 0xFC78_0015 }; // RDFAST #0,r21.
    cog.events = .full;
    try std.testing.expectEqual(ExecResult.next, execute.execute_instruction(cog, read));
    var remaining = EventSet.full;
    remaining.remove(.FBW);
    try std.testing.expect(cog.events.eql(remaining));

    cog.pipeline[3] = read;
    cog.collecting_writes = true;
    defer cog.collecting_writes = false;

    cog.events.insert(.FBW);
    try std.testing.expectEqual(ExecResult.wait, execute.execute_instruction(cog, read));
    try std.testing.expect(!cog.events.contains(.FBW));
    cog.events.insert(.FBW);
    try std.testing.expectEqual(ExecResult.wait, execute.execute_instruction(cog, read));
    try std.testing.expect(cog.events.contains(.FBW));
    cog.fifo_state.engine.?.count = 1;
    cog.fifo_state.engine.?.ready_at = null;
    try std.testing.expectEqual(ExecResult.next, execute.execute_instruction(cog, read));
    try std.testing.expect(cog.events.contains(.FBW));

    // A finite block is accepted and clears the latch just like an unbounded stream.
    cog.pipeline[3] = null;
    cog.collecting_writes = false;
    cog.write_reg(@enumFromInt(20), 1);
    try std.testing.expectEqual(ExecResult.next, execute.execute_instruction(cog, .{ .pc = 0, .instr = 0xFC70_2815 })); // RDFAST r20,r21.
    try std.testing.expect(!cog.events.contains(.FBW));
    try std.testing.expectEqual(@as(u19, 16), cog.fifo_state.engine.?.remaining);
}

test "FBLOCK replaces the descriptor of a configuration still draining prior writes" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    hub.init();
    const cog = &hub.cogs[0];
    cog.begin_fifo(.write, 0, 0x1000);
    try std.testing.expect(cog.write_fifo(0x11223344, 4));
    cog.begin_fifo(.read, 1, 0x2000);
    cog.fifo_block(2, 0x3000);
    try std.testing.expectEqual(FifoMode.write, cog.fifo_state.engine.?.mode);
    try std.testing.expectEqual(@as(u20, 0x3000), cog.fifo_state.request.?.block.address);
    while (cog.fifo_state.request != null) {
        _ = hub.step();
    }
    try std.testing.expectEqual(@as(u32, 0x11223344), hub.read_memory(0x1000, 4));
    try std.testing.expectEqual(FifoMode.read, cog.fifo_state.engine.?.mode);
    try std.testing.expectEqual(@as(u19, 32), cog.fifo_state.engine.?.remaining);
    try std.testing.expectEqual(@as(u20, 0x3000), cog.fifo_state.engine.?.block.?.address);
    try std.testing.expectEqual(@as(u19, 32), cog.fifo_state.engine.?.block.?.longs);
    while (!cog.events.contains(.FBW)) {
        if (cog.peek_fifo(3) != null) {
            _ = cog.read_fifo(4);
        }
        _ = hub.step();
    }
    try std.testing.expectEqual(@as(u20, 0x3000), cog.fifo_state.engine.?.address);
    try std.testing.expectEqual(@as(u19, 32), cog.fifo_state.engine.?.remaining);
    try std.testing.expectEqual(@as(u19, 0x40000), FifoBlock.init(0x7fffc000, 0).longs);
}

test "FIFO ring reads cross the physical end and full writes retain their augmented operand across retries" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    hub.init();
    const cog = &hub.cogs[0];
    cog.begin_fifo(.read, 0, 0xfffff);
    cog.fifo_state.engine.?.head = 18;
    cog.fifo_state.engine.?.count = 2;
    cog.fifo_state.engine.?.words[18] = 0x12000000;
    cog.fifo_state.engine.?.words[0] = 0x00563478;
    try std.testing.expectEqual(@as(u32, 0x56347812), cog.read_fifo(4).?);
    try std.testing.expectEqual(@as(u20, 3), cog.fifo_state.address.?);
    try std.testing.expectEqual(@as(usize, 1), cog.fifo_state.engine.?.count);

    cog.begin_fifo(.write, 0, 0x1000);
    for (0..19) |_| {
        try std.testing.expect(cog.write_fifo(0, 4));
    }
    try std.testing.expect(!cog.write_fifo(1, 1));
    cog.pipeline[3] = .{ .pc = 0, .instr = 0xfd64f017 }; // WFLONG #$78.
    cog.augd = 0x12345600;
    cog.augd_pending = true;
    cog.collecting_writes = true;
    try std.testing.expectEqual(ExecResult.wait, execute.execute_instruction(cog, cog.pipeline[3].?));
    try std.testing.expect(!cog.augd_pending);
    cog.fifo_state.engine.?.write_count -= 4;
    try std.testing.expectEqual(ExecResult.next, execute.execute_instruction(cog, cog.pipeline[3].?));
    try std.testing.expectEqualSlices(u8, &.{ 0x78, 0x56, 0x34, 0x12 }, cog.fifo_state.engine.?.write_bytes[72..76]);
    try std.testing.expectEqual(@as(u20, 0x1050), cog.fifo_state.address.?);
}

test "FIFO byte eligibility survives CT rollover and reset discards only uncommitted writes" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    hub.init();
    const cog = &hub.cogs[0];
    hub.counter = std.math.maxInt(u64) - 1;
    cog.begin_fifo(.write, 0, 0);
    try std.testing.expect(cog.write_fifo(0x11223344, 4));
    _ = hub.step();
    _ = hub.step();
    try std.testing.expectEqual(@as(u32, 0), hub.read_memory(0, 4));
    _ = hub.step();
    try std.testing.expectEqual(@as(u32, 0x11223344), hub.read_memory(0, 4));
    try std.testing.expect(cog.write_fifo(0x55667788, 4));
    cog.begin_fifo(.read, 0, 0x2000);
    cog.reset();
    for (0..32) |_| {
        _ = hub.step();
    }
    try std.testing.expectEqual(@as(u32, 0x11223344), hub.read_memory(0, 4));
    try std.testing.expectEqual(@as(u32, 0), hub.read_memory(4, 4));
    try std.testing.expect(cog.fifo_state.engine == null);
    try std.testing.expect(cog.fifo_state.request == null);
}

test "FIFO write eligibility still waits for the rotating hub RAM slice" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    for (0..8) |phase| {
        hub.init();
        hub.counter = phase;
        const cog = &hub.cogs[0];
        cog.begin_fifo(.write, 0, 0);
        try std.testing.expect(cog.write_fifo(0x1234_5678, 4));
        var first_grant = (8 - phase) % 8;
        if (first_grant < 2) {
            first_grant += 8;
        }
        for (0..first_grant) |_| {
            try std.testing.expectEqual(@as(u32, 0), hub.read_memory(0, 4));
            _ = hub.step();
        }
        try std.testing.expectEqual(@as(u32, 0), hub.read_memory(0, 4));
        _ = hub.step();
        try std.testing.expectEqual(@as(u32, 0x1234_5678), hub.read_memory(0, 4));
    }
}

test "pattern comparison reads architectural INA or INB and retains the SETPAT flag selections" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    const cases = [_]struct { mask: u32, value: u32, input: u32, equal: bool }{
        .{ .mask = 0xf, .value = 5, .input = 0xffff_fff5, .equal = true },
        .{ .mask = 0xf, .value = 5, .input = 6, .equal = false },
        .{ .mask = 0xf, .value = 0x15, .input = 0x15, .equal = false },
        .{ .mask = 0, .value = 0, .input = 0xffff_ffff, .equal = true },
        .{ .mask = 0, .value = 1, .input = 1, .equal = false },
        .{ .mask = 0xffff_ffff, .value = 0x8000_0000, .input = 0x8000_0000, .equal = true },
        .{ .mask = 0xffff_ffff, .value = 0x8000_0000, .input = 0, .equal = false },
    };
    for (cases) |case| {
        for ([_]bool{ false, true }) |port_b| {
            for ([_]bool{ false, true }) |equal| {
                hub.init();
                const cog = &hub.cogs[0];
                hub.io.samples.active = true;
                // Pads and unsampled routing deliberately disagree with the architectural register.
                hub.io.pads = @splat(.x);
                hub.io.routed_inputs = ~@as(u64, case.input);
                hub.io.samples.input_registers = if (port_b)
                    (@as(u64, case.input) << 32) | ~case.input
                else
                    (@as(u64, ~case.input) << 32) | case.input;
                cog.c = port_b;
                cog.z = equal;
                cog.events = .full;
                cog.configure_pattern(case.mask, case.value);
                try std.testing.expectEqual(port_b, cog.c);
                try std.testing.expectEqual(equal, cog.z);
                var remaining = EventSet.full;
                remaining.remove(.PAT);
                try std.testing.expect(cog.events.eql(remaining));
                // Changing the CPU flags later must not change the configured sensor.
                cog.c = !port_b;
                cog.z = !equal;
                try std.testing.expectEqual(case.equal == equal, cog.pattern_matches());
            }
        }
    }
}

test "pattern latches survive a false condition and rearm on new edges during waits until reset" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    hub.init();
    const cog = &hub.cogs[0];
    const other_cog = &hub.cogs[1];
    hub.io.samples.active = true;
    hub.io.samples.input_registers = 0;
    cog.exec_mode = .cog;
    other_cog.exec_mode = .cog;
    cog.z = true;
    cog.configure_pattern(1, 1);
    other_cog.c = true;
    other_cog.z = true;
    other_cog.configure_pattern(1, 1);
    hub.io.samples.input_registers = 1;
    // Freeze both instruction pipelines on a long WAITX while sensors keep sampling.
    const wait: PipelineState = .{ .pc = 0, .instr = 0xFD64_C81F }; // WAITX #100.
    cog.pipeline[3] = wait;
    other_cog.pipeline[3] = wait;
    cog.step();
    other_cog.step();
    try std.testing.expect(cog.events.contains(.PAT));
    try std.testing.expect(!other_cog.events.contains(.PAT));
    try std.testing.expect(cog.pipeline[3] != null);
    hub.io.samples.input_registers = 0;
    hub.counter += 1;
    cog.step();
    try std.testing.expect(cog.events.contains(.PAT));
    // Consume after the condition is gone; it stays clear until a later match.
    cog.events.remove(.PAT);
    hub.counter += 1;
    cog.step();
    try std.testing.expect(!cog.events.contains(.PAT));
    hub.io.samples.input_registers = 1;
    hub.counter += 1;
    cog.step();
    try std.testing.expect(cog.events.contains(.PAT));
    cog.events.remove(.PAT);
    hub.counter += 1;
    cog.step();
    try std.testing.expect(!cog.events.contains(.PAT));
    hub.io.samples.input_registers = 0;
    hub.counter += 1;
    cog.step();
    hub.io.samples.input_registers = 1;
    hub.counter += 1;
    cog.step();
    try std.testing.expect(cog.events.contains(.PAT));
    // SETPAT establishes a new baseline even when its new condition is already true.
    cog.configure_pattern(0, 0);
    hub.counter += 1;
    cog.step();
    try std.testing.expect(!cog.events.contains(.PAT));
    cog.events.insert(.ATN);
    cog.z = true;
    cog.configure_pattern(0, 1);
    try std.testing.expect(!cog.events.contains(.PAT));
    try std.testing.expect(cog.events.contains(.ATN));
    hub.counter += 1;
    cog.step();
    try std.testing.expect(!cog.events.contains(.PAT));
    cog.reset();
    try std.testing.expect(cog.pattern == null);
    try std.testing.expect(!cog.pattern_matches());
    try std.testing.expectEqual(@as(usize, 0), cog.events.count());
}

test "SETPAT dispatch resolves registers immediates augmentation and cancellation without changing operands or flags" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    for ([_]bool{ false, true }) |d_imm| {
        for ([_]bool{ false, true }) |s_imm| {
            for ([_]bool{ false, true }) |augmented| {
                for (0..4) |flags| {
                    hub.init();
                    const cog = &hub.cogs[0];
                    const mask: u32 = if (augmented)
                        0x8000_000f
                    else
                        0xf;
                    const value: u32 = if (augmented)
                        0x8000_0005
                    else
                        5;
                    cog.c = flags & 2 != 0;
                    cog.z = flags & 1 != 0;
                    cog.write_reg(@enumFromInt(20), mask);
                    cog.write_reg(@enumFromInt(21), value);
                    cog.augd = if (d_imm and augmented)
                        mask & ~@as(u32, 0x1ff)
                    else
                        0;
                    cog.augs = if (s_imm and augmented)
                        value & ~@as(u32, 0x1ff)
                    else
                        0;
                    cog.augs_pending = s_imm and augmented;
                    const d: u32 = if (d_imm)
                        mask & 0x1ff
                    else
                        20;
                    const s: u32 = if (s_imm)
                        value & 0x1ff
                    else
                        21;
                    const instruction = @as(u32, 0xFBF0_0000) | (@as(u32, @intFromBool(d_imm)) << 19) |
                        (@as(u32, @intFromBool(s_imm)) << 18) | (d << 9) | s;
                    try std.testing.expectEqual(ExecResult.next, execute.execute_instruction(cog, .{ .pc = 0, .instr = instruction }));
                    try std.testing.expectEqual(mask, cog.pattern.?.mask);
                    try std.testing.expectEqual(value, cog.pattern.?.value);
                    try std.testing.expectEqual(@as(Register, if (flags & 2 != 0)
                        .INB
                    else
                        .INA), cog.pattern.?.input);
                    try std.testing.expectEqual(flags & 1 != 0, cog.pattern.?.equal);
                    try std.testing.expectEqual(mask, cog.read_reg(@enumFromInt(20)));
                    try std.testing.expectEqual(value, cog.read_reg(@enumFromInt(21)));
                    try std.testing.expectEqual(flags & 2 != 0, cog.c);
                    try std.testing.expectEqual(flags & 1 != 0, cog.z);
                    if (d_imm) {
                        try std.testing.expectEqual(@as(u32, 0), cog.augd);
                    }
                    if (s_imm) {
                        try std.testing.expect(!cog.augs_pending);
                    }
                    const previous = cog.pattern.?;
                    cog.events.insert(.PAT);
                    cog.z = false;
                    const canceled = (instruction & 0x0fff_ffff) | 0xA000_0000; // IF_Z.
                    try std.testing.expectEqual(ExecResult.skip, execute.execute_instruction(cog, .{ .pc = 0, .instr = canceled }));
                    try std.testing.expectEqualDeep(previous, cog.pattern.?);
                    try std.testing.expect(cog.events.contains(.PAT));
                }
            }
        }
    }
}

test "interrupt requests select every event source independently of event latches" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    for (std.enums.values(EventId)[1..]) |event| {
        for (std.enums.values(InterruptLevel)) |level| {
            hub.init();
            const cog = &hub.cogs[3];
            const slot = cog.interrupts.levels.getPtr(level);
            cog.events.insert(event);
            cog.configure_interrupt(level, 0xffff_fff0 | @as(u32, @intFromEnum(event)));
            try std.testing.expectEqual(@as(?EventId, event), slot.source);
            try std.testing.expectEqual(InterruptState.idle, slot.state);
            cog.interrupts.stalled = true;
            hub.counter = std.math.maxInt(u64);
            cog.raise_event(event);
            try std.testing.expectEqual(InterruptState.pending, slot.state);
            try std.testing.expectEqual(hub.counter, slot.pending_since);
            cog.events.remove(event);
            cog.configure_interrupt(level, 0);
            try std.testing.expect(slot.source == null);
            try std.testing.expectEqual(InterruptState.pending, slot.state);
            cog.trigger_interrupt(level);
            try std.testing.expectEqual(hub.counter, slot.pending_since);
            slot.state = .active;
            cog.raise_event(event);
            cog.trigger_interrupt(level);
            try std.testing.expectEqual(InterruptState.active, slot.state);
            cog.cancel_interrupt(level);
            try std.testing.expectEqual(InterruptState.idle, slot.state);
            cog.trigger_interrupt(level);
            try std.testing.expectEqual(InterruptState.pending, slot.state);
            cog.reset();
            try std.testing.expectEqual(InterruptState.idle, cog.interrupts.levels.get(level).state);
            try std.testing.expect(cog.interrupts.levels.get(level).source == null);
            try std.testing.expect(!cog.interrupts.stalled);
        }
    }
}

test "interrupt completion requires a register IRET source and both flag writes" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    for (std.enums.values(InterruptLevel), [_]Register{ .IRET1, .IRET2, .IRET3 }) |level, source| {
        hub.init();
        const cog = &hub.cogs[0];
        for (&cog.interrupts.levels.values) |*slot| {
            slot.state = .active;
        }
        cog.complete_interrupt(source, true, true);
        cog.complete_interrupt(source, false, false);
        cog.complete_interrupt(.INB, false, true); // Debug return is outside the ordinary controller.
        try std.testing.expectEqual(InterruptState.active, cog.interrupts.levels.get(level).state);
        cog.complete_interrupt(source, false, true);
        for (std.enums.values(InterruptLevel)) |other_level| {
            try std.testing.expectEqual(if (other_level == level)
                InterruptState.idle
            else
                .active, cog.interrupts.levels.get(other_level).state);
        }
    }
}

test "interrupt injection holds the PC and FIFO and respects priority reservation and rollover" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    hub.init();
    const cog = &hub.cogs[0];
    cog.exec_mode = .hub;
    cog.pc = 0x4003;
    cog.fifo_state.engine = .{ .byte_offset = 3, .address = 0x4040, .ready_at = null, .count = 12 };
    hub.counter = std.math.maxInt(u64);
    for (std.enums.values(InterruptLevel)) |level| {
        cog.trigger_interrupt(level);
    }
    try std.testing.expect(cog.inject_interrupt() == null);
    hub.counter +%= 1;
    cog.issue_phase = false;
    try std.testing.expect(cog.inject_interrupt() == null);
    cog.issue_phase = true;
    const injected = cog.inject_interrupt().?;
    try std.testing.expectEqual(@as(?InjectedInterrupt, .int1), injected.interrupt);
    try std.testing.expectEqual(@as(u32, 0xFB3B_EBF4), injected.instr);
    try std.testing.expectEqual(@as(u20, 0x4003), cog.pc);
    try std.testing.expectEqual(cog.pc, injected.pc);
    try std.testing.expectEqual(@as(usize, 12), cog.fifo_state.engine.?.count);
    try std.testing.expectEqual(@as(u2, 3), cog.fifo_state.engine.?.byte_offset);
    cog.pipeline[1] = injected;
    try std.testing.expect(cog.inject_interrupt() == null);
    cog.jump(0x200); // A taken branch may flush a reservation, preserving the request.
    try std.testing.expectEqual(InterruptState.pending, cog.interrupts.levels.get(.int1).state);
    cog.issue_phase = true;
    try std.testing.expectEqual(@as(?InjectedInterrupt, .int1), cog.inject_interrupt().?.interrupt);
    cog.interrupts.levels.getPtr(.int1).state = .active;
    try std.testing.expect(cog.inject_interrupt() == null);
    cog.complete_interrupt(.IRET1, false, true);
    try std.testing.expectEqual(@as(?InjectedInterrupt, .int2), cog.inject_interrupt().?.interrupt);
    cog.interrupts.levels.getPtr(.int2).state = .active;
    cog.trigger_interrupt(.int1);
    hub.counter +%= 1;
    try std.testing.expectEqual(@as(?InjectedInterrupt, .int1), cog.inject_interrupt().?.interrupt);
    cog.pipeline[3] = injected;
    cog.dispatch_pc = injected.pc;
    cog.c = true;
    cog.z = true;
    try std.testing.expectEqual(@as(u32, 0xc000_4003), cog.return_address());
}

test "zero augmentations shield until a non-canceled consumer is ahead of the interrupt" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    hub.init();
    const cog = &hub.cogs[0];
    cog.trigger_interrupt(.int1);
    hub.counter += 1;
    cog.augs_pending = true;
    try std.testing.expect(cog.inject_interrupt() == null);
    cog.pipeline[2] = .{ .pc = 1, .instr = 0xC604_2807 }; // IF_C MOV r20,#7, canceled.
    try std.testing.expect(cog.inject_interrupt() == null);
    cog.pipeline[2].?.instr = 0xF604_2807;
    try std.testing.expect(cog.inject_interrupt() != null);
    cog.pipeline[2].?.instr = 0xFF00_0000; // AUGS #0 is itself a shield.
    try std.testing.expect(cog.inject_interrupt() == null);
    cog.augs_pending = false;
    cog.pipeline[2].?.instr = 0xCF00_0000; // A canceled prefix has no shield.
    try std.testing.expect(cog.inject_interrupt() != null);
    cog.augd_pending = true;
    cog.pipeline[2].?.instr = 0xF604_2807; // Immediate S does not consume AUGD.
    try std.testing.expect(cog.inject_interrupt() == null);
    cog.pipeline[2].?.instr = 0xFD64_001F; // WAITX #0 consumes AUGD before the injected CALLD.
    try std.testing.expect(cog.inject_interrupt() != null);
    cog.augd_pending = false;
    cog.setq_pending = true;
    cog.pipeline[2] = null;
    try std.testing.expect(cog.inject_interrupt() == null);
    cog.pipeline[2] = .{ .pc = 1, .instr = 0xF604_2807 };
    try std.testing.expect(cog.inject_interrupt() != null);
    cog.pipeline[2].?.instr = 0xFD60_4224; // STALLI already in flight shields entry.
    try std.testing.expect(cog.inject_interrupt() == null);
}

test "WAITINT samples pending requests rather than the pollable interrupt latch" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    hub.init();
    const cog = &hub.cogs[0];
    cog.events.insert(.INT);
    try std.testing.expect(!cog.event_for_wait(.INT));
    for (std.enums.values(InterruptLevel)) |level| {
        cog.interrupts.stalled = true;
        cog.trigger_interrupt(level);
        try std.testing.expect(cog.event_for_wait(.INT));
        cog.events.remove(.INT);
        try std.testing.expect(cog.event_for_wait(.INT));
        cog.cancel_interrupt(level);
        try std.testing.expect(!cog.event_for_wait(.INT));
    }
}
