//! Cog/LUT RAM and the five-stage execution pipeline.
//! Hub services clock shared requests; the cog captures operands, stalls and redirects fetch.

const std = @import("std");
const logger = std.log.scoped(.cog);

const Cog = @This();
const Hub = @import("Hub.zig");

const decode = @import("decode.zig");
const encoding = @import("encoding.zig");
const execute = @import("execute.zig");
const enums = @import("enums.zig");
const EventId = @import("p2").types.EventId;

pub const Register = enums.Register;

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
setq_pending: bool = false,
block_pointer_delta: bool = false,
z: bool = false,
c: bool = false,
q2: bool = false,

augs: u32 = 0,
augs_pending: bool = false,
augd: u32 = 0,

lut_sharing: bool = false,
interrupts: bool = false,

exec_mode: ExecMode = .stopped,

/// Slots being processed on this edge: fetch, read/decode, latch, execute, store.
pipeline: [5]?PipelineState = @splat(null),
issue_phase: bool = true,
collecting_writes: bool = false,
writeback: Writeback = .{},
retired: ?PipelineState = null,
ram_collision: ?struct { clock: u64, address: u9 } = null,
lut_collision: ?struct { clock: u64, address: u9 } = null,
dispatch_pc: u20 = 0,
branched: bool = false,
stack: [8]u32 = @splat(0),
wait_until: ?u64 = null,
fifo_address: ?u20 = null,
fifo: ?HubFifo = null,
fifo_granted_at: ?u64 = null,
ct_targets: [3]?u32 = @splat(null),
events: u16 = 0,
selectable_events: [4]?u6 = @splat(null),
pixel_pivot: u8 = 0,
pixel_mode: u6 = 0,
memory_transfer: ?MemoryTransfer = null,
hub_writeback: ?struct { address: u32, value: u32, size: u3, masked: bool } = null,
startup: ?Startup = null,

pub const Startup = struct {
    address: u32,
    grant_at: u64,
    remaining: u10,
    reg: u9 = 0,
    release_at: ?u64 = null,
    responses: [8]?struct { reg: u9, value: u32 } = @splat(null),
};

pub const MemoryTransfer = struct {
    address: u32,
    remaining: u64,
    reg: u9,
    lut: bool,
    immediate: ?u32 = null,
    no_result: bool = false,
    block: bool = false,
    timed: bool = false,
    size: u3 = 0,
    grant_at: u64 = 0,
    grant_address: u32 = 0,
    grants_left: u64 = 0,
    responses: [8]?u32 = @splat(null),
    value: ?u32 = null,
    write_ready_at: u64 = 0,
    partial: ?u32 = null,
    write: bool = false,
    masked: bool = false,
    source_value: ?u32 = null,
};

/// The RAM return path holds five requests; the FIFO holds nineteen longs.
/// Stop issuing at fifteen occupied words so returning reads cannot overflow.
pub const HubFifo = struct {
    words: [19]u32 = undefined,
    head: usize = 0,
    count: usize = 0,
    byte_offset: u2,
    address: u20,
    ready_at: ?u64,
    responses: [8]?u32 = @splat(null),

    /// Consume the next instruction slice; read/decode joins the adjacent slice for an unaligned word.
    fn read(fifo: *HubFifo) ?u32 {
        if (fifo.count == 0) return null;
        const low = fifo.words[fifo.head];
        const value = low >> (@as(u5, fifo.byte_offset) * 8);
        fifo.head = (fifo.head + 1) % fifo.words.len;
        fifo.count -= 1;
        return value;
    }

    /// Consume one software-FIFO byte, releasing its buffered long after the fourth byte.
    pub fn read_byte(fifo: *HubFifo) ?u8 {
        if (fifo.count == 0) return null;
        const value: u8 = @truncate(fifo.words[fifo.head] >> (@as(u5, fifo.byte_offset) * 8));
        fifo.byte_offset +%= 1;
        if (fifo.byte_offset == 0) {
            fifo.head = (fifo.head + 1) % fifo.words.len;
            fifo.count -= 1;
        }
        return value;
    }
};

const Writeback = struct {
    writes: [3]struct { reg: Register, value: u32, raw: bool = false } = undefined,
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
    for (&hub.locks) |*lock| {
        if (lock.taken and lock.owner == id) hub.release_lock(lock);
    }
    cog.* = .init(hub, id);
    // Cog and LUT RAM survive COGSTOP and no-load COGINIT.
    cog.registers = registers;
    cog.ram_tail = ram_tail;
    cog.lut = lut;
    for ([_]Register{ .PTRA, .PTRB, .DIRA, .DIRB, .OUTA, .OUTB }) |reg| cog.registers.set(reg, 0);
    if (!hub.clocking) hub.io.updateDirections(hub);
}

/// Advance instruction stages for one clock after the hub has serviced shared resources.
/// An execute-stage wait freezes younger stages; completed results are committed on the next edge.
pub fn step(cog: *Cog) void {
    if (cog.exec_mode == .stopped or cog.startup != null) {
        return;
    }

    for (&cog.ct_targets, [3]EventId{ .CT1, .CT2, .CT3 }) |*target, event| {
        if (target.*) |value| if (@as(u32, @truncate(cog.hub.counter)) -% value < 0x8000_0000) {
            cog.events |= event.mask();
        };
    }

    // Unaligned fetch begins with the low word. The adjacent slice returns
    // on the next clock, completing the instruction at its read/decode stage.
    // Complete it before an older ALTx can alter this younger instruction.
    if (cog.pipeline[1]) |*state| if (state.fifo_tail) {
        const fifo = &cog.fifo.?;
        if (fifo.count == 0) return;
        state.instr |= fifo.words[fifo.head] << @intCast(32 - @as(u6, fifo.byte_offset) * 8);
        state.fifo_tail = false;
    };

    if (cog.pipeline[3]) |*state| {
        const enabled = state.instr == 0 or cog.is_condition_met(@enumFromInt(@as(u4, @truncate(state.instr >> 28))));
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
        cog.trace(.execute, instr);
        cog.dispatch_pc = instr.pc;
        cog.branched = false;
        const old_c = cog.c;
        const old_z = cog.z;
        cog.collecting_writes = true;
        const result = if (instr.debug_rom) ExecResult.not_implemented else if (instr.command_result) |result| blk: {
            cog.writeback = instr.command_writeback;
            break :blk result;
        } else execute.execute_instruction(cog, instr);
        cog.collecting_writes = false;
        // COGSTOP/self-restart can clear the entire pipeline during execution.
        if (cog.exec_mode == .stopped or cog.pipeline[3] == null) return;
        if (cog.c != old_c) cog.writeback.c = cog.c;
        if (cog.z != old_z) cog.writeback.z = cog.z;
        cog.c = old_c;
        cog.z = old_z;
        if (std.meta.activeTag(result) == .next and instr.instr != 0 and instr.instr >> 28 == 0 and !cog.branched) {
            cog.jump(@truncate(cog.pop()));
        }
        switch (result) {
            .wait => {
                cog.trace(.stall, instr);
                return;
            },
            .next, .skip => {},
            .trap, .not_implemented, .illegal => {
                cog.hub.fault = .{ .cog = cog.id, .pc = instr.pc, .instruction = instr.instr, .result = result, .debug_rom = instr.debug_rom };
                cog.exec_mode = .stopped;
                cog.pipeline = @splat(null);
                cog.writeback = .{};
                return;
            },
        }
    }

    if (cog.pipeline[2]) |*state| {
        cog.capture_operands(state);
        cog.trace(.latch, state.*);
    }
    if (cog.pipeline[1]) |state| cog.trace(.read, state);
    var fetched = false;
    if (cog.issue_phase) {
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
    const startup = if (cog.startup) |*value| value else return;
    if (startup.release_at) |clock| {
        if (cog.hub.counter == clock) cog.startup = null;
        return;
    }
    const response = &startup.responses[cog.hub.counter % startup.responses.len];
    if (response.*) |beat| {
        cog.write_ram(beat.reg, beat.value);
        response.* = null;
        // The completed load passes through the startup/fetch handoff.
        if (beat.reg == 503) startup.release_at = cog.hub.counter +% 3;
    }
    if (startup.remaining == 0 or cog.hub.counter != startup.grant_at) return;
    startup.responses[(cog.hub.counter +% 5) % startup.responses.len] = .{
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
    if (cog.exec_mode == .stopped or cog.startup != null) return;
    const state = if (cog.pipeline[3]) |*value| value else return;
    if (state.command_result != null or !cog.is_condition_met(@enumFromInt(@as(u4, @truncate(state.instr >> 28))))) return;
    const opcode = decode.decode(state.instr);
    const extra: u64 = switch (opcode) {
        .cogid, .locknew => 2,
        .coginit, .locktry, .lockrel => if (state.instr & (1 << 20) != 0) 2 else 0,
        .cogstop, .lockret, .hubset => 0,
        else => return,
    };
    if (state.command_at == null) {
        state.command_at = cog.hub.counter +% ((@as(u64, cog.id) -% cog.hub.counter) & 7);
        state.ready_at = state.command_at.? +% extra;
    }
    if (cog.hub.counter != state.command_at.?) return;
    const old_c = cog.c;
    const old_z = cog.z;
    cog.collecting_writes = true;
    const result = execute.execute_instruction(cog, state.*);
    cog.collecting_writes = false;
    // A self-stop/restart discarded this command along with its old pipeline.
    if (cog.pipeline[3] == null) return;
    if (cog.c != old_c) cog.writeback.c = cog.c;
    if (cog.z != old_z) cog.writeback.z = cog.z;
    cog.c = old_c;
    cog.z = old_z;
    state.command_writeback = cog.writeback;
    cog.writeback = .{};
    state.command_result = result;
}

/// Deliver five-clock RAM responses, then issue an eligible prefetch unless the FIFO is sufficiently full.
pub fn clock_fifo(cog: *Cog) void {
    const fifo = if (cog.fifo) |*value| value else return;
    if (fifo.ready_at) |deadline| if (Hub.clock_reached(cog.hub.counter, deadline)) {
        fifo.ready_at = null;
    };
    const response = &fifo.responses[cog.hub.counter % fifo.responses.len];
    if (response.*) |value| {
        std.debug.assert(fifo.count < fifo.words.len);
        fifo.words[(fifo.head + fifo.count) % fifo.words.len] = value;
        fifo.count += 1;
        response.* = null;
    }
    if (!cog.fifo_grants(cog.hub.counter)) return;
    cog.fifo_granted_at = cog.hub.counter;
    fifo.responses[(cog.hub.counter +% 5) % fifo.responses.len] = cog.hub.read_memory(fifo.address, 4);
    fifo.address +%= 4;
}

/// Test FIFO eligibility on the current or next clock, including a response returning on the lookahead edge.
pub fn fifo_grants(cog: *const Cog, clock: u64) bool {
    const fifo = cog.fifo orelse return false;
    const returning: usize = @intFromBool(clock != cog.hub.counter and fifo.responses[clock % fifo.responses.len] != null);
    return (fifo.ready_at == null or Hub.clock_reached(clock, fifo.ready_at.?)) and fifo.count + returning < 15 and
        ((fifo.address >> 2) & 7) == ((clock +% cog.id) & 7);
}

/// Commit the preceding execution result or block beat before new operands are captured.
/// Record possible cog fetch/write collisions and retire the completed store-stage instruction.
pub fn commit_results(cog: *Cog) void {
    cog.retired = null;
    // This edge completes the previous ALU result, including streaming beats
    // while an instruction holds the execute stage.
    for (cog.writeback.writes[0..cog.writeback.count]) |write| {
        if (cog.issue_phase and cog.exec_mode == .cog and cog.pc < 504 and cog.pc == @intFromEnum(write.reg)) {
            cog.ram_collision = .{ .clock = cog.hub.counter, .address = @intFromEnum(write.reg) };
            cog.trace_collision(@intFromEnum(write.reg), .cog_ram);
        }
        if (write.raw) cog.write_ram(@intFromEnum(write.reg), write.value) else cog.write_reg(write.reg, write.value);
    }
    if (cog.writeback.lut) |write| cog.write_lut(write.address, write.value);
    if (cog.writeback.c) |value| cog.c = value;
    if (cog.writeback.z) |value| cog.z = value;
    cog.writeback = .{};
    cog.retired = cog.pipeline[4];
    if (cog.retired) |state| cog.trace(.store, state);
    cog.pipeline[4] = null;
}

/// Hub read grants continue while the cog pipeline is stalled. Each aligned
/// grant produces a response five clocks later; SETQ streams one per clock.
pub fn clock_memory(cog: *Cog) void {
    const transfer = if (cog.memory_transfer) |*value| value else return;
    if (transfer.timed and transfer.write) {
        if (cog.hub.counter == transfer.grant_at and cog.fifo_granted_at == cog.hub.counter) {
            transfer.grant_at +%= 8;
            transfer.write_ready_at +%= 8;
            return;
        }
        if (cog.hub.counter == transfer.grant_at and (transfer.address & 3) + transfer.size > 4) {
            const low_size: u3 = @intCast(4 - (transfer.address & 3));
            const value = cog.transfer_source(transfer);
            cog.hub.write_memory(transfer.address, value, low_size, transfer.masked);
        }
        return;
    }
    if (!transfer.timed or transfer.grants_left == 0) {
        if (transfer.timed) {
            const slot = &transfer.responses[cog.hub.counter % transfer.responses.len];
            transfer.value = slot.*;
            slot.* = null;
        }
        return;
    }
    const slot = &transfer.responses[cog.hub.counter % transfer.responses.len];
    transfer.value = slot.*;
    slot.* = null;
    if (cog.hub.counter != transfer.grant_at) return;
    if (cog.fifo_granted_at == cog.hub.counter) {
        transfer.grant_at +%= 8;
        return;
    }
    const low_size: u3 = @intCast(@min(transfer.size, 4 - (transfer.grant_address & 3)));
    const crossing = low_size < transfer.size;
    if (crossing and transfer.partial == null) {
        transfer.partial = cog.hub.read_memory(transfer.grant_address, low_size);
        transfer.grant_at +%= 1;
        return;
    }
    const high_word = if (crossing) cog.hub.read_memory(transfer.grant_address +% low_size, 4) else 0;
    const high_size = transfer.size - low_size;
    const high_mask: u32 = if (crossing) (@as(u32, 1) << @intCast(@as(u6, high_size) * 8)) - 1 else 0;
    const value = if (crossing) transfer.partial.? | ((high_word & high_mask) << @intCast(@as(u6, low_size) * 8)) else cog.hub.read_memory(transfer.grant_address, transfer.size);
    if (crossing) transfer.partial = high_word >> @intCast(@as(u6, high_size) * 8);
    const response = &transfer.responses[(cog.hub.counter +% 5) % transfer.responses.len];
    std.debug.assert(response.* == null);
    response.* = value;
    transfer.grants_left -= 1;
    transfer.grant_address +%= transfer.size;
    transfer.grant_at +%= 1;
}

/// Capture a block-write source once per beat, bypassing architectural LUT-read event generation.
pub fn transfer_source(cog: *Cog, transfer: *MemoryTransfer) u32 {
    if (transfer.source_value) |value| return value;
    const value = transfer.immediate orelse if (transfer.lut) cog.read_lut(transfer.reg) else if (transfer.block and transfer.reg >= 504) cog.ram_tail[transfer.reg - 504] else cog.registers.values[transfer.reg];
    transfer.source_value = value;
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

/// Redirect fetch and discard younger stages; entering hub execution restarts its instruction FIFO.
/// Leaving hub execution keeps the existing FIFO filling, so it may still contend with data transfers.
pub fn jump(cog: *Cog, target: u20) void {
    if (cog.pipeline[3]) |state| cog.trace(.flush, state);
    const was_hub = cog.exec_mode == .hub;
    cog.pc = target;
    cog.exec_mode = if (target < 0x400) .cog else .hub;
    if (cog.exec_mode == .hub) {
        cog.fifo_address = null;
        cog.fifo = .{
            .byte_offset = @truncate(target),
            .address = target & 0xffffc,
            .ready_at = cog.hub.counter +% 5,
        };
    } else if (was_hub) {
        // The read FIFO keeps filling after leaving hubexec. Cog fetch no
        // longer consumes it, so it soon fills and stops granting RAM reads.
        cog.fifo_address = null;
    }
    @memset(cog.pipeline[0..3], null);
    // The target's fetch overlaps the branch's store on the next clock.
    cog.issue_phase = false;
    cog.branched = true;
}

/// Write a bounded instruction-stage event when pipeline tracing is enabled.
fn trace(cog: *Cog, stage: enum { fetch, read, latch, execute, store, stall, flush }, state: PipelineState) void {
    const writer = cog.hub.trace_writer orelse return;
    if (cog.hub.trace_lines_left == 0) return;
    cog.hub.trace_lines_left -= 1;
    writer.print("CT={d} cog={d} stage={t} pc=0x{x} instruction=0x{x}\n", .{ cog.hub.counter, cog.id, stage, state.pc, state.instr }) catch {};
    if (cog.hub.trace_lines_left == 0) writer.writeAll("pipeline trace truncated after 10000 events\n") catch {};
}

/// Write a bounded collision diagnostic without turning ambiguous RAM data into an execution fault.
pub fn trace_collision(cog: *Cog, address: u9, kind: enum { cog_ram, lut_write }) void {
    const writer = cog.hub.trace_writer orelse return;
    if (cog.hub.trace_lines_left == 0) return;
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
    cog.jump(target);
}

/// Pack C/Z and the next execution address; local PCs advance by longs and hub PCs by bytes.
pub fn return_address(cog: *Cog) u32 {
    const pc = cog.dispatch_pc +% @as(u20, if (cog.exec_mode == .hub) 4 else 1);
    return (@as(u32, @intFromBool(cog.c)) << 31) | (@as(u32, @intFromBool(cog.z)) << 30) | pc;
}

/// Apply ALTR/ALTI result redirection or cancellation before scheduling a register write.
pub fn write_result(cog: *Cog, reg: Register, value: u32) void {
    if (cog.pipeline[3]) |state| {
        if (state.no_result) return;
        cog.write_reg(state.alt_r orelse reg, value);
    } else cog.write_reg(reg, value);
}

/// Raise each selectable event whose configured LUT/lock sensor matches this notification.
pub fn signal_selectable(cog: *Cog, config: u6) void {
    for (cog.selectable_events, [4]EventId{ .SE1, .SE2, .SE3, .SE4 }) |selection, event| {
        if (selection != null and selection.? == config) cog.hub.signal_event(cog.id, event.mask());
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

/// Queue an instruction write during execution, or update the live register and its peripheral effects.
pub fn write_reg(cog: *Cog, reg: Register, value: u32) void {
    if (cog.collecting_writes) {
        std.debug.assert(cog.writeback.count < cog.writeback.writes.len);
        cog.writeback.writes[cog.writeback.count] = .{ .reg = reg, .value = value };
        cog.writeback.count += 1;
        return;
    }
    cog.registers.set(reg, value);
    switch (reg) {
        .DIRA, .DIRB => if (!cog.hub.clocking) cog.hub.io.updateDirections(cog.hub),
        .INA, .INB => {},
        .OUTA, .OUTB => cog.hub.io.update_out(),
        else => {},
    }
}

/// Write underlying cog RAM, including the tail hidden by ordinary special-register access.
pub fn write_ram(cog: *Cog, address: u9, value: u32) void {
    if (cog.collecting_writes) {
        std.debug.assert(cog.writeback.count < cog.writeback.writes.len);
        cog.writeback.writes[cog.writeback.count] = .{ .reg = @enumFromInt(address), .value = value, .raw = true };
        cog.writeback.count += 1;
    } else if (address >= 504) cog.ram_tail[address - 504] = value else cog.registers.values[address] = value;
}

/// Read live architectural state; INA/INB are supplied by the pin model rather than stored RAM.
pub fn read_reg(cog: *Cog, reg: Register) u32 {
    return switch (reg) {
        .INA => @truncate(cog.hub.io.get_in() >> 0),
        .INB => @truncate(cog.hub.io.get_in() >> 32),
        else => cog.registers.get(reg),
    };
}

/// Only execution reads use latched data; debugger/test reads see live state.
pub fn read_operand(cog: *Cog, reg: Register) u32 {
    if (cog.pipeline[3]) |state| if (state.operands) |values| {
        const d: Register = state.alt_d orelse @enumFromInt(@as(u9, @truncate(state.instr >> 9)));
        const s: Register = state.alt_s orelse @enumFromInt(@as(u9, @truncate(state.instr)));
        if (reg == d) return values.d;
        if (reg == s) return values.s;
        return switch (reg) {
            .PTRA => values.ptra,
            .PTRB => values.ptrb,
            .PA => values.pa,
            .PB => values.pb,
            else => cog.read_reg(reg),
        };
    };
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
}

/// Find the next instruction behind execution for ALTx modification and next-source forwarding.
pub fn following_instruction(cog: *Cog) ?*PipelineState {
    var i: usize = 3;
    while (i > 0) {
        i -= 1;
        if (cog.pipeline[i]) |*state| return state;
    }
    return null;
}

/// Queue a local LUT write and its sensor event, or apply an untimed write with receiver-controlled sharing.
pub fn write_lut(cog: *Cog, addr: u9, value: u32) void {
    if (cog.collecting_writes) {
        std.debug.assert(cog.writeback.lut == null);
        cog.writeback.lut = .{ .address = addr, .value = value };
        if (addr >= 0x1fc) cog.signal_selectable(4 | (@as(u6, @truncate(addr)) & 3));
        return;
    }
    cog.lut[addr] = value;
    if (!cog.hub.clocking and cog.other().lut_sharing)
        cog.other().lut[addr] = value;
    if (addr >= 0x1fc) {
        // Own writes signal from the execution request; a companion sees the
        // physical RAM commit a clock later. Direct semantic calls are untimed.
        if (!cog.hub.clocking) cog.signal_selectable(4 | (@as(u6, @truncate(addr)) & 3));
        cog.other().signal_selectable(12 | (@as(u6, @truncate(addr)) & 3));
    }
}

/// Read local LUT RAM and notify local/companion selectable sensors for the four monitored addresses.
pub fn read_lut(cog: *Cog, addr: u9) u32 {
    if (addr >= 0x1fc) {
        cog.signal_selectable(@as(u6, @truncate(addr)) & 3);
        cog.other().signal_selectable(8 | (@as(u6, @truncate(addr)) & 3));
    }
    return cog.lut[addr];
}

/// Checks if `cond` would currently apply to this cog or not.
pub fn is_condition_met(cog: *Cog, cond: enums.Condition) bool {
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
    fifo_tail: bool = false,
    debug_rom: bool = false,
    fifo_started: bool = false,

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
    return augd;
}

/// Fetch local cog/LUT code or consume a hub FIFO slice without changing mode on sequential PC wrap.
/// Mark execute-only debug-ROM fetches so only instructions that reach execution report missing support.
fn fetch_instruction(cog: *Cog) ?PipelineState {
    switch (cog.exec_mode) {
        .stopped => unreachable,
        .cog => {
            const instr: PipelineState = .{
                .pc = cog.pc,
                .debug_rom = cog.pc >= 504 and cog.pc < 512,
                .instr = if (cog.pc < 0x200)
                    if (cog.pc >= 504) 0 else cog.registers.values[cog.pc]
                else
                    cog.lut[cog.pc - 0x200],
            };
            cog.pc += 1;
            if (cog.pc == 0x400) {
                cog.pc = 0;
            }
            std.debug.assert(cog.pc < 0x400);
            return instr;
        },
        .hub => {
            if (cog.fifo == null) {
                cog.fifo = .{ .byte_offset = @truncate(cog.pc), .address = cog.pc & 0xffffc, .ready_at = cog.hub.counter +% 5 };
                return null;
            }
            const word = cog.fifo.?.read() orelse return null;
            const instr: PipelineState = .{ .pc = cog.pc, .instr = word, .fifo_tail = cog.fifo.?.byte_offset != 0 };
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
            .not_implemented => if (fault.debug_rom) "debug ROM execution is not implemented" else "instruction is not implemented",
            .illegal => |message| message,
            else => unreachable,
        };
    }
};

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
        hub.step();
        try std.testing.expectEqual(@as(u20, 0), cog.pipeline[stage].?.pc);
        try std.testing.expectEqual(@as(u32, 0), cog.registers.values[20]);
    }
    hub.step();
    try std.testing.expectEqual(@as(u32, 7), cog.registers.values[20]);
    try std.testing.expectEqual(@as(u20, 0), cog.retired.?.pc);
    try std.testing.expectEqual(@as(u20, 1), cog.pipeline[3].?.pc);
    try std.testing.expectEqual(@as(u32, 7), cog.pipeline[3].?.operands.?.d);
    hub.step();
    try std.testing.expectEqual(@as(u32, 7), cog.registers.values[20]);
    hub.step();
    try std.testing.expectEqual(@as(u32, 8), cog.registers.values[20]);
}

test "debug ROM faults at execution and discarded fetches do not fault" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);
    hub.init();
    const cog = &hub.cogs[0];
    cog.exec_mode = .cog;
    cog.pc = 504;
    cog.ram_tail[0] = 0xF604_2807; // Data RAM is not the execute-only ROM.
    for (0..3) |_| {
        hub.step();
        try std.testing.expect(hub.fault == null);
    }
    hub.step();
    try std.testing.expect(hub.fault.?.debug_rom);
    try std.testing.expectEqual(@as(u20, 504), hub.fault.?.pc);
    try std.testing.expectEqual(@as(u32, 0), cog.registers.values[20]);

    hub.init();
    cog.exec_mode = .cog;
    cog.pc = 502;
    cog.registers.values[502] = 0xFD80_0000; // JMP #0, younger ROM fetch discarded.
    for (0..12) |_| hub.step();
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
    hub.step();
    const fault = hub.fault.?;
    try std.testing.expectEqualStrings("hub execution uses the FIFO for instruction fetching", fault.result.illegal);
    try std.testing.expectEqual(@as(u20, 0x1000), fault.pc);
    try std.testing.expectEqual(ExecMode.stopped, cog.exec_mode);
    for (cog.pipeline) |stage| try std.testing.expect(stage == null);
    try std.testing.expectEqual(@as(usize, 0), cog.writeback.count);
    for (0..8) |_| hub.step();
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
    cog.fifo = .{ .address = 0, .ready_at = 0, .byte_offset = 0 };
    cog.reset();
    try std.testing.expect(cog.startup == null);
    try std.testing.expect(cog.fifo == null);
    try std.testing.expect(cog.memory_transfer == null);
    try std.testing.expect(cog.pipeline[3] == null);
    try std.testing.expectEqual(@as(u2, 0), cog.writeback.count);
    for (0..32) |_| hub.step();
    try std.testing.expectEqual(@as(u32, 77), cog.registers.values[100]);
    try std.testing.expectEqual(@as(u32, 88), cog.lut[100]);
}
