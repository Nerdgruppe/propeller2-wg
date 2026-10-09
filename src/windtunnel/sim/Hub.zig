//! Shared hub memory, clocks, locks and cross-cog edge ordering.
//! A step commits simultaneous writes before any cog samples the new edge.

const std = @import("std");

const Cog = @import("Cog.zig");
const IO = @import("IO.zig");
const Hub = @This();

memory: [512 * 1024]u8 = @splat(0),
cogs: [8]Cog,
counter: u64 = 0,
io: IO,

output_writer: ?*std.Io.Writer = null,
trace_writer: ?*std.Io.Writer = null,
trace_lines_left: u32 = 10000,
fault: ?Cog.Fault = null,
locks: [16]Lock = undefined,
/// True only while step() processes a clock edge; direct semantic calls apply shared effects immediately.
clocking: bool = false,
pending_starts: [8]?struct { at: u64, address: u32, ptra: u32, load: bool, command_clock: u64 } = @splat(null),
pending_events: [4][8]u16 = @splat(@splat(0)),

pub const Lock = struct { allocated: bool = false, taken: bool = false, owner: u3 = 0, id: u4 };

/// Scheduled delays are below half the counter period. Compare modulo 2^64
/// so a deadline after rollover remains in the future until its clock arrives.
pub fn clock_reached(clock: u64, deadline: u64) bool {
    return clock -% deadline < (@as(u64, 1) << 63);
}

/// Initialize all shared resources and stopped cogs, binding each cog back to this hub.
pub fn init(hub: *Hub) void {
    hub.* = .{
        .cogs = .{
            .init(hub, 0),
            .init(hub, 1),
            .init(hub, 2),
            .init(hub, 3),
            .init(hub, 4),
            .init(hub, 5),
            .init(hub, 6),
            .init(hub, 7),
        },
        .io = .{},
    };
    for (&hub.locks, 0..) |*lock, i| lock.* = .{ .id = @intCast(i) };
}

/// Release ownership while retaining allocation and last owner, and signal lock-release events.
pub fn release_lock(hub: *Hub, lock: *Lock) void {
    lock.taken = false;
    hub.signal_lock(lock.id, false);
}

/// Notify selectable lock sensors of a taken/released edge and the corresponding change.
pub fn signal_lock(hub: *Hub, id: u4, taken: bool) void {
    for (&hub.cogs) |*cog| {
        cog.signal_selectable((if (taken) @as(u6, 0x10) else 0x20) | id);
        cog.signal_selectable(0x30 | @as(u6, id));
    }
}

/// Attention and selectable inputs cross two event-sampling registers.
pub fn signal_event(hub: *Hub, id: u3, mask: u16) void {
    if (hub.clocking) hub.pending_events[(hub.counter +% 2) % hub.pending_events.len][id] |= mask else hub.cogs[id].events |= mask;
}

/// Read little-endian bytes through the 20-bit RAM/hole/mirror map, wrapping each byte address.
pub fn read_memory(hub: *Hub, address: u32, size: u3) u32 {
    var value: u32 = 0;
    for (0..size) |i| {
        const byte_address = (address +% @as(u32, @intCast(i))) & 0xfffff;
        if (byte_address < 0x80000 or byte_address >= 0xfc000)
            value |= @as(u32, hub.memory[byte_address & 0x7ffff]) << @intCast(i * 8);
    }
    return value;
}

/// Write through the 20-bit hub map; ignore holes and optionally preserve bytes where the source is zero.
pub fn write_memory(hub: *Hub, address: u32, value: u32, size: u3, masked: bool) void {
    for (0..size) |i| {
        const byte: u8 = @truncate(value >> @intCast(i * 8));
        const byte_address = (address +% @as(u32, @intCast(i))) & 0xfffff;
        if ((byte_address < 0x80000 or byte_address >= 0xfc000) and (!masked or byte != 0)) hub.memory[byte_address & 0x7ffff] = byte;
    }
}

/// Process one shared clock edge: commit old writes, service resources, advance cogs, then UART and CT.
/// All cogs observe previous-edge writes before any cog executes on this edge.
pub fn step(hub: *Hub) void {
    hub.clocking = true;
    defer hub.clocking = false;
    const events = &hub.pending_events[hub.counter % hub.pending_events.len];
    for (&hub.cogs, events) |*cog, pending| cog.events |= pending;
    events.* = @splat(0);
    var lut_writes: [8]@TypeOf(hub.cogs[0].writeback.lut) = undefined;
    for (&hub.cogs, &lut_writes) |*cog, *write| write.* = cog.writeback.lut;
    for (&hub.cogs) |*cog| cog.commit_results();
    // Both RAM ports complete together. Distinct-address writes both become
    // visible. Same-address collisions vary by physical RAM instance; use a
    // deterministic local-write convention and expose the collision.
    for (&hub.cogs, lut_writes) |*cog, write| if (write) |value| {
        const other = cog.other();
        const own = lut_writes[other.id];
        if (other.lut_sharing and own != null and own.?.address == value.address) {
            other.lut_collision = .{ .clock = hub.counter, .address = value.address };
            other.trace_collision(value.address, .lut_write);
        }
        if (other.lut_sharing and (own == null or own.?.address != value.address)) other.lut[value.address] = value.value;
    };
    // All previous-edge writes are visible before any cog samples this edge.
    for (&hub.cogs) |*cog| if (cog.hub_writeback) |write| {
        hub.write_memory(write.address, write.value, write.size, write.masked);
        cog.hub_writeback = null;
    };
    for (&hub.pending_starts, 0..) |*pending, id| if (pending.*) |start| {
        if (start.at == hub.counter) {
            pending.* = null;
            hub.start_cog(@intCast(id), .{ .hub_address = start.address, .ptra = start.ptra, .load_image = start.load, .command_clock = start.command_clock }) catch unreachable;
        }
    };
    for (&hub.cogs) |*cog| cog.clock_startup();
    for (&hub.cogs) |*cog| cog.clock_fifo();
    for (&hub.cogs) |*cog| cog.clock_memory();
    for (&hub.cogs) |*cog| cog.clock_command();
    hub.io.updateDirections(hub);
    for (&hub.cogs) |*cog| {
        cog.step();
    }

    hub.io.step(hub);

    hub.counter +%= 1;
}

/// Return whether any cog is executing or waiting for startup to complete.
pub fn is_any_cog_active(hub: *Hub) bool {
    for (hub.cogs) |cog| {
        if (cog.exec_mode != .stopped)
            return true;
    }
    return false;
}

/// Safe only when all active pipelines are frozen on timed waits and no
/// peripheral, FIFO, memory transfer, or writeback can advance before CT does.
pub fn next_idle_clock(hub: *Hub) ?u64 {
    for (hub.pending_starts) |start| if (start != null) return null;
    if (hub.io.txBusy() or hub.io.input_index != hub.io.input.len) return null;
    for (hub.pending_events) |edge| for (edge) |events| if (events != 0) return null;
    var next_delta: ?u64 = null;
    for (&hub.cogs) |*cog| {
        if (cog.exec_mode == .stopped) continue;
        const deadline = cog.wait_until orelse return null;
        if (cog.exec_mode != .cog or cog.pipeline[3] == null or cog.pipeline[4] != null or
            cog.memory_transfer != null or cog.hub_writeback != null or cog.events != 0 or
            cog.writeback.count != 0 or cog.writeback.c != null or cog.writeback.z != null or cog.writeback.lut != null or clock_reached(hub.counter, deadline)) return null;
        if (cog.fifo) |fifo| {
            if (fifo.count < 15) return null;
            for (fifo.responses) |response| if (response != null) return null;
        }
        const delay = deadline -% hub.counter;
        next_delta = @min(next_delta orelse delay, delay);
        for (cog.ct_targets) |target| if (target) |value| {
            const delta = value -% @as(u32, @truncate(hub.counter));
            next_delta = @min(next_delta.?, delta);
        };
    }
    return if (next_delta) |delay| hub.counter +% delay else null;
}

/// Only the low 20 source bits select memory/PC; PTRB receives all 32 bits.
pub fn start_cog(hub: *Hub, index: u3, options: struct { hub_address: u32 = 0, ptra: u32 = 0, load_image: bool = true, clocked: bool = true, command_clock: ?u64 = null }) !void {
    const cog = &hub.cogs[index];
    cog.reset();
    const command_clock = options.command_clock orelse hub.counter;
    const base = options.hub_address & 0xffffc;
    if (options.load_image) {
        if (options.clocked) {
            cog.startup = .{ .address = base, .grant_at = command_clock +% 17 +% (((@as(u64, base) >> 2) -% (command_clock +% 17 +% index)) & 7), .remaining = 504 };
        } else for (0..504) |i| cog.registers.values[i] = hub.read_memory(base + @as(u32, @intCast(i * 4)), 4);
        cog.exec_mode = .cog;
    } else {
        cog.pc = @truncate(options.hub_address);
        cog.exec_mode = if (cog.pc < 0x400) .cog else .hub;
        if (options.clocked) {
            cog.startup = .{ .address = base, .grant_at = 0, .remaining = 0, .release_at = command_clock +% @as(u64, if (cog.exec_mode == .hub) 17 else 18) };
            if (cog.exec_mode == .hub) cog.fifo = .{
                .byte_offset = @truncate(cog.pc),
                .address = cog.pc & 0xffffc,
                .ready_at = command_clock +% 22,
            };
        }
    }
    cog.registers.set(.PTRA, options.ptra);
    cog.registers.set(.PTRB, options.hub_address);
}

test "UART DIR follows register writes, cog reset and simultaneous handoff" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);
    hub.init();
    hub.io.pins[62].mode = 0x7c;
    hub.io.pins[62].x = (100 << 16) | 7;
    hub.cogs[0].write_reg(.DIRB, 1 << 30);
    try std.testing.expect(hub.io.transmit('x'));
    hub.step();
    try std.testing.expect(hub.io.txBusy());
    hub.cogs[1].write_reg(.DIRB, 1 << 30);
    hub.cogs[0].reset();
    try std.testing.expect(hub.io.pins[62].enabled);
    try std.testing.expect(hub.io.txBusy());

    hub.cogs[0].write_reg(.DIRB, 1 << 30);
    hub.cogs[1].reset();
    // On one edge, cog 0 releases the pin and cog 1 takes over. The OR stays
    // high; a sequential transient must not reset the smart pin.
    hub.cogs[1].writeback.count = 1;
    hub.cogs[1].writeback.writes[0] = .{ .reg = .DIRB, .value = 1 << 30 };
    hub.cogs[0].writeback.count = 1;
    hub.cogs[0].writeback.writes[0] = .{ .reg = .DIRB, .value = 0 };
    hub.step();
    try std.testing.expect(hub.io.txBusy());
    hub.cogs[1].write_reg(.DIRB, 0);
    try std.testing.expect(!hub.io.pins[62].enabled);
    try std.testing.expect(!hub.io.txBusy());
    try std.testing.expect(!hub.io.pins[62].ready);
    try std.testing.expect(!hub.io.transmit('y'));

    hub.io.pins[63].mode = 0x3e;
    hub.io.pins[63].x = (100 << 16) | 7;
    hub.cogs[0].write_reg(.DIRB, 1 << 31);
    try hub.io.supplyInput("z", hub.counter);
    hub.cogs[0].write_reg(.DIRB, 0);
    hub.counter = hub.io.rx_end;
    hub.step();
    try std.testing.expect(!hub.io.pins[63].ready);
    try std.testing.expectEqual(@as(u32, 0), hub.io.pins[63].result);
}

test "pending events keep their two-clock delay across counter rollover" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);
    hub.init();
    hub.counter = std.math.maxInt(u64) - 1;
    hub.clocking = true;
    hub.signal_event(0, 0x8000);
    hub.clocking = false;
    for (0..2) |_| {
        hub.step();
        try std.testing.expectEqual(@as(u16, 0), hub.cogs[0].events);
    }
    hub.step();
    try std.testing.expectEqual(@as(u16, 0x8000), hub.cogs[0].events);
}

test "FIFO and hub reads keep five-clock responses across counter rollover" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);
    for (0..8) |offset| {
        hub.init();
        hub.counter = std.math.maxInt(u64) - offset;
        const fifo_address: u32 = @intCast((hub.counter & 7) * 4);
        const read_address: u32 = @intCast(((hub.counter +% 1) & 7) * 4);
        hub.write_memory(fifo_address, 0x1122_3344, 4, false);
        hub.write_memory(read_address, 0x5566_7788, 4, false);
        hub.cogs[0].fifo = .{ .address = @intCast(fifo_address), .byte_offset = 0, .ready_at = hub.counter };
        hub.cogs[1].memory_transfer = .{
            .address = read_address,
            .remaining = 1,
            .reg = 20,
            .lut = false,
            .timed = true,
            .size = 4,
            .grant_at = hub.counter,
            .grant_address = read_address,
            .grants_left = 1,
        };
        for (0..5) |_| {
            hub.step();
            try std.testing.expectEqual(@as(usize, 0), hub.cogs[0].fifo.?.count);
            try std.testing.expect(hub.cogs[1].memory_transfer.?.value == null);
        }
        hub.step();
        try std.testing.expectEqual(@as(usize, 1), hub.cogs[0].fifo.?.count);
        try std.testing.expectEqual(@as(u32, 0x1122_3344), hub.cogs[0].fifo.?.words[0]);
        try std.testing.expectEqual(@as(?u32, 0x5566_7788), hub.cogs[1].memory_transfer.?.value);
    }
}

test "wait deadlines and idle skipping remain future across counter rollover" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);
    hub.init();
    hub.counter = std.math.maxInt(u64) - 4;
    const cog = &hub.cogs[0];
    cog.exec_mode = .cog;
    cog.pipeline[3] = .{ .pc = 0, .instr = 0xFD64_161F }; // WAITX #11.
    hub.step();
    try std.testing.expectEqual(@as(?u64, 6), cog.wait_until);
    try std.testing.expectEqual(@as(?u64, 6), hub.next_idle_clock());
    hub.cogs[1].exec_mode = .cog;
    hub.cogs[1].pipeline[3] = .{ .pc = 0, .instr = 0xFD64_001F };
    hub.cogs[1].wait_until = std.math.maxInt(u64) - 1;
    try std.testing.expectEqual(@as(?u64, std.math.maxInt(u64) - 1), hub.next_idle_clock());
    hub.cogs[1].reset();
    for (1..11) |_| {
        hub.step();
        try std.testing.expect(cog.wait_until != null);
    }
    hub.step();
    try std.testing.expect(cog.wait_until == null);
}

test "UART and image startup finish at their scheduled clocks across rollover" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);
    hub.init();
    hub.counter = std.math.maxInt(u64) - 4;
    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();
    hub.output_writer = &output.writer;
    hub.io.pins[62].mode = 0x7c;
    hub.io.pins[62].x = (1 << 16) | 7;
    hub.cogs[0].write_reg(.DIRB, 1 << 30);
    try std.testing.expect(hub.io.transmit('x'));
    for (0..10) |_| {
        hub.step();
        try std.testing.expectEqual(@as(usize, 0), output.written().len);
    }
    hub.step();
    try std.testing.expectEqualStrings("x", output.written());

    hub.init();
    hub.counter = std.math.maxInt(u64) - 4;
    // The first loaded instruction stops cog 1; the other words are data.
    hub.write_memory(0x1000, 0xFD64_0203, 4, false);
    for (1..504) |i| hub.write_memory(@intCast(0x1000 + i * 4), @intCast(i), 4, false);
    try hub.start_cog(1, .{ .hub_address = 0x1000 });
    for (0..550) |_| hub.step();
    try std.testing.expect(hub.cogs[1].startup == null);
    try std.testing.expectEqual(Cog.ExecMode.stopped, hub.cogs[1].exec_mode);
    try std.testing.expectEqual(@as(u32, 0xFD64_0203), hub.cogs[1].registers.values[0]);
    for (1..504) |i| try std.testing.expectEqual(@as(u32, @intCast(i)), hub.cogs[1].registers.values[i]);
}
