//! Headless debugger, inspector, disassembly and host-frame regression coverage.
const std = @import("std");
const Hub = @import("sim/Hub.zig");
const Cog = @import("sim/Cog.zig");
const Debugger = @import("sim/Debugger.zig");
const Frame = @import("sim/Frame.zig");
const inspect = @import("sim/inspect.zig");
const disassembly = @import("sim/disassemble.zig");
const decode = @import("sim/decode.zig");
const encoding = @import("sim/encoding.zig");

comptime {
    _ = disassembly;
}
pub const std_options: std.Options = .{ .log_level = .err };

/// Allocate the large backing RAM off the test stack and initialize all links.
fn newHub() !*Hub {
    const hub = try std.testing.allocator.create(Hub);
    hub.init();
    return hub;
}

/// Advance at most a bounded number of edges, requiring a structured debugger stop.
fn untilStop(hub: *Hub, limit: usize) !void {
    for (0..limit) |_| {
        hub.step();
        if (hub.debugger.?.stop != .none)
            return;
    }
    return error.ExpectedDebuggerStop;
}

/// Add a watch covering an entire selected domain, optionally restricted to one local cog.
fn watch(debugger: *Debugger, space: Debugger.Space, cog: ?u3, scope: Debugger.Scope, access: Debugger.AccessMode) !void {
    try debugger.watches.append(debugger.allocator, .{
        .space = space,
        .cog = cog,
        .start = 0,
        .end = if (space == .hub) 0x80000 else 512,
        .scope = scope,
        .access = access,
    });
}

/// Build an execution encounter without advancing or preloading the pipeline.
fn encounter(cog: *Cog, pc: u20, word: u32, domain: Debugger.Domain) void {
    cog.exec_mode = if (domain == .hub) .hub else .cog;
    cog.pipeline[3] = .{
        .pc = pc,
        .instr = word,
        .local_address = if (domain == .hub) null else @as(u10, @truncate(pc)),
    };
}

test "execution stops precede the whole edge and acknowledge all simultaneous encounters" {
    const hub = try newHub();
    defer std.testing.allocator.destroy(hub);
    var debugger: Debugger = .{ .allocator = std.testing.allocator };
    defer debugger.deinit();
    hub.debugger = &debugger;
    for (0..2) |i| {
        encounter(&hub.cogs[i], 7, 0xf604_2809, .cog); // MOV r20, #9
        try debugger.executions.append(debugger.allocator, .{ .cog = @intCast(i), .domain = .cog, .pc = 7 });
    }
    hub.cogs[2].hub_writeback = .{
        .provenance = .{ .cog = 2 },
        .address = 8,
        .value = 99,
        .size = 1,
        .masked = false,
        .origin = .application,
    };
    const random = hub.random;
    hub.step();
    try std.testing.expectEqual(Debugger.Stop.execution, debugger.stop);
    try std.testing.expectEqual(@as(usize, 2), debugger.encounters.items.len);
    try std.testing.expectEqual(@as(u64, 0), hub.counter);
    try std.testing.expectEqual(@as(u8, 0), hub.memory[8]);
    try std.testing.expectEqualDeep(random, hub.random);
    debugger.acknowledge(hub);
    hub.step();
    try std.testing.expectEqual(@as(u64, 1), hub.counter);
    try std.testing.expectEqual(@as(u8, 99), hub.memory[8]);
    try std.testing.expectEqual(Debugger.Stop.none, debugger.stop);
    hub.step();
    try std.testing.expectEqual(@as(u32, 9), hub.cogs[0].registers.values[20]);
    try std.testing.expectEqual(@as(u32, 9), hub.cogs[1].registers.values[20]);
}

test "loop encounters retrigger but stalled retries do not" {
    const hub = try newHub();
    defer std.testing.allocator.destroy(hub);
    var debugger: Debugger = .{ .allocator = std.testing.allocator };
    defer debugger.deinit();
    hub.debugger = &debugger;
    const cog = &hub.cogs[0];
    cog.exec_mode = .cog;
    cog.registers.values[0] = 0xfd80_0000; // JMP #\$00000
    try debugger.executions.append(debugger.allocator, .{ .cog = 0, .domain = .cog, .pc = 0 });
    try untilStop(hub, 20);
    const first = hub.counter;
    debugger.acknowledge(hub);
    try untilStop(hub, 20);
    try std.testing.expect(hub.counter > first);
    debugger.acknowledge(hub);
    cog.reset();
    encounter(cog, 0, 0xfd64_141f, .cog); // WAITX #10
    try untilStop(hub, 1);
    debugger.acknowledge(hub);
    for (0..4) |_| {
        hub.step();
        try std.testing.expectEqual(Debugger.Stop.none, debugger.stop);
        try std.testing.expect(cog.pipeline[3] != null);
    }
}

test "execution matches domain and cog and excludes cancelled instructions" {
    const hub = try newHub();
    defer std.testing.allocator.destroy(hub);
    var debugger: Debugger = .{ .allocator = std.testing.allocator };
    defer debugger.deinit();
    hub.debugger = &debugger;
    encounter(&hub.cogs[0], 0x210, 0, .hub);
    try debugger.executions.append(debugger.allocator, .{ .cog = 0, .domain = .lut, .pc = 0x210 });
    try std.testing.expect(debugger.beforeClock(hub));
    encounter(&hub.cogs[1], 0x210, 0, .lut);
    try std.testing.expect(debugger.beforeClock(hub));
    encounter(&hub.cogs[0], 0x210, 0, .lut);
    hub.cogs[0].pipeline[3].?.cancelled = true;
    try std.testing.expect(debugger.beforeClock(hub));
    hub.cogs[0].pipeline[3].?.cancelled = false;
    hub.step();
    try std.testing.expectEqual(Debugger.Stop.execution, debugger.stop);
    try std.testing.expectEqual(@as(u64, 0), hub.counter);
}

test "data hits include same-value commits and every cog sharing the edge" {
    const hub = try newHub();
    defer std.testing.allocator.destroy(hub);
    var debugger: Debugger = .{ .allocator = std.testing.allocator };
    defer debugger.deinit();
    hub.debugger = &debugger;
    try watch(&debugger, .registers, null, .data, .write);
    for (0..2) |i| {
        const cog = &hub.cogs[i];
        cog.registers.values[20] = 9;
        encounter(cog, 7, 0xf604_2809, .cog);
    }
    hub.step(); // Issue both writes.
    try std.testing.expectEqual(@as(usize, 0), debugger.hits.items.len);
    hub.step(); // Commit both on the same edge.
    try std.testing.expectEqual(Debugger.Stop.data, debugger.stop);
    try std.testing.expectEqual(@as(u64, 2), hub.counter);
    try std.testing.expectEqual(@as(usize, 2), debugger.hits.items.len);
    for (debugger.hits.items, 0..) |hit, i| {
        try std.testing.expectEqual(@as(u64, 1), hit.clock);
        try std.testing.expectEqual(@as(?u3, @intCast(i)), hit.provenance.cog);
        try std.testing.expectEqual(@as(?u20, 7), hit.provenance.pc);
        try std.testing.expectEqual(@as(u32, 20), hit.address);
        try std.testing.expectEqual(@as(u32, 9), hit.value);
    }
}

test "physical preloads see cancelled words while data reads see only consumed operands" {
    const hub = try newHub();
    defer std.testing.allocator.destroy(hub);
    var debugger: Debugger = .{ .allocator = std.testing.allocator };
    defer debugger.deinit();
    hub.debugger = &debugger;
    try watch(&debugger, .registers, 0, .data, .read);
    try watch(&debugger, .registers, 0, .physical, .read);
    const cog = &hub.cogs[0];
    cog.exec_mode = .cog;
    cog.registers.values[20] = 123;
    cog.pipeline[2] = .{ .pc = 9, .local_address = 9, .instr = 0xf600_2a14, .cancelled = true }; // MOV r21,r20
    hub.step();
    try std.testing.expect(debugger.hits.items.len >= 6);
    for (debugger.hits.items) |hit| {
        try std.testing.expectEqual(@as(usize, 1), hit.watchpoint);
        try std.testing.expectEqual(Debugger.ReadClass.physical, hit.class);
    }
    debugger.acknowledge(hub);
    hub.step(); // Cancelled execute consumes nothing.
    for (debugger.hits.items) |hit| {
        try std.testing.expectEqual(@as(usize, 1), hit.watchpoint);
    }
    debugger.acknowledge(hub);
    encounter(cog, 9, 0xf600_2a14, .cog);
    cog.pipeline[3].?.operands = .{ .d = 0, .s = 123, .ptra = 0, .ptrb = 0, .pa = 0, .pb = 0 };
    hub.step();
    var consumed = false;
    for (debugger.hits.items) |hit| {
        if (hit.watchpoint == 0) {
            consumed = true;
            try std.testing.expectEqual(@as(u32, 20), hit.address);
            try std.testing.expectEqual(@as(u32, 123), hit.value);
            try std.testing.expectEqual(Debugger.ReadClass.consumed, hit.class);
        }
    }
    try std.testing.expect(consumed);
}

test "RAM watches use backing aliases and exclude holes protection and masked lanes across wrap" {
    const hub = try newHub();
    defer std.testing.allocator.destroy(hub);
    var debugger: Debugger = .{ .allocator = std.testing.allocator };
    defer debugger.deinit();
    hub.debugger = &debugger;
    try watch(&debugger, .hub, null, .data, .both);
    const cog = &hub.cogs[2];
    hub.counter = std.math.maxInt(u64);
    cog.hub_writeback = .{
        .provenance = .{ .cog = 2, .pc = 10, .instruction = 7 },
        .address = 0xfffff,
        .value = 0x11002233,
        .size = 4,
        .masked = true,
        .origin = .application,
    };
    hub.step();
    try std.testing.expectEqual(@as(u64, 0), hub.counter);
    try std.testing.expectEqual(@as(usize, 3), debugger.hits.items.len);
    try std.testing.expectEqual(@as(u32, 0x7ffff), debugger.hits.items[0].address);
    try std.testing.expectEqual(@as(u32, 0), debugger.hits.items[1].address);
    try std.testing.expectEqual(@as(u32, 2), debugger.hits.items[2].address);
    debugger.acknowledge(hub);
    hub.debug.protected = true;
    cog.hub_writeback = .{
        .provenance = .{ .cog = 2 },
        .address = 0xfc000,
        .value = 1,
        .size = 1,
        .masked = false,
        .origin = .application,
    };
    hub.step();
    try std.testing.expectEqual(@as(usize, 0), debugger.hits.items.len);
    cog.hub_writeback = .{
        .provenance = .{ .cog = 2 },
        .address = 0x80000,
        .value = 1,
        .size = 1,
        .masked = false,
        .origin = .debug,
    };
    hub.step();
    try std.testing.expectEqual(@as(usize, 0), debugger.hits.items.len);
    cog.hub_writeback = .{
        .provenance = .{ .cog = 2 },
        .address = 0xfc000,
        .value = 1,
        .size = 1,
        .masked = false,
        .origin = .debug,
    };
    hub.step();
    try std.testing.expectEqual(@as(usize, 1), debugger.hits.items.len);
    try std.testing.expectEqual(@as(u32, 0x7c000), debugger.hits.items[0].address);
}

test "startup reads and delayed RAM writes retain their issuer" {
    const hub = try newHub();
    defer std.testing.allocator.destroy(hub);
    var debugger: Debugger = .{ .allocator = std.testing.allocator };
    defer debugger.deinit();
    hub.debugger = &debugger;
    try watch(&debugger, .hub, null, .data, .read);
    try watch(&debugger, .registers, 1, .data, .write);
    hub.access_context = .{ .cog = 3, .pc = 44, .instruction = 99 };
    try hub.start_cog(1, .{});
    hub.access_context = .{};
    try untilStop(hub, 30);
    try std.testing.expectEqual(@as(?u3, 3), debugger.hits.items[0].provenance.cog);
    try std.testing.expectEqual(@as(?u20, 44), debugger.hits.items[0].provenance.pc);
    var saw_write = false;
    for (0..10) |_| {
        debugger.acknowledge(hub);
        hub.step();
        for (debugger.hits.items) |hit| {
            if (hit.space == .registers) {
                saw_write = true;
                try std.testing.expectEqual(@as(?u3, 3), hit.provenance.cog);
                try std.testing.expectEqual(@as(?u3, 1), hit.cog);
            }
        }
    }
    try std.testing.expect(saw_write);
}

test "shared LUT commits report receiver address and original issuer" {
    const hub = try newHub();
    defer std.testing.allocator.destroy(hub);
    var debugger: Debugger = .{ .allocator = std.testing.allocator };
    defer debugger.deinit();
    hub.debugger = &debugger;
    try watch(&debugger, .lut, null, .data, .write);
    debugger.watches.items[0].issuer = 0;
    hub.cogs[1].lut_sharing = true;
    hub.cogs[0].writeback.lut = .{ .address = 510, .value = 1234 };
    hub.cogs[0].writeback.provenance = .{ .cog = 0, .pc = 10, .instruction = 42 };
    hub.step();
    try std.testing.expectEqual(@as(usize, 2), debugger.hits.items.len);
    try std.testing.expectEqual(@as(?u3, 0), debugger.hits.items[0].cog);
    try std.testing.expectEqual(@as(?u3, 1), debugger.hits.items[1].cog);
    for (debugger.hits.items) |hit| {
        try std.testing.expectEqual(@as(?u3, 0), hit.provenance.cog);
        try std.testing.expectEqual(@as(?u20, 10), hit.provenance.pc);
        try std.testing.expectEqual(@as(u32, 510), hit.address);
    }
}

test "block writes read raw tail RAM and commit delayed hub lanes with provenance" {
    const hub = try newHub();
    defer std.testing.allocator.destroy(hub);
    var debugger: Debugger = .{ .allocator = std.testing.allocator };
    defer debugger.deinit();
    hub.debugger = &debugger;
    try watch(&debugger, .registers, 0, .data, .read);
    try watch(&debugger, .hub, null, .data, .write);
    const cog = &hub.cogs[0];
    cog.ram_tail[0] = 0x12345678;
    cog.write_reg(.PTRA, 0x100);
    cog.q = 0;
    cog.setq_pending = true;
    encounter(cog, 12, 0xfc64_0000 | (504 << 9) | 0x100, .cog); // WRLONG r504,PTRA
    var raw_seen = false;
    var committed = false;
    for (0..40) |_| {
        hub.step();
        for (debugger.hits.items) |hit| {
            if (hit.space == .registers and hit.address == 504 and hit.storage == .ram) {
                raw_seen = true;
                try std.testing.expectEqual(@as(u32, 0x12345678), hit.value);
            }
            if (hit.space == .hub and hit.address == 0x100) {
                committed = true;
                try std.testing.expectEqual(@as(?u20, 12), hit.provenance.pc);
                try std.testing.expectEqual(@as(u32, 0x78), hit.value);
            }
        }
        if (committed)
            break;
        debugger.acknowledge(hub);
    }
    try std.testing.expect(raw_seen and committed);
}

test "software FIFO reads consume data while prefetch is physical and writes preserve enqueue provenance" {
    const hub = try newHub();
    defer std.testing.allocator.destroy(hub);
    var debugger: Debugger = .{ .allocator = std.testing.allocator };
    defer debugger.deinit();
    hub.debugger = &debugger;
    try watch(&debugger, .hub, null, .data, .both);
    const cog = &hub.cogs[0];
    cog.begin_fifo_read(0x100);
    hub.write_memory(0x100, 0x12345678, 4, false);
    for (0..16) |_| {
        hub.step();
        try std.testing.expectEqual(Debugger.Stop.none, debugger.stop);
    }
    // RFLONG r20 consumes the already-prefetched bytes at its own instruction PC.
    encounter(cog, 7, 0xfd60_2812, .cog);
    try std.testing.expectEqual(decode.OpCode.rflong, decode.decode(cog.pipeline[3].?.instr));
    hub.step();
    try std.testing.expectEqual(@as(usize, 4), debugger.hits.items.len);
    try std.testing.expectEqual(@as(?u20, 7), debugger.hits.items[0].provenance.pc);
    try std.testing.expectEqual(@as(u32, 0x78), debugger.hits.items[0].value);
    debugger.acknowledge(hub);
    cog.reset();
    cog.begin_fifo(.write, 0, 0x200);
    encounter(cog, 15, 0, .cog);
    try std.testing.expect(cog.write_fifo(0xaabbccdd, 4));
    cog.pipeline[3] = null; // Enqueuing instruction is no longer present at commit.
    try untilStop(hub, 20);
    try std.testing.expectEqual(@as(usize, 4), debugger.hits.items.len);
    for (debugger.hits.items) |hit| {
        try std.testing.expectEqual(@as(?u20, 15), hit.provenance.pc);
    }
}

test "repeated inspection is pure and debugger without breakpoints preserves execution" {
    const detached = try newHub();
    defer std.testing.allocator.destroy(detached);
    const attached = try newHub();
    defer std.testing.allocator.destroy(attached);
    var debugger: Debugger = .{ .allocator = std.testing.allocator };
    defer debugger.deinit();
    attached.debugger = &debugger;
    for ([_]*Hub{ detached, attached }) |hub| {
        hub.cogs[0].exec_mode = .cog;
        hub.cogs[0].registers.values[0] = 0xf604_2809;
        hub.cogs[0].registers.values[1] = 0xfd80_0000;
        hub.cogs[0].stack = @splat(0xc0000210);
    }
    for (0..100) |_| {
        detached.step();
        attached.step();
        try std.testing.expectEqualSlices(u8, &detached.memory, &attached.memory);
        try std.testing.expectEqualSlices(u32, &detached.cogs[0].registers.values, &attached.cogs[0].registers.values);
        try std.testing.expectEqualDeep(detached.cogs[0].pipeline, attached.cogs[0].pipeline);
        try std.testing.expectEqualDeep(detached.random, attached.random);
        try std.testing.expectEqualDeep(detached.io, attached.io);
        try std.testing.expectEqualDeep(detached.cordic, attached.cordic);
        try std.testing.expectEqualDeep(detached.locks, attached.locks);
        try std.testing.expectEqual(detached.counter, attached.counter);
        for (detached.cogs, attached.cogs) |left, right| {
            try std.testing.expectEqualSlices(u32, &left.lut, &right.lut);
            try std.testing.expectEqualSlices(u32, &left.ram_tail, &right.ram_tail);
            try std.testing.expectEqualSlices(u32, &left.stack, &right.stack);
            try std.testing.expectEqualDeep(left.events, right.events);
            try std.testing.expectEqualDeep(left.interrupts, right.interrupts);
            try std.testing.expectEqualDeep(left.debug, right.debug);
            try std.testing.expectEqual(left.c, right.c);
            try std.testing.expectEqual(left.z, right.z);
            try std.testing.expectEqual(left.q, right.q);
        }
    }
    try watch(&debugger, .registers, null, .physical, .both);
    try watch(&debugger, .lut, null, .physical, .both);
    try watch(&debugger, .hub, null, .physical, .both);
    const before = attached.cogs[0].events;
    const counter = attached.counter;
    for (0..10) |_| {
        for (0..512) |i| {
            _ = inspect.register(&attached.cogs[0], @intCast(i));
            _ = inspect.annotations(&attached.cogs[0], .{ .domain = .cog, .address = @intCast(i) });
            _ = inspect.annotations(&attached.cogs[0], .{ .domain = .lut, .address = @intCast(i) });
        }
        _ = attached.read_memory(0, 4);
        _ = attached.cogs[0].peek_fifo(0);
    }
    try std.testing.expectEqualDeep(before, attached.cogs[0].events);
    try std.testing.expectEqual(counter, attached.counter);
    try std.testing.expectEqual(@as(usize, 0), debugger.hits.items.len);
}

test "annotations retain overlays physical locations packed flags and all sticky stack slots" {
    const hub = try newHub();
    defer std.testing.allocator.destroy(hub);
    const cog = &hub.cogs[0];
    cog.debug.state = .active;
    cog.debug.ijmp = 0xdeadbeef;
    cog.debug.iret = 0xc0000210;
    cog.registers.values[0x1fe] = 123;
    cog.ram_tail[6] = 456;
    try std.testing.expectEqual(@as(u32, 0xdeadbeef), inspect.register(cog, 0x1fe));
    const overlay = inspect.annotations(cog, .{ .domain = .cog, .address = 0x1fe });
    try std.testing.expect(overlay.special and overlay.debug_overlay and overlay.rom);
    cog.stack = @splat(0xc0000210);
    cog.registers.set(.IJMP1, 0x210);
    cog.registers.set(.IRET2, 0xc0000210);
    cog.pipeline[2] = .{ .pc = 0xa10, .local_address = 0x210, .instr = 0 };
    const badges = inspect.annotations(cog, .{ .domain = .lut, .address = 0x10 });
    try std.testing.expectEqual(@as(u8, 0xff), badges.stack);
    try std.testing.expectEqual(@as(u5, 4), badges.pipeline);
    try std.testing.expectEqual(@as(u4, 2), badges.vectors);
    try std.testing.expectEqual(@as(u4, 5), badges.returns);
    cog.exec_mode = .cog;
    cog.pc = 0xa10;
    try std.testing.expect(inspect.annotations(cog, .{ .domain = .lut, .address = 0x10 }).fetch);
    cog.pc = 0x800;
    cog.pipeline[2] = .{ .pc = 0x7ff, .instr = 0xf600000c };
    try std.testing.expect(inspect.annotations(cog, .{ .domain = .cog, .address = 12 }).fetch);
    cog.stack[3] = 0xc0012345;
    try std.testing.expectEqual(@as(u8, 8), inspect.annotations(cog, .{ .domain = .hub, .address = 0x12345 }).stack);
    try std.testing.expectEqual(inspect.Address{ .domain = .hub, .address = 0x12345 }, inspect.target(0xc0012345));
    try std.testing.expectEqual(inspect.Address{ .domain = .cog, .address = 12 }, inspect.target(0x8000000c));
}

const FakeClock = struct {
    value: i96 = 0,
    increment: i96,
    /// Return deterministic samples so deadline behavior is independent of test host speed.
    fn now(context: *anyopaque) i96 {
        const clock: *FakeClock = @ptrCast(@alignCast(context));
        const value = clock.value;
        clock.value += clock.increment;
        return value;
    }
    /// Expose the same clock contract used by the GUI's monotonic host clock.
    fn source(clock: *FakeClock) Frame.Clock {
        return .{ .context = clock, .now = now };
    }
};

test "frame budget expires between edges Step is one edge and fault or break interrupts immediately" {
    const hub = try newHub();
    defer std.testing.allocator.destroy(hub);
    var clock: FakeClock = .{ .increment = 7_000_000 };
    var frame = Frame.advance(hub, .run, clock.source());
    try std.testing.expectEqual(@as(u64, 1), frame.clocks);
    try std.testing.expect(frame.running);
    clock.increment = 20_000_000;
    frame = Frame.advance(hub, .run, clock.source());
    try std.testing.expectEqual(@as(u64, 0), frame.clocks);
    frame = Frame.advance(hub, .step, clock.source());
    try std.testing.expectEqual(@as(u64, 1), frame.clocks);
    try std.testing.expect(!frame.running);
    var debugger: Debugger = .{ .allocator = std.testing.allocator };
    defer debugger.deinit();
    hub.debugger = &debugger;
    encounter(&hub.cogs[0], 0, 0, .cog);
    try debugger.executions.append(debugger.allocator, .{ .cog = 0, .domain = .cog, .pc = 0 });
    clock.increment = 1;
    frame = Frame.advance(hub, .run, clock.source());
    try std.testing.expectEqual(@as(u64, 0), frame.clocks);
    try std.testing.expect(!frame.running);
    debugger.acknowledge(hub);
    hub.cogs[0].pipeline[3].?.instr = 0xfd600002; // Invalid encoding.
    frame = Frame.advance(hub, .run, clock.source());
    try std.testing.expectEqual(@as(u64, 1), frame.clocks);
    try std.testing.expect(hub.fault != null);
    try std.testing.expect(!frame.running);
    frame = Frame.advance(hub, .step, clock.source());
    try std.testing.expectEqual(@as(u64, 0), frame.clocks);
}

test "FIFO reloads retain actual backing read addresses while the architectural pointer advances" {
    const hub = try newHub();
    defer std.testing.allocator.destroy(hub);

    var debugger: Debugger = .{ .allocator = std.testing.allocator };
    defer debugger.deinit();

    hub.debugger = &debugger;
    try watch(&debugger, .hub, null, .data, .read);
    const cog = &hub.cogs[0];
    cog.begin_fifo_read(0x200);
    const fifo = &cog.fifo_state.engine.?;
    fifo.remaining = 1;
    fifo.block = .{ .address = 0x100, .longs = 1 };
    hub.write_memory(0x200, 0x11223344, 4, false);
    hub.write_memory(0x100, 0xaabbccdd, 4, false);
    for (0..24) |_| {
        hub.step();
    }
    encounter(cog, 7, 0xfd602812, .cog); // RFLONG r20
    hub.step();
    try std.testing.expectEqual(@as(usize, 4), debugger.hits.items.len);
    try std.testing.expectEqual(@as(u32, 0x200), debugger.hits.items[0].address);
    debugger.acknowledge(hub);
    encounter(cog, 8, 0xfd602812, .cog);
    hub.step();
    try std.testing.expectEqual(@as(usize, 4), debugger.hits.items.len);
    try std.testing.expectEqual(@as(u32, 0x100), debugger.hits.items[0].address);
    try std.testing.expectEqual(@as(u32, 0xdd), debugger.hits.items[0].value);
}

test "frame stops after a data-hit edge and host register edits preserve captured work" {
    const hub = try newHub();
    defer std.testing.allocator.destroy(hub);

    var debugger: Debugger = .{ .allocator = std.testing.allocator };
    defer debugger.deinit();

    hub.debugger = &debugger;
    try watch(&debugger, .registers, 0, .data, .write);
    const cog = &hub.cogs[0];
    encounter(cog, 3, 0xf6002a14, .cog); // MOV r21,r20
    cog.pipeline[3].?.operands = .{ .d = 0, .s = 123, .ptra = 0, .ptrb = 0, .pa = 0, .pb = 0 };
    cog.write_reg(@enumFromInt(20), 456);
    hub.step(); // Uses captured 123 and queues the write.
    cog.write_reg(@enumFromInt(21), 789);
    try std.testing.expectEqual(@as(usize, 0), debugger.hits.items.len);
    var clock: FakeClock = .{ .increment = 1 };
    const frame = Frame.advance(hub, .run, clock.source());
    try std.testing.expectEqual(@as(u64, 1), frame.clocks);
    try std.testing.expect(!frame.running);
    try std.testing.expectEqual(Debugger.Stop.data, debugger.stop);
    try std.testing.expectEqual(@as(u32, 123), cog.registers.values[21]);
    try std.testing.expectEqual(@as(u32, 456), cog.registers.values[20]);
}

test "pad output enable follows delayed GPIO and smart TT for all sixty-four pads" {
    const hub = try newHub();
    defer std.testing.allocator.destroy(hub);

    hub.cogs[0].write_reg(.DIRA, std.math.maxInt(u32));
    hub.cogs[0].write_reg(.DIRB, std.math.maxInt(u32));
    hub.step();
    for (0..64) |i| {
        try std.testing.expect(!hub.io.outputEnabled(@enumFromInt(i)));
    }
    for (0..5) |_| {
        hub.step();
    }
    for (0..64) |i| {
        try std.testing.expect(hub.io.outputEnabled(@enumFromInt(i)));
        hub.io.editConfiguration(@enumFromInt(i), 0x3c); // UART TX, TT off despite DIR high.
        try std.testing.expect(!hub.io.outputEnabled(@enumFromInt(i)));
        hub.io.editConfiguration(@enumFromInt(i), 0x7c); // UART TX, TT on.
        try std.testing.expect(hub.io.outputEnabled(@enumFromInt(i)));
    }
    hub.io.editConfiguration(@enumFromInt(0), 2); // Repository.
    hub.io.editSmartRegister(@enumFromInt(0), .x, 123);
    try std.testing.expectEqual(@as(u32, 123), hub.io.pins[0].smart.registers.x);
    hub.step();
    try std.testing.expectEqual(@as(u32, 123), hub.io.pins[0].smart.registers.result);
    try std.testing.expect(hub.io.pins[0].edits.x == null);
}
