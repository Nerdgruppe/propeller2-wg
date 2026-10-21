//! Completed-edge issue, provenance and allocation invariants.
const std = @import("std");
const test_support = @import("test_support.zig");
const access_types = @import("sim/access.zig");
const p2 = @import("p2");
const Hub = @import("sim/Hub.zig");
const Cog = @import("sim/Cog.zig");
const Issue = @import("sim/Issue.zig");
const Streamer = @import("sim/Streamer.zig");
const Debugger = @import("sim/Debugger.zig");
const execute = @import("sim/execute.zig");
const Vcd = @import("sim/Vcd.zig");
const Frame = @import("sim/Frame.zig");
const smart_pin = @import("sim/smart_pin.zig");

test "rejected streamer commands preserve idle active and full-buffer states" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    for ([_]u32{ 0xfca0_0000, 0xfcb0_0000, 0xfcc0_0000 }) |opcode| {
        for ([_]u32{ 0xf008_0000, 0xf009_0000 }) |mode| {
            for ([_]u16{ 0, 1, 0xffff }) |count| {
                for (0..3) |occupancy| {
                    try hub.init(.ideal);
                    const cog = &hub.cogs[2];
                    cog.exec_mode = .cog;
                    if (occupancy > 0) {
                        cog.streamer.apply(cog, .{ .command = try Streamer.Command.init(
                            0x4000_ffff,
                            0xa5a5_a5a5,
                            0,
                            .immediate,
                            .{ .cog = 2, .pc = 9, .instruction = 123 },
                        ) });
                    }
                    if (occupancy > 1) {
                        cog.streamer.apply(cog, .{ .command = try Streamer.Command.init(
                            0x4000_ffff,
                            0x5a5a_5a5a,
                            null,
                            .continuous,
                            .{ .cog = 2, .pc = 10, .instruction = 124 },
                        ) });
                    }
                    const saved = cog.streamer;
                    // Both augmented immediates are captured even when the command is rejected.
                    const augmentation_mask: u32 = 0xffff_fe00;
                    cog.augd = mode | (@as(u32, count) & augmentation_mask);
                    cog.augd_pending = true;
                    cog.augs = 0x7654_3200;
                    cog.augs_pending = true;
                    cog.q = 0x1234_5678;
                    cog.setq_pending = true;
                    const word = opcode | 0x000c_0000 | (@as(u32, count & 511) << 9) | 0x12;
                    cog.pipeline[3] = .{ .pc = 33, .instr = word };
                    try std.testing.expectEqual(Cog.ExecResult.next, execute.execute_instruction(cog, cog.pipeline[3].?));
                    try std.testing.expectEqualDeep(saved, cog.streamer);
                    try std.testing.expectEqual(@as(usize, 1), hub.issues.len);
                    const issue = hub.issues.items()[0];
                    try std.testing.expectEqual(Issue.Severity.trap, issue.severity());
                    try std.testing.expectEqual(Issue.ModeReason.reserved_encoding, issue.kind.streamer_mode.reason);
                    try std.testing.expectEqual(mode | count, issue.kind.streamer_mode.control);
                    try std.testing.expectEqual(@as(u32, 0x7654_3212), issue.kind.streamer_mode.data);
                    try std.testing.expectEqual(@as(?u32, 0x1234_5678), issue.kind.streamer_mode.frequency);
                    try std.testing.expectEqual(@as(?u20, 33), issue.provenance.pc);
                    try std.testing.expect(!cog.setq_pending and !cog.augs_pending and !cog.augd_pending);
                }
            }
        }
    }
}

test "all cogs complete a hazardous edge and pause preserves the reports" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    for ([_]u3{ 0, 4 }) |id| {
        const cog = &hub.cogs[id];
        cog.exec_mode = .cog;
        cog.pipeline[3] = .{ .pc = 22, .instr = 0xfd60_0002 }; // Unassigned slot.
    }
    hub.cogs[7].writeback.count = 1;
    hub.cogs[7].writeback.writes[0] = .{
        .provenance = .{ .cog = 7, .pc = 19, .instruction = 0xf604_5407 },
        .reg = @enumFromInt(42),
        .value = 7,
    };
    try std.testing.expectEqual(Hub.StepResult.normal, hub.step());
    try std.testing.expectEqual(@as(u64, 1), hub.counter);
    try std.testing.expectEqual(@as(u32, 7), hub.cogs[7].registers.values[42]);
    try std.testing.expectEqual(@as(usize, 2), hub.issues.len);
    try std.testing.expectEqual(@as(?u3, 0), hub.issues.items()[0].provenance.cog);
    try std.testing.expectEqual(@as(?u3, 4), hub.issues.items()[1].provenance.cog);
    try std.testing.expect(hub.cogs[0].exec_mode == .cog and hub.cogs[4].exec_mode == .cog);
    _ = hub.step();
    try std.testing.expectEqual(@as(usize, 0), hub.issues.len);
}

test "actual fetch collisions retain different issuing instructions" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    const cog = &hub.cogs[3];
    cog.exec_mode = .cog;
    cog.pc = 42;
    cog.writeback.count = 1;
    const writer: access_types.Provenance = .{ .cog = 3, .pc = 19, .instruction = 0xf604_5400 };
    cog.writeback.writes[0] = .{ .provenance = writer, .reg = @enumFromInt(42), .value = 0 };
    _ = hub.step();
    try std.testing.expectEqual(@as(usize, 0), hub.issues.len);
    for (0..3) |_| {
        _ = hub.step();
    }
    try std.testing.expectEqual(@as(usize, 1), hub.issues.len);
    const conflict = hub.issues.items()[0].kind.cog_fetch_write;
    try std.testing.expectEqualDeep(writer, conflict.writer.provenance);
    try std.testing.expectEqual(@as(u64, 0), conflict.clock);
    try std.testing.expectEqual(@as(u64, 3), hub.issues.items()[0].clock);
    try std.testing.expectEqual(@as(?u20, 42), conflict.reader.provenance.pc);
    try std.testing.expectEqual(@as(?u32, 0), conflict.reader.provenance.instruction);
    try std.testing.expectEqual(@as(u32, 42), conflict.reader.address);
}

test "discarded and condition-false fetches stay silent and stalled execution reports once" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    const Outcome = enum { cancelled, condition_false, stalled };
    for (std.enums.values(Outcome)) |outcome| {
        try hub.init(.ideal);
        const cog = &hub.cogs[0];
        cog.exec_mode = .cog;
        cog.pc = 42;
        const word: u32 = switch (outcome) {
            .cancelled => 0,
            .condition_false => 0x8604_0000, // MOV IF_C_AND_Z with both flags clear.
            .stalled => 0xfd64_161f, // WAITX #11.
        };
        cog.writeback.count = 1;
        cog.writeback.writes[0] = .{
            .provenance = .{ .cog = 0, .pc = 19, .instruction = 123 },
            .reg = @enumFromInt(42),
            .value = word,
        };
        _ = hub.step();
        try std.testing.expectEqual(@as(usize, 0), hub.issues.len);
        const fetched = cog.pipeline[1].?;
        try std.testing.expect(fetched.fetch_conflict_slot != null);
        cog.pipeline = @splat(null);
        cog.pipeline[3] = fetched;
        cog.pipeline[3].?.cancelled = outcome == .cancelled;
        cog.pc = 0;
        var reported: usize = 0;
        for (0..20) |_| {
            _ = hub.step();
            for (hub.issues.items()) |issue| {
                try std.testing.expect(issue.kind == .cog_fetch_write);
                try std.testing.expectEqual(@as(u64, 0), issue.kind.cog_fetch_write.clock);
                reported += 1;
            }
        }
        const expected: usize = switch (outcome) {
            .cancelled, .condition_false => 0,
            .stalled => 1,
        };
        try std.testing.expectEqual(expected, reported);
    }
}

test "unreadable long accesses report once per failure kind without hiding readable lanes" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    hub.clocking = true;
    try std.testing.expectEqual(@as(u32, 0), hub.read_memory(0x80000, 4));
    try std.testing.expectEqual(@as(usize, 1), hub.issues.len);
    try std.testing.expectEqual(@as(u20, 0x80000), hub.issues.items()[0].kind.memory_hole_read.address);
    hub.issues.clear();
    hub.memory[0x7fffe] = 0x12;
    hub.memory[0x7ffff] = 0x34;
    try std.testing.expectEqual(@as(u32, 0x3412), hub.read_memory(0x7fffe, 4));
    try std.testing.expectEqual(@as(usize, 1), hub.issues.len);
    try std.testing.expectEqual(@as(u20, 0x80000), hub.issues.items()[0].kind.memory_hole_read.address);
    hub.issues.clear();
    hub.debug.protected = true;
    try std.testing.expectEqual(@as(u32, 0), hub.read_memory(0x7fffe, 4));
    try std.testing.expectEqual(@as(usize, 2), hub.issues.len);
    try std.testing.expectEqual(@as(u20, 0x7fffe), hub.issues.items()[0].kind.memory_protected_read.address);
    try std.testing.expectEqual(@as(u20, 0x80000), hub.issues.items()[1].kind.memory_hole_read.address);
}

test "analog substeps aggregate extrema and firmware reset retains same-edge issues" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    var configuration = Hub.SystemConfiguration.ideal;
    configuration.oversample = 128;
    try hub.init(configuration);
    try hub.io.setExternal(@enumFromInt(0), .{ .voltage = 5, .resistance = 1000 });
    _ = hub.step();
    try std.testing.expectEqual(@as(usize, 1), hub.issues.len);
    const pad = hub.issues.items()[0].kind.pad_overvoltage;
    try std.testing.expectEqual(@as(u6, 0), pad.pin);
    try std.testing.expect(pad.maximum >= pad.minimum and pad.minimum > pad.limit);

    try hub.init(.ideal);
    var debugger: Debugger = .{ .allocator = std.testing.allocator };
    defer debugger.deinit();

    try debugger.addWatch(.{
        .space = .registers,
        .cog = null,
        .start = 0,
        .end = 512,
        .access = .write,
    });
    hub.debugger = &debugger;
    const writer: access_types.Provenance = .{ .cog = 2, .pc = 99, .instruction = 123 };
    hub.cogs[2].writeback.count = 1;
    hub.cogs[2].writeback.writes[0] = .{
        .provenance = writer,
        .reg = @enumFromInt(20),
        .value = 7,
    };
    hub.cogs[0].exec_mode = .cog;
    hub.debug.protected = true;
    // A deferred protected write reports before the reset's legal command slot.
    hub.cogs[2].hub_writeback = .{
        .provenance = writer,
        .address = 0x7c008,
        .value = 42,
        .size = 1,
        .masked = false,
        .origin = .application,
    };
    hub.cogs[0].registers.values[20] = 0x1000_0000;
    hub.cogs[0].pipeline[3] = .{ .pc = 40, .instr = 0xfd60_2800 };
    try std.testing.expectEqual(Hub.StepResult.reset, hub.step());
    try std.testing.expectEqual(@as(usize, 1), hub.issues.len);
    try std.testing.expectEqual(@as(u20, 0x7c008), hub.issues.items()[0].kind.memory_protected_write.address);
    try std.testing.expectEqualDeep(writer, hub.issues.items()[0].provenance);
    try std.testing.expectEqual(@as(u64, 0), hub.counter);
    debugger.afterClock(hub);
    hub.debugger = &debugger;
    try std.testing.expectEqual(Debugger.Stop.data, debugger.stop);
    try std.testing.expectEqual(@as(usize, 1), debugger.hits.items.len);
    try std.testing.expectEqualDeep(writer, debugger.hits.items[0].provenance);
    debugger.acknowledge(hub);
    _ = hub.step();
    try std.testing.expectEqual(@as(usize, 0), hub.issues.len);
}

test "maximum simultaneous breakpoint encounters and overlapping watches never allocate while stepping" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    var debugger: Debugger = .{ .allocator = std.testing.allocator };
    defer debugger.deinit();
    hub.debugger = &debugger;
    for (&hub.cogs) |*cog| {
        cog.exec_mode = .cog;
        cog.pc = 42;
        cog.pipeline[3] = .{ .pc = 40, .instr = 0xfd60_0002 };
        cog.pipeline[2] = .{ .pc = 39, .instr = 0 };
        cog.writeback.count = 3;
        for (0..3) |index| {
            cog.writeback.writes[index] = .{
                .provenance = .{ .cog = cog.id, .pc = 37, .instruction = 123 },
                .reg = @enumFromInt(20 + index),
                .value = 0,
            };
        }
        try debugger.toggleExecution(.{ .cog = cog.id, .domain = .cog, .pc = 40 });
    }
    for (0..16) |_| {
        try debugger.addWatch(.{ .space = .registers, .cog = null, .start = 0, .end = 512, .access = .both, .scope = .physical });
    }
    var exported: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer exported.deinit();
    try exported.ensureTotalCapacity(1 << 20);
    var vcd: Vcd = .{ .writer = &exported.writer };
    hub.attachVcd(&vcd);
    var failing = std.testing.FailingAllocator.init(std.testing.allocator, .{ .fail_index = 0, .resize_fail_index = 0 });
    debugger.allocator = failing.allocator();
    exported.allocator = failing.allocator();
    try std.testing.expectEqual(Hub.StepResult.breakpoint, hub.step());
    try std.testing.expectEqual(@as(usize, 8), debugger.encounters.items.len);
    debugger.acknowledge(hub);
    try std.testing.expectEqual(Hub.StepResult.breakpoint, hub.step());
    try std.testing.expectEqual(@as(usize, 8), hub.issues.len);
    try std.testing.expectEqual(@as(usize, 8 * (3 + 6 + 1) * 16), debugger.hits.items.len);
    try std.testing.expect(!failing.has_induced_failure);
}

test "issue storage holds its bound and coalesces repeated electrical limits" {
    const collection = try std.testing.allocator.create(Issue.Collection);
    defer std.testing.allocator.destroy(collection);

    collection.* = .{};
    for (0..Issue.Collection.capacity) |_| {
        collection.append(.{ .clock = 0, .provenance = .{}, .kind = .{ .invalid_encoding = {} } });
    }
    try std.testing.expectEqual(Issue.Collection.capacity, collection.items().len);
    collection.clear();
    for (0..1024) |index| {
        collection.analog(.{ .clock = 0, .provenance = .{}, .kind = .{ .pad_overvoltage = .{
            .pin = 0,
            .minimum = @floatFromInt(index),
            .maximum = @floatFromInt(index),
            .limit = 3.6,
        } } });
    }
    try std.testing.expectEqual(@as(usize, 1), collection.len);
    try std.testing.expectEqual(@as(f64, 0), collection.items()[0].kind.pad_overvoltage.minimum);
    try std.testing.expectEqual(@as(f64, 1023), collection.items()[0].kind.pad_overvoltage.maximum);
}

test "source sink and VCD failures complete the edge and stop retrying independently" {
    const Endpoints = struct {
        calls: usize = 0,
        broken: bool,

        /// Count an adapter read and optionally fail at its closed service boundary.
        fn read(context: *anyopaque) smart_pin.ReadError!?u32 {
            const endpoint: *@This() = @ptrCast(@alignCast(context));
            endpoint.calls += 1;
            if (endpoint.broken)
                return error.ReadFailed;
            return 42;
        }

        /// Count an adapter write and optionally fail at its closed service boundary.
        fn write(context: *anyopaque, _: u32) smart_pin.WriteError!void {
            const endpoint: *@This() = @ptrCast(@alignCast(context));
            endpoint.calls += 1;
            if (endpoint.broken)
                return error.WriteFailed;
        }
    };
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    var broken_reader: Endpoints = .{ .broken = true };
    var reader: Endpoints = .{ .broken = false };
    var broken_writer: Endpoints = .{ .broken = true };
    var writer: Endpoints = .{ .broken = false };
    var sources = [_]smart_pin.DataSource{
        .{ .endpoint = .{ .callback = .{ .context = &broken_reader, .read = Endpoints.read } } },
        .{ .endpoint = .{ .callback = .{ .context = &reader, .read = Endpoints.read } } },
    };
    var sinks = [_]smart_pin.DataSink{
        .{ .endpoint = .{ .callback = .{ .context = &broken_writer, .write = Endpoints.write } } },
        .{ .endpoint = .{ .callback = .{ .context = &writer, .write = Endpoints.write } } },
    };
    hub.cogs[0].write_reg(.DIRA, 15);
    for (&sources, 0..) |*source, index| {
        const pin = &hub.io.pins[index].smart;
        pin.configure(.uart_rx);
        pin.rx_mode = .{ .virtual = source };
        pin.registers.x = 7;
    }
    for (&sinks, 0..) |*sink, index| {
        const pin = &hub.io.pins[index + 2].smart;
        pin.configure(.uart_tx);
        pin.tx_mode = .{ .virtual = sink };
        pin.registers.x = 7;
        hub.io.editSmartRegister(@enumFromInt(index + 2), .y, 42);
    }
    var initial: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer initial.deinit();

    var vcd: Vcd = .{ .writer = &initial.writer };
    hub.attachVcd(&vcd);
    var tiny_buffer: [1]u8 = undefined;
    var tiny = std.Io.Writer.fixed(&tiny_buffer);
    vcd.writer = &tiny;
    _ = hub.step();
    try std.testing.expectEqual(@as(u64, 1), hub.counter);
    try std.testing.expect(hub.issues.len == 0 and test_support.serviceFailed(hub));
    try std.testing.expectEqual(error.ReadFailed, sources[0].failure.?);
    try std.testing.expectEqual(error.WriteFailed, sinks[0].failure.?);
    try std.testing.expectEqual(error.WriteFailed, vcd.failure.?);
    try std.testing.expectEqual(@as(usize, 1), reader.calls);
    try std.testing.expectEqual(@as(usize, 1), writer.calls);
    try std.testing.expect(hub.io.pins[1].smart.registers.ready);
    const StoppedClock = struct {
        /// A failed host service must prevent another edge before reading its frame clock.
        fn now(_: *anyopaque) i96 {
            @panic("failed adapter advanced the host frame");
        }
    };
    var clock_context: u8 = 0;
    const frame = if (vcd.failure == null and sources[0].failure == null and sinks[0].failure == null)
        Frame.advance(hub, .run, .{ .context = &clock_context, .now = StoppedClock.now }, test_support.needsAttention)
    else
        Frame{};
    try std.testing.expectEqual(@as(u64, 0), frame.clocks);
    try std.testing.expect(!frame.running);
    try std.testing.expectEqual(@as(u64, 1), hub.counter);
    const export_length = tiny.end;
    _ = hub.step();
    try std.testing.expectEqual(@as(u64, 2), hub.counter);
    try std.testing.expectEqual(@as(usize, 1), broken_reader.calls);
    try std.testing.expectEqual(@as(usize, 1), broken_writer.calls);
    try std.testing.expectEqual(export_length, tiny.end);
    var diagnostic: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer diagnostic.deinit();

    try test_support.diagnostics(hub, &diagnostic.writer);
    for ([_][]const u8{ "pin 0 source service: ReadFailed", "pin 2 sink service: WriteFailed", "VCD service: WriteFailed" }) |expected| {
        try std.testing.expect(std.mem.indexOf(u8, diagnostic.written(), expected) != null);
    }
}

test "late fetch address selection still finds the actual committed writer" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    const cog = &hub.cogs[0];
    cog.exec_mode = .cog;
    cog.pc = 40;
    cog.issue_phase = false;
    cog.writeback.count = 1;
    cog.writeback.writes[0] = .{
        .provenance = .{ .cog = 0, .pc = 17, .instruction = 123 },
        .reg = @enumFromInt(42),
        .value = 0,
    };
    hub.clocking = true;
    defer hub.clocking = false;

    cog.commit_results();
    try std.testing.expect(cog.ram_collision == null);
    cog.pc = 42;
    cog.issue_phase = true;
    cog.step();
    try std.testing.expectEqual(@as(usize, 0), hub.issues.len);
    const slot = cog.pipeline[1].?.fetch_conflict_slot.?;
    const conflict = cog.fetch_conflicts[slot].items[0];
    try std.testing.expectEqual(@as(?u20, 17), conflict.writer.provenance.pc);
    try std.testing.expectEqual(@as(?u20, 42), conflict.reader.provenance.pc);
}

test "register and LUT write conflicts retain distinct writer provenance and committed values" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    hub.clocking = true;
    defer hub.clocking = false;

    const first: access_types.Provenance = .{ .cog = 3, .pc = 17, .instruction = 123 };
    const second: access_types.Provenance = .{ .cog = 3, .pc = 19, .instruction = 456 };
    hub.access_context = first;
    hub.cogs[3].write_reg(@enumFromInt(42), 0x1122_3344);
    hub.cogs[3].write_lut(17, 0x5566_7788);
    hub.access_context = second;
    hub.cogs[3].write_reg(@enumFromInt(42), 0xaabb_ccdd);
    hub.cogs[3].write_lut(17, 0xeeff_0011);
    try std.testing.expectEqual(@as(usize, 2), hub.issues.len);
    const registers = hub.issues.items()[0].kind.reg_write_conflict;
    const lut = hub.issues.items()[1].kind.lut_write_conflict;
    for ([_]Issue.Writers{ registers, lut }) |conflict| {
        try std.testing.expectEqualDeep(first, conflict.first.provenance);
        try std.testing.expectEqualDeep(second, conflict.second.provenance);
        try std.testing.expectEqual(conflict.first.address, conflict.second.address);
        try std.testing.expectEqual(@as(?u3, 3), conflict.second.cog);
    }
    try std.testing.expectEqual(@as(u32, 42), registers.first.address);
    try std.testing.expectEqual(@as(u32, 17), lut.first.address);
    try std.testing.expectEqual(@as(u32, 0xaabb_ccdd), hub.cogs[3].registers.values[42]);
    try std.testing.expectEqual(@as(u32, 0xeeff_0011), hub.cogs[3].lut[17]);
}

test "replacement of pending FIFO and startup work retains both original issuers" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    const cog = &hub.cogs[3];
    cog.fifo_state.engine = .{
        .mode = .write,
        .byte_offset = 0,
        .address = 0x2000,
        .ready_at = null,
        .write_count = 1,
        .write_pending = .{ 0, 1 },
    };
    cog.pipeline[3] = .{ .pc = 17, .instr = 123 };
    const first = cog.provenance();
    cog.begin_fifo(.read, 0, 0x3000);
    cog.pipeline[3] = .{ .pc = 19, .instr = 456 };
    const second = cog.provenance();
    cog.begin_fifo(.read, 0, 0x4000);
    const fifo = hub.issues.items()[0].kind.fifo_configuration_lost;
    try std.testing.expectEqualDeep(first, fifo.previous);
    try std.testing.expectEqualDeep(second, fifo.replacement);
    try std.testing.expectEqual(@as(u20, 0x4000), cog.fifo_state.request.?.block.address);
    hub.pending_starts[3] = .{
        .provenance = first,
        .at = 12,
        .address = 0x5000,
        .ptra = 0,
        .load = true,
        .command_clock = 0,
    };
    hub.access_context = second;
    hub.start_cog(3, .{ .hub_address = 0x6000 });
    try std.testing.expectEqual(@as(usize, 2), hub.issues.len);
    const start = hub.issues.items()[1].kind.cog_start_replaced;
    try std.testing.expectEqualDeep(first, start.previous);
    try std.testing.expectEqualDeep(second, start.replacement);
    try std.testing.expectEqual(@as(u20, 0x6000), cog.startup.?.address);
}

test "streamer capture loss and FIFO ownership loss preserve command provenance" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    const cog = &hub.cogs[2];
    const issuer: access_types.Provenance = .{ .cog = 2, .pc = 17, .instruction = 123 };
    cog.begin_fifo(.write, 0, 0x2000);
    cog.fifo_state.engine.?.write_count = cog.fifo_state.engine.?.write_bytes.len;
    cog.fifo_state.engine.?.write_pending[1] = cog.fifo_state.engine.?.write_count;
    const command = try Streamer.Command.init(0xf081_ffff, 0, 0x8000_0000, .immediate, issuer);
    cog.streamer.apply(cog, .{ .command = command });
    try std.testing.expectEqual(@as(usize, 1), hub.issues.len);
    try std.testing.expectEqualDeep(issuer, hub.issues.items()[0].provenance);
    try std.testing.expectEqual(command.control, hub.issues.items()[0].kind.streamer_capture_dropped.control);
    hub.issues.clear();
    cog.fifo_state.engine = null;
    cog.streamer.step(cog);
    cog.streamer.step(cog);
    try std.testing.expectEqual(@as(usize, 1), hub.issues.len);
    try std.testing.expectEqualDeep(issuer, hub.issues.items()[0].provenance);
    try std.testing.expectEqual(command.control, hub.issues.items()[0].kind.streamer_fifo_lost.control);
    try std.testing.expect(cog.streamer.active != null);
}

test "clock fallback and unsafe postdivider transitions report the issuing configuration" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    var configuration = Hub.SystemConfiguration.realistic;
    configuration.clocks.crystal.frequency = 12_000_000;
    try hub.init(configuration);
    const issuer: access_types.Provenance = .{ .cog = 4, .pc = 17, .instruction = 123 };
    hub.access_context = issuer;
    hub.configure(0x0100_07fb);
    try std.testing.expectEqual(@as(usize, 1), hub.issues.len);
    try std.testing.expectEqual(@as(u32, 0x0100_07fb), hub.issues.items()[0].kind.uncharacterized_pll);
    try std.testing.expectEqualDeep(issuer, hub.issues.items()[0].provenance);

    try hub.init(.ideal);
    hub.access_context = issuer;
    hub.configure(0x0100_0f0b);
    try std.testing.expectEqual(@as(usize, 0), hub.issues.len);
    hub.configure(0x0100_0ffb);
    try std.testing.expectEqual(@as(usize, 1), hub.issues.len);
    try std.testing.expectEqual(@as(u32, 0x0100_0ffb), hub.issues.items()[0].kind.unsafe_postdivider);
    try std.testing.expectEqualDeep(issuer, hub.issues.items()[0].provenance);
}

test "electrical violations from different pins retain signed extrema across substeps" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    var configuration = Hub.SystemConfiguration.ideal;
    configuration.oversample = 128;
    try hub.init(configuration);
    hub.cogs[0].write_reg(.DIRA, 3);
    try hub.io.setExternal(@enumFromInt(0), .{ .voltage = -1, .resistance = 1 });
    try hub.io.setExternal(@enumFromInt(1), .{ .voltage = 5, .resistance = 1 });
    for (0..8) |_| {
        _ = hub.step();
    }
    var undervoltage = false;
    var overvoltage = false;
    var overcurrent: [2]bool = @splat(false);
    for (hub.issues.items()) |issue| {
        switch (issue.kind) {
            .pad_undervoltage => |pad| {
                try std.testing.expectEqual(@as(u6, 0), pad.pin);
                try std.testing.expect(pad.minimum <= pad.maximum and pad.maximum < pad.limit);
                undervoltage = true;
            },
            .pad_overvoltage => |pad| {
                try std.testing.expectEqual(@as(u6, 1), pad.pin);
                try std.testing.expect(pad.minimum <= pad.maximum and pad.minimum > pad.limit);
                overvoltage = true;
            },
            .pad_overcurrent => |pad| {
                try std.testing.expect(pad.minimum <= pad.maximum);
                try std.testing.expect(@max(@abs(pad.minimum), @abs(pad.maximum)) > pad.limit);
                overcurrent[pad.pin] = true;
            },
            else => return error.UnexpectedIssue,
        }
    }
    try std.testing.expect(undervoltage and overvoltage);
    try std.testing.expect(overcurrent[0] and overcurrent[1]);
    try std.testing.expectEqual(@as(usize, 4), hub.issues.len);
}

test "overflow fetch reports the old cog RAM port value and its actual writer" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    const cog = &hub.cogs[3];
    cog.exec_mode = .cog;
    cog.pc = 0x400;
    cog.pipeline[1] = .{ .pc = 0x3fe, .instr = 42 };
    cog.pipeline[2] = .{ .pc = 0x3ff, .instr = 42 };
    cog.registers.values[42] = 0;
    const writer: access_types.Provenance = .{ .cog = 3, .pc = 19, .instruction = 123 };
    cog.writeback.count = 1;
    cog.writeback.writes[0] = .{ .provenance = writer, .reg = @enumFromInt(42), .value = 456 };
    _ = hub.step();
    try std.testing.expectEqual(@as(usize, 1), hub.issues.len); // PC-domain warning at the physical read.
    const fetched = cog.pipeline[1].?;
    cog.pipeline = @splat(null);
    cog.pipeline[3] = fetched;
    cog.pc = 0;
    _ = hub.step();
    try std.testing.expectEqual(@as(usize, 1), hub.issues.len);
    const conflict = hub.issues.items()[0].kind.cog_fetch_write;
    try std.testing.expectEqualDeep(writer, conflict.writer.provenance);
    try std.testing.expectEqual(@as(u32, 456), conflict.writer.value);
    try std.testing.expectEqual(@as(u32, 0), conflict.reader.value);
    try std.testing.expectEqual(@as(?u20, 0x400), conflict.reader.provenance.pc);
    try std.testing.expectEqual(@as(u9, 42), conflict.read_source.old_cog_ram);
    try std.testing.expectEqual(@as(u32, 456), cog.registers.values[42]);
}

test "paired LUT write retains the delayed writer and original streamer sample source" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    const cog = &hub.cogs[0];
    cog.exec_mode = .cog;
    cog.lut_sharing = true;
    cog.lut[0] = 0x1122_3344;
    cog.begin_fifo(.read, 0, 0x2000);
    const fifo = &cog.fifo_state.engine.?;
    fifo.words[0] = 2; // LUT indices zero, then one, from consecutive source bits.
    fifo.word_addresses[0] = 0x2000;
    fifo.word_valid[0] = 0xf;
    fifo.count = 1;
    const reader: access_types.Provenance = .{ .cog = 0, .pc = 17, .instruction = 123 };
    const command = try Streamer.Command.init(0x7082_ffff, 0, 0x8000_0000, .immediate, reader);
    cog.streamer.apply(cog, .{ .command = command });
    cog.streamer.step(cog);
    const writer: access_types.Provenance = .{ .cog = 1, .pc = 19, .instruction = 456 };
    hub.cogs[1].writeback.provenance = writer;
    hub.cogs[1].writeback.lut = .{ .address = 1, .value = 0x5566_7788 };
    cog.pipeline[3] = .{ .pc = 99, .instr = 0 };
    _ = hub.step();
    try std.testing.expectEqual(@as(usize, 1), hub.issues.len);
    const conflict = hub.issues.items()[0].kind.lut_read_write;
    try std.testing.expectEqualDeep(writer, conflict.writer.provenance);
    try std.testing.expectEqualDeep(reader, conflict.reader.provenance);
    try std.testing.expectEqual(@as(?u3, 0), conflict.writer.cog);
    try std.testing.expectEqual(@as(u32, 1), conflict.writer.address);
    try std.testing.expectEqual(@as(u32, 1), conflict.reader.address);
    try std.testing.expectEqual(@as(u32, 0x1122_3344), conflict.reader.value);
    try std.testing.expectEqual(@as(u9, 0), conflict.read_source.retained_lut);
    try std.testing.expectEqual(@as(u32, 0x5566_7788), cog.lut[1]);
}

test "memory map and protection failures distinguish access kind address and severity" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    const Expected = struct { kind: std.meta.Tag(Issue.Kind), severity: Issue.Severity };
    const Case = struct {
        address: u32,
        protected: bool,
        origin: Hub.WriteOrigin,
        read: ?Expected,
        write: ?Expected,
    };
    const cases = [_]Case{
        .{
            .address = 0x80000,
            .protected = false,
            .origin = .application,
            .read = .{ .kind = .memory_hole_read, .severity = .warning },
            .write = .{ .kind = .memory_hole_write, .severity = .warning },
        },
        .{
            .address = 0x1fbfff,
            .protected = true,
            .origin = .application,
            .read = .{ .kind = .memory_hole_read, .severity = .warning },
            .write = .{ .kind = .memory_hole_write, .severity = .warning },
        },
        .{ .address = 0x7c000, .protected = false, .origin = .application, .read = null, .write = null },
        .{
            .address = 0x7c000,
            .protected = true,
            .origin = .application,
            .read = .{ .kind = .memory_protected_read, .severity = .trap },
            .write = .{ .kind = .memory_protected_write, .severity = .trap },
        },
        .{
            .address = 0xfc000,
            .protected = true,
            .origin = .application,
            .read = null,
            .write = .{ .kind = .memory_protected_write, .severity = .trap },
        },
        .{ .address = 0xfc000, .protected = true, .origin = .debug, .read = null, .write = null },
    };
    const issuer: access_types.Provenance = .{ .cog = 3, .pc = 17, .instruction = 123 };
    for (cases) |case| {
        try hub.init(.ideal);
        hub.debug.protected = case.protected;
        hub.access_context = issuer;
        const backing = case.address & 0x7ffff;
        hub.memory[backing] = 0x33;
        hub.clocking = true;
        const read = hub.read_memory(case.address, 1);
        if (case.read) |expected| {
            try std.testing.expectEqual(@as(u32, 0), read);
            try std.testing.expectEqual(@as(usize, 1), hub.issues.len);
            const issue = hub.issues.items()[0];
            try std.testing.expectEqual(expected.kind, std.meta.activeTag(issue.kind));
            try std.testing.expectEqual(expected.severity, issue.severity());
            try std.testing.expectEqualDeep(issuer, issue.provenance);
            switch (issue.kind) {
                .memory_hole_read, .memory_protected_read => |payload| {
                    try std.testing.expectEqual(case.address & 0xfffff, payload.address);
                },
                else => return error.UnexpectedIssue,
            }
        } else {
            try std.testing.expectEqual(@as(u32, 0x33), read);
            try std.testing.expectEqual(@as(usize, 0), hub.issues.len);
        }
        hub.issues.clear();
        hub.write_memory_from(case.address, 0x5a, 1, false, case.origin);
        if (case.write) |expected| {
            try std.testing.expectEqual(@as(u8, 0x33), hub.memory[backing]);
            try std.testing.expectEqual(@as(usize, 1), hub.issues.len);
            const issue = hub.issues.items()[0];
            try std.testing.expectEqual(expected.kind, std.meta.activeTag(issue.kind));
            try std.testing.expectEqual(expected.severity, issue.severity());
            try std.testing.expectEqualDeep(issuer, issue.provenance);
            switch (issue.kind) {
                .memory_hole_write, .memory_protected_write => |payload| {
                    try std.testing.expectEqual(case.address & 0xfffff, payload.address);
                    try std.testing.expectEqual(@as(u8, 0x5a), payload.value);
                },
                else => return error.UnexpectedIssue,
            }
        } else {
            try std.testing.expectEqual(@as(u8, 0x5a), hub.memory[backing]);
            try std.testing.expectEqual(@as(usize, 0), hub.issues.len);
        }
        hub.clocking = false;
    }
    hub.clocking = true;
    for ([_]u20{ 0x7c000, 0x80000, 0xfc000 }) |address| {
        hub.write_memory(address, 0, 4, true);
    }
    try std.testing.expectEqual(@as(usize, 0), hub.issues.len);
}

test "only dither DAC modes warn when a written fractional code exceeds full scale" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    for ([_]p2.types.SmartPinMode{ .dac_noise, .dac_dither_rnd, .dac_dither_pwm }) |mode| {
        for ([_]bool{ false, true }) |dac_mode| {
            for ([_]u32{ 0xff00, 0xffff, 0x1234_0000 }) |value| {
                try hub.init(.ideal);
                const configuration: p2.types.PinConfiguration = .{
                    .mode = mode,
                    .pad_mode = if (dac_mode) .dac_990r_600r else .logic,
                };
                hub.io.editConfiguration(@enumFromInt(0), @bitCast(configuration));
                hub.io.editSmartRegister(@enumFromInt(0), .y, value);
                _ = hub.step();
                const clipped = dac_mode and mode != .dac_noise and value == 0xffff;
                try std.testing.expectEqual(@as(usize, @intFromBool(clipped)), hub.issues.len);
                if (clipped) {
                    const issue = hub.issues.items()[0];
                    try std.testing.expectEqual(Issue.Severity.warning, issue.severity());
                    try std.testing.expectEqual(value, issue.kind.smart_dac_clipped.value);
                }
            }
        }
    }
}

test "capture overflow requires drain gaps rather than steady full-rate sequential traffic" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    for ([_]bool{ false, true }) |change_bank| {
        try hub.init(.ideal);
        const cog = &hub.cogs[2];
        cog.exec_mode = .cog;
        cog.pipeline[3] = .{ .pc = 0, .instr = 0xf604_0000, .ready_at = 10_000 };
        cog.begin_fifo(.write, 1, 0x2000);
        const stride: u32 = if (change_bank) 28 else 64;
        var next_address: u32 = 0x2000 + stride;
        cog.fifo_block(1, next_address);
        const issuer: access_types.Provenance = .{ .cog = 2, .pc = 17, .instruction = 123 };
        cog.streamer.apply(cog, .{ .command = try Streamer.Command.init(
            0xf081_ffff,
            0,
            0x8000_0000,
            .immediate,
            issuer,
        ) });
        var dropped: usize = 0;
        var maximum: usize = 0;
        for (0..512) |_| {
            if (cog.events.contains(.FBW)) {
                cog.events.remove(.FBW);
                next_address += stride;
                cog.fifo_block(1, next_address);
            }
            _ = hub.step();
            maximum = @max(maximum, cog.fifo_state.engine.?.write_count);
            for (hub.issues.items()) |issue| {
                try std.testing.expect(issue.kind == .streamer_capture_dropped);
                try std.testing.expectEqualDeep(issuer, issue.provenance);
                dropped += 1;
            }
        }
        if (change_bank) {
            try std.testing.expect(dropped != 0);
            try std.testing.expectEqual(@as(usize, 76), maximum);
        } else {
            try std.testing.expectEqual(@as(usize, 0), dropped);
            try std.testing.expect(maximum < 76);
        }
    }
}

test "invalid HUBSET commands and clock modes remain warning diagnostics" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    const issuer: access_types.Provenance = .{ .cog = 3, .pc = 17, .instruction = 123 };
    for ([_]u32{ 0x3000_0000, 0x5fff_ffff, 0x6000_0000, 0x7fff_ffff, 2, 3, 0xfb }) |mode| {
        try hub.init(.ideal);
        const before = hub.io.clock;
        hub.access_context = issuer;
        hub.configure(mode);
        try std.testing.expectEqual(@as(usize, 1), hub.issues.len);
        const issue = hub.issues.items()[0];
        try std.testing.expectEqual(mode, issue.kind.unsupported_hubset);
        try std.testing.expectEqual(Issue.Severity.warning, issue.severity());
        try std.testing.expectEqualDeep(issuer, issue.provenance);
        try std.testing.expectEqualDeep(before, hub.io.clock);
        try std.testing.expect(!hub.reset_requested);
    }
}

test "eight FIFO users arbitrate reads and writes to the same mirrored hub block" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    for (&hub.cogs) |*cog| {
        cog.exec_mode = .cog;
        cog.pipeline[3] = .{ .pc = 0, .instr = 0xf604_0000, .ready_at = 10_000 };
        const mode: Cog.FifoMode = if (cog.id % 2 == 0)
            .read
        else
            .write;
        const address: u32 = if (mode == .read)
            0x7c000
        else
            0xfc000;
        cog.begin_fifo(mode, 1, address);
    }
    var reads: usize = 0;
    var writes: usize = 0;
    for (0..512) |_| {
        for (&hub.cogs) |*cog| {
            if (cog.fifo_state.engine.?.mode == .read) {
                _ = cog.read_fifo(4);
            } else {
                _ = cog.write_fifo(0x1122_3344, 4);
            }
        }
        const clock = hub.counter;
        try std.testing.expectEqual(Hub.StepResult.normal, hub.step());
        var grants: usize = 0;
        for (&hub.cogs) |*cog| {
            if (cog.fifo_state.granted_at == clock) {
                grants += 1;
                switch (cog.fifo_state.engine.?.mode) {
                    .read => reads += 1,
                    .write => writes += 1,
                }
            }
        }
        try std.testing.expectEqual(grants, hub.hub_transfer_count);
        try std.testing.expectEqual(@as(usize, 0), hub.issues.len);
    }
    try std.testing.expect(reads > 128 and writes > 128);
}

test "unaligned block-write byte lanes form one physical grant" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    for (1..4) |alignment| {
        try hub.init(.ideal);
        const cog = &hub.cogs[3];
        cog.exec_mode = .cog;
        cog.registers.set(.PTRA, @as(u32, @intCast(0x2000 + alignment)));
        cog.registers.values[20] = 0x1122_3344;
        cog.registers.values[21] = 0x5566_7788;
        cog.q = 1;
        cog.setq_pending = true;
        const instruction: u32 = 0xfc64_0000 | (20 << 9) | 0x100; // WRLONG r20, PTRA.
        cog.pipeline[3] = .{ .pc = 17, .instr = instruction };
        var combined = false;
        for (0..64) |_| {
            _ = hub.step();
            try std.testing.expectEqual(@as(usize, 0), hub.issues.len);
            for (hub.hub_transfers[0..hub.hub_transfer_count]) |transfer| {
                if (transfer.address == 0x2004 / 4 and transfer.lanes.count() == 4) {
                    try std.testing.expectEqual(@as(usize, 1), hub.hub_transfer_count);
                    combined = true;
                }
            }
            if (hub.counter > 1 and cog.memory_transfer == null and cog.hub_writeback == null)
                break;
        }
        try std.testing.expect(combined);
        const address: u32 = @intCast(0x2000 + alignment);
        try std.testing.expectEqual(@as(u32, 0x1122_3344), hub.read_memory(address, 4));
        try std.testing.expectEqual(@as(u32, 0x5566_7788), hub.read_memory(address + 4, 4));
    }
}

test "shared commands use distinct cog slots across counter rollover" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    hub.counter = std.math.maxInt(u64) - 3;
    for (&hub.cogs) |*cog| {
        cog.exec_mode = .cog;
        cog.registers.values[20] = 0x7000_0000;
        cog.pipeline[3] = .{ .pc = 40, .instr = 0xfd60_2800 };
    }
    var seen = std.StaticBitSet(8).initEmpty();
    for (0..8) |_| {
        const expected: u3 = @truncate(hub.counter);
        try std.testing.expectEqual(Hub.StepResult.normal, hub.step());
        try std.testing.expectEqual(@as(?u3, expected), hub.command_issuer);
        try std.testing.expect(!seen.isSet(expected));
        seen.set(expected);
        try std.testing.expectEqual(@as(usize, 1), hub.issues.len);
        try std.testing.expectEqual(@as(?u3, expected), hub.issues.items()[0].provenance.cog);
        try std.testing.expectEqual(@as(u32, 0x7000_0000), hub.issues.items()[0].kind.unsupported_hubset);
    }
    try std.testing.expectEqual(@as(usize, 8), seen.count());
    _ = hub.step();
    try std.testing.expect(hub.command_issuer == null);
}

test "registered streamer delivery and pending cog startup survive counter rollover" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    hub.counter = std.math.maxInt(u64);
    const cog = &hub.cogs[0];
    const command = try Streamer.Command.init(0x4000_ffff, 0, null, .immediate, .{ .cog = 0 });
    hub.clocking = true;
    cog.streamer.apply(cog, .{ .frequency = 0x1234_5678 });
    cog.streamer.apply(cog, .{ .command = command });
    hub.clocking = false;
    hub.pending_starts[1] = .{
        .provenance = .{ .cog = 0 },
        .at = 1,
        .address = 0,
        .ptra = 0,
        .load = false,
        .command_clock = 1,
    };
    _ = hub.step();
    try std.testing.expect(cog.streamer.issued != null and cog.streamer.issued_frequency != null);
    _ = hub.step();
    try std.testing.expect(cog.streamer.issued == null and cog.streamer.issued_frequency == null);
    try std.testing.expectEqual(@as(u32, 0x1234_5678), cog.streamer.frequency);
    try std.testing.expect(hub.pending_starts[1] != null);
    _ = hub.step();
    try std.testing.expect(hub.pending_starts[1] == null);
    try std.testing.expect(hub.cogs[1].startup != null);
    for (0..32) |_| {
        _ = hub.step();
    }
    try std.testing.expect(hub.cogs[1].startup == null);
}
