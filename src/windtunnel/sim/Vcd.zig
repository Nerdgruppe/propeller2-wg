//! Digital traces: one synthetic nanosecond denotes one simulated system clock.
const std = @import("std");
const Hub = @import("Hub.zig");
const IO = @import("IO.zig");
const signal = @import("signal.zig");
const Signal = signal.Signal;
const Vcd = @This();

writer: *std.Io.Writer,
previous: ?Snapshot = null,
last_clock: u64 = 0,
elapsed: u128 = 0,

const names = [_][]const u8{ "OUTA", "OUTB", "DIRA", "DIRB", "INA", "INB" };
const Snapshot = struct { cog: [8][6]u32, combined: [6]u32, pads: [64]Signal, ct: u64 };

fn snapshot(hub: *const Hub) Snapshot {
    const input = hub.io.get_in();
    var state: Snapshot = .{
        .cog = undefined,
        .combined = .{ @truncate(hub.io.outputs), @truncate(hub.io.outputs >> 32), @truncate(hub.io.directions), @truncate(hub.io.directions >> 32), @truncate(input), @truncate(input >> 32) },
        .pads = hub.io.pads,
        .ct = hub.counter,
    };
    for (&hub.cogs, &state.cog) |*cog, *registers| {
        registers.* = .{ cog.registers.get(.OUTA), cog.registers.get(.OUTB), cog.registers.get(.DIRA), cog.registers.get(.DIRB), @truncate(input), @truncate(input >> 32) };
    }
    return state;
}

fn header(vcd: *Vcd) !void {
    const writer = vcd.writer;
    try writer.writeAll("$version Windtunnel $end\n$comment One time unit is one simulated system clock, not elapsed wall time. $end\n$timescale 1ns $end\n$scope module windtunnel $end\n$var wire 64 ct CT $end\n");
    for (0..8) |cog| {
        try writer.print("$scope module cog{d} $end\n", .{cog});
        for (names, 0..) |name, reg| try writer.print("$var wire 32 v{d} {s} $end\n", .{ cog * 6 + reg, name });
        try writer.writeAll("$upscope $end\n");
    }
    try writer.writeAll("$scope module io $end\n");
    for (names, 0..) |name, reg| try writer.print("$var wire 32 v{d} {s} $end\n", .{ 48 + reg, name });
    try writer.writeAll("$scope module pads $end\n");
    for (0..64) |pin| try writer.print("$var wire 1 p{d} P{d} $end\n", .{ pin, pin });
    try writer.writeAll("$upscope $end\n$upscope $end\n$upscope $end\n$enddefinitions $end\n#0\n$dumpvars\n");
}

pub fn capture(vcd: *Vcd, hub: *const Hub) !void {
    const current = snapshot(hub);
    const previous = vcd.previous;
    if (previous == null) {
        try vcd.header();
    } else {
        vcd.elapsed += hub.counter -% vcd.last_clock;
        if (std.meta.eql(previous.?, current)) return;
        try vcd.writer.print("#{d}\n", .{vcd.elapsed});
    }
    if (previous == null or current.ct != previous.?.ct) try vcd.writer.print("b{b} ct\n", .{current.ct});
    for (current.cog, 0..) |registers, cog| for (registers, 0..) |value, reg| {
        if (previous == null or value != previous.?.cog[cog][reg]) try vcd.writer.print("b{b} v{d}\n", .{ value, cog * 6 + reg });
    };
    for (current.combined, 0..) |value, reg| {
        if (previous == null or value != previous.?.combined[reg]) try vcd.writer.print("b{b} v{d}\n", .{ value, 48 + reg });
    }
    for (current.pads, 0..) |value, pin| {
        const bit: u8 = switch (value) {
            .zero => '0',
            .one => '1',
            .x => 'x',
            .z => 'z',
        };
        if (previous == null or value != previous.?.pads[pin]) try vcd.writer.print("{c}p{d}\n", .{ bit, pin });
    }
    if (previous == null) try vcd.writer.writeAll("$end\n");
    vcd.previous = current;
    vcd.last_clock = hub.counter;
}

pub fn finish(vcd: *Vcd) !void {
    try vcd.writer.flush();
}

test "initial declarations, pad changes, sparse clocks and CT rollover" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);
    hub.init();
    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();
    var vcd: Vcd = .{ .writer = &output.writer };
    hub.counter = std.math.maxInt(u64) - 1;
    try vcd.capture(hub);
    hub.io.setExternal(@enumFromInt(63), .one);
    hub.counter = 0;
    hub.step();
    try vcd.capture(hub);
    try vcd.capture(hub); // No duplicate timestamp for an unchanged snapshot.
    try vcd.finish();
    const text = output.written();
    try std.testing.expect(std.mem.indexOf(u8, text, "$scope module cog7 $end") != null);
    try std.testing.expect(std.mem.indexOf(u8, text, "$var wire 1 p63 P63 $end") != null);
    try std.testing.expect(std.mem.indexOf(u8, text, "zp63\n") != null);
    try std.testing.expect(std.mem.indexOf(u8, text, "#3\n") != null);
    try std.testing.expect(std.mem.indexOf(u8, text, "1p63\n") != null);
    try std.testing.expectEqual(@as(u128, 3), vcd.elapsed);
}

test "tracing does not change shared command timing or idle-clock eligibility" {
    const plain = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(plain);
    const traced = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(traced);
    plain.init();
    traced.init();
    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();
    var vcd: Vcd = .{ .writer = &output.writer };
    try traced.attachVcd(&vcd);
    for ([_]*Hub{ plain, traced }) |hub| {
        hub.cogs[0].write_reg(.DIRA, 1 << 8);
        hub.io.enqueue(hub, .{ .mask = 1 << 8, .kind = .configure, .value = 2, .cog = 0, .pc = 0, .instruction = 0 });
        for (0..IO.command_delay + 1) |_| hub.step();
        hub.io.enqueue(hub, .{ .mask = 1 << 8, .kind = .write_x, .value = 123, .cog = 0, .pc = 0, .instruction = 0 });
    }
    for (0..10) |_| {
        plain.step();
        traced.step();
        try std.testing.expectEqual(plain.counter, traced.counter);
        try std.testing.expectEqual(plain.io.get_in(), traced.io.get_in());
        try std.testing.expectEqual(plain.io.pins[8].smart.registers.result, traced.io.pins[8].smart.registers.result);
        try std.testing.expectEqual(plain.next_idle_clock(), traced.next_idle_clock());
    }
}
