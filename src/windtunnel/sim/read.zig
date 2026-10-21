//! Live split-read state and retained diagnostic payloads; no history or recycled references.
const std = @import("std");
const HubRam = @import("HubRam.zig");
const access_types = @import("access.zig");

pub const Sample = struct { address: u20, clock: u64, readable: HubRam.LaneMask };
pub const Write = struct { address: u19, clock: u64, value: u8, provenance: access_types.Provenance };
/// The edge already supplies its clock; do not duplicate that timestamp for every committed byte.
pub const CommittedWrite = struct { address: u19, value: u8, provenance: access_types.Provenance };
pub const Race = struct {
    address: u20,
    size: u3,
    samples: [2]Sample,
    lanes: [2]HubRam.LaneMask,
    writer: Write,
};

/// The entire detector descriptor. Timestamps and writer snapshots are diagnostic payloads.
pub const Boundary = packed struct(u64) {
    first: u20,
    next: u20,
    written: packed struct(u6) { lower: u3 = 0, upper: u3 = 0 } = .{},
    open: bool = true,
    complete: bool = false,
    reserved: u16 = 0,
};

/// A writer's address follows from the sample and lane instead of being copied per witness.
pub const Witness = struct {
    clock: u64,
    provenance: access_types.Provenance,
    value: u8,
    lane: u2,

    /// Snapshot an actual committed byte write.
    fn init(write: Write) Witness {
        return .{ .clock = write.clock, .provenance = write.provenance, .value = write.value, .lane = @truncate(write.address) };
    }

    /// Restore the canonical address in the public diagnostic payload.
    fn expand(witness: Witness, source: u20) Write {
        return .{
            .address = HubRam.Address.init(source).backing().? + witness.lane,
            .clock = witness.clock,
            .value = witness.value,
            .provenance = witness.provenance,
        };
    }
};

/// Sampling facts retained only while their data can still be consumed.
pub const Window = struct {
    boundary: Boundary,
    clocks: [2]u64 = undefined,
    readable: packed struct(u2) { first: bool, second: bool = false },

    /// Begin watching the two physical source longs of an unfinished read.
    pub fn init(first: Sample, next: u20) Window {
        std.debug.assert(first.address & 3 == 0 and next & 3 == 0);
        std.debug.assert(first.readable.count() == 0 or first.readable.count() == 4);
        return .{
            .boundary = .{ .first = first.address, .next = next },
            .clocks = .{ first.clock, undefined },
            .readable = .{ .first = first.readable.count() != 0 },
        };
    }

    /// Close the physical sampling interval; its final edge still admits lower-part writes.
    pub fn close(window: *Window, second: Sample) void {
        std.debug.assert(window.boundary.open and window.boundary.next == second.address);
        std.debug.assert(second.readable.count() == 0 or second.readable.count() == 4);
        window.boundary.open = false;
        window.boundary.complete = true;
        window.clocks[1] = second.clock;
        window.readable.second = second.readable.count() != 0;
    }

    /// Match a committed write to one physical half, with the documented endpoint semantics.
    pub fn matches(window: *const Window, part: usize, write: Write) bool {
        if (window.boundary.complete and write.clock != window.clocks[1])
            return false;
        const address = switch (part) {
            0 => blk: {
                if (!window.readable.first or write.clock == window.clocks[0])
                    return false;
                break :blk window.boundary.first;
            },
            1 => blk: {
                if (!window.boundary.open)
                    return false;
                break :blk window.boundary.next;
            },
            else => unreachable,
        };
        const backing = HubRam.Address.init(address).backing() orelse return false;
        return write.address / 4 == backing / 4;
    }

    /// Reconstruct public sampling metadata without storing redundant addresses or lane masks.
    pub fn samples(window: *const Window) [2]Sample {
        std.debug.assert(window.boundary.complete);
        return .{
            .{
                .address = window.boundary.first,
                .clock = window.clocks[0],
                .readable = if (window.readable.first)
                    .initFull()
                else
                    .initEmpty(),
            },
            .{
                .address = window.boundary.next,
                .clock = window.clocks[1],
                .readable = if (window.readable.second)
                    .initFull()
                else
                    .initEmpty(),
            },
        };
    }
};

/// Select the physical byte lanes of a logical crossing read, including three-byte FIFO spans.
pub fn lanes(address: u20, size: u3) [2]HubRam.LaneMask {
    const offset: u2 = @truncate(address);
    const lower: u3 = 4 - @as(u3, offset);
    std.debug.assert(size > lower and size <= 4);
    var result = [2]HubRam.LaneMask{ .initEmpty(), .initEmpty() };
    result[0].setRangeValue(.{ .start = offset, .end = 4 }, true);
    result[1].setRangeValue(.{ .start = 0, .end = size - lower }, true);
    return result;
}

/// Prefer chronological witnesses, then the original lower-to-upper lane ordering.
fn earlier(candidate: Witness, previous: Witness, start: u64) bool {
    const delta = candidate.clock -% start;
    const old_delta = previous.clock -% start;
    return delta < old_delta or (delta == old_delta and candidate.lane < previous.lane);
}

/// A known-width ordinary or startup read needs only one witness per source half.
pub const Split = struct {
    window: Window,
    witnesses: [2]Witness = undefined,
    address: u20,
    size: u3,

    /// Capture a selected write while keeping the two readability decisions independent.
    pub fn capture(split: *Split, write: Write) void {
        const selected = lanes(split.address, split.size);
        for (0..2) |part| {
            if (!selected[part].isSet(write.address % 4) or !split.window.matches(part, write))
                continue;
            const old = switch (part) {
                0 => split.window.boundary.written.lower,
                1 => split.window.boundary.written.upper,
                else => unreachable,
            };
            const witness = Witness.init(write);
            if (old == 0 or earlier(witness, split.witnesses[part], split.window.clocks[0])) {
                split.witnesses[part] = witness;
            }
            switch (part) {
                0 => {
                    split.window.boundary.written.lower = 1;
                },
                1 => {
                    split.window.boundary.written.upper = 1;
                },
                else => unreachable,
            }
        }
    }

    /// Finalize a diagnostic when the completed logical value reaches its consumer.
    pub fn race(split: *const Split) ?Race {
        std.debug.assert(split.window.boundary.complete);
        var chosen: ?usize = null;
        if (split.window.readable.first and split.window.boundary.written.lower != 0) {
            chosen = 0;
        }
        if (split.window.readable.second and split.window.boundary.written.upper != 0) {
            if (chosen == null or split.witnesses[1].clock -% split.window.clocks[0] <
                split.witnesses[0].clock -% split.window.clocks[0])
            {
                chosen = 1;
            }
        }
        const part = chosen orelse return null;
        const source = if (part == 0)
            split.window.boundary.first
        else
            split.window.boundary.next;
        return .{
            .address = split.address,
            .size = split.size,
            .samples = split.window.samples(),
            .lanes = lanes(split.address, split.size),
            .writer = split.witnesses[part].expand(source),
        };
    }
};

/// One actual FIFO long and its boundary metadata; six potentially selected bytes, never eight.
pub const FifoWord = struct {
    window: Window,
    witnesses: [6]Witness = undefined,
    value: u32,

    /// Capture the first witness for each possible crossing-consumer byte.
    pub fn capture(word: *FifoWord, write: Write) void {
        const lane: u2 = @truncate(write.address);
        for (0..2) |part| {
            if ((part == 0 and lane == 0) or (part == 1 and lane == 3) or !word.window.matches(part, write))
                continue;
            const bit: u2 = if (part == 0)
                lane - 1
            else
                lane;
            const mask = @as(u3, 1) << bit;
            const old = switch (part) {
                0 => word.window.boundary.written.lower,
                1 => word.window.boundary.written.upper,
                else => unreachable,
            };
            if (old & mask == 0) {
                word.witnesses[part * 3 + bit] = Witness.init(write);
                switch (part) {
                    0 => {
                        word.window.boundary.written.lower |= mask;
                    },
                    1 => {
                        word.window.boundary.written.upper |= mask;
                    },
                    else => unreachable,
                }
            }
        }
    }

    /// Select the retained witness only after the consumer's actual byte span is known.
    pub fn race(word: *const FifoWord, address: u20, size: u3) ?Race {
        if (!word.window.boundary.complete)
            return null;
        const selected = lanes(address, size);
        var chosen: ?usize = null;
        for (0..6) |index| {
            const part = index / 3;
            const bit: u2 = @intCast(index % 3);
            const lane: u2 = if (part == 0)
                bit + 1
            else
                bit;
            const mask = switch (part) {
                0 => word.window.boundary.written.lower,
                1 => word.window.boundary.written.upper,
                else => unreachable,
            };
            const readable = if (part == 0)
                word.window.readable.first
            else
                word.window.readable.second;
            if (!readable or !selected[part].isSet(lane) or mask & (@as(u3, 1) << bit) == 0)
                continue;
            if (chosen == null or word.witnesses[index].clock -% word.window.clocks[0] <
                word.witnesses[chosen.?].clock -% word.window.clocks[0])
            {
                chosen = index;
            }
        }
        const index = chosen orelse return null;
        const source = if (index < 3)
            word.window.boundary.first
        else
            word.window.boundary.next;
        return .{
            .address = address,
            .size = size,
            .samples = word.window.samples(),
            .lanes = selected,
            .writer = word.witnesses[index].expand(source),
        };
    }
};

comptime {
    std.debug.assert(@sizeOf(Boundary) == 8);
    std.debug.assert(@sizeOf(Witness) <= 32);
    std.debug.assert(@sizeOf(FifoWord) <= 256);
}

test "split endpoints selected lanes aliases delayed sampling and clock rollover" {
    const base: u20 = 0x7c000;
    const provenance: access_types.Provenance = .{ .cog = 6, .pc = 17, .instruction = 123 };
    for ([_]u64{ 0, std.math.maxInt(u64) - 1 }) |start| {
        for (0..8) |offset| {
            for (0..5) |phase| {
                var word: FifoWord = .{
                    .window = .init(.{ .address = base, .clock = start, .readable = .initFull() }, 0xfc004),
                    .value = 0,
                };
                var split: Split = .{ .window = word.window, .address = base + 3, .size = 2 };
                const write: Write = .{
                    .address = @as(u19, @intCast(base)) + @as(u19, @intCast(offset)),
                    .clock = start +% @as(u64, @intCast(phase)),
                    .value = 0x33,
                    .provenance = provenance,
                };
                for (0..4) |step| {
                    if (step == 3) {
                        const second: Sample = .{ .address = 0xfc004, .clock = start +% 3, .readable = .initFull() };
                        word.window.close(second);
                        split.window.close(second);
                    }
                    if (phase == step) {
                        word.capture(write);
                        split.capture(write);
                    }
                }
                const detected = word.race(base + 3, 2);
                const expected = (offset == 3 and phase >= 1 and phase <= 3) or
                    (offset == 4 and phase < 3);
                try std.testing.expectEqual(expected, detected != null);
                try std.testing.expectEqualDeep(detected, split.race());
                if (detected) |race| {
                    try std.testing.expectEqualDeep(write, race.writer);
                    try std.testing.expectEqual(start, race.samples[0].clock);
                    try std.testing.expectEqual(start +% 3, race.samples[1].clock);
                }
            }
        }
    }
}

test "every crossing consumer width selects witnesses after arbitrarily long buffering" {
    for (1..4) |offset| {
        for (2..5) |size| {
            if (offset + size <= 4)
                continue;
            for (0..8) |written| {
                var word: FifoWord = .{
                    .window = .init(.{ .address = 0x100, .clock = 10, .readable = .initFull() }, 0x104),
                    .value = 0,
                };
                word.capture(.{ .address = @intCast(0x100 + written), .clock = 2000, .value = 5, .provenance = .{ .cog = 2 } });
                word.window.close(.{ .address = 0x104, .clock = 4000, .readable = .initFull() });
                const race = word.race(@intCast(0x100 + offset), @intCast(size));
                try std.testing.expectEqual(written >= offset and written < offset + size, race != null);
            }
        }
    }
}

test "a newly protected second sample does not hide a valid lower-part witness" {
    var split: Split = .{
        .window = .init(.{ .address = 0, .clock = 0, .readable = .initFull() }, 4),
        .address = 3,
        .size = 2,
    };
    split.capture(.{ .address = 4, .clock = 0, .value = 7, .provenance = .{ .cog = 1 } });
    split.capture(.{ .address = 3, .clock = 2, .value = 9, .provenance = .{ .cog = 7 } });
    split.window.close(.{ .address = 4, .clock = 3, .readable = .initEmpty() });
    const race = split.race().?;
    try std.testing.expectEqual(@as(u19, 3), race.writer.address);
    try std.testing.expectEqual(@as(u64, 2), race.writer.clock);
}

test "writes after the closing edge cannot alter retained FIFO diagnostics" {
    var word: FifoWord = .{
        .window = .init(.{ .address = 0, .clock = 0, .readable = .initFull() }, 4),
        .value = 0,
    };
    word.capture(.{ .address = 4, .clock = 0, .value = 7, .provenance = .{ .cog = 1 } });
    word.window.close(.{ .address = 4, .clock = 1, .readable = .initFull() });
    const before = word.race(3, 2).?;
    word.capture(.{ .address = 3, .clock = 1000, .value = 9, .provenance = .{ .cog = 7 } });
    try std.testing.expectEqualDeep(before, word.race(3, 2).?);
    try std.testing.expectEqual(@as(u3, 0), word.window.boundary.written.lower);
}
