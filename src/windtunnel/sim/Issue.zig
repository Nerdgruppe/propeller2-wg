//! Bounded, allocation-free diagnostics for a completed simulation edge.
const std = @import("std");
const access_types = @import("access.zig");
const Issue = @This();

pub const Severity = enum { warning, trap };
pub const ModeReason = enum { reserved_encoding, uncharacterized_behavior };
pub const Participant = struct {
    provenance: access_types.Provenance,
    space: access_types.Space,
    cog: ?u3,
    address: u32,
    access: access_types.Access,
    value: u32,
    class: access_types.ReadClass,
    storage: access_types.Storage,
};
pub const ReadSource = union(enum) {
    addressed: access_types.Storage,
    old_cog_ram: u9,
    retained_lut: u9,
};
pub const Conflict = struct {
    /// Clock of the physical overlap; a fetched instruction can report it later at execution.
    clock: u64,
    writer: Participant,
    reader: Participant,
    read_source: ReadSource,
};
pub const Writers = struct { first: Participant, second: Participant };
pub const PinCommand = struct { provenance: access_types.Provenance, value: u32, kind: enum { configure, write_x, write_y, acknowledge } };
pub const PinCollision = struct { pin: u6, count: usize, commands: [8]PinCommand };
pub const PinOverwrite = struct { pin: u6, value: u32, discarded: u32, previous: [8]?access_types.Provenance, current: [8]?access_types.Provenance };
pub const PinProblem = struct { pin: u6, value: u32 };
pub const StreamProblem = struct { control: u32, value: u32 };
pub const PadProblem = struct { pin: u6, minimum: f64, maximum: f64, limit: f64 };
pub const WorkConflict = struct { previous: access_types.Provenance, replacement: access_types.Provenance, target: u3 };
pub const MemoryRead = struct { address: u20 };
pub const MemoryWrite = struct { address: u20, value: u8 };

pub const Kind = union(enum) {
    /// Executed instruction has no valid decoded operation.
    invalid_encoding: void,
    /// A FIFO instruction or FIFO-based streamer command is attempted during hub execution.
    hub_fifo_conflict: void,
    /// A software FIFO read is attempted without a configured read FIFO.
    fifo_read_unconfigured: void,
    /// A software FIFO write is attempted without a configured write FIFO.
    fifo_write_unconfigured: void,
    /// A new streamer command requires a FIFO that is absent or has the wrong direction.
    streamer_fifo_mismatch: void,
    /// XBYTE is attempted without a configured read FIFO.
    xbyte_fifo_unconfigured: void,
    /// SKIPF is attempted in hub execution mode.
    hub_skipf: void,
    /// A streamer command selects a reserved or uncharacterized format; existing work is preserved.
    streamer_mode: struct { reason: ModeReason, control: u32, data: u32, frequency: ?u32, start: enum { immediate, zero, continuous } },
    /// A fetch uses an address outside its selected cog/LUT or hub execution domain.
    pc_domain: struct { pc: u20, hub: bool, address: u32, storage: access_types.Storage },
    /// An executed instruction was fetched during a write to the same cog RAM long; discarded fetches are silent.
    cog_fetch_write: Conflict,
    /// A companion LUT write takes the streamer read port, causing reuse of its retained sample.
    lut_read_write: Conflict,
    /// Different captured issuers commit writes to the same cog register or RAM long on one edge.
    reg_write_conflict: Writers,
    /// Different captured issuers commit writes to the same receiving LUT long on one edge.
    lut_write_conflict: Writers,
    /// Multiple cog commands reach one pin on the same delivery edge, including identical commands.
    pin_command_collision: PinCollision,
    /// A pending UART, USART or USB transmit word is replaced before consumption.
    pin_buffer_overwrite: PinOverwrite,
    /// USB receive error changes from clear to set during a packet or its completion edge.
    usb_receive_error: PinProblem,
    /// A DAC dither Y write exceeds $FF00 in its low 16 bits and saturates.
    smart_dac_clipped: PinProblem,
    /// An active streamer loses its required FIFO engine, direction or ownership.
    streamer_fifo_lost: StreamProblem,
    /// A FIFO-sourced streamer needs a new word but the read FIFO is empty; substitute zero.
    streamer_starvation: StreamProblem,
    /// An automatic capture sample cannot fit in the write FIFO and is discarded.
    streamer_capture_dropped: StreamProblem,
    /// A software FIFO write takes the automatic capture write opportunity on the same edge.
    streamer_capture_displaced: Writers,
    /// A pending FIFO configuration is replaced before the preceding write stream drains.
    fifo_configuration_lost: WorkConflict,
    /// A pending cog start is replaced before its scheduled delivery.
    cog_start_replaced: WorkConflict,
    /// A read touches the unmapped hub hole; report its first unreadable byte once per access.
    memory_hole_read: MemoryRead,
    /// A byte write targets the unmapped hub hole and is discarded.
    memory_hole_write: MemoryWrite,
    /// A read touches debug-protected lower RAM; report its first blocked byte once per access.
    memory_protected_read: MemoryRead,
    /// An application byte write targets debug-protected RAM and is discarded.
    memory_protected_write: MemoryWrite,
    /// A resolved pad voltage falls below the configured lower electrical limit.
    pad_undervoltage: PadProblem,
    /// A resolved pad voltage exceeds the configured upper electrical limit.
    pad_overvoltage: PadProblem,
    /// A resolved pad current exceeds the configured absolute electrical limit.
    pad_overcurrent: PadProblem,
    /// HUBSET selects an unsupported command family or clock configuration; preserve prior state.
    unsupported_hubset: u32,
    /// The configured PLL behavior lacks a hardware characterization and uses the model fallback.
    uncharacterized_pll: u32,
    /// An active clock transition changes a PLL postdivider without the documented disable sequence.
    unsafe_postdivider: u32,
};

clock: u64,
provenance: access_types.Provenance,
kind: Kind,

/// Rejected execution and protected-memory accesses are traps; other modeled hazards are warnings.
pub fn severity(issue: Issue) Severity {
    return switch (issue.kind) {
        .invalid_encoding,
        .hub_fifo_conflict,
        .fifo_read_unconfigured,
        .fifo_write_unconfigured,
        .streamer_fifo_mismatch,
        .xbyte_fifo_unconfigured,
        .hub_skipf,
        .streamer_mode,
        .streamer_fifo_lost,
        .memory_protected_read,
        .memory_protected_write,
        => .trap,
        else => .warning,
    };
}

/// Print every typed payload, including both sides of a memory conflict.
pub fn format(issue: Issue, writer: *std.Io.Writer) std.Io.Writer.Error!void {
    try writer.print("clock {d} {t} {t} issuer {any}: ", .{
        issue.clock, issue.severity(), std.meta.activeTag(issue.kind), issue.provenance,
    });
    switch (issue.kind) {
        .pin_command_collision => |collision| try writer.print("pin {d}, commands {any}", .{
            collision.pin, collision.commands[0..collision.count],
        }),
        inline else => |payload| try writer.print("{any}", .{payload}),
    }
}

/// Producer bounds: 128 per cog, eight non-electrical reports per pin, three pad limits per pin,
/// and three HUBSET reports per cog. Analog reports aggregate extrema rather than substeps.
pub const Collection = struct {
    pub const capacity = 8 * (128 + 3) + 64 * (8 + 3);
    entries: [capacity]Issue = undefined,
    len: usize = 0,
    analog_epoch: u64 = 0,
    analog_slots: [3][64]struct { epoch: u64 = std.math.maxInt(u64), index: usize = 0 } = @splat(@splat(.{})),

    /// Expose only initialized reports in detection order.
    pub fn items(collection: *const Collection) []const Issue {
        return collection.entries[0..collection.len];
    }

    /// Begin a new edge without allocating or clearing unused storage.
    pub fn clear(collection: *Collection) void {
        collection.len = 0;
        collection.analog_epoch += 1;
        if (collection.analog_epoch == std.math.maxInt(u64)) {
            collection.analog_epoch = 0;
            collection.analog_slots = @splat(@splat(.{}));
        }
    }

    /// Enforce the producer bound in every build; exceeding it is a simulator programming error.
    pub fn append(collection: *Collection, issue: Issue) void {
        if (collection.len == capacity)
            @panic("simulation issue emission bound exceeded");
        collection.entries[collection.len] = issue;
        collection.len += 1;
    }

    /// Coalesce repeated analog violations on one pin while retaining their complete extrema.
    pub fn analog(collection: *Collection, issue: Issue) void {
        const incoming = switch (issue.kind) {
            .pad_undervoltage, .pad_overvoltage, .pad_overcurrent => |value| value,
            else => unreachable,
        };
        const category: usize = switch (issue.kind) {
            .pad_undervoltage => 0,
            .pad_overvoltage => 1,
            .pad_overcurrent => 2,
            else => unreachable,
        };
        const slot = &collection.analog_slots[category][incoming.pin];
        if (slot.epoch == collection.analog_epoch) {
            switch (collection.entries[slot.index].kind) {
                .pad_undervoltage, .pad_overvoltage, .pad_overcurrent => |*value| {
                    value.minimum = @min(value.minimum, incoming.minimum);
                    value.maximum = @max(value.maximum, incoming.maximum);
                },
                else => unreachable,
            }
            return;
        }
        slot.* = .{ .epoch = collection.analog_epoch, .index = collection.len };
        collection.append(issue);
    }
};
