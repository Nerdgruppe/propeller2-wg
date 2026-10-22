//! Strict edge checks and verbose diagnostics for simulator regression tests only.
const std = @import("std");
const Hub = @import("sim/Hub.zig");
const IO = @import("sim/IO.zig");
const Cog = @import("sim/Cog.zig");
const reads = @import("sim/read.zig");

/// Stage an already-returned physical FIFO word using the same live ownership as production.
pub fn bufferFifoWord(cog: *Cog, sample: reads.Sample, next: u20, value: u32) Cog.FifoSlot {
    const fifo = &cog.fifo_state.engine.?;
    const read = &fifo.direction.read;
    if (read.pending) |previous| {
        read.get(previous).window.close(sample);
    }
    const slot = read.allocate(sample, next, value);
    read.pending = slot;
    read.order[(fifo.head + fifo.count) % read.order.len] = slot;
    fifo.count += 1;
    return slot;
}

/// Apply the tests' strict issue and endpoint policy after a serial-host edge.
pub const Strict = struct {
    /// Reject every issue or failed attached service; application policies belong to their callers.
    pub fn afterStep(_: Strict, hub: *const Hub) !void {
        if (needsAttention(hub))
            return error.BootIssue;
    }
};

/// Return whether a completed edge needs host attention.
pub fn needsAttention(hub: *const Hub) bool {
    return hub.issues.len != 0 or serviceFailed(hub);
}

/// Print all reports and service errors from the completed edge.
pub fn diagnostics(hub: *const Hub, writer: *std.Io.Writer) std.Io.Writer.Error!void {
    for (0..hub.issues.len) |issue_index| {
        const issue = hub.issues.at(issue_index);
        try writer.print("{f}\n", .{issue});
    }
    try ioDiagnostics(&hub.io, writer);
}

/// Inspect endpoint failures without changing or retrying their I/O state.
pub fn serviceFailed(hub: *const Hub) bool {
    for (&hub.io.pins) |*pin| {
        const source = switch (pin.smart.rx_mode) {
            .exact => null,
            .emulated, .virtual => |value| value,
        };
        if (source) |value| {
            if (value.failure != null)
                return true;
        }
        const sink = switch (pin.smart.tx_mode) {
            .emulated, .virtual => |value| value,
        };
        if (sink.failure != null)
            return true;
    }
    return false;
}

/// Print every failed pin endpoint and its retained command contributors.
fn ioDiagnostics(io: *const IO, writer: *std.Io.Writer) std.Io.Writer.Error!void {
    for (&io.pins, 0..) |*pin, index| {
        const source = switch (pin.smart.rx_mode) {
            .exact => null,
            .emulated, .virtual => |value| value,
        };
        if (source) |value| {
            if (value.failure) |err| {
                try writer.print("pin {d} source service: {t}, contributors {any}\n", .{ index, err, io.owners[index] });
            }
        }
        const sink = switch (pin.smart.tx_mode) {
            .emulated, .virtual => |value| value,
        };
        if (sink.failure) |err| {
            try writer.print("pin {d} sink service: {t}, contributors {any}\n", .{ index, err, io.owners[index] });
        }
    }
}
