//! Strict edge checks and verbose diagnostics for simulator regression tests only.
const std = @import("std");
const Hub = @import("sim/Hub.zig");
const IO = @import("sim/IO.zig");

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

/// Inspect sticky endpoint and export failures without performing I/O.
pub fn serviceFailed(hub: *const Hub) bool {
    if (hub.vcd) |vcd| {
        if (vcd.failure != null)
            return true;
    }
    return ioFailed(&hub.io);
}

/// Print all reports and service errors from the completed edge.
pub fn diagnostics(hub: *const Hub, writer: *std.Io.Writer) std.Io.Writer.Error!void {
    for (hub.issues.items()) |issue| {
        try writer.print("{f}\n", .{issue});
    }
    if (hub.vcd) |vcd| {
        if (vcd.failure) |err| {
            try writer.print("VCD service: {t}\n", .{err});
        }
    }
    try ioDiagnostics(&hub.io, writer);
}

/// Inspect endpoint failures without changing or retrying their I/O state.
fn ioFailed(io: *const IO) bool {
    for (&io.pins) |*pin| {
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
