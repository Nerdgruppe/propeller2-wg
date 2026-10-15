//! Nominal serial-ROM handoff for loaded images; raw Hub.init still models hardware reset.
const std = @import("std");
const Hub = @import("sim/Hub.zig");

pub const baudrate: u32 = 2_000_000;

/// Retain the ROM's calibrated 8N1 RX/TX dividers at nominal 20 MHz RCFAST.
/// Hardware calibrates these against the upload stream, then clears modes and DIR only.
pub fn prepare(hub: *Hub) void {
    const configured = hub.io.setClock(0xf0);
    std.debug.assert(configured);
    const divider = ((20_000_000 / baudrate) << 16) | 7;
    hub.io.pins[62].smart.registers.x = divider;
    hub.io.pins[63].smart.registers.x = divider;
}
