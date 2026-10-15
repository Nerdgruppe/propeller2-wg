//! Pure inspector views. Never execute read helpers that signal hardware events.
const std = @import("std");
const Cog = @import("Cog.zig");
const Debugger = @import("Debugger.zig");
const p2 = @import("p2");

pub const Address = struct { domain: Debugger.Domain, address: u20 };
pub const Badges = struct {
    special: bool = false,
    debug_overlay: bool = false,
    rom: bool = false,
    fetch: bool = false,
    pipeline: u5 = 0,
    vectors: u4 = 0,
    returns: u4 = 0,
    stack: u8 = 0,

    /// Report whether a row needs highlighting, retaining all simultaneous reference badges.
    pub fn any(badges: Badges) bool {
        return badges.special or badges.debug_overlay or badges.rom or badges.fetch or
            badges.pipeline != 0 or badges.vectors != 0 or badges.returns != 0 or badges.stack != 0;
    }

    /// Format every matching slot; sticky stack entries do not imply a known call depth.
    pub fn write(badges: Badges, writer: *std.Io.Writer) !void {
        if (badges.special) {
            try writer.writeAll("[special] ");
        }
        if (badges.debug_overlay) {
            try writer.writeAll("[debug overlay] ");
        }
        if (badges.rom) {
            try writer.writeAll("[execute ROM] ");
        }
        if (badges.fetch) {
            try writer.writeAll("[fetch PC] ");
        }
        for (0..5) |i| {
            if (badges.pipeline & (@as(u5, 1) << @intCast(i)) != 0) {
                try writer.print("[pipe {}] ", .{i});
            }
        }
        for (0..4) |i| {
            if (badges.vectors & (@as(u4, 1) << @intCast(i)) != 0) {
                try writer.print("[IJMP{}] ", .{i});
            }
            if (badges.returns & (@as(u4, 1) << @intCast(i)) != 0) {
                try writer.print("[IRET{}] ", .{i});
            }
        }
        for (0..8) |i| {
            if (badges.stack & (@as(u8, 1) << @intCast(i)) != 0) {
                try writer.print("[stack {}] ", .{i});
            }
        }
    }
};

/// Decode packed flags and infer the architectural branch target's local-long/hub-byte domain.
pub fn target(value: u32) Address {
    const saved: p2.types.ReturnAddress = @bitCast(value);
    return .{
        .domain = if (saved.address < 0x200) .cog else if (saved.address < 0x400) .lut else .hub,
        .address = if (saved.address >= 0x200 and saved.address < 0x400)
            saved.address - 0x200
        else
            saved.address,
    };
}

/// Read ordinary architectural values without consuming input samples or generating events.
pub fn register(cog: *const Cog, index: u9) u32 {
    if (cog.debug.state == .active and cog.debug.control & (1 << 11) == 0) {
        if (index == 0x1fe)
            return cog.debug.ijmp;
        if (index == 0x1ff)
            return cog.debug.iret;
    }
    return switch (index) {
        0x1fe => @truncate(cog.hub.io.get_in()),
        0x1ff => @truncate(cog.hub.io.get_in() >> 32),
        else => cog.registers.values[index],
    };
}

/// Identify all references using physical local fetch addresses and packed branch targets.
pub fn annotations(cog: *const Cog, address: Address) Badges {
    var badges: Badges = .{};
    if (address.domain == .cog) {
        badges.special = address.address >= 0x1f0;
        badges.rom = address.address >= 0x1f8;
        badges.debug_overlay = address.address >= 0x1fe and cog.debug.state == .active and
            cog.debug.control & (1 << 11) == 0;
    }
    const fetch: Address = if (cog.exec_mode == .hub)
        .{ .domain = .hub, .address = cog.pc }
    else
        .{
            .domain = if (cog.local_fetch_address() < 0x200) .cog else .lut,
            .address = cog.local_fetch_address() & 0x1ff,
        };
    badges.fetch = cog.exec_mode != .stopped and std.meta.eql(fetch, address);
    for (cog.pipeline, 0..) |slot, i| {
        if (slot) |state| {
            if (state.interrupt != null)
                continue;
            const source: Address = if (state.local_address) |local|
                .{ .domain = if (local < 0x200) .cog else .lut, .address = local & 0x1ff }
            else
                .{ .domain = if (state.interrupt != null) .cog else .hub, .address = state.pc };
            if (std.meta.eql(source, address)) {
                badges.pipeline |= @as(u5, 1) << @intCast(i);
            }
        }
    }
    const vectors = [_]u32{ cog.debug.ijmp, cog.registers.get(.IJMP1), cog.registers.get(.IJMP2), cog.registers.get(.IJMP3) };
    const returns = [_]u32{ cog.debug.iret, cog.registers.get(.IRET1), cog.registers.get(.IRET2), cog.registers.get(.IRET3) };
    for (vectors, returns, 0..) |vector, ret, i| {
        if (std.meta.eql(target(vector), address)) {
            badges.vectors |= @as(u4, 1) << @intCast(i);
        }
        if (std.meta.eql(target(ret), address)) {
            badges.returns |= @as(u4, 1) << @intCast(i);
        }
    }
    for (cog.stack, 0..) |entry, i| {
        if (std.meta.eql(target(entry), address)) {
            badges.stack |= @as(u8, 1) << @intCast(i);
        }
    }
    return badges;
}
