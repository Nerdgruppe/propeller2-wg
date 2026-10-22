//! Eight interleaved single-port 16K x 32 RAM slices with explicit byte lanes.
const std = @import("std");
const access_types = @import("access.zig");
const HubRam = @This();

pub const byte_count = 512 * 1024;
pub const row_count = 16384;
pub const slice_count = 8;
pub const LaneMask = std.StaticBitSet(4);
pub const Long = [4]u8;

/// The upper address bit selects mapping; it is not a physical RAM row bit.
pub const Address = packed struct(u20) {
    lane: u2,
    slice: u3,
    row: u14,
    upper: bool,

    /// Wrap a hub address to its twenty decoded address lines.
    pub fn init(raw: u32) Address {
        return @bitCast(@as(u20, @truncate(raw)));
    }

    /// Return the address driven on the hub bus.
    pub fn value(address: Address) u20 {
        return @bitCast(address);
    }

    /// Select the physical long, retaining the logical alias for diagnostics.
    pub fn aligned(address: Address) Address {
        var result = address;
        result.lane = 0;
        return result;
    }

    /// Return a canonical RAM byte address, excluding addresses in the hole.
    pub fn backing(address: Address) ?u19 {
        if (address.upper and address.row < debug_first_row)
            return null;
        return @truncate(address.value());
    }
};

pub const debug_first_row = 0x3e00;
pub const Resolution = union(enum) {
    ram: Address,
    hole,
    protected,
};

rows: [row_count][slice_count]Long = @splat(@splat(@splat(0))),

comptime {
    std.debug.assert(@sizeOf(@TypeOf(@as(HubRam, undefined).rows)) == byte_count);
}

/// Expose address-order storage for host loading and edits, bypassing simulated accesses.
pub fn bytes(ram: *HubRam) *[byte_count]u8 {
    return @ptrCast(&ram.rows);
}

/// Inspect address-order storage without issuing reads or changing simulator state.
pub fn bytesConst(ram: *const HubRam) *const [byte_count]u8 {
    return @ptrCast(&ram.rows);
}

/// Decode one physical long's map and authorization before selecting its lanes.
pub fn resolve(address: Address, protected: bool, access: access_types.Access, debug_write: bool) Resolution {
    if (address.backing() == null)
        return .hole;
    if (protected and address.row >= debug_first_row and
        (!address.upper or (access == .write and !debug_write)))
        return .protected;
    return .{ .ram = address };
}

/// Read all four lanes of an already authorized physical long.
pub fn read(ram: *const HubRam, address: Address) Long {
    return ram.rows[address.row][address.slice];
}

/// Drive selected byte write enables without modifying neighboring lanes.
pub fn write(ram: *HubRam, address: Address, value: Long, lanes: LaneMask) void {
    for (0..4) |lane| {
        if (lanes.isSet(lane)) {
            ram.rows[address.row][address.slice][lane] = value[lane];
        }
    }
}

test "row slice and lane layout is identical to address-order bytes" {
    const ram = try std.testing.allocator.create(HubRam);
    defer std.testing.allocator.destroy(ram);

    ram.* = .{};
    for (0..row_count) |row| {
        for (0..slice_count) |slice| {
            for (0..4) |lane| {
                const offset = row * 32 + slice * 4 + lane;
                const address = Address.init(@intCast(offset));
                try std.testing.expectEqual(row, address.row);
                try std.testing.expectEqual(slice, address.slice);
                try std.testing.expectEqual(lane, address.lane);
                ram.rows[row][slice][lane] = @truncate(offset ^ (offset >> 8));
                try std.testing.expectEqual(ram.rows[row][slice][lane], ram.bytesConst()[offset]);
            }
        }
    }
    ram.bytes()[37] = 0x12;
    try std.testing.expectEqual(@as(u8, 0x12), ram.rows[1][1][1]);
    try std.testing.expectEqual(Address.init(0x7ffff).backing(), Address.init(0xfffff).backing());
    try std.testing.expect(Address.init(0x80000).backing() == null);
    try std.testing.expectEqual(@as(u20, 0), Address.init(0x100000).value());
}
