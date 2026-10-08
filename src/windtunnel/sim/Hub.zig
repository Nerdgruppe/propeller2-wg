const std = @import("std");

const Cog = @import("Cog.zig");
const IO = @import("IO.zig");
const Hub = @This();

memory: [512 * 1024]u8 = @splat(0),
cogs: [8]Cog,
counter: u64 = 0,
io: IO,

output_writer: ?*std.Io.Writer = null,
fault: ?Cog.Fault = null,
locks: [16]Lock = undefined,

pub const Lock = struct { allocated: bool = false, taken: bool = false, owner: u3 = 0, id: u4 };

pub fn init(hub: *Hub) void {
    hub.* = .{
        .cogs = .{
            .init(hub, 0),
            .init(hub, 1),
            .init(hub, 2),
            .init(hub, 3),
            .init(hub, 4),
            .init(hub, 5),
            .init(hub, 6),
            .init(hub, 7),
        },
        .io = .{},
    };
    for (&hub.locks, 0..) |*lock, i| lock.* = .{ .id = @intCast(i) };
}

pub fn release_lock(hub: *Hub, lock: *Lock) void {
    lock.taken = false;
    hub.signal_lock(lock.id, false);
}

pub fn signal_lock(hub: *Hub, id: u4, taken: bool) void {
    for (&hub.cogs) |*cog| {
        cog.signal_selectable((if (taken) @as(u6, 0x10) else 0x20) | id);
        cog.signal_selectable(0x30 | @as(u6, id));
    }
}

pub fn read_memory(hub: *Hub, address: u32, size: u3) u32 {
    var value: u32 = 0;
    for (0..size) |i| value |= @as(u32, hub.memory[(address +% @as(u32, @intCast(i))) & 0x7ffff]) << @intCast(i * 8);
    return value;
}

pub fn write_memory(hub: *Hub, address: u32, value: u32, size: u3, masked: bool) void {
    for (0..size) |i| {
        const byte: u8 = @truncate(value >> @intCast(i * 8));
        if (!masked or byte != 0) hub.memory[(address +% @as(u32, @intCast(i))) & 0x7ffff] = byte;
    }
}

pub fn step(hub: *Hub) void {
    for (&hub.cogs) |*cog| {
        cog.step();
    }

    hub.io.step(hub);

    hub.counter +%= 1;
}

pub fn is_any_cog_active(hub: *Hub) bool {
    for (hub.cogs) |cog| {
        if (cog.exec_mode != .stopped)
            return true;
    }
    return false;
}

pub fn start_cog(hub: *Hub, index: u3, options: struct { hub_address: u20 = 0, ptra: u32 = 0, load_image: bool = true }) !void {
    const cog = &hub.cogs[index];
    cog.reset();
    const base = options.hub_address & 0x7fffc;
    if (options.load_image) {
        for (0..504) |i| cog.registers.values[i] = hub.read_memory(base + @as(u32, @intCast(i * 4)), 4);
        cog.exec_mode = .cog;
    } else {
        cog.pc = options.hub_address;
        cog.exec_mode = if (options.hub_address < 0x400) .cog else .hub;
    }
    cog.registers.set(.PTRA, options.ptra);
    cog.registers.set(.PTRB, options.hub_address);
}
