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
        if (base + 504 * 4 > hub.memory.len) return error.InvalidCogImage;
        for (0..504) |i| cog.registers.values[i] = std.mem.readInt(u32, hub.memory[base + i * 4 ..][0..4], .little);
        cog.exec_mode = .cog;
    } else {
        cog.pc = options.hub_address;
        cog.exec_mode = .hub;
    }
    cog.registers.set(.PTRA, options.ptra);
    cog.registers.set(.PTRB, options.hub_address);
}
