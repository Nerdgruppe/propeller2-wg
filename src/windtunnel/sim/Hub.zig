const std = @import("std");

const Cog = @import("Cog.zig");
const IO = @import("IO.zig");
const Hub = @This();

pub const DebugFifo = std.fifo.LinearFifo(u32, .{ .Static = 512 });

memory: [512 * 1024]u8 = @splat(0),
cogs: [8]Cog,
counter: u64 = 0,
io: IO,

debug_stream: ?*DebugFifo = null,

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

    hub.io.step();

    hub.counter +%= 1;
}

pub fn is_any_cog_active(hub: *Hub) bool {
    for (hub.cogs) |cog| {
        if (cog.exec_mode != .stopped)
            return true;
    }
    return false;
}

pub fn start_cog(hub: *Hub, index: u3, options: struct {}) !void {
    _ = options;

    const cog = &hub.cogs[index];

    cog.reset();

    @memcpy(std.mem.sliceAsBytes(&cog.registers.values), hub.memory[0 .. 512 * 4]);

    cog.exec_mode = .cog;
}
