const std = @import("std");

const Cog = @import("Cog.zig");
const IO = @import("IO.zig");
const Hub = @This();

cogs: [8]Cog,
counter: u64 = 0,
io: IO,

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
