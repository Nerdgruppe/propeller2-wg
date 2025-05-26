const Cog = @import("Cog.zig");
const Hub = @import("Hub.zig");
const IO = @This();

pub fn step(io: *IO) void {
    _ = io;
}

pub fn get_in(io: *IO) u64 {
    _ = io;
    @panic("TODO: Implement IO");
}

pub fn update_out(io: *IO) void {
    _ = io;
    @panic("TODO: Implement IO");
}
