const std = @import("std");
const eval = @import("stdlib/eval.zig");

pub fn from_name(name: []const u8) ?eval.ExecMode {
    if (std.ascii.eqlIgnoreCase(name, ".cogexec")) return .cog;
    if (std.ascii.eqlIgnoreCase(name, ".lutexec")) return .lut;
    if (std.ascii.eqlIgnoreCase(name, ".hubexec")) return .hub;
    if (std.ascii.eqlIgnoreCase(name, ".regspace")) return .regspace;
    if (std.ascii.eqlIgnoreCase(name, ".data")) return .data;
    return null;
}
