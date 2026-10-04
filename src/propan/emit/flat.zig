const std = @import("std");

pub fn emit(io: std.Io, file: std.Io.File, data: []const u8) !void {
    try file.writeStreamingAll(io, data);
}
