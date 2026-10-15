const std = @import("std");

pub fn emit(writer: *std.Io.Writer, data: []const u8) !void {
    try writer.writeAll(data);
}
