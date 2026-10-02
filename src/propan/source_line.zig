const std = @import("std");

pub fn get(source: []const u8, one_based_line: u32) ?[]const u8 {
    if (one_based_line == 0) return null;

    var start: usize = 0;
    var current_line: u32 = 1;
    while (current_line < one_based_line) : (current_line += 1) {
        const newline = std.mem.indexOfScalarPos(u8, source, start, '\n') orelse return null;
        start = newline + 1;
    }

    var end = std.mem.indexOfScalarPos(u8, source, start, '\n') orelse source.len;
    if (end > start and source[end - 1] == '\r') end -= 1;
    return source[start..end];
}
