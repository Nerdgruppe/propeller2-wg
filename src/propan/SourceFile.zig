const std = @import("std");

/// A source buffer with a stable path for AST locations and diagnostics.
pub const SourceFile = @This();

path: []const u8,
identity: []const u8,
text: []const u8,
line_starts: []const u32,
dir_index: usize = 0,
relative_path: []const u8 = "",

/// Borrows path and text; owns the line index allocated with allocator.
pub fn init(allocator: std.mem.Allocator, path: []const u8, text: []const u8) !SourceFile {
    if (text.len > std.math.maxInt(u32)) return error.SourceTooLarge;

    var line_starts: std.ArrayListUnmanaged(u32) = .empty;
    errdefer line_starts.deinit(allocator);
    try line_starts.append(allocator, 0);
    for (text, 0..) |byte, offset| {
        if (byte == '\n') try line_starts.append(allocator, @intCast(offset + 1));
    }
    return .{
        .path = path,
        .identity = path,
        .text = text,
        .line_starts = try line_starts.toOwnedSlice(allocator),
    };
}

pub fn deinit(self: *SourceFile, allocator: std.mem.Allocator) void {
    allocator.free(self.line_starts);
    self.* = undefined;
}

pub fn line(self: SourceFile, one_based_line: u32) ?[]const u8 {
    if (one_based_line == 0 or one_based_line > self.line_starts.len) return null;
    const start = self.line_starts[one_based_line - 1];
    var end = if (one_based_line < self.line_starts.len) self.line_starts[one_based_line] - 1 else self.text.len;
    if (end > start and self.text[end - 1] == '\r') end -= 1;
    return self.text[start..end];
}

test "line index includes empty lines and strips CRLF from excerpts" {
    var source: SourceFile = try .init(std.testing.allocator, "lines.propan", "first\r\n\nlast\n");
    defer source.deinit(std.testing.allocator);
    try std.testing.expectEqualSlices(u32, &.{ 0, 7, 8, 13 }, source.line_starts);
    try std.testing.expectEqualStrings("first", source.line(1).?);
    try std.testing.expectEqualStrings("", source.line(2).?);
    try std.testing.expectEqualStrings("last", source.line(3).?);
    try std.testing.expectEqualStrings("", source.line(4).?);
    try std.testing.expectEqual(null, source.line(0));
    try std.testing.expectEqual(null, source.line(5));

    var empty: SourceFile = try .init(std.testing.allocator, "empty.propan", "");
    defer empty.deinit(std.testing.allocator);
    try std.testing.expectEqualSlices(u32, &.{0}, empty.line_starts);
    try std.testing.expectEqualStrings("", empty.line(1).?);

    var unterminated: SourceFile = try .init(std.testing.allocator, "unterminated.propan", "last");
    defer unterminated.deinit(std.testing.allocator);
    try std.testing.expectEqualStrings("last", unterminated.line(1).?);
}
