const std = @import("std");

pub fn main(init: std.process.Init) !void {
    var args = try init.minimal.args.iterateAllocator(init.gpa);
    defer args.deinit();
    _ = args.next() orelse return error.MissingExecutableName;
    const expected_path = args.next() orelse return error.MissingExpectedPath;
    const actual_path = args.next() orelse return error.MissingActualPath;
    if (args.next() != null) return error.TooManyArguments;

    const cwd = std.Io.Dir.cwd();
    const expected = try cwd.readFileAlloc(init.io, expected_path, init.gpa, .limited(1 << 20));
    defer init.gpa.free(expected);
    const actual = try cwd.readFileAlloc(init.io, actual_path, init.gpa, .limited(1 << 20));
    defer init.gpa.free(actual);

    try std.testing.expectEqualSlices(u8, expected, actual);
}
