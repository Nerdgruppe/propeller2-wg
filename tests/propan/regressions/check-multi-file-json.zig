const std = @import("std");

const Output = struct {
    total_size: usize,
    segments: []const Segment,
    symbols: []const Symbol,
    line_map: []const Line,

    const Segment = struct {
        id: u32,
        data: []const u8,
    };

    const Symbol = struct {
        name: []const u8,
        segment_id: u32,
    };

    const Line = struct {
        file: []const u8,
    };
};

pub fn main(init: std.process.Init) !void {
    var args = try init.minimal.args.iterateAllocator(init.gpa);
    defer args.deinit();
    _ = args.next() orelse return error.MissingExecutableName;
    const path = args.next() orelse return error.MissingInputPath;
    if (args.next() != null) return error.TooManyArguments;

    const source = try std.Io.Dir.cwd().readFileAlloc(init.io, path, init.gpa, .limited(1 << 20));
    defer init.gpa.free(source);

    const parsed = try std.json.parseFromSlice(Output, init.gpa, source, .{ .ignore_unknown_fields = true });
    defer parsed.deinit();
    const output = parsed.value;

    try std.testing.expectEqual(@as(usize, 2), output.total_size);
    try std.testing.expectEqual(@as(usize, 2), output.segments.len);
    try expectDecodedData(output.segments[0].data, &.{ 1, 2 });
    try expectDecodedData(output.segments[1].data, &.{3});
    try std.testing.expect(output.segments[0].id != output.segments[1].id);

    try std.testing.expectEqual(@as(usize, 2), output.symbols.len);
    var found_first = false;
    var found_second = false;
    for (output.symbols) |symbol| {
        if (std.mem.eql(u8, symbol.name, "first")) {
            try std.testing.expectEqual(output.segments[0].id, symbol.segment_id);
            found_first = true;
        } else if (std.mem.eql(u8, symbol.name, "second")) {
            try std.testing.expectEqual(output.segments[1].id, symbol.segment_id);
            found_second = true;
        } else {
            return error.UnexpectedSymbol;
        }
    }
    try std.testing.expect(found_first and found_second);

    var found_first_file = false;
    var found_second_file = false;
    for (output.line_map) |line| {
        const filename = std.fs.path.basename(line.file);
        if (std.mem.eql(u8, filename, "multi-file-first.propan")) {
            found_first_file = true;
        } else if (std.mem.eql(u8, filename, "multi-file-second.propan")) {
            found_second_file = true;
        } else {
            return error.UnexpectedSourceFile;
        }
    }
    try std.testing.expect(found_first_file and found_second_file);
}

fn expectDecodedData(encoded: []const u8, expected: []const u8) !void {
    const decoder = std.base64.standard.Decoder;
    const decoded_len = try decoder.calcSizeForSlice(encoded);
    var buffer: [8]u8 = undefined;
    try std.testing.expect(decoded_len <= buffer.len);
    try decoder.decode(buffer[0..decoded_len], encoded);
    try std.testing.expectEqualSlices(u8, expected, buffer[0..decoded_len]);
}
