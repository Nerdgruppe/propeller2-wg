const std = @import("std");
const frontend = @import("../frontend.zig");
const sema = @import("../sema.zig");
const diagnostics = @import("../diagnostics.zig");

test "frontend source and AST renderers cover expression variants" {
    const source = for (@import("fuzz-corpus").files) |candidate| {
        if (std.mem.startsWith(u8, candidate, "// FRONTEND ROUNDTRIP")) break candidate;
    } else return error.MissingRendererFixture;
    var collection: diagnostics.Collection = .init(std.testing.allocator);
    defer collection.deinit();
    var parser: frontend.Parser = .init(source, "render.propan", &collection);
    var parsed = try parser.parse(std.testing.allocator);
    defer parsed.deinit();

    var rendered: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer rendered.deinit();
    try frontend.render.pretty_print(&rendered.writer, parsed.file);
    var reparsing: frontend.Parser = .init(rendered.written(), "roundtrip.propan", &collection);
    var reparsed = try reparsing.parse(std.testing.allocator);
    defer reparsed.deinit();
    var original = try sema.analyze(std.testing.allocator, parsed.file, .{}, &collection);
    defer original.deinit();
    var roundtrip = try sema.analyze(std.testing.allocator, reparsed.file, .{}, &collection);
    defer roundtrip.deinit();
    try std.testing.expectEqual(original.segments.len, roundtrip.segments.len);
    for (original.segments, roundtrip.segments) |left, right| {
        try std.testing.expectEqualSlices(u8, left.data, right.data);
    }

    var dumped: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer dumped.deinit();
    try frontend.dump_ast(&dumped.writer, parsed.file);
    try std.testing.expect(std.mem.indexOf(u8, dumped.written(), "enumerator: '#on'") != null);
    try std.testing.expect(std.mem.indexOf(u8, dumped.written(), "string:") != null);
}
