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

test "AST renderer preserves binary grouping without extra parentheses" {
    const source =
        \\const PRECEDENCE = (1 + 2) * 3
        \\const ASSOCIATIVITY = 20 - (5 - 2)
        \\const PREFIX = -(1 + 2)
        \\const CHAIN = 1 + 2 + 3
        \\const WRAPPED = (1 + 2)
        \\const NATURAL = 1 + 2 * 3
        \\BYTE PRECEDENCE, ASSOCIATIVITY, CHAIN, WRAPPED, NATURAL
        \\.assert PREFIX == -3
    ;
    var collection: diagnostics.Collection = .init(std.testing.allocator);
    defer collection.deinit();
    var parser: frontend.Parser = .init(source, "grouping.propan", &collection);
    var parsed = try parser.parse(std.testing.allocator);
    defer parsed.deinit();

    // Model AST transformations that have already removed source parentheses.
    const lhs = parsed.file.sequence[0].constant.value.binary_transform.lhs;
    lhs.* = lhs.wrapped.*;
    const rhs = parsed.file.sequence[1].constant.value.binary_transform.rhs;
    rhs.* = rhs.wrapped.*;
    const unary_value = parsed.file.sequence[2].constant.value.unary_transform.value;
    unary_value.* = unary_value.wrapped.*;

    var rendered: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer rendered.deinit();
    try frontend.render.pretty_print(&rendered.writer, parsed.file);
    try std.testing.expectEqualStrings(
        \\const PRECEDENCE = (1 + 2) * 3
        \\const ASSOCIATIVITY = 20 - (5 - 2)
        \\const PREFIX = -(1 + 2)
        \\const CHAIN = 1 + 2 + 3
        \\const WRAPPED = (1 + 2)
        \\const NATURAL = 1 + 2 * 3
        \\    BYTE PRECEDENCE, ASSOCIATIVITY, CHAIN, WRAPPED, NATURAL
        \\    .assert PREFIX == -3
        \\
    ,
        rendered.written(),
    );

    var reparsing: frontend.Parser = .init(rendered.written(), "grouping-roundtrip.propan", &collection);
    var reparsed = try reparsing.parse(std.testing.allocator);
    defer reparsed.deinit();
    var original = try sema.analyze(std.testing.allocator, parsed.file, .{}, &collection);
    defer original.deinit();
    var roundtrip = try sema.analyze(std.testing.allocator, reparsed.file, .{}, &collection);
    defer roundtrip.deinit();
    try std.testing.expectEqual(@as(usize, 1), original.segments.len);
    try std.testing.expectEqual(@as(usize, 1), roundtrip.segments.len);
    try std.testing.expectEqualSlices(u8, &.{ 9, 17, 6, 3, 7 }, original.segments[0].data);
    try std.testing.expectEqualSlices(u8, original.segments[0].data, roundtrip.segments[0].data);
}
