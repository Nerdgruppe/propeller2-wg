const std = @import("std");
const frontend = @import("../frontend.zig");
const sema = @import("../sema.zig");
const diagnostics = @import("../diagnostics.zig");
const SourceFile = @import("../SourceFile.zig");

test "frontend source and AST renderers cover expression variants" {
    const source = for (@import("fuzz-corpus").files) |candidate| {
        if (std.mem.startsWith(u8, candidate, "// FRONTEND ROUNDTRIP")) break candidate;
    } else return error.MissingRendererFixture;
    var collection: diagnostics.Collection = .init(std.testing.allocator);
    defer collection.deinit();
    var parser_source: SourceFile = try .init(std.testing.allocator, "render.propan", source);
    defer parser_source.deinit(std.testing.allocator);
    var parser: frontend.Parser = .init(&parser_source, &collection);
    var parsed = try parser.parse(std.testing.allocator);
    defer parsed.deinit();

    var rendered: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer rendered.deinit();
    try frontend.render.pretty_print(&rendered.writer, parsed.file);
    var reparsing_source: SourceFile = try .init(std.testing.allocator, "roundtrip.propan", rendered.written());
    defer reparsing_source.deinit(std.testing.allocator);
    var reparsing: frontend.Parser = .init(&reparsing_source, &collection);
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
    var parser_source: SourceFile = try .init(std.testing.allocator, "grouping.propan", source);
    defer parser_source.deinit(std.testing.allocator);
    var parser: frontend.Parser = .init(&parser_source, &collection);
    var parsed = try parser.parse(std.testing.allocator);
    defer parsed.deinit();

    // Model AST transformations that have already removed source parentheses.
    const lhs = parsed.file.sequence[0].constant.value.binary_transform.lhs;
    lhs.* = lhs.wrapped.value.*;
    const rhs = parsed.file.sequence[1].constant.value.binary_transform.rhs;
    rhs.* = rhs.wrapped.value.*;
    const unary_value = parsed.file.sequence[2].constant.value.unary_transform.value;
    unary_value.* = unary_value.wrapped.value.*;

    var rendered: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer rendered.deinit();
    try frontend.render.pretty_print(&rendered.writer, parsed.file);
    try std.testing.expectEqualStrings(
        \\const PRECEDENCE    = (1 + 2) * 3
        \\const ASSOCIATIVITY = 20 - (5 - 2)
        \\const PREFIX        = -(1 + 2)
        \\const CHAIN         = 1 + 2 + 3
        \\const WRAPPED       = (1 + 2)
        \\const NATURAL       = 1 + 2 * 3
        \\                BYTE    PRECEDENCE, ASSOCIATIVITY,  CHAIN,  WRAPPED,    NATURAL
        \\.assert PREFIX == -3
        \\
        \\
    ,
        rendered.written(),
    );

    var reparsing_source: SourceFile = try .init(std.testing.allocator, "grouping-roundtrip.propan", rendered.written());
    defer reparsing_source.deinit(std.testing.allocator);
    var reparsing: frontend.Parser = .init(&reparsing_source, &collection);
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

test "pretty printer aligns blocks and preserves comments and emitted bytes" {
    const source = try std.Io.Dir.cwd().readFileAlloc(std.testing.io, "tests/propan/format/input.propan", std.testing.allocator, .limited(1 << 20));
    defer std.testing.allocator.free(source);
    const expected = try std.Io.Dir.cwd().readFileAlloc(std.testing.io, "tests/propan/format/expected.propan", std.testing.allocator, .limited(1 << 20));
    defer std.testing.allocator.free(expected);
    var collection: diagnostics.Collection = .init(std.testing.allocator);
    defer collection.deinit();

    var parser_source: SourceFile = try .init(std.testing.allocator, "input.propan", source);
    defer parser_source.deinit(std.testing.allocator);
    var parser: frontend.Parser = .init(&parser_source, &collection);
    var parsed = try parser.parse(std.testing.allocator);
    defer parsed.deinit();
    var formatted_buffer: [8192]u8 = undefined;
    var formatted: std.Io.Writer = .fixed(&formatted_buffer);
    try frontend.render.pretty_print(&formatted, parsed.file);
    try std.testing.expectEqualStrings(expected, formatted.buffered());

    var reparsing_source: SourceFile = try .init(std.testing.allocator, "expected.propan", formatted.buffered());
    defer reparsing_source.deinit(std.testing.allocator);
    var reparsing: frontend.Parser = .init(&reparsing_source, &collection);
    var reparsed = try reparsing.parse(std.testing.allocator);
    defer reparsed.deinit();
    var second_buffer: [8192]u8 = undefined;
    var second_pass: std.Io.Writer = .fixed(&second_buffer);
    try frontend.render.pretty_print(&second_pass, reparsed.file);
    try std.testing.expectEqualStrings(expected, second_pass.buffered());

    var original_module = try sema.analyze(std.testing.allocator, parsed.file, .{}, &collection);
    defer original_module.deinit();
    var formatted_module = try sema.analyze(std.testing.allocator, reparsed.file, .{}, &collection);
    defer formatted_module.deinit();
    try std.testing.expectEqual(original_module.segments.len, formatted_module.segments.len);
    for (original_module.segments, formatted_module.segments) |left, right| {
        try std.testing.expectEqual(left.hub_offset, right.hub_offset);
        try std.testing.expectEqualSlices(u8, left.data, right.data);
    }
}

test "comment in an empty call stays inside its parentheses" {
    const source = "const X = foo( // inside empty call\n)\n";
    var collection: diagnostics.Collection = .init(std.testing.allocator);
    defer collection.deinit();
    var parser_source: SourceFile = try .init(std.testing.allocator, "empty-call.propan", source);
    defer parser_source.deinit(std.testing.allocator);
    var parser: frontend.Parser = .init(&parser_source, &collection);
    var parsed = try parser.parse(std.testing.allocator);
    defer parsed.deinit();
    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();
    try frontend.render.pretty_print(&output.writer, parsed.file);
    try std.testing.expectEqualStrings("const X = foo(  // inside empty call\n          )\n\n", output.written());

    var reparsing_source: SourceFile = try .init(std.testing.allocator, "empty-call-formatted.propan", output.written());
    defer reparsing_source.deinit(std.testing.allocator);
    var reparsing: frontend.Parser = .init(&reparsing_source, &collection);
    var reparsed = try reparsing.parse(std.testing.allocator);
    defer reparsed.deinit();
    try std.testing.expectEqual(@as(usize, 1), reparsed.file.comments.len);
}

test "pretty printer ends empty and unterminated inputs with an empty line" {
    for ([_]struct { source: []const u8, expected: []const u8 }{
        .{ .source = "", .expected = "\n" },
        .{ .source = "// comment", .expected = "// comment\n\n" },
        .{ .source = "const A = 1", .expected = "const A = 1\n\n" },
        .{ .source = "const A = 1\n\n", .expected = "const A = 1\n\n" },
        .{ .source = "const A = 1\n\n\n\n", .expected = "const A = 1\n\n\n" },
    }) |case| {
        var collection: diagnostics.Collection = .init(std.testing.allocator);
        defer collection.deinit();
        var source: SourceFile = try .init(std.testing.allocator, "ending.propan", case.source);
        defer source.deinit(std.testing.allocator);
        var parser: frontend.Parser = .init(&source, &collection);
        var parsed = try parser.parse(std.testing.allocator);
        defer parsed.deinit();
        var buffer: [128]u8 = undefined;
        var output: std.Io.Writer = .fixed(&buffer);
        try frontend.render.pretty_print(&output, parsed.file);
        try std.testing.expectEqualStrings(case.expected, output.buffered());
    }
}
