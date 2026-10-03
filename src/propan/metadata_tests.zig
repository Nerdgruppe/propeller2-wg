const std = @import("std");
const frontend = @import("frontend.zig");
const sema = @import("sema.zig");
const diagnostics = @import("diagnostics.zig");

test "module source locations survive mutation of the caller's path" {
    var path = "source.propan".*;
    const source = "const X = 1\nentry:\nLONG X\n";
    var collection: diagnostics.Collection = .init(std.testing.allocator);
    defer collection.deinit();
    var parser: frontend.Parser = .init(source, &path, &collection);
    var parsed = try parser.parse(std.testing.allocator);
    defer parsed.deinit();
    var module = try sema.analyze(std.testing.allocator, parsed.file, .{}, &collection);
    defer module.deinit();

    @memset(&path, '?');
    try std.testing.expectEqualStrings("source.propan", module.constants[0].location.source.?);
    try std.testing.expectEqualStrings("source.propan", module.symbols[0].source_location.?.source.?);
    for (module.line_data) |line| {
        try std.testing.expectEqualStrings("source.propan", line.location.source.?);
    }
}

test "module instruction metadata owns operand syntax and values" {
    var source = "const X = 7\nMOV PTRA, X\n".*;
    var collection: diagnostics.Collection = .init(std.testing.allocator);
    defer collection.deinit();
    var parser: frontend.Parser = .init(&source, "source.propan", &collection);
    var parsed = try parser.parse(std.testing.allocator);
    var module = try sema.analyze(std.testing.allocator, parsed.file, .{}, &collection);
    defer module.deinit();
    parsed.deinit();
    @memset(&source, '?');

    const line = module.line_data[0];
    try std.testing.expectEqual(.code, line.kind);
    try std.testing.expectEqualStrings("MOV", line.mnemonic.?);
    try std.testing.expectEqualStrings("X", line.operands[1].syntax);
    try std.testing.expectEqual(@as(i64, 7), line.operands[1].value.value.int);
}
