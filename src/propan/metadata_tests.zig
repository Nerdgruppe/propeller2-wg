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
