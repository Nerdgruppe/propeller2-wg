const std = @import("std");
const define = @import("define.zig");
const eval = @import("eval.zig");

test "function wrappers accept both pointer registers and pointer expressions" {
    const function = comptime define.function(struct {
        pub const docs = "Pointer conversion regression.";
        pub const params = .{ .ptr = .{ .docs = "Pointer argument." } };
        pub fn invoke(ptr: eval.PointerExpression) u32 {
            return @intFromEnum(ptr.pointer);
        }
    });
    for ([_]u9{ 0x1F8, 0x1F9 }, 0..) |reg, expected| {
        const result = try function.invoke(undefined, &.{eval.Value.register(reg)});
        try std.testing.expectEqual(@as(i64, @intCast(expected)), result.value.int);
    }
    const expression: eval.Value = .{
        .value = .{ .pointer_expr = .{ .pointer = .PTRB, .increment = .post_increment, .index = 3 } },
        .flags = .{ .usage = .register },
    };
    try std.testing.expectEqual(@as(i64, 1), (try function.invoke(undefined, &.{expression})).value.int);
    try std.testing.expectError(error.InvalidArg, function.invoke(undefined, &.{eval.Value.register(0x1F6)}));
    try std.testing.expectError(error.TypeMismatch, function.invoke(undefined, &.{eval.Value.int(1)}));
}
