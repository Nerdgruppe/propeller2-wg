const std = @import("std");
const propan = @import("propan");

pub fn hover(allocator: std.mem.Allocator, name: []const u8, value: propan.eval.Value) ![]const u8 {
    var out: std.Io.Writer.Allocating = .init(allocator);
    try out.writer.print("**{s}**\n\n```text\n", .{name});
    try write(&out.writer, value);
    try out.writer.writeAll("\n```\n");
    return try out.toOwnedSlice();
}

pub fn write(writer: *std.Io.Writer, value: propan.eval.Value) !void {
    try writer.print("type: {t}\nusage: {t}\nvalue: ", .{ value.value, value.flags.usage });
    switch (value.value) {
        .int => |number| try writer.print("{d} (decimal)\n       {s}0x{X} (hexadecimal)", .{ number, if (number < 0) "-" else "", @abs(number) }),
        .string => |string| {
            if (std.unicode.utf8ValidateSlice(string)) {
                try std.json.Stringify.encodeJsonString(string, .{}, writer);
            } else {
                // Binary strings need JSON escapes for bytes that are not UTF-8.
                try writer.writeByte('"');
                for (string) |byte| {
                    if (byte >= 0x80) {
                        try writer.print("\\u00{X:0>2}", .{byte});
                    } else {
                        try std.json.Stringify.encodeJsonStringChars(&.{byte}, .{}, writer);
                    }
                }
                try writer.writeByte('"');
            }
        },
        .sequence => |items| try std.json.Stringify.value(items, .{}, writer),
        .register => |register| {
            try writer.print("{d} (0x{X:0>3}", .{ @intFromEnum(register), @intFromEnum(register) });
            for (propan.stdlib.p2.constants.keys(), propan.stdlib.p2.constants.values()) |name, builtin| {
                if (builtin.value == .register and builtin.value.register == register and !std.mem.eql(u8, name, "altered")) {
                    try writer.print(", {s}", .{name});
                    break;
                }
            }
            try writer.writeByte(')');
        },
        .address => |address| {
            try writer.writeAll("\n  hub: ");
            if (address.hub_address) |hub| {
                try writer.print("${X:0>5} (bytes)", .{hub});
            } else try writer.writeAll("none");
            try writer.writeAll("\n  local: ");
            if (address.get_local(.pc)) |local| {
                try writer.print("${X:0>3} ({t}, {s})", .{ local, address.local, if (address.local == .hub) "bytes" else "longs" });
            } else try writer.writeAll("none");
            if (address.subreg_byte != 0) try writer.print("\n  byte offset within long: {d}", .{address.subreg_byte});
        },
        .enumerator => |name| try writer.print("#{s}", .{name}),
        .pointer_expr => |pointer| try writer.print("{f}", .{pointer}),
    }
}
