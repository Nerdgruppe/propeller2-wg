const std = @import("std");

pub const Context = struct {
    //
};

pub const Symbol = struct {
    //
};

pub const Value = struct {
    value: Payload,
    usage: UsageHint,
    augment: bool = false,

    pub fn int(value: i64) Value {
        return .{
            .usage = .literal,
            .value = .{ .int = value },
        };
    }

    pub fn string(value: []const u8) Value {
        return .{
            .usage = .literal,
            .value = .{ .string = value },
        };
    }

    pub fn offset(value: Offset, usage: UsageHint) Value {
        return .{
            .usage = usage,
            .value = .{ .string = value },
        };
    }

    pub const UsageHint = enum {
        /// This value is meant to be passed as a literal
        /// value
        /// ```pasm
        ///     JMP #function ' literal
        /// ```
        literal,

        /// This value is meant to be used as a
        /// literal register index.
        /// ```pasm
        ///     JMP foo ' register
        /// foo
        ///     LONG #function
        /// ```
        register,
    };

    pub const Payload = union(enum) {
        int: i64,
        string: []const u8,
        offset: Offset,
    };

    pub fn format(val: Value, fmt: []const u8, opt: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = opt;
        try writer.writeAll("Value(");
        if (val.augment) {
            try writer.writeAll("AUG,");
        }
        switch (val.value) {
            .int => |v| try writer.print("{}", .{v}),
            .string => |v| try writer.print("\"{}\"", .{std.zig.fmtEscapes(v)}),
            .offset => |v| try writer.print("{}", .{v}),
        }
        try writer.print(", {s})", .{@tagName(val.usage)});
    }
};

pub const Offset = struct {
    hub: u32,
    cog: ?u9,

    pub fn init(hub: u32, cog: ?u9) Offset {
        return .{ .hub = hub, .cog = cog };
    }

    pub fn format(offset: Offset, fmt: []const u8, opt: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = opt;
        if (offset.cog) |cog| {
            try writer.print("Offset(hub=0x{X:0>5}, cog=0x{X:0>3})", .{ offset.hub, cog });
        } else {
            try writer.print("Offset(hub=0x{X:0>5}, cog=-)", .{offset.hub});
        }
    }
};
