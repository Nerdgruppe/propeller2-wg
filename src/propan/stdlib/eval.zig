const std = @import("std");

pub const Context = struct {
    //
};

pub const Symbol = struct {
    //
};

pub const Value = struct {
    value: Payload,
    flags: Flags,

    pub fn init(value: Payload, usage: UsageHint) Value {
        return .{
            .flags = .{ .usage = usage },
            .value = value,
        };
    }

    pub fn int(value: i64) Value {
        return .init(.{ .int = value }, .literal);
    }

    pub fn string(value: []const u8) Value {
        return .init(.{ .string = value }, .literal);
    }

    pub fn offset(value: Offset, usage: UsageHint) Value {
        return .init(.{ .offset = value }, usage);
    }

    pub fn register(index: u9) Value {
        return .init(
            .{ .register = @enumFromInt(index) },
            .register,
        );
    }

    pub fn enumerator(text: []const u8) Value {
        return .init(
            .{ .enumerator = text },
            .literal,
        );
    }

    pub const Flags = packed struct {
        usage: UsageHint,
        augment: bool = false,
        addressing: AddressingMode = .auto,
    };

    pub const AddressingMode = enum(u2) {
        auto = 0,
        absolute = 1,
        relative = 2,
    };

    pub const UsageHint = enum(u1) {
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

    pub const Type = enum {
        int,
        string,
        offset,
        register,
        enumerator,
    };

    pub const Payload = union(Type) {
        int: i64,
        string: []const u8,
        offset: Offset,
        register: Register,
        enumerator: []const u8,
    };

    pub fn format(val: Value, fmt: []const u8, opt: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = opt;
        try writer.writeAll("Value(");
        if (val.flags.augment) {
            try writer.writeAll("AUG,");
        }
        switch (val.flags.addressing) {
            .auto => {},
            .relative => try writer.writeAll("REL,"),
            .absolute => try writer.writeAll("ABS,"),
        }
        switch (val.value) {
            .int => |v| try writer.print("{}", .{v}),
            .string => |v| try writer.print("\"{}\"", .{std.zig.fmtEscapes(v)}),
            .offset => |v| try writer.print("{}", .{v}),
            .register => |v| try writer.print("{}", .{v}),
            .enumerator => |v| try writer.print("#{s}", .{v}),
        }
        try writer.print(", {s})", .{@tagName(val.flags.usage)});
    }
};

pub const Register = enum(u9) {
    _,

    pub fn format(reg: Register, fmt: []const u8, opt: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = opt;
        try writer.print("r{}", .{@intFromEnum(reg)});
    }
};

pub const ExecMode = enum {
    hub,
    cog,
    lut,
};

pub const Offset = struct {
    hub: u32,
    local: Local,

    pub const Local = union(ExecMode) {
        /// The offset points into hub memory
        hub,

        /// The offset points into cog memory
        cog: u9,

        /// The offset points into lut memory
        lut: u9,
    };

    pub fn get_local(offset: Offset) u32 {
        return switch (offset.local) {
            .cog, .lut => |val| val,
            .hub => offset.hub,
        };
    }

    pub fn init(hub: u32, local: Local) Offset {
        return .{ .hub = hub, .local = local };
    }

    pub fn init_hub(hub: u32) Offset {
        return .{ .hub = hub, .local = .hub };
    }

    pub fn init_cog(hub: u32, cog: u9) Offset {
        return .{ .hub = hub, .local = .{ .cog = cog } };
    }

    pub fn init_lut(hub: u32, lut: u9) Offset {
        return .{ .hub = hub, .local = .{ .lut = lut } };
    }

    pub fn format(offset: Offset, fmt: []const u8, opt: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = opt;
        switch (offset.local) {
            .cog => |cog| try writer.print("Offset(hub=0x{X:0>5}, cog=0x{X:0>3})", .{ offset.hub, cog }),
            .lut => |lut| try writer.print("Offset(hub=0x{X:0>5}, lut=0x{X:0>3})", .{ offset.hub, lut }),
            .hub => try writer.print("Offset(hub=0x{X:0>5})", .{offset.hub}),
        }
    }
};
