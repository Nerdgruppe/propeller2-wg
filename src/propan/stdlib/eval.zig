const std = @import("std");

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

    pub fn address(value: TaggedAddress, usage: UsageHint) Value {
        return .init(.{ .address = value }, usage);
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
        address,
        register,
        enumerator,
        pointer_expr,
    };

    pub const Payload = union(Type) {
        int: i64,
        string: []const u8,
        address: TaggedAddress,
        register: Register,
        enumerator: []const u8,
        pointer_expr: PointerExpression,
    };

    pub fn format(val: Value, fmt: []const u8, opt: std.fmt.FormatOptions, writer: anytype) !void {
        _ = opt;

        if (std.mem.eql(u8, fmt, "nice")) {
            switch (val.value) {
                .int => |v| try writer.print("{}", .{v}),
                .string => |v| try writer.print("\"{}\"", .{std.zig.fmtEscapes(v)}),
                .address => |v| try writer.print("{}", .{v}),
                .register => |v| try writer.print("{}", .{v}),
                .enumerator => |v| try writer.print("#{s}", .{v}),
                .pointer_expr => |v| try writer.print("{}", .{v}),
            }
        } else {
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
                .address => |v| try writer.print("{}", .{v}),
                .register => |v| try writer.print("{}", .{v}),
                .enumerator => |v| try writer.print("#{s}", .{v}),
                .pointer_expr => |v| try writer.print("{}", .{v}),
            }
            try writer.print(", {s})", .{@tagName(val.flags.usage)});
        }
    }
};

pub const PointerExpression = struct {
    pub const Pointer = enum(u1) {
        PTRA = 0,
        PTRB = 1,
    };

    pointer: Pointer,
    increment: enum {
        none,
        pre_increment,
        pre_decrement,
        post_increment,
        post_decrement,
    },
    index: ?i64,

    pub fn format(ptr_expr: PointerExpression, fmt: []const u8, opt: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = opt;

        switch (ptr_expr.increment) {
            .none => {},
            .pre_increment => try writer.writeAll("++"),
            .pre_decrement => try writer.writeAll("--"),
            .post_increment => {},
            .post_decrement => {},
        }

        try writer.print("{s}", .{@tagName(ptr_expr.pointer)});

        switch (ptr_expr.increment) {
            .none => {},
            .pre_increment => {},
            .pre_decrement => {},
            .post_increment => try writer.writeAll("++"),
            .post_decrement => try writer.writeAll("--"),
        }

        if (ptr_expr.index) |index| {
            try writer.print("[{}]", .{index});
        }
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

pub const Segment_ID = enum(u32) { _ };

pub const TaggedAddress = struct {
    hub_address: u20,
    segment_id: Segment_ID,
    local: Local,

    pub const Local = union(ExecMode) {
        /// The offset points into hub memory
        hub,

        /// The offset points into cog memory
        cog: u9,

        /// The offset points into lut memory
        lut: u9,
    };

    pub fn get_local(offset: TaggedAddress) u20 {
        return switch (offset.local) {
            .cog, .lut => |val| val,
            .hub => offset.hub_address,
        };
    }

    pub fn init(segment: Segment_ID, hub: u20, local: Local) TaggedAddress {
        return .{ .segment_id = segment, .hub = hub, .local = local };
    }

    pub fn init_hub(segment: Segment_ID, hub: u20) TaggedAddress {
        return .{ .segment_id = segment, .hub_address = hub, .local = .hub };
    }

    pub fn init_cog(segment: Segment_ID, hub: u20, cog: u9) TaggedAddress {
        return .{ .segment_id = segment, .hub_address = hub, .local = .{ .cog = cog } };
    }

    pub fn init_lut(segment: Segment_ID, hub: u20, lut: u9) TaggedAddress {
        return .{ .segment_id = segment, .hub_address = hub, .local = .{ .lut = lut } };
    }

    pub fn format(offset: TaggedAddress, fmt: []const u8, opt: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = opt;
        switch (offset.local) {
            .cog => |cog| try writer.print("Address(segment=#{}, hub=0x{X:0>5}, cog=0x{X:0>3})", .{ @intFromEnum(offset.segment_id), offset.hub_address, cog }),
            .lut => |lut| try writer.print("Address(segment=#{}, hub=0x{X:0>5}, lut=0x{X:0>3})", .{ @intFromEnum(offset.segment_id), offset.hub_address, lut }),
            .hub => try writer.print("Address(segment=#{}, hub=0x{X:0>5})", .{ @intFromEnum(offset.segment_id), offset.hub_address }),
        }
    }
};
