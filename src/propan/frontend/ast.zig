const std = @import("std");
const ptk = @import("ptk");

const parser = @import("parser.zig");

const Token = parser.Token;
pub const Location = ptk.Location;

pub const File = struct {
    sequence: []const Line,
};

pub const Line = union(enum) {
    empty,
    label: Label,
    constant: Constant,
    instruction: Instruction,
};

pub const Label = struct {
    location: Location,
    identifier: []const u8,
    type: Type,

    pub const Type = enum {
        @"var",
        code,
    };
};

pub const Constant = struct {
    location: Location,
    identifier: []const u8,
    value: Expression,
};

pub const Instruction = struct {
    location: Location,
    mnemonic: []const u8,

    arguments: []const Expression,

    condition: ?ConditionNode,
    effect: ?Effect,
};

pub const ConditionNode = struct {
    location: Location,
    type: Condition,
};

pub const Expression = union(enum) {
    wrapped: *Expression,
    integer: IntegerLiteral,
    string: StringLiteral,
    symbol: SymbolReference,
    unary_transform: UnaryTransform,
    binary_transform: BinaryTransform,

    function_call: FunctionInvocation,

    pub fn location(expr: Expression) Location {
        return switch (expr) {
            .wrapped => |value| value.location(),
            inline else => |value| value.location,
        };
    }
};

pub const IntegerLiteral = struct {
    location: Location,
    source_text: []const u8,
    value: u63,
};

pub const StringLiteral = struct {
    location: Location,
    source_text: []const u8,
    value: []const u8,
};

pub const SymbolReference = struct {
    location: Location,
    symbol_name: []const u8,
};

pub const UnaryTransform = struct {
    location: Location,
    value: *Expression,
    operator: UnaryOperator,
};

pub const BinaryTransform = struct {
    location: Location,
    lhs: *Expression,
    rhs: *Expression,
    operator: BinaryOperator,
};

pub const FunctionInvocation = struct {
    location: Location,
    function: []const u8,
    arguments: []const Argument,
    has_trailing_comma: bool,

    pub const Argument = struct {
        location: Location,
        name: ?[]const u8,
        value: Expression,
    };
};

pub const Condition = union(enum) {
    @"return",
    c_is: bool,
    z_is: bool,
    c_and_z: BinOp,
    c_or_z: BinOp,
    c_is_z,
    c_is_not_z,
    comparison: Comparison,

    pub const BinOp = struct {
        c: bool,
        z: bool,
    };

    pub const Comparison = enum {
        @">=",
        @"<=",
        @"==",
        @"!=",
        @"<",
        @">",
    };

    pub fn encode(cond: Condition) Code {
        return switch (cond) {
            .@"return" => .@"return",

            .c_is => |c| if (c) .if_c else .if_nc,
            .z_is => |z| if (z) .if_z else .if_nz,

            .c_and_z => |op| if (op.c)
                (if (op.z) .if_c_and_z else .if_c_and_nz)
            else
                (if (op.z) .if_nc_and_z else .if_nc_and_nz),

            .c_or_z => |op| if (op.c)
                (if (op.z) .if_c_or_z else .if_c_or_nz)
            else
                (if (op.z) .if_nc_or_z else .if_nc_or_nz),

            .c_is_z => .if_c_eq_z,
            .c_is_not_z => .if_c_ne_z,

            .comparison => |comp| switch (comp) {
                .@">=" => .if_nc,
                .@"<=" => .if_c_or_z,
                .@"==" => .if_z,
                .@"!=" => .if_nz,
                .@"<" => .if_c,
                .@">" => .if_nc_and_nz,
            },
        };
    }

    pub const Code = enum(u4) {
        /// Always execute and return (More Info)
        @"return" = 0b0000,

        /// Execute if C=0 AND Z=0
        if_nc_and_nz = 0b0001,

        /// Execute if C=0 AND Z=1
        if_nc_and_z = 0b0010,

        ///  Execute if C=0
        if_nc = 0b0011,

        /// Execute if C=1 AND Z=0
        if_c_and_nz = 0b0100,

        /// Execute if Z=0
        if_nz = 0b0101,

        /// Execute if C!=Z
        if_c_ne_z = 0b0110,

        /// Execute if C=0 OR Z=0
        if_nc_or_nz = 0b0111,

        /// Execute if C=1 AND Z=1
        if_c_and_z = 0b1000,

        /// Execute if C=Z
        if_c_eq_z = 0b1001,

        /// Execute if Z=1
        if_z = 0b1010,

        /// Execute if C=0 OR Z=1
        if_nc_or_z = 0b1011,

        /// Execute if C=1
        if_c = 0b1100,

        /// Execute if C=1 OR Z=0
        if_c_or_nz = 0b1101,

        /// Execute if C=1 OR Z=1
        if_c_or_z = 0b1110,

        /// Always execute
        always = 0b1111,
    };
};

pub const UnaryOperator = enum {
    @"!",
    @"~",
    @"+",
    @"-",
    @"@",
    @"*",
    @"&",
    pre_increment, // ++FOO
    pre_decrement, // --FOO
    post_increment, // FOO++
    post_decrement, // FOO--
};

pub const BinaryOperator = enum {
    @"and",
    @"or",
    xor,
    @"==",
    @"!=",
    @"<=>",
    @"<",
    @">",
    @"<=",
    @">=",
    @"+",
    @"-",
    @"|",
    @"^",
    @">>",
    @"<<",
    @"&",
    @"*",
    @"/",
    @"%",
    array_index,
};

pub const Effect = enum {
    and_c,
    and_z,
    or_c,
    or_z,
    xor_c,
    xor_z,
    wc,
    wcz,
    wz,

    pub const FillMask = packed struct(u2) {
        c: bool,
        z: bool,
    };

    pub fn get_write_mask(effect: Effect) FillMask {
        const lut: std.EnumArray(Effect, u2) = comptime .init(.{
            .and_c = 0b01,
            .and_z = 0b10,
            .or_c = 0b01,
            .or_z = 0b10,
            .xor_c = 0b01,
            .xor_z = 0b10,
            .wc = 0b01,
            .wcz = 0b11,
            .wz = 0b10,
        });
        return @bitCast(lut.get(effect));
    }
};
