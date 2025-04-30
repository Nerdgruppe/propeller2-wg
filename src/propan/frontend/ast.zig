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
    value: u32,
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
};

pub const UnaryOperator = enum {
    @"!",
    @"~",
    @"+",
    @"-",
    @"@",
    @"*",
    @"&",
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
};
