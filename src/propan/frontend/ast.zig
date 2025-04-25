const std = @import("std");
const ptk = @import("ptk");

const parser = @import("parser.zig");

const Token = parser.Token;
const Location = ptk.Location;

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
};

pub const ConditionNode = struct {
    location: Location,
    type: Condition,
};

pub const Expression = union(enum) {
    integer: IntegerLiteral,
};

pub const IntegerLiteral = struct {
    location: Location,
    source_text: []const u8,
    value: u64,
};

pub const StringLiteral = struct {
    location: Location,
    source_text: []const u8,
    value: []const u8,
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
