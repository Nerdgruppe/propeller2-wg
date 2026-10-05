const std = @import("std");
const ptk = @import("ptk");

const parser = @import("parser.zig");

const Token = parser.Token;
pub const Location = ptk.Location;

/// Byte offsets are half-open and refer to one parsed source file.
pub const SourceSpan = struct {
    source: ?*const Source = null,
    start: u32,
    end: u32,

    pub const empty: SourceSpan = .{ .start = 0, .end = 0 };

    pub const Source = struct {
        name: ?[]const u8,
        text: []const u8,
        line_starts: []const usize,
    };

    pub fn location(span: SourceSpan) Location {
        const source = span.source orelse return .empty;
        const offset: usize = @min(span.start, source.text.len);
        var lower: usize = 0;
        var upper: usize = source.line_starts.len;
        while (lower + 1 < upper) {
            const mid = lower + (upper - lower) / 2;
            if (source.line_starts[mid] <= offset) lower = mid else upper = mid;
        }
        return .{
            .source = source.name,
            .line = @intCast(lower + 1),
            .column = @intCast(offset - source.line_starts[lower] + 1),
        };
    }

    pub fn endLocation(span: SourceSpan) Location {
        return (SourceSpan{ .source = span.source, .start = span.end, .end = span.end }).location();
    }

    pub fn at(span: SourceSpan, offset: u32) SourceSpan {
        return .{ .source = span.source, .start = offset, .end = offset + 1 };
    }
};

pub const File = struct {
    span: SourceSpan = .empty,
    sequence: []const Line,
    comments: []const Comment = &.{},
    source: []const u8 = "",
};

pub const Comment = struct {
    span: SourceSpan,
    text: []const u8,
};

pub const Line = union(enum) {
    empty: SourceSpan,
    label: Label,
    constant: Constant,
    instruction: Instruction,
};

pub const Label = struct {
    span: SourceSpan,
    identifier: []const u8,
    type: Type,
    local_scope: ?LocalScope = null,

    pub const Type = enum {
        @"var",
        code,
    };
};

pub const LocalScope = struct {
    id: usize,
    parent: ?[]const u8,
};

pub const Constant = struct {
    span: SourceSpan,
    identifier: []const u8,
    value: Expression,
};

pub const Instruction = struct {
    span: SourceSpan,
    mnemonic_span: SourceSpan,
    mnemonic: []const u8,

    arguments: []const Expression,

    condition: ?ConditionNode,
    effect: ?EffectNode,

    pub fn location(instruction: Instruction) Location {
        return instruction.mnemonic_span.location();
    }
};

pub const ConditionNode = struct {
    span: SourceSpan,
    type: Condition,
};

pub const EffectNode = struct {
    span: SourceSpan,
    type: Effect,
};

pub const Expression = union(enum) {
    wrapped: WrappedExpression,
    current_pc: SourceSpan,
    integer: IntegerLiteral,
    enumerator: SymbolReference,
    string: StringLiteral,
    sequence: SequenceLiteral,
    symbol: SymbolReference,
    unary_transform: UnaryTransform,
    binary_transform: BinaryTransform,

    function_call: FunctionInvocation,

    pub fn span(expr: Expression) SourceSpan {
        return switch (expr) {
            .current_pc => |value| value,
            inline else => |value| value.span,
        };
    }

    pub fn location(expr: Expression) Location {
        return switch (expr) {
            .wrapped => |value| value.value.location(),
            .unary_transform => |value| value.operator_span.location(),
            .binary_transform => |value| value.operator_span.location(),
            else => expr.span().location(),
        };
    }
};

pub const WrappedExpression = struct {
    span: SourceSpan,
    value: *Expression,
};

pub const IntegerLiteral = struct {
    span: SourceSpan,
    source_text: []const u8,
    value: u63,
};

pub const StringLiteral = struct {
    span: SourceSpan,
    source_text: []const u8,
    value: []const u8,
};

pub const SequenceLiteral = struct {
    span: SourceSpan,
    items: []const Expression,
};

pub const SymbolReference = struct {
    span: SourceSpan,
    symbol_name: []const u8,
    local_scope: ?LocalScope = null,
};

pub const UnaryTransform = struct {
    span: SourceSpan,
    operator_span: SourceSpan,
    value: *Expression,
    operator: UnaryOperator,
};

pub const BinaryTransform = struct {
    span: SourceSpan,
    operator_span: SourceSpan,
    lhs: *Expression,
    rhs: *Expression,
    operator: BinaryOperator,
};

pub const FunctionInvocation = struct {
    span: SourceSpan,
    function: []const u8,
    arguments: []const Argument,
    has_trailing_comma: bool,

    pub const Argument = struct {
        span: SourceSpan,
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
