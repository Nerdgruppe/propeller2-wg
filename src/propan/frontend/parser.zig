const std = @import("std");
const ptk = @import("ptk");

const ast = @import("ast.zig");

pub const Parser = struct {
    tokenizer: Tokenizer,

    pub fn init(source_code: []const u8, file_name: ?[]const u8) Parser {
        return .{
            .tokenizer = .init(source_code, file_name),
        };
    }

    pub fn parse(parser: *Parser, allocator: std.mem.Allocator) !ParsedFile {
        var arena: std.heap.ArenaAllocator = .init(allocator);
        errdefer arena.deinit();

        var sequence: std.ArrayListUnmanaged(ast.Line) = .empty;

        var core: Core = .{
            .arena = arena.allocator(),
            .core = .init(&parser.tokenizer),
        };

        try core.accept_file(&sequence);

        return .{
            .arena = arena,
            .file = .{
                .sequence = try sequence.toOwnedSlice(arena.allocator()),
            },
        };
    }

    const Core = struct {
        arena: std.mem.Allocator,
        core: ptk.ParserCore(Tokenizer, .{ .whitespace, .comment }),

        fn accept_file(c: *Core, sequence: *std.ArrayListUnmanaged(ast.Line)) !void {
            while (true) {
                if (try c.accept_line()) |line| {
                    try sequence.append(c.arena, line);
                } else {
                    break;
                }
            }
        }

        fn accept_line(c: *Core) !?ast.Line {
            const token = if (try c.next_token()) |token| token else return null;

            switch (token.type) {
                .linefeed => return .empty,

                .designator => return .{
                    .label = .{
                        .location = token.location,
                        .identifier = token.text[0 .. token.text.len - 1],
                    },
                },

                .@"const" => {
                    // constants
                    @panic("constants not supported yet!");
                },

                .@"if" => {
                    // if(X) mnemonic ...
                    const condition = try c.accept_condition();
                    const mnemonic = try c.accept_one(.identifier);

                    return .{
                        .instruction = try c.accept_instruction(condition, mnemonic),
                    };
                },

                .identifier => {
                    // mnemonic ...
                    return .{
                        .instruction = try c.accept_instruction(null, token),
                    };
                },

                else => return c.fatal_syntax_error(),
            }
        }

        fn accept_instruction(core: *Core, condition: ?ast.ConditionNode, mnemonic: Token) !ast.Instruction {
            std.debug.assert(mnemonic.type == .identifier);
            errdefer std.log.err("failed to accept {s}", .{mnemonic.text});

            var args: std.ArrayListUnmanaged(ast.Expression) = .empty;
            defer args.deinit(core.arena);

            var first_expr = true;

            while (core.accept_expression()) |expr| {
                if (!first_expr) {
                    _ = core.accept_one(.@",") catch break;
                }
                try args.append(core.arena, expr);
                first_expr = false;
            } else |_| {}

            var effect: ?ast.Effect = null;

            if (core.accept_one(.effect)) |effect_token| {
                const ok = for (allowed_effect_names) |ef| {
                    const name, const eff = ef;
                    if (std.ascii.eqlIgnoreCase(name, effect_token.text)) {
                        effect = eff;
                        break true;
                    }
                } else false;

                if (!ok) {
                    std.log.err("TODO: Emit error here!", .{});
                }
            } else |_| {}

            try core.accept_eol_or_eof();

            return .{
                .location = mnemonic.location,
                .condition = condition,
                .mnemonic = mnemonic.text,
                .effect = effect,

                .arguments = try args.toOwnedSlice(core.arena),
            };
        }

        fn accept_eol_or_eof(core: *Core) !void {
            _ = core.accept_one(.linefeed) catch |err| switch (err) {
                error.UnexpectedEndOfFile => {},
                else => |e| return e,
            };
        }

        fn accept_expression(core: *Core) !ast.Expression {
            _ = core;
            return error.NotImplemented;
        }

        // if(!C & !Z)`, `if(>)`
        // if(!C & Z)`
        // if(!C)`,`if(>=)`
        // if(C & !Z)`
        // if(!Z)`,`if(!=)`
        // if(C != Z)`
        // if(!C \| !Z)`
        // if(C & Z)`
        // if(C == Z)`
        // if(Z)`, `if(==)`
        // if(!C \| Z)`
        // if(C)`, `if(<)`
        // if(C \| !Z)`
        // if(C \| Z)`, `if(<=)`
        fn accept_condition(core: *Core) !ast.ConditionNode {
            _ = try core.accept_one(.@"(");

            const which_lhs, var lhs_token = try core.accept_any(&.{
                .identifier,
                .@"!",
                .@"<",
                .@"<=",
                .@">",
                .@">=",
                .@"==",
                .@"!=",
            });

            const cond: ast.Condition = switch (which_lhs) {
                inline .@"<", .@"<=", .@">=", .@">", .@"!=", .@"==" => |comp| .{
                    .comparison = @field(ast.Condition.Comparison, @tagName(comp)),
                },

                else => blk: {
                    const Flag = enum {
                        c,
                        z,

                        pub fn parse(tok: Token) error{ InvalidFlag, SyntaxError }!@This() {
                            if (tok.type != .identifier)
                                return error.SyntaxError;
                            if (std.ascii.eqlIgnoreCase(tok.text, "c")) return .c;
                            if (std.ascii.eqlIgnoreCase(tok.text, "z")) return .z;
                            return error.InvalidFlag;
                        }
                    };

                    const lhs_level = if (which_lhs == .@"!") inv: {
                        lhs_token = try core.accept_one(.identifier);
                        break :inv false;
                    } else true;
                    const lhs = try Flag.parse(lhs_token);

                    const which_op, _ = try core.accept_any(&.{
                        .@"!=",
                        .@"==",
                        .@"|",
                        .@"&",
                        .@")",
                    });

                    if (which_op == .@")") {
                        switch (lhs) {
                            .c => return .{
                                .location = lhs_token.location,
                                .type = .{ .c_is = lhs_level },
                            },
                            .z => return .{
                                .location = lhs_token.location,
                                .type = .{ .z_is = lhs_level },
                            },
                        }
                    }

                    const which_rhs, var rhs_token = try core.accept_any(&.{
                        .identifier,
                        .@"!",
                    });

                    const rhs_level = if (which_rhs == .@"!") inv: {
                        rhs_token = try core.accept_one(.identifier);
                        break :inv false;
                    } else true;
                    const rhs = try Flag.parse(rhs_token);

                    if (lhs == rhs)
                        return error.SyntaxError;

                    const c, const z = switch (lhs) {
                        .c => .{ lhs_level, rhs_level },
                        .z => .{ rhs_level, lhs_level },
                    };

                    switch (which_op) {
                        .@"==" => {
                            if (lhs_level == false or rhs_level == false)
                                return error.SyntaxError;
                            break :blk .c_is_z;
                        },
                        .@"!=" => {
                            if (lhs_level == false or rhs_level == false)
                                return error.SyntaxError;
                            break :blk .c_is_not_z;
                        },

                        .@"|" => break :blk .{
                            .c_or_z = .{ .c = c, .z = z },
                        },
                        .@"&" => break :blk .{
                            .c_and_z = .{ .c = c, .z = z },
                        },
                        .@")" => unreachable,
                    }
                },
            };

            _ = try core.accept_one(.@")");
            return .{
                .location = lhs_token.location,
                .type = cond,
            };
        }

        fn AcceptKey(comptime options: []const TokenType) type {
            var fields: [options.len]std.builtin.Type.EnumField = undefined;
            for (&fields, options, 0..) |*fld, opt, i| {
                fld.* = .{
                    .name = @tagName(opt),
                    .value = i,
                };
            }
            return @Type(.{
                .@"enum" = .{
                    .is_exhaustive = true,
                    .tag_type = u8,
                    .fields = &fields,
                    .decls = &.{},
                },
            });
        }

        fn accept_any(c: *Core, comptime options: []const TokenType) !struct { AcceptKey(options), Token } {
            const state = c.core.saveState();
            errdefer c.core.restoreState(state);

            const token_or_maybe = try c.next_token();
            const token = token_or_maybe orelse return error.UnexpectedEndOfFile;

            inline for (options) |opt| {
                if (token.type == opt)
                    return .{ @field(AcceptKey(options), @tagName(opt)), token };
            }

            std.log.err("failed to accept token {s}. expected one of {any}", .{ @tagName(token.type), options });

            return error.UnexpectedToken;
        }

        fn accept_one(c: *Core, comptime token_type: TokenType) !Token {
            _, const token = try c.accept_any(&.{token_type});
            return token;
        }

        fn next_token(c: *Core) !?Token {
            const tok = c.core.accept(r.any) catch |err| switch (err) {
                error.EndOfStream => null,
                else => |e| return e,
            };
            std.log.debug("{?}", .{tok});
            return tok;
        }

        fn fatal_syntax_error(c: *Core) error{SyntaxError} {
            _ = c;
            std.log.err("syntax error!", .{});
            return error.SyntaxError;
        }

        const r = ptk.RuleSet(TokenType);
    };
};

pub const ParsedFile = struct {
    arena: std.heap.ArenaAllocator,
    file: ast.File,

    pub fn deinit(parsed: *ParsedFile) void {
        parsed.arena.deinit();
        parsed.* = undefined;
    }
};

pub const Token = Tokenizer.Token;

pub const TokenType = enum {
    // aux
    comment,
    whitespace,
    linefeed,
    unexpected_character,

    // keywords
    @"const",
    @"var",
    @"if",
    @"return",
    @"and",
    @"or",
    xor,

    // symbols
    @"=",
    @"(",
    @")",
    @"[",
    @"]",
    @",",
    @"@",
    @"&",
    @"*",
    @"!",
    @"~",
    @"+",
    @"-",
    @"==",
    @"!=",
    @"<=>",
    @"<",
    @">",
    @"<=",
    @">=",
    @"|",
    @"^",
    @">>",
    @"<<",
    @"/",
    @"%",
    @":",
    @"?",

    // values
    integer, // 1234, 0xDEAD, 0b1100101, 0q12340123, 0o1234567
    char_literal, // 'x', '\U{1234F}'
    string_literal, //
    identifier, // [A-Za-z0-9_\.\-]+
    designator, // identifier, but ends with ":"
    effect, // identifier, but starts with ":"

};

const Pattern = ptk.Pattern(TokenType);

const match = ptk.matchers;
const pat_sequence = match.sequenceOf;

fn basic_ident(str: []const u8) ?usize {
    const first_char = "_.-abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
    const all_chars = first_char ++ "0123456789";
    for (str, 0..) |c, i| {
        if (std.mem.indexOfScalar(u8, if (i > 0) all_chars else first_char, c) == null) {
            return i;
        }
    }
    return str.len;
}

fn generic_string_literal(str: []const u8, comptime delim: u8) ?usize {
    if (str.len == 0 or str[0] != delim)
        return null;

    var i: usize = 1;
    while (true) {
        switch (str[i]) {
            // The end of the string, length includes the string delimiter:
            delim => return i + 1,

            // Skip over the next character if possible, as we're escaping it!
            '\\' => i += 1,

            // All non-space whitespace is forbidden inside strings:
            0x00...0x1F => return null,

            // The rest is ok
            else => {},
        }

        i += 1;
        if (i >= str.len)
            return null;
    }
}

fn string_literal(str: []const u8) ?usize {
    return generic_string_literal(str, '"');
}

fn char_literal(str: []const u8) ?usize {
    return generic_string_literal(str, '\'');
}

fn whitespace(str: []const u8) ?usize {
    for (str, 0..) |c, i| {
        if (!std.ascii.isWhitespace(c) or c == '\r' or c == '\n')
            return i;
    }
    return str.len;
}

pub const Tokenizer = ptk.Tokenizer(TokenType, &[_]Pattern{
    .create(.linefeed, match.linefeed),
    .create(.whitespace, whitespace),
    .create(.comment, pat_sequence(.{ match.literal("//"), match.takeNoneOf("\r\n") })),

    .create(.integer, pat_sequence(.{ match.literal("0b"), match.takeAnyOfIgnoreCase("_01") })),
    .create(.integer, pat_sequence(.{ match.literal("0q"), match.takeAnyOfIgnoreCase("_0123") })),
    .create(.integer, pat_sequence(.{ match.literal("0o"), match.takeAnyOfIgnoreCase("_01234567") })),
    .create(.integer, pat_sequence(.{ match.literal("0x"), match.takeAnyOfIgnoreCase("_0123456789ABCDEF") })),

    .create(.integer, pat_sequence(.{ match.takeAnyOfIgnoreCase("0123456789"), match.takeAnyOfIgnoreCase("_0123456789") })),
    .create(.integer, pat_sequence(.{match.takeAnyOfIgnoreCase("0123456789")})),

    .create(.string_literal, string_literal),
    .create(.char_literal, char_literal),

    .create(.@"const", match.word("const")),
    .create(.@"var", match.word("var")),
    .create(.@"if", match.word("if")),
    .create(.@"return", match.word("return")),
    .create(.@"and", match.word("and")),
    .create(.@"or", match.word("or")),
    .create(.xor, match.word("xor")),

    .create(.designator, pat_sequence(.{ basic_ident, match.literal(":") })),
    .create(.effect, pat_sequence(.{ match.literal(":"), basic_ident })),

    .create(.identifier, basic_ident),

    .create(.@"<=>", match.literal("<=>")),
    .create(.@"==", match.literal("==")),
    .create(.@"!=", match.literal("!=")),
    .create(.@"<=", match.literal("<=")),
    .create(.@">>", match.literal(">>")),
    .create(.@"<<", match.literal("<<")),
    .create(.@">=", match.literal(">=")),

    .create(.@"<", match.literal("<")),
    .create(.@">", match.literal(">")),
    .create(.@"|", match.literal("|")),
    .create(.@"^", match.literal("^")),
    .create(.@"/", match.literal("/")),
    .create(.@"%", match.literal("%")),
    .create(.@":", match.literal(":")),
    .create(.@"?", match.literal("?")),
    .create(.@"=", match.literal("=")),
    .create(.@"(", match.literal("(")),
    .create(.@")", match.literal(")")),
    .create(.@"[", match.literal("[")),
    .create(.@"]", match.literal("]")),
    .create(.@",", match.literal(",")),
    .create(.@"@", match.literal("@")),
    .create(.@"&", match.literal("&")),
    .create(.@"*", match.literal("*")),
    .create(.@"!", match.literal("!")),
    .create(.@"~", match.literal("~")),
    .create(.@"+", match.literal("+")),
    .create(.@"-", match.literal("-")),

    // A bare '-' would be a .@"-" token, so any other token caught here
    // is illegal:
    .create(.unexpected_character, match.takeNoneOf("-")),
});

const ParserCore = ptk.ParserCore(Tokenizer, .{.whitespace});

const allowed_effect_names: []const struct { []const u8, ast.Effect } = &.{
    .{ ":and_c", .and_c },
    .{ ":andc", .and_c },
    .{ ":and_z", .and_z },
    .{ ":andz", .and_z },
    .{ ":or_c", .or_c },
    .{ ":orc", .or_c },
    .{ ":or_z", .or_z },
    .{ ":orz", .or_z },
    .{ ":xor_c", .xor_c },
    .{ ":xorc", .xor_c },
    .{ ":xor_z", .xor_z },
    .{ ":xorz", .xor_z },
    .{ ":wc", .wc },
    .{ ":wcz", .wcz },
    .{ ":wzc", .wcz },
    .{ ":wz", .wz },
};

fn fuzz_tokenizer(_: void, input: []const u8) !void {
    var tokenizer: Tokenizer = .init(input, null);
    while (try tokenizer.next()) |item| {
        _ = item;
    }
}

test "fuzz tokenizer" {
    try std.testing.fuzz({}, fuzz_tokenizer, .{});
}

test "parse conditions (positive)" {
    const Expect = struct {
        expected: ast.Condition,
        input: []const u8,
    };

    const expects = [_]Expect{
        .{ .input = "(C)", .expected = .{ .c_is = true } },
        .{ .input = "(!C)", .expected = .{ .c_is = false } },
        .{ .input = "(Z)", .expected = .{ .z_is = true } },
        .{ .input = "(!Z)", .expected = .{ .z_is = false } },

        .{ .input = "(C == Z)", .expected = .c_is_z },
        .{ .input = "(C != Z)", .expected = .c_is_not_z },

        .{ .input = "(Z == C)", .expected = .c_is_z },
        .{ .input = "(Z != C)", .expected = .c_is_not_z },

        .{ .input = "(C & Z)", .expected = .{ .c_and_z = .{ .c = true, .z = true } } },
        .{ .input = "(C & !Z)", .expected = .{ .c_and_z = .{ .c = true, .z = false } } },
        .{ .input = "(!C & Z)", .expected = .{ .c_and_z = .{ .c = false, .z = true } } },
        .{ .input = "(!C & !Z)", .expected = .{ .c_and_z = .{ .c = false, .z = false } } },

        .{ .input = "(Z & C)", .expected = .{ .c_and_z = .{ .c = true, .z = true } } },
        .{ .input = "(Z & !C)", .expected = .{ .c_and_z = .{ .c = false, .z = true } } },
        .{ .input = "(!Z & C)", .expected = .{ .c_and_z = .{ .c = true, .z = false } } },
        .{ .input = "(!Z & !C)", .expected = .{ .c_and_z = .{ .c = false, .z = false } } },

        .{ .input = "(C | Z)", .expected = .{ .c_or_z = .{ .c = true, .z = true } } },
        .{ .input = "(C | !Z)", .expected = .{ .c_or_z = .{ .c = true, .z = false } } },
        .{ .input = "(!C | Z)", .expected = .{ .c_or_z = .{ .c = false, .z = true } } },
        .{ .input = "(!C | !Z)", .expected = .{ .c_or_z = .{ .c = false, .z = false } } },

        .{ .input = "(Z | C)", .expected = .{ .c_or_z = .{ .c = true, .z = true } } },
        .{ .input = "(Z | !C)", .expected = .{ .c_or_z = .{ .c = false, .z = true } } },
        .{ .input = "(!Z | C)", .expected = .{ .c_or_z = .{ .c = true, .z = false } } },
        .{ .input = "(!Z | !C)", .expected = .{ .c_or_z = .{ .c = false, .z = false } } },

        .{ .input = "(>=)", .expected = .{ .comparison = .@">=" } },
        .{ .input = "(<=)", .expected = .{ .comparison = .@"<=" } },
        .{ .input = "(==)", .expected = .{ .comparison = .@"==" } },
        .{ .input = "(!=)", .expected = .{ .comparison = .@"!=" } },
        .{ .input = "(<)", .expected = .{ .comparison = .@"<" } },
        .{ .input = "(>)", .expected = .{ .comparison = .@">" } },
    };

    for (expects) |expectation| {
        errdefer std.log.err("condition parsing failed: expected {}", .{expectation.expected});

        var tok: Tokenizer = .init(expectation.input, null);

        var arena: std.heap.ArenaAllocator = .init(std.testing.allocator);
        errdefer arena.deinit();

        var core: Parser.Core = .{
            .arena = arena.allocator(),
            .core = .init(&tok),
        };

        const cond = try core.accept_condition();

        try std.testing.expectEqualDeep(expectation.expected, cond.type);
    }
}

test "parse effect (positive)" {
    const Expect = struct {
        expected: ast.Effect,
        input: []const u8,
    };

    const expects = [_]Expect{
        .{ .input = ":wc", .expected = .wc },
        .{ .input = ":wz", .expected = .wz },
        .{ .input = ":wcz", .expected = .wcz },
        .{ .input = ":wzc", .expected = .wcz },
        .{ .input = ":xor_c", .expected = .xor_c },
        .{ .input = ":xorc", .expected = .xor_c },
        .{ .input = ":and_c", .expected = .and_c },
        .{ .input = ":andc", .expected = .and_c },
        .{ .input = ":or_c", .expected = .or_c },
        .{ .input = ":orc", .expected = .or_c },
        .{ .input = ":xor_z", .expected = .xor_z },
        .{ .input = ":xorz", .expected = .xor_z },
        .{ .input = ":and_z", .expected = .and_z },
        .{ .input = ":andz", .expected = .and_z },
        .{ .input = ":or_z", .expected = .or_z },
        .{ .input = ":orz", .expected = .or_z },
    };

    for ([2]bool{ false, true }) |upper_case| {
        for (expects) |expectation| {
            errdefer std.log.err("condition parsing failed: expected {}", .{expectation.expected});

            var buffer: [32]u8 = @splat(0);
            var input_buf: [64]u8 = @splat(0);
            const input = try std.fmt.bufPrint(&input_buf, "NOP {s}\r\n", .{
                if (upper_case)
                    std.ascii.upperString(&buffer, expectation.input)
                else
                    std.ascii.lowerString(&buffer, expectation.input),
            });

            var tok: Tokenizer = .init(input, null);

            var arena: std.heap.ArenaAllocator = .init(std.testing.allocator);
            errdefer arena.deinit();

            var core: Parser.Core = .{
                .arena = arena.allocator(),
                .core = .init(&tok),
            };

            const identifier = try core.accept_one(.identifier);

            const instr = try core.accept_instruction(null, identifier);

            try std.testing.expectEqualStrings("NOP", instr.mnemonic);

            try std.testing.expectEqual(expectation.expected, instr.effect);
        }
    }
}
