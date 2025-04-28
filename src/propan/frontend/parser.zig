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
        lf_is_whitespace: bool = false,

        fn move_to_heap(core: *Core, comptime T: type, value: T) !*T {
            const copy = try core.arena.create(T);
            copy.* = value;
            return copy;
        }

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

                .@"var" => {
                    const name = try c.accept_one(.designator);

                    return .{
                        .label = .{
                            .location = token.location,
                            .identifier = name.text[0 .. token.text.len - 1],
                            .type = .@"var",
                        },
                    };
                },

                .designator => return .{
                    .label = .{
                        .location = token.location,
                        .identifier = token.text[0 .. token.text.len - 1],
                        .type = .code,
                    },
                },

                .@"const" => {
                    const name = try c.accept_one(.identifier);

                    _ = try c.accept_one(.@"=");

                    const value = try c.accept_expression();

                    try c.accept_eol_or_eof();

                    return .{
                        .constant = .{
                            .location = token.location,
                            .identifier = name.text,
                            .value = value,
                        },
                    };
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

            while (core.accept_expression()) |expr| {
                try args.append(core.arena, expr);

                // If accepting a "," fails, we're at the end of
                // the argument list.
                _ = core.accept_one(.@",") catch break;

                // After each argument, we get the option to
                // insert a *single* line break:

                if (core.accept_one(.linefeed)) |_| {} else |_| {}
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

        const AcceptExprError = error{
            OutOfMemory,
            SyntaxError,
            UnexpectedToken,
            UnexpectedCharacter,
            UnexpectedEndOfFile,
            Overflow,
            InvalidCharacter,
            Utf8CannotEncodeSurrogateHalf,
            CodepointTooLarge,
            InvalidUtf8,
        };
        fn accept_expression(core: *Core) AcceptExprError!ast.Expression {
            const which, const token = try core.accept_any(&.{
                .integer,
                .identifier,
                .char_literal,
                .string_literal,
                .@"(",
            });

            switch (which) {
                .@"(" => {
                    const whitespace = core.push_ignore_whitespace();
                    defer whitespace.pop();

                    const value = try core.accept_expression();

                    _ = try core.accept_one(.@")");

                    return .{
                        .wrapped = try core.move_to_heap(ast.Expression, value),
                    };
                },

                .integer => return .{
                    .integer = .{
                        .location = token.location,
                        .source_text = token.text,
                        .value = try core.parse_int(token.text),
                    },
                },
                .identifier => {
                    if (core.accept_one(.@"(")) |_| {
                        // <identifier> "(" is a function call

                        const whitespace = core.push_ignore_whitespace();
                        defer whitespace.pop();

                        const args, const trailing_comma = try core.accept_argv();

                        return .{
                            .function_call = .{
                                .arguments = args,
                                .has_trailing_comma = trailing_comma,
                                .location = token.location,
                                .function = token.text,
                            },
                        };
                    } else |_| {
                        // symbol reference, not function call
                        return .{
                            .symbol = .{
                                .location = token.location,
                                .symbol_name = token.text,
                            },
                        };
                    }
                },
                .char_literal => {
                    var buffer: [32]u8 = undefined;
                    var fba: std.heap.FixedBufferAllocator = .init(&buffer);

                    const string = try core.unescape_string(fba.allocator(), token.text);

                    const view = try std.unicode.Utf8View.init(string);

                    var iter = view.iterator();

                    const codepoint: u32 = if (iter.nextCodepoint()) |codepoint|
                        codepoint
                    else blk: {
                        std.log.err("empty character literal not allowed!", .{});
                        break :blk 0;
                    };

                    return .{
                        .integer = .{
                            .location = token.location,
                            .source_text = token.text,
                            .value = codepoint,
                        },
                    };
                },
                .string_literal => {
                    const string = try core.unescape_string(core.arena, token.text);

                    return .{
                        .string = .{
                            .location = token.location,
                            .source_text = token.text,
                            .value = string,
                        },
                    };
                },
            }
        }

        fn accept_argv(core: *Core) !struct { []ast.FunctionInvocation.Argument, bool } {
            var argv: std.ArrayListUnmanaged(ast.FunctionInvocation.Argument) = try .initCapacity(core.arena, 3);
            defer argv.deinit(core.arena);

            // shortcut: if we accept a ")", we're having an empty argument list:
            if (core.accept_one(.@")")) |_| {
                return .{ &.{}, false };
            } else |_| {}

            const last_is_comma: bool = while (true) {
                const backup = core.core.saveState();

                const maybe_name: ?Token = if (core.accept_one(.identifier)) |ident| blk: {
                    _ = core.accept_one(.@"=") catch {
                        core.core.restoreState(backup);
                        break :blk null;
                    };

                    break :blk ident;
                } else |_| null;

                const value = try core.accept_expression();

                try argv.append(core.arena, .{
                    .location = if (maybe_name) |tok| tok.location else value.location(),
                    .name = if (maybe_name) |tok| tok.text else null,
                    .value = value,
                });

                const terminator, _ = try core.accept_any(&.{ .@",", .@")" });
                switch (terminator) {
                    .@")" => {
                        break false;
                    },

                    .@"," => {
                        if (core.accept_one(.@")")) |_| {
                            break true;
                        } else |_| {}
                    },
                }
            };

            return .{
                try argv.toOwnedSlice(core.arena),
                last_is_comma,
            };
        }

        fn unescape_string(core: *Core, allocator: std.mem.Allocator, raw: []const u8) ![]u8 {
            _ = core;

            std.debug.assert(raw.len >= 2);

            const body = raw[1 .. raw.len - 1];

            var output: std.ArrayListUnmanaged(u8) = try .initCapacity(allocator, body.len);
            defer output.deinit(allocator);

            var i: usize = 0;
            while (i < body.len) : (i += 1) {
                const char = body[i];
                if (char < 0x20 or char == 0x7F) {
                    std.log.err("invalid character in string/char literal: 0x{X:0>2}", .{char});
                } else if (i == '\\') {
                    i += 1;
                    if (i >= body.len) {
                        std.log.err("unterminated escape sequence", .{});
                        break;
                    }
                    const escape = body[i];
                    switch (escape) {
                        // single-char escapes:
                        'e' => try output.append(allocator, std.ascii.control_code.esc),
                        'r' => try output.append(allocator, std.ascii.control_code.cr),
                        'n' => try output.append(allocator, std.ascii.control_code.lf),
                        't' => try output.append(allocator, std.ascii.control_code.ht),
                        '\"' => try output.append(allocator, '\"'),
                        '\'' => try output.append(allocator, '\''),

                        // \xHH
                        'x' => {
                            const start = i + 1;
                            i += 2;
                            if (i >= body.len) {
                                std.log.err("unterminated escape sequence", .{});
                                break;
                            }
                            const hex = body[start..i];
                            std.log.err("escape: {}", .{std.fmt.fmtSliceHexLower(hex)});

                            try output.append(
                                allocator,
                                try std.fmt.parseInt(u8, hex, 16),
                            );
                        },

                        // \u{HHHHH}
                        'u' => {
                            const start = i + 1;
                            while (i < body.len) {
                                if (body[i] == '}')
                                    break;
                                i += 1;
                            }
                            if (i >= body.len) {
                                std.log.err("unterminated escape sequence", .{});
                                break;
                            }
                            const slice = body[start..i];

                            std.log.err("escape: {}", .{std.fmt.fmtSliceHexLower(slice)});

                            const codepoint = try std.fmt.parseInt(u21, slice, 16);

                            var buf: [8]u8 = undefined;
                            const len = try std.unicode.utf8Encode(codepoint, &buf);
                            try output.appendSlice(allocator, buf[0..len]);
                        },
                        else => {
                            std.log.warn("invalid escape sequence: \\x{c}", .{body[i .. i + 1]});
                            try output.append(allocator, char);
                        },
                    }
                } else {
                    try output.append(allocator, char);
                }
            }

            return output.toOwnedSlice(allocator);
        }

        fn parse_int(core: *Core, text: []const u8) !u64 {
            _ = core;
            if (std.mem.startsWith(u8, text, "0b"))
                return try std.fmt.parseInt(u64, text[2..], 2);

            if (std.mem.startsWith(u8, text, "0q"))
                return try std.fmt.parseInt(u64, text[2..], 4);

            if (std.mem.startsWith(u8, text, "0o"))
                return try std.fmt.parseInt(u64, text[2..], 8);

            if (std.mem.startsWith(u8, text, "0x"))
                return try std.fmt.parseInt(u64, text[2..], 16);

            return try std.fmt.parseInt(u64, text, 10);
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

            std.log.debug("failed to accept token {s}. expected one of {any}", .{ @tagName(token.type), options });

            return error.UnexpectedToken;
        }

        fn accept_one(c: *Core, comptime token_type: TokenType) !Token {
            _, const token = try c.accept_any(&.{token_type});
            return token;
        }

        const SpaceGuard = struct {
            core: *Core,
            restore: bool,

            pub fn pop(sg: SpaceGuard) void {
                sg.core.lf_is_whitespace = sg.restore;
            }
        };

        fn push_ignore_whitespace(core: *Core) SpaceGuard {
            const restore: SpaceGuard = .{ .core = core, .restore = core.lf_is_whitespace };
            core.lf_is_whitespace = true;
            return restore;
        }

        fn next_token(c: *Core) !?Token {
            while (true) {
                const tok = c.core.accept(r.any) catch |err| switch (err) {
                    error.EndOfStream => return null,
                    else => |e| return e,
                };
                if (c.lf_is_whitespace and tok.type == .linefeed)
                    continue;
                std.log.debug("next_token() => {}", .{tok});
                return tok;
            }
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

const patterns = struct {
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

    const list = [_]Pattern{
        .create(.linefeed, match.linefeed),
        .create(.whitespace, whitespace),
        .create(.comment, pat_sequence(.{ match.literal("//"), match.takeNoneOf("\r\n") })),
        .create(.comment, pat_sequence(.{match.literal("//")})),

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
    };
};

pub const Tokenizer = ptk.Tokenizer(TokenType, &patterns.list);

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
