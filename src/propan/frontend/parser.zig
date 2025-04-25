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
        core: ptk.ParserCore(Tokenizer, .{.whitespace}),

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
            while (true) {
                const token = if (try c.next_token()) |token| token else return null;

                var condition: ?ast.Condition = null;
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
                        // if(X) ... instruction ...

                        condition = try c.accept_condition();
                    },

                    .identifier => {
                        // instruction ...

                    },

                    else => return c.fatal_syntax_error(),
                }
            }
        }

        fn accept_condition(c: *Core) !ast.Condition {
            //
        }

        fn next_token(c: *Core) !?Token {
            const tok = try c.core.accept(r.any);
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
    tag, // identifier, but starts with ":"

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

pub const Tokenizer = ptk.Tokenizer(TokenType, &[_]Pattern{
    .create(.linefeed, match.linefeed),
    .create(.whitespace, match.whitespace),
    .create(.comment, pat_sequence(.{ match.literal("//"), match.takeNoneOf("\r\n") })),

    .create(.integer, pat_sequence(.{ match.literal("0b"), match.takeAnyOfIgnoreCase("_01") })),
    .create(.integer, pat_sequence(.{ match.literal("0q"), match.takeAnyOfIgnoreCase("_0123") })),
    .create(.integer, pat_sequence(.{ match.literal("0o"), match.takeAnyOfIgnoreCase("_01234567") })),
    .create(.integer, pat_sequence(.{ match.literal("0x"), match.takeAnyOfIgnoreCase("_0123456789ABCDEF") })),

    .create(.integer, pat_sequence(.{ match.takeAnyOfIgnoreCase("0123456789"), match.takeAnyOfIgnoreCase("_0123456789") })),
    .create(.integer, pat_sequence(.{match.takeAnyOfIgnoreCase("0123456789")})),

    .create(.designator, pat_sequence(.{ basic_ident, match.literal(":") })),
    .create(.tag, pat_sequence(.{ match.literal(":"), basic_ident })),

    .create(.identifier, basic_ident),

    .create(.string_literal, string_literal),
    .create(.char_literal, char_literal),

    .create(.@"const", match.word("const")),
    .create(.@"var", match.word("var")),
    .create(.@"if", match.word("if")),
    .create(.@"return", match.word("return")),
    .create(.@"and", match.word("and")),
    .create(.@"or", match.word("or")),
    .create(.xor, match.word("xor")),

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

fn fuzz_tokenizer(_: void, input: []const u8) !void {
    var tokenizer: Tokenizer = .init(input, null);
    while (try tokenizer.next()) |item| {
        _ = item;
    }
}

test "fuzz tokenizer" {
    try std.testing.fuzz({}, fuzz_tokenizer, .{});
}
