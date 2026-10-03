const std = @import("std");
const ptk = @import("ptk");

const ast = @import("ast.zig");
const diagnostics = @import("../diagnostics.zig");
const mode_directive = @import("../mode_directive.zig");
const SourceFile = @import("../SourceFile.zig");

const logger = std.log.scoped(.parser);

pub const Parser = struct {
    tokenizer: Tokenizer,
    diagnostics: *diagnostics.Collection,

    pub fn init(source_code: []const u8, file_name: ?[]const u8, diagnostics_collection: *diagnostics.Collection) Parser {
        return .{
            .tokenizer = .init(source_code, file_name),
            .diagnostics = diagnostics_collection,
        };
    }

    pub fn init_file(source: *const SourceFile, diagnostics_collection: *diagnostics.Collection) Parser {
        return .init(source.text, source.path, diagnostics_collection);
    }

    pub fn parse(parser: *Parser, allocator: std.mem.Allocator) !ParsedFile {
        var arena: std.heap.ArenaAllocator = .init(allocator);
        errdefer arena.deinit();

        var sequence: std.ArrayListUnmanaged(ast.Line) = .empty;

        var line_starts: std.ArrayListUnmanaged(usize) = .empty;
        try line_starts.append(arena.allocator(), 0);
        for (parser.tokenizer.source, 0..) |byte, offset| {
            if (byte == '\n') try line_starts.append(arena.allocator(), offset + 1);
        }
        const source_ref = try arena.allocator().create(ast.SourceSpan.Source);
        source_ref.* = .{
            .name = parser.tokenizer.current_location.source,
            .text = parser.tokenizer.source,
            .line_starts = try line_starts.toOwnedSlice(arena.allocator()),
        };

        var core: Core = .{
            .arena = arena.allocator(),
            .core = .init(&parser.tokenizer),
            .diagnostics = parser.diagnostics,
            .source_ref = source_ref,
        };

        core.accept_file(&sequence) catch |err| {
            if (err == error.OutOfMemory) return err;
            if (core.ok) try core.emit_diag(parser.tokenizer.current_location, .{
                .err_syntax_error = .{ .text = @errorName(err) },
            });
            return error.SyntaxError;
        };
        if (!core.ok)
            return error.SyntaxError;

        var comments: std.ArrayListUnmanaged(ast.Comment) = .empty;
        var comment_tokenizer: Tokenizer = .init(parser.tokenizer.source, parser.tokenizer.current_location.source);
        while (try comment_tokenizer.next()) |token| {
            if (token.type == .comment)
                try comments.append(arena.allocator(), .{ .span = core.token_span(token), .text = token.text });
        }

        return .{
            .arena = arena,
            .file = .{
                .span = .{ .source = source_ref, .start = 0, .end = parser.tokenizer.source.len },
                .sequence = try sequence.toOwnedSlice(arena.allocator()),
                .comments = try comments.toOwnedSlice(arena.allocator()),
                .source = parser.tokenizer.source,
            },
        };
    }

    const Core = struct {
        arena: std.mem.Allocator,
        core: ptk.ParserCore(Tokenizer, .{ .whitespace, .comment }),
        diagnostics: *diagnostics.Collection,
        source_ref: *const ast.SourceSpan.Source,
        lf_is_whitespace: bool = false,
        ok: bool = true,
        local_scope: ast.LocalScope = .{ .id = 0, .parent = null },

        fn token_span(c: *Core, token: Token) ast.SourceSpan {
            const start = @intFromPtr(token.text.ptr) - @intFromPtr(c.source_ref.text.ptr);
            return .{ .source = c.source_ref, .start = start, .end = start + token.text.len };
        }

        fn through_current(c: *Core, start: ast.SourceSpan) ast.SourceSpan {
            return .{ .source = c.source_ref, .start = start.start, .end = c.core.tokenizer.offset };
        }

        fn label(c: *Core, token: Token, name: []const u8, kind: ast.Label.Type) ast.Label {
            if (name[0] != '.') {
                c.local_scope.id += 1;
                c.local_scope.parent = name;
            }
            return .{
                .span = c.token_span(token),
                .identifier = name,
                .type = kind,
                .local_scope = if (name[0] == '.') c.local_scope else null,
            };
        }

        fn ends_local_scope(name: []const u8) bool {
            return mode_directive.from_name(name) != null;
        }

        fn emit_fatal_error(core: *Core, location: ptk.Location, diagnostic: diagnostics.Kind) error{ OutOfMemory, SyntaxError } {
            try core.emit_diag(location, diagnostic);
            return error.SyntaxError;
        }

        fn emit_diag(core: *Core, location: ptk.Location, diagnostic: diagnostics.Kind) !void {
            if (diagnostic.level() == .@"error") core.ok = false;
            try core.diagnostics.emit_diag(location, diagnostic);
        }

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
                .linefeed => return .{ .empty = c.token_span(token) },

                .@"var" => {
                    const name = try c.accept_one(.designator);
                    return .{ .label = c.label(token, name.text[0 .. name.text.len - 1], .@"var") };
                },

                .designator => return .{ .label = c.label(token, token.text[0 .. token.text.len - 1], .code) },

                .@"const" => {
                    const name = try c.accept_one(.identifier);

                    _ = try c.accept_one(.@"=");

                    const value = try c.accept_expression();

                    const span = c.through_current(c.token_span(token));

                    try c.accept_eol_or_eof();

                    return .{
                        .constant = .{
                            .span = span,
                            .identifier = name.text,
                            .value = value,
                        },
                    };
                },

                .@"if" => {
                    // if(X) mnemonic ...
                    var condition = try c.accept_condition();
                    condition.span.start = c.token_span(token).start;
                    const mnemonic = try c.accept_one(.identifier);

                    return .{
                        .instruction = try c.accept_instruction(condition, mnemonic),
                    };
                },

                .@"return" => {
                    // return mnemonic ...
                    const condition: ast.ConditionNode = .{ .span = c.token_span(token), .type = .@"return" };
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

                else => return c.emit_fatal_error(token.location, .{
                    .err_unrecognized_token = .{
                        .token_type = token.type,
                    },
                }),
            }
        }

        fn accept_instruction(core: *Core, condition: ?ast.ConditionNode, mnemonic: Token) !ast.Instruction {
            std.debug.assert(mnemonic.type == .identifier);
            errdefer logger.info("failed to accept {s}", .{mnemonic.text});

            var args: std.ArrayListUnmanaged(ast.Expression) = .empty;
            defer args.deinit(core.arena);

            while (true) {
                const state = core.core.saveState();
                const next = try core.next_token();
                core.core.restoreState(state);
                if (next == null or next.?.type == .linefeed or next.?.type == .effect) break;

                const expr = try core.accept_expression();
                try args.append(core.arena, expr);

                // If accepting a "," fails, we're at the end of
                // the argument list.
                _ = core.accept_one(.@",") catch break;

                // After each argument, we get the option to
                // insert a *single* line break:

                if (core.accept_one(.linefeed)) |_| {} else |_| {}
            }

            var effect: ?ast.EffectNode = null;

            if (core.accept_one(.effect)) |effect_token| {
                const ok = for (allowed_effect_names) |ef| {
                    const name, const eff = ef;
                    if (std.ascii.eqlIgnoreCase(name, effect_token.text)) {
                        effect = .{ .span = core.token_span(effect_token), .type = eff };
                        break true;
                    }
                } else false;

                if (!ok) {
                    return core.emit_fatal_error(effect_token.location, .{
                        .err_unknown_instruction_effect = .{
                            .text = effect_token.text,
                        },
                    });
                }
            } else |_| {}

            const span = core.through_current(core.token_span(mnemonic));
            try core.accept_eol_or_eof();

            if (ends_local_scope(mnemonic.text)) {
                core.local_scope.id += 1;
                core.local_scope.parent = null;
            }

            return .{
                .span = .{ .source = core.source_ref, .start = if (condition) |cond| cond.span.start else span.start, .end = span.end },
                .mnemonic_span = core.token_span(mnemonic),
                .condition = condition,
                .mnemonic = mnemonic.text,
                .effect = effect,

                .arguments = try args.toOwnedSlice(core.arena),
            };
        }

        fn accept_eol_or_eof(core: *Core) !void {
            const context = core.core.tokenizer.current_location;
            _ = core.accept_one(.linefeed) catch |err| switch (err) {
                error.UnexpectedEndOfFile => {},
                error.UnexpectedToken => {
                    const any: Token = blk: {
                        const state = core.core.saveState();
                        defer core.core.restoreState(state);
                        break :blk (try core.next_token()).?;
                    };

                    return core.emit_fatal_error(context, .{
                        .err_unexpected_token_expected_end_of_line_but_found = .{
                            .token_type = any.type,
                        },
                    });
                },
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

        fn BinaryOperatorGroup(
            comptime accept_subexpression: fn (core: *Core) AcceptExprError!ast.Expression,
            comptime allowed_ops: []const ast.BinaryOperator,
        ) type {
            var allowed_tokens_mut: [allowed_ops.len]TokenType = undefined;
            for (&allowed_tokens_mut, allowed_ops) |*tok, op| {
                tok.* = @field(TokenType, @tagName(op));
            }
            const allowed_tokens = allowed_tokens_mut;

            return struct {
                fn accept(core: *Core) AcceptExprError!ast.Expression {
                    var lhs = try accept_subexpression(core);

                    while (core.accept_any(&allowed_tokens)) |bundle| {
                        const which, const token = bundle;

                        const op: ast.BinaryOperator = switch (which) {
                            inline else => |tag| @field(ast.BinaryOperator, @tagName(tag)),
                        };

                        const rhs = try accept_subexpression(core);
                        const lhs_node = try core.move_to_heap(ast.Expression, lhs);
                        const rhs_node = try core.move_to_heap(ast.Expression, rhs);

                        lhs = .{
                            .binary_transform = .{
                                .span = .{ .source = core.source_ref, .start = lhs.span().start, .end = rhs.span().end },
                                .operator_span = core.token_span(token),
                                .operator = op,
                                .lhs = lhs_node,
                                .rhs = rhs_node,
                            },
                        };
                    } else |_| {}

                    return lhs;
                }
            };
        }

        fn accept_expression(core: *Core) AcceptExprError!ast.Expression {
            return try opgroup_0.accept(core);
        }

        const opgroup_0 = BinaryOperatorGroup(
            opgroup_1.accept,
            &.{ .@"and", .@"or", .xor },
        );

        const opgroup_1 = BinaryOperatorGroup(
            opgroup_2.accept,
            &.{ .@"==", .@"!=", .@"<=>", .@"<", .@">", .@"<=", .@">=" },
        );

        const opgroup_2 = BinaryOperatorGroup(
            opgroup_3.accept,
            &.{ .@"+", .@"-", .@"|", .@"^" },
        );

        const opgroup_3 = BinaryOperatorGroup(
            opgroup_4.accept,
            &.{ .@"&", .@"*", .@"/", .@"%" },
        );

        const opgroup_4 = BinaryOperatorGroup(
            accept_unary_expression,
            &.{ .@"<<", .@">>" },
        );

        fn accept_unary_expression(core: *Core) AcceptExprError!ast.Expression {
            const unary_ops: []const TokenType = &.{
                .@"-",
                .@"+",
                .@"~",
                .@"!",
                .@"@",
                .@"*",
                .@"&",
                .@"++",
                .@"--",
            };

            if (core.accept_any(unary_ops)) |bundle| {
                const which, const token = bundle;
                const operator: ast.UnaryOperator = switch (which) {
                    .@"-" => .@"-",
                    .@"+" => .@"+",
                    .@"~" => .@"~",
                    .@"!" => .@"!",
                    .@"@" => .@"@",
                    .@"*" => .@"*",
                    .@"&" => .@"&",
                    .@"++" => .pre_increment,
                    .@"--" => .pre_decrement,
                };
                const value = try core.accept_unary_expression();

                return .{
                    .unary_transform = .{
                        .span = .{ .source = core.source_ref, .start = core.token_span(token).start, .end = value.span().end },
                        .operator_span = core.token_span(token),
                        .operator = operator,
                        .value = try core.move_to_heap(ast.Expression, value),
                    },
                };
            } else |_| {
                return try core.accept_value_expression();
            }
        }

        fn accept_value_expression(core: *Core) AcceptExprError!ast.Expression {
            const which, const token = try core.accept_any(&.{
                .@"$",
                .integer,
                .identifier,
                .char_literal,
                .string_literal,
                .enumerator,
                .@"(",
                .@"[",
            });

            switch (which) {
                .@"$" => return .{ .current_pc = core.token_span(token) },
                .@"(" => {
                    const whitespace = core.push_ignore_whitespace();
                    defer whitespace.pop();

                    const value = try core.accept_expression();

                    const closing = try core.accept_one(.@")");

                    return .{
                        .wrapped = .{
                            .span = .{ .source = core.source_ref, .start = core.token_span(token).start, .end = core.token_span(closing).end },
                            .value = try core.move_to_heap(ast.Expression, value),
                        },
                    };
                },
                .@"[" => {
                    const whitespace = core.push_ignore_whitespace();
                    defer whitespace.pop();

                    var items: std.ArrayListUnmanaged(ast.Expression) = .empty;
                    defer items.deinit(core.arena);
                    var end_offset = core.token_span(token).end;
                    if (core.accept_one(.@"]")) |closing| {
                        end_offset = core.token_span(closing).end;
                    } else |_| {
                        while (true) {
                            try items.append(core.arena, try core.accept_expression());
                            const terminator, const terminator_token = try core.accept_any(&.{ .@",", .@"]" });
                            if (terminator == .@"]") end_offset = core.token_span(terminator_token).end;
                            if (terminator == .@"]") break;
                            if (core.accept_one(.@"]")) |closing| {
                                end_offset = core.token_span(closing).end;
                                break;
                            } else |_| {}
                        }
                    }
                    return .{ .sequence = .{ .span = .{ .source = core.source_ref, .start = core.token_span(token).start, .end = end_offset }, .items = try items.toOwnedSlice(core.arena) } };
                },

                .integer => return .{
                    .integer = .{
                        .span = core.token_span(token),
                        .source_text = token.text,
                        .value = core.parse_int(token.text) catch blk: {
                            try core.emit_diag(token.location, .{
                                .err_integer_overflow_does_not_fit_into_a_i64 = .{
                                    .text = token.text,
                                },
                            });
                            break :blk 0;
                        },
                    },
                },
                .enumerator => return .{
                    .enumerator = .{
                        .span = core.token_span(token),
                        .symbol_name = token.text[1..],
                    },
                },
                .identifier => {
                    // function calls are a really special case in that they don't consume
                    // an identifier expression, but a function name:
                    if (core.accept_one(.@"(")) |_| {
                        // <identifier> "(" is a function call

                        const whitespace = core.push_ignore_whitespace();
                        defer whitespace.pop();

                        const args, const trailing_comma = try core.accept_argv();

                        return .{
                            .function_call = .{
                                .arguments = args,
                                .has_trailing_comma = trailing_comma,
                                .span = core.through_current(core.token_span(token)),
                                .function = token.text,
                            },
                        };
                    } else |_| {}

                    // All other expressions consume regular expressions:
                    var result_expr: ast.Expression = .{
                        .symbol = .{
                            .span = core.token_span(token),
                            .symbol_name = token.text,
                            .local_scope = if (token.text[0] == '.') core.local_scope else null,
                        },
                    };

                    if (core.accept_any(&.{ .@"++", .@"--" })) |tokwrap| {
                        const which_op, const op_tok = tokwrap;
                        const op: ast.UnaryOperator = switch (which_op) {
                            .@"++" => .post_increment,
                            .@"--" => .post_decrement,
                        };
                        const value = try core.move_to_heap(ast.Expression, result_expr);
                        result_expr = .{
                            .unary_transform = .{
                                .span = .{ .source = core.source_ref, .start = value.span().start, .end = core.token_span(op_tok).end },
                                .operator_span = core.token_span(op_tok),
                                .operator = op,
                                .value = value,
                            },
                        };
                    } else |_| {}

                    if (core.accept_one(.@"[")) |open_tok| {
                        // <identifier> "[" is an index operator

                        const whitespace = core.push_ignore_whitespace();
                        defer whitespace.pop();

                        const index = try core.accept_expression();

                        const closing = try core.accept_one(.@"]");

                        const lhs = try core.move_to_heap(ast.Expression, result_expr);
                        const rhs = try core.move_to_heap(ast.Expression, index);
                        result_expr = .{
                            .binary_transform = .{
                                .span = .{ .source = core.source_ref, .start = lhs.span().start, .end = core.token_span(closing).end },
                                .operator_span = core.token_span(open_tok),
                                .operator = .array_index,
                                .lhs = lhs,
                                .rhs = rhs,
                            },
                        };
                    } else |_| {}

                    return result_expr;
                },
                .char_literal => {
                    const string = try core.unescape_string(token.location, core.arena, token.text);

                    if (string.len == 1) {
                        return .{
                            .integer = .{
                                .span = core.token_span(token),
                                .source_text = token.text,
                                .value = string[0],
                            },
                        };
                    }

                    const view = try std.unicode.Utf8View.init(string);

                    var iter = view.iterator();

                    const codepoint: u32 = if (iter.nextCodepoint()) |codepoint|
                        codepoint
                    else blk: {
                        try core.emit_diag(token.location, .err_empty_character_literal_not_allowed);
                        break :blk 0;
                    };

                    if (iter.nextCodepoint() != null) {
                        try core.emit_diag(token.location, .err_character_literal_contains_more_than_one_character);
                    }

                    return .{
                        .integer = .{
                            .span = core.token_span(token),
                            .source_text = token.text,
                            .value = codepoint,
                        },
                    };
                },
                .string_literal => {
                    const string = try core.unescape_string(token.location, core.arena, token.text);

                    return .{
                        .string = .{
                            .span = core.token_span(token),
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
                    .span = .{ .source = core.source_ref, .start = if (maybe_name) |tok| core.token_span(tok).start else value.span().start, .end = value.span().end },
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

        fn unescape_string(core: *Core, location: ast.Location, allocator: std.mem.Allocator, raw: []const u8) ![]u8 {
            std.debug.assert(raw.len >= 2);

            const body = raw[1 .. raw.len - 1];

            var output: std.ArrayListUnmanaged(u8) = try .initCapacity(allocator, body.len);
            defer output.deinit(allocator);

            var i: usize = 0;
            while (i < body.len) : (i += 1) {
                const char = body[i];
                if (char < 0x20 or char == 0x7F) {
                    try core.emit_diag(location, .{
                        .err_invalid_character_in_string_char_literal_0x_x_0_2 = .{
                            .character = char,
                        },
                    });
                } else if (char == '\\') {
                    i += 1;
                    if (i >= body.len) {
                        try core.emit_diag(location, .err_unterminated_escape_sequence);
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
                            i += 3;
                            if (i > body.len) {
                                try core.emit_diag(location, .err_unterminated_escape_sequence);
                                break;
                            }
                            const hex = body[start..i];
                            try output.append(
                                allocator,
                                try std.fmt.parseInt(u8, hex, 16),
                            );
                            i -= 1;
                        },

                        // \u{HHHHH}
                        'u' => {
                            if (i + 1 >= body.len) {
                                try core.emit_diag(location, .err_unterminated_escape_sequence);
                                break;
                            }

                            if (body[i + 1] != '{') {
                                try core.emit_diag(location, .err_invalid_unicode_escape_format);
                                break;
                            }

                            const start = i + 2;
                            while (i < body.len) {
                                if (body[i] == '}')
                                    break;
                                i += 1;
                            }
                            if (i >= body.len) {
                                try core.emit_diag(location, .err_unterminated_escape_sequence);
                                break;
                            }

                            const slice = body[start..i];

                            const codepoint = try std.fmt.parseInt(u21, slice, 16);

                            var buf: [8]u8 = undefined;
                            const len = try std.unicode.utf8Encode(codepoint, &buf);
                            try output.appendSlice(allocator, buf[0..len]);
                        },
                        else => {
                            try core.emit_diag(location, .{
                                .warn_invalid_escape_sequence = .{
                                    .character = body[i],
                                },
                            });
                            try output.append(allocator, char);
                        },
                    }
                } else {
                    try output.append(allocator, char);
                }
            }

            return output.toOwnedSlice(allocator);
        }

        fn parse_int(core: *Core, text: []const u8) !u63 {
            _ = core;
            if (std.mem.startsWith(u8, text, "0b"))
                return try std.fmt.parseInt(u63, text[2..], 2);

            if (std.mem.startsWith(u8, text, "0q"))
                return try std.fmt.parseInt(u63, text[2..], 4);

            if (std.mem.startsWith(u8, text, "0o"))
                return try std.fmt.parseInt(u63, text[2..], 8);

            if (std.mem.startsWith(u8, text, "0x"))
                return try std.fmt.parseInt(u63, text[2..], 16);

            return try std.fmt.parseInt(u63, text, 10);
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
            const open = try core.accept_one(.@"(");

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
                                .span = core.through_current(core.token_span(open)),
                                .type = .{ .c_is = lhs_level },
                            },
                            .z => return .{
                                .span = core.through_current(core.token_span(open)),
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
                .span = core.through_current(core.token_span(open)),
                .type = cond,
            };
        }

        fn AcceptKey(comptime options: []const TokenType) type {
            var names: [options.len][]const u8 = undefined;
            var values: [options.len]u8 = undefined;
            for (options, 0..) |opt, i| {
                names[i] = @tagName(opt);
                values[i] = i;
            }
            return @Enum(u8, .exhaustive, &names, &values);
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

            logger.debug("failed to accept token {s}. expected one of {any}", .{ @tagName(token.type), options });

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
                logger.debug("next_token() => {f}", .{tok});
                return tok;
            }
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
    @"$",
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
    @"++",
    @"--",

    // values
    integer, // 1234, 0xDEAD, 0b1100101, 0q12340123, 0o1234567
    char_literal, // 'x', '\U{1234F}'
    string_literal, //
    identifier, // [A-Za-z0-9_\.\-]+
    designator, // identifier, but ends with ":"
    effect, // identifier, but starts with ":"
    enumerator, // identifier, but starts with "#"
};

const patterns = struct {
    const Pattern = ptk.Pattern(TokenType);

    const match = ptk.matchers;
    const pat_sequence = match.sequenceOf;

    const ident_first_chars = "_.abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
    const ident_suffix_chars = ident_first_chars ++ "0123456789";

    fn basic_ident(str: []const u8) ?usize {
        for (str, 0..) |c, i| {
            if (std.mem.indexOfScalar(u8, if (i > 0) ident_suffix_chars else ident_first_chars, c) == null) {
                return i;
            }
        }
        return str.len;
    }

    fn generic_string_literal(str: []const u8, comptime delim: u8) ?usize {
        if (str.len < 2 or str[0] != delim)
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
        .create(.enumerator, pat_sequence(.{ match.literal("#"), match.takeAnyOf(ident_suffix_chars) })),

        .create(.identifier, basic_ident),

        .create(.@"<=>", match.literal("<=>")),

        .create(.@"==", match.literal("==")),
        .create(.@"!=", match.literal("!=")),
        .create(.@"<=", match.literal("<=")),
        .create(.@">>", match.literal(">>")),
        .create(.@"<<", match.literal("<<")),
        .create(.@">=", match.literal(">=")),

        .create(.@"++", match.literal("++")),
        .create(.@"--", match.literal("--")),

        .create(.@"<", match.literal("<")),
        .create(.@"$", match.literal("$")),
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

fn fuzz_tokenizer_bytes(input: []const u8) !void {
    var tokenizer: Tokenizer = .init(input, null);
    while (try tokenizer.next()) |item| {
        _ = item;
    }
}

fn fuzz_tokenizer(_: void, smith: *std.testing.Smith) !void {
    var input_buffer: [8192]u8 = undefined;
    const input = input_buffer[0..smith.slice(&input_buffer)];
    try fuzz_tokenizer_bytes(input);
}

test "fuzz tokenizer" {
    const corpus = @import("fuzz-corpus").files;
    try std.testing.fuzz({}, fuzz_tokenizer, .{
        .corpus = corpus,
    });
}

fn fuzz_parser_bytes(input: []const u8) !void {
    var diagnostics_collection: diagnostics.Collection = .init(std.testing.allocator);
    defer diagnostics_collection.deinit();

    var parser: Parser = .init(input, null, &diagnostics_collection);

    var parsed = parser.parse(std.testing.allocator) catch {
        // parser errors are oke
        return;
    };
    parsed.deinit();
}

fn fuzz_parser(_: void, smith: *std.testing.Smith) !void {
    var input_buffer: [8192]u8 = undefined;
    const input = input_buffer[0..smith.slice(&input_buffer)];
    try fuzz_parser_bytes(input);
}

test "fuzz parser" {
    const corpus = @import("fuzz-corpus").files;

    try std.testing.fuzz({}, fuzz_parser, .{
        .corpus = corpus,
    });
}

test "parser records syntax diagnostics" {
    const source = "@\n";

    var diagnostics_collection: diagnostics.Collection = .init(std.testing.allocator);
    defer diagnostics_collection.deinit();
    try diagnostics_collection.register_source("test.propan", source);

    var parser: Parser = .init(source, "test.propan", &diagnostics_collection);
    const result = parser.parse(std.testing.allocator);

    try std.testing.expectError(error.SyntaxError, result);
    try std.testing.expect(diagnostics_collection.has_errors());

    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();

    try diagnostics_collection.render(&output.writer, .{});

    const actual = try output.toOwnedSlice();
    defer std.testing.allocator.free(actual);

    try std.testing.expect(std.mem.indexOf(u8, actual, "test.propan:1:1: error: unrecognized token") != null);
}

test "unknown instruction effect is rejected" {
    const source = "NOP :nonesuch\n";

    var diagnostics_collection: diagnostics.Collection = .init(std.testing.allocator);
    defer diagnostics_collection.deinit();
    try diagnostics_collection.register_source("test.propan", source);

    var parser: Parser = .init(source, "test.propan", &diagnostics_collection);
    try std.testing.expectError(error.SyntaxError, parser.parse(std.testing.allocator));

    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();
    try diagnostics_collection.render(&output.writer, .{});
    try std.testing.expect(std.mem.indexOf(u8, output.written(), "unknown instruction effect: :nonesuch") != null);
}

test "recoverable parser diagnostics reject the source" {
    for ([_][]const u8{
        "BYTE ''\n",
        "BYTE 'ab'\n",
        "BYTE 9223372036854775808\n",
    }) |source| {
        var diagnostics_collection: diagnostics.Collection = .init(std.testing.allocator);
        defer diagnostics_collection.deinit();
        try diagnostics_collection.register_source("test.propan", source);

        var parser: Parser = .init(source, "test.propan", &diagnostics_collection);
        try std.testing.expectError(error.SyntaxError, parser.parse(std.testing.allocator));
        try std.testing.expect(diagnostics_collection.has_errors());
    }
}

test "parse conditions (positive)" {
    var diagnostics_collection: diagnostics.Collection = .init(std.testing.allocator);
    defer diagnostics_collection.deinit();

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
        errdefer logger.err("condition parsing failed: expected {}", .{expectation.expected});

        var tok: Tokenizer = .init(expectation.input, null);

        var arena: std.heap.ArenaAllocator = .init(std.testing.allocator);
        errdefer arena.deinit();

        var core: Parser.Core = .{
            .arena = arena.allocator(),
            .core = .init(&tok),
            .diagnostics = &diagnostics_collection,
            .source_ref = &.{ .name = null, .text = expectation.input, .line_starts = &.{0} },
        };

        const cond = try core.accept_condition();

        try std.testing.expectEqualDeep(expectation.expected, cond.type);
    }
}

test "parse effect (positive)" {
    var diagnostics_collection: diagnostics.Collection = .init(std.testing.allocator);
    defer diagnostics_collection.deinit();

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
            errdefer logger.err("condition parsing failed: expected {}", .{expectation.expected});

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
                .diagnostics = &diagnostics_collection,
                .source_ref = &.{ .name = null, .text = input, .line_starts = &.{ 0, input.len - 1 } },
            };

            const identifier = try core.accept_one(.identifier);

            const instr = try core.accept_instruction(null, identifier);

            try std.testing.expectEqualStrings("NOP", instr.mnemonic);

            try std.testing.expectEqual(expectation.expected, instr.effect.?.type);
        }
    }
}

test "AST spans retain source offsets and diagnostic positions" {
    const source =
        \\// heading
        \\outer: if(C) add foo + 1, bar :wc // tail
        \\
    ;
    var collection: diagnostics.Collection = .init(std.testing.allocator);
    defer collection.deinit();
    var parser: Parser = .init(source, "sample.propan", &collection);
    var parsed = try parser.parse(std.testing.allocator);
    defer parsed.deinit();

    const file = parsed.file;
    try std.testing.expectEqual(source.len, file.span.end);
    try std.testing.expectEqualStrings("// heading", file.comments[0].span.source.?.text[file.comments[0].span.start..file.comments[0].span.end]);
    try std.testing.expectEqualStrings("// tail", file.comments[1].span.source.?.text[file.comments[1].span.start..file.comments[1].span.end]);

    const label = for (file.sequence) |line| {
        if (line == .label) break line.label;
    } else unreachable;
    try std.testing.expectEqualStrings("outer:", source[label.span.start..label.span.end]);

    const instruction = for (file.sequence) |line| {
        if (line == .instruction) break line.instruction;
    } else unreachable;
    try std.testing.expectEqualStrings("if(C) add foo + 1, bar :wc", source[instruction.span.start..instruction.span.end]);
    try std.testing.expectEqualStrings("add", source[instruction.mnemonic_span.start..instruction.mnemonic_span.end]);
    try std.testing.expectEqualStrings("if(C)", source[instruction.condition.?.span.start..instruction.condition.?.span.end]);
    try std.testing.expectEqualStrings(":wc", source[instruction.effect.?.span.start..instruction.effect.?.span.end]);
    try std.testing.expectEqual(@as(u32, 2), instruction.location().line);
    try std.testing.expectEqual(@as(u32, 14), instruction.location().column);

    const binary = instruction.arguments[0].binary_transform;
    try std.testing.expectEqualStrings("foo + 1", source[binary.span.start..binary.span.end]);
    try std.testing.expectEqualStrings("+", source[binary.operator_span.start..binary.operator_span.end]);
    try std.testing.expectEqual(@as(u32, 22), binary.operator_span.location().column);
}
