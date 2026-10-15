const std = @import("std");
const lsp = @import("lsp");
const propan = @import("propan");
const analysis = @import("document.zig");
const completion = @import("completion.zig");
const values = @import("values.zig");

pub const std_options: std.Options = .{ .log_level = .warn };

pub fn main(init: std.process.Init) !void {
    var buffer: [4096]u8 = undefined;
    var transport: lsp.Transport.Stdio = .init(&buffer, .stdin(), .stdout());
    var handler: Handler = .{ .allocator = init.gpa };
    defer handler.deinit();
    try lsp.basic_server.run(init.io, init.gpa, &transport.transport, &handler, std.log.err);
}

pub const Handler = struct {
    allocator: std.mem.Allocator,
    files: std.StringHashMapUnmanaged([]u8) = .empty,
    encoding: lsp.offsets.Encoding = .@"utf-16",

    fn deinit(self: *Handler) void {
        var it = self.files.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.*);
        }
        self.files.deinit(self.allocator);
    }

    pub fn initialize(self: *Handler, _: std.mem.Allocator, params: lsp.types.InitializeParams) lsp.types.InitializeResult {
        if (params.capabilities.general) |general| {
            for (general.positionEncodings orelse &.{}) |encoding| {
                self.encoding = switch (encoding) {
                    .@"utf-8" => .@"utf-8",
                    .@"utf-16" => .@"utf-16",
                    .@"utf-32" => .@"utf-32",
                    .custom_value => continue,
                };
                break;
            }
        }
        return .{
            .serverInfo = .{ .name = "propan-lsp", .version = "0.0.0" },
            .capabilities = .{
                .positionEncoding = switch (self.encoding) {
                    .@"utf-8" => .@"utf-8",
                    .@"utf-16" => .@"utf-16",
                    .@"utf-32" => .@"utf-32",
                },
                .textDocumentSync = .{ .text_document_sync_options = .{ .openClose = true, .change = .Incremental } },
                .hoverProvider = .{ .bool = true },
                .definitionProvider = .{ .bool = true },
                .documentFormattingProvider = .{ .bool = true },
                .completionProvider = .{ .triggerCharacters = &.{ ".", ":", "(", ",", "=" } },
                .semanticTokensProvider = .{ .semantic_tokens_options = .{
                    .legend = .{ .tokenTypes = &.{ "propanMnemonic", "propanCodeLabel", "propanVarLabel", "propanConstant", "function", "parameter" }, .tokenModifiers = &.{ "declaration", "readonly" } },
                    .full = .{ .bool = true },
                } },
                .codeLensProvider = .{ .resolveProvider = false },
                .inlayHintProvider = .{ .inlay_hint_options = .{ .resolveProvider = false } },
            },
        };
    }

    pub fn shutdown(_: *Handler, _: std.mem.Allocator, _: void) ?void {
        return null;
    }
    pub fn exit(_: *Handler, _: std.mem.Allocator, _: void) void {}
    pub fn onResponse(_: *Handler, _: std.mem.Allocator, _: lsp.JsonRPCMessage.Response) void {}

    pub fn @"textDocument/didOpen"(self: *Handler, _: std.mem.Allocator, params: lsp.types.TextDocument.DidOpenParams) !void {
        const text = try self.allocator.dupe(u8, params.textDocument.text);
        errdefer self.allocator.free(text);
        const gop = try self.files.getOrPut(self.allocator, params.textDocument.uri);
        if (gop.found_existing) {
            self.allocator.free(gop.value_ptr.*);
        } else {
            errdefer _ = self.files.remove(params.textDocument.uri);
            gop.key_ptr.* = try self.allocator.dupe(u8, params.textDocument.uri);
        }
        gop.value_ptr.* = text;
    }

    pub fn @"textDocument/didChange"(self: *Handler, _: std.mem.Allocator, params: lsp.types.TextDocument.DidChangeParams) !void {
        const current = self.files.getPtr(params.textDocument.uri) orelse return;
        var buffer: std.ArrayList(u8) = .empty;
        defer buffer.deinit(self.allocator);
        try buffer.appendSlice(self.allocator, current.*);
        for (params.contentChanges) |change| switch (change) {
            .text_document_content_change_whole_document => |c| {
                buffer.clearRetainingCapacity();
                try buffer.appendSlice(self.allocator, c.text);
            },
            .text_document_content_change_partial => |c| {
                if (c.range.start.line > c.range.end.line or
                    (c.range.start.line == c.range.end.line and c.range.start.character > c.range.end.character)) return error.InvalidRange;
                const loc = lsp.offsets.rangeToLoc(buffer.items, c.range, self.encoding);
                try buffer.replaceRange(self.allocator, loc.start, loc.end - loc.start, c.text);
            },
        };
        const new_text = try buffer.toOwnedSlice(self.allocator);
        self.allocator.free(current.*);
        current.* = new_text;
    }

    pub fn @"textDocument/didClose"(self: *Handler, _: std.mem.Allocator, params: lsp.types.TextDocument.DidCloseParams) void {
        const entry = self.files.fetchRemove(params.textDocument.uri) orelse return;
        self.allocator.free(entry.key);
        self.allocator.free(entry.value);
    }

    // ponytail: parse/analyze per request; cache per document version if editor latency warrants it.
    fn document(self: *Handler, arena: std.mem.Allocator, uri: []const u8, evaluate: bool) !?analysis.Document {
        const text = self.files.get(uri) orelse return null;
        return try analysis.Document.init(arena, uri, text, evaluate);
    }

    fn range(self: Handler, doc: analysis.Document, span: propan.frontend.ast.SourceSpan) lsp.types.Range {
        return lsp.offsets.locToRange(doc.source.text, .{ .start = span.start, .end = span.end }, self.encoding);
    }

    pub fn @"textDocument/hover"(self: *Handler, arena: std.mem.Allocator, params: lsp.types.Hover.Params) !?lsp.types.Hover {
        var doc = (try self.document(arena, params.textDocument.uri, true)) orelse return null;
        defer doc.deinit();
        const index = lsp.offsets.positionToIndex(doc.source.text, params.position, self.encoding);
        const token_at = doc.at(index);
        if (doc.valueAt(index)) |evaluated| {
            if (token_at == null or (evaluated.value.value == .pointer_expr and token_at.?.kind != .function and token_at.?.kind != .parameter)) {
                return .{
                    .range = self.range(doc, evaluated.span),
                    .contents = .{ .markup_content = .{ .kind = .markdown, .value = try values.hover(arena, "Expression", evaluated.value) } },
                };
            }
        }
        const token = token_at orelse return null;
        const value = switch (token.kind) {
            .mnemonic => try std.fmt.allocPrint(arena, "**{s}**\n\n{s}", .{ token.name, analysis.directiveDocs(token.name) orelse "Propeller 2 instruction. Instruction documentation is coming soon." }),
            .function => if (analysis.function(token.name)) |func| try analysis.functionDocs(arena, token.name, func) else return null,
            .parameter => blk: {
                const param = token.parameter.?;
                break :blk try std.fmt.allocPrint(arena, "**{s}: {t}**\n\n{s}", .{ param.name, param.type, if (param.docs.len > 0) param.docs else "Value passed to this builtin function parameter." });
            },
            .constant => if (doc.constant(token.name)) |v|
                try values.hover(arena, token.name, v)
            else
                try std.fmt.allocPrint(arena, "**{s}**\n\n```text\ntype: unavailable\nusage: unavailable\nvalue: unavailable until the document assembles successfully\n```", .{token.name}),
            .code_label, .var_label => blk: {
                const decl = doc.definition(token.name, token.scope) orelse return null;
                const usage: propan.eval.Value.UsageHint = if (decl.kind == .code_label) .literal else .register;
                const info = if (doc.label(token)) |symbol|
                    try values.hover(arena, token.name, .address(symbol.label, usage))
                else
                    try std.fmt.allocPrint(arena, "**{s}**\n\n```text\ntype: address\nusage: {t}\nvalue: unavailable until the document assembles successfully\n```", .{ token.name, usage });
                break :blk try std.fmt.allocPrint(arena, "{s}\nReferences: {d}", .{ info, doc.references(decl) });
            },
        };
        return .{ .range = self.range(doc, token.span), .contents = .{ .markup_content = .{ .kind = .markdown, .value = value } } };
    }

    pub fn @"textDocument/definition"(self: *Handler, arena: std.mem.Allocator, params: lsp.types.Definition.Params) !lsp.ResultType("textDocument/definition") {
        var doc = (try self.document(arena, params.textDocument.uri, false)) orelse return null;
        defer doc.deinit();
        const token = doc.at(lsp.offsets.positionToIndex(doc.source.text, params.position, self.encoding)) orelse return null;
        if (token.kind != .constant and token.kind != .code_label and token.kind != .var_label) return null;
        const decl = doc.definition(token.name, token.scope) orelse return null;
        return .{ .definition = .{ .location = .{ .uri = params.textDocument.uri, .range = self.range(doc, decl.span) } } };
    }

    pub fn @"textDocument/formatting"(self: *Handler, arena: std.mem.Allocator, params: lsp.ParamsType("textDocument/formatting")) !lsp.ResultType("textDocument/formatting") {
        var doc = (try self.document(arena, params.textDocument.uri, false)) orelse return null;
        defer doc.deinit();
        if (doc.recovered or doc.parsed == null) return null;
        var out: std.Io.Writer.Allocating = .init(arena);
        try propan.frontend.render.pretty_print(&out.writer, doc.parsed.?.file);
        const formatted = try out.toOwnedSlice();
        if (std.mem.eql(u8, formatted, doc.source.text)) return &.{};
        return try arena.dupe(lsp.types.TextEdit, &.{.{
            .range = lsp.offsets.locToRange(doc.source.text, .{ .start = 0, .end = doc.source.text.len }, self.encoding),
            .newText = formatted,
        }});
    }

    pub fn @"textDocument/completion"(self: *Handler, arena: std.mem.Allocator, params: lsp.types.completion.Params) !lsp.ResultType("textDocument/completion") {
        var doc = (try self.document(arena, params.textDocument.uri, false)) orelse return null;
        defer doc.deinit();
        const index = lsp.offsets.positionToIndex(doc.source.text, params.position, self.encoding);
        return .{ .completion_items = try completion.complete(arena, doc, index, self.encoding) };
    }

    pub fn @"textDocument/semanticTokens/full"(self: *Handler, arena: std.mem.Allocator, params: lsp.types.semantic_tokens.Params) !lsp.ResultType("textDocument/semanticTokens/full") {
        var doc = (try self.document(arena, params.textDocument.uri, false)) orelse return null;
        defer doc.deinit();
        std.mem.sort(analysis.Token, doc.tokens.items, {}, struct {
            fn less(_: void, a: analysis.Token, b: analysis.Token) bool {
                return a.span.start < b.span.start;
            }
        }.less);
        var data: std.ArrayList(u32) = .empty;
        var previous: lsp.types.Position = .{ .line = 0, .character = 0 };
        for (doc.tokens.items) |token| {
            // Argument spans include their expressions; only highlight parameter names.
            if (token.kind == .parameter) continue;
            const r = self.range(doc, token.span);
            if (r.start.line != r.end.line or r.end.character <= r.start.character) continue;
            try data.appendSlice(arena, &.{
                r.start.line - previous.line,
                if (r.start.line == previous.line) r.start.character - previous.character else r.start.character,
                r.end.character - r.start.character,
                @intFromEnum(token.kind),
                @as(u32, if (token.declaration) 1 else 0) | @as(u32, if (token.kind == .constant) 2 else 0),
            });
            previous = r.start;
        }
        return .{ .data = data.items };
    }

    pub fn @"textDocument/inlayHint"(self: *Handler, arena: std.mem.Allocator, params: lsp.types.InlayHint.Params) !lsp.ResultType("textDocument/inlayHint") {
        var doc = (try self.document(arena, params.textDocument.uri, true)) orelse return null;
        defer doc.deinit();
        var hints: std.ArrayList(lsp.types.InlayHint) = .empty;
        const Address = struct { hub: ?u32, pc: ?u32 };
        const addresses = try arena.alloc(?Address, doc.source.line_starts.len);
        @memset(addresses, null);
        if (doc.module) |module| {
            for (module.line_data) |line| {
                // Symbols supply label rows, including labels without emitted bytes.
                if (line.kind == .label or line.location.line == 0 or line.location.line > addresses.len) continue;
                if (line.location.source) |source| {
                    if (!std.mem.eql(u8, source, doc.source.path)) continue;
                }
                const index = line.location.line - 1;
                if (addresses[index] != null) continue;
                const pc = blk: {
                    for (module.segments) |segment| {
                        if (line.offset < segment.hub_offset or
                            line.offset >= @as(usize, segment.hub_offset) + segment.data.len) continue;
                        break :blk switch (segment.exec_mode) {
                            .cog, .lut => line.pc,
                            .hub, .data, .regspace => null,
                        };
                    }
                    break :blk null;
                };
                addresses[index] = .{ .hub = line.offset, .pc = pc };
            }
            for (module.symbols) |symbol| {
                const location = symbol.source_location orelse continue;
                if (location.line == 0 or location.line > addresses.len) continue;
                if (location.source) |source| {
                    if (!std.mem.eql(u8, source, doc.source.path)) continue;
                }
                const index = location.line - 1;
                if (addresses[index] != null) continue;
                addresses[index] = .{ .hub = if (symbol.label.hub_address) |hub| hub else null, .pc = switch (symbol.label.local) {
                    .cog, .lut, .regspace => symbol.label.get_local(.pc),
                    .hub, .data => null,
                } };
            }
        }
        for (addresses, 0..) |maybe_address, index| {
            const address = maybe_address orelse Address{ .hub = null, .pc = null };
            const position: lsp.types.Position = .{ .line = @intCast(index), .character = 0 };
            if (index < params.range.start.line or index > params.range.end.line or
                (index == params.range.start.line and params.range.start.character > 0)) continue;
            const hub = if (address.hub) |value| try std.fmt.allocPrint(arena, "${X:0>5}", .{value}) else "      ";
            const local = if (address.pc) |value| try std.fmt.allocPrint(arena, "{X:0>3}", .{value}) else "   ";
            const local_description = if (address.pc) |value|
                try std.fmt.allocPrint(arena, "Local PC (longs): ${X:0>3}", .{value})
            else
                "No local PC in this segment.";
            try hints.append(arena, .{
                .position = position,
                .label = .{ .string = try std.fmt.allocPrint(arena, "{s} | {s} | ", .{ hub, local }) },
                .tooltip = .{ .string = if (maybe_address == null) "No assembled address for this line." else try std.fmt.allocPrint(arena, "Hub byte address: {s}\n{s}", .{
                    if (address.hub != null) hub else "none (register space)", local_description,
                }) },
            });
        }
        return hints.items;
    }

    pub fn @"textDocument/codeLens"(self: *Handler, arena: std.mem.Allocator, params: lsp.types.code_lens.Params) !lsp.ResultType("textDocument/codeLens") {
        var doc = (try self.document(arena, params.textDocument.uri, true)) orelse return null;
        defer doc.deinit();
        var lenses: std.ArrayList(lsp.types.code_lens.Response) = .empty;
        for (doc.tokens.items) |token| {
            if (!token.declaration or (token.kind != .code_label and token.kind != .var_label)) continue;
            try lenses.append(arena, .{
                .range = self.range(doc, token.span),
                .command = .{ .title = try labelInfo(arena, doc, token), .command = "" },
            });
        }
        return lenses.items;
    }
};

fn labelInfo(arena: std.mem.Allocator, doc: analysis.Document, token: analysis.Token) ![]const u8 {
    const refs = doc.references(token);
    if (doc.label(token)) |symbol| {
        const hub = if (symbol.label.hub_address) |addr| try std.fmt.allocPrint(arena, "0x{X}", .{addr}) else "n/a";
        const pc = if (symbol.label.get_local(.pc)) |addr| try std.fmt.allocPrint(arena, "0x{X}", .{addr}) else "n/a";
        return try std.fmt.allocPrint(arena, "Hub: {s} | PC/local: {s} ({t}) | {d} references", .{ hub, pc, symbol.label.local, refs });
    }
    return try std.fmt.allocPrint(arena, "Hub: unavailable | PC/local: unavailable | {d} references", .{refs});
}
