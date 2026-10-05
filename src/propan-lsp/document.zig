const std = @import("std");
const propan = @import("propan");
const ast = propan.frontend.ast;
const values = @import("values.zig");

pub const Kind = enum(u32) { mnemonic, code_label, var_label, constant, function, parameter };
pub const Token = struct {
    span: ast.SourceSpan,
    name: []const u8,
    kind: Kind,
    scope: ?ast.LocalScope = null,
    parameter: ?propan.sema.Function.Parameter = null,
    declaration: bool = false,
};

pub const Document = struct {
    allocator: std.mem.Allocator,
    source: *propan.SourceFile,
    parsed: ?propan.frontend.ParsedFile = null,
    module: ?propan.Module = null,
    tokens: std.ArrayList(Token) = .empty,
    recovered: bool = false,
    recovery_source: ?*propan.SourceFile = null,

    pub fn init(allocator: std.mem.Allocator, uri: []const u8, text: []const u8, evaluate: bool) !Document {
        const source = try allocator.create(propan.SourceFile);
        source.* = try .init(allocator, uri, text);
        var doc: Document = .{ .allocator = allocator, .source = source };
        errdefer doc.deinit();
        var diagnostics: propan.diagnostics.Collection = .init(allocator);
        defer diagnostics.deinit();
        var parser: propan.frontend.Parser = .init(source, &diagnostics);
        doc.parsed = parser.parse(allocator) catch |err| switch (err) {
            error.OutOfMemory => return err,
            else => null,
        };
        if (doc.parsed == null) {
            // ponytail: blank erroneous physical lines for an editing-time AST;
            // use a recovering Propan parser if multiline recovery needs improvement.
            const scratch = try allocator.dupe(u8, text);
            const recovery_source = try allocator.create(propan.SourceFile);
            recovery_source.* = try .init(allocator, uri, scratch);
            doc.recovery_source = recovery_source;
            doc.recovered = true;
            for (0..32) |_| {
                const last = if (diagnostics.diagnostics.items.len > 0) diagnostics.diagnostics.items[diagnostics.diagnostics.items.len - 1].location else null;
                const loc = last orelse break;
                if (loc.line == 0 or loc.line > source.line_starts.len) break;
                var line_index: usize = loc.line - 1;
                var start = source.line_starts[line_index];
                var end = if (line_index + 1 < source.line_starts.len) source.line_starts[line_index + 1] - 1 else text.len;
                while (std.mem.trim(u8, scratch[start..end], " \r\t").len == 0 and line_index > 0) {
                    line_index -= 1;
                    start = source.line_starts[line_index];
                    end = source.line_starts[line_index + 1] - 1;
                }
                if (std.mem.trim(u8, scratch[start..end], " \r\t").len == 0) break;
                @memset(scratch[start..end], ' ');
                diagnostics.diagnostics.clearRetainingCapacity();
                parser = .init(recovery_source, &diagnostics);
                doc.parsed = parser.parse(allocator) catch |err| switch (err) {
                    error.OutOfMemory => return err,
                    else => null,
                };
                if (doc.parsed != null) break;
            }
            // Source spans retain their source pointer. Its arena lifetime is the request.
        }
        if (doc.parsed) |parsed| {
            if (evaluate and !doc.recovered) {
                doc.module = propan.sema.analyze(allocator, parsed.file, .{}, &diagnostics) catch |err| switch (err) {
                    error.OutOfMemory => return err,
                    else => null,
                };
            }
            // Declare first so forward references and local scopes resolve correctly.
            for (parsed.file.sequence) |line| switch (line) {
                .label => |lbl| try doc.add(.{
                    .span = doc.nameSpan(lbl.span, lbl.identifier),
                    .name = lbl.identifier,
                    .scope = lbl.local_scope,
                    .kind = if (lbl.type == .code) .code_label else .var_label,
                    .declaration = true,
                }),
                .constant => |con| try doc.add(.{ .span = doc.nameSpan(con.span, con.identifier), .name = con.identifier, .kind = .constant, .declaration = true }),
                else => {},
            };
            for (parsed.file.sequence) |line| switch (line) {
                .constant => |con| {
                    try doc.add(.{ .span = .{ .start = con.span.start, .end = con.span.start + 5 }, .name = "const", .kind = .mnemonic });
                    try doc.expression(con.value);
                },
                .label => |lbl| if (lbl.type == .@"var") {
                    try doc.add(.{ .span = .{ .start = lbl.span.start, .end = lbl.span.start + 3 }, .name = "var", .kind = .mnemonic });
                },
                .instruction => |ins| {
                    try doc.add(.{ .span = ins.mnemonic_span, .name = ins.mnemonic, .kind = .mnemonic });
                    for (ins.arguments) |arg| try doc.expression(arg);
                },
                else => {},
            };
        }
        return doc;
    }

    pub fn deinit(doc: *Document) void {
        if (doc.module) |*module| module.deinit();
        if (doc.parsed) |*parsed| parsed.deinit();
        if (doc.recovery_source) |source| source.deinit(doc.allocator);
        doc.tokens.deinit(doc.allocator);
        doc.source.deinit(doc.allocator);
    }

    fn add(doc: *Document, token: Token) !void {
        var normalized = token;
        normalized.span.source = doc.source;
        try doc.tokens.append(doc.allocator, normalized);
    }

    fn nameSpan(doc: Document, span: ast.SourceSpan, name: []const u8) ast.SourceSpan {
        const start = @intFromPtr(name.ptr) - @intFromPtr(span.source.?.text.ptr);
        return .{ .source = doc.source, .start = @intCast(start), .end = @intCast(start + name.len) };
    }

    fn expression(doc: *Document, expr: ast.Expression) error{OutOfMemory}!void {
        switch (expr) {
            .symbol => |sym| {
                const decl = doc.definition(sym.symbol_name, sym.local_scope);
                if (decl == null and doc.constant(sym.symbol_name) == null) return;
                try doc.add(.{ .span = sym.span, .name = sym.symbol_name, .scope = sym.local_scope, .kind = if (decl) |d| d.kind else .constant });
            },
            .wrapped => |v| try doc.expression(v.value.*),
            .unary_transform => |v| try doc.expression(v.value.*),
            .binary_transform => |v| {
                try doc.expression(v.lhs.*);
                try doc.expression(v.rhs.*);
            },
            .sequence => |v| for (v.items) |item| try doc.expression(item),
            .function_call => |call| {
                try doc.add(.{ .span = .{ .start = call.span.start, .end = @intCast(call.span.start + call.function.len) }, .name = call.function, .kind = .function });
                const func = function(call.function);
                var positional: usize = 0;
                for (call.arguments) |arg| {
                    if (func) |f| {
                        const param = if (arg.name) |name| blk: {
                            for (f.params) |p| if (std.mem.eql(u8, p.name, name)) break :blk p;
                            break :blk null;
                        } else if (positional < f.params.len) f.params[positional] else null;
                        if (param) |p| try doc.add(.{ .span = arg.span, .name = p.name, .kind = .parameter, .parameter = p });
                    }
                    if (arg.name == null) positional += 1;
                    try doc.expression(arg.value);
                }
            },
            else => {},
        }
    }

    pub fn definition(doc: Document, name: []const u8, scope: ?ast.LocalScope) ?Token {
        for (doc.tokens.items) |token| {
            if (!token.declaration or !std.mem.eql(u8, token.name, name)) continue;
            if (scopeId(token.scope) == scopeId(scope)) return token;
        }
        return null;
    }

    pub fn at(doc: Document, index: usize) ?Token {
        var found: ?Token = null;
        for (doc.tokens.items) |token| {
            if (index < token.span.start or index >= token.span.end) continue;
            if (found == null or token.span.end - token.span.start <= found.?.span.end - found.?.span.start) found = token;
        }
        return found;
    }

    pub fn scopeAt(doc: Document, index: usize) ast.LocalScope {
        var scope: ast.LocalScope = .{ .id = 0, .parent = null };
        if (doc.parsed) |parsed| for (parsed.file.sequence) |line| switch (line) {
            .label => |lbl| {
                if (lbl.span.start > index) break;
                if (lbl.local_scope) |local| {
                    scope = local;
                } else {
                    scope.id += 1;
                    scope.parent = lbl.identifier;
                }
            },
            .instruction => |ins| {
                if (ins.span.start > index) break;
                if (propan.mode_directive.from_name(ins.mnemonic) != null) {
                    scope.id += 1;
                    scope.parent = null;
                }
            },
            else => {},
        };
        return scope;
    }

    pub fn references(doc: Document, decl: Token) usize {
        var count: usize = 0;
        for (doc.tokens.items) |token| {
            if (!token.declaration and token.kind != .parameter and token.kind != .function and token.kind != .mnemonic and
                std.mem.eql(u8, token.name, decl.name) and scopeId(token.scope) == scopeId(decl.scope)) count += 1;
        }
        return count;
    }

    pub fn label(doc: Document, token: Token) ?propan.Module.Symbol {
        const decl = doc.definition(token.name, token.scope) orelse return null;
        const module = doc.module orelse return null;
        const loc = decl.span.location();
        for (module.symbols) |sym| {
            if (sym.source_location) |s| {
                if (s.line != loc.line) continue;
                if (decl.scope) |scope| {
                    if (scope.parent) |parent| {
                        if (std.mem.startsWith(u8, sym.name, parent) and sym.name.len > parent.len and
                            sym.name[parent.len] == ':' and std.mem.eql(u8, sym.name[parent.len + 1 ..], decl.name[1..])) return sym;
                    } else if (std.mem.eql(u8, sym.name, decl.name)) return sym;
                } else if (std.mem.eql(u8, sym.name, decl.name)) return sym;
            }
        }
        return null;
    }

    pub fn constant(doc: Document, name: []const u8) ?propan.eval.Value {
        if (doc.module) |module| for (module.constants) |con| {
            if (std.mem.eql(u8, con.name, name)) return con.value;
        };
        return propan.stdlib.p2.constants.get(name) orelse propan.stdlib.common.constants.get(name);
    }

    pub fn valueAt(doc: Document, index: usize) ?struct { span: ast.SourceSpan, value: propan.eval.Value } {
        const parsed = doc.parsed orelse return null;
        const module = doc.module orelse return null;
        for (parsed.file.sequence) |entry| switch (entry) {
            .constant => |con| {
                const span = con.value.span();
                if (index >= span.start and index < span.end) {
                    return .{ .span = span, .value = doc.constant(con.identifier) orelse continue };
                }
            },
            .instruction => |instruction| {
                for (instruction.arguments, 0..) |argument, argument_index| {
                    const span = argument.span();
                    if (index < span.start or index >= span.end) continue;
                    const location = instruction.span.location();
                    for (module.line_data) |line| {
                        if (line.location.line != location.line or line.location.column != location.column or argument_index >= line.operands.len) continue;
                        return .{ .span = span, .value = line.operands[argument_index].value };
                    }
                }
            },
            else => {},
        };
        return null;
    }
};

pub fn scopeId(scope: ?ast.LocalScope) ?usize {
    return if (scope) |s| s.id else null;
}

pub const Function = struct { docs: []const u8, params: []const propan.sema.Function.Parameter };

pub const directives = [_]struct { name: []const u8, docs: []const u8 }{
    .{ .name = ".cogexec", .docs = "`.cogexec [hub_origin[, local_start]]`\n\nStart a segment executed from cog RAM. Hub addresses count bytes; the local PC counts longs and defaults to `$000`. Omit the hub origin to continue at the current hub address. Starts a new local-label scope." },
    .{ .name = ".lutexec", .docs = "`.lutexec [hub_origin[, local_start]]`\n\nStart a segment executed from LUT RAM. Hub addresses count bytes; the local PC counts longs and defaults to `$200`. Omit the hub origin to continue at the current hub address. Starts a new local-label scope." },
    .{ .name = ".hubexec", .docs = "`.hubexec [hub_origin]`\n\nStart a segment executed directly from hub RAM. The execution PC is the hub byte address. Omit the origin to continue at the current hub address. Starts a new local-label scope." },
    .{ .name = ".regspace", .docs = "`.regspace [hub_origin[, local_start]]`\n\nStart an uninitialized cog-register allocation segment. Use `RES` to reserve registers; labels have local register indices but no hub storage address. Code and data cannot be emitted in this mode. Starts a new local-label scope." },
    .{ .name = ".data", .docs = "`.data [hub_origin]`\n\nStart a hub data segment with no execution PC. Use `BYTE`, `WORD`, `LONG`, or `FILE` to emit data; machine instructions are not permitted. Omit the origin to continue at the current hub address. Starts a new local-label scope." },
    .{ .name = ".org", .docs = "`.org target`\n\nMove the current address forward: a register/long PC in cog, LUT, or register space, or a byte address in hub execution mode. Moving backward is an error. Does not start a new segment; not valid in `.data` mode." },
    .{ .name = ".align", .docs = "`.align alignment`\n\nAdvance to the next byte boundary divisible by `alignment`, which must be a nonzero power of two. Place this before a label when the label must name the aligned value. The alignment expression must not depend on labels." },
    .{ .name = ".pack", .docs = "`.pack off|byte|word|long`\n\nControl alignment of subsequent data and instruction lines. `off` restores natural alignment; `byte`, `word`, and `long` use 1, 2, and 4 bytes. Values on a single data line remain contiguous. An explicit `.align` still applies." },
    .{ .name = ".pic", .docs = "`.pic default|prefer|avoid|force`\n\nControl branch instructions with both absolute and relative encodings. `default` uses assembler options; `prefer` favors relative addressing; `avoid` favors absolute addressing; `force` requires relative addressing. Remains active across segment changes and emits no bytes. `nrel(...)` requests absolute addressing and is incompatible with `force`." },
    .{ .name = ".assert", .docs = "`.assert expression[, \"message\"]`\n\nRequire an integer expression to be nonzero. A zero value reports an assembly error, optionally with the supplied message. Emits no data." },
    .{ .name = ".fit", .docs = "`.fit limit[, \"message\"]`\n\nCheck that the current cog/LUT/register PC or hub/data byte address is at most `limit`. The limit may be a compatible address label. Emits no data and does not replace execution-space bounds checks." },
    .{ .name = ".import", .docs = "`.import \"path\"` or `.import once`\n\nInsert another Propan source file. Relative paths are searched beside the importing file, then in command-line include directories. Put `.import once` in the imported file to include it only on its first visit. Imports expand before conditional compilation. The language server currently analyzes open documents independently and does not load imported files." },
    .{ .name = ".if", .docs = "`.if expression`\n\nBegin conditional assembly. Zero is false; any nonzero integer is true. Conditions can use builtins and constants declared earlier in active code, but not labels, `$`, or forward references. Requires a matching `.endif`. Inactive lines must still have valid syntax." },
    .{ .name = ".elif", .docs = "`.elif expression`\n\nSelect another branch of the current `.if` block. The condition is evaluated only if the parent is active and no earlier branch matched. Uses the same expression rules as `.if`. Must precede `.else`." },
    .{ .name = ".else", .docs = "`.else`\n\nSelect the remaining branch of the current `.if` block when no earlier branch matched. Takes no arguments; each block permits at most one `.else`." },
    .{ .name = ".endif", .docs = "`.endif`\n\nEnd the current conditional-assembly `.if` block. Takes no arguments." },
    .{ .name = "BYTE", .docs = "`BYTE value[, value...]`\n\nEmit 8-bit data. Integers, strings, and sequences are expanded into byte elements. Uses natural byte alignment unless `.pack` overrides it. Does not emit machine instructions." },
    .{ .name = "WORD", .docs = "`WORD value[, value...]`\n\nEmit 16-bit data in little-endian order. Integers, strings, and sequences are expanded into word elements. Uses natural 2-byte alignment unless `.pack` overrides it." },
    .{ .name = "LONG", .docs = "`LONG value[, value...]`\n\nEmit 32-bit data in little-endian order. Integers, strings, and sequences are expanded into long elements. Uses natural 4-byte alignment unless `.pack` overrides it." },
    .{ .name = "RES", .docs = "`RES count`\n\nReserve `count` uninitialized long registers in cog or register-space mode without emitting hub bytes. Code and data cannot be emitted after a reservation until a new segment begins." },
    .{ .name = "FILE", .docs = "`FILE \"path\"`\n\nInsert the raw bytes of a binary file into the current segment. Takes one string path. Not permitted in register space or after `RES`. The language server currently does not load external file payloads." },
    .{ .name = "const", .docs = "`const name = expression`\n\nDefine an immutable compile-time value. Constants outside conditional-compilation expressions may refer to other constants in either source order, without dependency cycles. Values needed for layout must not depend on labels." },
    .{ .name = "var", .docs = "`var name: [data directive]`\n\nDeclare a variable label at the current address. A reference has register-address usage; a code label has literal-address usage. Follow with data or `RES` to allocate storage. Local names beginning with `.` belong to the enclosing global-label scope." },
};

pub fn directiveDocs(name: []const u8) ?[]const u8 {
    for (directives) |directive| {
        if (std.ascii.eqlIgnoreCase(name, directive.name)) return directive.docs;
    }
    return null;
}

pub const intrinsic_names = [_][]const u8{ "aug", "nrel", "hubaddr", "cogaddr", "lutaddr", "localaddr", "byteoffset", "wordoffset" };

pub fn function(name: []const u8) ?Function {
    if (propan.stdlib.p2.functions.get(name)) |f| return .{ .docs = f.docs, .params = f.params };
    inline for (intrinsic_names) |key| {
        if (std.mem.eql(u8, key, name)) {
            const f: propan.sema.Function = @field(propan.sema.Function, key);
            return .{ .docs = f.get_docs(), .params = f.get_parameters() };
        }
    }
    return null;
}

pub fn functionDocs(allocator: std.mem.Allocator, name: []const u8, f: Function) ![]const u8 {
    var out: std.Io.Writer.Allocating = .init(allocator);
    try out.writer.print("**{s}**\n\n{s}\n\n", .{ name, f.docs });
    for (f.params) |p| {
        try out.writer.print("- `{s}: {t}`: {s}", .{ p.name, p.type, p.docs });
        try out.writer.writeByte('\n');
        if (p.default_value) |v| {
            try out.writer.writeAll("\nDefault:\n\n```text\n");
            try values.write(&out.writer, v);
            try out.writer.writeAll("\n```\n\n");
        }
    }
    return try out.toOwnedSlice();
}
