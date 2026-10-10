const std = @import("std");
const lsp = @import("lsp");
const propan = @import("propan");
const analysis = @import("document.zig");
const Item = lsp.types.completion.Item;
const Token = propan.frontend.parser.Token;

const conditions = [_][]const u8{ "if(C)", "if(!C)", "if(Z)", "if(!Z)", "if(C == Z)", "if(C != Z)", "if(C & Z)", "if(C & !Z)", "if(!C & Z)", "if(!C & !Z)", "if(C | Z)", "if(C | !Z)", "if(!C | Z)", "if(!C | !Z)", "if(>=)", "if(<=)", "if(==)", "if(!=)", "if(<)", "if(>)", "return" };

pub fn identifier(c: u8) bool {
    return std.ascii.isAlphanumeric(c) or c == '_' or c == '.';
}

pub fn complete(allocator: std.mem.Allocator, doc: analysis.Document, index: usize, encoding: lsp.offsets.Encoding) ![]const Item {
    const text = doc.source.text;
    var start = index;
    while (start > 0 and identifier(text[start - 1])) start -= 1;
    const effect = start > 0 and text[start - 1] == ':';
    if (effect) start -= 1;
    var end = index;
    while (end < text.len and identifier(text[end])) end += 1;
    const prefix = text[start..index];
    const range = lsp.offsets.locToRange(text, .{ .start = start, .end = end }, encoding);
    var items: std.ArrayList(Item) = .empty;
    var seen: std.StringHashMapUnmanaged(void) = .empty;
    var tokenizer: propan.frontend.parser.Tokenizer = .init(text, doc.source.path);
    var tokens: std.ArrayList(Token) = .empty;
    while (tokenizer.next() catch null) |token| {
        const off = @intFromPtr(token.text.ptr) - @intFromPtr(text.ptr);
        if (off > index) break;
        if ((token.type == .comment or token.type == .string_literal or token.type == .char_literal) and index >= off and (index < off + token.text.len or (token.type == .comment and index == off + token.text.len))) return &.{};
        if (off >= start) break;
        if (token.type != .whitespace and token.type != .comment) try tokens.append(allocator, token);
    }
    // Unterminated strings may be emitted as unexpected characters by the tokenizer.
    if (tokens.items.len > 0 and tokens.items[tokens.items.len - 1].type == .unexpected_character) return &.{};

    const Frame = struct { name: []const u8, begin: usize };
    var frames: std.ArrayList(Frame) = .empty;
    var line_start: usize = 0;
    for (tokens.items, 0..) |token, i| switch (token.type) {
        .@"(" => try frames.append(allocator, .{ .name = if (i > 0 and (tokens.items[i - 1].type == .identifier or tokens.items[i - 1].type == .@"if")) tokens.items[i - 1].text else "", .begin = i }),
        .@")" => {
            _ = frames.pop();
        },
        .linefeed => if (frames.items.len == 0) {
            line_start = i + 1;
        },
        else => {},
    };
    if (frames.items.len > 0) {
        const frame = frames.items[frames.items.len - 1];
        if (std.mem.eql(u8, frame.name, "if")) {
            for ([_][]const u8{ "C", "Z", "!C", "!Z", "C == Z", "C != Z", "C & Z", "C | Z", ">=", "<=", "==", "!=", "<", ">" }) |condition|
                try add(allocator, &items, &seen, prefix, range, condition, .Keyword, "Instruction condition", null);
            return items.items;
        }
        if (analysis.function(frame.name)) |func| {
            var tail = tokens.items.len;
            while (tail > 0 and tokens.items[tail - 1].type == .linefeed) tail -= 1;
            const last = if (tail > 0) tokens.items[tail - 1].type else null;
            if (last == .@"(" or last == .@",") {
                // Named parameters are suggested only at an argument boundary.
                for (func.params) |param| {
                    var used = false;
                    var depth: usize = 0;
                    var positional: usize = 0;
                    var arg_named = false;
                    var arg_has_value = false;
                    const args = tokens.items[frame.begin + 1 ..];
                    for (args, 0..) |t, i| {
                        if (t.type == .@"(" or t.type == .@"[") depth += 1;
                        if ((t.type == .@")" or t.type == .@"]") and depth > 0) depth -= 1;
                        if (depth != 0) continue;
                        if (t.type == .@"=" and i > 0) {
                            arg_named = true;
                            if (std.mem.eql(u8, args[i - 1].text, param.name)) used = true;
                        }
                        if (t.type == .@",") {
                            if (!arg_named and arg_has_value) positional += 1;
                            arg_named = false;
                            arg_has_value = false;
                        } else if (t.type != .linefeed) arg_has_value = true;
                    }
                    for (func.params[0..@min(positional, func.params.len)]) |p| if (std.mem.eql(u8, p.name, param.name)) {
                        used = true;
                    };
                    if (!used) try add(allocator, &items, &seen, prefix, range, param.name, .Field, param.docs, if (end < text.len and text[end] == '=') param.name else try std.fmt.allocPrint(allocator, "{s}=", .{param.name}));
                }
            }
        }
    }

    const line = tokens.items[line_start..];
    for (line) |token| if (token.type == .effect) return &.{};
    // Skip a label and an optional condition before looking for the mnemonic.
    var mnemonic_index: usize = 0;
    if (line.len > 0 and line[0].type == .@"var") {
        if (line.len < 2) return &.{};
        mnemonic_index = 2;
    } else if (line.len > 0 and line[0].type == .designator) mnemonic_index = 1;
    if (mnemonic_index < line.len and line[mnemonic_index].type == .@"return") mnemonic_index += 1;
    if (mnemonic_index < line.len and line[mnemonic_index].type == .@"if") {
        var depth: usize = 0;
        mnemonic_index += 1;
        while (mnemonic_index < line.len) : (mnemonic_index += 1) {
            const t = line[mnemonic_index];
            if (t.type == .@"(") depth += 1;
            if (t.type == .@")") {
                depth -|= 1;
                if (depth == 0) {
                    mnemonic_index += 1;
                    break;
                }
            }
        }
    }
    if (frames.items.len == 0 and mnemonic_index < line.len and line[mnemonic_index].type == .identifier) {
        const mnemonic = line[mnemonic_index].text;
        var after_operands = false;
        if (!effect and prefix.len == 0 and start > 0 and std.ascii.isWhitespace(text[start - 1])) {
            if (doc.parsed) |parsed| {
                const mnemonic_start = @intFromPtr(line[mnemonic_index].text.ptr) - @intFromPtr(text.ptr);
                for (parsed.file.sequence) |entry| {
                    if (entry != .instruction) continue;
                    const instruction = entry.instruction;
                    if (instruction.mnemonic_span.start != mnemonic_start) continue;
                    if (instruction.effect) |existing| {
                        if (existing.span.start < index) continue;
                    }
                    const operands_end = if (instruction.arguments.len > 0)
                        instruction.arguments[instruction.arguments.len - 1].span().end
                    else
                        instruction.mnemonic_span.end;
                    if (operands_end > start) continue;
                    for (propan.stdlib.p2.instructions) |ins| {
                        if (std.ascii.eqlIgnoreCase(ins.mnemonic, mnemonic) and ins.operands.len == instruction.arguments.len) after_operands = true;
                    }
                }
            }
        }
        if (effect or after_operands) {
            inline for (std.meta.fields(propan.frontend.ast.Effect)) |field| {
                const eff: propan.frontend.ast.Effect = @enumFromInt(field.value);
                var allowed = false;
                for (propan.stdlib.p2.instructions) |ins| {
                    if (std.ascii.eqlIgnoreCase(ins.mnemonic, mnemonic) and ins.effects.contains(eff)) allowed = true;
                }
                if (allowed) try add(allocator, &items, &seen, prefix, range, ":" ++ field.name, .Keyword, "Instruction effect", null);
            }
            return items.items;
        }
    }

    const expression = frames.items.len > 0 or (mnemonic_index < line.len and line[mnemonic_index].type != .@"const") or
        (line.len > 0 and line[0].type == .@"const" and std.mem.indexOf(u8, text[@intFromPtr(line[0].text.ptr) - @intFromPtr(text.ptr) .. start], "=") != null);
    if (!expression) {
        if (line.len > 0 and line[0].type == .@"const") return &.{};
        for (analysis.directives) |directive| {
            try add(allocator, &items, &seen, prefix, range, directive.name, .Keyword, "Assembler directive", null);
            if (items.items.len > 0 and std.mem.eql(u8, items.items[items.items.len - 1].label, directive.name)) {
                items.items[items.items.len - 1].documentation = .{ .markup_content = .{ .kind = .markdown, .value = directive.docs } };
            }
        }
        for (propan.stdlib.p2.instructions) |ins| try add(allocator, &items, &seen, prefix, range, ins.mnemonic, .Operator, "Propeller 2 instruction", null);
        for (conditions) |name| try add(allocator, &items, &seen, prefix, range, name, .Keyword, "Instruction condition", null);
        return items.items;
    }
    const scope = doc.scopeAt(index);
    for (doc.tokens.items) |token| {
        if (!token.declaration) continue;
        if (token.scope) |local| {
            if (local.id != scope.id) continue;
        }
        try add(allocator, &items, &seen, prefix, range, token.name, if (token.kind == .constant) .Constant else if (token.kind == .code_label) .Function else .Variable, "Symbol", null);
    }
    for (propan.stdlib.p2.constants.keys()) |name| try add(allocator, &items, &seen, prefix, range, name, .Constant, "Builtin constant", null);
    for (propan.stdlib.common.constants.keys()) |name| try add(allocator, &items, &seen, prefix, range, name, .Constant, "Builtin constant", null);
    for (propan.stdlib.p2.functions.keys()) |name| try add(allocator, &items, &seen, prefix, range, name, .Function, analysis.function(name).?.docs, null);
    for (analysis.intrinsic_names) |name| try add(allocator, &items, &seen, prefix, range, name, .Function, analysis.function(name).?.docs, null);
    return items.items;
}

fn add(allocator: std.mem.Allocator, items: *std.ArrayList(Item), seen: *std.StringHashMapUnmanaged(void), prefix: []const u8, range: lsp.types.Range, name: []const u8, kind: lsp.types.completion.Item.Kind, detail: []const u8, insert: ?[]const u8) !void {
    if (prefix.len > name.len or !std.ascii.eqlIgnoreCase(prefix, name[0..prefix.len])) return;
    const gop = try seen.getOrPut(allocator, name);
    if (gop.found_existing) return;
    try items.append(allocator, .{ .label = name, .kind = kind, .detail = detail, .textEdit = .{ .text_edit = .{ .range = range, .newText = insert orelse name } } });
}
