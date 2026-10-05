const std = @import("std");

const stdlib = @import("stdlib/stdlib.zig");
const eval = @import("stdlib/eval.zig");
const frontend = @import("frontend.zig");
const ast = frontend.ast;
const diagnostics = @import("diagnostics.zig");
const SourceFile = @import("SourceFile.zig");
const mode_directive = @import("mode_directive.zig");

const logger = std.log.scoped(.sema);

const Value = eval.Value;

const Module = @import("Module.zig");
const Segment = Module.Segment;

const TaggedAddress = eval.TaggedAddress;
const Segment_ID = eval.Segment_ID;

const PicMode = enum { default, prefer, avoid, force };

pub const CalldAmbiguousEncoding = enum { flexspin, general, address };

const PA: eval.Register = @enumFromInt(0x1F6);
const PB: eval.Register = @enumFromInt(0x1F7);
const PTRA: eval.Register = @enumFromInt(0x1F8);
const PTRB: eval.Register = @enumFromInt(0x1F9);

pub const AnalyzeOptions = struct {
    io: ?std.Io = null,
    fill_byte: u8 = 0x00,
    rebind_scopes: bool = false,
    blank_pointer_expr: enum {
        as_ptr_epxr,
        as_register,
    } = .as_ptr_epxr,

    flip_augs_on_pcrel: bool = true,

    /// Selects the encoding when CALLD with PA/PB/PTRA/PTRB and a literal
    /// source fits both the general and the 20-bit address forms.
    calld_ambiguous_encoding: CalldAmbiguousEncoding = .flexspin,

    /// If this is `true`, the emitted code will use relative addressing for
    /// `JMP #{/}A` and friends if `A` is a label value, and addresses an address
    /// that jumps from hub exec mode to a hub exec mode address which is *not* in the
    /// same segment.
    use_label_relative_hub_to_hub_jmp: bool = true,

    /// If this is `true`, the emitted code will use relative addressing for
    /// `JMP #{/}A` and friends if `A` is a non-label value, and addresses
    /// an address in the same execution domain (cog and LUT share a domain).
    /// TODO: Make this more fine granular for exec modes and cross-segment.
    use_relative_jmp_for_same_mode_nonlabel_address: bool = true,
};

pub fn analyze(allocator: std.mem.Allocator, file: ast.File, options: AnalyzeOptions, diagnostics_collection: *diagnostics.Collection) !Module {
    var filter_arena: std.heap.ArenaAllocator = .init(allocator);
    defer filter_arena.deinit();

    var active_file = file;
    var condition_references: std.StringHashMapUnmanaged(void) = .empty;
    var condition_values: std.StringHashMapUnmanaged(Value) = .empty;
    // Rebind local scopes after imports have been spliced into one sequence.
    if (has_conditional_directives(file) or options.rebind_scopes) {
        var probe: Analyzer = try .init(allocator, file, options, diagnostics_collection);
        defer probe.deinit();
        try probe.load_constants(stdlib.common.constants);
        try probe.load_constants(stdlib.p2.constants);
        try probe.load_functions(stdlib.p2.functions);

        var filter: ConditionalFilter = .{ .allocator = filter_arena.allocator(), .probe = &probe };
        active_file = try filter.run(file);
        if (!probe.ok) return error.SemanticErrors;
        condition_references = filter.referenced;
        var references = condition_references.iterator();
        while (references.next()) |entry| {
            const value = probe.symbols.get(entry.key_ptr.*).?.value orelse continue;
            // Invalid constant types still go through ordinary constant validation.
            if (value.value == .address or value.value == .pointer_expr) continue;
            try condition_values.put(filter_arena.allocator(), entry.key_ptr.*, try copy_value(filter_arena.allocator(), value));
        }
    }

    var analyzer: Analyzer = try .init(allocator, active_file, options, diagnostics_collection);
    defer analyzer.deinit();

    errdefer dump_analyzer(&analyzer);

    // Prepare
    try analyzer.load_constants(stdlib.common.constants);
    try analyzer.load_constants(stdlib.p2.constants);

    try analyzer.load_functions(stdlib.p2.functions);

    try analyzer.load_instructions(stdlib.p2.instructions);

    // Validate
    try analyzer.declare_symbols();
    var referenced = condition_references.iterator();
    while (referenced.next()) |entry| {
        const name = entry.key_ptr.*;
        if (analyzer.symbols.getPtr(name)) |sym| {
            sym.referenced = true;
            if (condition_values.get(name)) |value| {
                sym.value = value;
                sym.constant_state = .evaluated;
            }
        }
    }
    try analyzer.validate_symbol_refs();

    // Lay Out
    try analyzer.prepare_instruction_stream();
    try analyzer.select_instruction_mnemonic();

    if (!analyzer.ok)
        return error.SemanticErrors;

    try analyzer.assign_locations();

    if (!analyzer.ok)
        return error.SemanticErrors;

    try analyzer.check_symbols(.labels);

    // beyond  this check, all symbols are defined
    // and expression evaluation can happen:
    if (!analyzer.ok)
        return error.SemanticErrors;

    try analyzer.evaluate_constant_values();

    if (!analyzer.ok)
        return error.SemanticErrors;

    try analyzer.check_symbols(.constants);

    try analyzer.evaluate_instruction_arguments();

    if (!analyzer.ok)
        return error.SemanticErrors;

    try analyzer.select_instruction_encoding();

    try analyzer.evaluate_asserts();

    if (!analyzer.ok)
        return error.SemanticErrors;

    // Translate

    var output_arena: std.heap.ArenaAllocator = .init(allocator);
    errdefer output_arena.deinit();

    const output_allocator = output_arena.allocator();

    const segments = try analyzer.emit_code(output_allocator);

    if (!analyzer.ok)
        return error.SemanticErrors;

    var symbols: std.ArrayList(Module.Symbol) = .empty;
    defer symbols.deinit(output_allocator);

    var constants: std.ArrayList(Module.Constant) = .empty;
    defer constants.deinit(output_allocator);

    var sources: std.ArrayList(Module.Source) = .empty;
    defer sources.deinit(output_allocator);
    for (diagnostics_collection.sources.values()) |source| {
        try sources.append(output_allocator, .{
            .path = try output_allocator.dupe(u8, source.path),
            .text = try output_allocator.dupe(u8, source.text),
        });
    }

    for (analyzer.symbols.values()) |sym| {
        const stype: Module.Symbol.Type = switch (sym.type) {
            .undefined => continue,
            .code => .code,
            .data => .data,
            .constant => continue,
            .builtin => continue,
        };

        try symbols.append(output_allocator, .{
            .name = try output_allocator.dupe(u8, sym.name),
            .label = sym.offset.?,
            .type = stype,
            .source_location = try copy_location(output_allocator, sym.location().?),
        });
    }

    for (analyzer.file.sequence) |seq| {
        if (seq != .constant)
            continue;

        const con = seq.constant;
        const sym = analyzer.symbols.get(con.identifier) orelse unreachable;
        std.debug.assert(sym.type == .constant);

        try constants.append(output_allocator, .{
            .name = try output_allocator.dupe(u8, con.identifier),
            .value = try copy_value(output_allocator, sym.value.?),
            .location = try copy_location(output_allocator, con.span.location()),
        });
    }

    for (analyzer.line_data.items) |*line| {
        line.location = try copy_location(output_allocator, line.location);
        if (line.mnemonic) |name| line.mnemonic = try output_allocator.dupe(u8, name);
        for (@constCast(line.operands)) |*operand| operand.value = try copy_value(output_allocator, operand.value);
    }

    return .{
        .arena = output_arena,
        .segments = segments,
        .regspace_segments = try output_allocator.dupe(u32, analyzer.regspace_segments.items),
        .line_data = try analyzer.line_data.toOwnedSlice(output_allocator),
        .symbols = try symbols.toOwnedSlice(output_allocator),
        .constants = try constants.toOwnedSlice(output_allocator),
        .sources = try sources.toOwnedSlice(output_allocator),
    };
}

fn has_conditional_directives(file: ast.File) bool {
    for (file.sequence) |line| {
        if (line != .instruction) continue;
        const name = line.instruction.mnemonic;
        if (std.ascii.eqlIgnoreCase(name, ".if") or
            std.ascii.eqlIgnoreCase(name, ".elif") or
            std.ascii.eqlIgnoreCase(name, ".else") or
            std.ascii.eqlIgnoreCase(name, ".endif")) return true;
    }
    return false;
}

fn copy_location(allocator: std.mem.Allocator, location: ast.Location) !ast.Location {
    var copied = location;
    if (location.source) |source| copied.source = try allocator.dupe(u8, source);
    return copied;
}

fn copy_value(allocator: std.mem.Allocator, value: Value) !Value {
    var copied = value;
    switch (value.value) {
        .string => |text| copied.value = .{ .string = try allocator.dupe(u8, text) },
        .sequence => |items| copied.value = .{ .sequence = try allocator.dupe(i64, items) },
        .enumerator => |text| copied.value = .{ .enumerator = try allocator.dupe(u8, text) },
        .int, .address, .register, .pointer_expr => {},
    }
    return copied;
}

fn number_style(expr: ast.Expression) Module.LineData.NumberStyle {
    return switch (expr) {
        .integer => |literal| if (std.mem.startsWith(u8, literal.source_text, "0x") or std.mem.startsWith(u8, literal.source_text, "0X") or std.mem.startsWith(u8, literal.source_text, "$")) .hex else .decimal,
        .wrapped => |inner| number_style(inner.value.*),
        else => .hex,
    };
}

const ConditionalFilter = struct {
    allocator: std.mem.Allocator,
    probe: *Analyzer,
    constants: std.StringHashMapUnmanaged(struct { declaration: ast.Constant, mode: eval.ExecMode }) = .empty,
    resolving: std.StringHashMapUnmanaged(void) = .empty,
    referenced: std.StringHashMapUnmanaged(void) = .empty,
    frames: std.ArrayListUnmanaged(Frame) = .empty,
    active: bool = true,
    mode: eval.ExecMode = .cog,
    scope: ast.LocalScope = .{ .id = 0, .parent = null },

    const Frame = struct {
        location: ast.Location,
        parent_active: bool,
        branch_taken: bool,
        else_seen: bool = false,
        active: bool,
    };

    const Directive = enum { none, if_, elif, else_, endif };

    fn run(filter: *ConditionalFilter, file: ast.File) !ast.File {
        var output: std.ArrayListUnmanaged(ast.Line) = .empty;
        for (file.sequence) |line| {
            if (line == .instruction) {
                const instr = line.instruction;
                const kind: Directive = if (std.ascii.eqlIgnoreCase(instr.mnemonic, ".if"))
                    .if_
                else if (std.ascii.eqlIgnoreCase(instr.mnemonic, ".elif"))
                    .elif
                else if (std.ascii.eqlIgnoreCase(instr.mnemonic, ".else"))
                    .else_
                else if (std.ascii.eqlIgnoreCase(instr.mnemonic, ".endif"))
                    .endif
                else
                    .none;
                if (kind != .none) {
                    try filter.directive(instr, kind);
                    continue;
                }
            }

            if (!filter.active) continue;
            if (line == .constant) {
                const con = line.constant;
                const gop = try filter.constants.getOrPut(filter.allocator, con.identifier);
                if (!gop.found_existing) gop.value_ptr.* = .{ .declaration = con, .mode = filter.mode };
            }
            try output.append(filter.allocator, try filter.rebind_line(line));
        }
        for (filter.frames.items) |frame|
            try filter.probe.emit_diag(frame.location, .err_unterminated_conditional_if);
        return .{ .span = file.span, .sequence = try output.toOwnedSlice(filter.allocator), .comments = file.comments, .source = file.source };
    }

    fn directive(filter: *ConditionalFilter, instr: ast.Instruction, kind: Directive) !void {
        const expected: usize = if (kind == .if_ or kind == .elif) 1 else 0;
        const valid_args = instr.arguments.len == expected;
        if (!valid_args) try filter.probe.emit_diag(instr.location(), .{ .err_argument_count_mismatch = .{
            .subject = instr.mnemonic,
            .min = expected,
            .max = expected,
            .found = instr.arguments.len,
        } });
        if (instr.condition != null or instr.effect != null)
            try filter.probe.emit_diag(instr.location(), .err_conditional_directive_requires_plain_line);

        switch (kind) {
            .none => unreachable,
            .if_ => {
                const enabled = if (filter.active and valid_args) try filter.eval_condition(instr) else false;
                try filter.frames.append(filter.allocator, .{
                    .location = instr.location(),
                    .parent_active = filter.active,
                    .branch_taken = enabled,
                    .active = enabled,
                });
                filter.active = enabled;
            },
            .elif, .else_, .endif => {
                if (filter.frames.items.len == 0) {
                    try filter.probe.emit_diag(instr.location(), .{ .err_conditional_directive_without_if = .{ .text = instr.mnemonic } });
                    return;
                }
                const frame = &filter.frames.items[filter.frames.items.len - 1];
                switch (kind) {
                    .elif => {
                        if (frame.else_seen) {
                            try filter.probe.emit_diag(instr.location(), .err_conditional_branch_after_else);
                            frame.active = false;
                        } else {
                            frame.active = if (frame.parent_active and !frame.branch_taken and valid_args)
                                try filter.eval_condition(instr)
                            else
                                false;
                            frame.branch_taken = frame.branch_taken or frame.active;
                        }
                        filter.active = frame.active;
                    },
                    .else_ => {
                        if (frame.else_seen) {
                            try filter.probe.emit_diag(instr.location(), .err_conditional_branch_after_else);
                            frame.active = false;
                        } else {
                            frame.else_seen = true;
                            frame.active = frame.parent_active and !frame.branch_taken;
                            frame.branch_taken = true;
                        }
                        filter.active = frame.active;
                    },
                    .endif => {
                        filter.active = frame.parent_active;
                        _ = filter.frames.pop();
                    },
                    else => unreachable,
                }
            },
        }
    }

    fn eval_condition(filter: *ConditionalFilter, instr: ast.Instruction) !bool {
        filter.prepare_refs(instr.arguments[0]) catch |err| {
            try filter.probe.emit_eval_error(instr.location(), .expression, err);
            return false;
        };
        const value = filter.probe.evaluate_root_expr(instr.arguments[0], .{ .exec_mode = filter.mode }) catch |err| {
            try filter.probe.emit_eval_error(instr.location(), .expression, err);
            return false;
        };
        if (value.value != .int) {
            try filter.probe.emit_diag(instr.location(), .{ .err_expected_value_type = .{
                .subject = instr.mnemonic,
                .expected = .int,
                .actual = value.value,
            } });
            return false;
        }
        return value.value.int != 0;
    }

    fn resolve_constant(filter: *ConditionalFilter, name: []const u8) Analyzer.EvalError!void {
        const declared = filter.constants.get(name) orelse return error.UndefinedSymbol;
        const con = declared.declaration;
        const sym = try filter.probe.get_symbol_info(name);
        if (sym.type == .builtin) return error.InvalidArg;
        if (sym.value != null) return;
        if (filter.resolving.contains(name)) return error.UndefinedSymbol;
        try filter.resolving.put(filter.allocator, name, {});
        defer _ = filter.resolving.remove(name);
        sym.type = .{ .constant = con.span.location() };
        try filter.prepare_refs(con.value);
        const value = try filter.probe.evaluate_root_expr(con.value, .{ .exec_mode = declared.mode });
        (try filter.probe.get_symbol_info(name)).value = value;
    }

    fn prepare_refs(filter: *ConditionalFilter, expr: ast.Expression) Analyzer.EvalError!void {
        switch (expr) {
            .current_pc => return error.InvalidArg,
            .symbol => |ref| {
                if (ref.symbol_name[0] == '.') return error.UndefinedSymbol;
                if (filter.constants.contains(ref.symbol_name)) {
                    try filter.referenced.put(filter.allocator, ref.symbol_name, {});
                    try filter.resolve_constant(ref.symbol_name);
                } else if (filter.probe.symbols.get(ref.symbol_name)) |sym| {
                    if (sym.type != .builtin) return error.UndefinedSymbol;
                } else return error.UndefinedSymbol;
            },
            .wrapped => |inner| try filter.prepare_refs(inner.value.*),
            .sequence => |seq| for (seq.items) |item| try filter.prepare_refs(item),
            .unary_transform => |op| try filter.prepare_refs(op.value.*),
            .binary_transform => |op| {
                try filter.prepare_refs(op.lhs.*);
                try filter.prepare_refs(op.rhs.*);
            },
            .function_call => |call| for (call.arguments) |arg| try filter.prepare_refs(arg.value),
            .integer, .string, .enumerator => {},
        }
    }

    fn rebind_line(filter: *ConditionalFilter, line: ast.Line) !ast.Line {
        switch (line) {
            .label => |original| {
                var label = original;
                if (label.identifier[0] != '.') {
                    filter.scope.id += 1;
                    filter.scope.parent = label.identifier;
                }
                label.local_scope = if (label.identifier[0] == '.') filter.scope else null;
                return .{ .label = label };
            },
            .constant => |original| {
                var con = original;
                con.value = try filter.rebind_expr(con.value);
                return .{ .constant = con };
            },
            .instruction => |original| {
                var instr = original;
                const args = try filter.allocator.alloc(ast.Expression, instr.arguments.len);
                for (instr.arguments, args) |arg, *dest| dest.* = try filter.rebind_expr(arg);
                instr.arguments = args;
                if (mode_directive.from_name(instr.mnemonic)) |mode| {
                    filter.mode = mode;
                    filter.scope.id += 1;
                    filter.scope.parent = null;
                }
                return .{ .instruction = instr };
            },
            .empty => |span| return .{ .empty = span },
        }
    }

    fn rebind_expr(filter: *ConditionalFilter, original: ast.Expression) std.mem.Allocator.Error!ast.Expression {
        switch (original) {
            .symbol => |old| {
                var ref = old;
                if (ref.symbol_name[0] == '.') ref.local_scope = filter.scope;
                return .{ .symbol = ref };
            },
            .wrapped => |old| {
                const inner = try filter.allocator.create(ast.Expression);
                inner.* = try filter.rebind_expr(old.value.*);
                return .{ .wrapped = .{ .span = old.span, .value = inner } };
            },
            .sequence => |old| {
                var seq = old;
                const items = try filter.allocator.alloc(ast.Expression, seq.items.len);
                for (seq.items, items) |item, *dest| dest.* = try filter.rebind_expr(item);
                seq.items = items;
                return .{ .sequence = seq };
            },
            .unary_transform => |old| {
                var op = old;
                op.value = try filter.allocator.create(ast.Expression);
                op.value.* = try filter.rebind_expr(old.value.*);
                return .{ .unary_transform = op };
            },
            .binary_transform => |old| {
                var op = old;
                op.lhs = try filter.allocator.create(ast.Expression);
                op.rhs = try filter.allocator.create(ast.Expression);
                op.lhs.* = try filter.rebind_expr(old.lhs.*);
                op.rhs.* = try filter.rebind_expr(old.rhs.*);
                return .{ .binary_transform = op };
            },
            .function_call => |old| {
                var call = old;
                const args = try filter.allocator.alloc(ast.FunctionInvocation.Argument, call.arguments.len);
                for (call.arguments, args) |arg, *dest| {
                    dest.* = arg;
                    dest.value = try filter.rebind_expr(arg.value);
                }
                call.arguments = args;
                return .{ .function_call = call };
            },
            else => return original,
        }
    }
};

fn dump_analyzer(analyzer: *Analyzer) void {
    logger.info("symbols:", .{});
    for (analyzer.symbols.values()) |sym| {
        if (sym.type == .builtin)
            continue;
        logger.info("  {s}: {t} => offset={?f}, value={?f}, referenced={}", .{
            sym.name,
            sym.type,
            sym.offset,
            sym.value,
            sym.referenced,
        });
    }

    logger.info("map file:", .{});
    for (analyzer.instructions) |instr| {
        logger.info("  {?f}: {s} (+{?} bytes)", .{
            instr.start_addr,
            instr.ast_node.mnemonic,
            instr.byte_size,
        });
    }

    logger.info("map file:", .{});
    for (analyzer.instructions) |instr| {
        logger.info("  {?f}: {s}", .{
            instr.start_addr,
            instr.ast_node.mnemonic,
        });
        for (instr.arguments, 0..) |arg, i| {
            logger.info("    [{}] = {f}", .{ i, arg });
        }
    }
}

pub const Constant = struct {
    value: Constant.Value,

    pub const Value = union(enum) {
        integer: u64,
        string: []const u8,
    };
};

const Analyzer = struct {
    allocator: std.mem.Allocator,
    arena: std.heap.ArenaAllocator,
    file: ast.File,
    options: AnalyzeOptions,
    diagnostics: *diagnostics.Collection,

    symbols: std.StringArrayHashMapUnmanaged(SymbolInfo) = .empty,

    functions: std.StringArrayHashMapUnmanaged(Function) = .empty,
    mnemonics: CaseInsensitiveStringHashMap(Mnemonic) = .empty,

    seq_to_instr_lut: []const ?usize = &.{},
    instructions: []InstructionInfo = &.{},
    line_data: std.ArrayListUnmanaged(Module.LineData) = .empty,
    regspace_segments: std.ArrayListUnmanaged(u32) = .empty,

    ok: bool = true,
    error_count: usize = 0,
    in_layout: bool = false,
    suppress_diagnostics: bool = false,

    fn init(allocator: std.mem.Allocator, file: ast.File, options: AnalyzeOptions, diagnostics_collection: *diagnostics.Collection) !Analyzer {
        var ana: Analyzer = .{
            .allocator = allocator,
            .arena = .init(allocator),
            .file = file,
            .options = options,
            .diagnostics = diagnostics_collection,
        };
        try ana.functions.ensureUnusedCapacity(allocator, 8);

        ana.functions.putAssumeCapacityNoClobber("hubaddr", .hubaddr);
        ana.functions.putAssumeCapacityNoClobber("cogaddr", .cogaddr);
        ana.functions.putAssumeCapacityNoClobber("lutaddr", .lutaddr);
        ana.functions.putAssumeCapacityNoClobber("localaddr", .localaddr);
        ana.functions.putAssumeCapacityNoClobber("byteoffset", .byteoffset);
        ana.functions.putAssumeCapacityNoClobber("wordoffset", .wordoffset);
        ana.functions.putAssumeCapacityNoClobber("aug", .aug);
        ana.functions.putAssumeCapacityNoClobber("nrel", .nrel);

        try ana.mnemonics.ensureUnusedCapacity(allocator, 16);

        ana.mnemonics.putAssumeCapacityNoClobber(".cogexec", .cogexec);
        ana.mnemonics.putAssumeCapacityNoClobber(".lutexec", .lutexec);
        ana.mnemonics.putAssumeCapacityNoClobber(".hubexec", .hubexec);
        ana.mnemonics.putAssumeCapacityNoClobber(".align", .@"align");
        ana.mnemonics.putAssumeCapacityNoClobber(".pack", .pack);
        ana.mnemonics.putAssumeCapacityNoClobber(".pic", .pic);
        ana.mnemonics.putAssumeCapacityNoClobber(".org", .org);
        ana.mnemonics.putAssumeCapacityNoClobber("RES", .res);
        ana.mnemonics.putAssumeCapacityNoClobber(".regspace", .regspace);
        ana.mnemonics.putAssumeCapacityNoClobber(".data", .data);
        ana.mnemonics.putAssumeCapacityNoClobber("FILE", .file);
        ana.mnemonics.putAssumeCapacityNoClobber(".assert", .assert);
        ana.mnemonics.putAssumeCapacityNoClobber(".fit", .fit);
        ana.mnemonics.putAssumeCapacityNoClobber("LONG", .long);
        ana.mnemonics.putAssumeCapacityNoClobber("WORD", .word);
        ana.mnemonics.putAssumeCapacityNoClobber("BYTE", .byte);

        return ana;
    }

    fn deinit(ana: *Analyzer) void {
        ana.arena.deinit();
        ana.symbols.deinit(ana.allocator);
        ana.functions.deinit(ana.allocator);
        ana.mnemonics.deinit(ana.allocator);
        ana.* = undefined;
    }

    fn emit_diag(ana: *Analyzer, location: ?ast.Location, diagnostic: diagnostics.Kind) !void {
        if (ana.suppress_diagnostics) return;
        if (diagnostic.level() == .@"error") {
            ana.ok = false;
            ana.error_count += 1;
        }
        try ana.diagnostics.emit_diag(location, diagnostic);
    }

    fn emit_eval_error(ana: *Analyzer, location: ast.Location, context: diagnostics.EvaluationContext, err: EvalError) !void {
        const reason: diagnostics.EvaluationFailure = switch (err) {
            error.DiagnosedFailure => return,
            error.OutOfMemory => .out_of_memory,
            error.UndefinedSymbol => .undefined_symbol,
            error.InvalidFunctionCall => .invalid_function_call,
            error.Overflow => .overflow,
            error.DivideByZero => .divide_by_zero,
            error.InvalidArg => .invalid_argument,
            error.TypeMismatch => .type_mismatch,
            error.CyclicConstant => .cyclic_constant,
            error.ConstantNeedsLabelDuringLayout => .constant_needs_label_during_layout,
        };
        try ana.emit_diag(location, .{ .err_expression_evaluation_failed = .{ .context = context, .reason = reason } });
    }

    fn get_symbol_info(ana: *Analyzer, name: []const u8) !*SymbolInfo {
        const gop = try ana.symbols.getOrPut(ana.allocator, name);
        if (!gop.found_existing) {
            gop.key_ptr.* = try ana.arena.allocator().dupe(u8, name);
            gop.value_ptr.* = SymbolInfo{
                .name = gop.key_ptr.*,
            };
        }
        return gop.value_ptr;
    }

    fn get_label_info(ana: *Analyzer, name: []const u8, scope: ?ast.LocalScope) !*SymbolInfo {
        const local = scope orelse return ana.get_symbol_info(name);
        // A NUL cannot occur in a source identifier, so this key cannot alias a global name.
        const key = try std.fmt.allocPrint(ana.allocator, "\x00{d}:{s}", .{ local.id, name });
        defer ana.allocator.free(key);
        const sym = try ana.get_symbol_info(key);
        if (std.mem.eql(u8, sym.name, key)) {
            sym.name = if (local.parent) |parent|
                try std.fmt.allocPrint(ana.arena.allocator(), "{s}:{s}", .{ parent, name[1..] })
            else
                name;
        }
        return sym;
    }

    fn get_mnemonic(ana: *Analyzer, name: []const u8) ?*const Mnemonic {
        return ana.mnemonics.getPtr(name);
    }

    fn get_function(ana: *Analyzer, name: []const u8) ?*const Function {
        return ana.functions.getPtr(name);
    }

    /// Loads predefined constants from a string hash map.
    /// The keys must have a lifetime longer than the Analyzer!
    fn load_constants(ana: *Analyzer, constants: std.StaticStringMap(Value)) !void {
        try ana.symbols.ensureUnusedCapacity(ana.allocator, constants.values().len);

        for (constants.keys(), constants.values()) |name, value| {
            const gop = ana.symbols.getOrPutAssumeCapacity(name);
            if (gop.found_existing) {
                try ana.emit_diag(null, .{
                    .err_duplicate_definition = .{
                        .name = name,
                        .previous = .{ .symbol = gop.value_ptr.type },
                    },
                });
                return error.DuplicateSymbol;
            }
            gop.value_ptr.* = SymbolInfo{
                .name = name,
                .referenced = true, // suppress "unused symbol X" warning
                .value = value,
                .type = .builtin,
            };
        }
    }

    fn load_functions(ana: *Analyzer, functions: std.StaticStringMap(UserFunction)) !void {
        for (functions.keys(), functions.values()) |name, func| {
            const gop = try ana.functions.getOrPut(ana.allocator, name);
            if (gop.found_existing) {
                try ana.emit_diag(null, .{
                    .err_duplicate_definition = .{
                        .name = name,
                        .previous = .function,
                    },
                });
                return error.DuplicateFunction;
            }
            gop.value_ptr.* = .{ .user = func };
        }
    }

    fn load_instructions(ana: *Analyzer, instructions: []const EncodedInstruction) !void {
        for (instructions) |instr| {
            try ana.load_instruction(instr);
        }
    }

    fn load_instruction(ana: *Analyzer, instr: EncodedInstruction) !void {
        var key_buf: [32]u8 = undefined;

        const key = std.ascii.upperString(&key_buf, instr.mnemonic);

        const gop = try ana.mnemonics.getOrPut(ana.allocator, key);
        if (!gop.found_existing) {
            gop.key_ptr.* = try ana.arena.allocator().dupe(u8, key);
            gop.value_ptr.* = .{
                .encoded = .{
                    .variants = .empty,
                },
            };
            // most instructions won't have more than 2 variants anyways:
            try gop.value_ptr.*.encoded.variants.ensureTotalCapacity(ana.arena.allocator(), 2);
        }
        if (gop.value_ptr.* != .encoded)
            return error.InstructionNameMismatch;

        const encoded: *EncodedMnemonic = &gop.value_ptr.*.encoded;

        for (encoded.variants.items) |other| {
            if (other.operands.len == instr.operands.len) {
                const all_eq = for (other.operands, instr.operands) |aop, bop| {
                    if (@as(EncodedInstruction.Operand.TypeId, aop.type) != @as(EncodedInstruction.Operand.TypeId, bop.type))
                        break false;
                } else true;

                const effects_overlap = instr.effects.overlaps(other.effects);

                if (all_eq and effects_overlap) {
                    std.log.err("{s}, {s}", .{ instr.mnemonic, other.mnemonic });
                    for (other.operands) |op| {
                        std.log.err("ops: {t}", .{op.type});
                    }
                    return error.DuplicateInstruction;
                }
            }
        }

        try encoded.variants.append(ana.arena.allocator(), instr);
    }

    /// Declares all symbols from labels and constants.
    fn declare_symbols(ana: *Analyzer) !void {
        var mode: eval.ExecMode = .cog;
        for (ana.file.sequence) |*item| {
            switch (item.*) {
                .empty => {},
                .label => |lbl| {
                    const sym = try ana.get_label_info(lbl.identifier, lbl.local_scope);
                    if (sym.type != .undefined) {
                        try ana.emit_diag(lbl.span.location(), .{
                            .err_duplicate_definition = .{
                                .name = lbl.identifier,
                                .previous = .{ .symbol = sym.type },
                            },
                        });
                    } else {
                        sym.type = switch (lbl.type) {
                            .@"var" => .{ .data = lbl.span.location() },
                            .code => .{ .code = lbl.span.location() },
                        };
                    }
                },
                .constant => |*con| {
                    const sym = try ana.get_symbol_info(con.identifier);
                    if (sym.type != .undefined) {
                        try ana.emit_diag(con.span.location(), .{
                            .err_duplicate_definition = .{
                                .name = con.identifier,
                                .previous = .{ .symbol = sym.type },
                            },
                        });
                    } else {
                        sym.type = .{ .constant = con.span.location() };
                        sym.constant = con;
                        sym.constant_mode = mode;
                    }
                },
                .instruction => |instr| {
                    if (mode_directive.from_name(instr.mnemonic)) |new_mode| mode = new_mode;
                },
            }
        }
    }

    /// Recursively checks all expressions for symbol references and emits "undefined symbol" for
    /// all non-defined syms.
    fn validate_symbol_refs(ana: *Analyzer) !void {
        for (ana.file.sequence) |item| {
            switch (item) {
                .empty => {},
                .label => {},
                .constant => |con| {
                    try ana.validate_expr_symbol_refs(con.value);
                },
                .instruction => |instr| {
                    if (std.ascii.eqlIgnoreCase(instr.mnemonic, ".pack") or std.ascii.eqlIgnoreCase(instr.mnemonic, ".pic")) continue;
                    for (instr.arguments) |arg| {
                        try ana.validate_expr_symbol_refs(arg);
                    }
                },
            }
        }
    }

    /// Recursively validates 'expr' for undefined references.
    fn validate_expr_symbol_refs(ana: *Analyzer, expr: ast.Expression) !void {
        switch (expr) {
            // Symbols are to be checked:
            .symbol => |symref| {
                const sym = try ana.get_label_info(symref.symbol_name, symref.local_scope);
                sym.referenced = true;
                if (sym.type == .undefined) {
                    try ana.emit_diag(symref.span.location(), .{
                        .err_undefined_reference_to_symbol_at = .{
                            .name = symref.symbol_name,
                            .reference_location = symref.span.location(),
                        },
                    });
                }
            },

            .wrapped => |inner| try ana.validate_expr_symbol_refs(inner.value.*),
            .sequence => |seq| for (seq.items) |item| try ana.validate_expr_symbol_refs(item),

            .unary_transform => |trafo| try ana.validate_expr_symbol_refs(trafo.value.*),
            .binary_transform => |trafo| {
                try ana.validate_expr_symbol_refs(trafo.lhs.*);
                try ana.validate_expr_symbol_refs(trafo.rhs.*);
            },
            .function_call => |fncall| {
                if (ana.get_function(fncall.function) == null) {
                    try ana.emit_diag(fncall.span.location(), .{
                        .err_unknown_function = .{
                            .function = fncall.function,
                        },
                    });
                }

                for (fncall.arguments) |arg| {
                    try ana.validate_expr_symbol_refs(arg.value);
                }
            },

            // These values don't require checks:
            .integer, .current_pc => {},
            .string => {},
            .enumerator => {},
        }
    }

    /// Prepares the output stream of instructions to be populated by the
    /// translator
    fn prepare_instruction_stream(ana: *Analyzer) !void {
        const seq_to_instr_lut = try ana.arena.allocator().alloc(?usize, ana.file.sequence.len);
        @memset(seq_to_instr_lut, null);

        const instr_count = blk: {
            var instr_count: usize = 0;
            for (ana.file.sequence) |seq| {
                if (seq == .instruction) {
                    instr_count += 1;
                }
            }
            break :blk instr_count;
        };

        const instructions = try ana.arena.allocator().alloc(InstructionInfo, instr_count);
        {
            var index: usize = 0;
            for (ana.file.sequence, seq_to_instr_lut, 0..) |*seq, *lut, seq_index| {
                if (seq.* == .instruction) {
                    lut.* = index;
                    instructions[index] = InstructionInfo{
                        .seq_index = seq_index,
                        .ast_node = &seq.instruction,
                    };
                    index += 1;
                }
            }
        }

        ana.instructions = instructions;
        ana.seq_to_instr_lut = seq_to_instr_lut;
    }

    ///
    /// Selects the correct mnemonic for each instruction and assigns
    /// the instruction its corresponding size.
    ///
    /// NOTE: This does not select the explicit instruction encoding,
    ///       but just if a mnemonic is an internal directive or a regular
    ///       instruction.
    ///
    fn select_instruction_mnemonic(ana: *Analyzer) !void {
        std.debug.assert(ana.seq_to_instr_lut.len == ana.file.sequence.len);

        for (ana.instructions) |*instr| {
            const mnemonic = ana.get_mnemonic(instr.ast_node.mnemonic) orelse {
                try ana.emit_diag(instr.ast_node.location(), .{
                    .err_unknown_mnemonic = .{
                        .mnemonic = instr.ast_node.mnemonic,
                    },
                });
                continue;
            };

            instr.mnemonic = mnemonic;

            if (instr.ast_node.condition) |condition| {
                if (std.ascii.eqlIgnoreCase(instr.ast_node.mnemonic, "NOP"))
                    try ana.emit_diag(condition.span.location(), .err_nop_cannot_have_condition);
            }

            instr.byte_size = switch (mnemonic.*) {
                .cogexec, .lutexec, .hubexec, .regspace, .data, .org, .res, .assert, .fit, .@"align", .pack, .pic => 0,

                .file => blk: {
                    if (instr.ast_node.arguments.len != 1 or instr.ast_node.arguments[0] != .string) {
                        try ana.emit_diag(instr.ast_node.location(), .err_file_requires_one_string_literal_path);
                        break :blk 0;
                    }
                    const io = ana.options.io orelse {
                        try ana.emit_diag(instr.ast_node.location(), .err_file_requires_file_i_o);
                        break :blk 0;
                    };
                    const dir = if (instr.ast_node.location().source) |source|
                        std.fs.path.dirname(source) orelse "."
                    else
                        ".";
                    const path = instr.ast_node.arguments[0].string.value;
                    const resolved = if (std.fs.path.isAbsolute(path)) path else try std.fs.path.join(ana.arena.allocator(), &.{ dir, path });
                    instr.file_data = std.Io.Dir.cwd().readFileAlloc(io, resolved, ana.arena.allocator(), .limited(512 * 1024)) catch |err| {
                        try ana.emit_diag(instr.ast_node.location(), .{
                            .err_cannot_read_file = .{
                                .path = resolved,
                                .reason = err,
                            },
                        });
                        break :blk 0;
                    };
                    break :blk @intCast(instr.file_data.len);
                },

                .long, .word, .byte => 0, // Sized during layout, after the current address is known.

                .encoded => blk: {
                    var size: u32 = 4;
                    for (instr.ast_node.arguments) |arg| {
                        if (ana.has_augment(arg)) size += 4;
                    }
                    break :blk size;
                },
            };
        }
    }

    fn has_augment(ana: *Analyzer, expr: ast.Expression) bool {
        return switch (expr) {
            .wrapped => |inner| ana.has_augment(inner.value.*),
            .unary_transform => |op| ana.has_augment(op.value.*),
            .binary_transform => |op| op.operator == .array_index and ana.has_augment(op.rhs.*),
            .function_call => |call| if (ana.get_function(call.function)) |func| func.* == .aug else false,
            else => false,
        };
    }

    ///
    /// Assigns all label and instruction locations to their associated
    /// position in hub/cog/lut ram.
    ///
    fn assign_locations(ana: *Analyzer) !void {
        std.debug.assert(ana.seq_to_instr_lut.len == ana.file.sequence.len);
        ana.in_layout = true;
        defer ana.in_layout = false;

        var idgen: Segment_ID_Gen = .{};
        var cursor: Cursor = .init(idgen.next(), .cog, 0);
        var warned_packed_code = false;

        for (ana.file.sequence, 0..) |*seq, i| {
            switch (seq.*) {
                .empty, .constant => {},

                .label => |lbl| {
                    const sym = ana.get_label_info(lbl.identifier, lbl.local_scope) catch unreachable;
                    sym.offset = cursor.offset;
                    if (cursor.hub >= 0x80000 and !cursor.reserved and cursor.mode != .regspace) {
                        try ana.emit_diag(lbl.span.location(), .{ .err_address_outside_space = .{ .subject = .label, .space = .hub, .actual = cursor.hub, .max_exclusive = 0x80000 } });
                    }
                    switch (cursor.mode) {
                        .cog, .lut, .regspace => {
                            if (cursor.local_bytes >= 0x200 * 4) {
                                try ana.emit_diag(lbl.span.location(), .{ .err_address_outside_space = .{ .subject = .label, .space = cursor.mode, .actual = cursor.local_bytes / 4 + @as(u32, if (cursor.mode == .lut) 0x200 else 0), .max_exclusive = if (cursor.mode == .lut) 0x400 else 0x200 } });
                            }
                        },
                        .hub, .data => {},
                    }
                },

                .instruction => |*instr| {
                    const coded = &ana.instructions[ana.seq_to_instr_lut[i].?];
                    std.debug.assert(coded.ast_node == instr);

                    coded.start_addr = cursor.offset;
                    defer {
                        coded.end_addr = cursor.offset;
                        coded.end_pc = cursor.fit_position();
                    }

                    switch (coded.mnemonic.?.*) {
                        .assert => {},
                        .fit => coded.fit_pc = cursor.fit_position(),

                        .hubexec, .lutexec, .cogexec, .regspace, .data => {
                            const mode = mode_directive.from_name(instr.mnemonic).?;
                            const max_args: usize = switch (mode) {
                                .data, .hub => 1,
                                .cog, .lut, .regspace => 2,
                            };
                            if (instr.arguments.len > max_args) {
                                try ana.emit_diag(instr.location(), .{
                                    .err_argument_count_mismatch = .{
                                        .subject = instr.mnemonic,
                                        .min = 0,
                                        .max = max_args,
                                        .found = instr.arguments.len,
                                    },
                                });
                            }
                            var hub_offset: ?u32 = null;
                            if (instr.arguments.len >= 1 and instr.arguments.len <= max_args) {
                                const hub_start = TaggedAddress.init(cursor.offset.segment_id, std.math.cast(u20, cursor.hub), .hub);
                                hub_offset = try ana.layout_integer(instr.arguments[0], instr.location(), instr.mnemonic, hub_start, null);
                            }
                            if (hub_offset) |addr| {
                                if (addr > 0x80000) {
                                    try ana.emit_diag(instr.location(), .{ .err_address_outside_space = .{ .subject = .origin, .space = .hub, .actual = addr, .max_exclusive = 0x80001 } });
                                    continue;
                                }
                            }
                            var local_start: ?u32 = null;
                            if (instr.arguments.len == 2 and max_args == 2) {
                                local_start = try ana.layout_integer(instr.arguments[1], instr.location(), instr.mnemonic, cursor.offset, mode);
                                if (local_start) |target| {
                                    const min: u32 = if (mode == .lut) 0x200 else 0;
                                    const max: u32 = if (mode == .lut) 0x400 else 0x200;
                                    if (target < min or target > max) {
                                        try ana.emit_diag(instr.location(), .{ .err_numeric_value_out_of_range = .{ .subject = "local start", .min = min, .max = max, .actual = target } });
                                        continue;
                                    }
                                }
                            }
                            cursor.change_mode(idgen.next(), mode, hub_offset, local_start);
                            if (mode == .regspace) try ana.regspace_segments.append(ana.arena.allocator(), cursor.hub);

                            // We must change the start address here as we're changing the cursor mode here.
                            coded.start_addr = cursor.offset;
                        },

                        .@"align" => blk: {
                            if (coded.ast_node.arguments.len != 1) {
                                try ana.emit_diag(coded.ast_node.location(), .{
                                    .err_argument_count_mismatch = .{
                                        .subject = ".align",
                                        .min = 1,
                                        .max = 1,
                                        .found = coded.ast_node.arguments.len,
                                    },
                                });
                            }
                            if (coded.ast_node.arguments.len < 1) {
                                break :blk;
                            }

                            if (ana.evaluate_root_expr(coded.ast_node.arguments[0], .{ .start = cursor.offset })) |value| {
                                switch (value.value) {
                                    .int => |int| {
                                        if (std.math.cast(u32, int)) |alignment| {
                                            if (alignment == 0 or !std.math.isPowerOfTwo(alignment)) {
                                                try ana.emit_diag(coded.ast_node.location(), .{
                                                    .err_align_value_must_be_a_nonzero_power_of_two = .{
                                                        .value = alignment,
                                                    },
                                                });
                                            } else if (alignment > 0x80000) {
                                                try ana.emit_diag(coded.ast_node.location(), .{ .err_numeric_value_out_of_range = .{ .subject = ".align value", .min = 1, .max = 0x80000, .actual = alignment } });
                                            } else {
                                                cursor.alignas(alignment);
                                                coded.start_addr = cursor.offset;
                                            }
                                        } else {
                                            try ana.emit_diag(coded.ast_node.location(), .{
                                                .err_numeric_value_out_of_range = .{
                                                    .subject = ".align value",
                                                    .min = 0,
                                                    .max = 0x80000,
                                                    .actual = int,
                                                },
                                            });
                                        }
                                    },
                                    else => {
                                        try ana.emit_diag(coded.ast_node.location(), .{
                                            .err_expected_value_type = .{
                                                .subject = ".align value",
                                                .expected = .int,
                                                .actual = value.value,
                                            },
                                        });
                                    },
                                }
                            } else |err| {
                                if (err == error.UndefinedSymbol) {
                                    try ana.emit_diag(coded.ast_node.location(), .err_align_references_label);
                                } else {
                                    try ana.emit_eval_error(coded.ast_node.location(), .alignment, err);
                                }
                            }
                        },

                        .pack => {
                            if (instr.arguments.len != 1) {
                                try ana.emit_diag(instr.location(), .{ .err_argument_count_mismatch = .{ .subject = ".pack", .min = 1, .max = 1, .found = instr.arguments.len } });
                            } else {
                                const name = if (instr.arguments[0] == .symbol) instr.arguments[0].symbol.symbol_name else "";
                                if (std.ascii.eqlIgnoreCase(name, "off")) {
                                    cursor.pack = 0;
                                } else if (std.ascii.eqlIgnoreCase(name, "byte")) {
                                    cursor.pack = 1;
                                } else if (std.ascii.eqlIgnoreCase(name, "word")) {
                                    cursor.pack = 2;
                                } else if (std.ascii.eqlIgnoreCase(name, "long")) {
                                    cursor.pack = 4;
                                } else {
                                    try ana.emit_diag(instr.location(), .err_invalid_pack_mode);
                                }
                                warned_packed_code = false;
                            }
                        },

                        .pic => {
                            if (instr.arguments.len != 1) {
                                try ana.emit_diag(instr.location(), .{ .err_argument_count_mismatch = .{ .subject = ".pic", .min = 1, .max = 1, .found = instr.arguments.len } });
                            } else {
                                const name = if (instr.arguments[0] == .symbol) instr.arguments[0].symbol.symbol_name else "";
                                inline for (std.meta.tags(PicMode)) |mode| {
                                    if (std.ascii.eqlIgnoreCase(name, @tagName(mode))) {
                                        coded.pic_mode = mode;
                                        break;
                                    }
                                }
                                if (coded.pic_mode == null) try ana.emit_diag(instr.location(), .err_invalid_pic_mode);
                            }
                        },

                        .org => {
                            if (cursor.mode == .data) {
                                try ana.emit_diag(instr.location(), .{ .err_directive_invalid_in_mode = .{ .directive = ".org", .mode = cursor.mode } });
                            } else if (instr.arguments.len != 1) {
                                try ana.emit_diag(instr.location(), .{ .err_argument_count_mismatch = .{ .subject = ".org", .min = 1, .max = 1, .found = instr.arguments.len } });
                            } else if (try ana.layout_integer(instr.arguments[0], instr.location(), ".org", cursor.offset, null)) |target| {
                                const max: u32 = switch (cursor.mode) {
                                    .cog, .regspace => 0x200,
                                    .lut => 0x400,
                                    .hub => 0x80000,
                                    .data => unreachable,
                                };
                                if (target > max or (cursor.mode == .lut and target < 0x200)) {
                                    try ana.emit_diag(instr.location(), .{ .err_numeric_value_out_of_range = .{ .subject = ".org target", .min = if (cursor.mode == .lut) 0x200 else 0, .max = max, .actual = target } });
                                    continue;
                                }
                                const current = if (cursor.mode == .hub) cursor.hub else cursor.local_bytes;
                                const requested = switch (cursor.mode) {
                                    .hub => target,
                                    .lut => (target - 0x200) * 4,
                                    else => target * 4,
                                };
                                if (requested < current) {
                                    try ana.emit_diag(instr.location(), .err_org_cannot_move_pc_backward);
                                } else {
                                    cursor.org(target);
                                    coded.start_addr = cursor.offset;
                                }
                            }
                        },

                        .res => {
                            if (cursor.mode != .cog and cursor.mode != .regspace) {
                                try ana.emit_diag(instr.location(), .{ .err_directive_invalid_in_mode = .{ .directive = "RES", .mode = cursor.mode } });
                            } else if (instr.arguments.len != 1) {
                                try ana.emit_diag(instr.location(), .{ .err_argument_count_mismatch = .{ .subject = "RES", .min = 1, .max = 1, .found = instr.arguments.len } });
                            } else if (try ana.layout_integer(instr.arguments[0], instr.location(), "RES", cursor.offset, null)) |count| {
                                if (cursor.local_bytes / 4 > 0x200 or count > 0x200 - @min(cursor.local_bytes / 4, 0x200)) {
                                    try ana.emit_diag(instr.location(), .{ .err_numeric_value_out_of_range = .{ .subject = "RES count", .min = 0, .max = 0x200 - @min(cursor.local_bytes / 4, 0x200), .actual = count } });
                                } else {
                                    cursor.reserve(count);
                                    coded.start_addr = cursor.offset;
                                }
                            }
                        },

                        .long, .word, .byte, .file => {
                            if (cursor.mode == .regspace or cursor.reserved) {
                                try ana.emit_diag(instr.location(), .err_cannot_emit_data_after_reserve_or_inside_regspace);
                            } else {
                                const unit: u32 = switch (coded.mnemonic.?.*) {
                                    .long => 4,
                                    .word => 2,
                                    else => 1,
                                };
                                const alignment = if (cursor.pack == 0) unit else cursor.pack;
                                if (coded.mnemonic.?.* != .file) {
                                    var aligned = cursor;
                                    aligned.align_data(alignment);
                                    coded.byte_size = try ana.data_instruction_size(coded.ast_node, aligned.offset, unit);
                                    if (coded.byte_size.? != 0) cursor = aligned;
                                } else {
                                    cursor.align_data(alignment);
                                }
                                coded.start_addr = cursor.offset;
                                const size = coded.byte_size.?;
                                cursor.hub += size;
                                if (cursor.mode == .cog or cursor.mode == .lut) cursor.local_bytes += size;
                                cursor.sync();
                            }
                        },

                        .encoded => {
                            if (cursor.mode == .data or cursor.mode == .regspace or cursor.reserved) {
                                try ana.emit_diag(instr.location(), .err_cannot_emit_code_in_this_segment);
                            } else {
                                cursor.align_data(if (cursor.pack == 0) 4 else cursor.pack);
                                if ((cursor.mode == .cog or cursor.mode == .lut) and cursor.local_bytes % 4 != 0) {
                                    try ana.emit_diag(instr.location(), .err_unaligned_cog_lut_instruction);
                                    continue;
                                }
                                if (cursor.pack != 0 and cursor.pack != 4 and !warned_packed_code) {
                                    try ana.emit_diag(instr.location(), .warn_unaligned_code);
                                    warned_packed_code = true;
                                }
                                coded.start_addr = cursor.offset;
                                for (0..@divExact(coded.byte_size.?, 4)) |_| cursor.advance_data(.long);
                            }
                        },
                    }
                    if (cursor.hub > 0x80000) try ana.emit_diag(instr.location(), .{ .err_address_outside_space = .{ .subject = .cursor, .space = .hub, .actual = cursor.hub, .max_exclusive = 0x80001 } });
                    switch (cursor.mode) {
                        .cog, .regspace => {
                            if (cursor.local_bytes > 0x200 * 4)
                                try ana.emit_diag(instr.location(), .{ .err_address_outside_space = .{ .subject = .cursor, .space = cursor.mode, .actual = cursor.local_bytes / 4, .max_exclusive = 0x201 } });
                        },
                        .lut => {
                            if (cursor.local_bytes > 0x200 * 4)
                                try ana.emit_diag(instr.location(), .{ .err_address_outside_space = .{ .subject = .cursor, .space = .lut, .actual = 0x200 + cursor.local_bytes / 4, .max_exclusive = 0x401 } });
                        },
                        .data, .hub => {},
                    }
                },
            }
        }
    }

    fn layout_integer(ana: *Analyzer, expr: ast.Expression, location: ast.Location, name: []const u8, start: TaggedAddress, local_mode: ?eval.ExecMode) !?u32 {
        const value = ana.evaluate_root_expr(expr, .{ .start = start }) catch |err| {
            if (err != error.DiagnosedFailure) try ana.emit_diag(location, .{
                .err_requires_an_integer_known_during_layout = .{
                    .name = name,
                    .reason = err,
                },
            });
            return null;
        };
        if (value.value == .address and local_mode != null) {
            return try ana.local_address_value(value.value.address, local_mode.?, location, name);
        }
        if (value.value != .int) {
            try ana.emit_diag(location, .{
                .err_expected_value_type = .{
                    .subject = name,
                    .expected = .int,
                    .actual = value.value,
                },
            });
            return null;
        }
        const number = std.math.cast(u32, value.value.int) orelse {
            try ana.emit_diag(location, .{
                .err_numeric_value_out_of_range = .{
                    .subject = name,
                    .min = 0,
                    .max = std.math.maxInt(u32),
                    .actual = value.value.int,
                },
            });
            return null;
        };
        return number;
    }

    fn constant_expression(ana: *Analyzer, name: []const u8) ?ast.Expression {
        const sym = ana.symbols.get(name) orelse return null;
        return if (sym.constant) |con| con.value else null;
    }

    fn is_expandable(ana: *Analyzer, expr: ast.Expression) !bool {
        var visiting: std.StringHashMapUnmanaged(void) = .empty;
        defer visiting.deinit(ana.allocator);
        return ana.is_expandable_inner(expr, &visiting);
    }

    fn is_expandable_inner(ana: *Analyzer, expr: ast.Expression, visiting: *std.StringHashMapUnmanaged(void)) std.mem.Allocator.Error!bool {
        return switch (expr) {
            .string, .sequence => true,
            .wrapped => |inner| try ana.is_expandable_inner(inner.value.*, visiting),
            .symbol => |sym| blk: {
                const value = ana.constant_expression(sym.symbol_name) orelse break :blk false;
                if (visiting.contains(sym.symbol_name)) break :blk false;
                try visiting.put(ana.allocator, sym.symbol_name, {});
                defer _ = visiting.remove(sym.symbol_name);
                break :blk try ana.is_expandable_inner(value, visiting);
            },
            .binary_transform => |op| op.operator == .@"*" and
                ((try ana.is_expandable_inner(op.lhs.*, visiting)) or (try ana.is_expandable_inner(op.rhs.*, visiting))),
            .function_call => |call| std.mem.eql(u8, call.function, "utf16") or std.mem.eql(u8, call.function, "utf32"),
            else => false,
        };
    }

    fn data_expr_count(ana: *Analyzer, expr: ast.Expression, start: TaggedAddress) !u32 {
        const count: u64 = switch (expr) {
            .string => |str| str.value.len,
            .sequence => |seq| blk: {
                var total: u64 = 0;
                for (seq.items) |item| {
                    total += try ana.data_expr_count(item, start);
                    if (total > 0x80000) break;
                }
                break :blk total;
            },
            .wrapped => |inner| return ana.data_expr_count(inner.value.*, start),
            .symbol => blk: {
                if (!(try ana.is_expandable(expr))) break :blk 1;
                const value = ana.evaluate_root_expr(expr, .{ .start = start }) catch |err| {
                    try ana.emit_eval_error(expr.location(), .expression, err);
                    return 0;
                };
                break :blk switch (value.value) {
                    .string => |str| str.len,
                    .sequence => |items| items.len,
                    else => 1,
                };
            },
            .function_call => blk: {
                if (!(try ana.is_expandable(expr))) break :blk 1;
                const value = ana.evaluate_root_expr(expr, .{ .start = start }) catch |err| {
                    try ana.emit_eval_error(expr.location(), .expression, err);
                    return 0;
                };
                break :blk if (value.value == .sequence) value.value.sequence.len else 1;
            },
            .binary_transform => |op| blk: {
                if (op.operator != .@"*" or !(try ana.is_expandable(expr))) break :blk 1;
                const left_expands = try ana.is_expandable(op.lhs.*);
                const values = if (left_expands) op.lhs.* else op.rhs.*;
                const repetitions = if (left_expands) op.rhs.* else op.lhs.*;
                const times = (try ana.layout_integer(repetitions, repetitions.location(), "array repetition count", start, null)) orelse return 0;
                break :blk @as(u64, try ana.data_expr_count(values, start)) * times;
            },
            else => 1,
        };
        if (count > 0x80000) {
            try ana.emit_diag(expr.location(), .err_array_output_too_large);
            return 0;
        }
        return @intCast(count);
    }

    fn data_instruction_size(ana: *Analyzer, instr: *const ast.Instruction, start: TaggedAddress, unit: u32) !u32 {
        var count: u64 = 0;
        for (instr.arguments) |arg| count += try ana.data_expr_count(arg, start);
        if (count * unit > 0x80000) {
            try ana.emit_diag(instr.location(), .err_array_output_too_large);
            return 0;
        }
        return @intCast(count * unit);
    }

    fn repeat_value(ana: *Analyzer, location: ast.Location, source: Value, raw_count: i64) EvalError!Value {
        const count = std.math.cast(usize, raw_count) orelse {
            try ana.emit_diag(location, .{ .err_numeric_value_out_of_range = .{ .subject = "array repetition count", .min = 0, .max = 0x80000, .actual = raw_count } });
            return error.DiagnosedFailure;
        };
        if (count == 0) try ana.emit_diag(location, .warn_zero_repetition);
        const len: usize = switch (source.value) {
            .string => |str| str.len,
            .sequence => |seq| seq.len,
            else => unreachable,
        };
        if (len == 0) return source;
        const total = std.math.mul(usize, len, count) catch return error.Overflow;
        if (total > 0x80000) return error.Overflow;
        switch (source.value) {
            .string => |str| {
                const result = try ana.arena.allocator().alloc(u8, total);
                for (0..count) |i| @memcpy(result[i * len ..][0..len], str);
                return .string(result);
            },
            .sequence => |seq| {
                const result = try ana.arena.allocator().alloc(i64, total);
                for (0..count) |i| @memcpy(result[i * len ..][0..len], seq);
                return .sequence(result);
            },
            else => unreachable,
        }
    }

    fn local_address_value(ana: *Analyzer, address: TaggedAddress, mode: eval.ExecMode, location: ast.Location, subject: []const u8) !?u32 {
        const actual: eval.ExecMode = address.local;
        const matches = switch (mode) {
            .cog, .regspace => actual == .cog or actual == .regspace,
            .lut => actual == .lut,
            .hub, .data => unreachable,
        };
        if (!matches) {
            try ana.emit_diag(location, .{ .err_address_space_mismatch = .{
                .subject = subject,
                .expected = if (mode == .cog or mode == .regspace) .initMany(&.{ .cog, .regspace }) else .initOne(mode),
                .actual = .initOne(actual),
            } });
            return null;
        }
        return address.get_local(.pc).?;
    }

    /// Check labels after layout and constants after evaluation.
    fn check_symbols(ana: *Analyzer, comptime phase: enum { labels, constants }) !void {
        for (ana.symbols.values()) |sym| {
            errdefer logger.err("invalid symbol {s}", .{sym.name});
            switch (sym.type) {
                .undefined => {
                    if (phase == .constants) {
                        std.debug.assert(sym.referenced);
                        ana.ok = false;
                    }
                },
                .code, .data => {
                    if (phase == .labels) {
                        if (sym.offset == null or sym.value != null) return error.InvalidSymbol;
                    }
                },
                .constant, .builtin => {
                    if (phase == .constants) {
                        if (sym.offset != null or sym.value == null) return error.InvalidSymbol;
                    }
                },
            }
            const defined_this_phase = switch (phase) {
                .labels => sym.type == .code or sym.type == .data,
                .constants => sym.type == .constant,
            };
            if (defined_this_phase and !sym.referenced) {
                try ana.emit_diag(sym.location(), .{
                    .warn_symbol_has_no_references = .{
                        .name = sym.name,
                    },
                });
            }
        }
    }

    fn resolve_constant(ana: *Analyzer, name: []const u8) EvalError!Value {
        const sym = ana.symbols.getPtr(name) orelse return error.UndefinedSymbol;
        std.debug.assert(sym.type == .constant);
        if (sym.value) |value| return value;
        if (sym.constant_state == .evaluating) return error.CyclicConstant;
        if (sym.constant_state == .failed) return error.DiagnosedFailure;
        const con = sym.constant orelse return error.UndefinedSymbol;

        sym.constant_state = .evaluating;
        errdefer {
            const current = ana.symbols.getPtr(name).?;
            if (current.constant_state == .evaluating) current.constant_state = .unvisited;
        }

        const errors_before = ana.error_count;
        const value = ana.evaluate_root_expr(con.value, .{
            .constant = true,
            .exec_mode = sym.constant_mode,
            .forbid_label_addresses = ana.in_layout,
        }) catch |err| {
            if (err == error.ConstantNeedsLabelDuringLayout) return err;
            (ana.symbols.getPtr(name).?).constant_state = .failed;
            try ana.emit_eval_error(con.span.location(), .expression, err);
            return error.DiagnosedFailure;
        };
        if (ana.error_count != errors_before) {
            (ana.symbols.getPtr(name).?).constant_state = .failed;
            return error.DiagnosedFailure;
        }

        switch (value.value) {
            .int, .string, .sequence, .enumerator => {},
            .register => {},
            .address => {
                try ana.emit_diag(con.span.location(), .{ .err_constant_requires_integer_not_offset = .{ .name = name } });
                (ana.symbols.getPtr(name).?).constant_state = .failed;
                return error.DiagnosedFailure;
            },
            .pointer_expr => {
                try ana.emit_diag(con.span.location(), .err_constants_cannot_store_pointer_expression);
                (ana.symbols.getPtr(name).?).constant_state = .failed;
                return error.DiagnosedFailure;
            },
        }

        const current = ana.symbols.getPtr(name).?;
        current.value = value;
        current.constant_state = .evaluated;
        return value;
    }

    fn evaluate_constant_values(ana: *Analyzer) !void {
        for (ana.file.sequence) |seq| {
            if (seq != .constant) continue;
            _ = ana.resolve_constant(seq.constant.identifier) catch |err| {
                try ana.emit_eval_error(seq.constant.span.location(), .expression, err);
            };
        }
    }

    fn evaluate_instruction_arguments(ana: *Analyzer) !void {
        for (ana.instructions) |*instr| {
            std.debug.assert(instr.mnemonic != null);
            std.debug.assert(instr.start_addr != null);
            std.debug.assert(instr.end_addr != null);

            const args = try ana.arena.allocator().alloc(eval.Value, instr.ast_node.arguments.len);
            switch (instr.mnemonic.?.*) {
                .cogexec, .lutexec, .hubexec, .regspace, .data, .pack, .pic => {
                    // These directive operands were handled during layout.
                    @memset(args, .int(0));
                    instr.arguments = args;
                    continue;
                },
                else => {},
            }
            for (args, instr.ast_node.arguments) |*value, expr| {
                value.* = ana.evaluate_root_expr(expr, .{ .after = instr.end_addr, .after_pc = instr.end_pc, .start = instr.start_addr, .fit_pc = instr.fit_pc, .allow_augment = instr.mnemonic.?.* == .encoded }) catch |err| {
                    try ana.emit_eval_error(instr.ast_node.location(), .expression, err);
                    value.* = .int(0);
                    continue;
                };
            }

            if (instr.mnemonic.?.* == .byte or instr.mnemonic.?.* == .word or instr.mnemonic.?.* == .long) {
                var count: u64 = 0;
                for (args) |arg| count += switch (arg.value) {
                    .string => |str| @as(u64, str.len),
                    .sequence => |seq| @as(u64, seq.len),
                    else => 1,
                };
                const unit: u64 = switch (instr.mnemonic.?.*) {
                    .byte => 1,
                    .word => 2,
                    .long => 4,
                    else => unreachable,
                };
                if (count * unit != instr.byte_size.?) try ana.emit_diag(instr.ast_node.location(), .err_array_length_requires_layout_known);
            }

            instr.arguments = args;
        }
    }

    ///
    /// Selects the fitting encoding and required arguments for the mnemonic.
    ///
    fn select_instruction_encoding(ana: *Analyzer) !void {
        var pic_mode: PicMode = .default;
        current_instr: for (ana.instructions) |*instr| {
            std.debug.assert(instr.mnemonic != null);
            std.debug.assert(instr.start_addr != null);
            std.debug.assert(instr.end_addr != null);
            std.debug.assert(instr.arguments.len == instr.ast_node.arguments.len);

            const mnemonic: *const EncodedMnemonic = switch (instr.mnemonic.?.*) {
                .encoded => |*mnemonic| mnemonic,

                .pic => {
                    pic_mode = instr.pic_mode.?;
                    continue :current_instr;
                },

                // these are all already defined
                else => continue :current_instr,
            };
            std.debug.assert(mnemonic.variants.items.len > 0);

            var alternatives_buffer: [8]*const EncodedInstruction = undefined;
            var alternatives: std.ArrayList(*const EncodedInstruction) = .initBuffer(&alternatives_buffer);

            for (mnemonic.variants.items) |*option| {
                if (option.operands.len != instr.arguments.len)
                    continue;
                alternatives.appendBounded(option) catch @panic("array too small");
            }
            if (alternatives.items.len == 0) {
                try ana.emit_diag(instr.ast_node.location(), .{
                    .err_instruction_operand_count_unmatched = .{
                        .mnemonic = instr.ast_node.mnemonic,
                        .found = instr.arguments.len,
                    },
                });
                continue :current_instr;
            }

            logger.debug("{s} => args={}, vars={}, argc_matching={}", .{
                instr.ast_node.mnemonic,
                instr.arguments.len,
                mnemonic.variants.items.len,
                alternatives.items.len,
            });

            logger.debug("  args:", .{});
            for (instr.arguments) |arg| {
                logger.debug("  - {t}: {t} aug={}", .{ arg.value, arg.flags.usage, arg.flags.augment });
            }
            logger.debug("  alts:", .{});

            var selection: ?*const EncodedInstruction = null;
            match_alternative: for (alternatives.items) |alt| {
                logger.debug("  - {s}", .{alt.mnemonic});

                var can_assign = true;
                for (alt.operands, instr.arguments) |op, arg| {
                    const op_ok = op.type.can_assign_from(arg);
                    logger.debug("    - {t}; type ok={}", .{
                        op.type,
                        op_ok,
                    });
                    if (!op_ok) {
                        can_assign = false;
                    }
                }
                if (instr.ast_node.effect) |effect| {
                    if (!alt.effects.contains(effect.type)) {
                        logger.debug("      : non-matching effect", .{});
                        can_assign = false;
                    }
                } else {
                    if (!alt.effects.none) {
                        logger.debug("      : requires effect", .{});
                        can_assign = false;
                    }
                }

                if (!can_assign) {
                    logger.debug("      : skip!", .{});
                    continue;
                }

                if (selection) |previous| amgigious_check: {
                    if (previous.operands.len != 0) {
                        const any_ptrreg_prev: bool = for (previous.operands) |op| {
                            if (op.type == .pointer_reg)
                                break true;
                        } else false;
                        const any_ptrreg_now: bool = for (alt.operands) |op| {
                            if (op.type == .pointer_reg)
                                break true;
                        } else false;

                        if (any_ptrreg_prev and any_ptrreg_now) {
                            @panic("incredibly amgigious instructions, should check the setup");
                        }

                        if (std.ascii.eqlIgnoreCase(alt.mnemonic, "CALLD") and (any_ptrreg_prev or any_ptrreg_now)) {
                            const wanted = ana.select_calld_ambiguous_encoding(instr, pic_mode);
                            if (any_ptrreg_prev == (wanted == .address))
                                continue :match_alternative;
                            break :amgigious_check;
                        }

                        if (any_ptrreg_prev) {
                            continue :match_alternative;
                        }

                        if (any_ptrreg_now) {
                            break :amgigious_check;
                        }

                        // neither use pointer_reg, so fall through into regular handling:
                    }

                    try ana.emit_diag(instr.ast_node.location(), .{
                        .err_ambigious_instruction_selection_for = .{
                            .mnemonic = alt.mnemonic,
                        },
                    });
                    continue :current_instr;
                }
                selection = alt;
            }

            instr.instruction = selection orelse {
                try ana.emit_diag(instr.ast_node.location(), .{
                    .err_ambigious_instruction_selection_for = .{
                        .mnemonic = instr.ast_node.mnemonic,
                    },
                });
                continue :current_instr;
            };

            switch (ana.options.blank_pointer_expr) {
                .as_register => {}, // keep as-is

                .as_ptr_epxr => {
                    // If we find an operand which is a .ptr_expr and the value is register PTRA or PTRB, we patch
                    // it to also use a pointer expression:

                    for (instr.arguments, instr.instruction.?.operands) |*arg, op| {
                        if (op.type != .pointer_expr)
                            continue;
                        switch (arg.value) {
                            .register => |reg| switch (reg) {
                                // rewrite PTRA, PTRB into a pointer_expr
                                PTRA => arg.* = .{
                                    .flags = arg.flags,
                                    .value = .{ .pointer_expr = .{ .pointer = .PTRA, .increment = .none, .index = null } },
                                },
                                PTRB => arg.* = .{
                                    .flags = arg.flags,
                                    .value = .{ .pointer_expr = .{ .pointer = .PTRB, .increment = .none, .index = null } },
                                },

                                else => {}, // keep all other registers
                            },
                            else => {}, // keep all other values
                        }
                    }
                },
            }
        }
    }

    fn select_calld_ambiguous_encoding(ana: *Analyzer, instr: *const InstructionInfo, pic_mode: PicMode) CalldAmbiguousEncoding {
        const source = instr.arguments[1];
        if (source.flags.addressing == .absolute or pic_mode == .avoid) return .address;
        if (source.flags.augment) return .general;
        if (ana.options.calld_ambiguous_encoding != .flexspin) return ana.options.calld_ambiguous_encoding;

        const start = instr.start_addr.?;
        const after = instr.end_addr.?;
        const current_mode = std.meta.activeTag(start.local);
        const target: u32 = switch (source.value) {
            .address => |address| if (address.local == .data)
                address.hub_address orelse return .address
            else
                address.get_local(.pc) orelse return .address,
            .int => |number| std.math.cast(u32, number) orelse return .address,
            else => return .address,
        };
        const target_mode: eval.ExecMode = if (source.value == .address and source.value.address.local != .data)
            std.meta.activeTag(source.value.address.local)
        else if (target < 0x400)
            if (target < 0x200) .cog else .lut
        else
            .hub;
        if (!same_exec_domain(current_mode, target_mode)) return .address;

        if (source.value == .address and source.value.address.local != .data) {
            const address = source.value.address;
            if (start.segment_id != address.segment_id and current_mode == .hub and target_mode == .hub and
                !(pic_mode == .prefer or pic_mode == .force or ana.options.use_label_relative_hub_to_hub_jmp)) return .address;
        } else if (pic_mode == .default and !ana.options.use_relative_jmp_for_same_mode_nonlabel_address) {
            return .address;
        }

        const pc = after.get_local(.pc) orelse return .address;
        const delta = @as(i64, target) - @as(i64, pc);
        if (current_mode == .hub and @mod(delta, 4) != 0) return .address;
        const instruction_delta = if (current_mode == .hub) @divTrunc(delta, 4) else delta;
        return if (std.math.cast(i9, instruction_delta) != null) .general else .address;
    }

    fn same_exec_domain(a: eval.ExecMode, b: eval.ExecMode) bool {
        return a == b or (a == .cog or a == .lut) and (b == .cog or b == .lut);
    }

    fn evaluate_asserts(ana: *Analyzer) !void {
        for (ana.instructions) |*instr| {
            const is_fit = instr.mnemonic.?.* == .fit;
            if (!is_fit and instr.mnemonic.?.* != .assert) continue;
            const directive = if (is_fit) ".fit" else ".assert";

            const with_message = switch (instr.arguments.len) {
                0 => {
                    try ana.emit_diag(instr.ast_node.location(), .{ .err_argument_count_mismatch = .{ .subject = directive, .min = 1, .max = 2, .found = 0 } });
                    continue;
                },
                1 => false,
                2 => true,
                else => blk: {
                    try ana.emit_diag(instr.ast_node.location(), .{
                        .err_argument_count_mismatch = .{
                            .subject = directive,
                            .min = 1,
                            .max = 2,
                            .found = instr.arguments.len,
                        },
                    });
                    break :blk true;
                },
            };

            std.debug.assert(instr.arguments.len >= 1);

            const condition = instr.arguments[0];
            const condition_true = if (is_fit) blk: {
                const mode: eval.ExecMode = std.meta.activeTag(instr.start_addr.?.local);
                const limit: i64 = switch (condition.value) {
                    .int => |number| number,
                    .address => |address| if (mode == .hub or mode == .data)
                        address.hub_address orelse {
                            try ana.emit_diag(instr.ast_node.location(), .{ .err_address_space_mismatch = .{
                                .subject = ".fit limit",
                                .expected = .initOne(mode),
                                .actual = .initOne(std.meta.activeTag(address.local)),
                            } });
                            continue;
                        }
                    else
                        (try ana.local_address_value(address, mode, instr.ast_node.location(), ".fit limit")) orelse continue,
                    else => {
                        try ana.emit_diag(instr.ast_node.location(), .{ .err_expected_value_type = .{ .subject = ".fit limit", .expected = .int, .actual = condition.value } });
                        continue;
                    },
                };
                break :blk @as(i64, instr.fit_pc.?) <= limit;
            } else blk: {
                if (condition.value != .int) {
                    try ana.emit_diag(instr.ast_node.location(), .{ .err_expected_value_type = .{ .subject = ".assert condition", .expected = .int, .actual = condition.value } });
                    continue;
                }
                break :blk condition.value.int != 0;
            };
            var message: []const u8 = "expression returned 0";

            if (with_message) {
                const msg = instr.arguments[1];

                if (msg.value != .string) {
                    try ana.emit_diag(instr.ast_node.location(), .{
                        .err_expected_value_type = .{
                            .subject = if (is_fit) ".fit message" else ".assert message",
                            .expected = .string,
                            .actual = msg.value,
                        },
                    });
                    continue;
                }
                message = msg.value.string;
            }

            if (condition_true) continue;

            if (!with_message and !is_fit) {
                const arg_expr = instr.ast_node.arguments[0];
                if (arg_expr == .binary_transform) {
                    const maybe_relation: ?[]const u8 = switch (arg_expr.binary_transform.operator) {
                        .@"==" => "is not equal to",
                        .@"!=" => "is equal to",
                        .@"<" => "is not less than",
                        .@">" => "is not greater than",
                        .@"<=" => "is greater than",
                        .@">=" => "is smaller than",
                        else => null,
                    };

                    if (maybe_relation) |relation| {
                        // Operands were already evaluated and diagnosed; only build the message.
                        ana.suppress_diagnostics = true;
                        defer ana.suppress_diagnostics = false;
                        const context: EvalContext = .{ .after = instr.end_addr, .after_pc = instr.end_pc, .start = instr.start_addr };
                        const lhs = try ana.evaluate_root_expr(arg_expr.binary_transform.lhs.*, context);
                        const rhs = try ana.evaluate_root_expr(arg_expr.binary_transform.rhs.*, context);

                        // TODO(0.15.2): Use "nice" formatting again:
                        message = try std.fmt.allocPrint(ana.arena.allocator(), "{f} {s} {f}!", .{ lhs, relation, rhs });
                    }
                }
            }

            try ana.emit_diag(instr.ast_node.location(), .{
                .err_assertion_failed = .{
                    .message = message,
                },
            });
        }
    }

    fn emit_code(ana: *Analyzer, segment_allocator: std.mem.Allocator) ![]Segment {
        var segments: std.ArrayListUnmanaged(Segment) = .empty;
        errdefer segments.deinit(segment_allocator);

        var sid: Segment_ID_Gen = .{};

        var current_segment: SegmentBuilder = .init(sid.next(), 0, .cog, segment_allocator);
        defer current_segment.data.deinit();
        var pic_mode: PicMode = .default;

        std.debug.assert(ana.line_data.items.len == 0);
        errdefer {
            ana.line_data.deinit(segment_allocator);
            ana.line_data = .empty;
        }

        seq_loop: for (ana.file.sequence, 0..) |seq, i| {
            const segment_end_hub_offset: u32 = @intCast(current_segment.hub_offset + current_segment.len());

            const instr: *InstructionInfo = switch (seq) {
                .constant, .empty => continue :seq_loop,

                .label => |lbl| {

                    // just assert we're not doing stupid things:
                    const sym = ana.get_label_info(lbl.identifier, lbl.local_scope) catch unreachable;
                    if (sym.offset.?.hub_address) |label_hub| {
                        try ana.line_data.append(segment_allocator, .{
                            .offset = label_hub,
                            .length = 0,
                            .location = lbl.span.location(),
                            .pc = sym.offset.?.get_local(.pc),
                        });
                    }

                    continue :seq_loop;
                },

                .instruction => &ana.instructions[ana.seq_to_instr_lut[i].?],
            };

            const mnemonic: Mnemonic = instr.mnemonic.?.*;

            if (instr.byte_size.? == 0 and switch (mnemonic) {
                .byte, .word, .long, .file => true,
                else => false,
            }) continue :seq_loop;

            switch (mnemonic) {
                .assert, .fit => continue :seq_loop,

                .pic => {
                    pic_mode = instr.pic_mode.?;
                    continue :seq_loop;
                },

                .@"align", .pack, .org, .res => continue :seq_loop,

                .cogexec, .lutexec, .hubexec, .regspace, .data => {
                    const new_mode = mode_directive.from_name(instr.ast_node.mnemonic).?;
                    const hub_offset = instr.start_addr.?.hub_address orelse segment_end_hub_offset;

                    if (current_segment.len() > 0) {
                        try segments.append(segment_allocator, .{
                            .id = current_segment.id,
                            .hub_offset = @intCast(current_segment.hub_offset),
                            .data = try current_segment.toOwnedSlice(),
                            .exec_mode = current_segment.exec_mode,
                        });
                    }

                    current_segment.deinit();
                    current_segment = .init(sid.next(), hub_offset, new_mode, segment_allocator);

                    std.debug.assert(instr.start_addr.?.segment_id == current_segment.id);

                    continue :seq_loop;
                },

                else => {},
            }

            const hub_offset = instr.start_addr.?.hub_address orelse unreachable;
            std.debug.assert(hub_offset >= segment_end_hub_offset);

            if (hub_offset > segment_end_hub_offset) {
                try current_segment.writer().splatByteAll(ana.options.fill_byte, hub_offset - segment_end_hub_offset);
                try ana.emit_diag(instr.ast_node.location(), .{
                    .warn_emitted_padding_byte_s = .{
                        .count = hub_offset - segment_end_hub_offset,
                    },
                });
            }

            const line_info = try ana.line_data.addOne(segment_allocator);
            line_info.* = .{
                .offset = hub_offset,
                .length = 0,
                .location = instr.ast_node.location(),
                .pc = instr.start_addr.?.get_local(.pc),
                .kind = switch (mnemonic) {
                    .encoded => .code,
                    .byte => .byte,
                    .word => .word,
                    .long => .long,
                    .file => .file,
                    else => unreachable,
                },
                .mnemonic = if (mnemonic == .encoded) instr.ast_node.mnemonic else null,
                .condition = if (instr.ast_node.condition) |condition| condition.type else null,
                .effect = if (instr.ast_node.effect) |effect| effect.type else null,
            };
            if (mnemonic == .encoded) {
                const operands = try segment_allocator.alloc(Module.LineData.Operand, instr.arguments.len);
                for (operands, instr.arguments, instr.ast_node.arguments, instr.instruction.?.operands) |*operand, value, expression, encoded_operand| {
                    var rendered: std.Io.Writer.Allocating = .init(segment_allocator);
                    defer rendered.deinit();
                    try frontend.render.pretty_print_expr(&rendered.writer, expression);
                    operand.* = .{
                        .value = value,
                        .syntax = try rendered.toOwnedSlice(),
                        .source_kind = switch (expression) {
                            .integer => .integer,
                            .symbol => .symbol,
                            .function_call => .function_call,
                            else => .other,
                        },
                        .encoding = switch (encoded_operand.type) {
                            .address => .address,
                            .register => .register,
                            .immediate => .immediate,
                            .reg_or_imm => .reg_or_imm,
                            .pointer_expr => .pointer_expr,
                            .pointer_reg => .pointer_reg,
                            .enumeration => .enumeration,
                        },
                        .pcrel = switch (encoded_operand.type) {
                            .reg_or_imm => |meta| meta.pcrel,
                            else => false,
                        },
                    };
                }
                line_info.operands = operands;
            }
            defer line_info.length = @intCast((current_segment.hub_offset + current_segment.len()) - hub_offset);

            logger.debug("emit {t}", .{mnemonic});

            switch (mnemonic) {
                .assert,
                .fit,
                .cogexec,
                .lutexec,
                .hubexec,
                .regspace,
                .data,
                .org,
                .res,
                .@"align",
                .pack,
                .pic,
                => unreachable,

                .file => try current_segment.writer().writeAll(instr.file_data),

                inline .byte, .word, .long => |_, tag| {
                    const T = switch (tag) {
                        .byte => u8,
                        .word => u16,
                        .long => u32,
                        else => unreachable,
                    };

                    var number_styles: std.ArrayList(Module.LineData.NumberStyle) = .empty;
                    defer number_styles.deinit(segment_allocator);
                    for (instr.arguments, instr.ast_node.arguments) |container_value, ast_node| {
                        const mode: eval.ExecMode = if (current_segment.exec_mode == .data) .hub else current_segment.exec_mode;
                        switch (container_value.value) {
                            .string => |str| for (str) |byte| {
                                const value: T = try ana.cast_value_to(ast_node.location(), mode, .int(byte), .data, T);
                                try current_segment.writer().writeInt(T, value, .little);
                                try number_styles.append(segment_allocator, .hex);
                            },
                            .sequence => |items| for (items) |item| {
                                const value: T = try ana.cast_value_to(ast_node.location(), mode, .int(item), .data, T);
                                try current_segment.writer().writeInt(T, value, .little);
                                try number_styles.append(segment_allocator, .hex);
                            },
                            else => {
                                const value: T = try ana.cast_value_to(ast_node.location(), mode, container_value, .data, T);
                                try current_segment.writer().writeInt(T, value, .little);
                                try number_styles.append(segment_allocator, number_style(ast_node));
                            },
                        }
                    }
                    line_info.number_styles = try number_styles.toOwnedSlice(segment_allocator);
                },

                .encoded => {
                    const encoded = instr.instruction.?;

                    var output: u32 = encoded.binary;

                    const cond_code: ast.Condition.Code = if (instr.ast_node.condition) |condition|
                        condition.type.encode()
                    else if (std.ascii.eqlIgnoreCase(encoded.mnemonic, "NOP"))
                        .@"return" // NOP has a fixed all-zero encoding, including its condition bits.
                    else
                        encoded.default_condition;

                    const condition_slot: EncodedInstruction.Slot = comptime .from_mask(0xF000_0000);
                    try condition_slot.write(&output, @intFromEnum(cond_code));

                    if (instr.ast_node.effect) |effect| {
                        const write_mask = effect.type.get_write_mask();

                        if (write_mask.c) {
                            const slot = encoded.c_effect_slot orelse return error.BadInstructionEncoding;
                            slot.fill(&output);
                        }

                        if (write_mask.z) {
                            const slot = encoded.z_effect_slot orelse return error.BadInstructionEncoding;
                            slot.fill(&output);
                        }
                    }

                    var pc_delta: u32 = 1;
                    for (instr.arguments) |value| {
                        if (value.flags.augment)
                            pc_delta += 1;
                    }

                    const hub_pc: u32 = @intCast(current_segment.hub_offset + current_segment.len() + 4 * pc_delta);
                    const cog_pc: u32 = (instr.start_addr.?.get_local(.pc) orelse @panic("instruction has no execution PC")) + pc_delta;

                    const Augments = struct {
                        d: ?u23 = null,
                        s: ?u23 = null,
                        flip: bool = false,
                    };
                    var aug: Augments = .{};

                    for (instr.arguments, encoded.operands, instr.ast_node.arguments) |value, operand, ast_node| {
                        const location = ast_node.location();

                        var fill_extra_slot: ?EncodedInstruction.Slot = null;

                        const slot_value: u32 = if (operand.type == .enumeration) blk: {
                            // this is the most special case here:

                            if (value.value != .enumerator) {
                                try ana.emit_diag(location, .{
                                    .err_expected_value_type = .{
                                        .subject = "instruction operand",
                                        .expected = .enumerator,
                                        .actual = value.value,
                                    },
                                });
                                continue;
                            }

                            const lut: std.StaticStringMap(u32) = operand.type.enumeration;

                            const key = value.value.enumerator;

                            if (lut.get(key)) |index| {
                                break :blk index;
                            }

                            try ana.emit_diag(location, .{
                                .err_is_not_a_valid_enumerator = .{
                                    .key = key,
                                },
                            });
                            continue;
                        } else blk: {
                            const hint = value.flags.usage;

                            // TODO: Fix bug with instruction selection. "abs" on reg_or_imm has no PCrel set.
                            //       validate that this doesn't exist.
                            const address_space: TaggedAddress.AddrSpace = switch (operand.type) {
                                .address => .pc,
                                .reg_or_imm => |meta| if (meta.pcrel) .pc else .data,
                                else => .data,
                            };
                            const int: u32 = try ana.cast_value_to(location, current_segment.exec_mode, value, address_space, u32);

                            if (operand.type == .address and value.value == .address and value.value.address.local == .data)
                                try ana.emit_diag(location, .warn_branch_into_data);

                            const full_enc: u32 = switch (operand.type) {
                                .address => |meta|
                                // If R = 1 then PC += A, else PC = A. "\" forces R = 0.

                                selector: switch (value.flags.addressing) {
                                    .auto => {
                                        if (pic_mode == .avoid) continue :selector .absolute;
                                        // xq  — 11:29
                                        // do you happen to know how (or where) flexspin chooses
                                        // when to use abs/relative addressing?
                                        //
                                        // Wuerfel_21 — 12:02
                                        // It's "relative unless label is in a different memory space
                                        // OR code has explicit absolute backslash"

                                        switch (value.value) {
                                            .address => |address| {
                                                if (address.local == .data) continue :selector .absolute;
                                                std.log.debug("#{{/}}A auto mode translation: segment is #{}, mode is {}, target is {f}", .{
                                                    @intFromEnum(current_segment.id),
                                                    current_segment.exec_mode,
                                                    address,
                                                });

                                                if (current_segment.id == address.segment_id) {
                                                    // Always use relative addressing when in the *same* segment
                                                    continue :selector .relative;
                                                } else if (current_segment.exec_mode == .hub and address.local == .hub) {
                                                    // Use configurable behaviour between two hubexec sections
                                                    if (pic_mode == .prefer or pic_mode == .force or ana.options.use_label_relative_hub_to_hub_jmp) {
                                                        continue :selector .relative;
                                                    } else {
                                                        continue :selector .absolute;
                                                    }
                                                } else {
                                                    // Otherwise, use absolute addressing
                                                    continue :selector .absolute;
                                                }
                                                unreachable;
                                            },
                                            else => {
                                                const target_mode: eval.ExecMode = switch (int) {
                                                    0x000...0x1FF => .cog,
                                                    0x200...0x3FF => .lut,
                                                    else => .hub,
                                                };

                                                if (same_exec_domain(current_segment.exec_mode, target_mode)) {
                                                    std.log.debug("#{{/}}A: src mode={} address={f} address:int=0x{X:0>6} cog={} lut={} hub={}", .{
                                                        current_segment.exec_mode,
                                                        value,
                                                        int,
                                                        int < 0x200,
                                                        int >= 0x200 and int < 0x400,
                                                        int >= 0x400,
                                                    });
                                                    // Cog and LUT addresses share the same relative branch domain.
                                                    if (pic_mode == .prefer or pic_mode == .force or ana.options.use_relative_jmp_for_same_mode_nonlabel_address) {
                                                        continue :selector .relative;
                                                    } else {
                                                        continue :selector .absolute;
                                                    }
                                                } else {
                                                    // Crossing between cog/LUT and hub requires an absolute branch.
                                                    continue :selector .absolute;
                                                }
                                            },
                                        }
                                    },

                                    .absolute => {
                                        if (pic_mode == .force) try ana.emit_diag(location, .err_pic_requires_relative_address);
                                        fill_extra_slot = null;
                                        break :selector int;
                                    },

                                    .relative => {

                                        // The encoded relative A field is a signed BYTE displacement.
                                        // Instruction-relative S fields (e.g. JINT) use compute_rel() below.
                                        // The source operand is a destination address, whose units depend
                                        // on the execution domain; it is not an already computed offset.
                                        const byte_delta_i33: i33 = switch (value.value) {
                                            .address => |addr| delta: {
                                                // Labels in the same segment share its hub-to-local mapping,
                                                // so subtracting their hub byte addresses gives the displacement.
                                                const target_address = addr.hub_address orelse {
                                                    try ana.emit_diag(location, .{ .err_address_has_no_hub_location = .branch_target });
                                                    break :selector 0;
                                                };
                                                break :delta @as(i33, target_address) - @as(i33, hub_pc);
                                            },
                                            else => delta: {
                                                // Numeric $000..$1FF destinations index cog RAM longs;
                                                // $200..$3FF index LUT RAM longs. Thus $400 is the boundary
                                                // in LONG addresses, not a limit on byte displacements.
                                                if ((current_segment.exec_mode == .cog or current_segment.exec_mode == .lut) and int < 0x400) {
                                                    const target_pc_longs: i33 = int;
                                                    const next_pc_longs: i33 = cog_pc;
                                                    // Subtract in execution-PC units, then convert longs to bytes.
                                                    // The segment's hub storage origin does not enter this calculation.
                                                    break :delta (target_pc_longs - next_pc_longs) * 4;
                                                }
                                                // Hub destinations and the next hub PC are already byte addresses.
                                                break :delta @as(i33, int) - @as(i33, hub_pc);
                                            },
                                        };

                                        const byte_delta_i20: i20 = std.math.cast(i20, byte_delta_i33) orelse delta: {
                                            try ana.emit_diag(location, .{
                                                .err_branch_too_far = .{
                                                    .distance = byte_delta_i33,
                                                    .unit = .bytes,
                                                },
                                            });
                                            break :delta 0;
                                        };

                                        const byte_delta_u20: u20 = @bitCast(byte_delta_i20);

                                        fill_extra_slot = meta.rel;
                                        fill_extra_slot = meta.rel;
                                        break :selector byte_delta_u20;
                                    },
                                },

                                .immediate => |shift| switch (hint) {
                                    .literal => (int >> shift),
                                    .register => {
                                        try ana.emit_diag(location, .{ .err_operand_usage_mismatch = .{ .expected = .register, .actual = .immediate } });
                                        continue;
                                    },
                                },

                                .register => switch (hint) {
                                    .register => int,
                                    .literal => {
                                        try ana.emit_diag(location, .{ .err_operand_usage_mismatch = .{ .expected = .immediate, .actual = .register } });
                                        continue;
                                    },
                                },

                                .reg_or_imm => |meta| enc: {
                                    switch (hint) {
                                        .register => fill_extra_slot = null,
                                        .literal => fill_extra_slot = meta.imm,
                                    }

                                    if (hint == .register)
                                        break :enc int;

                                    if (meta.pcrel) {
                                        aug.flip = true; // flexspin flips the operands here for some reason
                                        break :enc try ana.compute_rel(location, current_segment.exec_mode, cog_pc, hub_pc, int, if (value.flags.augment)
                                            .augmented
                                        else
                                            .default);
                                    } else {
                                        break :enc int;
                                    }
                                },

                                .pointer_expr => |meta| enc: {
                                    switch (hint) {
                                        .register => fill_extra_slot = null,
                                        .literal => fill_extra_slot = meta.imm,
                                    }

                                    if (value.value == .pointer_expr) {
                                        // is always "immediate"
                                        fill_extra_slot = meta.imm;
                                    } else if (hint == .literal and int > (if (value.flags.augment) @as(u32, 0xFFFFF) else 255)) {
                                        try ana.emit_diag(location, .{
                                            .err_numeric_value_out_of_range = .{
                                                .subject = "pointer immediate",
                                                .min = 0,
                                                .max = if (value.flags.augment) 0xFFFFF else 255,
                                                .actual = int,
                                            },
                                        });
                                        continue;
                                    }

                                    break :enc int;
                                },

                                .pointer_reg => switch (hint) {
                                    .register => switch (int) {
                                        0x1F6, 0x1F7, 0x1F8, 0x1F9 => int - 0x1F6,

                                        else => {
                                            try ana.emit_diag(location, .{
                                                .err_register_not_allowed = .{
                                                    .actual = @enumFromInt(int),
                                                    .allowed = .pointer_operand,
                                                },
                                            });
                                            continue;
                                        },
                                    },

                                    .literal => {
                                        try ana.emit_diag(location, .{ .err_operand_usage_mismatch = .{ .expected = .register, .actual = .immediate } });
                                        continue;
                                    },
                                },

                                .enumeration => unreachable,
                            };

                            const enc: u32 = if (value.flags.augment) aug: {
                                const aug_dst: *?u23 = if (operand.slot.eql(.S))
                                    &aug.s
                                else if (operand.slot.eql(.D))
                                    &aug.d
                                else {
                                    try ana.emit_diag(location, .err_cannot_aug_operand);
                                    continue;
                                };

                                std.debug.assert(aug_dst.* == null);

                                const P = packed struct(u32) {
                                    enc: u9,
                                    aug: u23,
                                };
                                const p: P = @bitCast(full_enc);

                                aug_dst.* = p.aug;

                                break :aug p.enc;
                            } else full_enc;

                            const max_value = operand.slot.max_value();
                            if (enc > max_value) {
                                try ana.emit_diag(location, .{
                                    .err_numeric_value_out_of_range = .{
                                        .subject = "encoded operand",
                                        .min = 0,
                                        .max = max_value,
                                        .actual = enc,
                                    },
                                });
                                continue;
                            }

                            break :blk enc;
                        };

                        operand.slot.write(&output, slot_value) catch |err| switch (err) {
                            error.Overflow => try ana.emit_diag(location, .err_cannot_write_operand_integer_overflow),
                        };

                        if (operand.copy_to) |slot| {
                            slot.write(&output, slot_value) catch |err| switch (err) {
                                error.Overflow => try ana.emit_diag(location, .err_cannot_write_operand_integer_overflow),
                            };
                        }

                        if (fill_extra_slot) |slot| {
                            slot.fill(&output);
                        }
                    }

                    if (aug.flip and ana.options.flip_augs_on_pcrel) {
                        if (aug.s) |s| {
                            try current_segment.writer().writeInt(u32, @as(u32, 0b1111_1111000_000_000000000_000000000) | s, .little);
                        }
                        if (aug.d) |d| {
                            try current_segment.writer().writeInt(u32, @as(u32, 0b1111_1111100_000_000000000_000000000) | d, .little);
                        }
                    } else {
                        if (aug.d) |d| {
                            try current_segment.writer().writeInt(u32, @as(u32, 0b1111_1111100_000_000000000_000000000) | d, .little);
                        }
                        if (aug.s) |s| {
                            try current_segment.writer().writeInt(u32, @as(u32, 0b1111_1111000_000_000000000_000000000) | s, .little);
                        }
                    }

                    try current_segment.writer().writeInt(u32, output, .little);
                },
            }
        }

        if (current_segment.len() > 0) {
            try segments.append(segment_allocator, .{
                .id = current_segment.id,
                .hub_offset = @intCast(current_segment.hub_offset),
                .data = try current_segment.toOwnedSlice(),
                .exec_mode = current_segment.exec_mode,
            });
        }

        for (segments.items, 0..) |left, i| {
            for (segments.items[0..i]) |right| {
                const start = @max(left.hub_offset, right.hub_offset);
                const end = @min(left.hub_offset + left.data.len, right.hub_offset + right.data.len);
                if (start < end) try ana.emit_diag(null, .{
                    .err_segments_and_overlap_at_hub_address_0x_x_0_5 = .{
                        .left = @intFromEnum(left.id),
                        .right = @intFromEnum(right.id),
                        .start = start,
                    },
                });
            }
        }

        return segments.toOwnedSlice(segment_allocator);
    }

    const RelativeAddressing = enum {
        default, // 9 bit, counts instructions
        augmented, // 20 bit, counts instructions
    };

    fn compute_rel(ana: *Analyzer, loc: ast.Location, exec_mode: eval.ExecMode, cog_pc: u32, hub_pc: u32, int: u32, mode: RelativeAddressing) !u32 {
        const delta33: i33 = switch (exec_mode) {
            .cog, .lut => @as(i33, int) - @as(i33, cog_pc),
            .hub => @divTrunc(@as(i33, int) - @as(i33, 0x400 + (hub_pc -| 0x400)), 4),
            .regspace, .data => unreachable,
        };

        logger.debug("pcrel: cog={} hub={} target={}:{t} => rel {}", .{ cog_pc, hub_pc, int, exec_mode, delta33 });

        switch (mode) {
            .default => {
                const delta9: i9 = std.math.cast(i9, delta33) orelse {
                    try ana.emit_diag(loc, .{
                        .err_branch_too_far = .{
                            .distance = delta33,
                            .unit = .instructions,
                        },
                    });
                    return 0;
                };

                const udelta9: u9 = @bitCast(delta9);

                return udelta9;
            },
            .augmented => {
                // somehow the augmented delta only uses 18 bits of PC ?

                const delta20: i20 = std.math.cast(i20, delta33) orelse {
                    try ana.emit_diag(loc, .{
                        .err_branch_too_far = .{
                            .distance = delta33,
                            .unit = .instructions,
                        },
                    });
                    return 0;
                };

                const udelta20: u20 = @bitCast(delta20);

                return udelta20;
            },
        }
    }

    fn cast_value_to(ana: *Analyzer, location: ast.Location, exec_mode: eval.ExecMode, value: Value, address_space: TaggedAddress.AddrSpace, comptime U: type) !U {
        const cast = try ana.cast_value_to_2(location, exec_mode, value, address_space, U);
        logger.debug("cast {f} to {}", .{ value, cast });
        return cast;
    }

    fn cast_value_to_2(ana: *Analyzer, location: ast.Location, exec_mode: eval.ExecMode, value: Value, address_space: TaggedAddress.AddrSpace, comptime U: type) !U {
        const I = std.meta.Int(.signed, @bitSizeOf(U));

        const raw_value: i64 = switch (value.value) {
            .int => |int| int,
            .address => |offset| try ana.get_offset_for_exec_mode(location, offset, exec_mode, address_space),
            .string, .sequence, .enumerator => {
                try ana.emit_diag(location, .{
                    .err_expected_value_type = .{
                        .subject = "data operand",
                        .expected = .int,
                        .actual = value.value,
                    },
                });
                return 0;
            },
            .register => |reg| @intFromEnum(reg),

            .pointer_expr => |ptr_expr| try ana.encode_ptr_expr(location, ptr_expr, value.flags.augment),
        };

        if (raw_value < 0) {
            const cast_val: I = @truncate(raw_value);
            if (cast_val != raw_value) {
                try ana.emit_diag(location, .{
                    .warn_integer_was_truncated_to_bits_expected_emitted = .{
                        .bits = @bitSizeOf(U),
                        .expected = raw_value,
                        .emitted = cast_val,
                    },
                });
            }
            return @bitCast(cast_val);
        } else {
            const cast_val: U = @truncate(@as(u64, @bitCast(raw_value)));
            if (cast_val != raw_value) {
                try ana.emit_diag(location, .{
                    .warn_integer_was_truncated_to_bits_expected_emitted = .{
                        .bits = @bitSizeOf(U),
                        .expected = raw_value,
                        .emitted = cast_val,
                    },
                });
            }
            return cast_val;
        }
    }

    fn encode_ptr_expr(ana: *Analyzer, location: ast.Location, expr: eval.PointerExpression, augmented: bool) !u32 {
        const ptr_mask = 0b1_0000_0000 | @as(u9, @intFromEnum(expr.pointer)) << 7;
        const opcode_mask: u7 = switch (expr.increment) {
            .none => 0b000_0000,
            .pre_increment => 0b100_0000, // ++PTRx
            .pre_decrement => 0b101_0000, // --PTRx
            .post_increment => 0b110_0000, // PTRx++
            .post_decrement => 0b111_0000, // PTRx--
        };
        const index: i64 = expr.index orelse switch (expr.increment) {
            .none => 0,
            .pre_decrement, .post_decrement => 1,
            .pre_increment, .post_increment => 1,
        };
        if (augmented) {
            const min: i64 = if (expr.increment == .none) -0x80000 else 1;
            const max: i64 = switch (expr.increment) {
                .none => 0xFFFFF,
                .pre_increment, .post_increment => 0x7FFFF,
                .pre_decrement, .post_decrement => 0x80000,
            };
            if (index < min or index > max) {
                try ana.emit_diag(location, .{ .err_numeric_value_out_of_range = .{
                    .subject = "pointer index",
                    .min = min,
                    .max = max,
                    .actual = index,
                } });
                return 0;
            }

            const offset: i64 = switch (expr.increment) {
                .pre_decrement, .post_decrement => -index,
                else => index,
            };
            const bits: u32 = @intCast(@as(u64, @bitCast(offset)) & 0xFFFFF);
            return (@as(u32, ptr_mask | (opcode_mask & 0b110_0000)) << 15) | bits;
        }

        const enc_index: u6 = switch (expr.increment) {
            .none => blk: {
                const index6: i6 = std.math.cast(i6, index) orelse err: {
                    try ana.emit_diag(location, .{
                        .err_numeric_value_out_of_range = .{
                            .subject = "pointer index",
                            .min = std.math.minInt(i6),
                            .max = std.math.maxInt(i6),
                            .actual = index,
                        },
                    });
                    break :err 0;
                };

                break :blk @bitCast(index6);
            },

            .pre_decrement,
            .post_decrement,
            .pre_increment,
            .post_increment,
            => blk: {
                const min = 1;
                const max = 16;
                if (index < min or index > max) {
                    try ana.emit_diag(location, .{
                        .err_numeric_value_out_of_range = .{
                            .subject = "pointer index",
                            .min = min,
                            .max = max,
                            .actual = index,
                        },
                    });
                    break :blk 0;
                }

                // wrap 16 to 0
                const encoded: u4 = @intCast(index & 0xF);

                break :blk switch (expr.increment) {
                    .pre_decrement, .post_decrement => if (encoded == 0)
                        0b1_0000 // special case for "-16" which is encoded as
                    else
                        @as(u5, @bitCast(-@as(i5, encoded))),
                    .pre_increment, .post_increment => encoded,
                    .none => unreachable,
                };
            },
        };

        return ptr_mask | opcode_mask | enc_index;
    }

    fn get_offset_for_exec_mode(ana: *Analyzer, location: ast.Location, offset: TaggedAddress, mode: eval.ExecMode, address_space: TaggedAddress.AddrSpace) !u32 {
        if (offset.local == .data) return offset.hub_address.?;
        if (address_space == .data) return offset.get_local(.data) orelse @panic("address has no data value");
        const target_mode: eval.ExecMode = switch (offset.local) {
            .hub => .hub,
            .cog, .regspace => .cog,
            .lut => .lut,
            .data => unreachable,
        };
        if (target_mode != mode and (mode == .hub or target_mode == .hub))
            try ana.emit_diag(location, .{
                .warn_jump_between_exec_modes = .{
                    .source_mode = mode,
                    .target_mode = target_mode,
                },
            });

        return offset.get_local(.pc) orelse @panic("address has no execution PC");
    }

    const EvalError = error{
        OutOfMemory,
        UndefinedSymbol,
        InvalidFunctionCall,
        Overflow,
        DivideByZero,
        InvalidArg,
        TypeMismatch,
        DiagnosedFailure,
        CyclicConstant,
        ConstantNeedsLabelDuringLayout,
    };

    const EvalContext = struct {
        after: ?TaggedAddress = null,
        after_pc: ?u32 = null,
        start: ?TaggedAddress = null,
        fit_pc: ?u32 = null,
        exec_mode: ?eval.ExecMode = null,
        constant: bool = false,
        forbid_label_addresses: bool = false,
        allow_augment: bool = false,
    };

    fn evaluate_root_expr(ana: *Analyzer, expr: ast.Expression, context: EvalContext) EvalError!eval.Value {
        return ana.evaluate_expr(expr, context, 0);
    }

    fn evaluate_expr(ana: *Analyzer, expr: ast.Expression, context: EvalContext, nesting: usize) EvalError!eval.Value {
        switch (expr) {
            .wrapped => |inner| return try ana.evaluate_expr(inner.value.*, context, nesting),
            .current_pc => |span| {
                const location = span.location();
                if (context.constant) {
                    try ana.emit_diag(location, .err_current_pc_unavailable);
                    return error.DiagnosedFailure;
                }
                if (context.fit_pc) |pc| return .int(pc);
                const address = context.start orelse {
                    try ana.emit_diag(location, .err_current_pc_unavailable);
                    return .int(0);
                };
                return switch (address.local) {
                    .cog, .lut => .int(address.get_local(.pc).?),
                    .hub, .data => .int(address.hub_address orelse {
                        try ana.emit_diag(location, .err_current_pc_unavailable);
                        return .int(0);
                    }),
                    .regspace => blk: {
                        try ana.emit_diag(location, .err_current_pc_unavailable);
                        break :blk .int(0);
                    },
                };
            },
            .integer => |int| return .int(int.value),
            .string => |string| return .string(string.value),
            .sequence => |seq| {
                var values: std.ArrayList(i64) = .empty;
                for (seq.items) |item| {
                    const value = try ana.evaluate_expr(item, context, nesting + 1);
                    switch (value.value) {
                        .int => |int| try values.append(ana.arena.allocator(), int),
                        .string => |str| for (str) |byte| try values.append(ana.arena.allocator(), byte),
                        .sequence => |items| try values.appendSlice(ana.arena.allocator(), items),
                        .address => |addr| {
                            const start = context.start orelse return error.TypeMismatch;
                            const mode: eval.ExecMode = if (start.local == .data) .hub else std.meta.activeTag(start.local);
                            const number = try ana.get_offset_for_exec_mode(item.location(), addr, mode, .data);
                            try values.append(ana.arena.allocator(), number);
                        },
                        else => {
                            try ana.emit_diag(item.location(), .{ .err_expected_value_type = .{ .subject = "array item", .expected = .int, .actual = value.value } });
                            return error.DiagnosedFailure;
                        },
                    }
                }
                return .sequence(try values.toOwnedSlice(ana.arena.allocator()));
            },
            .enumerator => |enumerator| return .enumerator(enumerator.symbol_name),
            .symbol => |symref| {
                const sym = ana.get_label_info(symref.symbol_name, symref.local_scope) catch unreachable;

                return switch (sym.type) {
                    .undefined => return error.UndefinedSymbol,
                    .code => if (context.forbid_label_addresses) error.ConstantNeedsLabelDuringLayout else .address(sym.offset orelse return error.UndefinedSymbol, .literal),
                    .data => if (context.forbid_label_addresses) error.ConstantNeedsLabelDuringLayout else .address(sym.offset orelse return error.UndefinedSymbol, .register),
                    .constant => sym.value orelse try ana.resolve_constant(symref.symbol_name),
                    .builtin => sym.value.?,
                };
            },

            .unary_transform => |op| {
                const value = try ana.evaluate_expr(op.value.*, context, nesting + 1);

                switch (op.operator) {
                    .post_decrement,
                    .post_increment,
                    .pre_decrement,
                    .pre_increment,
                    => {
                        var ptr_expr: eval.PointerExpression = switch (value.value) {
                            .register => |reg| try ana.ptr_expr_from_reg(op.operator_span.location(), reg),

                            .pointer_expr => |ptr_expr| ptr_expr,

                            else => blk: {
                                try ana.emit_diag(op.operator_span.location(), .{
                                    .err_operator_invalid_operand_type = .{
                                        .operator = .{ .unary = op.operator },
                                        .value_type = value.value,
                                    },
                                });

                                break :blk .{
                                    .pointer = .PTRA,
                                    .increment = .none,
                                    .index = null,
                                };
                            },
                        };

                        if (ptr_expr.index != null and ptr_expr.increment != .none) {
                            try ana.emit_diag(op.operator_span.location(), .{
                                .err_pointer_modifier_already_set = .{
                                    .operator = .{ .unary = op.operator },
                                    .modifier = .index,
                                },
                            });
                        } else if (ptr_expr.increment != .none) {
                            try ana.emit_diag(op.operator_span.location(), .{
                                .err_pointer_modifier_already_set = .{
                                    .operator = .{ .unary = op.operator },
                                    .modifier = .increment,
                                },
                            });
                        }

                        switch (op.operator) {
                            .pre_decrement => ptr_expr.increment = .pre_decrement,
                            .post_decrement => ptr_expr.increment = .post_decrement,
                            .pre_increment => ptr_expr.increment = .pre_increment,
                            .post_increment => ptr_expr.increment = .post_increment,
                            else => unreachable,
                        }

                        return .{
                            .value = .{ .pointer_expr = ptr_expr },
                            .flags = value.flags,
                        };
                    },
                    else => {},
                }

                if (value.value == .register) {
                    try ana.emit_diag(op.operator_span.location(), .{
                        .err_operator_invalid_operand_type = .{
                            .operator = .{ .unary = op.operator },
                            .value_type = .register,
                        },
                    });
                    return value;
                }
                if (value.value == .enumerator) {
                    try ana.emit_diag(op.operator_span.location(), .{
                        .err_operator_invalid_operand_type = .{
                            .operator = .{ .unary = op.operator },
                            .value_type = .enumerator,
                        },
                    });
                    return value;
                }

                switch (op.operator) {
                    .post_decrement,
                    .post_increment,
                    .pre_decrement,
                    .pre_increment,
                    => unreachable,

                    .@"!", .@"~", .@"+", .@"-" => {
                        if (value.value != .int) {
                            try ana.emit_diag(op.operator_span.location(), .{
                                .err_operator_invalid_operand_type = .{
                                    .operator = .{ .unary = op.operator },
                                    .value_type = value.value,
                                },
                            });
                            return .int(0);
                        }
                        return switch (op.operator) {
                            .@"!" => .int(@intFromBool(value.value.int == 0)),
                            .@"~" => .int(~value.value.int),
                            .@"+" => value,
                            .@"-" => .int(-%value.value.int),
                            else => unreachable,
                        };
                    },
                    .@"@" => {
                        if (value.value != .address) {
                            try ana.emit_diag(op.operator_span.location(), .{
                                .err_operator_invalid_operand_type = .{
                                    .operator = .{ .unary = op.operator },
                                    .value_type = value.value,
                                },
                            });
                            return .int(0);
                        }

                        const local_offset: TaggedAddress = context.after orelse {
                            try ana.emit_diag(op.operator_span.location(), .err_operator_at_cannot_be_used_in_this_scope);
                            return .int(0);
                        };
                        const target_offset: TaggedAddress = value.value.address;

                        const mode: eval.ExecMode = local_offset.local;
                        if (target_offset.local != mode) {
                            try ana.emit_diag(op.operator_span.location(), .{ .err_address_space_mismatch = .{
                                .subject = "@",
                                .expected = .initOne(mode),
                                .actual = .initOne(std.meta.activeTag(target_offset.local)),
                            } });
                            return .int(0);
                        }
                        if (mode == .data or mode == .regspace) {
                            try ana.emit_diag(op.operator_span.location(), .err_address_has_no_execution_pc);
                            return .int(0);
                        }
                        const local_pc = context.after_pc orelse local_offset.get_local(.pc).?;
                        const target_pc = target_offset.get_local(.pc).?;
                        const local_bytes: i64 = if (mode == .hub) local_pc else @as(i64, local_pc) * 4 + local_offset.subreg_byte;
                        const target_bytes: i64 = if (mode == .hub) target_pc else @as(i64, target_pc) * 4 + target_offset.subreg_byte;

                        const jmp_delta = target_bytes - local_bytes;

                        if (@mod(jmp_delta, 4) != 0) {
                            try ana.emit_diag(op.operator_span.location(), .err_address_delta_not_divisible_by_four);
                            return .int(0);
                        }
                        if (local_offset.segment_id != target_offset.segment_id)
                            try ana.emit_diag(op.operator_span.location(), .warn_relative_address_crosses_segments);

                        return .int(@divTrunc(jmp_delta, 4));
                    },
                    .@"*" => {
                        if (value.value != .address) {
                            try ana.emit_diag(op.operator_span.location(), .{
                                .err_operator_invalid_operand_type = .{
                                    .operator = .{ .unary = op.operator },
                                    .value_type = value.value,
                                },
                            });
                            return .address(if (context.after) |addr|
                                addr
                            else
                                .init_hub(undefined, 0), .literal);
                        }
                        if (value.flags.usage == .register) {
                            try ana.emit_diag(op.operator_span.location(), .{ .warn_operator_no_effect = .{ .operator = op.operator, .label = .data } });
                        }
                        return .{
                            .value = value.value,
                            .flags = .{
                                .usage = .register,
                                .augment = value.flags.augment,
                                .addressing = value.flags.addressing,
                            },
                        };
                    },
                    .@"&" => {
                        if (value.value != .address) {
                            try ana.emit_diag(op.operator_span.location(), .{
                                .err_operator_invalid_operand_type = .{
                                    .operator = .{ .unary = op.operator },
                                    .value_type = value.value,
                                },
                            });
                            return .address(.init_hub(undefined, 0), .literal);
                        }
                        if (value.flags.usage == .literal) {
                            try ana.emit_diag(op.operator_span.location(), .{ .warn_operator_no_effect = .{ .operator = op.operator, .label = .code } });
                        }
                        return .{
                            .value = value.value,
                            .flags = .{
                                .usage = .literal,
                                .augment = value.flags.augment,
                                .addressing = value.flags.addressing,
                            },
                        };
                    },
                }
            },
            .binary_transform => |op| {
                const lhs = try ana.evaluate_expr(op.lhs.*, context, nesting + 1);
                var index_expr = op.rhs.*;
                while (index_expr == .wrapped) index_expr = index_expr.wrapped.value.*;
                const augmented_index = op.operator == .array_index and
                    (lhs.value == .pointer_expr or (lhs.value == .register and (lhs.value.register == PTRA or lhs.value.register == PTRB))) and
                    index_expr == .function_call and
                    ana.has_augment(index_expr);
                const rhs = try ana.evaluate_expr(if (augmented_index) index_expr else op.rhs.*, context, if (augmented_index) 0 else nesting + 1);

                const lhs_type: Value.Type = lhs.value;
                const rhs_type: Value.Type = rhs.value;

                if (op.operator == .array_index) {
                    const lhs_ok = (lhs_type == .register or lhs_type == .pointer_expr);
                    const rhs_ok = (rhs_type == .int);

                    if (!lhs_ok or !rhs_ok) {
                        try ana.emit_diag(op.operator_span.location(), .{
                            .err_operator_invalid_operand_types = .{
                                .operator = .{ .binary = op.operator },
                                .lhs_type = lhs_type,
                                .rhs_type = rhs_type,
                            },
                        });
                        return .{
                            .flags = lhs.flags,
                            .value = .{
                                .pointer_expr = .{ .increment = .none, .pointer = .PTRA, .index = null },
                            },
                        };
                    }

                    const src_expr: eval.PointerExpression = switch (lhs.value) {
                        .pointer_expr => |ptr_expr| ptr_expr,
                        .register => |reg| try ana.ptr_expr_from_reg(op.operator_span.location(), reg),
                        else => unreachable,
                    };
                    if (src_expr.index != null) {
                        try ana.emit_diag(op.operator_span.location(), .{ .err_pointer_modifier_already_set = .{ .operator = .{ .binary = op.operator }, .modifier = .index } });
                    }

                    var dst_expr = src_expr;
                    dst_expr.index = rhs.value.int;

                    return .{
                        .value = .{ .pointer_expr = dst_expr },
                        .flags = .{
                            .addressing = lhs.flags.addressing,
                            .usage = lhs.flags.usage,
                            .augment = rhs.flags.augment,
                        },
                    };
                }

                if (op.operator == .@"*") {
                    if (lhs_type == .int and (rhs_type == .string or rhs_type == .sequence))
                        return try ana.repeat_value(op.operator_span.location(), rhs, lhs.value.int);
                    if (rhs_type == .int and (lhs_type == .string or lhs_type == .sequence))
                        return try ana.repeat_value(op.operator_span.location(), lhs, rhs.value.int);
                }

                if (lhs_type != rhs_type) {
                    try ana.emit_diag(op.operator_span.location(), .{
                        .err_operator_invalid_operand_types = .{
                            .operator = .{ .binary = op.operator },
                            .lhs_type = lhs_type,
                            .rhs_type = rhs_type,
                        },
                    });
                    return .int(0);
                }

                switch (lhs_type) {
                    .int => return .int(
                        try ana.execute_int_op(op.operator_span.location(), lhs.value.int, rhs.value.int, op.operator),
                    ),
                    .register => {
                        try ana.emit_diag(op.operator_span.location(), .{
                            .err_operator_invalid_operand_type = .{
                                .operator = .{ .binary = op.operator },
                                .value_type = .register,
                            },
                        });
                        return .register(0);
                    },
                    .enumerator => {
                        try ana.emit_diag(op.operator_span.location(), .{
                            .err_operator_invalid_operand_type = .{
                                .operator = .{ .binary = op.operator },
                                .value_type = .enumerator,
                            },
                        });
                        return .enumerator("");
                    },
                    .pointer_expr => {
                        try ana.emit_diag(op.operator_span.location(), .{
                            .err_operator_invalid_operand_type = .{
                                .operator = .{ .binary = op.operator },
                                .value_type = .pointer_expr,
                            },
                        });
                        return .enumerator("");
                    },
                    .address, .string, .sequence => {
                        try ana.emit_diag(op.operator_span.location(), .{
                            .err_operator_invalid_operand_type = .{
                                .operator = .{ .binary = op.operator },
                                .value_type = lhs_type,
                            },
                        });
                        return .int(0);
                    },
                }
            },
            .function_call => |fncall| {
                const func = ana.get_function(fncall.function) orelse {
                    try ana.emit_diag(fncall.span.location(), .{ .err_unknown_function = .{
                        .function = fncall.function,
                    } });
                    return error.DiagnosedFailure;
                };

                const params = func.get_parameters();

                const argv_res = try ana.map_function_args(
                    fncall,
                    params,
                    context,
                    nesting,
                );
                const argv = argv_res.constSlice();
                std.debug.assert(argv.len == params.len);

                switch (func.*) {
                    .user => |f| {
                        const ctx: FunctionCallContext = .{
                            .ana = ana,
                            .location = fncall.span.location(),
                        };

                        return f.invoke(ctx, argv) catch |err| switch (err) {
                            error.InvalidArgCount => unreachable, // we check that before
                            else => |e| return e,
                        };
                    },

                    .aug => {
                        std.debug.assert(argv.len == 1);
                        if (nesting != 0) {
                            try ana.emit_diag(fncall.arguments[0].span.location(), .{ .err_function_must_be_root = .{ .function = "aug" } });
                            return error.DiagnosedFailure;
                        }
                        if (!context.allow_augment) {
                            try ana.emit_diag(fncall.span.location(), .err_augmentation_requires_instruction_operand);
                            return error.DiagnosedFailure;
                        }
                        var value = argv[0];
                        if (value.value == .pointer_expr) {
                            try ana.emit_diag(fncall.arguments[0].span.location(), .err_augmentation_requires_pointer_index);
                            return error.DiagnosedFailure;
                        }
                        if (value.flags.usage != .literal or (value.value != .int and value.value != .address)) {
                            try ana.emit_diag(fncall.arguments[0].span.location(), .err_augmentation_requires_immediate);
                            return error.DiagnosedFailure;
                        }
                        value.flags.augment = true;
                        return value;
                    },

                    .nrel => {
                        std.debug.assert(argv.len == 1);
                        if (nesting != 0) {
                            try ana.emit_diag(fncall.arguments[0].span.location(), .{ .err_function_must_be_root = .{ .function = "nrel" } });
                        }
                        var value = argv[0];
                        value.flags.addressing = .absolute;
                        return value;
                    },

                    .byteoffset, .wordoffset => {
                        std.debug.assert(argv.len == 1);
                        const value = argv[0];
                        if (value.value != .address) {
                            try ana.emit_diag(fncall.arguments[0].span.location(), .{ .err_expected_value_type = .{
                                .subject = fncall.function,
                                .expected = .address,
                                .actual = value.value,
                            } });
                            return .int(0);
                        }
                        const address = value.value.address;

                        const byte_offset: u2 = switch (address.local) {
                            .cog, .lut, .regspace => address.subreg_byte,
                            .hub, .data => {
                                try ana.emit_diag(fncall.arguments[0].span.location(), .{ .err_address_space_mismatch = .{
                                    .subject = fncall.function,
                                    .expected = .initMany(&.{ .cog, .lut, .regspace }),
                                    .actual = .initOne(std.meta.activeTag(address.local)),
                                } });
                                return .int(0);
                            },
                        };
                        return .int(if (func.* == .byteoffset) byte_offset else byte_offset / 2);
                    },

                    .cogaddr, .lutaddr, .localaddr => {
                        const address_function: diagnostics.AddressFunction = switch (func.*) {
                            .cogaddr => .cogaddr,
                            .lutaddr => .lutaddr,
                            .localaddr => .localaddr,
                            else => unreachable,
                        };
                        const loc = fncall.arguments[0].span.location();
                        std.debug.assert(argv.len == 1);
                        const value = argv[0];
                        switch (value.value) {
                            .string, .sequence, .enumerator, .pointer_expr => {
                                try ana.emit_diag(fncall.arguments[0].span.location(), .{
                                    .err_address_function_invalid_operand_type = .{
                                        .function = address_function,
                                        .value_type = value.value,
                                    },
                                });
                                return value;
                            },

                            .int => {
                                try ana.emit_diag(loc, .{
                                    .warn_address_function_expected_offset = .{
                                        .function = address_function,
                                        .value_type = value.value,
                                    },
                                });
                                return value;
                            },

                            .register => |reg| {
                                if (func.* == .lutaddr) {
                                    try ana.emit_diag(loc, .{ .err_address_function_invalid_operand_type = .{ .function = .lutaddr, .value_type = .register } });
                                    return .int(0);
                                } else if (func.* == .localaddr) {
                                    const mode = context.exec_mode orelse if (context.start) |start| std.meta.activeTag(start.local) else null;
                                    if (mode != .cog) {
                                        try ana.emit_diag(loc, .err_localaddr_is_only_valid_for_registers_in_a_cogexec_scope);
                                        return .int(0);
                                    }
                                }

                                try ana.emit_diag(fncall.arguments[0].span.location(), .{
                                    .warn_address_function_expected_offset = .{
                                        .function = address_function,
                                        .value_type = value.value,
                                    },
                                });
                                return .int(@intFromEnum(reg));
                            },

                            .address => |offset| {
                                const maybe_expected_type: ?eval.ExecMode = switch (func.*) {
                                    .lutaddr => .lut,
                                    .cogaddr => .cog,
                                    .localaddr => null,
                                    else => unreachable,
                                };

                                if (maybe_expected_type) |expected| {
                                    if (offset.local != expected and !(expected == .cog and offset.local == .regspace)) {
                                        try ana.emit_diag(fncall.arguments[0].span.location(), .{
                                            .err_expected_offset_of_type_but_got_type = .{
                                                .function = address_function,
                                                .expected_type = expected,
                                                .actual_type = offset.local,
                                            },
                                        });
                                        return .int(0);
                                    }
                                }

                                const local = offset.get_local(.data) orelse {
                                    try ana.emit_diag(loc, .err_address_has_no_execution_pc);
                                    return .int(0);
                                };
                                return .int(local);
                            },
                        }
                    },

                    .hubaddr => {
                        std.debug.assert(argv.len == 1);
                        const value = argv[0];
                        switch (value.value) {
                            .int => {
                                try ana.emit_diag(fncall.arguments[0].span.location(), .{
                                    .warn_address_function_expected_offset = .{
                                        .function = .hubaddr,
                                        .value_type = value.value,
                                    },
                                });
                                return value;
                            },
                            .string, .sequence, .register, .enumerator, .pointer_expr => {
                                try ana.emit_diag(fncall.arguments[0].span.location(), .{
                                    .err_address_function_invalid_operand_type = .{
                                        .function = .hubaddr,
                                        .value_type = value.value,
                                    },
                                });
                                return value;
                            },
                            .address => |address| {
                                const hub = address.hub_address orelse {
                                    try ana.emit_diag(fncall.arguments[0].span.location(), .{ .err_address_has_no_hub_location = .hubaddr_argument });
                                    return .int(0);
                                };
                                return .int(hub);
                            },
                        }
                    },
                }
            },
        }
    }

    fn ptr_expr_from_reg(ana: *Analyzer, location: ast.Location, reg: eval.Register) !eval.PointerExpression {
        return .{
            .pointer = switch (reg) {
                PTRA => .PTRA,
                PTRB => .PTRB,
                else => blk: {
                    try ana.emit_diag(location, .{
                        .err_register_not_allowed = .{
                            .actual = reg,
                            .allowed = .pointer_expression,
                        },
                    });
                    break :blk .PTRA;
                },
            },
            .increment = .none,
            .index = null,
        };
    }

    fn execute_int_op(ana: *Analyzer, location: ast.Location, lhs: i64, rhs: i64, op: ast.BinaryOperator) !i64 {
        _ = location;
        _ = ana;
        return switch (op) {
            .@"and" => @intFromBool((lhs != 0) and (rhs != 0)),
            .@"or" => @intFromBool((lhs != 0) or (rhs != 0)),
            .xor => @intFromBool((lhs != 0) != (rhs != 0)),
            .@"==" => @intFromBool(lhs == rhs),
            .@"!=" => @intFromBool(lhs != rhs),
            .@"<=>" => if (lhs < rhs) -1 else if (lhs > rhs) 1 else 0,
            .@"<" => @intFromBool(lhs < rhs),
            .@">" => @intFromBool(lhs > rhs),
            .@"<=" => @intFromBool(lhs <= rhs),
            .@">=" => @intFromBool(lhs >= rhs),
            .@"+" => (lhs +% rhs),
            .@"-" => (lhs -% rhs),
            .@"|" => (lhs | rhs),
            .@"^" => (lhs ^ rhs),
            .@">>" => if (std.math.cast(u6, rhs)) |shift| (lhs >> shift) else return error.Overflow,
            .@"<<" => if (std.math.cast(u6, rhs)) |shift| (lhs << shift) else return error.Overflow,
            .@"&" => lhs & rhs,
            .@"*" => lhs *% rhs,
            .@"/" => if (rhs == 0)
                return error.DivideByZero
            else if (lhs == std.math.minInt(i64) and rhs == -1)
                return error.Overflow
            else
                @divFloor(lhs, rhs),
            .@"%" => if (rhs == 0) return error.DivideByZero else if (rhs == -1) 0 else @mod(lhs, rhs),
            .array_index => unreachable,
        };
    }

    const max_supported_parameters = 16;

    fn map_function_args(
        ana: *Analyzer,
        fncall: ast.FunctionInvocation,
        params: []const Function.Parameter,
        context: EvalContext,
        nesting: usize,
    ) !BoundedArray(Value, max_supported_parameters) {
        if (params.len > max_supported_parameters)
            @panic("BUG: argument storage buffer is too small");

        const first_kwarg_index: usize = for (fncall.arguments, 0..) |arg, i| {
            if (arg.name != null)
                break i;
        } else fncall.arguments.len;

        const default_arg_count = blk: {
            var cnt: usize = 0;
            for (params) |p| {
                if (p.default_value != null)
                    cnt += 1;
            }
            break :blk cnt;
        };

        {
            var ok = true;
            for (fncall.arguments[first_kwarg_index..]) |arg| {
                if (arg.name == null) {
                    try ana.emit_diag(arg.span.location(), .err_positional_after_named_argument);
                    ok = false;
                }
            }

            if (fncall.arguments.len < params.len - default_arg_count or fncall.arguments.len > params.len) {
                try ana.emit_diag(fncall.span.location(), .{
                    .err_argument_count_mismatch = .{
                        .subject = fncall.function,
                        .min = params.len - default_arg_count,
                        .max = params.len,
                        .found = fncall.arguments.len,
                    },
                });
                ok = false;
            }

            if (!ok)
                return error.InvalidFunctionCall;
        }

        var argv: BoundedArray(Value, max_supported_parameters) = .{};
        var argv_ok: std.bit_set.IntegerBitSet(max_supported_parameters) = .initEmpty();

        argv.resize(params.len) catch unreachable; // we asserted capacity above

        for (argv.slice(), params, 0..) |*arg, param, index| {
            arg.* = param.default_value orelse continue;
            argv_ok.set(index);
        }

        const pos_argin = fncall.arguments[0..first_kwarg_index];
        const kw_argin = fncall.arguments[first_kwarg_index..];

        const pos_argv = argv.slice()[0..first_kwarg_index];
        const kw_argv = argv.slice()[first_kwarg_index..];

        const pos_params = params[0..first_kwarg_index];
        const kw_params = params[first_kwarg_index..];

        std.debug.assert(pos_argv.len == pos_params.len);
        std.debug.assert(kw_argv.len >= kw_params.len);

        std.debug.assert(pos_argv.len == pos_argin.len);
        std.debug.assert(kw_argv.len >= kw_argin.len);

        for (pos_argv, pos_argin, 0..) |*value, arg, index| {
            std.debug.assert(arg.name == null);
            value.* = try ana.evaluate_expr(arg.value, context, nesting + 1);
            argv_ok.set(index);
        }

        {
            var ok = true;
            for (kw_argin) |arg| {
                std.debug.assert(arg.name != null);

                const index = index_of_param(params, arg.name.?) orelse {
                    ok = false;
                    try ana.emit_diag(arg.span.location(), .{
                        .err_has_no_parameter_named = .{
                            .function = fncall.function,
                            .parameter = arg.name.?,
                        },
                    });
                    continue;
                };
                if (index < first_kwarg_index) {
                    ok = false;
                    try ana.emit_diag(arg.span.location(), .{
                        .err_parameter_already_passed = .{
                            .parameter = arg.name.?,
                            .function = fncall.function,
                            .previous = .{ .positional = index },
                        },
                    });
                    continue;
                }
            }
            for (kw_argin, 0..) |arg1, i| {
                for (kw_argin[i + 1 ..]) |arg2| {
                    if (std.mem.eql(u8, arg1.name.?, arg2.name.?)) {
                        ok = false;
                        try ana.emit_diag(arg2.span.location(), .{
                            .err_parameter_already_passed = .{
                                .parameter = arg1.name.?,
                                .function = fncall.function,
                                .previous = .{ .named = arg1.span.location() },
                            },
                        });
                    }
                }
            }

            if (!ok)
                return error.InvalidFunctionCall;
        }

        // If we reached here, we don't have duplicates, and we don't have undefined parameters,
        // which means we have a 1:1 mapping of all parameters.
        for (kw_argin) |arg| {
            std.debug.assert(arg.name != null);

            const index = index_of_param(params, arg.name.?).? - first_kwarg_index;
            kw_argv[index] = try ana.evaluate_expr(arg.value, context, nesting + 1);
            argv_ok.set(index + first_kwarg_index);
        }

        {
            var all_ok = true;
            for (params, 0..) |param, index| {
                if (!argv_ok.isSet(index)) {
                    // This error can only happen for non-defaulted parameters
                    std.debug.assert(param.default_value == null);
                    try ana.emit_diag(fncall.span.location(), .{
                        .err_missing_parameter_for_function = .{
                            .parameter = param.name,
                            .function = fncall.function,
                        },
                    });
                    all_ok = false;
                }
            }
            if (!all_ok)
                return error.InvalidFunctionCall;
        }

        return argv;
    }

    fn index_of_param(params: []const Function.Parameter, name: []const u8) ?usize {
        for (params, 0..) |param, i| {
            if (std.mem.eql(u8, param.name, name))
                return i;
        }
        return null;
    }
};

const SegmentBuilder = struct {
    id: Segment_ID,
    hub_offset: u32,
    exec_mode: eval.ExecMode,
    data: std.Io.Writer.Allocating,

    fn init(id: Segment_ID, hub_offset: u32, exec_mode: eval.ExecMode, allocator: std.mem.Allocator) SegmentBuilder {
        return .{
            .id = id,
            .hub_offset = hub_offset,
            .exec_mode = exec_mode,
            .data = .init(allocator),
        };
    }

    fn deinit(sb: *SegmentBuilder) void {
        sb.data.deinit();
        sb.* = undefined;
    }

    fn len(sb: *SegmentBuilder) usize {
        return sb.data.written().len;
    }

    fn slice(sb: *SegmentBuilder) []u8 {
        return sb.data.written();
    }

    fn toOwnedSlice(sb: *SegmentBuilder) error{OutOfMemory}![]u8 {
        return sb.data.toOwnedSlice();
    }

    fn seek_forward(sb: *SegmentBuilder, hub_offset: u32) !void {
        std.debug.assert(hub_offset >= sb.hub_offset);
        try sb.writer().writeByteNTimes(sb.hub_offset < hub_offset);
    }

    fn writer(sb: *SegmentBuilder) *std.Io.Writer {
        return &sb.data.writer;
    }
};

const Segment_ID_Gen = struct {
    current: Segment_ID = @enumFromInt(0),

    pub fn next(sig: *Segment_ID_Gen) Segment_ID {
        const res = sig.current;
        sig.current = @enumFromInt(@intFromEnum(sig.current) + 1);
        return res;
    }
};

const Cursor = struct {
    offset: TaggedAddress,
    mode: eval.ExecMode,
    hub: u32,
    local_bytes: u32,
    reserved: bool,
    pack: u32 = 0,

    fn fit_position(cursor: Cursor) u32 {
        return switch (cursor.mode) {
            .cog, .regspace => cursor.local_bytes / 4,
            .lut => 0x200 + cursor.local_bytes / 4,
            .hub, .data => cursor.hub,
        };
    }

    fn init(segment: Segment_ID, mode: eval.ExecMode, hub: u32) Cursor {
        var cursor: Cursor = .{
            .offset = .init_cog(segment, 0, 0),
            .mode = mode,
            .hub = hub,
            .local_bytes = 0,
            .reserved = false,
        };
        cursor.sync();
        return cursor;
    }

    fn sync(cursor: *Cursor) void {
        const seg = cursor.offset.segment_id;
        const pc = cursor.local_bytes / 4;
        const local: u9 = @truncate(pc);
        const physical_hub: ?u20 = std.math.cast(u20, cursor.hub);
        cursor.offset = switch (cursor.mode) {
            .hub => .init(seg, physical_hub, .hub),
            .cog => .init_cog(seg, if (cursor.reserved) null else physical_hub, local),
            .lut => .init_lut(seg, physical_hub, local),
            .regspace => .init(seg, null, .{ .regspace = local }),
            .data => .init(seg, physical_hub, .data),
        };
        if (cursor.mode == .cog or cursor.mode == .lut)
            cursor.offset = cursor.offset.with_subreg_byte(@truncate(cursor.local_bytes));
    }

    fn change_mode(cursor: *Cursor, seg: Segment_ID, mode: eval.ExecMode, hub_offset: ?u32, local_start: ?u32) void {
        cursor.mode = mode;
        cursor.hub = hub_offset orelse cursor.hub;
        cursor.local_bytes = if (local_start) |start| (start - if (mode == .lut) @as(u32, 0x200) else 0) * 4 else 0;
        cursor.reserved = false;
        cursor.offset.segment_id = seg;
        cursor.sync();
    }

    fn advance_code(cursor: *Cursor) void {
        cursor.align_data(4);
        cursor.advance_data(.long);
    }

    fn advance_data(cursor: *Cursor, size: enum(u4) { byte = 1, word = 2, long = 4 }) void {
        const n = @intFromEnum(size);
        cursor.hub += n;
        if (cursor.mode == .cog or cursor.mode == .lut) cursor.local_bytes += n;
        cursor.sync();
    }

    fn align_data(cursor: *Cursor, alignment: u32) void {
        const position = if (cursor.mode == .cog or cursor.mode == .lut) cursor.local_bytes else cursor.hub;
        const padding = std.mem.alignForward(u32, position, alignment) - position;
        cursor.hub += padding;
        if (cursor.mode == .cog or cursor.mode == .lut) cursor.local_bytes += padding;
        cursor.sync();
    }

    fn alignas(cursor: *Cursor, alignment: u32) void {
        switch (cursor.mode) {
            .cog, .lut, .regspace => {
                const base: u32 = if (cursor.mode == .lut) 0x200 else 0;
                const pc = base + (cursor.local_bytes + 3) / 4;
                const next = (std.mem.alignForward(u32, pc, alignment) - base) * 4;
                const delta = next - cursor.local_bytes;
                cursor.local_bytes = next;
                if (cursor.mode != .regspace and !cursor.reserved) cursor.hub += delta;
            },
            .hub, .data => cursor.hub = std.mem.alignForward(u32, cursor.hub, alignment),
        }
        cursor.sync();
    }

    fn org(cursor: *Cursor, target: u32) void {
        switch (cursor.mode) {
            .cog, .lut, .regspace => {
                const next = (target - if (cursor.mode == .lut) @as(u32, 0x200) else 0) * 4;
                const delta = next - cursor.local_bytes;
                cursor.local_bytes = next;
                if (cursor.mode != .regspace and !cursor.reserved) cursor.hub += delta;
            },
            .hub => cursor.hub = target,
            .data => unreachable,
        }
        cursor.sync();
    }

    fn reserve(cursor: *Cursor, count: u32) void {
        cursor.local_bytes += count * 4;
        cursor.reserved = true;
        cursor.sync();
    }
};

test "semantic errors are collected" {
    const source =
        \\LONG missing
        \\
    ;

    var diagnostics_collection: diagnostics.Collection = .init(std.testing.allocator);
    defer diagnostics_collection.deinit();
    const source_file = try diagnostics_collection.register_source("test.propan", source);
    var parser: frontend.Parser = .init(source_file, &diagnostics_collection);
    var parsed = try parser.parse(std.testing.allocator);
    defer parsed.deinit();

    const result = analyze(std.testing.allocator, parsed.file, .{}, &diagnostics_collection);
    try std.testing.expectError(error.SemanticErrors, result);
    try std.testing.expect(diagnostics_collection.has_errors());

    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();

    try diagnostics_collection.render(&output.writer, .{});

    const actual = try output.toOwnedSlice();
    defer std.testing.allocator.free(actual);

    try std.testing.expect(std.mem.indexOf(u8, actual, "test.propan:1:6: error: undefined reference to symbol missing") != null);
}

test "invalid alignments produce semantic errors" {
    for ([_][]const u8{ ".align 0\n", ".align 3\n" }) |source| {
        var diagnostics_collection: diagnostics.Collection = .init(std.testing.allocator);
        defer diagnostics_collection.deinit();
        const source_file = try diagnostics_collection.register_source("test.propan", source);
        var parser: frontend.Parser = .init(source_file, &diagnostics_collection);
        var parsed = try parser.parse(std.testing.allocator);
        defer parsed.deinit();

        try std.testing.expectError(error.SemanticErrors, analyze(std.testing.allocator, parsed.file, .{}, &diagnostics_collection));
        try std.testing.expect(diagnostics_collection.has_errors());
    }
}

test "assert message diagnostic reports the message type" {
    var collection: diagnostics.Collection = .init(std.testing.allocator);
    defer collection.deinit();
    try std.testing.expectError(error.SemanticErrors, analyze_test_source(".assert 0, PTRA\n", "assert.propan", &collection, .{}));
    for (collection.diagnostics.items) |item| {
        if (item.kind == .err_expected_value_type and std.mem.eql(u8, item.kind.err_expected_value_type.subject, ".assert message")) {
            try std.testing.expectEqual(eval.Value.Type.register, item.kind.err_expected_value_type.actual);
            return;
        }
    }
    try std.testing.expect(false);
}

test "final segment retains the label segment ID" {
    const source = "last:\nBYTE 1\n";

    var diagnostics_collection: diagnostics.Collection = .init(std.testing.allocator);
    defer diagnostics_collection.deinit();
    const source_file = try diagnostics_collection.register_source("test.propan", source);
    var parser: frontend.Parser = .init(source_file, &diagnostics_collection);
    var parsed = try parser.parse(std.testing.allocator);
    defer parsed.deinit();

    var module = try analyze(std.testing.allocator, parsed.file, .{}, &diagnostics_collection);
    defer module.deinit();
    try std.testing.expectEqual(@as(usize, 1), module.segments.len);
    try std.testing.expectEqual(@as(usize, 1), module.symbols.len);
    try std.testing.expectEqual(module.symbols[0].label.segment_id, module.segments[0].id);
}

test "CALLD encoding selection is independent of variant order" {
    const source =
        \\CALLD PA, target
        \\CALLD PB, target
        \\CALLD PTRA, target
        \\CALLD PTRB, target
        \\CALLD PA, 1000
        \\CALLD PA, nrel(target)
        \\CALLD PA, target :wc
        \\CALLD PA, aug(1000)
        \\target:
        \\NOP
    ;
    var collection: diagnostics.Collection = .init(std.testing.allocator);
    defer collection.deinit();
    var parser_source: SourceFile = try .init(std.testing.allocator, "pointer-order.propan", source);
    defer parser_source.deinit(std.testing.allocator);
    var parser: frontend.Parser = .init(&parser_source, &collection);
    var parsed = try parser.parse(std.testing.allocator);
    defer parsed.deinit();

    for ([_]CalldAmbiguousEncoding{ .flexspin, .general, .address }) |policy| {
        for ([_]bool{ false, true }) |reversed| {
            var analyzer: Analyzer = try .init(std.testing.allocator, parsed.file, .{ .calld_ambiguous_encoding = policy }, &collection);
            defer analyzer.deinit();
            try analyzer.load_constants(stdlib.p2.constants);
            for (0..stdlib.p2.instructions.len) |i| {
                const index = if (reversed) stdlib.p2.instructions.len - i - 1 else i;
                try analyzer.load_instruction(stdlib.p2.instructions[index]);
            }
            try analyzer.declare_symbols();
            try analyzer.validate_symbol_refs();
            try analyzer.prepare_instruction_stream();
            try analyzer.select_instruction_mnemonic();
            try analyzer.assign_locations();
            try analyzer.evaluate_instruction_arguments();
            try analyzer.select_instruction_encoding();
            try std.testing.expect(analyzer.ok);
            for (analyzer.instructions[0..4]) |instr| {
                try std.testing.expectEqual(policy == .address, instr.instruction.?.operands[0].type == .pointer_reg);
            }
            try std.testing.expectEqual(policy != .general, analyzer.instructions[4].instruction.?.operands[0].type == .pointer_reg);
            try std.testing.expect(analyzer.instructions[5].instruction.?.operands[0].type == .pointer_reg);
            try std.testing.expect(analyzer.instructions[6].instruction.?.operands[0].type == .register);
            try std.testing.expect(analyzer.instructions[7].instruction.?.operands[0].type == .register);
        }
    }
}

test "semantic warnings are collected without failing analysis" {
    const source =
        \\_start:
        \\NOP
        \\
    ;

    var diagnostics_collection: diagnostics.Collection = .init(std.testing.allocator);
    defer diagnostics_collection.deinit();
    const source_file = try diagnostics_collection.register_source("test.propan", source);
    var parser: frontend.Parser = .init(source_file, &diagnostics_collection);
    var parsed = try parser.parse(std.testing.allocator);
    defer parsed.deinit();

    var module = try analyze(std.testing.allocator, parsed.file, .{}, &diagnostics_collection);
    defer module.deinit();

    try std.testing.expect(!diagnostics_collection.has_errors());
    try std.testing.expect(diagnostics_collection.has_warnings());

    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();

    try diagnostics_collection.render(&output.writer, .{});

    const actual = try output.toOwnedSlice();
    defer std.testing.allocator.free(actual);

    try std.testing.expect(std.mem.indexOf(u8, actual, "warning: symbol _start has no references") != null);
}

test Cursor {
    const seg: Segment_ID = @enumFromInt(0x1234_5678);

    var cursor: Cursor = .init(seg, .cog, 0);

    cursor.advance_data(.long);
    try std.testing.expectEqual(TaggedAddress.init_cog(seg, 4, 1), cursor.offset);

    cursor.advance_data(.long);
    try std.testing.expectEqual(TaggedAddress.init_cog(seg, 8, 2), cursor.offset);

    cursor.advance_data(.word);
    try std.testing.expectEqual(TaggedAddress.init_cog(seg, 10, 2).with_subreg_byte(2), cursor.offset);

    cursor.advance_data(.word);
    try std.testing.expectEqual(TaggedAddress.init_cog(seg, 12, 3), cursor.offset);

    cursor.advance_data(.byte);
    try std.testing.expectEqual(TaggedAddress.init_cog(seg, 13, 3).with_subreg_byte(1), cursor.offset);

    cursor.advance_data(.byte);
    try std.testing.expectEqual(TaggedAddress.init_cog(seg, 14, 3).with_subreg_byte(2), cursor.offset);

    cursor.advance_data(.byte);
    try std.testing.expectEqual(TaggedAddress.init_cog(seg, 15, 3).with_subreg_byte(3), cursor.offset);

    cursor.advance_data(.byte);
    try std.testing.expectEqual(TaggedAddress.init_cog(seg, 16, 4), cursor.offset);

    cursor.advance_code();
    try std.testing.expectEqual(TaggedAddress.init_cog(seg, 20, 5), cursor.offset);

    cursor.advance_data(.byte);
    try std.testing.expectEqual(TaggedAddress.init_cog(seg, 21, 5).with_subreg_byte(1), cursor.offset);

    cursor.advance_code();
    try std.testing.expectEqual(TaggedAddress.init_cog(seg, 28, 7), cursor.offset);

    cursor.advance_data(.byte);
    try std.testing.expectEqual(TaggedAddress.init_cog(seg, 29, 7).with_subreg_byte(1), cursor.offset);

    cursor.alignas(2);
    try std.testing.expectEqual(TaggedAddress.init_cog(seg, 32, 8), cursor.offset);

    cursor.alignas(2);
    try std.testing.expectEqual(TaggedAddress.init_cog(seg, 32, 8), cursor.offset);

    cursor.alignas(4);
    try std.testing.expectEqual(TaggedAddress.init_cog(seg, 32, 8), cursor.offset);

    var hub_cursor: Cursor = .init(seg, .hub, 29);
    hub_cursor.alignas(2);
    try std.testing.expectEqual(TaggedAddress.init_hub(seg, 30), hub_cursor.offset);
}

fn analyze_test_source(source: []const u8, path: []const u8, collection: *diagnostics.Collection, options: AnalyzeOptions) !Module {
    const source_file = try collection.register_source(path, source);
    var parser: frontend.Parser = .init(source_file, collection);
    var parsed = try parser.parse(std.testing.allocator);
    defer parsed.deinit();
    return analyze(std.testing.allocator, parsed.file, options, collection);
}

fn test_symbol(module: Module, name: []const u8) TaggedAddress {
    for (module.symbols) |symbol| if (std.mem.eql(u8, symbol.name, name)) return symbol.label;
    @panic("missing test symbol");
}

test ".pic default restores AnalyzeOptions after prefer" {
    const source =
        \\.hubexec 0x400
        \\target:
        \\LONG 0
        \\.hubexec
        \\JMP target
        \\.pic prefer
        \\JMP target
        \\.pic default
        \\JMP target
        \\JMP 0x400
        \\.pic prefer
        \\JMP 0x400
        \\.pic default
        \\JMP 0x400
    ;
    var collection: diagnostics.Collection = .init(std.testing.allocator);
    defer collection.deinit();
    var module = try analyze_test_source(source, "pic-options.propan", &collection, .{
        .use_label_relative_hub_to_hub_jmp = false,
        .use_relative_jmp_for_same_mode_nonlabel_address = false,
    });
    defer module.deinit();

    const expected = [_]u32{ 0xFD800400, 0xFD9FFFF4, 0xFD800400, 0xFD800400, 0xFD9FFFE8, 0xFD800400 };
    try std.testing.expectEqual(@as(usize, 2), module.segments.len);
    for (expected, 0..) |word, i| {
        try std.testing.expectEqual(word, std.mem.readInt(u32, module.segments[1].data[i * 4 ..][0..4], .little));
    }
    try std.testing.expect(!collection.has_errors());
}

test "CALLD address mode follows PIC and the numeric target default" {
    const source =
        \\CALLD PA, 1000
        \\.pic avoid
        \\CALLD PA, 1000
        \\.pic prefer
        \\CALLD PA, 1000
        \\.pic default
        \\CALLD PA, 1000
        \\.pic force
        \\CALLD PA, 1000
        \\.pic avoid
        \\CALLD PA, 1
        \\.pic default
        \\CALLD PA, 1
    ;

    for ([_]bool{ true, false }) |relative_default| {
        var collection: diagnostics.Collection = .init(std.testing.allocator);
        defer collection.deinit();
        var module = try analyze_test_source(source, "calld-pic.propan", &collection, .{
            .use_relative_jmp_for_same_mode_nonlabel_address = relative_default,
        });
        defer module.deinit();
        const bytes = module.segments[0].data;
        try std.testing.expectEqual(@as(usize, 7 * 4), bytes.len);
        for ([_]bool{ relative_default, false, true, relative_default, true, false }, 0..) |relative, i| {
            const word = std.mem.readInt(u32, bytes[i * 4 ..][0..4], .little);
            try std.testing.expectEqual(@as(u32, 0xFE000000), word & 0xFFE00000);
            try std.testing.expectEqual(relative, word & 0x00100000 != 0);
        }
        try std.testing.expectEqual(if (relative_default) @as(u32, 0xFE100F9C) else 0xFE0003E8, std.mem.readInt(u32, bytes[0..4], .little));
        const last = std.mem.readInt(u32, bytes[6 * 4 ..][0..4], .little);
        try std.testing.expectEqual(relative_default, last & 0xFFE00000 == 0xFB200000);
        try std.testing.expect(!collection.has_errors());
    }
}

test "forced address CALLD uses a byte displacement in cog mode" {
    var collection: diagnostics.Collection = .init(std.testing.allocator);
    defer collection.deinit();
    var module = try analyze_test_source("CALLD PA, 1\n", "calld-address.propan", &collection, .{
        .calld_ambiguous_encoding = .address,
    });
    defer module.deinit();
    try std.testing.expectEqual(@as(u32, 0xFE100000), std.mem.readInt(u32, module.segments[0].data[0..4], .little));
    try std.testing.expect(!collection.has_errors());
}

test "near CALLD crosses from cog to LUT with the general encoding" {
    const source =
        \\.cogexec
        \\.org 0x1EE
        \\CALLD PA, target
        \\.lutexec
        \\target:
        \\NOP
    ;
    var collection: diagnostics.Collection = .init(std.testing.allocator);
    defer collection.deinit();
    var module = try analyze_test_source(source, "calld-lut.propan", &collection, .{});
    defer module.deinit();
    try std.testing.expectEqual(@as(u32, 0xFB27EC11), std.mem.readInt(u32, module.segments[0].data[0x1EE * 4 ..][0..4], .little));
    try std.testing.expect(!collection.has_errors());
}

test "forced general CALLD reports an out-of-range branch" {
    var collection: diagnostics.Collection = .init(std.testing.allocator);
    defer collection.deinit();
    try std.testing.expectError(error.SemanticErrors, analyze_test_source("CALLD PA, 1000\n", "calld-general.propan", &collection, .{
        .calld_ambiguous_encoding = .general,
    }));
    try std.testing.expect(collection.has_errors());
    try std.testing.expect(for (collection.diagnostics.items) |item| {
        if (item.kind == .err_branch_too_far) break true;
    } else false);
}

test "cog packing, origin, and emitted padding" {
    const source =
        \\.cogexec 0x101
        \\BYTE 1, 2
        \\BYTE 3, 4
        \\packed:
        \\WORD 0x0506
        \\LONG 0x0708090A
        \\.org 5
        \\after:
        \\BYTE 9
    ;
    var collection: diagnostics.Collection = .init(std.testing.allocator);
    defer collection.deinit();
    var module = try analyze_test_source(source, "packing.propan", &collection, .{});
    defer module.deinit();

    try std.testing.expectEqual(@as(usize, 1), module.segments.len);
    try std.testing.expectEqual(@as(u20, 0x101), module.segments[0].hub_offset);
    try std.testing.expectEqualSlices(u8, &.{ 1, 2, 3, 4, 6, 5, 0, 0, 0x0A, 9, 8, 7, 0, 0, 0, 0, 0, 0, 0, 0, 9 }, module.segments[0].data);
    try std.testing.expectEqual(@as(?u20, 0x105), test_symbol(module, "packed").hub_address);
    try std.testing.expectEqual(@as(?u32, 1), test_symbol(module, "packed").get_local(.data));
    try std.testing.expectEqual(@as(?u20, 0x115), test_symbol(module, "after").hub_address);
    try std.testing.expectEqual(@as(?u32, 5), test_symbol(module, "after").get_local(.data));
    var padding_warnings: usize = 0;
    for (collection.diagnostics.items) |item| if (item.kind == .warn_emitted_padding_byte_s) {
        padding_warnings += 1;
    };
    try std.testing.expectEqual(@as(usize, 2), padding_warnings);
}

test "reserve, regspace, and data labels" {
    const source =
        \\.cogexec 0x100
        \\LONG 1
        \\RES 2
        \\var cogvar:
        \\.regspace
        \\.org 0x1F0
        \\var reg:
        \\RES 2
        \\var reg2:
        \\.data 0x200
        \\table:
        \\BYTE 0x42
        \\.hubexec 0x300
        \\JMP table
    ;
    var collection: diagnostics.Collection = .init(std.testing.allocator);
    defer collection.deinit();
    var module = try analyze_test_source(source, "modes.propan", &collection, .{});
    defer module.deinit();

    try std.testing.expectEqual(@as(usize, 3), module.segments.len);
    try std.testing.expectEqual(@as(?u20, null), test_symbol(module, "cogvar").hub_address);
    try std.testing.expectEqual(@as(?u32, 3), test_symbol(module, "cogvar").get_local(.data));
    try std.testing.expectEqual(@as(?u20, null), test_symbol(module, "reg").hub_address);
    try std.testing.expectEqual(@as(?u32, 0x1F2), test_symbol(module, "reg2").get_local(.data));
    try std.testing.expectEqual(@as(?u20, 0x200), test_symbol(module, "table").hub_address);
    try std.testing.expect(test_symbol(module, "table").local == .data);
    var saw_branch_warning = false;
    for (collection.diagnostics.items) |item| if (item.kind == .warn_branch_into_data) {
        saw_branch_warning = true;
    };
    try std.testing.expect(saw_branch_warning);
}

test "regspace labels work as cog register operands" {
    const source =
        \\.regspace
        \\var temp:
        \\RES 1
        \\.cogexec 0x100
        \\MOV temp, 1
    ;
    var collection: diagnostics.Collection = .init(std.testing.allocator);
    defer collection.deinit();
    var module = try analyze_test_source(source, "regspace.propan", &collection, .{});
    defer module.deinit();
    try std.testing.expectEqual(@as(?u20, null), test_symbol(module, "temp").hub_address);
    try std.testing.expectEqual(@as(?u32, 0), test_symbol(module, "temp").get_local(.data));
    try std.testing.expectEqual(@as(usize, 4), module.segments[0].data.len);
}

test "FILE resolves beside source and data alignment pads hub" {
    const source =
        \\.data 0x101
        \\BYTE 1
        \\.align 4
        \\FILE "fixtures/payload.txt"
    ;
    var collection: diagnostics.Collection = .init(std.testing.allocator);
    defer collection.deinit();
    var module = try analyze_test_source(source, "tests/propan/sema/file_source.propan", &collection, .{ .io = std.testing.io });
    defer module.deinit();
    try std.testing.expectEqualSlices(u8, &.{ 1, 0, 0, 'x', 'y', 'z', '\n' }, module.segments[0].data);
    try std.testing.expect(collection.has_warnings());
}

test "LUT alignment and hub origin use their respective PCs" {
    const source =
        \\.lutexec 0x123
        \\LONG 1
        \\.align 4
        \\lut_aligned:
        \\WORD 2
        \\.hubexec 0x200
        \\.org 0x208
        \\hub_origin:
        \\BYTE 3
    ;
    var collection: diagnostics.Collection = .init(std.testing.allocator);
    defer collection.deinit();
    var module = try analyze_test_source(source, "lut.propan", &collection, .{});
    defer module.deinit();
    try std.testing.expectEqual(@as(?u20, 0x133), test_symbol(module, "lut_aligned").hub_address);
    try std.testing.expectEqual(@as(?u32, 0x004), test_symbol(module, "lut_aligned").get_local(.data));
    try std.testing.expectEqual(@as(?u32, 0x204), test_symbol(module, "lut_aligned").get_local(.pc));
    try std.testing.expectEqual(@as(?u20, 0x208), test_symbol(module, "hub_origin").hub_address);
    try std.testing.expectEqual(@as(?u32, 0x208), test_symbol(module, "hub_origin").get_local(.pc));
}

test "LUT data operands use indices and jumps use execution PCs" {
    const source =
        \\.lutexec 0x100
        \\LONG 0
        \\target:
        \\LONG 1
        \\.cogexec 0x200
        \\RDLUT dst, &target
        \\WRLUT dst, &target
        \\JMP nrel(target)
        \\var dst: LONG 0
    ;
    var collection: diagnostics.Collection = .init(std.testing.allocator);
    defer collection.deinit();
    var module = try analyze_test_source(source, "lut-operands.propan", &collection, .{});
    defer module.deinit();
    try std.testing.expectEqual(@as(?u32, 1), test_symbol(module, "target").get_local(.data));
    try std.testing.expectEqual(@as(?u32, 0x201), test_symbol(module, "target").get_local(.pc));
    try std.testing.expectEqual(@as(u32, 1), std.mem.readInt(u32, module.segments[1].data[0..4], .little) & 0x1FF);
    try std.testing.expectEqual(@as(u32, 1), std.mem.readInt(u32, module.segments[1].data[4..8], .little) & 0x1FF);
    try std.testing.expectEqual(@as(u32, 0xFD800201), std.mem.readInt(u32, module.segments[1].data[8..12], .little));
    try std.testing.expect(!collection.has_errors());
}

test "LUT origin uses execution PC and list entries retain it" {
    const source =
        \\.lutexec 0x100
        \\.org 0x203
        \\target:
        \\LONG 1
    ;
    var collection: diagnostics.Collection = .init(std.testing.allocator);
    defer collection.deinit();
    var module = try analyze_test_source(source, "lut-origin.propan", &collection, .{});
    defer module.deinit();
    try std.testing.expectEqual(@as(?u32, 3), test_symbol(module, "target").get_local(.data));
    try std.testing.expectEqual(@as(?u32, 0x203), test_symbol(module, "target").get_local(.pc));
    try std.testing.expectEqual(@as(u20, 0x100), module.segments[0].hub_offset);
    try std.testing.expectEqual(@as(u8, 0), module.segments[0].data[0]);
    try std.testing.expectEqual(@as(?u32, 0x203), module.line_data[module.line_data.len - 1].pc);
    try std.testing.expect(collection.has_warnings());
}

test "missing FILE is a diagnostic" {
    var collection: diagnostics.Collection = .init(std.testing.allocator);
    defer collection.deinit();
    try std.testing.expectError(error.SemanticErrors, analyze_test_source(
        "FILE \"fixtures/missing.bin\"\n",
        "tests/propan/sema/file_source.propan",
        &collection,
        .{ .io = std.testing.io },
    ));
    try std.testing.expect(collection.has_errors());
}

test "one-past-end hub cursor emits no segment" {
    var collection: diagnostics.Collection = .init(std.testing.allocator);
    defer collection.deinit();
    var module = try analyze_test_source(".hubexec 0x80000\n", "empty-end.propan", &collection, .{});
    defer module.deinit();
    try std.testing.expectEqual(@as(usize, 0), module.segments.len);
    try std.testing.expect(!collection.has_errors());
}

test "segment layout rejects overlaps and invalid emission" {
    const cases = [_][]const u8{
        ".hubexec 0x100\nBYTE 1\n.data 0x100\nBYTE 2\n",
        ".cogexec\nRES 1\nLONG 2\n",
        ".regspace\nBYTE 1\n",
        ".data\nNOP\n",
        ".data\n.org 4\n",
        ".hubexec 0x7FFFF\nWORD 1\n",
        ".hubexec 0x80000\nBYTE 1\n",
        ".cogexec\n.org 0x1FF\nLONG 1\nLONG 2\n",
        ".lutexec\n.org 0x3FF\nLONG 1\nLONG 2\n",
        ".lutexec\n.org 0x1FF\nLONG 1\n",
        ".lutexec\n.org 0x200\nLONG 0\n.org 0x200\n",
        ".cogexec\n.org 4\n.org 3\n",
        ".regspace\nvar x:\n.assert hubaddr(x) == 0\n",
        ".cogexec\nRES 1\nvar x:\n.assert hubaddr(x) == 0\n",
    };
    for (cases) |source| {
        var collection: diagnostics.Collection = .init(std.testing.allocator);
        defer collection.deinit();
        try std.testing.expectError(error.SemanticErrors, analyze_test_source(source, "invalid.propan", &collection, .{}));
        try std.testing.expect(collection.has_errors());
    }
}

const SymbolInfo = struct {
    name: []const u8,

    type: Type = .undefined,
    referenced: bool = false,

    offset: ?TaggedAddress = null,
    value: ?Value = null,
    constant: ?*const ast.Constant = null,
    constant_mode: eval.ExecMode = .cog,
    constant_state: enum { unvisited, evaluating, evaluated, failed } = .unvisited,

    pub fn location(sym: SymbolInfo) ?ast.Location {
        return switch (sym.type) {
            .code, .data, .constant => |loc| loc,
            .undefined, .builtin => null,
        };
    }

    pub const Type = diagnostics.SymbolDefinition;
};

const InstructionInfo = struct {
    /// Pointer into the 'file.sequence'
    seq_index: usize,
    ast_node: *const ast.Instruction,

    mnemonic: ?*const Mnemonic = null,
    instruction: ?*const EncodedInstruction = null,

    start_addr: ?TaggedAddress = null,
    end_addr: ?TaggedAddress = null,
    /// Retain the one-past-end PC, which may exceed the address's 9-bit index.
    end_pc: ?u32 = null,
    fit_pc: ?u32 = null,

    /// Size of the instruction slot in bytes
    byte_size: ?u32 = null,
    file_data: []const u8 = &.{},
    pic_mode: ?PicMode = null,

    arguments: []eval.Value = &.{},
};

pub const Function = union(enum) {
    // the builtin functions are hardcoded here:
    aug, // adds auto-augmentation to the argument
    nrel, // computes the absolute address

    hubaddr,
    cogaddr,
    lutaddr,
    localaddr,
    byteoffset,
    wordoffset,

    // stdlib functions are defined as "generic" ones:
    user: UserFunction,

    pub fn get_docs(func: Function) []const u8 {
        return switch (func) {
            .aug => "Encode an immediate instruction operand with AUGS/AUGD, allowing values beyond the instruction's immediate field. Use aug(value) as the outermost operand expression, or augment a pointer index with PTRA[aug(index)]. It is not valid in constant definitions. The value itself is unchanged.",
            .nrel => "Force absolute rather than PC-relative addressing for an instruction operand. Use nrel(addr) as the outermost operand expression. The value itself is unchanged; only its addressing mode changes.",
            .hubaddr => "Return a label's hub byte address as an integer, independent of its execution mode. Labels in register space have no hub address.",
            .cogaddr => "Return a cog or register-space label's register index as an integer (0 through $1FF). Hub, LUT, and data labels are not accepted.",
            .lutaddr => "Return a LUT label's long index as an integer (0 through $1FF). This is the LUT data index; the execution PC is this index plus $200.",
            .localaddr => "Return a label's local data address as an integer: a register index for cog/register space, a long index for LUT, or a byte address for hub execution. Data-only labels have no local address. Hardware register operands are accepted only in cog execution mode.",
            .byteoffset => "Return a cog, LUT, or register-space label's byte offset within its containing long (0 through 3). Useful for packed BYTE data; hub and data-only labels are not accepted.",
            .wordoffset => "Return a cog, LUT, or register-space label's word offset within its containing long (0 or 1), computed as byteoffset(addr) / 2. Hub and data-only labels are not accepted.",
            .user => |f| f.docs,
        };
    }

    pub fn get_parameters(func: Function) []const Parameter {
        // Callers retain these slices, so intrinsic metadata needs static storage.
        return switch (func) {
            .aug => comptime &.{Parameter{ .name = "value", .type = .int, .docs = "Immediate integer or literal label address to encode with augmentation." }},
            .nrel => comptime &.{Parameter{ .name = "addr", .type = .int, .docs = "Integer or label address to encode with absolute addressing." }},
            .hubaddr => comptime &.{Parameter{ .name = "addr", .type = .address, .docs = "Label with an allocated hub address; register-space labels are not accepted." }},
            .cogaddr => comptime &.{Parameter{ .name = "addr", .type = .address, .docs = "Cog or register-space label whose register index should be returned." }},
            .lutaddr => comptime &.{Parameter{ .name = "addr", .type = .address, .docs = "LUT label whose data index (without the $200 execution-PC base) should be returned." }},
            .localaddr => comptime &.{Parameter{ .name = "addr", .type = .address, .docs = "Label with a local address, or a hardware register in cog execution mode." }},
            .byteoffset => comptime &.{Parameter{ .name = "addr", .type = .address, .docs = "Cog, LUT, or register-space label whose byte position within a long should be returned." }},
            .wordoffset => comptime &.{Parameter{ .name = "addr", .type = .address, .docs = "Cog, LUT, or register-space label whose word position within a long should be returned." }},
            .user => |f| f.params,
        };
    }

    pub const Parameter = struct {
        name: []const u8,
        type: Type,
        docs: []const u8 = "",
        default_value: ?Value = null,

        pub fn init(name: []const u8, ptype: Type) Parameter {
            return .{ .name = name, .type = ptype };
        }

        pub const Type = enum {
            int,
            string,
            address,
            register,
            enumerator,
            pointer_expr,
            any,
        };
    };
};

pub const UserFunction = struct {
    name: ?[]const u8 = null,
    docs: []const u8,
    params: []const Function.Parameter,
    invoke: *const fn (ctx: FunctionCallContext, []const eval.Value) FunctionCallError!eval.Value,
};

pub const FunctionCallContext = struct {
    ana: *Analyzer,
    location: ast.Location,

    pub fn allocator(ctx: FunctionCallContext) std.mem.Allocator {
        return ctx.ana.arena.allocator();
    }

    pub fn fatal_error(ctx: FunctionCallContext, diagnostic: diagnostics.Kind) error{ OutOfMemory, DiagnosedFailure } {
        try ctx.ana.emit_diag(ctx.location, diagnostic);
        return error.DiagnosedFailure;
    }

    pub fn emit_diag(ctx: FunctionCallContext, diagnostic: diagnostics.Kind) !void {
        try ctx.ana.emit_diag(ctx.location, diagnostic);
    }
};

pub const FunctionCallError = error{
    OutOfMemory,
    InvalidArg,
    InvalidArgCount,
    Overflow,
    DivideByZero,
    TypeMismatch,
    /// Special error which is silently swallowed and does not emit an explicit diagnostic code
    DiagnosedFailure,
};

const Mnemonic = union(enum) {
    // data encoded
    long,
    word,
    byte,
    file,

    // directives:

    cogexec,
    lutexec,
    hubexec,
    regspace,
    data,
    org,
    res,
    @"align",
    pack,
    pic,
    assert,
    fit,

    encoded: EncodedMnemonic,
};

const EncodedMnemonic = struct {
    variants: std.ArrayListUnmanaged(EncodedInstruction),
};

pub const EncodedInstruction = struct {
    mnemonic: []const u8,

    binary: u32,
    effects: Effects,
    operands: []const Operand,
    flags: Flags = .{},

    c_effect_slot: ?Slot = null,
    z_effect_slot: ?Slot = null,

    default_condition: ast.Condition.Code = .always,

    pub const Slot = struct {
        pub const S: Slot = .init(0, 9);
        pub const D: Slot = .init(9, 9);

        shift: u5,
        bits: u5,

        pub fn init(shift: u8, bits: u5) Slot {
            return .{ .shift = shift, .bits = bits };
        }

        pub fn from_mask(mval: u32) Slot {
            const shift = @ctz(mval);
            const shifted = mval >> shift;
            const bits = @ctz(~shifted);
            std.debug.assert(shift + bits <= 32);
            return .{
                .shift = shift,
                .bits = bits,
            };
        }

        pub fn eql(a: Slot, b: Slot) bool {
            return a.shift == b.shift and a.bits == b.bits;
        }

        pub fn mask(slot: Slot) u32 {
            return ((@as(u32, 1) << slot.bits) - 1) << slot.shift;
        }

        pub fn max_value(slot: Slot) u32 {
            return (@as(u32, 1) << slot.bits) - 1;
        }

        pub fn write(slot: Slot, container: *u32, value: u32) error{Overflow}!void {
            const shifted = value << slot.shift;
            if ((shifted & ~slot.mask()) != 0)
                return error.Overflow;
            container.* |= shifted;
        }

        pub fn read(slot: Slot, container: u32) u32 {
            return (container & slot.mask()) >> slot.shift;
        }

        /// Sets all values inside the slot to 1.
        pub fn fill(slot: Slot, container: *u32) void {
            slot.write(container, slot.max_value()) catch unreachable; // max_value() always fits the slot.
        }
    };

    pub const Operand = struct {
        type: Type,
        slot: Slot,
        copy_to: ?Slot = null,

        pub fn init(optype: Type, opslot: Slot) Operand {
            return .{
                .type = optype,
                .slot = opslot,
            };
        }

        pub const TypeId = std.meta.Tag(Type);
        pub const Type = union(enum) {
            /// Immediate value with absolute or relative addressing
            /// #{\}A
            /// slot marks the bits where relative=1, absolute=0 should be written
            address: struct { rel: Slot },

            /// D or S
            register,

            /// Absolute immediate value
            /// #D, #S or #N
            /// limit is encoded by `(1 << op.slot.length)`
            /// value encodes the right-shift of the value, which is used to align it.
            ///     use case: AUGS/AUGD take a "#n" argument, which is top-most 23 bits of a value
            immediate: u5,

            /// {#}D or {#}S
            /// limit is encoded by `(1 << op.slot.length)`
            /// slot marks the bits where immediate=1, register=0 should be written
            reg_or_imm: struct { imm: Slot, pcrel: bool },

            /// Either #N or PTRx++, --PTRx, ...
            /// with N <= 255
            pointer_expr: struct { imm: Slot },

            /// PA, PB, PTRA or PTRB
            pointer_reg,

            /// One of the given named values.
            enumeration: std.StaticStringMap(u32),

            pub fn can_assign_from(opt: Type, value: Value) bool {
                if (value.value == .pointer_expr) return opt == .pointer_expr;
                if (value.value == .string or value.value == .sequence) {
                    // strings and sequences cannot be assigned to instruction operands
                    return false;
                }
                if (value.value == .enumerator) {
                    // enumerators can only be assigned to an enumeration type
                    return (opt == .enumeration);
                }

                const vtype: Value.Type = value.value;
                const usage: Value.UsageHint = value.flags.usage;

                return switch (opt) {
                    .address => (usage == .literal),
                    .register => (usage == .register),
                    .immediate => (usage == .literal),
                    .reg_or_imm => true,

                    .pointer_expr => switch (vtype) {
                        .pointer_expr => true,
                        .address => true,
                        .int => (usage == .literal),
                        .register => (usage == .register) or switch (value.value.register) {
                            PTRA, PTRB => true,
                            else => false,
                        },
                        else => false,
                    },

                    .pointer_reg => (vtype == .register) and switch (value.value.register) {
                        PA, PB, PTRA, PTRB => true,
                        else => false,
                    },

                    // integers and offsets can't be assigned to enums
                    .enumeration => false,
                };
            }
        };
    };

    pub const Effects = packed struct {
        const empty: Effects = std.mem.zeroes(Effects);

        none: bool, // allow no effect
        wz: bool,
        wc: bool,
        wcz: bool,
        and_c: bool,
        and_z: bool,
        or_c: bool,
        or_z: bool,
        xor_c: bool,
        xor_z: bool,

        pub fn from_list(comptime set: []const std.meta.FieldEnum(Effects)) Effects {
            @setEvalBranchQuota(10_000);
            var results = empty;
            inline for (set) |key| {
                @field(results, @tagName(key)) = true;
            }
            return results;
        }

        pub fn overlaps(lhs: Effects, rhs: Effects) bool {
            inline for (std.meta.fields(Effects)) |fld| {
                if (@field(lhs, fld.name) and @field(rhs, fld.name)) return true;
            }
            return false;
        }

        pub fn contains(set: Effects, item: ast.Effect) bool {
            return switch (item) {
                inline else => |tag| return @field(set, @tagName(tag)),
            };
        }
    };

    pub const Flags = packed struct {
        wcz_not_used: enum(u2) { ignore, warn, err } = .ignore,
    };
};

fn CaseInsensitiveStringHashMap(comptime V: type) type {
    return std.ArrayHashMapUnmanaged(
        []const u8,
        V,
        CaseInsensitiveStringContext,
        true,
    );
}

const CaseInsensitiveStringContext = struct {
    pub fn hash(self: @This(), s: []const u8) u32 {
        var hasher: std.hash.Wyhash = .init(0);
        var buffer: [32]u8 = undefined;
        var i: usize = 0;
        while (s.len - i > 0) {
            const lower = std.ascii.lowerString(&buffer, s[i..@min(i + buffer.len, s.len)]);

            hasher.update(lower);

            i += lower.len;
        }
        _ = self;
        return @truncate(hasher.final());
    }
    pub fn eql(self: @This(), a: []const u8, b: []const u8, b_index: usize) bool {
        _ = self;
        _ = b_index;
        return std.ascii.eqlIgnoreCase(a, b);
    }
};

fn BoundedArray(comptime T: type, comptime cap: usize) type {
    return struct {
        items: [cap]T = undefined,
        len: usize = 0,

        pub fn resize(arr: *@This(), size: usize) error{OutOfMemory}!void {
            if (size > cap)
                return error.OutOfMemory;
            arr.len = size;
        }

        pub fn slice(arr: *@This()) []T {
            return arr.items[0..arr.len];
        }

        pub fn constSlice(arr: *const @This()) []const T {
            return arr.items[0..arr.len];
        }
    };
}

test "function argument buffer accepts its full capacity" {
    var args: BoundedArray(u8, 16) = .{};
    try args.resize(16);
    try std.testing.expectEqual(@as(usize, 16), args.slice().len);
    try std.testing.expectError(error.OutOfMemory, args.resize(17));
}
