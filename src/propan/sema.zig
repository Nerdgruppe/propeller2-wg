const std = @import("std");

const stdlib = @import("stdlib/stdlib.zig");
const eval = @import("stdlib/eval.zig");
const frontend = @import("frontend.zig");
const ast = frontend.ast;
const diagnostics = @import("diagnostics.zig");

const logger = std.log.scoped(.sema);

const Value = eval.Value;

const Module = @import("Module.zig");
const Segment = Module.Segment;

const TaggedAddress = eval.TaggedAddress;
const Segment_ID = eval.Segment_ID;

const PA: eval.Register = @enumFromInt(0x1F6);
const PB: eval.Register = @enumFromInt(0x1F7);
const PTRA: eval.Register = @enumFromInt(0x1F8);
const PTRB: eval.Register = @enumFromInt(0x1F9);

pub const AnalyzeOptions = struct {
    io: ?std.Io = null,
    blank_pointer_expr: enum {
        as_ptr_epxr,
        as_register,
    } = .as_ptr_epxr,

    flip_augs_on_pcrel: bool = true,

    /// If this is `true`, the emitted code will use relative addressing for
    /// `JMP #{/}A` and friends if `A` is a label value, and addresses an address
    /// that jumps from hub exec mode to a hub exec mode address which is *not* in the
    /// same segment.
    use_label_relative_hub_to_hub_jmp: bool = true,

    /// If this is `true`, the emitted code will use relative addressing for
    /// `JMP #{/}A` and friends if `A` is a non-label value, and addresses
    /// an address in the same execution mode.
    /// TODO: Make this more fine granular for exec modes and cross-segment.
    use_relative_jmp_for_same_mode_nonlabel_address: bool = true,
};

pub fn analyze(allocator: std.mem.Allocator, file: ast.File, options: AnalyzeOptions, diagnostics_collection: *diagnostics.Collection) !Module {
    var analyzer: Analyzer = try .init(allocator, file, options, diagnostics_collection);
    defer analyzer.deinit();

    errdefer dump_analyzer(&analyzer);

    // Prepare
    try analyzer.load_constants(stdlib.common.constants);
    try analyzer.load_constants(stdlib.p2.constants);

    try analyzer.load_functions(stdlib.p2.functions);

    try analyzer.load_instructions(stdlib.p2.instructions);

    // Validate
    try analyzer.declare_symbols();
    try analyzer.validate_symbol_refs();

    // Lay Out
    try analyzer.prepare_instruction_stream();
    try analyzer.select_instruction_mnemonic();

    if (!analyzer.ok)
        return error.SemanticErrors;

    try analyzer.assign_locations();

    if (!analyzer.ok)
        return error.SemanticErrors;

    try analyzer.check_undefined_labels();

    // beyond  this check, all symbols are defined
    // and expression evaluation can happen:
    if (!analyzer.ok)
        return error.SemanticErrors;

    try analyzer.evaluate_constant_values();

    try analyzer.check_undefined_symbols();

    try analyzer.evaluate_instruction_arguments();

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

    for (analyzer.symbols.values()) |sym| {
        const stype: Module.Symbol.Type = switch (sym.type) {
            .undefined => continue,
            .code => .code,
            .data => .data,
            .constant => continue,
            .builtin => continue,
        };

        var source_location = sym.location().?;
        if (source_location.source) |source| source_location.source = try output_allocator.dupe(u8, source);
        try symbols.append(output_allocator, .{
            .name = try output_allocator.dupe(u8, sym.name),
            .label = sym.offset.?,
            .type = stype,
            .source_location = source_location,
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
            .location = con.location,
        });
    }

    return .{
        .arena = output_arena,
        .segments = segments,
        .regspace_segments = try output_allocator.dupe(u32, analyzer.regspace_segments.items),
        .line_data = try analyzer.line_data.toOwnedSlice(output_allocator),
        .symbols = try symbols.toOwnedSlice(output_allocator),
        .constants = try constants.toOwnedSlice(output_allocator),
    };
}

fn copy_value(allocator: std.mem.Allocator, value: Value) !Value {
    var copied = value;
    switch (value.value) {
        .string => |text| copied.value = .{ .string = try allocator.dupe(u8, text) },
        .enumerator => |text| copied.value = .{ .enumerator = try allocator.dupe(u8, text) },
        .int, .address, .register, .pointer_expr => {},
    }
    return copied;
}

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

    fn init(allocator: std.mem.Allocator, file: ast.File, options: AnalyzeOptions, diagnostics_collection: *diagnostics.Collection) !Analyzer {
        var ana: Analyzer = .{
            .allocator = allocator,
            .arena = .init(allocator),
            .file = file,
            .options = options,
            .diagnostics = diagnostics_collection,
        };
        try ana.functions.ensureUnusedCapacity(allocator, 6);

        ana.functions.putAssumeCapacityNoClobber("hubaddr", .hubaddr);
        ana.functions.putAssumeCapacityNoClobber("cogaddr", .cogaddr);
        ana.functions.putAssumeCapacityNoClobber("lutaddr", .lutaddr);
        ana.functions.putAssumeCapacityNoClobber("localaddr", .localaddr);
        ana.functions.putAssumeCapacityNoClobber("aug", .aug);
        ana.functions.putAssumeCapacityNoClobber("nrel", .nrel);

        try ana.mnemonics.ensureUnusedCapacity(allocator, 14);

        ana.mnemonics.putAssumeCapacityNoClobber(".cogexec", .cogexec);
        ana.mnemonics.putAssumeCapacityNoClobber(".lutexec", .lutexec);
        ana.mnemonics.putAssumeCapacityNoClobber(".hubexec", .hubexec);
        ana.mnemonics.putAssumeCapacityNoClobber(".align", .@"align");
        ana.mnemonics.putAssumeCapacityNoClobber(".org", .org);
        ana.mnemonics.putAssumeCapacityNoClobber(".reserve", .reserve);
        ana.mnemonics.putAssumeCapacityNoClobber(".regspace", .regspace);
        ana.mnemonics.putAssumeCapacityNoClobber(".data", .data);
        ana.mnemonics.putAssumeCapacityNoClobber("FILE", .file);
        ana.mnemonics.putAssumeCapacityNoClobber(".assert", .assert);
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
        if (diagnostic.level() == .@"error") ana.ok = false;
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

                const effects_overlap = instr.effects.@"union"(other.effects).any();

                if (all_eq and effects_overlap) {
                    std.log.err("{s}, {s}", .{ instr.mnemonic, other.mnemonic });
                    for (other.operands) |op| {
                        std.log.err("ops: {s}", .{@tagName(op.type)});
                    }
                    return error.DuplicateInstruction;
                }
            }
        }

        try encoded.variants.append(ana.arena.allocator(), instr);
    }

    /// Declares all symbols from labels and constants.
    fn declare_symbols(ana: *Analyzer) !void {
        for (ana.file.sequence) |item| {
            switch (item) {
                .empty => {},
                .label => |lbl| {
                    const sym = try ana.get_label_info(lbl.identifier, lbl.local_scope);
                    if (sym.type != .undefined) {
                        try ana.emit_diag(lbl.location, .{
                            .err_duplicate_definition = .{
                                .name = lbl.identifier,
                                .previous = .{ .symbol = sym.type },
                            },
                        });
                    } else {
                        sym.type = switch (lbl.type) {
                            .@"var" => .{ .data = lbl.location },
                            .code => .{ .code = lbl.location },
                        };
                    }
                },
                .constant => |con| {
                    const sym = try ana.get_symbol_info(con.identifier);
                    if (sym.type != .undefined) {
                        try ana.emit_diag(con.location, .{
                            .err_duplicate_definition = .{
                                .name = con.identifier,
                                .previous = .{ .symbol = sym.type },
                            },
                        });
                    } else {
                        sym.type = .{ .constant = con.location };
                    }
                },
                .instruction => {},
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
                    try ana.emit_diag(symref.location, .{
                        .err_undefined_reference_to_symbol_at = .{
                            .name = symref.symbol_name,
                            .reference_location = symref.location,
                        },
                    });
                }
            },

            .wrapped => |inner| try ana.validate_expr_symbol_refs(inner.*),

            .unary_transform => |trafo| try ana.validate_expr_symbol_refs(trafo.value.*),
            .binary_transform => |trafo| {
                try ana.validate_expr_symbol_refs(trafo.lhs.*);
                try ana.validate_expr_symbol_refs(trafo.rhs.*);
            },
            .function_call => |fncall| {
                if (ana.get_function(fncall.function) == null) {
                    try ana.emit_diag(fncall.location, .{
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
            .integer => {},
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
                try ana.emit_diag(instr.ast_node.location, .{
                    .err_unknown_mnemonic = .{
                        .mnemonic = instr.ast_node.mnemonic,
                    },
                });
                continue;
            };

            instr.mnemonic = mnemonic;

            instr.byte_size = switch (mnemonic.*) {
                .cogexec, .lutexec, .hubexec, .regspace, .data, .org, .reserve, .assert, .@"align" => 0,

                .file => blk: {
                    if (instr.ast_node.arguments.len != 1 or instr.ast_node.arguments[0] != .string) {
                        try ana.emit_diag(instr.ast_node.location, .err_file_requires_one_string_literal_path);
                        break :blk 0;
                    }
                    const io = ana.options.io orelse {
                        try ana.emit_diag(instr.ast_node.location, .err_file_requires_file_i_o);
                        break :blk 0;
                    };
                    const dir = if (instr.ast_node.location.source) |source|
                        std.fs.path.dirname(source) orelse "."
                    else
                        ".";
                    const path = instr.ast_node.arguments[0].string.value;
                    const resolved = if (std.fs.path.isAbsolute(path)) path else try std.fs.path.join(ana.arena.allocator(), &.{ dir, path });
                    instr.file_data = std.Io.Dir.cwd().readFileAlloc(io, resolved, ana.arena.allocator(), .limited(512 * 1024)) catch |err| {
                        try ana.emit_diag(instr.ast_node.location, .{
                            .err_cannot_read_file = .{
                                .path = resolved,
                                .reason = err,
                            },
                        });
                        break :blk 0;
                    };
                    break :blk @intCast(instr.file_data.len);
                },

                .long => @intCast(4 * instr.ast_node.arguments.len),
                .word => @intCast(2 * instr.ast_node.arguments.len),
                .byte => @intCast(1 * instr.ast_node.arguments.len),

                .encoded => blk: {
                    // we search for a super-special case here,
                    // which is an `aug(…)` argument that augments the
                    // argument with a prefix instruction and increments
                    // the size by an additional instruction
                    var size: u32 = 4;
                    for (instr.ast_node.arguments) |arg| {
                        switch (arg) {
                            .function_call => |fncall| {
                                const func = ana.get_function(fncall.function) orelse continue;
                                if (func.* == .aug) {
                                    // Each aug() increments the size
                                    size += 4;
                                }
                            },
                            else => {},
                        }
                    }
                    break :blk size;
                },
            };
        }
    }

    ///
    /// Assigns all label and instruction locations to their associated
    /// position in hub/cog/lut ram.
    ///
    fn assign_locations(ana: *Analyzer) !void {
        std.debug.assert(ana.seq_to_instr_lut.len == ana.file.sequence.len);

        var idgen: Segment_ID_Gen = .{};
        var cursor: Cursor = .init(idgen.next(), .cog, 0);

        for (ana.file.sequence, 0..) |*seq, i| {
            switch (seq.*) {
                .empty, .constant => {},

                .label => |lbl| {
                    const sym = ana.get_label_info(lbl.identifier, lbl.local_scope) catch unreachable;
                    sym.offset = cursor.offset;
                    if (cursor.hub >= 0x80000 and !cursor.reserved and cursor.mode != .regspace) {
                        try ana.emit_diag(lbl.location, .{ .err_address_outside_space = .{ .subject = .label, .space = .hub, .actual = cursor.hub, .max_exclusive = 0x80000 } });
                    }
                    switch (cursor.mode) {
                        .cog, .lut, .regspace => {
                            if (cursor.local_bytes >= 0x200 * 4) {
                                try ana.emit_diag(lbl.location, .{ .err_address_outside_space = .{ .subject = .label, .space = cursor.mode, .actual = cursor.local_bytes / 4 + @as(u32, if (cursor.mode == .lut) 0x200 else 0), .max_exclusive = if (cursor.mode == .lut) 0x400 else 0x200 } });
                            }
                        },
                        .hub, .data => {},
                    }
                },

                .instruction => |*instr| {
                    const coded = &ana.instructions[ana.seq_to_instr_lut[i].?];
                    std.debug.assert(coded.ast_node == instr);

                    coded.start_addr = cursor.offset;
                    defer coded.end_addr = cursor.offset;

                    switch (coded.mnemonic.?.*) {
                        .assert => {},

                        .hubexec, .lutexec, .cogexec, .regspace, .data => {
                            const mode: eval.ExecMode = switch (coded.mnemonic.?.*) {
                                .hubexec => .hub,
                                .lutexec => .lut,
                                .cogexec => .cog,
                                .regspace => .regspace,
                                .data => .data,
                                else => unreachable,
                            };
                            const max_args: usize = if (mode == .data or mode == .hub or mode == .cog or mode == .lut) 1 else 0;
                            if (instr.arguments.len > max_args) {
                                try ana.emit_diag(instr.location, .{
                                    .err_argument_count_mismatch = .{
                                        .subject = instr.mnemonic,
                                        .min = 0,
                                        .max = max_args,
                                        .found = instr.arguments.len,
                                    },
                                });
                            }
                            var hub_offset: ?u32 = null;
                            if (instr.arguments.len == 1 and max_args == 1) hub_offset = try ana.layout_integer(instr.arguments[0], instr.location, instr.mnemonic);
                            if (hub_offset) |addr| {
                                if (addr > 0x80000) try ana.emit_diag(instr.location, .{ .err_address_outside_space = .{ .subject = .origin, .space = .hub, .actual = addr, .max_exclusive = 0x80001 } });
                            }
                            cursor.change_mode(idgen.next(), mode, hub_offset);
                            if (mode == .regspace) try ana.regspace_segments.append(ana.arena.allocator(), cursor.hub);

                            // We must change the start address here as we're changing the cursor mode here.
                            coded.start_addr = cursor.offset;
                        },

                        .@"align" => blk: {
                            if (coded.ast_node.arguments.len != 1) {
                                try ana.emit_diag(coded.ast_node.location, .{
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

                            if (ana.evaluate_root_expr(coded.ast_node.arguments[0], null)) |value| {
                                switch (value.value) {
                                    .int => |int| {
                                        if (std.math.cast(u32, int)) |alignment| {
                                            if (alignment == 0 or !std.math.isPowerOfTwo(alignment)) {
                                                try ana.emit_diag(coded.ast_node.location, .{
                                                    .err_align_value_must_be_a_nonzero_power_of_two = .{
                                                        .value = alignment,
                                                    },
                                                });
                                            } else if (alignment > 0x80000) {
                                                try ana.emit_diag(coded.ast_node.location, .{ .err_numeric_value_out_of_range = .{ .subject = ".align value", .min = 1, .max = 0x80000, .actual = alignment } });
                                            } else {
                                                cursor.alignas(alignment);
                                                coded.start_addr = cursor.offset;
                                            }
                                        } else {
                                            try ana.emit_diag(coded.ast_node.location, .{
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
                                        try ana.emit_diag(coded.ast_node.location, .{
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
                                    try ana.emit_diag(coded.ast_node.location, .err_align_references_label);
                                } else {
                                    try ana.emit_eval_error(coded.ast_node.location, .alignment, err);
                                }
                            }
                        },

                        .org => {
                            if (cursor.mode == .data) {
                                try ana.emit_diag(instr.location, .{ .err_directive_invalid_in_mode = .{ .directive = ".org", .mode = cursor.mode } });
                            } else if (instr.arguments.len != 1) {
                                try ana.emit_diag(instr.location, .{ .err_argument_count_mismatch = .{ .subject = ".org", .min = 1, .max = 1, .found = instr.arguments.len } });
                            } else if (try ana.layout_integer(instr.arguments[0], instr.location, ".org")) |target| {
                                const max: u32 = switch (cursor.mode) {
                                    .cog, .regspace => 0x200,
                                    .lut => 0x400,
                                    .hub => 0x80000,
                                    .data => unreachable,
                                };
                                if (target > max or (cursor.mode == .lut and target < 0x200)) {
                                    try ana.emit_diag(instr.location, .{ .err_numeric_value_out_of_range = .{ .subject = ".org target", .min = if (cursor.mode == .lut) 0x200 else 0, .max = max, .actual = target } });
                                    continue;
                                }
                                const current = if (cursor.mode == .hub) cursor.hub else cursor.local_bytes;
                                const requested = switch (cursor.mode) {
                                    .hub => target,
                                    .lut => (target - 0x200) * 4,
                                    else => target * 4,
                                };
                                if (requested < current) {
                                    try ana.emit_diag(instr.location, .err_org_cannot_move_pc_backward);
                                } else {
                                    cursor.org(target);
                                    coded.start_addr = cursor.offset;
                                }
                            }
                        },

                        .reserve => {
                            if (cursor.mode != .cog and cursor.mode != .regspace) {
                                try ana.emit_diag(instr.location, .{ .err_directive_invalid_in_mode = .{ .directive = ".reserve", .mode = cursor.mode } });
                            } else if (instr.arguments.len != 1) {
                                try ana.emit_diag(instr.location, .{ .err_argument_count_mismatch = .{ .subject = ".reserve", .min = 1, .max = 1, .found = instr.arguments.len } });
                            } else if (try ana.layout_integer(instr.arguments[0], instr.location, ".reserve")) |count| {
                                if (cursor.local_bytes / 4 > 0x200 or count > 0x200 - @min(cursor.local_bytes / 4, 0x200)) {
                                    try ana.emit_diag(instr.location, .{ .err_numeric_value_out_of_range = .{ .subject = ".reserve count", .min = 0, .max = 0x200 - @min(cursor.local_bytes / 4, 0x200), .actual = count } });
                                } else {
                                    cursor.reserve(count);
                                    coded.start_addr = cursor.offset;
                                }
                            }
                        },

                        .long, .word, .byte, .file => {
                            if (cursor.mode == .regspace or cursor.reserved) {
                                try ana.emit_diag(instr.location, .err_cannot_emit_data_after_reserve_or_inside_regspace);
                            } else {
                                const unit: u32 = switch (coded.mnemonic.?.*) {
                                    .long => 4,
                                    .word => 2,
                                    else => 1,
                                };
                                cursor.align_data(unit);
                                coded.start_addr = cursor.offset;
                                const size = coded.byte_size.?;
                                cursor.hub += size;
                                if (cursor.mode == .cog or cursor.mode == .lut) cursor.local_bytes += size;
                                cursor.sync();
                            }
                        },

                        .encoded => {
                            if (cursor.mode == .data or cursor.mode == .regspace or cursor.reserved) {
                                try ana.emit_diag(instr.location, .err_cannot_emit_code_in_this_segment);
                            } else {
                                cursor.align_data(4);
                                coded.start_addr = cursor.offset;
                                for (0..@divExact(coded.byte_size.?, 4)) |_| cursor.advance_data(.long);
                            }
                        },
                    }
                    if (cursor.hub > 0x80000) try ana.emit_diag(instr.location, .{ .err_address_outside_space = .{ .subject = .cursor, .space = .hub, .actual = cursor.hub, .max_exclusive = 0x80001 } });
                    switch (cursor.mode) {
                        .cog, .regspace => {
                            if (cursor.local_bytes > 0x200 * 4)
                                try ana.emit_diag(instr.location, .{ .err_address_outside_space = .{ .subject = .cursor, .space = cursor.mode, .actual = cursor.local_bytes / 4, .max_exclusive = 0x201 } });
                        },
                        .lut => {
                            if (cursor.local_bytes > 0x200 * 4)
                                try ana.emit_diag(instr.location, .{ .err_address_outside_space = .{ .subject = .cursor, .space = .lut, .actual = 0x200 + cursor.local_bytes / 4, .max_exclusive = 0x401 } });
                        },
                        .data, .hub => {},
                    }
                },
            }
        }
    }

    fn layout_integer(ana: *Analyzer, expr: ast.Expression, location: ast.Location, name: []const u8) !?u32 {
        const value = ana.evaluate_root_expr(expr, null) catch |err| {
            try ana.emit_diag(location, .{
                .err_requires_an_integer_known_during_layout = .{
                    .name = name,
                    .reason = err,
                },
            });
            return null;
        };
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

    ///
    /// Checks if any undefined labels are in our symbol table.
    /// NOTE: This function does not check "const" declarations!
    ///
    fn check_undefined_labels(ana: *Analyzer) !void {
        for (ana.symbols.values()) |sym| {
            errdefer logger.err("invalid symbol {s}", .{sym.name});
            switch (sym.type) {
                .code, .data => {
                    if (sym.offset == null)
                        return error.InvalidSymbol;
                    if (sym.value != null)
                        return error.InvalidSymbol;
                },
                .undefined, .constant, .builtin => {
                    // ignored
                    continue;
                },
            }
            if (sym.type != .builtin and !sym.referenced) {
                try ana.emit_diag(sym.location(), .{
                    .warn_symbol_has_no_references = .{
                        .name = sym.name,
                    },
                });
            }
        }
    }

    ///
    /// Checks if any undefined symbols are in our symbol table.
    ///
    fn check_undefined_symbols(ana: *Analyzer) !void {
        for (ana.symbols.values()) |sym| {
            errdefer logger.err("invalid symbol {s}", .{sym.name});
            switch (sym.type) {
                .undefined => {
                    std.debug.assert(sym.referenced);
                    ana.ok = false;
                    continue;
                },
                .code, .data => {
                    if (sym.offset == null)
                        return error.InvalidSymbol;
                    if (sym.value != null)
                        return error.InvalidSymbol;
                    continue;
                },
                .constant, .builtin => {
                    errdefer logger.err("invalid symbol {s}", .{sym.name});
                    if (sym.offset != null)
                        return error.InvalidSymbol;
                    if (sym.value == null)
                        return error.InvalidSymbol;
                },
            }
            if (sym.type != .builtin and !sym.referenced) {
                try ana.emit_diag(sym.location(), .{
                    .warn_symbol_has_no_references = .{
                        .name = sym.name,
                    },
                });
            }
        }
    }

    fn evaluate_constant_values(ana: *Analyzer) !void {
        for (ana.file.sequence) |seq| {
            if (seq != .constant)
                continue;
            const con = &seq.constant;
            const sym = ana.get_symbol_info(con.identifier) catch unreachable;
            std.debug.assert(sym.type == .constant);
            std.debug.assert(sym.value == null);
            std.debug.assert(sym.offset == null);

            const value = ana.evaluate_root_expr(con.value, null) catch |err| {
                try ana.emit_eval_error(con.location, .expression, err);
                continue;
            };

            switch (value.value) {
                .int, .string, .enumerator => {},

                .register => {
                    // TODO: Consider if this is OK or not. It's kinda handy, but not sure if hazardous
                },

                .address => {
                    try ana.emit_diag(con.location, .{
                        .err_constant_requires_integer_not_offset = .{
                            .name = con.identifier,
                        },
                    });
                    continue;
                },

                .pointer_expr => {
                    try ana.emit_diag(con.location, .err_constants_cannot_store_pointer_expression);
                    continue;
                },
            }

            sym.value = value;
        }
    }

    fn evaluate_instruction_arguments(ana: *Analyzer) !void {
        for (ana.instructions) |*instr| {
            std.debug.assert(instr.mnemonic != null);
            std.debug.assert(instr.start_addr != null);
            std.debug.assert(instr.end_addr != null);

            const args = try ana.arena.allocator().alloc(eval.Value, instr.ast_node.arguments.len);
            for (args, instr.ast_node.arguments) |*value, expr| {
                value.* = ana.evaluate_root_expr(expr, instr.end_addr.?) catch |err| {
                    try ana.emit_eval_error(instr.ast_node.location, .expression, err);
                    continue;
                };
            }

            instr.arguments = args;
        }
    }

    ///
    /// Selects the fitting encoding and required arguments for the mnemonic.
    ///
    fn select_instruction_encoding(ana: *Analyzer) !void {
        current_instr: for (ana.instructions) |*instr| {
            std.debug.assert(instr.mnemonic != null);
            std.debug.assert(instr.start_addr != null);
            std.debug.assert(instr.end_addr != null);
            std.debug.assert(instr.arguments.len == instr.ast_node.arguments.len);

            const mnemonic: *const EncodedMnemonic = switch (instr.mnemonic.?.*) {
                .encoded => |*mnemonic| mnemonic,

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
                try ana.emit_diag(instr.ast_node.location, .{
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
                logger.debug("  - {s}: {s} aug={}", .{ @tagName(arg.value), @tagName(arg.flags.usage), arg.flags.augment });
            }
            logger.debug("  alts:", .{});

            var selection: ?*const EncodedInstruction = null;
            match_alternative: for (alternatives.items) |alt| {
                logger.debug("  - {s}", .{alt.mnemonic});

                var can_assign = true;
                for (alt.operands, instr.arguments) |op, arg| {
                    const op_ok = op.type.can_assign_from(arg);
                    logger.debug("    - {s}; type ok={}", .{
                        @tagName(op.type),
                        op_ok,
                    });
                    if (!op_ok) {
                        can_assign = false;
                    }
                }
                if (instr.ast_node.effect) |effect| {
                    if (!alt.effects.contains(effect)) {
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
                        // special handling for .pointer_reg operands:
                        // "PA/PB/PTRA/PTRB" is preferred over regular "D" operands, so
                        // keep the instruction which fits better:

                        const any_ptrreg_prev: bool = for (previous.operands) |op| {
                            if (op.type == .pointer_reg)
                                break true;
                        } else false;
                        const any_ptrreg_now: bool = for (alt.operands) |op| {
                            if (op.type == .pointer_reg)
                                break true;
                        } else false;

                        if (any_ptrreg_prev == true and any_ptrreg_prev == true) {
                            @panic("incredibly amgigious instructions, should check the setup");
                        }

                        if (any_ptrreg_prev) {
                            // previous instruction is using the pointer_reg operand, so keep the old one:
                            continue :match_alternative;
                        }

                        if (any_ptrreg_now) {
                            // current instruction is using the pointer_reg operand, so use the new one
                            break :amgigious_check;
                        }

                        // neither use pointer_reg, so fall through into regular handling:
                    }

                    try ana.emit_diag(instr.ast_node.location, .{
                        .err_ambigious_instruction_selection_for = .{
                            .mnemonic = alt.mnemonic,
                        },
                    });
                    continue :current_instr;
                }
                selection = alt;
            }

            instr.instruction = selection orelse {
                try ana.emit_diag(instr.ast_node.location, .{
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

    fn evaluate_asserts(ana: *Analyzer) !void {
        for (ana.instructions) |*instr| {
            if (instr.mnemonic.?.* != .assert)
                continue;

            const with_message = switch (instr.arguments.len) {
                0 => {
                    try ana.emit_diag(instr.ast_node.location, .{ .err_argument_count_mismatch = .{ .subject = ".assert", .min = 1, .max = 2, .found = 0 } });
                    continue;
                },
                1 => false,
                2 => true,
                else => blk: {
                    try ana.emit_diag(instr.ast_node.location, .{
                        .err_argument_count_mismatch = .{
                            .subject = ".assert",
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
            if (condition.value != .int) {
                try ana.emit_diag(instr.ast_node.location, .{
                    .err_expected_value_type = .{
                        .subject = ".assert condition",
                        .expected = .int,
                        .actual = condition.value,
                    },
                });
                continue;
            }
            if (condition.value.int != 0) {
                // TODO: Think about emitting a warning if not 1/TRUE is yielded.
                continue;
            }

            var message: []const u8 = "expression returned 0";

            if (with_message) {
                const msg = instr.arguments[1];

                if (msg.value != .string) {
                    try ana.emit_diag(instr.ast_node.location, .{
                        .err_expected_value_type = .{
                            .subject = ".assert message",
                            .expected = .string,
                            .actual = msg.value,
                        },
                    });
                    continue;
                }
                message = msg.value.string;
            } else {
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
                        const lhs = try ana.evaluate_root_expr(arg_expr.binary_transform.lhs.*, null);
                        const rhs = try ana.evaluate_root_expr(arg_expr.binary_transform.rhs.*, null);

                        // TODO(0.15.2): Use "nice" formatting again:
                        message = try std.fmt.allocPrint(ana.arena.allocator(), "{f} {s} {f}!", .{ lhs, relation, rhs });
                    }
                }
            }

            try ana.emit_diag(instr.ast_node.location, .{
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
                            .location = lbl.location,
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
                .assert => continue :seq_loop,

                .@"align", .org, .reserve => continue :seq_loop,

                .cogexec, .lutexec, .hubexec, .regspace, .data => {
                    const new_mode: eval.ExecMode = switch (mnemonic) {
                        .cogexec => .cog,
                        .lutexec => .lut,
                        .hubexec => .hub,
                        .regspace => .regspace,
                        .data => .data,
                        else => unreachable,
                    };
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
                try current_segment.writer().splatByteAll(0xFF, hub_offset - segment_end_hub_offset);
                try ana.emit_diag(instr.ast_node.location, .{
                    .warn_emitted_padding_byte_s = .{
                        .count = hub_offset - segment_end_hub_offset,
                    },
                });
            }

            const line_info = try ana.line_data.addOne(segment_allocator);
            line_info.* = .{
                .offset = hub_offset,
                .length = 0,
                .location = instr.ast_node.location,
                .pc = instr.start_addr.?.get_local(.pc),
            };
            defer line_info.length = @intCast((current_segment.hub_offset + current_segment.len()) - hub_offset);

            logger.debug("emit {s}", .{@tagName(mnemonic)});

            switch (mnemonic) {
                .assert,
                .cogexec,
                .lutexec,
                .hubexec,
                .regspace,
                .data,
                .org,
                .reserve,
                .@"align",
                => unreachable,

                .file => try current_segment.writer().writeAll(instr.file_data),

                inline .byte, .word, .long => |_, tag| {
                    const T = switch (tag) {
                        .byte => u8,
                        .word => u16,
                        .long => u32,
                        else => unreachable,
                    };

                    for (instr.arguments, instr.ast_node.arguments) |container_value, ast_node| {
                        const value: T = try ana.cast_value_to(
                            ast_node.location(),
                            if (current_segment.exec_mode == .data) .hub else current_segment.exec_mode,
                            container_value,
                            .data,
                            T,
                        );
                        try current_segment.writer().writeInt(T, value, .little);
                    }
                },

                .encoded => {
                    const encoded = instr.instruction.?;

                    var output: u32 = encoded.binary;

                    const cond_code: ast.Condition.Code = if (instr.ast_node.condition) |condition|
                        condition.type.encode()
                    else if (std.ascii.eqlIgnoreCase(encoded.mnemonic, "NOP"))
                        .@"return" // TODO: Remove this special case, it's weird. Should be encoded as a flag
                    else
                        encoded.default_condition;

                    const condition_slot: EncodedInstruction.Slot = comptime .from_mask(0xF000_0000);
                    try condition_slot.write(&output, @intFromEnum(cond_code));

                    if (instr.ast_node.effect) |effect| {
                        if (!encoded.effects.contains(effect)) {
                            try ana.emit_diag(instr.ast_node.location, .{
                                .err_canont_use_the_effect_operator = .{
                                    .mnemonic = encoded.mnemonic,
                                    .effect = effect,
                                },
                            });
                            continue;
                        }

                        const write_mask = effect.get_write_mask();

                        if (write_mask.c) {
                            const slot = encoded.c_effect_slot orelse return error.BadInstructionEncoding;
                            slot.fill(&output);
                        }

                        if (write_mask.z) {
                            const slot = encoded.z_effect_slot orelse return error.BadInstructionEncoding;
                            slot.fill(&output);
                        }
                    } else {
                        if (!encoded.effects.none) {
                            try ana.emit_diag(instr.ast_node.location, .{
                                .err_cannot_be_used_without_effect_operator = .{
                                    .mnemonic = encoded.mnemonic,
                                },
                            });
                            continue;
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
                                                    if (ana.options.use_label_relative_hub_to_hub_jmp) {
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

                                                if (current_segment.exec_mode == target_mode) {
                                                    std.log.debug("#{{/}}A: src mode={} address={f} address:int=0x{X:0>6} cog={} lut={} hub={}", .{
                                                        current_segment.exec_mode,
                                                        value,
                                                        int,
                                                        int < 0x200,
                                                        int >= 0x200 and int < 0x400,
                                                        int >= 0x400,
                                                    });
                                                    // We're targeting the same execution mode with a non-label address,
                                                    // so we need to adhere to the user option selection:
                                                    if (ana.options.use_relative_jmp_for_same_mode_nonlabel_address) {
                                                        continue :selector .relative;
                                                    } else {
                                                        continue :selector .absolute;
                                                    }
                                                } else {
                                                    // If we would change the execution mode, we must
                                                    // always perform absolute jumps:
                                                    continue :selector .absolute;
                                                }
                                            },
                                        }
                                    },

                                    .absolute => {
                                        fill_extra_slot = null;
                                        break :selector int;
                                    },

                                    .relative => {

                                        // "A" addressing always uses byte offsets, even if jumping in cog/lut mode:
                                        const target_address: u32 = switch (value.value) {
                                            .address => |addr| addr.hub_address orelse {
                                                try ana.emit_diag(location, .{ .err_address_has_no_hub_location = .branch_target });
                                                break :selector 0;
                                            },
                                            else => int,
                                        };

                                        const byte_delta_i33: i33 = @as(i33, target_address) - @as(i33, hub_pc);

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
                                    } else if (hint == .literal and int > 255) {
                                        try ana.emit_diag(location, .{
                                            .err_numeric_value_out_of_range = .{
                                                .subject = "pointer immediate",
                                                .min = 0,
                                                .max = 255,
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

        logger.debug("pcrel: cog={} hub={} target={}:{s} => rel {}", .{ cog_pc, hub_pc, int, @tagName(exec_mode), delta33 });

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
            .string => @panic("string emission not supported yet"),
            .enumerator => @panic("BUG: enumerators must be handled before this!"),
            .register => |reg| @intFromEnum(reg),

            .pointer_expr => |ptr_expr| try ana.encode_ptr_expr(location, ptr_expr),
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

    fn encode_ptr_expr(ana: *Analyzer, location: ast.Location, expr: eval.PointerExpression) !u9 {
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
    };

    fn evaluate_root_expr(ana: *Analyzer, expr: ast.Expression, current_address: ?TaggedAddress) EvalError!eval.Value {
        return ana.evaluate_expr(expr, current_address, 0);
    }

    fn evaluate_expr(ana: *Analyzer, expr: ast.Expression, maybe_current_address: ?TaggedAddress, nesting: usize) EvalError!eval.Value {
        switch (expr) {
            .wrapped => |inner| return try ana.evaluate_expr(inner.*, maybe_current_address, nesting + 1),
            .integer => |int| return .int(int.value),
            .string => |string| return .string(string.value),
            .enumerator => |enumerator| return .enumerator(enumerator.symbol_name),
            .symbol => |symref| {
                const sym = ana.get_label_info(symref.symbol_name, symref.local_scope) catch unreachable;

                return switch (sym.type) {
                    .undefined => return error.UndefinedSymbol,
                    .code => .address(sym.offset orelse return error.UndefinedSymbol, .literal),
                    .data => .address(sym.offset orelse return error.UndefinedSymbol, .register),
                    .constant => sym.value orelse return error.UndefinedSymbol,
                    .builtin => sym.value.?,
                };
            },

            .unary_transform => |op| {
                const value = try ana.evaluate_expr(op.value.*, maybe_current_address, nesting + 1);

                switch (op.operator) {
                    .post_decrement,
                    .post_increment,
                    .pre_decrement,
                    .pre_increment,
                    => {
                        var ptr_expr: eval.PointerExpression = switch (value.value) {
                            .register => |reg| try ana.ptr_expr_from_reg(op.location, reg),

                            .pointer_expr => |ptr_expr| ptr_expr,

                            else => blk: {
                                try ana.emit_diag(op.location, .{
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
                            try ana.emit_diag(op.location, .{
                                .err_pointer_modifier_already_set = .{
                                    .operator = .{ .unary = op.operator },
                                    .modifier = .index,
                                },
                            });
                        } else if (ptr_expr.increment != .none) {
                            try ana.emit_diag(op.location, .{
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
                    try ana.emit_diag(op.location, .{
                        .err_operator_invalid_operand_type = .{
                            .operator = .{ .unary = op.operator },
                            .value_type = .register,
                        },
                    });
                    return value;
                }
                if (value.value == .enumerator) {
                    try ana.emit_diag(op.location, .{
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

                    .@"!" => {
                        if (value.value != .int) {
                            try ana.emit_diag(op.location, .{
                                .err_operator_invalid_operand_type = .{
                                    .operator = .{ .unary = op.operator },
                                    .value_type = value.value,
                                },
                            });
                            return .int(0);
                        }
                        return .int(@intFromBool(value.value.int == 0));
                    },
                    .@"~" => {
                        if (value.value != .int) {
                            try ana.emit_diag(op.location, .{
                                .err_operator_invalid_operand_type = .{
                                    .operator = .{ .unary = op.operator },
                                    .value_type = value.value,
                                },
                            });
                            return .int(0);
                        }
                        return .int(~value.value.int);
                    },
                    .@"+" => {
                        if (value.value != .int) {
                            try ana.emit_diag(op.location, .{
                                .err_operator_invalid_operand_type = .{
                                    .operator = .{ .unary = op.operator },
                                    .value_type = value.value,
                                },
                            });
                            return .int(0);
                        }
                        return value;
                    },
                    .@"-" => {
                        if (value.value != .int) {
                            try ana.emit_diag(op.location, .{
                                .err_operator_invalid_operand_type = .{
                                    .operator = .{ .unary = op.operator },
                                    .value_type = value.value,
                                },
                            });
                            return .int(0);
                        }
                        return .int(-value.value.int);
                    },
                    .@"@" => {
                        if (value.value != .address) {
                            try ana.emit_diag(op.location, .{
                                .err_operator_invalid_operand_type = .{
                                    .operator = .{ .unary = op.operator },
                                    .value_type = value.value,
                                },
                            });
                            return .int(0);
                        }

                        const local_offset: TaggedAddress = maybe_current_address orelse {
                            try ana.emit_diag(op.location, .err_operator_at_cannot_be_used_in_this_scope);
                            return .int(0);
                        };
                        const target_offset: TaggedAddress = value.value.address;

                        // TODO: Validate local_offset and target_offset point into the same segment

                        const local_hub_addr: u32 = local_offset.hub_address orelse {
                            try ana.emit_diag(op.location, .{ .err_address_has_no_hub_location = .current });
                            return .int(0);
                        };
                        const target_hub_addr: u32 = target_offset.hub_address orelse {
                            try ana.emit_diag(op.location, .{ .err_address_has_no_hub_location = .target });
                            return .int(0);
                        };

                        const jmp_delta = @as(i33, target_hub_addr) - @as(i33, local_hub_addr);

                        if (@mod(jmp_delta, 4) != 0) {
                            try ana.emit_diag(op.location, .err_address_delta_not_divisible_by_four);
                        }

                        return .int(@divTrunc(jmp_delta, 4));
                    },
                    .@"*" => {
                        if (value.value != .address) {
                            try ana.emit_diag(op.location, .{
                                .err_operator_invalid_operand_type = .{
                                    .operator = .{ .unary = op.operator },
                                    .value_type = value.value,
                                },
                            });
                            return .address(if (maybe_current_address) |addr|
                                addr
                            else
                                .init_hub(undefined, 0), .literal);
                        }
                        if (value.flags.usage == .register) {
                            try ana.emit_diag(op.location, .{ .warn_operator_no_effect = .{ .operator = op.operator, .label = .data } });
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
                            try ana.emit_diag(op.location, .{
                                .err_operator_invalid_operand_type = .{
                                    .operator = .{ .unary = op.operator },
                                    .value_type = value.value,
                                },
                            });
                            return .address(.init_hub(undefined, 0), .literal);
                        }
                        if (value.flags.usage == .literal) {
                            try ana.emit_diag(op.location, .{ .warn_operator_no_effect = .{ .operator = op.operator, .label = .code } });
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
                const lhs = try ana.evaluate_expr(op.lhs.*, maybe_current_address, nesting + 1);
                const rhs = try ana.evaluate_expr(op.rhs.*, maybe_current_address, nesting + 1);

                const lhs_type: Value.Type = lhs.value;
                const rhs_type: Value.Type = rhs.value;

                if (op.operator == .array_index) {
                    const lhs_ok = (lhs_type == .register or lhs_type == .pointer_expr);
                    const rhs_ok = (rhs_type == .int);

                    if (!lhs_ok or !rhs_ok) {
                        try ana.emit_diag(op.location, .{
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
                        .register => |reg| try ana.ptr_expr_from_reg(op.location, reg),
                        else => unreachable,
                    };
                    if (src_expr.index != null) {
                        try ana.emit_diag(op.location, .{ .err_pointer_modifier_already_set = .{ .operator = .{ .binary = op.operator }, .modifier = .index } });
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

                if (lhs_type != rhs_type) {
                    try ana.emit_diag(op.location, .{
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
                        try ana.execute_int_op(op.location, lhs.value.int, rhs.value.int, op.operator),
                    ),
                    .register => {
                        try ana.emit_diag(op.location, .{
                            .err_operator_invalid_operand_type = .{
                                .operator = .{ .binary = op.operator },
                                .value_type = .register,
                            },
                        });
                        return .register(0);
                    },
                    .enumerator => {
                        try ana.emit_diag(op.location, .{
                            .err_operator_invalid_operand_type = .{
                                .operator = .{ .binary = op.operator },
                                .value_type = .enumerator,
                            },
                        });
                        return .enumerator("");
                    },
                    .pointer_expr => {
                        try ana.emit_diag(op.location, .{
                            .err_operator_invalid_operand_type = .{
                                .operator = .{ .binary = op.operator },
                                .value_type = .pointer_expr,
                            },
                        });
                        return .enumerator("");
                    },
                    .address => @panic("TODO: Implement binary operators on offsets."),
                    .string => @panic("TODO: Implement binary operators on strings."),
                }
            },
            .function_call => |fncall| {
                const func = ana.get_function(fncall.function).?;

                const params = func.get_parameters();

                const argv_res = try ana.map_function_args(
                    fncall,
                    params,
                    maybe_current_address,
                    nesting,
                );
                const argv = argv_res.constSlice();
                std.debug.assert(argv.len == params.len);

                switch (func.*) {
                    .user => |f| {
                        const ctx: FunctionCallContext = .{
                            .ana = ana,
                            .location = fncall.location,
                        };

                        return f.invoke(ctx, argv) catch |err| switch (err) {
                            error.InvalidArgCount => unreachable, // we check that before
                            else => |e| return e,
                        };
                    },

                    .aug => {
                        std.debug.assert(argv.len == 1);
                        if (nesting != 0) {
                            try ana.emit_diag(fncall.arguments[0].location, .{ .err_function_must_be_root = .{ .function = "aug" } });
                        }
                        var value = argv[0];
                        value.flags.augment = true;
                        return value;
                    },

                    .nrel => {
                        std.debug.assert(argv.len == 1);
                        if (nesting != 0) {
                            try ana.emit_diag(fncall.arguments[0].location, .{ .err_function_must_be_root = .{ .function = "nrel" } });
                        }
                        var value = argv[0];
                        value.flags.addressing = .absolute;
                        return value;
                    },

                    .cogaddr, .lutaddr, .localaddr => {
                        const address_function: diagnostics.AddressFunction = switch (func.*) {
                            .cogaddr => .cogaddr,
                            .lutaddr => .lutaddr,
                            .localaddr => .localaddr,
                            else => unreachable,
                        };
                        const loc = fncall.arguments[0].location;
                        std.debug.assert(argv.len == 1);
                        const value = argv[0];
                        switch (value.value) {
                            .string, .enumerator, .pointer_expr => {
                                try ana.emit_diag(fncall.arguments[0].location, .{
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
                                    // TODO: Check if "execution mode" is .cogexec
                                    try ana.emit_diag(loc, .err_localaddr_is_only_valid_for_registers_in_a_cogexec_scope);
                                }

                                try ana.emit_diag(fncall.arguments[0].location, .{
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
                                        try ana.emit_diag(fncall.arguments[0].location, .{
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
                                try ana.emit_diag(fncall.arguments[0].location, .{
                                    .warn_address_function_expected_offset = .{
                                        .function = .hubaddr,
                                        .value_type = value.value,
                                    },
                                });
                                return value;
                            },
                            .string, .register, .enumerator, .pointer_expr => {
                                try ana.emit_diag(fncall.arguments[0].location, .{
                                    .err_address_function_invalid_operand_type = .{
                                        .function = .hubaddr,
                                        .value_type = value.value,
                                    },
                                });
                                return value;
                            },
                            .address => |address| {
                                const hub = address.hub_address orelse {
                                    try ana.emit_diag(fncall.arguments[0].location, .{ .err_address_has_no_hub_location = .hubaddr_argument });
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
            .@"/" => if (rhs != 0) @divFloor(lhs, rhs) else return error.DivideByZero,
            .@"%" => if (rhs != 0) @mod(lhs, rhs) else return error.DivideByZero,
            .array_index => unreachable,
        };
    }

    const max_supported_parameters = 16;

    fn map_function_args(
        ana: *Analyzer,
        fncall: ast.FunctionInvocation,
        params: []const Function.Parameter,
        maybe_current_address: ?TaggedAddress,
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
                    try ana.emit_diag(arg.location, .err_positional_after_named_argument);
                    ok = false;
                }
            }

            if (fncall.arguments.len < params.len - default_arg_count or fncall.arguments.len > params.len) {
                try ana.emit_diag(fncall.location, .{
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
            value.* = try ana.evaluate_expr(arg.value, maybe_current_address, nesting + 1);
            argv_ok.set(index);
        }

        {
            var ok = true;
            for (kw_argin) |arg| {
                std.debug.assert(arg.name != null);

                const index = index_of_param(params, arg.name.?) orelse {
                    ok = false;
                    try ana.emit_diag(arg.location, .{
                        .err_has_no_parameter_named = .{
                            .function = fncall.function,
                            .parameter = arg.name.?,
                        },
                    });
                    continue;
                };
                if (index < first_kwarg_index) {
                    ok = false;
                    try ana.emit_diag(arg.location, .{
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
                        try ana.emit_diag(arg2.location, .{
                            .err_parameter_already_passed = .{
                                .parameter = arg1.name.?,
                                .function = fncall.function,
                                .previous = .{ .named = arg1.location },
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
            kw_argv[index] = try ana.evaluate_expr(arg.value, maybe_current_address, nesting + 1);
            argv_ok.set(index);
        }

        {
            var all_ok = true;
            for (params, 0..) |param, index| {
                if (!argv_ok.isSet(index)) {
                    // This error can only happen for non-defaulted parameters
                    std.debug.assert(param.default_value == null);
                    try ana.emit_diag(fncall.location, .{
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
    }

    fn change_mode(cursor: *Cursor, seg: Segment_ID, mode: eval.ExecMode, hub_offset: ?u32) void {
        cursor.mode = mode;
        cursor.hub = hub_offset orelse cursor.hub;
        cursor.local_bytes = 0;
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
    try diagnostics_collection.register_source("test.propan", source);

    var parser: frontend.Parser = .init(source, "test.propan", &diagnostics_collection);
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
        try diagnostics_collection.register_source("test.propan", source);

        var parser: frontend.Parser = .init(source, "test.propan", &diagnostics_collection);
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
    try diagnostics_collection.register_source("test.propan", source);

    var parser: frontend.Parser = .init(source, "test.propan", &diagnostics_collection);
    var parsed = try parser.parse(std.testing.allocator);
    defer parsed.deinit();

    var module = try analyze(std.testing.allocator, parsed.file, .{}, &diagnostics_collection);
    defer module.deinit();
    try std.testing.expectEqual(@as(usize, 1), module.segments.len);
    try std.testing.expectEqual(@as(usize, 1), module.symbols.len);
    try std.testing.expectEqual(module.symbols[0].label.segment_id, module.segments[0].id);
}

test "semantic warnings are collected without failing analysis" {
    const source =
        \\_start:
        \\NOP
        \\
    ;

    var diagnostics_collection: diagnostics.Collection = .init(std.testing.allocator);
    defer diagnostics_collection.deinit();
    try diagnostics_collection.register_source("test.propan", source);

    var parser: frontend.Parser = .init(source, "test.propan", &diagnostics_collection);
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
    try std.testing.expectEqual(TaggedAddress.init_cog(seg, 10, 2), cursor.offset);

    cursor.advance_data(.word);
    try std.testing.expectEqual(TaggedAddress.init_cog(seg, 12, 3), cursor.offset);

    cursor.advance_data(.byte);
    try std.testing.expectEqual(TaggedAddress.init_cog(seg, 13, 3), cursor.offset);

    cursor.advance_data(.byte);
    try std.testing.expectEqual(TaggedAddress.init_cog(seg, 14, 3), cursor.offset);

    cursor.advance_data(.byte);
    try std.testing.expectEqual(TaggedAddress.init_cog(seg, 15, 3), cursor.offset);

    cursor.advance_data(.byte);
    try std.testing.expectEqual(TaggedAddress.init_cog(seg, 16, 4), cursor.offset);

    cursor.advance_code();
    try std.testing.expectEqual(TaggedAddress.init_cog(seg, 20, 5), cursor.offset);

    cursor.advance_data(.byte);
    try std.testing.expectEqual(TaggedAddress.init_cog(seg, 21, 5), cursor.offset);

    cursor.advance_code();
    try std.testing.expectEqual(TaggedAddress.init_cog(seg, 28, 7), cursor.offset);

    cursor.advance_data(.byte);
    try std.testing.expectEqual(TaggedAddress.init_cog(seg, 29, 7), cursor.offset);

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
    try collection.register_source(path, source);
    var parser: frontend.Parser = .init(source, path, collection);
    var parsed = try parser.parse(std.testing.allocator);
    defer parsed.deinit();
    return analyze(std.testing.allocator, parsed.file, options, collection);
}

fn test_symbol(module: Module, name: []const u8) TaggedAddress {
    for (module.symbols) |symbol| if (std.mem.eql(u8, symbol.name, name)) return symbol.label;
    @panic("missing test symbol");
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
    try std.testing.expectEqualSlices(u8, &.{ 1, 2, 3, 4, 6, 5, 0xFF, 0xFF, 0x0A, 9, 8, 7, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 9 }, module.segments[0].data);
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
        \\.reserve 2
        \\var cogvar:
        \\.regspace
        \\.org 0x1F0
        \\var reg:
        \\.reserve 2
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
        \\.reserve 1
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
    try std.testing.expectEqualSlices(u8, &.{ 1, 0xFF, 0xFF, 'x', 'y', 'z', '\n' }, module.segments[0].data);
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
    try std.testing.expectEqual(@as(u8, 0xFF), module.segments[0].data[0]);
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
        ".cogexec\n.reserve 1\nLONG 2\n",
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
        ".cogexec\n.reserve 1\nvar x:\n.assert hubaddr(x) == 0\n",
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

    /// Size of the instruction slot in bytes
    byte_size: ?u32 = null,
    file_data: []const u8 = &.{},

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

    // stdlib functions are defined as "generic" ones:
    user: UserFunction,

    pub fn get_parameters(func: Function) []const Parameter {
        return switch (func) {
            .aug => &.{.init("value", .int)},
            .nrel => &.{.init("addr", .int)},
            .hubaddr => &.{.init("addr", .address)},
            .cogaddr => &.{.init("addr", .address)},
            .lutaddr => &.{.init("addr", .address)},
            .localaddr => &.{.init("addr", .address)},
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
    reserve,
    @"align",
    assert,

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
                if (value.value == .string) {
                    // strings cannot be assigned to an operand
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

        pub fn @"union"(lhs: Effects, rhs: Effects) Effects {
            var results = std.mem.zeroes(Effects);
            inline for (std.meta.fields(Effects)) |fld| {
                @field(results, fld.name) = @field(lhs, fld.name) and @field(rhs, fld.name);
            }
            return results;
        }

        pub fn any(value: Effects) bool {
            inline for (std.meta.fields(Effects)) |fld| {
                if (@field(value, fld.name))
                    return true;
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
            if (size >= cap)
                return error.OutOfMemory;
            arr.len = size;
        }

        pub fn append(arr: *@This(), item: T) error{OutOfMemory}!void {
            if (arr.len >= cap)
                return error.OutOfMemory;
            arr.items[arr.len] = item;
            arr.len += 1;
        }

        pub fn slice(arr: *@This()) []T {
            return arr.items[0..arr.len];
        }

        pub fn constSlice(arr: *const @This()) []const T {
            return arr.items[0..arr.len];
        }
    };
}
