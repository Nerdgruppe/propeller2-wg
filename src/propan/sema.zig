const std = @import("std");

const stdlib = @import("stdlib/stdlib.zig");
const eval = @import("stdlib/eval.zig");
const ast = @import("frontend.zig").ast;

const logger = std.log.scoped(.sema);

const Value = eval.Value;

pub const Offset = eval.Offset;

const PA: eval.Register = @enumFromInt(0x1F6);
const PB: eval.Register = @enumFromInt(0x1F7);
const PTRA: eval.Register = @enumFromInt(0x1F8);
const PTRB: eval.Register = @enumFromInt(0x1F9);

pub fn analyze(allocator: std.mem.Allocator, file: ast.File) !Module {
    var analyzer: Analyzer = try .init(allocator, file);
    defer analyzer.deinit();

    errdefer dump_analyzer(&analyzer);

    // Prepare
    try analyzer.load_constants(stdlib.common.constants);
    try analyzer.load_constants(stdlib.p2.constants);

    try analyzer.load_instructions(stdlib.p2.instructions);

    // Validate
    try analyzer.declare_symbols();
    try analyzer.validate_symbol_refs();

    // Lay Out
    try analyzer.prepare_instruction_stream();
    try analyzer.select_instruction_mnemonic();

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

    const segments = try analyzer.emit_code(output_arena.allocator());

    return .{
        .arena = output_arena,
        .segments = segments,
    };
}

fn dump_analyzer(analyzer: *Analyzer) void {
    logger.info("symbols:", .{});
    for (analyzer.symbols.values()) |sym| {
        // if (sym.type == .builtin)
        //     continue;
        logger.info("  {s}: {s} => offset={?}, value={?}, referenced={}", .{
            sym.name,
            @tagName(sym.type),
            sym.offset,
            sym.value,
            sym.referenced,
        });
    }

    logger.info("map file:", .{});
    for (analyzer.instructions) |instr| {
        logger.info("  {}: {s} (+{} bytes)", .{
            instr.offset.?,
            instr.ast_node.mnemonic,
            instr.byte_size.?,
        });
    }

    logger.info("map file:", .{});
    for (analyzer.instructions) |instr| {
        logger.info("  {}: {s}", .{
            instr.offset.?,
            instr.ast_node.mnemonic,
        });
        for (instr.arguments, 0..) |arg, i| {
            logger.info("    [{}] = {}", .{ i, arg });
        }
    }
}

pub const Module = struct {
    arena: std.heap.ArenaAllocator,

    segments: []const Segment,

    pub fn deinit(mod: *Module) void {
        mod.arena.deinit();
        mod.* = undefined;
    }
};

pub const Symbol = struct {
    offset: Offset,
    type: Type,

    pub const Type = enum {
        code,
        data,
    };
};

pub const Constant = struct {
    value: Constant.Value,

    pub const Value = union(enum) {
        integer: u64,
        string: []const u8,
    };
};

pub const Segment = struct {
    hub_offset: u32,
    data: []const u8,
};

const Analyzer = struct {
    allocator: std.mem.Allocator,
    arena: std.heap.ArenaAllocator,
    file: ast.File,

    symbols: std.StringArrayHashMapUnmanaged(SymbolInfo) = .empty,

    functions: std.StringArrayHashMapUnmanaged(Function) = .empty,
    mnemonics: CaseInsensitiveStringHashMap(Mnemonic) = .empty,

    seq_to_instr_lut: []const ?usize = &.{},
    instructions: []InstructionInfo = &.{},

    ok: bool = true,

    fn init(allocator: std.mem.Allocator, file: ast.File) !Analyzer {
        var ana: Analyzer = .{
            .allocator = allocator,
            .arena = .init(allocator),
            .file = file,
        };
        try ana.functions.ensureUnusedCapacity(allocator, 5);

        ana.functions.putAssumeCapacityNoClobber("hubaddr", .hubaddr);
        ana.functions.putAssumeCapacityNoClobber("cogaddr", .cogaddr);
        ana.functions.putAssumeCapacityNoClobber("lutaddr", .lutaddr);
        ana.functions.putAssumeCapacityNoClobber("localaddr", .localaddr);
        ana.functions.putAssumeCapacityNoClobber("aug", .aug);
        ana.functions.putAssumeCapacityNoClobber("nrel", .nrel);

        try ana.mnemonics.ensureUnusedCapacity(allocator, 9);

        ana.mnemonics.putAssumeCapacityNoClobber(".cogexec", .cogexec);
        ana.mnemonics.putAssumeCapacityNoClobber(".lutexec", .lutexec);
        ana.mnemonics.putAssumeCapacityNoClobber(".hubexec", .hubexec);
        ana.mnemonics.putAssumeCapacityNoClobber(".align", .@"align");
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

    fn emit_error(ana: *Analyzer, location: ?ast.Location, comptime fmt: []const u8, args: anytype) !void {
        ana.ok = false;
        std.log.err("{?}: " ++ fmt, .{location} ++ args);
    }

    fn emit_warning(ana: *Analyzer, location: ?ast.Location, comptime fmt: []const u8, args: anytype) !void {
        _ = ana;
        std.log.warn("{?}: " ++ fmt, .{location} ++ args);
    }

    fn emit_eval_error(ana: *Analyzer, location: ast.Location, comptime prefix: []const u8, err: EvalError) !void {
        const reason = switch (err) {
            error.OutOfMemory => "out of memory",
            error.UndefinedSymbol => "referenced undefined symbol",
            error.InvalidFunctionCall => "invalid function call",
            error.Overflow => "integer overflow",
            error.DivideByZero => "division by zero",
        };
        const true_prefix = if (prefix.len == 0)
            "could not evaluate expression: "
        else
            prefix;

        try ana.emit_error(
            location,
            true_prefix ++ "{s}",
            .{reason},
        );
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
                try ana.emit_error(null, "duplicate predefined symbol: {s} {}", .{ name, gop.value_ptr.type });
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
                    const sym = try ana.get_symbol_info(lbl.identifier);
                    if (sym.type != .undefined) {
                        try ana.emit_error(lbl.location, "symbol {s} ({}) is already defined!", .{ lbl.identifier, sym.type });
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
                        try ana.emit_error(con.location, "symbol {s} ({}) is already defined!", .{ con.identifier, sym.type });
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
                const sym = try ana.get_symbol_info(symref.symbol_name);
                sym.referenced = true;
                if (sym.type == .undefined) {
                    try ana.emit_error(symref.location, "undefined reference to symbol {s} at {}", .{ sym.name, symref.location });
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
                    try ana.emit_error(fncall.location, "use of undefined function {s}", .{fncall.function});
                }

                for (fncall.arguments) |arg| {
                    try ana.validate_expr_symbol_refs(arg.value);
                }
            },

            // These values don't require checks:
            .integer => {},
            .string => {},
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
                try ana.emit_error(instr.ast_node.location, "unknown mnemonic {s}", .{instr.ast_node.mnemonic});
                continue;
            };

            instr.mnemonic = mnemonic;

            instr.byte_size = switch (mnemonic.*) {
                .cogexec, .lutexec, .hubexec, .assert, .@"align" => 0,

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
                                const func = ana.get_function(fncall.function) orelse {
                                    try ana.emit_error(fncall.location, "unknown function {s}", .{fncall.function});
                                    continue;
                                };
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

        var cursor: Cursor = .{};

        for (ana.file.sequence, 0..) |*seq, i| {
            switch (seq.*) {
                .empty, .constant => {},

                .label => |lbl| {
                    const sym = ana.get_symbol_info(lbl.identifier) catch unreachable;
                    sym.offset = cursor.offset;
                },

                .instruction => |*instr| {
                    const coded = &ana.instructions[ana.seq_to_instr_lut[i].?];
                    std.debug.assert(coded.ast_node == instr);

                    coded.offset = cursor.offset;
                    switch (coded.mnemonic.?.*) {
                        .assert => {},

                        .hubexec => cursor.change_mode(.hub),
                        .lutexec => cursor.change_mode(.lut),
                        .cogexec => cursor.change_mode(.cog),

                        .@"align" => blk: {
                            if (coded.ast_node.arguments.len != 1) {
                                try ana.emit_error(coded.ast_node.location, ".align requires exactly one argument, but found {}", .{coded.ast_node.arguments.len});
                            }
                            if (coded.ast_node.arguments.len < 1) {
                                break :blk;
                            }

                            if (ana.evaluate_root_expr(coded.ast_node.arguments[0])) |value| {
                                switch (value.value) {
                                    .int => |int| {
                                        if (std.math.cast(u32, int)) |alignment| {
                                            cursor.alignas(alignment);
                                        } else {
                                            try ana.emit_error(coded.ast_node.location, ".align value {} is out of range.", .{
                                                int,
                                            });
                                        }
                                    },
                                    else => {
                                        try ana.emit_error(coded.ast_node.location, ".align value evaluated to {s}, but expected integer.", .{
                                            @tagName(value.value),
                                        });
                                    },
                                }
                            } else |err| {
                                if (err == error.UndefinedSymbol) {
                                    try ana.emit_error(coded.ast_node.location, ".align value could not be evaluated: cannot refer to labels in .align", .{});
                                } else {
                                    try ana.emit_eval_error(coded.ast_node.location, ".align value could not be evaluated: ", err);
                                }
                            }

                            @panic("not implemented yet.");
                        },

                        .long => for (coded.ast_node.arguments) |_| {
                            cursor.advance_data(.long);
                        },
                        .word => for (coded.ast_node.arguments) |_| {
                            cursor.advance_data(.word);
                        },
                        .byte => for (coded.ast_node.arguments) |_| {
                            cursor.advance_data(.byte);
                        },

                        .encoded => {
                            for (0..@divExact(coded.byte_size.?, 4)) |_| {
                                cursor.advance_code();
                            }
                        },
                    }
                },
            }
        }
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
                },
            }
            if (sym.type != .builtin and !sym.referenced) {
                try ana.emit_warning(sym.location(), "symbol {s} has no references", .{sym.name});
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
                try ana.emit_warning(sym.location(), "symbol {s} has no references", .{sym.name});
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

            const value = ana.evaluate_root_expr(con.value) catch |err| {
                try ana.emit_eval_error(con.location, "", err);
                continue;
            };

            switch (value.value) {
                .int, .string, .enumerator => {},

                .register => {
                    // TODO: Consider if this is OK or not. It's kinda handy, but not sure if hazardous
                },

                .offset => {
                    try ana.emit_error(con.location, "constant {s} evaluated to memory offset, but expected integer. Use hubaddr() or cogaddr() to resolve the value.", .{con.identifier});
                    continue;
                },

                .pointer_expr => {
                    try ana.emit_error(con.location, "constants cannot store pointer expression.", .{});
                    continue;
                },
            }

            sym.value = value;
        }
    }

    fn evaluate_instruction_arguments(ana: *Analyzer) !void {
        for (ana.instructions) |*instr| {
            std.debug.assert(instr.mnemonic != null);

            const args = try ana.arena.allocator().alloc(eval.Value, instr.ast_node.arguments.len);
            for (args, instr.ast_node.arguments) |*value, expr| {
                value.* = ana.evaluate_root_expr(expr) catch |err| {
                    try ana.emit_eval_error(instr.ast_node.location, "", err);
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
            std.debug.assert(instr.offset != null);
            std.debug.assert(instr.arguments.len == instr.ast_node.arguments.len);

            const mnemonic: *const EncodedMnemonic = switch (instr.mnemonic.?.*) {
                .encoded => |*mnemonic| mnemonic,

                // these are all already defined
                else => continue :current_instr,
            };
            std.debug.assert(mnemonic.variants.items.len > 0);

            var alternatives: std.BoundedArray(*const EncodedInstruction, 8) = .{};

            for (mnemonic.variants.items) |*option| {
                if (option.operands.len != instr.arguments.len)
                    continue;
                alternatives.append(option) catch @panic("array too small");
            }
            if (alternatives.len == 0) {
                try ana.emit_error(instr.ast_node.location, "Could not find a matching instruction for {s}: No variant expects {} operands.", .{
                    instr.ast_node.mnemonic,
                    instr.arguments.len,
                });
                continue :current_instr;
            }

            logger.debug("{s} => args={}, vars={}, argc_matching={}", .{
                instr.ast_node.mnemonic,
                instr.arguments.len,
                mnemonic.variants.items.len,
                alternatives.len,
            });

            logger.debug("  args:", .{});
            for (instr.arguments) |arg| {
                logger.info("  - {s}: {s} aug={}", .{ @tagName(arg.value), @tagName(arg.flags.usage), arg.flags.augment });
            }
            logger.debug("  alts:", .{});

            var selection: ?*const EncodedInstruction = null;
            match_alternative: for (alternatives.constSlice()) |alt| {
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

                    try ana.emit_error(instr.ast_node.location, "Ambigious instruction selection for {s}", .{
                        alt.mnemonic,
                    });
                    continue :current_instr;
                }
                selection = alt;
            }

            if (selection == null) {
                try ana.emit_error(instr.ast_node.location, "Ambigious instruction selection for {s}", .{
                    instr.ast_node.mnemonic,
                });
                continue :current_instr;
            }
            instr.instruction = selection;
        }
    }

    fn evaluate_asserts(ana: *Analyzer) !void {
        for (ana.instructions) |*instr| {
            if (instr.mnemonic.?.* != .assert)
                continue;

            const with_message = switch (instr.arguments.len) {
                0 => {
                    try ana.emit_error(instr.ast_node.location, ".assert requires at least a single operand", .{});
                    continue;
                },
                1 => false,
                2 => true,
                else => blk: {
                    try ana.emit_error(instr.ast_node.location, ".assert can have up to 2 operands, but found {}", .{instr.arguments.len});
                    break :blk true;
                },
            };

            std.debug.assert(instr.arguments.len >= 1);

            const condition = instr.arguments[0];
            if (condition.value != .int) {
                try ana.emit_error(instr.ast_node.location, ".assert condition must be an integer value, but found {s}", .{@tagName(condition.value)});
                continue;
            }
            if (condition.value.int != 0) {
                // TODO: Think about emitting a warning if not 1/TRUE is yielded.
                continue;
            }

            const message: Value = if (with_message)
                instr.arguments[1]
            else
                .string("expression returned 0");

            if (message.value != .string) {
                try ana.emit_error(instr.ast_node.location, ".assert message must be a string value, but found {s}", .{@tagName(condition.value)});
                continue;
            }

            try ana.emit_error(
                instr.ast_node.location,
                "assertion failed: {s}",
                .{message.value.string},
            );
        }
    }

    fn emit_code(ana: *Analyzer, segment_allocator: std.mem.Allocator) ![]Segment {
        var segments: std.ArrayListUnmanaged(Segment) = .empty;
        errdefer segments.deinit(segment_allocator);

        var current_segment: SegmentBuilder = .init(0, .cog, segment_allocator);
        defer current_segment.data.deinit();

        seq_loop: for (ana.file.sequence, 0..) |seq, i| {
            const instr: *InstructionInfo = switch (seq) {
                .constant, .empty => continue :seq_loop,

                .label => |lbl| {
                    // just assert we're not doing stupid things:
                    const sym = ana.get_symbol_info(lbl.identifier) catch unreachable;
                    std.debug.assert(current_segment.hub_offset + current_segment.data.items.len == sym.offset.?.hub);
                    continue :seq_loop;
                },

                .instruction => &ana.instructions[ana.seq_to_instr_lut[i].?],
            };

            const mnemonic: Mnemonic = instr.mnemonic.?.*;

            logger.debug("emit {s}", .{@tagName(mnemonic)});

            switch (mnemonic) {
                .assert, .@"align" => {},

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
                            current_segment.exec_mode,
                            container_value,
                            T,
                        );
                        try current_segment.writer().writeInt(T, value, .little);
                    }
                },

                .cogexec => {
                    // TODO
                },

                .lutexec => {
                    // TODO
                },

                .hubexec => {
                    // TODO
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
                            try ana.emit_error(instr.ast_node.location, "{s} canont use the effect operator :{s}", .{
                                encoded.mnemonic,
                                @tagName(effect),
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
                            try ana.emit_error(instr.ast_node.location, "{s} cannot be used without effect operator", .{encoded.mnemonic});
                            continue;
                        }
                    }

                    var pc_delta: u32 = 1;
                    for (instr.arguments) |value| {
                        if (value.flags.augment)
                            pc_delta += 1;
                    }

                    const hub_pc: u32 = @intCast(current_segment.hub_offset + current_segment.data.items.len + 4 * pc_delta);
                    const cog_pc: u32 = @intCast(current_segment.data.items.len / 4 + pc_delta);

                    for (instr.arguments, encoded.operands, instr.ast_node.arguments) |value, operand, ast_node| {
                        if (value.flags.augment) {
                            @panic("TODO: Implement AUGS, AUGD");
                        }

                        const location = ast_node.location();

                        var fill_extra_slot: ?EncodedInstruction.Slot = null;

                        const slot_value: u32 = if (operand.type == .enumeration) blk: {
                            // this is the most special case here:
                            // TODO: enumerations take a "string" value (actually they need a enum literal!)

                            if (value.value != .enumerator) {
                                try ana.emit_error(location, "expected enumeration value, found {s}", .{@tagName(value.value)});
                                continue;
                            }

                            const lut: std.StaticStringMap(u32) = operand.type.enumeration;

                            const key = value.value.enumerator;

                            if (lut.get(key)) |index| {
                                break :blk index;
                            }

                            try ana.emit_error(location, "#{s} is not a valid enumerator", .{
                                key,
                            });
                            continue;
                        } else blk: {
                            const hint = value.flags.usage;

                            const int: u32 = try ana.cast_value_to(location, current_segment.exec_mode, value, u32);

                            const enc: u32 = switch (operand.type) {
                                .address => enc: {
                                    fill_extra_slot = null; // TODO: Handle relative/absolute addressing
                                    break :enc int;
                                },

                                .immediate => |shift| switch (hint) {
                                    .literal => (int >> shift),
                                    .register => {
                                        try ana.emit_error(location, "expected register value, but found immediate", .{});
                                        continue;
                                    },
                                },

                                .register => switch (hint) {
                                    .register => int,
                                    .literal => {
                                        try ana.emit_error(location, "expected immediate value, but found register", .{});
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
                                        const delta33: i33 = switch (current_segment.exec_mode) {
                                            .cog, .lut => @as(i33, int) - @as(i33, cog_pc),
                                            .hub => @as(i33, int) - @as(i33, hub_pc),
                                        };

                                        logger.debug("pcrel: {} {} {} {} => {}", .{ current_segment.exec_mode, cog_pc, hub_pc, int, delta33 });
                                        const delta9: i9 = @intCast(delta33);

                                        const udelta9: u9 = @bitCast(delta9);

                                        break :enc udelta9;
                                    } else {
                                        break :enc int;
                                    }
                                },

                                .pointer_expr => enc: {

                                    // TODO: Implement pointer operation
                                    if (int > 255) {
                                        try ana.emit_error(location, "pointer expression immediate must be in range 0 to 255, but found {}", .{int});
                                        continue;
                                    }

                                    break :enc int;
                                },

                                .pointer_reg => switch (hint) {
                                    .register => switch (int) {
                                        0x1F6, 0x1F7, 0x1F8, 0x1F9 => int - 0x1F6,

                                        else => {
                                            try ana.emit_error(location, "expected PA, PB, PTRA or PTRB, but found register {}", .{int});
                                            continue;
                                        },
                                    },

                                    .literal => {
                                        try ana.emit_error(location, "expected register value, but found immediate", .{});
                                        continue;
                                    },
                                },

                                .enumeration => unreachable,
                            };

                            const max_value = operand.slot.max_value();
                            if (enc > max_value) {
                                try ana.emit_error(location, "operand value out of range. max. allowed value is {}, but got {}", .{
                                    max_value,
                                    enc,
                                });
                                continue;
                            }

                            break :blk enc;
                        };

                        operand.slot.write(&output, slot_value) catch |err| switch (err) {
                            error.Overflow => try ana.emit_error(location, "cannot write operand: integer overflow", .{}),
                        };

                        if (fill_extra_slot) |slot| {
                            slot.fill(&output);
                        }
                    }

                    try current_segment.writer().writeInt(u32, output, .little);
                },
            }
        }

        if (current_segment.data.items.len > 0) {
            try segments.append(segment_allocator, .{
                .hub_offset = current_segment.hub_offset,
                .data = try current_segment.data.toOwnedSlice(),
            });
        }

        return segments.toOwnedSlice(segment_allocator);
    }

    fn cast_value_to(ana: *Analyzer, location: ast.Location, exec_mode: eval.ExecMode, value: Value, comptime U: type) !U {
        const I = std.meta.Int(.signed, @bitSizeOf(U));

        const raw_value: i64 = switch (value.value) {
            .int => |int| int,
            .offset => |offset| try ana.get_offset_for_exec_mode(offset, exec_mode),
            .string => @panic("string emission not supported yet"),
            .enumerator => @panic("BUG: enumerators must be handled before this!"),
            .register => |reg| @intFromEnum(reg),

            .pointer_expr => |ptr_expr| try ana.encode_ptr_expr(location, ptr_expr),
        };

        if (raw_value < 0) {
            const cast_val: I = @truncate(raw_value);
            if (cast_val != raw_value) {
                try ana.emit_warning(
                    location,
                    "Integer was truncated to {} bits. Expected: {}, Emitted: {}",
                    .{ @bitSizeOf(U), raw_value, cast_val },
                );
            }
            return @bitCast(cast_val);
        } else {
            const cast_val: U = @truncate(@as(u64, @bitCast(raw_value)));
            if (cast_val != raw_value) {
                try ana.emit_warning(
                    location,
                    "Integer was truncated to {} bits. Expected: {}, Emitted: {}",
                    .{ @bitSizeOf(U), raw_value, cast_val },
                );
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
                    try ana.emit_error(location, "Pointer expression index out of range. Expected value between {} and {}, but found {}", .{
                        std.math.minInt(i6),
                        std.math.maxInt(i6),
                        index,
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
                    try ana.emit_error(location, "Pointer expression index out of range. Expected value between {} and {}, but found {}", .{
                        min,
                        max,
                        index,
                    });
                    break :blk 0;
                }

                // wrap 16 to 0
                const encoded: u4 = @intCast(index & 0xFFFF);

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

    fn get_offset_for_exec_mode(ana: *Analyzer, offset: Offset, mode: eval.ExecMode) !u32 {
        const target_mode: eval.ExecMode = offset.local;
        if (target_mode != mode) {
            if (mode != .hub) {
                // TODO: IMPORTANT: WE HAVE TO ENCODE THE SEGMENT ID into Offset
                // SO WE DON'T JUMP BETWEEN DIFFERENT SEGMENTS!
                try ana.emit_error(null, "cannot refer to offset from different execution mode!", .{});
            } else {
                try ana.emit_warning(null, "jumping from hubexec mode into code that was defined in {s}exec mode. This is potentially unwanted behaviour!", .{
                    @tagName(target_mode),
                });
            }
        }

        return offset.get_local();
    }

    const EvalError = error{
        OutOfMemory,
        UndefinedSymbol,
        InvalidFunctionCall,
        Overflow,
        DivideByZero,
    };

    fn evaluate_root_expr(ana: *Analyzer, expr: ast.Expression) EvalError!eval.Value {
        return ana.evaluate_expr(expr, 0);
    }

    fn evaluate_expr(ana: *Analyzer, expr: ast.Expression, nesting: usize) EvalError!eval.Value {
        switch (expr) {
            .wrapped => |inner| return try ana.evaluate_expr(inner.*, nesting + 1),
            .integer => |int| return .int(int.value),
            .string => |string| return .string(string.value),
            .symbol => |symref| {
                const sym = ana.get_symbol_info(symref.symbol_name) catch unreachable;

                return switch (sym.type) {
                    .undefined => return error.UndefinedSymbol,
                    .code => .offset(sym.offset orelse return error.UndefinedSymbol, .literal),
                    .data => .offset(sym.offset orelse return error.UndefinedSymbol, .register),
                    .constant => sym.value orelse return error.UndefinedSymbol,
                    .builtin => sym.value.?,
                };
            },

            .unary_transform => |op| {
                const value = try ana.evaluate_expr(op.value.*, nesting + 1);

                switch (op.operator) {
                    .post_decrement,
                    .post_increment,
                    .pre_decrement,
                    .pre_increment,
                    => {
                        const op_name = switch (op.operator) {
                            .pre_decrement, .post_decrement => "--",
                            .pre_increment, .post_increment => "++",
                            else => unreachable,
                        };

                        var ptr_expr: eval.PointerExpression = switch (value.value) {
                            .register => |reg| try ana.ptr_expr_from_reg(op.location, reg),

                            .pointer_expr => |ptr_expr| ptr_expr,

                            else => blk: {
                                try ana.emit_error(op.location, "Type mismatch: Operator '{s}' cannot be applied to {s}s", .{
                                    op_name,
                                    @tagName(value.value),
                                });

                                break :blk .{
                                    .pointer = .PTRA,
                                    .increment = .none,
                                    .index = null,
                                };
                            },
                        };

                        if (ptr_expr.index != null) {
                            try ana.emit_error(op.location, "Cannot apply operator '{s}' to pointer expressions that already have an index set", .{
                                op_name,
                            });
                        } else if (ptr_expr.increment != .none) {
                            try ana.emit_error(op.location, "Cannot apply operator '{s}' to pointer expressions which already have an increment mode set ", .{
                                op_name,
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
                    try ana.emit_error(op.location, "Type mismatch: Operator '{s}' cannot be applied to registers", .{
                        @tagName(op.operator),
                    });
                    return value;
                }
                if (value.value == .enumerator) {
                    try ana.emit_error(op.location, "Type mismatch: Operator '{s}' cannot be applied to enumerators", .{
                        @tagName(op.operator),
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
                            try ana.emit_error(op.location, "Operator '!' cannot be applied to a value of type {s}", .{
                                @tagName(value.value),
                            });
                            return .int(0);
                        }
                        return .int(@intFromBool(value.value.int == 0));
                    },
                    .@"~" => {
                        if (value.value != .int) {
                            try ana.emit_error(op.location, "Operator '~' cannot be applied to a value of type {s}", .{
                                @tagName(value.value),
                            });
                            return .int(0);
                        }
                        return .int(~value.value.int);
                    },
                    .@"+" => {
                        if (value.value != .int) {
                            try ana.emit_error(op.location, "Operator '+' cannot be applied to a value of type {s}", .{
                                @tagName(value.value),
                            });
                            return .int(0);
                        }
                        return .int(-value.value.int);
                    },
                    .@"-" => {
                        if (value.value != .int) {
                            try ana.emit_error(op.location, "Operator '-' cannot be applied to a value of type {s}", .{
                                @tagName(value.value),
                            });
                            return .int(0);
                        }
                        return .int(-value.value.int);
                    },
                    .@"@" => {
                        @panic("TODO: Implement relative addressing");
                    },
                    .@"*" => {
                        if (value.value != .offset) {
                            try ana.emit_error(op.location, "Operator '*' cannot be applied to a value of type {s}", .{
                                @tagName(value.value),
                            });
                            return .offset(.init_hub(0), .register);
                        }
                        if (value.flags.usage == .register) {
                            try ana.emit_warning(op.location, "Operator '*' is applied to a data label and has no effect", .{});
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
                        if (value.value != .offset) {
                            try ana.emit_error(op.location, "Operator '&' cannot be applied to a value of type {s}", .{
                                @tagName(value.value),
                            });
                            return .offset(.init_hub(0), .literal);
                        }
                        if (value.flags.usage == .literal) {
                            try ana.emit_warning(op.location, "Operator '&' is applied to a code label and has no effect", .{});
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
                const lhs = try ana.evaluate_expr(op.lhs.*, nesting + 1);
                const rhs = try ana.evaluate_expr(op.rhs.*, nesting + 1);

                const lhs_type: Value.Type = lhs.value;
                const rhs_type: Value.Type = rhs.value;

                if (op.operator == .array_index) {
                    const lhs_ok = (lhs_type == .register or lhs_type == .pointer_expr);
                    const rhs_ok = (rhs_type == .int);

                    if (!lhs_ok or !rhs_ok) {
                        try ana.emit_error(op.location, "Type mismatch: Operator '[]' cannot be applied to {s} and {s}", .{
                            @tagName(lhs_type),
                            @tagName(rhs_type),
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
                        try ana.emit_error(op.location, "Operator '[]' cannot be applied to a pointer expression which already has an index set", .{});
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
                    try ana.emit_error(op.location, "Type mismatch: Operator '{s}' cannot be applied to {s} and {s}", .{
                        @tagName(op.operator),
                        @tagName(lhs_type),
                        @tagName(rhs_type),
                    });
                    return .int(0);
                }

                switch (lhs_type) {
                    .int => return .int(
                        try ana.execute_int_op(op.location, lhs.value.int, rhs.value.int, op.operator),
                    ),
                    .register => {
                        try ana.emit_error(op.location, "Type mismatch: Operator '{s}' cannot be applied to registers", .{
                            @tagName(op.operator),
                        });
                        return .register(0);
                    },
                    .enumerator => {
                        try ana.emit_error(op.location, "Type mismatch: Operator '{s}' cannot be applied to enumerators", .{
                            @tagName(op.operator),
                        });
                        return .enumerator("");
                    },
                    .pointer_expr => {
                        try ana.emit_error(op.location, "Type mismatch: Operator '{s}' cannot be applied to pointer expressions", .{
                            @tagName(op.operator),
                        });
                        return .enumerator("");
                    },
                    .offset => @panic("TODO: Implement binary operators on offsets."),
                    .string => @panic("TODO: Implement binary operators on strings."),
                }
            },
            .function_call => |fncall| {
                const func = ana.get_function(fncall.function).?;

                const params = func.get_parameters();

                var argv_buf: [16]eval.Value = undefined;
                const argv = try ana.map_function_args(fncall, params, &argv_buf, nesting);

                switch (func.*) {
                    .aug => {
                        std.debug.assert(argv.len == 1);
                        if (nesting != 0) {
                            try ana.emit_error(fncall.arguments[0].location, "aug() must be the root of an expression.", .{});
                        }
                        var value = argv[0];
                        value.flags.augment = true;
                        return value;
                    },

                    .nrel => {
                        std.debug.assert(argv.len == 1);
                        if (nesting != 0) {
                            try ana.emit_error(fncall.arguments[0].location, "aug() must be the root of an expression.", .{});
                        }
                        var value = argv[0];
                        value.flags.addressing = .absolute;
                        return value;
                    },

                    .cogaddr, .lutaddr, .localaddr => {
                        const loc = fncall.arguments[0].location;
                        std.debug.assert(argv.len == 1);
                        const value = argv[0];
                        switch (value.value) {
                            .string, .enumerator, .pointer_expr => {
                                try ana.emit_error(fncall.arguments[0].location, "{s}() cannot be applied to {s}s.", .{ @tagName(func.*), @tagName(value.value) });
                                return value;
                            },

                            .int => {
                                try ana.emit_warning(loc, "{s}() expected offset, but got {s}.", .{ @tagName(func.*), @tagName(value.value) });
                                return value;
                            },

                            .register => |reg| {
                                if (func.* == .lutaddr) {
                                    try ana.emit_error(loc, "lutaddr() cannot be applied to registers", .{});
                                    return .int(0);
                                } else if (func.* == .localaddr) {
                                    // TODO: Check if "execution mode" is .cogexec
                                    try ana.emit_error(loc, "localaddr() is only valid for registers in a cogexec scope", .{});
                                }

                                try ana.emit_warning(fncall.arguments[0].location, "{s}() expected offset, but got {s}.", .{ @tagName(func.*), @tagName(value.value) });
                                return .int(@intFromEnum(reg));
                            },

                            .offset => |offset| {
                                const maybe_expected_type: ?eval.ExecMode = switch (func.*) {
                                    .lutaddr => .lut,
                                    .cogaddr => .cog,
                                    .localaddr => null,
                                    else => unreachable,
                                };

                                if (maybe_expected_type) |expected| {
                                    if (offset.local != expected) {
                                        try ana.emit_error(fncall.arguments[0].location, "{s}() expected offset of type {s}, but got type {s}.", .{
                                            @tagName(func.*),
                                            @tagName(expected),
                                            @tagName(offset.local),
                                        });
                                        return .int(0);
                                    }
                                }

                                return .int(offset.get_local());
                            },
                        }
                    },

                    .hubaddr => {
                        std.debug.assert(argv.len == 1);
                        const value = argv[0];
                        switch (value.value) {
                            .int => {
                                try ana.emit_warning(fncall.arguments[0].location, "hubaddr() expected offset, but got {s}.", .{@tagName(value.value)});
                                return value;
                            },
                            .string, .register, .enumerator, .pointer_expr => {
                                try ana.emit_error(fncall.arguments[0].location, "hubaddr() cannot be applied to {s}s.", .{@tagName(value.value)});
                                return value;
                            },
                            .offset => |offset| return .int(offset.hub),
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
                    try ana.emit_error(location, "Only registers PTRA ({}) or PTRB ({}) can be used for pointer expressions, but not {}", .{
                        PTRA,
                        PTRB,
                        reg,
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

    fn map_function_args(
        ana: *Analyzer,
        fncall: ast.FunctionInvocation,
        params: []const Function.Parameter,
        argv_buffer: []eval.Value,
        nesting: usize,
    ) ![]const eval.Value {
        if (params.len > argv_buffer.len)
            @panic("BUG: argument storage buffer is too small");

        const first_kwarg_index: usize = for (fncall.arguments, 0..) |arg, i| {
            if (arg.name != null)
                break i;
        } else fncall.arguments.len;

        {
            var ok = true;
            for (fncall.arguments[first_kwarg_index..]) |arg| {
                if (arg.name == null) {
                    try ana.emit_error(arg.location, "positional arguments must not appear after a named argument", .{});
                    ok = false;
                }
            }

            if (fncall.arguments.len != params.len) {
                try ana.emit_error(fncall.location, "{s}() expects {} arguments, but found {}", .{
                    fncall.function,
                    params.len,
                    fncall.arguments.len,
                });
                ok = false;
            }

            if (!ok)
                return error.InvalidFunctionCall;
        }

        const argv = argv_buffer[0..fncall.arguments.len];

        const pos_argin = fncall.arguments[0..first_kwarg_index];
        const kw_argin = fncall.arguments[first_kwarg_index..];

        const pos_argv = argv[0..first_kwarg_index];
        const kw_argv = argv[first_kwarg_index..];

        const pos_params = params[0..first_kwarg_index];
        const kw_params = params[first_kwarg_index..];

        std.debug.assert(pos_argv.len == pos_params.len);
        std.debug.assert(kw_argv.len == kw_params.len);

        std.debug.assert(pos_argv.len == pos_argin.len);
        std.debug.assert(kw_argv.len == kw_argin.len);

        for (pos_argv, pos_argin) |*value, arg| {
            std.debug.assert(arg.name == null);
            value.* = try ana.evaluate_expr(arg.value, nesting + 1);
        }

        {
            var ok = true;
            for (kw_argin) |arg| {
                std.debug.assert(arg.name != null);

                const index = index_of_param(params, arg.name.?) orelse {
                    ok = false;
                    try ana.emit_error(arg.location, "{s}() has no parameter named {s}", .{
                        fncall.function,
                        arg.name.?,
                    });
                    continue;
                };
                if (index < first_kwarg_index) {
                    ok = false;
                    try ana.emit_error(arg.location, "Parameter {s} passed to {s}() was already given as a positional argument as {}th argument", .{
                        arg.name.?,
                        fncall.function,
                        index,
                    });
                    continue;
                }
            }
            for (kw_argin, 0..) |arg1, i| {
                for (kw_argin[i + 1 ..]) |arg2| {
                    if (std.mem.eql(u8, arg1.name.?, arg2.name.?)) {
                        ok = false;
                        try ana.emit_error(arg2.location, "Parameter {s} passed to {s}() was already given as a named argument here: {}", .{
                            arg1.name.?,
                            fncall.function,
                            arg1.location,
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

            kw_argv[index] = try ana.evaluate_expr(arg.value, nesting + 1);
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
    hub_offset: u32,
    exec_mode: eval.ExecMode,
    data: std.ArrayList(u8),

    fn init(hub_offset: u32, exec_mode: eval.ExecMode, allocator: std.mem.Allocator) SegmentBuilder {
        return .{
            .hub_offset = hub_offset,
            .exec_mode = exec_mode,
            .data = .init(allocator),
        };
    }

    fn seek_forward(sb: *SegmentBuilder, hub_offset: u32) !void {
        std.debug.assert(hub_offset >= sb.hub_offset);
        try sb.writer().writeByteNTimes(sb.hub_offset < hub_offset);
    }

    fn writer(sb: *SegmentBuilder) Writer {
        return .{ .context = sb };
    }

    const Error = error{OutOfMemory};
    const Writer = std.io.Writer(*SegmentBuilder, Error, append);

    fn append(sb: *SegmentBuilder, buffer: []const u8) Error!usize {
        try sb.data.appendSlice(buffer);
        return buffer.len;
    }
};

const Cursor = struct {
    offset: Offset = .init_cog(0, 0),

    fn change_mode(cursor: *Cursor, mode: eval.ExecMode) void {
        cursor.offset = switch (mode) {
            .hub => cursor.offset,
            .cog => .init_cog(cursor.offset.hub, 0),
            .lut => .init_lut(cursor.offset.hub, 0),
        };
    }

    fn advance_code(cursor: *Cursor) void {
        // instructions auto-align to 4 in non-hubexec mode:
        if (cursor.offset.local != .hub) {
            cursor.alignas(4);
        }
        cursor.advance_data(.long);
    }

    fn advance_data(cursor: *Cursor, size: enum(u4) { byte = 1, word = 2, long = 4 }) void {
        cursor.offset.hub += @intFromEnum(size);

        const is_instr_aligned = std.mem.isAligned(cursor.offset.hub, 4);
        switch (cursor.offset.local) {
            .cog, .lut => |*val| if (is_instr_aligned) {
                val.* += 1;
            },
            .hub => {},
        }
    }

    fn alignas(cursor: *Cursor, alignment: u32) void {
        const prev = cursor.offset.hub;
        const next = std.mem.alignForward(u32, cursor.offset.hub, alignment);

        cursor.offset.hub = next;

        switch (cursor.offset.local) {
            .cog, .lut => |*local| {
                const cogprev = prev / 4;
                const cognext = next / 4;
                // logger.err("{} {} {} {}", .{ prev, next, cogprev, cognext });

                local.* += @intCast(cognext - cogprev);
            },
            .hub => {},
        }
    }
};

test Cursor {
    var cursor: Cursor = .{};

    cursor.advance_data(.long);
    try std.testing.expectEqual(Offset.init_cog(4, 1), cursor.offset);

    cursor.advance_data(.long);
    try std.testing.expectEqual(Offset.init_cog(8, 2), cursor.offset);

    cursor.advance_data(.word);
    try std.testing.expectEqual(Offset.init_cog(10, 2), cursor.offset);

    cursor.advance_data(.word);
    try std.testing.expectEqual(Offset.init_cog(12, 3), cursor.offset);

    cursor.advance_data(.byte);
    try std.testing.expectEqual(Offset.init_cog(13, 3), cursor.offset);

    cursor.advance_data(.byte);
    try std.testing.expectEqual(Offset.init_cog(14, 3), cursor.offset);

    cursor.advance_data(.byte);
    try std.testing.expectEqual(Offset.init_cog(15, 3), cursor.offset);

    cursor.advance_data(.byte);
    try std.testing.expectEqual(Offset.init_cog(16, 4), cursor.offset);

    cursor.advance_code();
    try std.testing.expectEqual(Offset.init_cog(20, 5), cursor.offset);

    cursor.advance_data(.byte);
    try std.testing.expectEqual(Offset.init_cog(21, 5), cursor.offset);

    cursor.advance_code();
    try std.testing.expectEqual(Offset.init_cog(28, 7), cursor.offset);

    cursor.advance_data(.byte);
    try std.testing.expectEqual(Offset.init_cog(29, 7), cursor.offset);

    cursor.alignas(2);
    try std.testing.expectEqual(Offset.init_cog(30, 7), cursor.offset);

    cursor.alignas(2);
    try std.testing.expectEqual(Offset.init_cog(30, 7), cursor.offset);

    cursor.alignas(4);
    try std.testing.expectEqual(Offset.init_cog(32, 8), cursor.offset);
}

const SymbolInfo = struct {
    name: []const u8,

    type: Type = .undefined,
    referenced: bool = false,

    offset: ?Offset = null,
    value: ?Value = null,

    pub fn location(sym: SymbolInfo) ?ast.Location {
        return switch (sym.type) {
            .code, .data, .constant => |loc| loc,
            .undefined, .builtin => null,
        };
    }

    pub const Type = union(enum) {
        undefined,
        code: ast.Location,
        data: ast.Location,
        constant: ast.Location,
        builtin,
    };
};

const InstructionInfo = struct {
    /// Pointer into the 'file.sequence'
    seq_index: usize,
    ast_node: *const ast.Instruction,

    mnemonic: ?*const Mnemonic = null,
    instruction: ?*const EncodedInstruction = null,

    offset: ?Offset = null,

    /// Size of the instruction slot in bytes
    byte_size: ?u32 = null,

    arguments: []const eval.Value = &.{},
};

const Function = union(enum) {
    // the builtin functions are hardcoded here:
    aug, // adds auto-augmentation to the argument
    nrel, // computes the absolute address

    hubaddr,
    cogaddr,
    lutaddr,
    localaddr,

    // stdlib functions are defined as "generic" ones:
    // TODO: generic: GenericFunction,

    pub fn get_parameters(func: Function) []const Parameter {
        return switch (func) {
            .aug => &.{.init("value", .int)},
            .nrel => &.{.init("addr", .int)},
            .hubaddr => &.{.init("addr", .offset)},
            .cogaddr => &.{.init("addr", .offset)},
            .lutaddr => &.{.init("addr", .offset)},
            .localaddr => &.{.init("addr", .offset)},
        };
    }

    pub const Parameter = struct {
        name: []const u8,
        type: Type,

        pub fn init(name: []const u8, ptype: Type) Parameter {
            return .{ .name = name, .type = ptype };
        }

        pub const Type = enum {
            int,
            offset,
            string,
            @"enum",
        };
    };
};

const Mnemonic = union(enum) {

    // data encoded
    long,
    word,
    byte,

    // directives:

    cogexec,
    lutexec,
    hubexec,
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
            address: Slot,

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
            pointer_expr,

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
                    .pointer_expr => ((vtype == .int) and (usage == .literal)) or ((vtype == .register) and switch (value.value.register) {
                        PTRA, PTRB => true,
                        else => false,
                    }),

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
