const std = @import("std");

const stdlib = @import("stdlib/stdlib.zig");
const eval = @import("stdlib/eval.zig");
const ast = @import("frontend.zig").ast;

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

pub fn analyze(allocator: std.mem.Allocator, file: ast.File, options: AnalyzeOptions) !Module {
    var analyzer: Analyzer = try .init(allocator, file, options);
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

    var symbols: std.ArrayList(Module.Symbol) = .init(output_arena.allocator());
    defer symbols.deinit();

    for (analyzer.symbols.keys(), analyzer.symbols.values()) |name, sym| {
        const stype: Module.Symbol.Type = switch (sym.type) {
            .undefined => continue,
            .code => .code,
            .data => .data,
            .constant => continue,
            .builtin => continue,
        };

        try symbols.append(.{
            .name = try output_arena.allocator().dupe(u8, name),
            .label = sym.offset.?,
            .type = stype,
        });
    }

    return .{
        .arena = output_arena,
        .segments = segments,
        .line_data = try analyzer.line_data.toOwnedSlice(output_arena.allocator()),
        .symbols = try symbols.toOwnedSlice(),
    };
}

fn dump_analyzer(analyzer: *Analyzer) void {
    logger.info("symbols:", .{});
    for (analyzer.symbols.values()) |sym| {
        if (sym.type == .builtin)
            continue;
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
            instr.start_addr.?,
            instr.ast_node.mnemonic,
            instr.byte_size.?,
        });
    }

    logger.info("map file:", .{});
    for (analyzer.instructions) |instr| {
        logger.info("  {}: {s}", .{
            instr.start_addr.?,
            instr.ast_node.mnemonic,
        });
        for (instr.arguments, 0..) |arg, i| {
            logger.info("    [{}] = {}", .{ i, arg });
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

    symbols: std.StringArrayHashMapUnmanaged(SymbolInfo) = .empty,

    functions: std.StringArrayHashMapUnmanaged(Function) = .empty,
    mnemonics: CaseInsensitiveStringHashMap(Mnemonic) = .empty,

    seq_to_instr_lut: []const ?usize = &.{},
    instructions: []InstructionInfo = &.{},
    line_data: std.ArrayListUnmanaged(Module.LineData) = .empty,

    ok: bool = true,

    fn init(allocator: std.mem.Allocator, file: ast.File, options: AnalyzeOptions) !Analyzer {
        var ana: Analyzer = .{
            .allocator = allocator,
            .arena = .init(allocator),
            .file = file,
            .options = options,
        };
        try ana.functions.ensureUnusedCapacity(allocator, 6);

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
            error.DiagnosedFailure => {
                // This failure is a silent failure and assumes we already have emitted at least a single diagnostic mesasge
                return;
            },
            error.OutOfMemory => "out of memory",
            error.UndefinedSymbol => "referenced undefined symbol",
            error.InvalidFunctionCall => "invalid function call",
            error.Overflow => "integer overflow",
            error.DivideByZero => "division by zero",
            error.InvalidArg => "invalid argument",
            error.TypeMismatch => "type mismatch",
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

    fn load_functions(ana: *Analyzer, functions: std.StaticStringMap(UserFunction)) !void {
        for (functions.keys(), functions.values()) |name, func| {
            const gop = try ana.functions.getOrPut(ana.allocator, name);
            if (gop.found_existing) {
                try ana.emit_error(null, "duplicate function symbol: {s}", .{name});
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

        var idgen: Segment_ID_Gen = .{};
        var cursor: Cursor = .init(idgen.next());

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

                    coded.start_addr = cursor.offset;
                    defer coded.end_addr = cursor.offset;

                    switch (coded.mnemonic.?.*) {
                        .assert => {},

                        .hubexec, .lutexec, .cogexec => {
                            const mode: eval.ExecMode = switch (coded.mnemonic.?.*) {
                                .hubexec => .hub,
                                .lutexec => .lut,
                                .cogexec => .cog,
                                else => unreachable,
                            };

                            var hub_offset = cursor.offset.hub_address;

                            switch (instr.arguments.len) {
                                0 => {}, // ok, just change the mode
                                1 => blk: { // ok, requires a new hub offset

                                    const value = ana.evaluate_root_expr(instr.arguments[0], null) catch |err| {
                                        try ana.emit_error(instr.location, "Failed to evaluate argument for .{s}exec: {s}", .{
                                            @tagName(mode), @errorName(err),
                                        });
                                        break :blk;
                                    };

                                    const new_offset_or_err = ana.cast_value_to(instr.location, .hub, value, u20);
                                    if (new_offset_or_err) |new_offset| {
                                        if (new_offset < hub_offset) {
                                            try ana.emit_warning(instr.location, "New hub offset 0x{X:0>6} is smaller than previous offset 0x{X:0>6}.", .{
                                                new_offset,
                                                hub_offset,
                                            });
                                        }

                                        // TODO: Reinclude the overlap check!
                                        // for (segments.items) |segment| {
                                        //     if (new_offset >= segment.hub_offset and new_offset < segment.hub_offset + segment.data.len) {
                                        //         try ana.emit_error(instr.ast_node.location, " New segment at 0x{X:0>6} overlaps with previous segment {} at 0x{X:0>6}...0x{X:0>6}", .{
                                        //             new_offset,
                                        //             @intFromEnum(segment.id),
                                        //             segment.hub_offset,
                                        //             segment.hub_offset + segment.data.len - 1,
                                        //         });
                                        //     }
                                        // }

                                        hub_offset = new_offset;
                                    } else |err| {
                                        try ana.emit_error(instr.location, ".{s}exec expects a numeric operand, but got {}. Could not cast to number: {s}", .{
                                            @tagName(mode),
                                            instr.arguments[0],
                                            @errorName(err),
                                        });
                                    }
                                },
                                else => {
                                    try ana.emit_error(instr.location, ".{s}exec expects zero or one operand, but got {} operands.", .{
                                        @tagName(mode),
                                        instr.arguments.len,
                                    });
                                },
                            }

                            cursor.change_mode(idgen.next(), mode, hub_offset);

                            // We must change the start address here as we're changing the cursor mode here.
                            coded.start_addr = cursor.offset;
                        },

                        .@"align" => blk: {
                            if (coded.ast_node.arguments.len != 1) {
                                try ana.emit_error(coded.ast_node.location, ".align requires exactly one argument, but found {}", .{coded.ast_node.arguments.len});
                            }
                            if (coded.ast_node.arguments.len < 1) {
                                break :blk;
                            }

                            if (ana.evaluate_root_expr(coded.ast_node.arguments[0], null)) |value| {
                                switch (value.value) {
                                    .int => |int| {
                                        if (std.math.cast(u20, int)) |alignment| {
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
                    continue;
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

            const value = ana.evaluate_root_expr(con.value, null) catch |err| {
                try ana.emit_eval_error(con.location, "", err);
                continue;
            };

            switch (value.value) {
                .int, .string, .enumerator => {},

                .register => {
                    // TODO: Consider if this is OK or not. It's kinda handy, but not sure if hazardous
                },

                .address => {
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
            std.debug.assert(instr.start_addr != null);
            std.debug.assert(instr.end_addr != null);

            const args = try ana.arena.allocator().alloc(eval.Value, instr.ast_node.arguments.len);
            for (args, instr.ast_node.arguments) |*value, expr| {
                value.* = ana.evaluate_root_expr(expr, instr.end_addr.?) catch |err| {
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
            std.debug.assert(instr.start_addr != null);
            std.debug.assert(instr.end_addr != null);
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

            instr.instruction = selection orelse {
                try ana.emit_error(instr.ast_node.location, "Ambigious instruction selection for {s}", .{
                    instr.ast_node.mnemonic,
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

            var message: []const u8 = "expression returned 0";

            if (with_message) {
                const msg = instr.arguments[1];

                if (msg.value != .string) {
                    try ana.emit_error(instr.ast_node.location, ".assert message must be a string value, but found {s}", .{@tagName(condition.value)});
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

                        message = try std.fmt.allocPrint(ana.arena.allocator(), "{nice} {s} {nice}!", .{ lhs, relation, rhs });
                    }
                }
            }

            try ana.emit_error(instr.ast_node.location, "assertion failed: {s}", .{message});
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
            const segment_end_hub_offset: u32 = @intCast(current_segment.hub_offset + current_segment.data.items.len);

            const instr: *InstructionInfo = switch (seq) {
                .constant, .empty => continue :seq_loop,

                .label => |lbl| {

                    // just assert we're not doing stupid things:
                    const sym = ana.get_symbol_info(lbl.identifier) catch unreachable;
                    std.debug.assert(segment_end_hub_offset == sym.offset.?.hub_address);

                    try ana.line_data.append(segment_allocator, .{
                        .offset = segment_end_hub_offset,
                        .length = 0,
                        .location = lbl.location,
                    });

                    continue :seq_loop;
                },

                .instruction => &ana.instructions[ana.seq_to_instr_lut[i].?],
            };

            const mnemonic: Mnemonic = instr.mnemonic.?.*;

            const hub_offset = instr.start_addr.?.hub_address;
            std.debug.assert(hub_offset >= segment_end_hub_offset);

            switch (mnemonic) {
                .assert, .@"align" => continue :seq_loop,

                .cogexec, .lutexec, .hubexec => {
                    const new_mode: eval.ExecMode = switch (mnemonic) {
                        .cogexec => .cog,
                        .lutexec => .lut,
                        .hubexec => .hub,
                        else => unreachable,
                    };

                    if (current_segment.data.items.len > 0) {
                        try segments.append(segment_allocator, .{
                            .id = current_segment.id,
                            .hub_offset = current_segment.hub_offset,
                            .data = try current_segment.data.toOwnedSlice(),
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

            if (hub_offset > segment_end_hub_offset) {
                // insert alignment padding
                try current_segment.writer().writeByteNTimes(0xFF, hub_offset - segment_end_hub_offset);
            }

            const line_info = try ana.line_data.addOne(segment_allocator);
            line_info.* = .{
                .offset = hub_offset,
                .length = 0,
                .location = instr.ast_node.location,
            };
            defer line_info.length = @intCast((current_segment.hub_offset + current_segment.data.items.len) - hub_offset);

            logger.debug("emit {s}", .{@tagName(mnemonic)});

            switch (mnemonic) {
                .assert,
                .@"align",
                .cogexec,
                .lutexec,
                .hubexec,
                => unreachable,

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
                                                std.log.debug("#{{/}}A auto mode translation: segment is #{}, mode is {}, target is {}", .{
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
                                                    std.log.debug("#{{/}}A: src mode={} address={} address:int=0x{X:0>6} cog={} lut={} hub={}", .{
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
                                            .address => |addr| addr.hub_address,
                                            else => int,
                                        };

                                        const byte_delta_i33: i33 = @as(i33, target_address) - @as(i33, hub_pc);

                                        const byte_delta_i20: i20 = std.math.cast(i20, byte_delta_i33) orelse delta: {
                                            try ana.emit_error(location, "branch too far. Cannot jump by {} bytes", .{byte_delta_i33});
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

                            const enc: u32 = if (value.flags.augment) aug: {
                                const aug_dst: *?u23 = if (operand.slot.eql(.S))
                                    &aug.s
                                else if (operand.slot.eql(.D))
                                    &aug.d
                                else {
                                    try ana.emit_error(location, "Cannot aug() operand", .{});
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

        if (current_segment.data.items.len > 0) {
            try segments.append(segment_allocator, .{
                .id = sid.next(),
                .hub_offset = current_segment.hub_offset,
                .data = try current_segment.data.toOwnedSlice(),
                .exec_mode = current_segment.exec_mode,
            });
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
        };

        logger.info("pcrel: cog={} hub={} target={}:{s} => rel {}", .{ cog_pc, hub_pc, int, @tagName(exec_mode), delta33 });

        switch (mode) {
            .default => {
                const delta9: i9 = std.math.cast(i9, delta33) orelse {
                    try ana.emit_error(loc, "branch too far. Cannot jump by {} instructions", .{delta33});
                    return 0;
                };

                const udelta9: u9 = @bitCast(delta9);

                return udelta9;
            },
            .augmented => {
                // somehow the augmented delta only uses 18 bits of PC ?

                const delta20: i20 = std.math.cast(i20, delta33) orelse {
                    try ana.emit_error(loc, "branch too far. Cannot jump by {} instructions", .{delta33});
                    return 0;
                };

                const udelta20: u20 = @bitCast(delta20);

                return udelta20;
            },
        }
    }

    fn cast_value_to(ana: *Analyzer, location: ast.Location, exec_mode: eval.ExecMode, value: Value, comptime U: type) !U {
        const cast = try ana.cast_value_to_2(location, exec_mode, value, U);
        logger.debug("cast {} to {}", .{ value, cast });
        return cast;
    }

    fn cast_value_to_2(ana: *Analyzer, location: ast.Location, exec_mode: eval.ExecMode, value: Value, comptime U: type) !U {
        const I = std.meta.Int(.signed, @bitSizeOf(U));

        const raw_value: i64 = switch (value.value) {
            .int => |int| int,
            .address => |offset| try ana.get_offset_for_exec_mode(location, offset, exec_mode),
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

    fn get_offset_for_exec_mode(ana: *Analyzer, location: ast.Location, offset: TaggedAddress, mode: eval.ExecMode) !u32 {
        const target_mode: eval.ExecMode = offset.local;
        if (target_mode != mode) {
            if (mode == .hub or target_mode == .hub) {
                try ana.emit_warning(location, "jumping from {s}exec mode into code that was defined in {s}exec mode. This is potentially unwanted behaviour!", .{
                    @tagName(mode),
                    @tagName(target_mode),
                });
            } else {
                // TODO: IMPORTANT: WE HAVE TO ENCODE THE SEGMENT ID into Offset
                // SO WE DON'T JUMP BETWEEN DIFFERENT SEGMENTS!
                try ana.emit_error(location, "cannot perform jump from {s}exec mode into {s}exec mode.", .{
                    @tagName(mode),
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
                const sym = ana.get_symbol_info(symref.symbol_name) catch unreachable;

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

                        if (ptr_expr.index != null and ptr_expr.increment != .none) {
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
                        if (value.value != .address) {
                            try ana.emit_error(op.location, "Operator '@' cannot be applied to a value of type {s}", .{
                                @tagName(value.value),
                            });
                            return .int(0);
                        }

                        const local_offset: TaggedAddress = maybe_current_address orelse {
                            try ana.emit_error(op.location, "Operator '@' cannot be used in this scope", .{});
                            return .int(0);
                        };
                        const target_offset: TaggedAddress = value.value.address;

                        // TODO: Validate local_offset and target_offset point into the same segment

                        const local_hub_addr: u32 = local_offset.hub_address;
                        const target_hub_addr: u32 = target_offset.hub_address;

                        const jmp_delta = @as(i33, target_hub_addr) - @as(i33, local_hub_addr);

                        if (@mod(jmp_delta, 4) != 0) {
                            try ana.emit_error(op.location, "'@' cannot be applied to a offset range which is non-divisible by 4", .{});
                        }

                        return .int(@divTrunc(jmp_delta, 4));
                    },
                    .@"*" => {
                        if (value.value != .address) {
                            try ana.emit_error(op.location, "Operator '*' cannot be applied to a value of type {s}", .{
                                @tagName(value.value),
                            });
                            return .address(if (maybe_current_address) |addr|
                                addr
                            else
                                .init_hub(undefined, 0), .literal);
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
                        if (value.value != .address) {
                            try ana.emit_error(op.location, "Operator '&' cannot be applied to a value of type {s}", .{
                                @tagName(value.value),
                            });
                            return .address(.init_hub(undefined, 0), .literal);
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
                const lhs = try ana.evaluate_expr(op.lhs.*, maybe_current_address, nesting + 1);
                const rhs = try ana.evaluate_expr(op.rhs.*, maybe_current_address, nesting + 1);

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
                            try ana.emit_error(fncall.arguments[0].location, "aug() must be the root of an expression.", .{});
                        }
                        var value = argv[0];
                        value.flags.augment = true;
                        return value;
                    },

                    .nrel => {
                        std.debug.assert(argv.len == 1);
                        if (nesting != 0) {
                            try ana.emit_error(fncall.arguments[0].location, "nrel() must be the root of an expression.", .{});
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

                            .address => |offset| {
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
                            .address => |address| return .int(address.hub_address),
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

    const max_supported_parameters = 16;

    fn map_function_args(
        ana: *Analyzer,
        fncall: ast.FunctionInvocation,
        params: []const Function.Parameter,
        maybe_current_address: ?TaggedAddress,
        nesting: usize,
    ) !std.BoundedArray(Value, max_supported_parameters) {
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
                    try ana.emit_error(arg.location, "positional arguments must not appear after a named argument", .{});
                    ok = false;
                }
            }

            if (fncall.arguments.len < params.len - default_arg_count or fncall.arguments.len > params.len) {
                try ana.emit_error(fncall.location, "{s}() expects {}..{} arguments, but found {}", .{
                    fncall.function,
                    params.len - default_arg_count,
                    params.len,
                    fncall.arguments.len,
                });
                ok = false;
            }

            if (!ok)
                return error.InvalidFunctionCall;
        }

        var argv: std.BoundedArray(Value, max_supported_parameters) = .{};
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
            kw_argv[index] = try ana.evaluate_expr(arg.value, maybe_current_address, nesting + 1);
            argv_ok.set(index);
        }

        {
            var all_ok = true;
            for (params, 0..) |param, index| {
                if (!argv_ok.isSet(index)) {
                    // This error can only happen for non-defaulted parameters
                    std.debug.assert(param.default_value == null);
                    try ana.emit_error(fncall.location, "Missing parameter {s} for function {s}()", .{
                        param.name,
                        fncall.function,
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
    hub_offset: u20,
    exec_mode: eval.ExecMode,
    data: std.ArrayList(u8),

    fn init(id: Segment_ID, hub_offset: u20, exec_mode: eval.ExecMode, allocator: std.mem.Allocator) SegmentBuilder {
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

    fn seek_forward(sb: *SegmentBuilder, hub_offset: u20) !void {
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

    fn init(segment: Segment_ID) Cursor {
        return .{
            .offset = .init_cog(segment, 0, 0),
        };
    }

    fn change_mode(cursor: *Cursor, seg: Segment_ID, mode: eval.ExecMode, hub_offset: ?u20) void {
        cursor.offset = switch (mode) {
            .hub => .init_hub(seg, hub_offset orelse cursor.offset.hub_address),
            .cog => .init_cog(seg, hub_offset orelse cursor.offset.hub_address, 0),
            .lut => .init_lut(seg, hub_offset orelse cursor.offset.hub_address, 0),
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
        cursor.offset.hub_address += @intFromEnum(size);

        const is_instr_aligned = std.mem.isAligned(cursor.offset.hub_address, 4);
        switch (cursor.offset.local) {
            .cog, .lut => |*val| if (is_instr_aligned) {
                val.* += 1;
            },
            .hub => {},
        }
    }

    fn alignas(cursor: *Cursor, alignment: u20) void {
        const prev = cursor.offset.hub_address;
        const next = std.mem.alignForward(u20, cursor.offset.hub_address, alignment);

        cursor.offset.hub_address = next;

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
    const seg: Segment_ID = @enumFromInt(0x1234_5678);

    var cursor: Cursor = .init(seg);

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
    try std.testing.expectEqual(TaggedAddress.init_cog(seg, 30, 7), cursor.offset);

    cursor.alignas(2);
    try std.testing.expectEqual(TaggedAddress.init_cog(seg, 30, 7), cursor.offset);

    cursor.alignas(4);
    try std.testing.expectEqual(TaggedAddress.init_cog(seg, 32, 8), cursor.offset);
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

    start_addr: ?TaggedAddress = null,
    end_addr: ?TaggedAddress = null,

    /// Size of the instruction slot in bytes
    byte_size: ?u32 = null,

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

    pub fn fatal_error(ctx: FunctionCallContext, comptime msg: []const u8, args: anytype) error{DiagnosedFailure} {
        try ctx.ana.emit_error(ctx.location, msg, args);
        return error.DiagnosedFailure;
    }

    pub fn emit_error(ctx: FunctionCallContext, comptime msg: []const u8, args: anytype) !void {
        try ctx.ana.emit_error(ctx.location, msg, args);
    }

    pub fn emit_warning(ctx: FunctionCallContext, comptime msg: []const u8, args: anytype) !void {
        try ctx.ana.emit_warning(ctx.location, msg, args);
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
