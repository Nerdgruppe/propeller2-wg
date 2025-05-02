const std = @import("std");

const stdlib = @import("stdlib/stdlib.zig");
const ast = @import("frontend.zig").ast;

const logger = std.log.scoped(.sema);

pub fn analyze(allocator: std.mem.Allocator, file: ast.File) !Module {
    var analyzer: Analyzer = try .init(allocator, file);
    defer analyzer.deinit();

    // Prepare
    try analyzer.load_constants(stdlib.common.constants);
    try analyzer.load_constants(stdlib.p2.constants);

    try analyzer.load_instructions(stdlib.p2.instructions);

    // Validate
    try analyzer.declare_symbols();
    try analyzer.validate_symbol_refs();

    // Lay Out
    try analyzer.prepare_instruction_stream();
    try analyzer.select_instruction_type();

    try analyzer.assign_locations();

    // Translate

    //
    @panic("not implemented yet");
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
    hub_offset: u32,
    cog_offset: ?u32,
    type: Type,

    pub const Type = enum {
        code,
        data,
    };
};

pub const Constant = struct {
    value: Value,

    pub const Value = union(enum) {
        integer: u64,
        string: []const u8,
    };
};

pub const Segment = struct {
    hub_offset: u32,
    cog_offset: u9,

    data: []const u8,
};

const Analyzer = struct {
    allocator: std.mem.Allocator,
    arena: std.heap.ArenaAllocator,
    file: ast.File,

    symbols: std.StringArrayHashMapUnmanaged(SymbolInfo) = .empty,

    functions: std.StringArrayHashMapUnmanaged(Function) = .empty,
    mnemonics: std.StringArrayHashMapUnmanaged(MnemonicDesc) = .empty,

    seq_to_instr_lut: []const ?usize = &.{},
    instructions: []InstructionInfo = &.{},

    fn init(allocator: std.mem.Allocator, file: ast.File) !Analyzer {
        var ana: Analyzer = .{
            .allocator = allocator,
            .arena = .init(allocator),
            .file = file,
        };
        try ana.functions.ensureUnusedCapacity(allocator, 3);

        ana.functions.putAssumeCapacityNoClobber("hubaddr", .hubaddr);
        ana.functions.putAssumeCapacityNoClobber("cogaddr", .cogaddr);
        ana.functions.putAssumeCapacityNoClobber("aug", .aug);

        try ana.mnemonics.ensureUnusedCapacity(allocator, 9);

        ana.mnemonics.putAssumeCapacityNoClobber(".cogexec", .cogexec);
        ana.mnemonics.putAssumeCapacityNoClobber(".lutexec", .lutexec);
        ana.mnemonics.putAssumeCapacityNoClobber(".hubexec", .hubexec);
        ana.mnemonics.putAssumeCapacityNoClobber(".assert", .assert);
        ana.mnemonics.putAssumeCapacityNoClobber(".long", .long);
        ana.mnemonics.putAssumeCapacityNoClobber(".word", .word);
        ana.mnemonics.putAssumeCapacityNoClobber(".byte", .byte);

        return ana;
    }

    fn deinit(ana: *Analyzer) void {
        ana.arena.deinit();
        ana.symbols.deinit(ana.allocator);
        ana.functions.deinit(ana.allocator);
        ana.mnemonics.deinit(ana.allocator);
        ana.* = undefined;
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

    /// Loads predefined constants from a string hash map.
    /// The keys must have a lifetime longer than the Analyzer!
    fn load_constants(ana: *Analyzer, constants: std.StaticStringMap(i64)) !void {
        try ana.symbols.ensureUnusedCapacity(ana.allocator, constants.values().len);

        for (constants.keys(), constants.values()) |name, value| {
            const gop = ana.symbols.getOrPutAssumeCapacity(name);
            if (gop.found_existing) {
                logger.err("duplicate predefined symbol: {s} {}", .{ name, gop.value_ptr.type });
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

                if (all_eq) {
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
                        logger.err("symbol {s} ({}) is already defined!", .{ lbl.identifier, sym.type });
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
                        logger.err("symbol {s} ({}) is already defined!", .{ con.identifier, sym.type });
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
                if (sym.type == .undefined) {
                    logger.err("undefined reference to symbol {s} at {}", .{ sym.name, symref.location });
                }
            },

            .wrapped => |inner| try ana.validate_expr_symbol_refs(inner.*),

            .unary_transform => |trafo| try ana.validate_expr_symbol_refs(trafo.value.*),
            .binary_transform => |trafo| {
                try ana.validate_expr_symbol_refs(trafo.lhs.*);
                try ana.validate_expr_symbol_refs(trafo.rhs.*);
            },
            .function_call => |fncall| {
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

    fn select_instruction_type(ana: *Analyzer) !void {
        std.debug.assert(ana.seq_to_instr_lut.len == ana.file.sequence.len);

        for (ana.instructions) |*instr| {

            //
            _ = instr;
        }
    }

    ///
    /// Assigns all label and instruction locations to their associated position in hub/cog/lut ram
    ///
    fn assign_locations(ana: *Analyzer) !void {
        std.debug.assert(ana.seq_to_instr_lut.len == ana.file.sequence.len);

        for (ana.file.sequence, 0..) |seq, i| {
            _ = seq;
            _ = i;
        }
    }
};

const SymbolInfo = struct {
    name: []const u8,

    type: Type = .undefined,
    referenced: bool = false,

    hub_offset: ?u32 = null,
    cog_offset: ?u32 = null,
    value: ?i64 = null,

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

    /// Size of the instruction slot in bytes
    byte_size: ?u32 = null,
};

const Function = union(enum) {
    // the builtin functions are hardcoded here:
    aug,
    hubaddr,
    cogaddr,

    // stdlib functions are defined as "generic" ones:
    // TODO: generic: GenericFunction,
};

const MnemonicDesc = union(enum) {

    // data encoded
    long,
    word,
    byte,

    // directives:

    cogexec,
    lutexec,
    hubexec,
    assert,

    encoded: EncodedMnemonic,
};

const EncodedMnemonic = struct {
    variants: std.ArrayListUnmanaged(EncodedInstruction),
};

pub const EncodedInstruction = struct {
    mnemonic: []const u8,

    binary: u32,
    flags: Flags,
    operands: []const Operand,

    pub const Slot = struct {
        shift: u5,
        bits: u5,

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
            return ((1 << slot.bits) - 1) << slot.shift;
        }

        pub fn write(slot: Slot, container: *u32, value: u32) void {
            const shifted = value << slot.shift;
            std.debug.assert((shifted & ~slot.mask()) == 0);
            container.* |= shifted;
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
            /// D or S
            register,

            /// #D, #S or
            immediate: u32, // encodes limit

            /// {#}D or {#}S
            reg_or_imm: u32, // encodes limit

            /// Immediate value with absolute or relative addressing
            /// #{\}A
            address,

            /// Either #N or PTRx++, --PTRx, ...
            /// with N <= 255
            pointer_expr,

            /// PA, PB, PTRA or PTRB
            pointer_reg,

            /// One of the given named values.
            enumeration: std.StaticStringMap(u32),
        };
    };

    pub const Flags = packed struct {
        pub const none: Flags = .{
            .wz = false,
            .wc = false,
            .wcz = false,
            .wr = false,
            .and_c = false,
            .and_z = false,
            .or_c = false,
            .or_z = false,
            .xor_c = false,
            .xor_z = false,
            .wcz_not_used = .ignore,
        };

        wz: bool,
        wc: bool,
        wcz: bool,
        wr: bool,
        and_c: bool,
        and_z: bool,
        or_c: bool,
        or_z: bool,
        xor_c: bool,
        xor_z: bool,

        wcz_not_used: enum(u2) { ignore, warn, err },

        pub fn with(flags: Flags, comptime field: std.meta.FieldEnum(Flags), value: @FieldType(Flags, @tagName(field))) Flags {
            var copy = flags;
            @field(copy, @tagName(field)) = value;
            return copy;
        }

        pub fn add(flags: Flags, comptime field: std.meta.FieldEnum(Flags)) Flags {
            return flags.with(field, true);
        }
    };
};
