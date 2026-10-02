const std = @import("std");

const ast = @import("frontend/ast.zig");
const eval = @import("stdlib/eval.zig");
const parser = @import("frontend/parser.zig");
const emit = @import("emit.zig");

pub const Collection = @This();

arena: std.heap.ArenaAllocator,
diagnostics: std.ArrayListUnmanaged(Diagnostic) = .empty,
sources: std.StringArrayHashMapUnmanaged(Source) = .empty,

pub const Diagnostic = struct {
    location: ?ast.Location,
    kind: Kind,
};

pub const Source = struct {
    text: []const u8,
};

pub const RenderOptions = struct {
    include_warnings: bool = true,
    include_infos: bool = true,
};

pub const SymbolDefinition = union(enum) {
    undefined,
    code: ast.Location,
    data: ast.Location,
    constant: ast.Location,
    builtin,
};

pub const Level = enum { @"error", warning, info };
pub const ChecklistSymbolType = enum { code, data, constant, builtin };
pub const AddressFunction = enum { hubaddr, cogaddr, lutaddr, localaddr };
pub const EvaluationFailure = enum {
    out_of_memory,
    undefined_symbol,
    invalid_function_call,
    overflow,
    divide_by_zero,
    invalid_argument,
    type_mismatch,

    fn text(reason: EvaluationFailure) []const u8 {
        return switch (reason) {
            .out_of_memory => "out of memory",
            .undefined_symbol => "referenced undefined symbol",
            .invalid_function_call => "invalid function call",
            .overflow => "integer overflow",
            .divide_by_zero => "division by zero",
            .invalid_argument => "invalid argument",
            .type_mismatch => "type mismatch",
        };
    }
};

pub const OperatorKind = union(enum) {
    unary: ast.UnaryOperator,
    binary: ast.BinaryOperator,

    pub fn format(operator: OperatorKind, writer: *std.Io.Writer) !void {
        switch (operator) {
            .unary => |value| switch (value) {
                .pre_increment, .post_increment => try writer.writeAll("++"),
                .pre_decrement, .post_decrement => try writer.writeAll("--"),
                else => try writer.print("{t}", .{value}),
            },
            .binary => |value| if (value == .array_index) try writer.writeAll("[]") else try writer.print("{t}", .{value}),
        }
    }
};

pub const Check = struct { check: []const u8 };
pub const Token = struct { token: []const u8 };
pub const Name = struct { name: []const u8 };
pub const NameKindHubLocal = struct { name: []const u8, kind: ChecklistSymbolType, hub: ?u32, local: ?u32 };
pub const Start = struct { start: u32 };
pub const ExpectedActual = struct { expected: usize, actual: usize };
pub const OffsetExpectedActual = struct { offset: usize, expected: u8, actual: u8 };
pub const NameReferenceLocation = struct { name: []const u8, reference_location: ast.Location };
pub const Function = struct { function: []const u8 };
pub const Mnemonic = struct { mnemonic: []const u8 };
pub const PathReason = struct { path: []const u8, reason: anyerror };
pub const Value = struct { value: i64 };
pub const NameReason = struct { name: []const u8, reason: anyerror };
pub const MnemonicFound = struct { mnemonic: []const u8, found: usize };
pub const Message = struct { message: []const u8 };
pub const Count = struct { count: usize };
pub const CodeExpectedActual = struct { code: []const u8, expected: usize, actual: usize };
pub const MnemonicEffect = struct { mnemonic: []const u8, effect: ast.Effect };
pub const Key = struct { key: []const u8 };
pub const LeftRightStart = struct { left: u32, right: u32, start: u32 };
pub const BitsExpectedEmitted = struct { bits: usize, expected: i64, emitted: i64 };
pub const SourceModeTargetMode = struct { source_mode: eval.ExecMode, target_mode: eval.ExecMode };
pub const OperatorValueType = struct { operator: OperatorKind, value_type: eval.Value.Type };
pub const OperatorLhsTypeRhsType = struct { operator: OperatorKind, lhs_type: eval.Value.Type, rhs_type: eval.Value.Type };
pub const PointerModifier = struct { operator: OperatorKind, modifier: enum { index, increment } };
pub const RegisterConstraint = struct { actual: eval.Register, allowed: enum { pointer_expression, pointer_operand } };
pub const BranchDistance = struct { distance: i64, unit: enum { bytes, instructions } };
pub const MissingHubAddress = enum { current, target, branch_target, hubaddr_argument };
pub const ArgumentCount = struct { subject: []const u8, min: usize, max: usize, found: usize };
pub const ExpectedValueType = struct { subject: []const u8, expected: eval.Value.Type, actual: eval.Value.Type };
pub const NumericRange = struct { subject: []const u8, min: i64, max: i64, actual: i64 };
pub const AddressRange = struct { subject: enum { origin, cursor, label }, space: eval.ExecMode, actual: u32, max_exclusive: u32 };
pub const DuplicateDefinition = struct { name: []const u8, previous: union(enum) { symbol: SymbolDefinition, function } };
pub const ParameterAlreadyPassed = struct { parameter: []const u8, function: []const u8, previous: union(enum) { positional: usize, named: ast.Location } };
pub const EvaluationContext = enum { expression, alignment };
pub const EvaluationFailureContext = struct { context: EvaluationContext, reason: EvaluationFailure };
pub const DirectiveMode = struct { directive: []const u8, mode: eval.ExecMode };
pub const AddressSpaceMismatch = struct { subject: []const u8, expected: eval.ExecMode, actual: eval.ExecMode };
pub const OperandUsage = struct { expected: enum { immediate, register }, actual: enum { immediate, register } };
pub const OperatorNoEffect = struct { operator: ast.UnaryOperator, label: enum { code, data } };
pub const FunctionValueType = struct { function: AddressFunction, value_type: eval.Value.Type };
pub const FunctionExpectedTypeActualType = struct { function: AddressFunction, expected_type: eval.ExecMode, actual_type: eval.ExecMode };
pub const FunctionParameter = struct { function: []const u8, parameter: []const u8 };
pub const ParameterFunction = struct { parameter: []const u8, function: []const u8 };
pub const TokenType = struct { token_type: parser.TokenType };
pub const Text = struct { text: []const u8 };
pub const Character = struct { character: u8 };
pub const StartEnd = struct { start: u6, end: u6 };
pub const PeriodsDuration = struct { periods: u128, duration: std.Io.Duration };

pub const Format = struct { format: emit.BinaryFormat };

pub const Kind = union(enum) {
    err_a_delay_of_periods_cannot_be_represented_with_32_bits: PeriodsDuration,
    err_address_delta_not_divisible_by_four,
    err_address_function_invalid_operand_type: FunctionValueType,
    err_address_has_no_execution_pc,
    err_address_has_no_hub_location: MissingHubAddress,
    err_address_outside_space: AddressRange,
    err_address_space_mismatch: AddressSpaceMismatch,
    err_align_references_label,
    err_align_value_must_be_a_nonzero_power_of_two: Value,
    err_ambigious_instruction_selection_for: Mnemonic,
    err_argument_count_mismatch: ArgumentCount,
    err_assertion_failed: Message,
    err_branch_too_far: BranchDistance,
    err_cannot_aug_operand,
    err_cannot_be_used_without_effect_operator: Mnemonic,
    err_cannot_emit_code_in_this_segment,
    err_cannot_emit_data_after_reserve_or_inside_regspace,
    err_cannot_read_file: PathReason,
    err_cannot_write_operand_integer_overflow,
    err_canont_use_the_effect_operator: MnemonicEffect,
    err_character_literal_contains_more_than_one_character,
    err_checklist_diagnostic_count_mismatch: CodeExpectedActual,
    err_checklist_err_requires_one_diagnostic_code,
    err_checklist_hex_bytes_require_pairs_of_digits,
    err_checklist_mem_requires_address_comparison_format_and,
    err_checklist_memory_byte_mismatch: OffsetExpectedActual,
    err_checklist_memory_length_mismatch_expected_got: ExpectedActual,
    err_checklist_memory_range_exceeds_assembled_output,
    err_checklist_segment_at_0x_x_does_not_match_length_or_mode: Start,
    err_checklist_sym_requires_a_name_and_type_hub_local,
    err_checklist_symbol_does_not_exist: Name,
    err_checklist_symbol_does_not_match_actual_type_hub_local: NameKindHubLocal,
    err_checklist_whole_memory_comparison_must_start_at_zero,
    err_constant_requires_integer_not_offset: Name,
    err_constants_cannot_store_pointer_expression,
    err_current_pc_unavailable,
    err_directive_invalid_in_mode: DirectiveMode,
    err_duplicate_definition: DuplicateDefinition,
    err_empty_character_literal_not_allowed,
    err_expected_offset_of_type_but_got_type: FunctionExpectedTypeActualType,
    err_expected_value_type: ExpectedValueType,
    err_expression_evaluation_failed: EvaluationFailureContext,
    err_file_requires_file_i_o,
    err_file_requires_one_string_literal_path,
    err_function_must_be_root: Function,
    err_has_no_parameter_named: FunctionParameter,
    err_instruction_operand_count_unmatched: MnemonicFound,
    err_integer_overflow_does_not_fit_into_a_i64: Text,
    err_invalid_character_in_string_char_literal_0x_x_0_2: Character,
    err_invalid_checklist_diagnostic_code: Token,
    err_invalid_checklist_hex_byte,
    err_invalid_checklist_memory_address,
    err_invalid_checklist_memory_comparison,
    err_invalid_checklist_memory_format,
    err_invalid_checklist_memory_integer: Token,
    err_invalid_checklist_segment_specification,
    err_invalid_checklist_symbol_specification,
    err_invalid_unicode_escape_format,
    err_is_not_a_valid_enumerator: Key,
    err_localaddr_is_only_valid_for_registers_in_a_cogexec_scope,
    err_missing_parameter_for_function: ParameterFunction,
    err_multiple_input_files_are_not_supported_yet,
    err_numeric_value_out_of_range: NumericRange,
    err_operand_usage_mismatch: OperandUsage,
    err_operator_at_cannot_be_used_in_this_scope,
    err_operator_invalid_operand_type: OperatorValueType,
    err_operator_invalid_operand_types: OperatorLhsTypeRhsType,
    err_org_cannot_move_pc_backward,
    err_parameter_already_passed: ParameterAlreadyPassed,
    err_pins_and_are_not_in_the_same_pin_group: StartEnd,
    err_pointer_modifier_already_set: PointerModifier,
    err_positional_after_named_argument,
    err_register_not_allowed: RegisterConstraint,
    err_requires_an_integer_known_during_layout: NameReason,
    err_segments_and_overlap_at_hub_address_0x_x_0_5: LeftRightStart,
    err_text_after_checklist_memory_block,
    err_the_pin_range_from_to_wraps_inside_its_register: StartEnd,
    err_undefined_reference_to_symbol_at: NameReferenceLocation,
    err_unexpected_in_checklist_memory_block,
    err_unexpected_token_expected_end_of_line_but_found: TokenType,
    err_unknown_checklist_check: Check,
    err_unknown_function: Function,
    err_unknown_instruction_effect: Text,
    err_unknown_mnemonic: Mnemonic,
    err_unrecognized_token: TokenType,
    err_syntax_error: Text,
    err_unterminated_checklist_memory_block,
    err_unterminated_escape_sequence,
    err_usage_cannot_emit_to_stdio: Format,
    err_usage_missing_input_files,
    warn_address_function_expected_offset: FunctionValueType,
    warn_alti_state_segment_mismatch,
    warn_branch_into_data,
    warn_emitted_padding_byte_s: Count,
    warn_integer_was_truncated_to_bits_expected_emitted: BitsExpectedEmitted,
    warn_invalid_escape_sequence: Character,
    warn_jump_between_exec_modes: SourceModeTargetMode,
    warn_operator_no_effect: OperatorNoEffect,
    warn_pin_range_wraps: StartEnd,
    warn_symbol_has_no_references: Name,
    warn_waitx_delay_too_short,

    pub fn level(self: Kind) Level {
        comptime {
            @setEvalBranchQuota(10000);
        }
        return switch (self) {
            inline else => |_, tag| blk: {
                const name = @tagName(tag);
                if (comptime std.mem.startsWith(u8, name, "err_")) break :blk .@"error";
                if (comptime std.mem.startsWith(u8, name, "warn_")) break :blk .warning;
                if (comptime std.mem.startsWith(u8, name, "info_")) break :blk .info;
                @compileError("diagnostic tag must start with err_, warn_, or info_");
            },
        };
    }

    pub fn render(self: Kind, writer: *std.Io.Writer) !void {
        switch (self) {
            .err_checklist_err_requires_one_diagnostic_code => try writer.writeAll("checklist err requires one diagnostic code"),
            .err_invalid_checklist_diagnostic_code => |v| try writer.print("invalid checklist diagnostic code '{s}'", .{v.token}),
            .err_checklist_diagnostic_count_mismatch => |v| try writer.print("checklist diagnostic {s}: expected {}, got {}", .{ v.code, v.expected, v.actual }),
            .err_checklist_sym_requires_a_name_and_type_hub_local => try writer.print("checklist sym requires a name and type:hub[:local]", .{}),
            .err_invalid_checklist_symbol_specification => try writer.print("invalid checklist symbol specification", .{}),
            .err_invalid_checklist_segment_specification => try writer.print("invalid checklist segment specification", .{}),
            .err_checklist_mem_requires_address_comparison_format_and => try writer.print("checklist mem requires address, comparison, format, and '['", .{}),
            .err_invalid_checklist_memory_address => try writer.print("invalid checklist memory address", .{}),
            .err_invalid_checklist_memory_comparison => try writer.print("invalid checklist memory comparison", .{}),
            .err_invalid_checklist_memory_format => try writer.print("invalid checklist memory format", .{}),
            .err_unknown_checklist_check => |v| try writer.print("unknown checklist check '{s}'", .{v.check}),
            .err_unterminated_checklist_memory_block => try writer.print("unterminated checklist memory block", .{}),
            .err_text_after_checklist_memory_block => try writer.print("text after checklist memory block", .{}),
            .err_unexpected_in_checklist_memory_block => try writer.print("unexpected '[' in checklist memory block", .{}),
            .err_checklist_hex_bytes_require_pairs_of_digits => try writer.print("checklist hex bytes require pairs of digits", .{}),
            .err_invalid_checklist_hex_byte => try writer.print("invalid checklist hex byte", .{}),
            .err_invalid_checklist_memory_integer => |v| try writer.print("invalid checklist memory integer '{s}'", .{v.token}),
            .err_checklist_symbol_does_not_exist => |v| try writer.print("checklist symbol '{s}' does not exist", .{v.name}),
            .err_checklist_symbol_does_not_match_actual_type_hub_local => |v| try writer.print("checklist symbol '{s}' does not match: actual type {t}, hub {?}, local {?}", .{ v.name, v.kind, v.hub, v.local }),
            .err_checklist_segment_at_0x_x_does_not_match_length_or_mode => |v| try writer.print("checklist segment at 0x{X} does not match length or mode", .{v.start}),
            .err_checklist_whole_memory_comparison_must_start_at_zero => try writer.print("checklist whole-memory comparison must start at zero", .{}),
            .err_checklist_memory_length_mismatch_expected_got => |v| try writer.print("checklist memory length mismatch: expected {}, got {}", .{ v.expected, v.actual }),
            .err_checklist_memory_range_exceeds_assembled_output => try writer.print("checklist memory range exceeds assembled output", .{}),
            .err_checklist_memory_byte_mismatch => |v| try writer.print("checklist memory mismatch at 0x{X}: expected 0x{X:0>2}, got 0x{X:0>2}", .{ v.offset, v.expected, v.actual }),
            .err_multiple_input_files_are_not_supported_yet => try writer.print("multiple input files are not supported yet", .{}),
            .err_undefined_reference_to_symbol_at => |v| try writer.print("undefined reference to symbol {s} at {f}", .{ v.name, v.reference_location }),
            .err_unknown_mnemonic => |v| try writer.print("unknown mnemonic {s}", .{v.mnemonic}),
            .err_file_requires_one_string_literal_path => try writer.print("FILE requires one string literal path", .{}),
            .err_file_requires_file_i_o => try writer.print("FILE requires file I/O", .{}),
            .err_cannot_read_file => |v| try writer.print("cannot read FILE {s}: {s}", .{ v.path, @errorName(v.reason) }),
            .err_unknown_function => |v| try writer.print("unknown function {s}", .{v.function}),
            .err_align_value_must_be_a_nonzero_power_of_two => |v| try writer.print(".align value {} must be a nonzero power of two.", .{v.value}),
            .err_align_references_label => try writer.print(".align value could not be evaluated: cannot refer to labels in .align", .{}),
            .err_org_cannot_move_pc_backward => try writer.print(".org cannot move PC backward", .{}),
            .err_cannot_emit_data_after_reserve_or_inside_regspace => try writer.print("cannot emit data after .reserve or inside .regspace", .{}),
            .err_cannot_emit_code_in_this_segment => try writer.print("cannot emit code in this segment", .{}),
            .err_requires_an_integer_known_during_layout => |v| try writer.print("{s} requires an integer known during layout: {s}", .{ v.name, @errorName(v.reason) }),
            .warn_symbol_has_no_references => |v| try writer.print("symbol {s} has no references", .{v.name}),
            .err_constant_requires_integer_not_offset => |v| try writer.print("constant {s} evaluated to memory offset, but expected integer. Use hubaddr() or cogaddr() to resolve the value.", .{v.name}),
            .err_constants_cannot_store_pointer_expression => try writer.print("constants cannot store pointer expression.", .{}),
            .err_current_pc_unavailable => try writer.writeAll("$ requires a current instruction address"),
            .err_instruction_operand_count_unmatched => |v| try writer.print("Could not find a matching instruction for {s}: No variant expects {} operands.", .{ v.mnemonic, v.found }),
            .err_ambigious_instruction_selection_for => |v| try writer.print("Ambigious instruction selection for {s}", .{v.mnemonic}),
            .err_assertion_failed => |v| try writer.print("assertion failed: {s}", .{v.message}),
            .warn_emitted_padding_byte_s => |v| try writer.print("emitted {} padding byte(s)", .{v.count}),
            .err_canont_use_the_effect_operator => |v| try writer.print("{s} canont use the effect operator :{t}", .{ v.mnemonic, v.effect }),
            .err_cannot_be_used_without_effect_operator => |v| try writer.print("{s} cannot be used without effect operator", .{v.mnemonic}),
            .err_is_not_a_valid_enumerator => |v| try writer.print("#{s} is not a valid enumerator", .{v.key}),
            .warn_branch_into_data => try writer.print("branch into .data", .{}),
            .warn_alti_state_segment_mismatch => try writer.print("alti.state() combines addresses from different segments", .{}),
            .err_cannot_aug_operand => try writer.print("Cannot aug() operand", .{}),
            .err_cannot_write_operand_integer_overflow => try writer.print("cannot write operand: integer overflow", .{}),
            .err_segments_and_overlap_at_hub_address_0x_x_0_5 => |v| try writer.print("segments {} and {} overlap at hub address 0x{X:0>5}", .{ v.left, v.right, v.start }),
            .warn_integer_was_truncated_to_bits_expected_emitted => |v| try writer.print("Integer was truncated to {} bits. Expected: {}, Emitted: {}", .{ v.bits, v.expected, v.emitted }),
            .warn_jump_between_exec_modes => |v| try writer.print("jumping from {t}exec mode into code that was defined in {t}exec mode. This is potentially unwanted behaviour!", .{ v.source_mode, v.target_mode }),
            .err_operator_at_cannot_be_used_in_this_scope => try writer.print("Operator '@' cannot be used in this scope", .{}),
            .err_address_delta_not_divisible_by_four => try writer.print("'@' cannot be applied to a offset range which is non-divisible by 4", .{}),
            .err_localaddr_is_only_valid_for_registers_in_a_cogexec_scope => try writer.print("localaddr() is only valid for registers in a cogexec scope", .{}),
            .err_expected_offset_of_type_but_got_type => |v| try writer.print("{t}() expected offset of type {t}, but got type {t}.", .{ v.function, v.expected_type, v.actual_type }),
            .err_address_has_no_execution_pc => try writer.print("address has no execution PC", .{}),
            .err_positional_after_named_argument => try writer.print("positional arguments must not appear after a named argument", .{}),
            .err_has_no_parameter_named => |v| try writer.print("{s}() has no parameter named {s}", .{ v.function, v.parameter }),
            .err_missing_parameter_for_function => |v| try writer.print("Missing parameter {s} for function {s}()", .{ v.parameter, v.function }),
            .err_unrecognized_token => |v| try writer.print("unrecognized token: {t}", .{v.token_type}),
            .err_syntax_error => |v| try writer.print("invalid syntax: {s}", .{v.text}),
            .err_unknown_instruction_effect => |v| try writer.print("unknown instruction effect: {s}", .{v.text}),
            .err_unexpected_token_expected_end_of_line_but_found => |v| try writer.print("unexpected token: expected end of line, but found {t}", .{v.token_type}),
            .err_integer_overflow_does_not_fit_into_a_i64 => |v| try writer.print("integer overflow: {s} does not fit into a i64!", .{v.text}),
            .err_empty_character_literal_not_allowed => try writer.print("empty character literal not allowed!", .{}),
            .err_character_literal_contains_more_than_one_character => try writer.print("character literal contains more than one character!", .{}),
            .err_invalid_character_in_string_char_literal_0x_x_0_2 => |v| try writer.print("invalid character in string/char literal: 0x{X:0>2}", .{v.character}),
            .err_unterminated_escape_sequence => try writer.print("unterminated escape sequence", .{}),
            .err_invalid_unicode_escape_format => try writer.print("unicode escape sequence must have the format \\u{{...}} where ... is a hexadecimal notation of the code point", .{}),
            .warn_invalid_escape_sequence => |v| try writer.print("invalid escape sequence: \\{c}", .{v.character}),
            .err_pins_and_are_not_in_the_same_pin_group => |v| try writer.print("Pins {} and {} are not in the same pin group", .{ v.start, v.end }),
            .warn_pin_range_wraps => |v| try writer.print("The pin range from {} to {} wraps inside its register. Add wrap=#on to mute this, or wrap=#off to make it an error.", .{ v.start, v.end }),
            .err_the_pin_range_from_to_wraps_inside_its_register => |v| try writer.print("The pin range from {} to {} wraps inside its register.", .{ v.start, v.end }),
            .warn_waitx_delay_too_short => try writer.print("Requested delay time is less than 2 periods. It's recommended to remove the WAITX in question.", .{}),
            .err_a_delay_of_periods_cannot_be_represented_with_32_bits => |v| try writer.print("A delay of {} periods ({f}) cannot be represented with 32 bits.", .{ v.periods, v.duration }),
            .err_usage_missing_input_files => try writer.print("usage error: missing input files.", .{}),
            .err_usage_cannot_emit_to_stdio => |v| try writer.print("usage error: Cannot emit {t} to stdio. Use \"-o -\" to force emission to stdout.", .{v.format}),
            .err_address_function_invalid_operand_type => |v| try writer.print("{t}() cannot be applied to a value of type {t}", .{ v.function, v.value_type }),
            .err_address_has_no_hub_location => |v| try writer.print("{s} has no hub address", .{switch (v) {
                .current => "current address",
                .target => "target address",
                .branch_target => "branch target",
                .hubaddr_argument => "hubaddr() argument",
            }}),
            .err_address_outside_space => |v| try writer.print("{s} address 0x{x} is outside {t} space (must be below 0x{x})", .{ @tagName(v.subject), v.actual, v.space, v.max_exclusive }),
            .err_address_space_mismatch => |v| try writer.print("{s} requires an address in {t} space, got {t}", .{ v.subject, v.expected, v.actual }),
            .err_argument_count_mismatch => |v| try writer.print("{s} expects {}..{} arguments, found {}", .{ v.subject, v.min, v.max, v.found }),
            .err_branch_too_far => |v| try writer.print("branch too far: cannot jump by {} {s}", .{ v.distance, @tagName(v.unit) }),
            .err_directive_invalid_in_mode => |v| try writer.print("{s} is invalid in {t} mode", .{ v.directive, v.mode }),
            .err_duplicate_definition => |v| switch (v.previous) {
                .symbol => |kind| try writer.print("symbol {s} ({}) is already defined", .{ v.name, kind }),
                .function => try writer.print("function {s} is already defined", .{v.name}),
            },
            .err_expected_value_type => |v| try writer.print("{s} requires {t}, found {t}", .{ v.subject, v.expected, v.actual }),
            .err_expression_evaluation_failed => |v| try writer.print("{s} could not be evaluated: {s}", .{ @tagName(v.context), v.reason.text() }),
            .err_function_must_be_root => |v| try writer.print("{s}() must be the root of an expression", .{v.function}),
            .err_numeric_value_out_of_range => |v| try writer.print("{s} must be in range {}..{}, found {}", .{ v.subject, v.min, v.max, v.actual }),
            .err_operand_usage_mismatch => |v| try writer.print("expected {s} operand, found {s}", .{ @tagName(v.expected), @tagName(v.actual) }),
            .err_operator_invalid_operand_type => |v| try writer.print("operator '{f}' cannot be applied to a value of type {t}", .{ v.operator, v.value_type }),
            .err_operator_invalid_operand_types => |v| try writer.print("operator '{f}' cannot be applied to {t} and {t}", .{ v.operator, v.lhs_type, v.rhs_type }),
            .err_parameter_already_passed => |v| switch (v.previous) {
                .positional => |index| try writer.print("parameter {s} of {s}() was already passed positionally as argument {}", .{ v.parameter, v.function, index + 1 }),
                .named => |location| try writer.print("parameter {s} of {s}() was already passed by name at {f}", .{ v.parameter, v.function, location }),
            },
            .err_pointer_modifier_already_set => |v| try writer.print("operator '{f}' cannot be applied: pointer {s} is already set", .{ v.operator, @tagName(v.modifier) }),
            .err_register_not_allowed => |v| try writer.print("register {f} is not allowed for {s}", .{ v.actual, switch (v.allowed) {
                .pointer_expression => "pointer expressions",
                .pointer_operand => "pointer operands",
            } }),
            .warn_address_function_expected_offset => |v| try writer.print("{t}() expected offset, found {t}", .{ v.function, v.value_type }),
            .warn_operator_no_effect => |v| try writer.print("operator '{t}' has no effect on a {s} label", .{ v.operator, @tagName(v.label) }),
        }
    }
};

pub fn init(allocator: std.mem.Allocator) Collection {
    return .{
        .arena = .init(allocator),
    };
}

pub fn deinit(self: *Collection) void {
    const allocator = self.arena.allocator();
    self.diagnostics.deinit(allocator);
    self.sources.deinit(allocator);
    self.arena.deinit();
    self.* = undefined;
}

pub fn register_source(self: *Collection, path: []const u8, source: []const u8) !void {
    const allocator = self.arena.allocator();
    const gop = try self.sources.getOrPut(allocator, path);

    if (!gop.found_existing)
        gop.key_ptr.* = try allocator.dupe(u8, path);

    gop.value_ptr.* = .{
        .text = source,
    };
}

pub fn emit_diag(self: *Collection, location: ?ast.Location, diagnostic: Kind) !void {
    const allocator = self.arena.allocator();
    try self.diagnostics.append(allocator, .{
        .location = try own(allocator, location),
        .kind = try own(allocator, diagnostic),
    });
}

fn own(allocator: std.mem.Allocator, value: anytype) !@TypeOf(value) {
    const T = @TypeOf(value);
    return switch (@typeInfo(T)) {
        .pointer => |pointer| blk: {
            if (pointer.size != .slice) @compileError("diagnostic payloads cannot contain borrowed pointers");
            if (pointer.child == u8) break :blk try allocator.dupe(u8, value);
            const copy = try allocator.alloc(pointer.child, value.len);
            for (value, copy) |item, *target| target.* = try own(allocator, item);
            break :blk copy;
        },
        .optional => if (value) |item| try own(allocator, item) else null,
        .@"struct" => blk: {
            var copy: T = undefined;
            inline for (@typeInfo(T).@"struct".fields) |field|
                @field(copy, field.name) = if (@typeInfo(field.type) == .error_set)
                    @field(value, field.name)
                else
                    try own(allocator, @field(value, field.name));
            break :blk copy;
        },
        .@"union" => switch (value) {
            inline else => |item, tag| @unionInit(T, @tagName(tag), try own(allocator, item)),
        },
        else => value,
    };
}

pub fn has_errors(self: Collection) bool {
    for (self.diagnostics.items) |diagnostic| {
        if (diagnostic.kind.level() == .@"error")
            return true;
    }
    return false;
}

pub fn has_warnings(self: Collection) bool {
    for (self.diagnostics.items) |diagnostic| {
        if (diagnostic.kind.level() == .warning)
            return true;
    }
    return false;
}

pub fn render(self: Collection, writer: *std.Io.Writer, options: RenderOptions) !void {
    for (self.diagnostics.items) |item| {
        if (!should_render(item, options))
            continue;

        if (item.location) |location| {
            try render_location(self, writer, item, location);
        } else {
            try writer.print("{t}: ", .{item.kind.level()});
            try item.kind.render(writer);
            try writer.writeByte('\n');
        }
    }
}

fn should_render(item: Diagnostic, options: RenderOptions) bool {
    return switch (item.kind.level()) {
        .@"error" => true,
        .warning => options.include_warnings,
        .info => options.include_infos,
    };
}

fn render_location(self: Collection, writer: *std.Io.Writer, item: Diagnostic, location: ast.Location) !void {
    if (location.source) |path| {
        try writer.print("{s}:{d}:{d}: {t}: ", .{
            path,
            location.line,
            location.column,
            item.kind.level(),
        });
        try item.kind.render(writer);
        try writer.writeByte('\n');

        if (self.sources.get(path)) |source| {
            if (source_line(source.text, location.line)) |line| {
                const line_width = @max(@as(usize, 4), decimal_width(location.line));
                try writer.splatByteAll(' ', line_width - decimal_width(location.line));
                try writer.print("{d} | {s}\n", .{ location.line, line });
                try writer.splatByteAll(' ', line_width);
                try writer.writeAll(" | ");
                if (location.column > 1)
                    try writer.splatByteAll(' ', location.column - 1);
                try writer.writeAll("^\n");
            }
        }
    } else {
        try writer.print("{d}:{d}: {t}: ", .{
            location.line,
            location.column,
            item.kind.level(),
        });
        try item.kind.render(writer);
        try writer.writeByte('\n');
    }
}

fn source_line(source: []const u8, one_based_line: u32) ?[]const u8 {
    if (one_based_line == 0)
        return null;

    var start: usize = 0;
    var current_line: u32 = 1;

    while (current_line < one_based_line) : (current_line += 1) {
        const newline = std.mem.indexOfScalarPos(u8, source, start, '\n') orelse return null;
        start = newline + 1;
    }

    var end = std.mem.indexOfScalarPos(u8, source, start, '\n') orelse source.len;
    if (end > start and source[end - 1] == '\r')
        end -= 1;

    return source[start..end];
}

fn decimal_width(value: u32) usize {
    var digits: usize = 1;
    var rest = value;
    while (rest >= 10) : (digits += 1)
        rest /= 10;
    return digits;
}

test "collects diagnostics" {
    var collection: Collection = .init(std.testing.allocator);
    defer collection.deinit();

    try collection.emit_diag(null, .err_multiple_input_files_are_not_supported_yet);
    try collection.emit_diag(null, .warn_branch_into_data);

    try std.testing.expect(collection.has_errors());
    try std.testing.expect(collection.has_warnings());
}

test "renders source excerpts" {
    var collection: Collection = .init(std.testing.allocator);
    defer collection.deinit();

    try collection.register_source("test.propan", "first\n    BAD\n");
    try collection.emit_diag(.{ .source = "test.propan", .line = 2, .column = 5 }, .{
        .err_unknown_mnemonic = .{
            .mnemonic = "BAD",
        },
    });

    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();

    try collection.render(&output.writer, .{});

    const actual = try output.toOwnedSlice();
    defer std.testing.allocator.free(actual);

    try std.testing.expectEqualStrings(
        \\test.propan:2:5: error: unknown mnemonic BAD
        \\   2 |     BAD
        \\     |     ^
        \\
    , actual);
}

test "renders missing location fallback" {
    var collection: Collection = .init(std.testing.allocator);
    defer collection.deinit();

    try collection.emit_diag(null, .err_usage_missing_input_files);

    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();

    try collection.render(&output.writer, .{});

    const actual = try output.toOwnedSlice();
    defer std.testing.allocator.free(actual);

    try std.testing.expectEqualStrings("error: usage error: missing input files.\n", actual);
}

test "can suppress warnings" {
    var collection: Collection = .init(std.testing.allocator);
    defer collection.deinit();

    try collection.emit_diag(null, .warn_branch_into_data);
    try collection.emit_diag(null, .err_cannot_aug_operand);

    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();

    try collection.render(&output.writer, .{ .include_warnings = false });

    const actual = try output.toOwnedSlice();
    defer std.testing.allocator.free(actual);

    try std.testing.expectEqualStrings("error: Cannot aug() operand\n", actual);
}

test "owns diagnostic source and text properties" {
    var collection: Collection = .init(std.testing.allocator);
    defer collection.deinit();

    var path = [_]u8{ 't', '.', 'p' };
    var mnemonic = [_]u8{ 'B', 'A', 'D' };
    try collection.emit_diag(.{ .source = &path, .line = 1, .column = 2 }, .{
        .err_unknown_mnemonic = .{
            .mnemonic = &mnemonic,
        },
    });
    path[0] = 'x';
    mnemonic[0] = 'X';

    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();
    try collection.render(&output.writer, .{});
    const actual = try output.toOwnedSlice();
    defer std.testing.allocator.free(actual);
    try std.testing.expectEqualStrings("t.p:1:2: error: unknown mnemonic BAD\n", actual);
}

test "owns lists and their strings" {
    var arena: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer arena.deinit();

    var text = [_]u8{ 'a', 'b' };
    var list = [_][]const u8{text[0..]};
    const slice: []const []const u8 = &list;
    const copy = try own(arena.allocator(), slice);
    text[0] = 'x';
    list[0] = "changed";
    try std.testing.expectEqualStrings("ab", copy[0]);
}

test "renders escaped format braces" {
    var collection: Collection = .init(std.testing.allocator);
    defer collection.deinit();
    try collection.emit_diag(null, .err_invalid_unicode_escape_format);

    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();
    try collection.render(&output.writer, .{});
    const actual = try output.toOwnedSlice();
    defer std.testing.allocator.free(actual);
    try std.testing.expectEqualStrings(
        "error: unicode escape sequence must have the format \\u{...} where ... is a hexadecimal notation of the code point\n",
        actual,
    );
}

test "warning and error variants share pin range properties" {
    var collection: Collection = .init(std.testing.allocator);
    defer collection.deinit();
    try collection.emit_diag(null, .{
        .warn_pin_range_wraps = .{
            .start = 31,
            .end = 0,
        },
    });
    try collection.emit_diag(null, .{
        .err_the_pin_range_from_to_wraps_inside_its_register = .{
            .start = 31,
            .end = 0,
        },
    });

    try std.testing.expect(collection.has_warnings());
    try std.testing.expect(collection.has_errors());

    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();
    try collection.render(&output.writer, .{});
    const actual = try output.toOwnedSlice();
    defer std.testing.allocator.free(actual);
    try std.testing.expectEqualStrings(
        "warning: The pin range from 31 to 0 wraps inside its register. Add wrap=#on to mute this, or wrap=#off to make it an error.\n" ++
            "error: The pin range from 31 to 0 wraps inside its register.\n",
        actual,
    );
}

test "owns nested symbol definition source" {
    var collection: Collection = .init(std.testing.allocator);
    defer collection.deinit();
    var source = [_]u8{ 's', '.', 'p' };
    try collection.emit_diag(null, .{
        .err_duplicate_definition = .{
            .name = "symbol",
            .previous = .{ .symbol = .{ .code = .{ .source = &source, .line = 1, .column = 1 } } },
        },
    });
    source[0] = 'x';
    try std.testing.expectEqualStrings("s.p", collection.diagnostics.items[0].kind.err_duplicate_definition.previous.symbol.code.source.?);
}

test "renders enum diagnostic properties" {
    var collection: Collection = .init(std.testing.allocator);
    defer collection.deinit();
    try collection.emit_diag(null, .{
        .err_expression_evaluation_failed = .{ .context = .expression, .reason = .undefined_symbol },
    });
    try collection.emit_diag(null, .{
        .err_expression_evaluation_failed = .{ .context = .alignment, .reason = .divide_by_zero },
    });
    try collection.emit_diag(null, .{
        .warn_jump_between_exec_modes = .{
            .source_mode = .cog,
            .target_mode = .lut,
        },
    });
    try collection.emit_diag(null, .{
        .err_operator_invalid_operand_type = .{
            .operator = .{ .unary = .pre_increment },
            .value_type = .string,
        },
    });
    try collection.emit_diag(null, .{
        .err_operator_invalid_operand_types = .{
            .operator = .{ .binary = .array_index },
            .lhs_type = .int,
            .rhs_type = .string,
        },
    });
    try collection.emit_diag(null, .{ .err_branch_too_far = .{ .distance = -512, .unit = .bytes } });
    try collection.emit_diag(null, .{ .err_numeric_value_out_of_range = .{ .subject = "pointer index", .min = -32, .max = 31, .actual = 32 } });
    try collection.emit_diag(null, .{
        .err_cannot_read_file = .{
            .path = "missing.bin",
            .reason = error.FileNotFound,
        },
    });
    try collection.emit_diag(null, .{
        .err_usage_cannot_emit_to_stdio = .{ .format = .flat },
    });

    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();
    try collection.render(&output.writer, .{});
    const actual = try output.toOwnedSlice();
    defer std.testing.allocator.free(actual);
    try std.testing.expectEqualStrings(
        "error: expression could not be evaluated: referenced undefined symbol\n" ++
            "error: alignment could not be evaluated: division by zero\n" ++
            "warning: jumping from cogexec mode into code that was defined in lutexec mode. This is potentially unwanted behaviour!\n" ++
            "error: operator '++' cannot be applied to a value of type string\n" ++
            "error: operator '[]' cannot be applied to int and string\n" ++
            "error: branch too far: cannot jump by -512 bytes\n" ++
            "error: pointer index must be in range -32..31, found 32\n" ++
            "error: cannot read FILE missing.bin: FileNotFound\n" ++
            "error: usage error: Cannot emit flat to stdio. Use \"-o -\" to force emission to stdout.\n",
        actual,
    );
}
