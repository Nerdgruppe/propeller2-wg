package p2db

// Primitive types:
#Mnemonic: string & =~"^[A-Z][A-Z0-9]+$"

// A single expression of the "semantic language".
#Expression: string

// A full program of the "semantic language".
#SemanticProgram: string

#FlagModifier: (
		"none" |
	"WC" | "WZ" | "WCZ" |
	"ANDC" | "ANDZ" |
	"ORC" | "ORZ" |
	"XORC" | "XORZ")

#RegisterName: (
		// 'S' is the source operand
		"S" |
	// 'D' is the destination operand. If used in a writing context, will use the "R" register when "ALTR" was issued before.
	"D" |
	"PA" |
	"PB" |
	"PTRA" |
	"PTRB" |
	"OUTA" |
	"OUTB" |
	"DIRA" |
	"DIRB" |
	"INA" |
	"INB" |
	"IJMP3" |
	"IRET3" |
	"IJMP2" |
	"IRET2" |
	"IJMP1" |
	"IRET1" )

// The name of an instruction field. Always one character wide and has a limited set of options.
#FieldName: =~"^[AcCDEILnNRSWzZ]$"

#Integer: int | (string & =~"^0b[01]+$" )
#Binary:  int

#Family: {
	// Root name of the family, e.g. "jse_jnse"
	name: string
}

#Context: {
	group:                      string //  display group
	tags?: [...string] | *[]    // machine-searchable labels
	related?: [...string] | *[] //  ids of closely related instructions
}

#Description: {
	short!: string & !~"\n" // One-liner description, plaintext
	full?:  string | *""    // Markdown, multi-paragraph
}

// Aliases are syntactic shorthands: some programmer-visible operands of the
// canonical instruction are replaced by fixed bit values. The remaining
// operands (if any) keep their usual roles.
//
// An alias does NOT get a root-level entry.
#Alias: {
	mnemonic!:   #Mnemonic
	description: #Description

	// Keys point to #InstrField.name
	fixed_fields!: [#FieldName]: #Binary // TODO: Also allow binary/hexadecimal, constrain to parent encoding fields
}

// The role of an instruction field.
// - condition: The field encodes the condition code of the instruction.
// - c-flag: The field encodes where the "update C flag" modifier is stored.
// - z-flag: The field encodes where the "update Z flag" modifier is stored.
// - operand: The field is an operand.
// - operand-mode: The field marks the register/immediate or absolute/relative mode of an operand.
#FieldRole: "condition" | "c-flag" | "z-flag" | "operand" | "operand-mode"

// A variable field inside an instruction encoding.
// The offset and size is determined by the patterns inside `#Encoding.pattern`.
#InstrField: {
	// the character that is used to encode the field in "pattern"
	name: #FieldName

	// What role does this field plays in the instruction
	role: #FieldRole
}

// Describes the encoding of the instruction itself.
#Encoding: {
	// Normative bit pattern. 
	// Format is a sequence of 32 letters and 0/1 digits, with spaces between
	// the 5 groups. The groups have the sizes [4,7,3,9,9]
	// Each letter/digit defines the bits, starting at the MSB down to the LSB.
	// Spaces have no significant meaning, but must be present.
	//
	// Standard letter meanings:
	//   E  condition nibble (4 bits)
	//   C  WC effect bit
	//   Z  WZ effect bit
	//   I  S-field immediate bit
	//   L  D-field immediate bit
	//   N  selector/index literal bits (nibble/byte/word index)
	//   D  destination register address (9 bits)
	//   S  source register address / S-immediate value (9 bits)
	//   W  sub-selector (WW = PA/PB/PTRA/PTRB in CALLD-imm20)
	//   R  relative/absolute flag
	//   A  address bits (up to 20)
	//   n  augment payload (AUGS/AUGD, 23 bits total)
	//   0  A literal zero bit
	//   1  A literal one bit
	pattern: string & =~"^[A-Za-z01]{4} [A-Za-z01]{7} [A-Za-z01]{3} [A-Za-z01]{9} [A-Za-z01]{9}$"

	// Only programmer-visible or semantically meaningful variable fields.
	// Fixed opcode constant bits are already encoded in pattern; omit them here.
	fields: [...#InstrField]
}

#OperandFormat: (
		// A 32 bit value without concrete interpretation
		"data32" |

	// One of the 16 condition codes
	"condition_code" |

	// A 9 bit address in the LUT
	"lut_addr" |

	// A 9 bit encoding of either an 8 bit LUT address or a pointer expression
	"lut_addr_expr" |

	// A 20 bit absolute address (memory space address)
	"abs20" |

	// A 20 bit pc-relative offset (number of instructions).
	"pcrel20" |

	// A packed value encoding C, Z and PC in the form of '{C, Z, 10'b0, PC[19:0]}'
	"cpu_state")

#OperandRole: (
		// The operand consumes a value, either from a register or an immediate.
		"input" |
	// The operand writes the register value.
	"reg_out" |
	// The operand first reads, then writes the register value.
	"reg_in_out" |
	//  The operand encodes a value read/written from/to the LUT
	"lut_value" |
	// The operand encodes a target address for a branch
	"branch_target")

#OperandProperties: {
	// Describes the expected format of the value.
	format: #OperandFormat

	// If set, the operand value can be augmented by a previous AUGS/AUGD execution.
	augmentable?: "AUGS" | "AUGD" | *false
}

// Describes a semantic operand of the instruction.
#Operand: {
	// The name of the operand rendered to the user.
	name!: string

	// Describes what role the operand has in the execution of the instruction.
	role!: #OperandRole

	// Defines in which `#Encoding.field[].name` the operand is stored.
	stored_in!: #FieldName

	// When the operand stores a 20 bit address value, the relative_in field
	// marks which bit of the encoding tells the cpu whether the address is 
	// relative=1 or absolute=0.
	// TODO: Validate "R=1 ~ relative" (documentation inconsistency)!
	//
	// If `null`, the operand does not use the relative-style addressing
	relative_in: #FieldName | *null

	// Defines whether this operand has an immediate form or not.
	// 
	// If the value is not `null`, it defines in which `#Encoding.field[].name` the
	// immediate bit is stored. `immediate_mode` is used, the bit will be `1`, otherwise `0`.
	// 
	immediate_in: #FieldName | *null

	if (immediate_in != null) {
		register_mode:  #OperandProperties
		immediate_mode: #OperandProperties
	}

	if (relative_in != null) {
		absolute_mode: #OperandProperties
		relative_mode: #OperandProperties
	}

	if (immediate_in == null && relative_in == null ) {
		#OperandProperties
	}

	if (immediate_in != null && relative_in != null ) {
		error("immediate_in and relative_in cannot be set at the same time")
	}
}

// A single timing factor that determines how the full instruction timing is executed.
#TimingFactor: {
	type: (
		// A static timing offset that is always added. Split per cog/hub exec
		"static" |

		// A dynamic timing offset based on the availability of a hub access window
		// for a certain address.
		// Adds a delay of 0..7 clocks based on `condition` and `address`.
		"hub_access_window" |

		// +1 clock if the hub access crosses a hub-long boundary.
		// Documented as footnote "1" in the PASM2 manual timing table
		// for RDLONG/WRLONG/WMLONG/RDWORD/WRWORD/PUSHA/PUSHB/POPA/POPB/....
		// Only possible for byte/word/unaligned accesses.
		// Uses the address+width to compute if the long cross is applied.
		// cog: 1        hub: 1
		"hub_long_cross" )

	// The condition when the timing factor is enabled.
	// If not present, will always apply.
	condition?: #Expression | *"true"

	if (type == "static") {
		// Static cost when executing cog/lut exec mode
		cog: int & >=0

		// Static cost when executing in hub mode
		hub: int & >=0
	}

	if (type == "hub_access_window" || type == "hub_long_cross") {
		// The address that shall be accessed.
		address!: #Expression
	}

	if (type == "hub_long_cross") {
		// The size of the memory access.
		width: #MemorySize
	}
}

// Defines how a single flag is consumed or produced by an instruction.
#FlagSpec: {
	// true if C is consumed as an input by the instruction
	consumed: bool | *false
	// true if C will be updated by the instruction (needs WC or WCZ in allowed_modifiers)
	emitted: bool | *false
	if (emitted == true ) {
		// expression for the new flag value when written
		// this value will be used to update the flag based on the used modifier.
		source!: #Expression
	}
	note?: string | *null
}

#Flags: {
	// Which flag modifiers are allowed on the instruction.
	// These modifiers must match the existence of a  "c-flag" or "z-flag" encoding field.
	allowed_modifiers!: [...#FlagModifier]

	// How is the C flag processed by the instruction
	C?: #FlagSpec | *null

	// How is the Z flag processed by the instruction
	Z?: #FlagSpec | *null
}

#RegisterWriteCondition: (
				// The register is always written
				"always" |
	//  WC or WCZ modifier is present
	"if_wc" |
	//  WZ or WCZ modifier is present
	"if_wz" |
	//  WCZ modifier is present
	"if_wcz" |
	//  D operand is a register (not immediate)
	"if_reg_D" |
	//  S operand is a register (not immediate)
	"if_reg_S" |
	//  D is register AND WC present
	"if_reg_D_and_wc" |
	//  D is register AND WZ present
	"if_reg_D_and_wz" )

#RegisterRead: {
	reg:       #RegisterName
	condition: #Expression | *"always"
}

#RegisterWrite: {
	// The register written by the instruction. 
	// NOTE: "#RegisterName.D" is the R register which can be altered by "ALTR" or "ALTI"
	reg: #RegisterName

	// The condition when the register is written. Defaults to "always".
	condition: #RegisterWriteCondition | *"always"

	// The value that is written to the register.
	value?: #Expression
}

// Defines certain register effects.
//
// NOTE: Pointer expressions are inherent of the model and will be additionally considered
//		 when applicable.
#RegisterEffects: {
	reads: [...#RegisterRead]
	writes: [...#RegisterWrite]
}

#QEffects: {
	sets_q?: {
		mode:  "regular" | "alternative"
		value: #Expression
	}
	reads_q_value:  bool | *false // TODO: Add expected format
	resets_q_state: bool | *false

	q_behavior?: {
		// TODO: Specify better
		unset:       null
		regular:     null
		alternative: null
	}
}

#MemoryAccess: "read" | "write"
#MemorySize:   "byte" | "word" | "long"

#MemoryEffects: {
	hub?: {
		access:  #MemoryAccess
		width:   #MemorySize
		address: #Expression
		if (access == "read") {
			destination: #Expression
		}
		if (access == "write") {
			source: #Expression
		}
	} | *null
	lut?: {
		access:  #MemoryAccess
		address: #Expression
		if (access == "read") {
			destination: #Expression
		}
		if (access == "write") {
			source: #Expression
		}
	} | *null
}

#StackEffects: {
	operation: "push" | "pop"
	if (operation == "push") {
		// Expression describing the value that is pushed
		value!: #Expression
	}
	note?: string
}

#BranchType: (
		// An unconditional jump, always performed. Stores no return information.
		"jump" |

	// A conditional jump, has a condition when to be taken. Stores no return information.
	"branch" |

	// An unconditional call operation, always performed. Stores return information.
	"call")

#Branching: {

	// Defines what kind of branching is performed.
	type: #BranchType

	if (type == "branch") {
		// A boolean expression that describes when the branch is taken.
		condition!: #Expression
	}

	// The expression that describes where the branch will go to
	target: #Expression

	// Is the addressing mode fixed (absolute, relative) or does it depend on
	// the operands (either)?
	addressing: "absolute" | "relative" | "dependent"

	if (type == "call") {
		// Describes the format of the return context and
		// where it is stored.
		return_context!: {
			// originally, there was a "format" field, but all calls use "{C, Z, 10'b0, PC[19:0]}" as the format.

			// Where will the return location be stored?
			// - stack: pushed onto the local stack
			// - D: stored into the D operand (alternatively R)
			location: "stack" | "D" | "PA" | "PB" | "HUB[PTRA]" | "HUB[PTRB]"
		}
	}
}

#Instruction: {
	// unique slug; e.g. calld-reg, calld-imm20
	id!: string & =~"^([a-z][a-z0-9]*)(-[a-z][a-z0-9]*)*$"

	// primary assembler mnemonic, UPPERCASE
	mnemonic!: #Mnemonic

	// Human-readable description of the instruction
	description!: #Description

	family?: #Family

	context?: #Context

	// The raw instruction encoding
	encoding!: #Encoding

	// The list of semantic operands this instruction has.
	// Operands map their values into to fields of `encoding`.
	operands: [...#Operand]

	// Optional list of instruction aliases that have different syntax,
	// but are mapping to the same underlying instruction.
	aliases?: [...#Alias]

	// The timing spec is a ordered sequence of factors that 
	// together form the final timing rules for an instruction.
	timing: [...#TimingFactor]

	// Describes the effects of the instruction to the flag registers.
	flags: #Flags

	// Describes the effects of the instruction to regular registers.
	registers?: #RegisterEffects | *null

	// Describes the effects of the instruction to the Q registers.
	q_effect?: #QEffects | *null

	// Describes the effects of the instruction on memory.
	memory?: #MemoryEffects | *null

	// Describes the effects of the instruction on the internal stack.
	stack?: #StackEffects | *null

	// If set, describes the branching behavior of the instruction.
	branch?: #Branching

	cordic?: {}
	pipeline?: {}
	interrupt_shield?: {}
	events?: {}
	smart_pin?: {}

	// True only for BRK and similar instructions that execute regardless of
	// the EEEE condition-code field. Default false.
	ignores_condition?: bool | *false

	semantics?: {
		program: #SemanticProgram
	}
}

#EnumItem: {
	name:        string
	value:       #Integer
	description: string
}

#Enumeration: {
	size: int
	items: [...#EnumItem]
}

#Schema: {
	// Collection of defined instructions
	instructions: [...#Instruction]

	// Collection of enumeration types available
	enumerations: [string]: #Enumeration
}

#Schema

// Semantic Language:
// 
// Verilog-like subset. NOT Turing-complete.
// Purpose: transfer formal expressions from the specification verbatim,
// not model complex sequencing (REP, CORDIC scheduling).
//
// Named entities:
//
// - <op>: Alias of the operand with .name = "<op>"
// - R: The result register (typically the "D" operand, unless altered in the pipeline)
// - C/Z: The 1-bit flag values
// - PC_curr: The address of the instruction currently executed.
// - PC: Address of the instruction following the current one (PC_curr+1 cog/LUT, PC_curr+4 hub)
// - PA/PB/PTRA/PTRB/...: Value of the named register (#RegisterName)
// - REG[…]: The register memory for the executing cog
// - LUT[…]: The LUT memory for the executing cog
// - HUB[…]: The HUB memory
//
// Functions:
//
// - target(<op>): The resolved branch target (abs or PC+$signed(S) per immediate bit)
// - value_of(<op>): The effective resolved value of the operand
//
// Predicates:
// - $is_imm(<op>): true if operand "<op>" is an immediate
// - $is_reg(<op>): !$is_imm(<op>)
// - $is_ptrexpr(val): Returns true if "val" is a pointer expression
// - $is_ptrexpr(val, PTRA): Returns true if "val" is a pointer expression using PTRA
// - $is_ptrexpr(val, PTRB): Returns true if "val" is a pointer expression using PTRB
// - $is_hub_pc(val): Returns true if "val" is a hub address (val >= 0x400)
//
// Expressions:
// - value[index]: Indexes the array "value" at element "index" or the bit "index" of value.
// - value[msb:lsb]: Fetches the bit sequence from MSB (inclusive) to LSB (inclusive)
