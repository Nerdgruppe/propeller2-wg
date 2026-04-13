package p2db

// Primitive types:
#Mnemonic: string & =~"[A-Z]+"

#Family: {
	// Root name of the family, e.g. "jse_jnse"
	name: string

	// Index inside the family, must be unique for each family.
	index: int & >=1
}

#Context: {
	group: string //  display group
	tags?: [string] | *[] // machine-searchable labels
	related?: [string] | *[] //  ids of closely related instructions
}

#Description: {
	short: string & !~"\n" // One-liner description, plaintext
	full:  string          // Markdown, multi-paragraph
}

#InstrFieldEnum: {
	value:   string & =~"^[01]+$"
	meaning: string
}

#InstrField: {
    // the character that is used to encode the field in "pattern"
	name:        string & =~ "^[A-Za-z]$"

	description: string

    // optional named enumeration
	values?: [...#InstrFieldEnum] | *[]
}

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

// Aliases are syntactic shorthands: some programmer-visible operands of the
// canonical instruction are replaced by fixed bit values. The remaining
// operands (if any) keep their usual roles.
//
// An alias does NOT get a root-level entry.
#Alias: {
	mnemonic:    #Mnemonic
	description: #Description
    
    // Keys point to #InstrField.name
	fixed_fields: [string]: int // TODO: Also allow binary/hexadecimal, constrain to parent encoding fields
}

#Instruction: {
	// unique slug; e.g. calld-reg, calld-imm20
	id!: string & =~"([a-z][a-z0-9]*)(-[a-z][a-z0-9]*)*"

	// primary assembler mnemonic, UPPERCASE
	mnemonic!: #Mnemonic

	family?: #Family

	context?: #Context

	description: #Description

	encoding: #Encoding

	aliases: [...#Alias]

	// True only for BRK and similar instructions that execute regardless of
	// the EEEE condition-code field. Default false.
	ignores_condition?: bool | *false

    ...
}

instructions: [...#Instruction]
