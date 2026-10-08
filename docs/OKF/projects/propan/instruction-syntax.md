---
type: "Reference"
title: "Propan instruction syntax"
description: "Current instruction grammar, conditions, effects, operand categories, and instruction-variant selection."
tags: ["propan", "instructions", "conditions", "effects", "operands", "pasm2"]
status: "draft"
source_confidence: "high"
---
# Propan instruction syntax

This page describes the source syntax and semantic selection model used for generated P2 instructions. Assembler directives share the parser's instruction-shaped line form but have separate semantics documented in [/projects/propan/directives-and-data.md](/projects/propan/directives-and-data.md).

## Instruction grammar

A generated P2 instruction has one of these forms:

```text
<mnemonic> [<operand> [, <operand> ...]] [<effect>]
if(<condition>) <mnemonic> [<operand> [, <operand> ...]] [<effect>]
return <mnemonic> [<operand> [, <operand> ...]] [<effect>]
```

Examples:

```propan
ADD DIRA, 1
ADD DIRA, 1 :wc
if(!Z) ADD DIRA, 1 :wcz
return MOV DIRA, 0
```

The mnemonic is parsed as an identifier and looked up case-insensitively during semantic analysis. Operand expressions use the ordinary Propan expression grammar. An effect, when present, is a final `:name` token after the operands.

A single linefeed may follow an operand comma, allowing an instruction argument list to continue onto the next physical line.

The parser does not decide whether a mnemonic, operand shape, or effect combination exists. Semantic analysis resolves the mnemonic against the generated instruction table and then selects a matching encoded variant.

## Conditions

Conditions are written before the mnemonic. The parser provides two source forms:

- `if(<condition>)` for flag-based conditions;
- `return` for P2 condition code `0000`.

A normal instruction with no explicit condition uses its generated default condition, which is `1111` (always) unless the instruction metadata or an implementation special case says otherwise.

`C` and `Z` are matched case-insensitively in condition expressions.

### Condition table

| P2 code | Propan source | Meaning of flag expression |
|---:|---|---|
| `0000` | `return` | return-condition encoding |
| `0001` | `if(!C & !Z)` or `if(>)` | C=0 and Z=0 |
| `0010` | `if(!C & Z)` | C=0 and Z=1 |
| `0011` | `if(!C)` or `if(>=)` | C=0 |
| `0100` | `if(C & !Z)` | C=1 and Z=0 |
| `0101` | `if(!Z)` or `if(!=)` | Z=0 |
| `0110` | `if(C != Z)` | C and Z differ |
| `0111` | `if(!C | !Z)` | C=0 or Z=0 |
| `1000` | `if(C & Z)` | C=1 and Z=1 |
| `1001` | `if(C == Z)` | C and Z are equal |
| `1010` | `if(Z)` or `if(==)` | Z=1 |
| `1011` | `if(!C | Z)` | C=0 or Z=1 |
| `1100` | `if(C)` or `if(<)` | C=1 |
| `1101` | `if(C | !Z)` | C=1 or Z=0 |
| `1110` | `if(C | Z)` or `if(<=)` | C=1 or Z=1 |
| `1111` | no condition | always/default for ordinary generated instructions |

For the two-flag forms, `C` and `Z` may appear in either order. The two flags must be different: forms such as `if(C & C)` are not accepted. Negation is allowed on either side of `&` and `|`.

For flag equality/inequality, the accepted forms are `C == Z`, `Z == C`, `C != Z`, and `Z != C`. Negated operands are not accepted in these equality forms.

The comparison shorthands inside `if(...)` are condition syntax, not general comparison expressions with omitted operands. They map directly to the condition codes shown above.

### `NOP` condition special case

The generated P2 table records `NOP` as the all-zero word:

```text
0000 0000000 000 000000000 000000000
```

Emission therefore special-cases a source `NOP` with no explicit condition to condition code `0000`; otherwise the ordinary instruction path would replace its top condition field. The implementation contains a TODO to represent this special behavior in instruction metadata instead.

An explicitly conditioned `NOP` currently bypasses that protection. For example `if(Z) NOP` writes the requested nonzero condition code into the high four bits while leaving the lower 28 bits zero.

That resulting word is **not a NOP encoding**. The canonical/generated `ROR D,{#}S` instruction uses the same zero lower opcode bits (`EEEE 0000000 CZI DDDDDDDDD SSSSSSSSS`). With C/Z/I, D, and S all zero, a nonzero condition field therefore represents a conditionally executed `ROR r0, r0`-form word rather than the special all-zero NOP.

Do not use conditions on `NOP` in current Propan source. Parser acceptance is an implementation defect/encoding hazard, not supported conditional-NOP semantics. The finding is tracked in [/projects/propan/implementation-findings.md](/projects/propan/implementation-findings.md).

## Effects

Effects are parsed case-insensitively. The following source spellings are currently recognized:

| Canonical spelling used here | Accepted aliases | Semantic effect |
|---|---|---|
| `:wc` | — | write/update C according to the selected instruction variant |
| `:wz` | — | write/update Z according to the selected instruction variant |
| `:wcz` | `:wzc` | write/update both C and Z |
| `:and_c` | `:andc` | TEST-family C-combine effect |
| `:and_z` | `:andz` | TEST-family Z-combine effect |
| `:or_c` | `:orc` | TEST-family C-combine effect |
| `:or_z` | `:orz` | TEST-family Z-combine effect |
| `:xor_c` | `:xorc` | TEST-family C-combine effect |
| `:xor_z` | `:xorz` | TEST-family Z-combine effect |

Unknown effect names are rejected by the parser.

Recognizing an effect name does **not** mean every instruction accepts it. Each generated instruction variant carries an effect set, and semantic selection rejects variants whose effect set does not contain the requested effect.

### Effect classes in the generated P2 table

The main patterns are:

- ordinary flag-writing instruction variants commonly allow no effect plus `:wc`, `:wz`, and `:wcz`;
- instructions with no flag-write form allow no effect only;
- `TESTB`, `TESTBN`, `TESTP`, and `TESTPN` have effect-selected variants for `:wc`/`:wz`, `:and_c`/`:and_z`, `:or_c`/`:or_z`, and `:xor_c`/`:xor_z`; these generated variants require an appropriate effect rather than accepting an effect-free form;
- other restricted subsets are represented directly by the generated per-variant effect metadata.

The generated instruction table is the authority for the exact allowed effect set of a particular mnemonic/operand variant. A manually duplicated per-instruction effect matrix should not be maintained separately.

## Propan operand categories

Generated instruction variants describe operands using semantic categories derived from PASM2 operand fields. Propan source does not reproduce PASM2's punctuation literally; an expression's value category and usage hint determine which generated operand category it can satisfy.

| Internal category | PASM2-style role | Propan source behavior |
|---|---|---|
| `register` | `D` or `S` register field | requires register usage |
| `immediate` | absolute `#D`, `#S`, or `#N` | requires literal usage; optional generated right shift supports fields such as AUGS/AUGD high bits |
| `reg_or_imm` | `{#}D` or `{#}S` | accepts either register or literal usage and drives the instruction's immediate-select bit |
| `address` | `#{\}A` address field | literal address/value with automatic relative/absolute selection unless overridden |
| `pointer_expr` | `#N` or PTRA/PTRB pointer-expression encoding | accepts an encoded pointer expression and, where permitted, literal/register-compatible forms |
| `pointer_reg` | PA, PB, PTRA, or PTRB selector | accepts only those four special registers |
| `enumeration` | named finite operand choice | requires a `#name` enumerator present in that operand's generated lookup table |

Strings cannot satisfy encoded instruction operands.

### Literal versus register usage

Propan does not use PASM2 `#` as a general immediate marker. `#name` is reserved for enumerators.

Instead, expressions carry a usage hint:

- integer literals and ordinary integer results are literal values;
- builtin register names are register values;
- code labels default to literal/address usage;
- `var`/data labels default to register usage;
- `&address` requests literal usage;
- `*address` requests register usage.

This usage hint is part of instruction-variant selection. For example, a generated `register` operand rejects a literal-use value, while `reg_or_imm` accepts either and encodes the corresponding immediate-selection bit.

## Enumerated operands

An enumerator is written as:

```propan
#name
```

The tokenizer permits identifier-suffix characters after `#`, including a leading digit, so forms such as `#15pF` can be valid enumerator tokens.

For encoded instruction operands of type `enumeration`, the selected generated variant provides a lookup table mapping legal names to encoded numeric values. A name that is not present in that operand's table is rejected.

Enumerators are also used by typed standard-library function parameters. Their complete target-specific namespaces and function use sites are covered by the standard-library reference; they are not global integer symbols.

## Address operands and automatic relative selection

For generated operands categorized as `address`, the current automatic mode behaves as follows.

For a label/address value:

1. target in the same segment → relative;
2. target in another segment where both source and target are HUB-exec → relative by the default analyzer option;
3. other cross-segment/cross-domain cases → absolute.

For a non-label numeric value, the encoder infers a target execution domain from the numeric range (`0x000...0x1FF` COG, `0x200...0x3FF` LUT, otherwise HUB). A target inferred to be in the current execution mode is relative by the default analyzer option; a target in another mode is absolute.

`nrel(value)` forces absolute addressing and must be the root expression.

For a relative `address` operand, the current encoder calculates a signed HUB-byte displacement from the PC after the instruction and any augmentation prefixes. The displacement must fit the encoded address field.

Some generated register-or-immediate operands have separate PC-relative metadata and use `compute_rel()` rather than the `address` path. CALLD/PC-relative details are documented in [/projects/propan/pointer-addressing.md](/projects/propan/pointer-addressing.md).

## Augmented operands

`aug(value)` marks the root operand expression for augmentation.

During layout, each top-level augmented operand increases the encoded instruction size by four bytes. During emission, augmentation is only supported for operand fields corresponding to the generated `D` or `S` slots:

- a D-slot augmentation emits an AUGD prefix;
- an S-slot augmentation emits an AUGS prefix;
- the high 23 bits are placed in the augmentation instruction and the low operand bits remain in the main instruction.

Attempting to augment an operand whose generated slot is not D or S is diagnosed.

PC-relative register-or-immediate operands have additional augmentation ordering and displacement behavior documented with pointer/branch selection.

## Variant selection

For a recognized generated mnemonic, semantic analysis currently selects an encoding in this order:

1. collect variants with the same operand count;
2. discard variants whose operand categories cannot accept the evaluated argument values/usage hints;
3. discard variants whose allowed effect set does not match the supplied effect or lack of effect;
4. if one variant remains, select it;
5. if multiple variants remain, apply special pointer-register preference logic; otherwise report ambiguous instruction selection.

When the selected operand category is `pointer_expr`, a bare PTRA/PTRB register value is converted to the corresponding no-update pointer expression under the default analyzer option.

The current pointer-register ambiguity preference contains a known copy/paste defect that can panic instead of reliably resolving competing variants. See [/projects/propan/implementation-findings.md](/projects/propan/implementation-findings.md).

## Conditions/effects on assembler directives

Assembler directives are parsed using the same instruction-shaped syntax, so source such as `if(C) BYTE 1` or `.assert 1 :wc` can reach the directive semantic path.

Current directive processing does not validate or apply the parsed condition/effect fields. They are effectively ignored for directives. This is an implementation limitation; conditions and effects should be treated as meaningful syntax only for generated P2 instructions until the assembler rejects or defines directive modifiers explicitly.

## Instruction-reference generation policy

The complete P2 instruction set should be documented from generated/verifiable data rather than maintained as a second handwritten 400+ instruction table.

Current implementation flow:

- the assembler consumes generated instruction definitions under `src/propan/stdlib/p2/`;
- `src/propan/stdlib/p2/instructions.zig` records operand categories, effects, binary masks/slots, and generated mnemonic variants;
- `utility/gen_propan_instructions.zig.py` is the generation path;
- the repository-pinned Parallax Propeller 2 PASM Instructions v35 Rev B/C Silicon sheet is the canonical source for exact PASM2 syntax and encoding when generated material is checked against official instruction semantics.

A future generated reference can expose the exact accepted variants without duplicating that data manually.
