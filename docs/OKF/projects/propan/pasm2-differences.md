---
type: "Reference"
title: "PASM2 to Propan differences"
description: "Compact migration reference for syntax and semantic differences between canonical PASM2 notation and current Propan source."
tags: ["propan", "pasm2", "migration", "syntax", "differences"]
status: "draft"
source_confidence: "high"
---
# PASM2 to Propan differences

Propan targets the P2 instruction set, but it is not a source-compatible PASM2/Spin2 assembler syntax. The generated instruction database preserves the P2 mnemonic, operand order, encoded operand roles, flags/effects, and binary fields from the repository's canonical instruction data. Propan then maps those roles onto its own expression language, condition/effect syntax, address model, and assembler directives.

Use the canonical P2 instruction reference to answer hardware questions such as instruction meaning, field encoding, timing, aliases, and side effects. Use the Propan reference to answer how that instruction is written and selected in Propan source.

## Compact migration table

| Canonical PASM2 concept/notation | Current Propan form | Migration consequence |
|---|---|---|
| instruction mnemonic and operand order | same generated mnemonic/order | normally keep mnemonic and operand ordering |
| `#` immediate marker | no general immediate prefix | literal/register usage is derived from expression value and `&`/`*`; do not mechanically copy `#` |
| named operand selector such as `#name` in generated finite-choice fields | `#name` enumerator | `#` is reserved for enumerator values, not ordinary immediates |
| condition prefix such as `IF_Z`, `IF_NC`, etc. | `if(...)` or `return` | rewrite the condition as a C/Z boolean expression or comparison shorthand |
| flag suffixes such as `WC`, `WZ`, `WCZ` | `:wc`, `:wz`, `:wcz` | effects are colon-prefixed final tokens |
| TEST-family `ANDC`, `ANDZ`, `ORC`, etc. | `:and_c`, `:and_z`, `:or_c`, etc. | compact aliases such as `:andc` are also currently accepted |
| relative/absolute `#{\}A` notation | ordinary address expression with automatic selection | use `nrel(value)` to force absolute mode; relative mode is selected automatically where legal |
| augmented immediate using PASM2 augmentation conventions | `aug(value)` | augmentation is an expression wrapper and emits AUGD/AUGS according to generated operand slot |
| PTRA/PTRB pointer syntax | PTRA/PTRB with `[]`, prefix/postfix `++`/`--` | spelling is similar, but it evaluates to a dedicated `pointer_expr` value in Propan |
| assembler origin/fit/reservation directives from Spin2/PASM2 toolchains | not general current aliases | use the implemented `.hubexec`, `.cogexec`, `.lutexec`, `.align`, `.assert`, `BYTE`, `WORD`, `LONG` set; unsupported legacy names must not be assumed |
| Spin/PASM-style expression/operator spellings | Propan expression grammar | rewrite to current C-like/word operators; external comparison tables do not imply parser support |
| PASM/Spin local-label conventions | ordinary Propan identifiers only | no special local-label scope is currently implemented |
| source inclusion/object model | multiple CLI input modules | there is no current source-level include/import construct |

## Instructions: what is inherited directly

The generated Propan P2 instruction table is produced from the repository's decoded P2 instruction data. For each generated instruction it carries:

- the canonical mnemonic;
- the canonical operand sequence;
- the encoded field/slot for each operand;
- the immediate/relative selector fields associated with those operands;
- the legal effect set;
- the base binary encoding.

Consequently Propan does not intentionally reorder ordinary generated instruction operands. Differences are primarily in how the source expression is classified and how condition/effect/address controls are written.

This inheritance does **not** mean every canonical textual spelling is accepted. Canonical punctuation such as a general `#` immediate marker is translated into Propan's semantic operand model rather than parsed literally.

## Immediates and registers

The largest accidental carry-over from PASM2 is the immediate marker.

In Propan:

```propan
MOV DIRA, 1
MOV DIRA, value
```

An integer literal/result is a literal-use value. A predefined hardware-register symbol is a register-use value. Labels have context-sensitive defaults: code labels are literal/address use and `var`/data labels are register use.

The unary operators change this usage hint for address values:

```propan
&data_label   // request literal/address use
*code_label   // request register/local use
```

Therefore do not translate PASM2 `#expr` to Propan `#expr`. In Propan, `#name` creates an **enumerator** value for a finite named operand/function parameter.

## Conditions

Canonical PASM2 condition names are represented by Propan expressions over C and Z. Examples:

| P2 condition meaning | Propan |
|---|---|
| always | omit condition |
| Z set | `if(Z)` or `if(==)` |
| Z clear | `if(!Z)` or `if(!=)` |
| C set | `if(C)` or `if(<)` |
| C clear | `if(!C)` or `if(>=)` |
| C=0 and Z=0 | `if(!C & !Z)` or `if(>)` |
| C=1 or Z=1 | `if(C | Z)` or `if(<=)` |
| return-condition encoding `0000` | `return` |

The complete code table is in [/projects/propan/instruction-syntax.md](/projects/propan/instruction-syntax.md).

Do not carry PASM2 `IF_*` tokens into Propan source; they are not the current condition grammar.

## Effects

Effects are final colon-prefixed tokens:

```propan
ADD value, 1 :wc
ADD value, 1 :wz
ADD value, 1 :wcz
```

TEST-family combine effects use `:and_c`, `:and_z`, `:or_c`, `:or_z`, `:xor_c`, and `:xor_z`; compact aliases without the underscore are also accepted.

Effect availability is still per generated instruction variant. A recognized effect spelling does not make it legal for every instruction.

## Addresses and branches

Propan labels are tagged addresses carrying HUB location, segment identity, and execution-local domain. Branch/call operands therefore do not rely only on the textual PASM2 immediate/relative marker.

For generated `A` address operands, automatic selection chooses relative or absolute encoding according to target/source segment and execution mode. `nrel(value)` forces absolute addressing.

Generated P2 `S**` PC-relative operands are a different operand class. Their relative displacement is encoded through the source field and should not be conflated with `A` address operands.

`aug(value)` requests augmentation at the Propan expression level. For a selected D or S slot it emits AUGD or AUGS respectively.

CALLD is a notable migration hazard because P2 has overlapping regular and special PA/PB/PTRA/PTRB forms. The current Propan selector has a known ambiguity-guard defect for some overlaps; see [/projects/propan/pointer-addressing.md](/projects/propan/pointer-addressing.md) and [/projects/propan/implementation-findings.md](/projects/propan/implementation-findings.md).

## Pointer addressing

The familiar PTRA/PTRB spellings are retained:

```propan
PTRA
PTRA[5]
PTRA[-3]
PTRA++
++PTRA
PTRB--
--PTRB
PTRA++[4]
```

The semantic result is not general arithmetic: these forms become a dedicated encoded pointer expression.

Current encoded limits are:

- non-updating index: `-32...31`;
- updating magnitude: `1...16`.

Pre-update modifies the pointer before the memory access; post-update modifies it after the access. See the dedicated pointer page for exact forms and alias evidence.

## Directives and layout

Do not assume the directive vocabulary of another P2 assembler exists in Propan.

Current assembler-defined names are:

```text
BYTE WORD LONG
.align .assert
.cogexec .lutexec .hubexec
```

Names appearing in old fixtures/design material but not registered by current semantic analysis include `.org`, `.huborg`, `.cogorg`, `.lutorg`, `.reserve`, `.regspace`, `.data`, `.regs`, `.RES`, `.fit`, `.cogfit`, and `.section`.

The execution-mode directives start new tagged segments and optionally move the HUB emission cursor. They are not aliases for every origin/reservation concept found in Spin2/PASM2 assemblers.

## Expressions and literals

Propan has its own expression grammar. Important migration points include:

- numeric prefixes are `0b`, `0q`, `0o`, and `0x`, plus decimal;
- `and`, `or`, `xor` are logical operators;
- `&`, `|`, `^`, `~` are bitwise operators;
- `!` is boolean negation;
- comparison operators use C-like spellings plus `<=>`;
- current parser does not implement the externally advertised `?:` ternary expression;
- `++`/`--` and indexing are pointer-expression syntax, not general integer mutation/array access;
- `@`, `&`, and `*` have Propan address/usage semantics and should not be inferred from similarly named Spin operators.

See [/projects/propan/expressions.md](/projects/propan/expressions.md) for precedence and exact semantics.

## Labels and source structure

Propan currently uses one ordinary identifier syntax for labels. There is no special local-label scope analogous to common PASM/Spin local-label conventions.

Multiple CLI source inputs are analyzed as independent modules and overlaid into the common output by HUB address. There is no current source-level include/import form and no cross-module symbol import/linking mechanism.

## Migration checklist

When porting a PASM2 fragment:

1. keep the P2 mnemonic and canonical operand order unless the Propan generated table proves otherwise;
2. remove general PASM2 `#` immediate markers and express literal/register intent through Propan values plus `&`/`*` where needed;
3. convert condition names to `if(...)`/`return`;
4. convert flag/effect suffixes to `:effect` syntax;
5. review branch/call targets for Propan automatic relative selection, `nrel()`, and `aug()`;
6. verify PTRA/PTRB expressions against Propan's pointer-expression rules;
7. replace assembler layout/origin directives with the currently implemented Propan directive model rather than guessing aliases;
8. rewrite Spin/PASM expression syntax into the current Propan expression grammar;
9. check old/local label conventions and multi-file assumptions explicitly.

For exact P2 instruction syntax/encoding itself, consult the canonical P2 instruction data. This page only records the source-language translation layer introduced by Propan.
