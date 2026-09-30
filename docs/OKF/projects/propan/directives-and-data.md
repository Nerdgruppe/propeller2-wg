---
type: "Reference"
title: "Propan directives and data"
description: "Current assembler-defined directives and data-emission semantics."
tags: ["propan", "directives", "data", "alignment", "assertions"]
status: "draft"
source_confidence: "high"
---
# Propan directives and data

The current semantic mnemonic table defines these assembler directives/pseudo-instructions in addition to generated P2 instructions:

| Name | Arguments | Emits bytes | Purpose |
|---|---:|---:|---|
| `BYTE` | zero or more expressions | 1 per argument | emit 8-bit values |
| `WORD` | zero or more expressions | 2 per argument | emit 16-bit values |
| `LONG` | zero or more expressions | 4 per argument | emit 32-bit values |
| `.align` | exactly 1 expression | padding only | advance to a power-of-two HUB alignment |
| `.assert` | 1 condition, optional message | no | assembly-time assertion |
| `.cogexec` | 0 or 1 expression | no | start a COG-exec segment |
| `.lutexec` | 0 or 1 expression | no | start a LUT-exec segment |
| `.hubexec` | 0 or 1 expression | no | start a HUB-exec segment |

Mnemonic lookup is case-insensitive, so `long`, `LONG`, and mixed-case spellings resolve to the same directive.

### Conditions and effects are not directive modifiers

The parser uses the same instruction-shaped line form for directives and generated P2 instructions, so it can attach an `if(...)`/`return` condition or a recognized `:effect` token to a directive AST node.

Current directive semantic paths do not validate or apply those fields. For example, a condition on `BYTE` does not make the emitted byte conditional, and an effect on `.assert` has no assertion meaning. Such modifiers are currently ignored rather than rejected.

Do not use conditions/effects on assembler directives. The implementation finding is tracked in [/projects/propan/implementation-findings.md](/projects/propan/implementation-findings.md); generated-instruction condition/effect syntax is documented in [/projects/propan/instruction-syntax.md](/projects/propan/instruction-syntax.md).

## Data emission: `BYTE`, `WORD`, `LONG`

General form:

```propan
BYTE expr, expr, ...
WORD expr, expr, ...
LONG expr, expr, ...
```

Zero arguments are currently accepted and emit no bytes.

Each argument is evaluated independently and narrowed to the directive width:

- `BYTE` → `u8`;
- `WORD` → `u16`;
- `LONG` → `u32`.

Negative and oversized integer values are truncated to the destination width. The assembler emits a warning when the narrowed result differs from the original value.

Values are written **little-endian**.

### Address values

Address expressions are converted using the current segment's execution mode before being narrowed. This means a bare label in a data directive can produce its execution-local address rather than necessarily its HUB byte address.

Use explicit helpers such as `hubaddr(label)`, `cogaddr(label)`, or `lutaddr(label)` when the intended address domain must be unambiguous.

### Strings are not currently data emitters

Although string expressions exist in the language, `BYTE "text"`, `WORD "text"`, and `LONG "text"` do not currently expand a string into data. The generic emission cast reaches a `string emission not supported yet` panic for string values.

This is an implementation limitation, not supported string-data syntax.

### Enumerators in raw data directives

Enumerator values such as `#name` are normally resolved by instruction operands with an enumeration table. The generic data-emission cast treats an unresolved enumerator as an internal error path rather than a normal data value. Do not use enumerators directly with `BYTE`/`WORD`/`LONG` unless they have first been converted by some supported expression/function context.

## `.align`

Form:

```propan
.align alignment
```

The directive expects exactly one argument. The evaluated value must:

- be an integer;
- fit in `u20`;
- be nonzero;
- be a power of two.

The HUB cursor advances forward to the next address divisible by the requested alignment. `.align` itself has zero logical instruction size; when emission later encounters the gap, the assembler fills the skipped HUB bytes with `0xFF`.

In COG/LUT execution modes, the corresponding local long/register address is advanced consistently with the HUB movement.

### Current evaluation-phase limitation

`.align` is evaluated during location assignment. User-defined constants are evaluated only after location assignment, so an expression such as:

```propan
const boundary = 16
.align boundary
```

cannot currently use `boundary`, even though the symbol has already been declared. Builtin constants whose values were loaded before layout are available.

The current `.align` error path describes `UndefinedSymbol` as `cannot refer to labels in .align`; that message can therefore also be misleading for an unavailable user constant.

## `.assert`

Forms:

```propan
.assert condition
.assert condition, "message"
```

The condition must evaluate to an integer. Zero fails the assertion; any nonzero integer passes.

On failure:

- with no message, the assembler emits `assertion failed: expression returned 0`, with additional relation-specific formatting for several comparison expressions;
- with a second argument, that value must be a string and becomes the assertion message.

The second argument's string type is checked only on the failing path. If the condition is nonzero, semantic analysis returns before validating the message type.

Zero operands are rejected. More than two operands produce an arity error.

`.assert` emits no output bytes.

A known diagnostic defect causes the wrong value type to be named when a failing assertion has a non-string message; see [/projects/propan/implementation-findings.md](/projects/propan/implementation-findings.md).

## Execution-mode directives

### `.cogexec [hub_address]`

Starts a new COG-exec segment with local PC `0x000`. With no argument, HUB emission continues from the current HUB cursor. With one argument, the supplied value selects the segment's HUB emission address.

### `.lutexec [hub_address]`

Starts a new LUT-exec segment with local PC `0x200`, using either the current HUB cursor or an explicitly supplied HUB address.

### `.hubexec [hub_address]`

Starts a new HUB-exec segment. The local execution address is the HUB address itself.

All three accept zero or one operand; more operands are diagnosed.

Explicit HUB-address operands are evaluated during location assignment, so they share the same phase limitation as `.align`: ordinary user-defined constants do not yet have values at that point.

Moving an explicit HUB address backward emits a warning. Segment-overlap rejection is currently disabled; see [/projects/propan/addresses-and-segments.md](/projects/propan/addresses-and-segments.md).

## Names present elsewhere but not implemented as current directives

The current semantic mnemonic table does **not** register the following names found in parser fixtures, examples, or historical/design documentation:

- `.org`
- `.huborg`
- `.cogorg`
- `.lutorg`
- `.reserve`
- `.regspace`
- `.data`
- `.regs`
- `.RES`
- `.fit`
- `.cogfit`
- `.section`

Parser acceptance of an identifier in mnemonic position does not make it an implemented directive. These names remain tracked in [/projects/propan/documentation-discrepancies.md](/projects/propan/documentation-discrepancies.md).

No current compatibility aliases for the implemented directive set have been established by semantic analysis. For new code, use the spellings listed in this page.

## Compact behavior summary

| Directive | Cursor effect | Output behavior | Important failure/limitation |
|---|---|---|---|
| `BYTE` | +1 byte/arg | little-endian byte values | strings/enumerators are not supported raw data values |
| `WORD` | +2 bytes/arg | little-endian 16-bit values | narrowing can warn |
| `LONG` | +4 bytes/arg | little-endian 32-bit values | address meaning depends on exec mode unless converted explicitly |
| `.align` | forward to aligned HUB address | gap filled with `0xFF` | power-of-two `u20`; user constants unavailable during layout |
| `.assert` | none | none | integer condition; optional string checked on failure |
| `.cogexec` | new segment, COG local `0` | subsequent bytes at selected HUB cursor | 0/1 arg; user constants unavailable for explicit HUB address |
| `.lutexec` | new segment, LUT local `0x200` | subsequent bytes at selected HUB cursor | same |
| `.hubexec` | new segment, HUB-local address | subsequent bytes at selected HUB cursor | same |
