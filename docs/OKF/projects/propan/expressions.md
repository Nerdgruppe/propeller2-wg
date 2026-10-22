---
type: "Reference"
title: "Propan expressions and values"
description: "Merged Propan semantic values, expression operators, sequence/repetition behavior and address helpers."
tags: ["propan", "expressions", "operators", "values", "sequences", "semantics"]
status: "draft"
source_confidence: "high"
---
# Propan expressions and values

Verified for the 2026-10-15 merged implementation using `src/propan/frontend/parser.zig`, `src/propan/sema.zig`, `src/propan/stdlib/eval.zig`, and current `tests/propan/sema/` fixtures. Source grammar, conditions and numeric literal syntax are covered in [/projects/propan/lexical-and-source-grammar.md](/projects/propan/lexical-and-source-grammar.md).

## Semantic value types

| Payload | Meaning |
|---|---|
| `int` | signed 64-bit integer |
| `string` | UTF-8/source-escaped byte sequence |
| `sequence` | ordered sequence of signed 64-bit integer elements |
| `address` | segment-tagged local and/or HUB address |
| `register` | P2 register index |
| `enumerator` | `#name`, resolved by a receiving instruction/function's enum domain |
| `pointer_expr` | encoded PTRA/PTRB memory addressing with optional update/index |

Value flags independently record literal/register usage, augmentation and absolute/relative/automatic selection. They are not separate payloads. Boolean operators produce integer 0 or 1; there is no separate user-visible boolean type.

## Integers and arithmetic

Unsigned literal magnitude is limited to `u63`; unary minus is applied during semantic evaluation. Integer operators generally use signed `i64` values:

- `+`, `-`, `*`: wrapping arithmetic (including unary minus);
- `/`: floor division (`@divFloor`), not truncation toward zero; division by zero and the `minInt(i64) / -1` overflow case are errors;
- `%`: `@mod` rather than truncating signed remainder; division by zero is an error;
- `<<`/`>>`: shift count must fit `u6`, 0–63;
- logical/comparison results: 0 or 1, except `<=>` returning -1, 0, or 1.

These are **current implementation facts**; whether floor/mod semantics for negative operands should remain the language contract is still an open design decision. Width narrowing during field/data encoding can warn.

## Operator precedence

The current parser groups left-associative binary operators as follows, from lowest to highest:

| Level | Operators |
|---:|---|
| 0 | `and`, `or`, `xor` |
| 1 | `==`, `!=`, `<=>`, `<`, `>`, `<=`, `>=` |
| 2 | `+`, `-`, `|`, `^` |
| 3 | `&`, `*`, `/`, `%` |
| 4 | `<<`, `>>` |

Unary `+`, `-`, `!` and `~` operate on integers. Unary `*`/`&` change address usage to register/literal. `++`/`--` are pointer-update operations, not general integer increment/decrement. Parentheses override precedence. There is no implemented ternary `?:` production in this parser.

## Sequences, strings and repetition

Bracket literals are **data sequences**, not pointer indexing:

```propan
const WORDS = [1, 2, 3]
const COPIES = WORDS * 2
const TEXT = "Hi"
BYTE "NaN" * 2
WORD utf16("A")
LONG utf32("A")
```

In a bracket sequence, each integer is appended, each string expands to its constituent **bytes**, nested sequences flatten, and an address element is converted in the active data/segment context. Register, pointer and enumerator elements are not unrestricted sequence values and are diagnosed where invalid. `[]` is an empty sequence. A resulting sequence is a semantic value and can be stored in a user constant.

The multiplication operator has an additional **repetition** meaning only where one operand is a string or sequence and the other an integer, in either order. A negative count is invalid, zero counts warn, and expanded sizes are bounded. All other binary operations on string/sequence payloads are diagnosed rather than treated as concatenation, comparison, or general vector arithmetic.

These values are chiefly useful in `BYTE`, `WORD`, `LONG` data declarations, which emit **one element per string byte or integer sequence element** at the selected width. `WORD "AB"` therefore emits two 16-bit values rather than packing two bytes into one word. The length must be known when the assembler lays out data; deferred unexpected lengths produce diagnostics.

P2 library functions `utf16(text)` and `utf32(text)` decode UTF-8 into Unicode code-unit integer sequences. `utf16` emits a surrogate pair for a non-BMP code point; `utf32` emits the code point once. Invalid UTF-8 is an error. See `tests/propan/sema/array-emission.propan`, `array-constants.propan` and `string-emission.propan`.

This syntax is distinct from `PTRA[index]`: postfix indexing on PTRA/PTRB constructs an encoded pointer operand; it does not index sequence contents. See [/projects/propan/pointer-addressing.md](/projects/propan/pointer-addressing.md).

## Current location and address usage

A standalone `$` is the **current location** expression: COG/LUT mode yields the local long/PC index; HUB/data yields a HUB byte offset. Register-only mode has no execution location for `$` and diagnoses its use. Ordinary constant declarations are location-independent.

`*label` requests register-oriented usage, while `&label` requests literal-oriented usage. Neither changes the label's tagged address. Applying an already-effective hint can warn.

`@label` produces a displacement in longs from the **following/current evaluation PC** to the destination within an execution domain. The merged evaluator checks compatible native address domains, aligned 4-byte displacement and presence of execution PCs; crossing different segments emits a warning. It is no longer accurate to describe it as unconditionally subtracting two HUB addresses without any segment check. Register-only and data-only labels cannot provide an execution PC.

### Semantic address and encoding helpers

| Function | Implemented meaning/limitation |
|---|---|
| `hubaddr(x)` | HUB byte address, requires a tagged address with HUB storage; integer input passes through with a warning |
| `cogaddr(x)` | COG/regspace local register address; rejects a LUT/HUB/data address |
| `lutaddr(x)` | LUT-local address; rejects register values or wrong address domains |
| `localaddr(x)` | native data/local address; register input is permitted **inside COG-exec scope only** and warns that a register was supplied rather than an offset |
| `pcaddr(x)` | executable PC representation; rejects addresses with no execution PC (including data/regspace); HUB addresses at or below `0x400` are diagnosed as unreachable for this helper; COG-scope register inputs are permitted with a warning |
| `byteoffset(x)` | sub-register byte offset (0–3) of COG/LUT/regspace tagged address; rejects HUB/data and non-address payloads |
| `wordoffset(x)` | sub-register word offset (0–1) of COG/LUT/regspace tagged address; requires an even byte offset |
| `aug(x)` | request immediate augmentation in the appropriate instruction operand position; only permitted at root and with suitable operand values |
| `nrel(x)` | prefer absolute rather than automatically relative address selection; root-only, meaningful for operand forms supporting this choice |

Sub-register offsets depend on layout and `.pack`. Address functions never magically allocate/register or relocate storage. When using `localaddr(register)` outside COG-exec, the evaluator emits an error; this is not an unconditional failure in every mode.

The generated instruction selector and operand class decide which address flags have an encoding effect; `nrel` does not make a P2 `S**` immediate absolute.

## Diagnostics and evidence

Invalid combinations of two addresses, two strings, or two sequences in ordinary binary operators now result in **source diagnostics** (`err_operator_invalid_operand_type`), not the pre-merge `@panic` paths. Mixed operand types likewise have dedicated diagnostics. Tests in `tests/propan/sema/diagnostics/unsupported-binary-types.propan` cover representative string/address cases.

Some instruction-specific address constraints and the intended negative division policy remain open; do not extrapolate from this general expression reference into hardware encoding guarantees. For callable P2 arithmetic/bit-pattern functions, see [/projects/propan/stdlib.md](/projects/propan/stdlib.md).
