---
type: "Reference"
title: "Propan expressions and values"
description: "Current semantic behavior of Propan expressions and value categories."
tags: ["propan", "expressions", "operators", "values", "semantics"]
status: "draft"
source_confidence: "high"
timestamp: "2026-09-30T12:20:00+02:00"
---
# Propan expressions and values

Parser-level token syntax and lexical precedence are documented in [/projects/propan/lexical-and-source-grammar.md](/projects/propan/lexical-and-source-grammar.md).

## Runtime/semantic value categories

Semantic analysis evaluates expressions into one of these internal value categories:

| Category | Meaning |
|---|---|
| `int` | signed 64-bit integer value |
| `string` | byte string produced by a string literal |
| `address` | tagged code/data address carrying HUB address, segment identity, and execution-local address |
| `register` | P2 register index |
| `enumerator` | symbolic `#name` value, resolved by the consuming instruction/function context |
| `pointer_expr` | encoded PTRA/PTRB pointer expression with optional update/index information |

Values also carry flags that are separate from the payload: usage as a literal or register, an augmentation request, and an addressing mode (`auto`, `absolute`, or `relative`). These flags influence instruction selection/encoding rather than changing the underlying integer/address value.

There is no separate semantic boolean type. Boolean-producing operators return integer `0` or `1`.

## Integer model

Integer literals are initially parsed as non-negative values fitting in `u63`, then semantic evaluation uses signed `i64` values. Unary `-` is therefore an operator rather than part of the literal token.

Current integer arithmetic behavior:

- `+`, `-`, and `*` use wrapping signed 64-bit arithmetic;
- `/` uses floor division and reports division by zero;
- `%` uses modulo and reports division by zero;
- `<<` and `>>` require the shift count to fit in `u6` (`0...63`), otherwise evaluation reports overflow;
- bitwise operators operate directly on the `i64` bit pattern;
- comparison/boolean operators return `0` or `1`.

When a value is later emitted into a narrower instruction/data field, truncation can occur and the assembler emits a warning when the truncated value differs from the original.

## Binary operator precedence and associativity

All currently implemented binary operator groups are left-associative. From lowest to highest precedence:

| Precedence | Operators | Current integer behavior |
|---:|---|---|
| 0 | `and`, `or`, `xor` | logical combination; operands are true when nonzero; result `0` or `1` |
| 1 | `==`, `!=`, `<=>`, `<`, `>`, `<=`, `>=` | signed comparisons; `<=>` returns `-1`, `0`, or `1` |
| 2 | `+`, `-`, `|`, `^` | wrapping add/subtract, bitwise OR/XOR |
| 3 | `&`, `*`, `/`, `%` | bitwise AND, wrapping multiply, floor divide, modulo |
| 4 | `>>`, `<<` | signed-value shifts with a `0...63` shift count |

Parentheses override precedence.

The current semantic tests explicitly exercise left associativity for subtraction, division, and shifts.

### Ternary syntax conflict

The tokenizer contains `?` and `:` tokens and the root `README.md` advertises a ternary `? :` expression. The current Zig parser has no ternary-expression production. Ternary expressions are therefore **not current implemented syntax** despite that external documentation.

See [/projects/propan/documentation-discrepancies.md](/projects/propan/documentation-discrepancies.md).

## Unary operators

### `+value`

Requires an integer and returns it unchanged.

### `-value`

Requires an integer and returns its signed negation.

### `!value`

Requires an integer. Returns `1` when the operand is zero and `0` otherwise.

### `~value`

Requires an integer and performs bitwise complement.

### `*address`

Changes an address value's usage hint to **register**. This is how a code label can be requested in register/local-address form when an instruction expects that interpretation.

Applying `*` to a data label already carrying register usage emits a warning that the operator has no effect.

### `&address`

Changes an address value's usage hint to **literal**. This is how a data label can be requested in literal/address form when an instruction expects that interpretation.

Applying `&` to a code label already carrying literal usage emits a warning that the operator has no effect.

### `@address`

Computes a relative longword displacement using the current expression location and the target's HUB address:

```text
(target_hub_address - current_hub_address) / 4
```

The difference must be divisible by four. `@` requires a current location, so it cannot be evaluated in location-independent scopes such as ordinary constants.

**Current limitation:** the implementation contains a TODO to verify that current and target addresses belong to the same segment. It currently performs the HUB-address subtraction without that segment-identity validation.

### `++` / `--`

Prefix and postfix increment/decrement operators are not general integer increment/decrement operators. They construct PTRA/PTRB pointer-expression update modes and reject ordinary non-pointer values.

See the future pointer-addressing reference for encoding ranges and hardware behavior.

## Indexing

`value[index]` is currently a pointer-expression operation, not a general array operator.

- the left side must evaluate to PTRA/PTRB (register or existing pointer expression);
- the index must be an integer;
- applying a second index to a pointer expression is diagnosed.

The exact legal encoded range is checked later during pointer encoding.

## Address conversion and encoding-control functions

These functions are implemented directly by semantic analysis:

| Function | Current behavior |
|---|---|
| `hubaddr(value)` | address → absolute HUB byte address; integers pass through with a warning; other value categories are errors |
| `cogaddr(value)` | COG-tagged address → local COG address; wrong address domain is an error |
| `lutaddr(value)` | LUT-tagged address → local LUT address; wrong address domain is an error |
| `localaddr(value)` | address → its execution-local address without requiring a specific COG/LUT/HUB domain |
| `aug(value)` | requests operand augmentation; must be the root expression |
| `nrel(value)` | forces absolute rather than automatically relative addressing; must be the root expression |

`cogaddr()`/`localaddr()` also contain register-handling paths. `lutaddr()` rejects register values. The `localaddr(register)` path currently emits an error indicating it is only valid in COG-exec scope while also containing a TODO for execution-mode validation; treat register use here as unsettled behavior rather than a stable interface.

The P2 standard-library functions (`abs`, rotates, floating helpers, clock helpers, etc.) are separate user-function definitions loaded from `src/propan/stdlib/`; their complete signatures and edge behavior are not yet covered by this page.

## Type checking and failure behavior

Most invalid operator/value combinations produce source diagnostics. Examples include applying arithmetic unary operators to strings/addresses, using pointer operators on ordinary integers, or combining mismatched binary operand types.

Two important exceptions exist in the current implementation:

- a binary operator applied to two `address` values reaches an `@panic` placeholder;
- a binary operator applied to two `string` values reaches an `@panic` placeholder.

These are assembler defects/limitations, not intentional language semantics. They are tracked in [/projects/propan/implementation-findings.md](/projects/propan/implementation-findings.md).

## External syntax tables

The root README includes a useful PASM-to-Propan operator comparison, but it should not be treated as the current semantic authority. In particular, PASM/Spin spellings shown there do not become accepted Propan operators merely by appearing in the comparison table. The current Zig parser and evaluator define accepted behavior.
