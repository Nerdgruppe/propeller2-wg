---
type: "Reference"
title: "Propan pointer addressing"
description: "Current PTRA/PTRB pointer-expression syntax, encoding ranges, update behavior, and related branch-selection rules."
tags: ["propan", "pointer", "ptra", "ptrb", "addressing", "calld"]
status: "draft"
source_confidence: "high"
---
# Propan pointer addressing

Propan exposes the Propeller 2 PTRA/PTRB memory-pointer encoding through dedicated pointer expressions. These expressions are not general arithmetic on register values: semantic analysis builds a pointer-expression value which is accepted only by instruction operands generated as the P2 `{#}S/P` operand class.

`PTRA` and `PTRB` are predefined register values. `PA` and `PB` are also predefined registers, but they are not valid memory-pointer expressions.

## Source forms

Both pointer registers support the same forms:

| Form | Meaning | Explicit value range |
|---|---|---:|
| `PTRA`, `PTRB` | use pointer without update or index | — |
| `PTRA[n]`, `PTRB[n]` | indexed pointer without update | `-32...31` |
| `++PTRA`, `++PTRB` | pre-increment | default magnitude `1` |
| `--PTRA`, `--PTRB` | pre-decrement | default magnitude `1` |
| `PTRA++`, `PTRB++` | post-increment | default magnitude `1` |
| `PTRA--`, `PTRB--` | post-decrement | default magnitude `1` |
| `++PTRA[n]`, `--PTRA[n]`, and PTRB equivalents | pre-update with explicit magnitude | `1...16` |
| `PTRA++[n]`, `PTRA--[n]`, and PTRB equivalents | post-update with explicit magnitude | `1...16` |

For **non-augmented** updating forms, the index is a positive magnitude `1...16`, with `++`/`--` selecting direction. Augmented pointer indices permit wider, mode-dependent ranges as described below.

The current parser permits one optional index operation after an identifier/postfix-update expression. Prefix updates recurse into the value expression, so both forms such as `++PTRA[4]` and `PTRA++[4]` reach the pointer-expression model.

## Encoding model

A pointer expression becomes one 9-bit source-field value containing:

- a marker selecting the P2 pointer encoding;
- PTRA versus PTRB;
- one of no update, pre-increment, pre-decrement, post-increment, or post-decrement;
- the encoded index/update magnitude.

The current encoder uses:

| Mode | Current encoded input |
|---|---|
| no update | signed 6-bit index, `-32...31` |
| pre/post increment | magnitude `1...16` |
| pre/post decrement | magnitude `1...16`, encoded with negative direction |

For updating forms, omitting `[n]` supplies magnitude `1`. Magnitude `16` uses the P2 encoding's special/wrapped representation rather than being rejected.

When an actual pointer-expression value is emitted into a `{#}S/P` operand, the instruction's immediate/source-selector bit is set as required by the P2 pointer encoding.

### Augmented pointer indices

Use `aug(...)` *inside the index*, not around the whole pointer expression:

```propan
RDLONG PA, PTRA[aug(0x10000)]
RDLONG PA, PTRA++[aug(0x100)]
RDLONG PA, --PTRB[aug(0x12345)]
```

| Form | Augmented index/magnitude |
|---|---|
| without update | `-0x80000...0xFFFFF` |
| pre/post increment | `1...0x7FFFF` |
| pre/post decrement | `1...0x80000` |

`aug(PTRA[...])` is rejected with `err_augmentation_requires_pointer_index`; direct numeric augmented pointer immediates instead use unsigned `0...0xFFFFF`. Evidence: `encode_ptr_expr` in `src/propan/sema.zig`, `tests/propan/sema/aug-pointer-update.propan`, and the `aug-whole-pointer` / `aug-pointer-index-out-of-range` diagnostic fixtures.

## Bare PTRA/PTRB versus ordinary register use

A bare `PTRA` or `PTRB` starts as an ordinary predefined register value. Under the current default analyzer option, after an instruction variant with a `pointer_expr` operand has been selected, semantic analysis rewrites a bare PTRA/PTRB in that operand position to a no-update pointer expression.

This conversion is contextual. A bare PTRA/PTRB used by an ordinary register operand remains a register.

Applying `++`, `--`, or `[...]` explicitly requires PTRA or PTRB. Applying pointer-expression syntax to another register produces a diagnostic.

## Instructions with pointer-expression operands

The generated P2 instruction table currently contains nine `{#}S/P`/`pointer_expr` operands:

- `WMLONG`
- `RDLUT`
- `RDBYTE`
- `RDWORD`
- `RDLONG`
- `WRLUT`
- `WRBYTE`
- `WRWORD`
- `WRLONG`

Do not assume that an arbitrary instruction source field accepts PTRA/PTRB pointer syntax. Acceptance is determined by the generated operand type for the selected instruction variant.

The equivalence suite exercises bare, indexed, pre-update, and post-update PTRA/PTRB forms across these memory operations and compares the output against the corresponding Spin2/PASM2 source.

## Pre-update and post-update timing

The source spelling follows the P2 pointer-addressing operation:

- prefix `++PTRx` / `--PTRx` updates the pointer **before** the memory address is used;
- postfix `PTRx++` / `PTRx--` uses the current pointer address and updates it **after** the access.

The canonical P2 instruction sheet makes the distinction explicit in aliases. For example:

- `POPA D` is an alias for reading a long from `--PTRA`;
- `PUSHA D` is an alias for writing a long to `PTRA++`.

The pointer expression encodes the P2 update/index field. This reference does not reinterpret the hardware's address-unit behavior for individual memory instructions.

## Pointer-register selector operands are a different category

Some P2 instruction forms use `PA/PB/PTRA/PTRB` as a small register selector rather than as a memory pointer expression. The generated assembler calls this operand category `pointer_reg`.

Current examples are the special address forms:

```text
CALLD PA/PB/PTRA/PTRB, #{\}A
LOC   PA/PB/PTRA/PTRB, #{\}A
```

A `pointer_reg` operand accepts exactly PA, PB, PTRA, or PTRB and encodes them as selector values `0...3`. It does not use the PTRA/PTRB update/index encoding described above.

## Branch-S PC-relative operands

A separate generated operand category is the PASM2 `{#}S**` branch source used by `CALLD D,{#}S**`, `DJZ`, `DJNZ`, and related branch-S instructions.

For this operand class:

- a register-valued source uses the register directly;
- a literal-valued source is encoded as a PC-relative instruction displacement;
- the ordinary displacement must fit signed 9 bits;
- `aug(...)` selects the augmented calculation, which must fit signed 20 bits and emits the required augmentation prefix.

The displacement calculation depends on execution mode:

- COG/LUT execution uses the target local address minus the instruction PC, in instruction units;
- HUB execution uses the corresponding longword-relative displacement.

This matches the canonical P2 rule for `{#}S**`: register `S` supplies the PC directly, while immediate `#S` is a signed relative displacement (scaled by four in HUB execution by the hardware definition).

A common Propan distinction is therefore:

```propan
CALLD dst, src       // data label -> register-valued S
CALLD dst, &src      // force literal usage -> PC-relative S**
```

A code label already has literal usage by default, so it naturally selects the immediate/relative interpretation when the `S**` variant is selected.

### `nrel()` does not make `S**` an absolute immediate

`nrel()` sets Propan's address-mode hint to absolute. The `S**` encoder does not consult that hint because the P2 encoding has no absolute-immediate `S**` form: it is register-direct or immediate-relative.

`nrel()` is meaningful when an `A`-address variant is selected, where P2 has an explicit relative/absolute bit. It should not be read as a universal "make every branch operand absolute" operator.

## `CALLD` overlapping forms

`CALLD` is the important current overlap between two generated instruction families:

```text
CALLD D, {#}S**                 {WC/WZ/WCZ}
CALLD PA/PB/PTRA/PTRB, #{\}A
```

A source line beginning with PA, PB, PTRA, or PTRB can potentially satisfy the ordinary register destination of the first form as well as the special `pointer_reg` destination of the second form. Some second operands can likewise satisfy both generic type checks.

The merged selector now checks `pointer_reg` use in **both** variants and applies `select_calld_ambiguous_encoding` to the ordinary/special `CALLD` overlap. The former copy/paste defect is fixed.

Explicit absolute addressing (`nrel(...)`) or `.pic avoid` favors the special address form. A source augmentation favors the ordinary form. Otherwise the policy considers execution domain and whether a signed 9-bit relative displacement is reachable. This is current **selection logic**, not a promise of correctness for every possible operand combination.

Current equivalence material demonstrates both regular CALLD forms and special address forms, including:

```propan
CALLD dst, src
CALLD dst, &src
CALLD PA, address
CALLD PA, nrel(address)
CALLD PTRA, address
CALLD PTRA, nrel(address)
```

`tests/propan/sema/pointer-variant-order.propan` checks PA, PB, PTRA, PTRB special destinations; `tests/propan/equivalence/ambigious.propan` now contains numeric/label and `nrel` cases. Remaining selection combinations should still be verified before assuming complete coverage.

The general ambiguity handler emits `err_ambigious_instruction_selection_for`. A residual defensive panic exists only if two candidate variants **both** use pointer-register operand categories; this is separate from the corrected comparison typo.

## Relation to PASM2 spelling

The pointer update/index spelling itself intentionally stays close to PASM2:

| Propan | PASM2 comparison |
|---|---|
| `PTRA` | `PTRA` |
| `PTRA[n]` | `PTRA[n]` |
| `++PTRA[n]` | `++PTRA[n]` |
| `--PTRA[n]` | `--PTRA[n]` |
| `PTRA++[n]` | `PTRA++[n]` |
| `PTRA--[n]` | `PTRA--[n]` |

The larger syntax difference is how non-pointer source values express register versus immediate use: Propan uses value/label semantics plus `&`/`*` usage transforms instead of using PASM2 `#` as the general immediate marker. See [/projects/propan/instruction-syntax.md](/projects/propan/instruction-syntax.md) and [/projects/propan/addresses-and-segments.md](/projects/propan/addresses-and-segments.md).
