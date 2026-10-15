---
type: "Reference"
title: "Propan addresses and segments"
description: "HUB/COG/LUT addresses, register and data modes, segment origins, and allocation as implemented after the upstream merge."
tags: ["propan", "addresses", "segments", "cog", "lut", "hub", "regspace", "data"]
status: "draft"
source_confidence: "high"
---
# Propan addresses and segments

**Revalidated for the merged implementation (2026-10-15)** using `src/propan/sema.zig`, `src/propan/stdlib/eval.zig`, the mode directive table, and current semantic/diagnostic fixtures. Some instruction-specific address controls remain documented elsewhere.

## Tagged addresses

Labels retain a segment identity and a native address domain, rather than being plain integers. Depending on the mode they can carry:

- a HUB **byte** address where emitted content resides;
- a COG/LUT **long/register/PC** address identifying a local instruction or data slot;
- a register-only location with **no HUB address** (`.regspace`);
- a HUB-resident data location with **no execution PC** (`.data`).

A COG/LUT label can thus identify an execution-local address and the HUB byte offset where its initializer/code is emitted. Use explicit address helpers when address units matter.

## Label usage and address conversion

A normal code label defaults to literal/address-oriented operand usage; `var` declares a data label with register-oriented usage. Neither label declaration emits or reserves storage. `&label` requests literal-style use; `*label` requests register-style use.

| Expression | Role |
|---|---|
| `hubaddr(label)` | obtain HUB byte location when present |
| `cogaddr(label)`, `lutaddr(label)` | obtain the matching local register/PC address |
| `localaddr(label)` | obtain a local-domain address |
| `byteoffset(label)`, `wordoffset(label)` | obtain sub-register offset information for packed data |
| `pcaddr(...)` | explicit execution-PC-domain helper; consult current evaluator for operand restrictions |
| `@label` | execution-relative offset; alignment and domain restrictions apply |
| `aug(expr)`, `nrel(expr)` | instruction encoding/address-selection controls |

The register forms of these helpers have additional checks. Their behavior must not be inferred from a label-only example; see [/projects/propan/expressions.md](/projects/propan/expressions.md) for the remaining helper audit.

## Execution modes

| Directive | Label mode | Origin interpretation | Allowed content |
|---|---|---|---|
| `.cogexec [hub[, local]]` | COG | HUB byte address plus optional local index (default `0`) | code and initialized data |
| `.lutexec [hub[, local]]` | LUT | HUB byte address plus optional local index (default `0x200`) | code and initialized data |
| `.hubexec [hub]` | HUB | HUB byte/execution address | code and initialized data |
| `.regspace [hub[, local]]` | register-only | no HUB storage address on labels; optional local COG index | non-emitting register declarations/reservation |
| `.data [hub]` | HUB data-only | HUB byte origin; no execution PC | data but no encoded instructions |

Mode changes create new segments. The HUB origin defaults to the current HUB cursor if absent. COG/regspace local indices are checked against `0x000..0x200` and LUT against `0x200..0x400`; entering a local mode does not implicitly shift HUB contents to match those register indices.

## Local layout versus output bytes

The `Cursor` maintains a HUB offset, a local **byte** cursor for COG/LUT/register-only mode, an active mode and a reservation state. Its exposed COG/LUT local PC is derived from long positions (`local_bytes / 4`, with LUT base `0x200`).

- An emitted `LONG` or instruction normally advances the HUB cursor by 4 bytes and an emitting COG/LUT local address by one long.
- An emitted `BYTE` or `WORD` can occupy part of a local long; natural alignment or `.pack` controls element placement.
- A `var name:` declaration only records the position; an accompanying `LONG`/`WORD`/`BYTE` emits initialized bytes.
- `.org target` advances the selected origin in the current address domain; it rejects backward movement and cannot be used in `.data`.
- `RES count` in COG/regspace advances the *local* register cursor without emitting HUB data, and blocks further emission in that mode until a mode change.
- `.align N` aligns local indices in COG/LUT/regspace but byte offsets in HUB/data. The actual emitted gap uses the configured fill byte.

Do not equate a reserved COG register with four initialized HUB bytes. A register-space address need not have a HUB location at all.

## Mixed-mode example

```propan
.cogexec 0x1000
code_start:
    NOP
var cog_value:
    LONG 0

.lutexec
lut_start:
    NOP
var lut_value:
    LONG 0

.data
table:
    BYTE "OK"
```

The first COG code label is local `0` at HUB `0x1000`, and `cog_value` follows it at local `1` / HUB `0x1004`. The LUT label is local `0x200` at HUB `0x1008` and its `LONG`-initialized value is local `0x201` / HUB `0x100C`. The `.data` table starts at HUB `0x1010`, emits the string bytes, and has no executable PC. These addresses follow from the current natural alignment and the four-byte instruction/long lengths.

For a reserved register slot instead of emitted initialized bytes, switch into `.regspace` or use `RES` in COG mode, subject to the non-emitting semantics and restrictions described in [/projects/propan/directives-and-data.md](/projects/propan/directives-and-data.md).

## Relative addressing, crossings and limitations

Instruction selection distinguishes HUB/local domains, native address units, and when relative encoding can be used. The merged branch adds `.pic` selection policy and origin/sub-register helpers; precise per-instruction behavior is governed by generated instruction metadata and the encoding selector, not a blanket rule that COG↔LUT transitions are always rejected.

A warning may be emitted for jumps between HUB and local execution domains. This is **not** a proof of complete cross-segment safety: the address-conversion code still needs careful same-segment validation for some constructs.

The semantic emitter **rejects positive-length overlaps between nonempty emitted HUB segments**. `src/propan/sema.zig` (`emit_code`) compares each pair of segment ranges and emits a semantic error for intersections before successful flat output. Separate register-only regions may have no emitted HUB bytes. This is not a general object linker or relocation model.

## Evidence and remaining verification

- **Implemented:** `Cursor.init/sync/change_mode/align_data/alignas/org/reserve`, the semantic directive dispatch, and the current tagged-address unions.
- **Tested:** `tests/propan/sema/data-mode.propan`, `spin2-register-origins.propan`, `pack.propan`, `local-labels.propan` and origin/reservation diagnostic fixtures, plus JSON/listing tests in `src/propan/test_suite.zig`.
- **Open:** revalidate all instruction-class-specific PC-relative conversions, PIC controls, cross-local-segment checks, and helper behavior against source + dedicated tests. Do not rely on the older pre-merge wording for those details.
