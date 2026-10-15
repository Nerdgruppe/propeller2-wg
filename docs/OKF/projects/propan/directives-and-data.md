---
type: "Reference"
title: "Propan directives and data"
description: "Current semantic assembler directives, origin/reservation modes, data emission, packing, alignment, and assertions."
tags: ["propan", "directives", "data", "alignment", "origins", "reservation", "assertions"]
status: "draft"
source_confidence: "high"
---
# Propan directives and data

**Verified against the 2026-10-15 merged source.** The semantic mnemonic table is in `src/propan/sema.zig` (`Analyzer.init`); the implementation is in `assign_locations`, `evaluate_asserts` and `emit_code`. These directives are *not* inferred merely from parser fixtures.

## Implemented names

Mnemonic lookup is case-insensitive.

| Directive | Arguments | Effect |
|---|---|---|
| `BYTE`, `WORD`, `LONG` | zero or more data expressions | emit 1-, 2-, or 4-byte elements |
| `FILE` | exactly one literal string path | embed external file bytes |
| `.cogexec`, `.lutexec` | optional HUB origin, optional local origin | start executable COG/LUT segment |
| `.hubexec` | optional HUB origin | start executable HUB segment |
| `.regspace` | optional HUB origin, optional local origin | enter register-only allocation mode |
| `.data` | optional HUB origin | enter HUB data-only mode |
| `.org` | one integer/address-origin expression | move the current origin forward |
| `RES` | one nonnegative register count | non-emitting register reservation in COG/regspace |
| `.align` | one nonzero power-of-two integer | align the active cursor |
| `.pack` | `off`, `byte`, `word`, or `long` | control implicit data alignment |
| `.pic` | `default`, `prefer`, `avoid`, or `force` | choose branch-addressing preference |
| `.assert` | condition; optional message string | compile-time assertion |
| `.fit` | limit; optional message string | assert active cursor fits within limit |

`.import "path"` and `.import once` are handled by the source expander *before* semantic analysis; see [/projects/propan/tooling.md](/projects/propan/tooling.md).

**Not registered as equivalents in the current semantic mnemonic table:** `.huborg`, `.cogorg`, `.lutorg`, `.reserve`, `.regs`, `.RES` (leading dot), `.cogfit`, `.section`. In particular, `RES` (without dot) is implemented and must not be confused with `.RES`.

## Data: BYTE, WORD, LONG

```propan
BYTE 0x12, "Hi"
WORD 0x1234
LONG 0x11223344
BYTE [1, 2, 3]
BYTE "ab" * 3
```

Each scalar is narrowed to its target element width; narrowing that changes the value can warn. Values are emitted in **little-endian** order. Zero-argument data directives are accepted and emit no bytes.

**Strings are now supported.** A string produces one element per encoded byte; for example, `WORD "AB"` emits two 16-bit values (`0x0041` and `0x0042`), not one packed word. Integer sequences likewise expand into separate elements. `*` repetition accepts a sequence/string and an integer repetition count, with zero repetitions warned about and size limits checked. Nested literal sequences and UTF conversion helpers are represented as sequence data, not as ordinary pointer dereferences.

The element count and emitted size are computed during layout (`data_expr_count`); if final evaluation disagrees with that planned size, the assembler emits a diagnostic. Arbitrary non-layout-known lengths therefore are not a general dynamic array facility.

Address-valued expressions are converted to the current execution/address context; use `hubaddr(label)` etc. to select an explicit domain. Bare enumerator operands belong to instruction enumeration contexts, not to an unrestricted raw-data convention.

### Implicit alignment and .pack

Ordinary `WORD` and `LONG` data are implicitly aligned to the element width, as are encoded instructions (long-aligned). `.pack byte`, `.pack word`, `.pack long` select alignment of 1, 2, or 4 bytes respectively, and `.pack off` restores natural alignment. This changes layout and can generate warnings for unaligned packed instructions; it does **not** change each element's stored width.

Evidence: `tests/propan/sema/pack.propan` and `src/propan/sema.zig` (`Cursor.align_data`).

## Binary inclusion: FILE

```propan
FILE "assets/table.bin"
```

The assembler reads the named file into the image as raw bytes. The path is absolute or relative to the source file containing `FILE` (important with imports); it requires one literal string path. The current I/O path limits files to 512 KiB and diagnoses unreadable files. `FILE` is a **binary data inclusion**, distinct from source-code `.import`.

## Execution and data modes

`.cogexec` / `.lutexec` accept **zero, one or two** operands:

```propan
.cogexec 0x1000, 0x20    // HUB emission origin, COG-local register/PC origin
.lutexec 0x1200, 0x220  // HUB emission origin, LUT-local origin
.hubexec 0x1400         // HUB emission/execution origin
.regspace 0, 0x100      // no emitted bytes, registers from COG 0x100
.data 0x1800            // HUB-resident data, no instruction execution PC
```

An omitted HUB origin continues at the current HUB cursor. An omitted local origin defaults to COG `0` or LUT `0x200`. Local origins in COG/regspace are bounded by `0x000..0x200`; LUT origins by `0x200..0x400`. `.hubexec` and `.data` accept at most one HUB-origin operand.

`.regspace` creates register-tagged addresses with **no HUB storage address**; emitted data/code inside it is rejected. `.data` creates HUB-addressed data labels without an executable PC; encoded instructions inside it are rejected. These are distinct from `var`, which labels the current address but does not reserve any storage.

Changing mode creates a new segment. The merged semantic emitter **rejects overlaps between nonempty emitted HUB segments**; see [/projects/propan/addresses-and-segments.md](/projects/propan/addresses-and-segments.md).

## .org and RES

`.org target` advances the position in the **current address domain**. In HUB-exec the target is a HUB byte address; in COG/LUT/regspace the target is a local long/register index. It cannot move the cursor backward or be used in `.data` mode; local target ranges are checked. Forward origins in emitting modes increase HUB allocation correspondingly, with padding using the configured fill byte.

`RES count` reserves **count local registers (longs)** without emitting HUB bytes. It is legal only in `.cogexec` or `.regspace`; it advances the local cursor by `count * 4` bytes, sets the reservation state, and prevents subsequent data/code emission in that segment until a new mode directive. This is not an alias for a data directive and cannot be used in HUB/LUT/data modes.

**Evidence:** `assign_locations`, `Cursor.org`, `Cursor.reserve`, and `tests/propan/sema/diagnostics/res-invalid-in-mode.propan`.

## .align

`.align alignment` requires one nonzero power-of-two integer no greater than `0x80000`. In HUB/data modes it aligns the HUB byte cursor; in COG/LUT/regspace modes it aligns the corresponding **local register/PC index**, accounting for the LUT base index. This distinction matters: `.align 4` in COG mode means a multiple-of-four *long index*, not merely a four-byte HUB alignment.

The required address growth in emitting modes produces padding with the configured `--fill-byte`, not a fixed `0xFF` guarantee. In register-only space, alignment adjusts the local position without emitting bytes.

## .pic

`.pic default`, `.pic prefer`, `.pic avoid`, and `.pic force` set the running policy for relative versus absolute branch-address selection. `force` can diagnose a branch that would require an absolute address. The directive itself emits no bytes. Specific instruction encodings still depend on the generated operand metadata; see [/projects/propan/instruction-syntax.md](/projects/propan/instruction-syntax.md).

## .assert and .fit

```propan
.assert 1 == 1, "failed condition"
.fit 0x1F0, "code exceeds expected register range"
```

`.assert` accepts an integer condition; zero fails. `.fit` instead compares the active cursor's position against the supplied limit: COG/regspace and LUT use local register/PC indices, while HUB/data use HUB byte addresses. Both accept an optional string message, reject incorrect argument counts, and emit no bytes. The optional message's type is validated before the pass/fail decision in the merged implementation.

## Evaluation phase, conditions and effects

Mode origins, `.org`, `.align`, `RES` and sizes of sequence expressions are decided during **location assignment**. Ordinary constant dependencies may resolve recursively, but constants that need a label address that is still being assigned cannot necessarily be used in those positions. This is a layout-phase restriction, not a blanket prohibition on user-defined constants.

The parser may accept instruction-style `if(C)`/`return` and `:effect` on data/layout directives, but normal directive encoding does **not** make these conditional data emission or flag changes. Do not depend on such modifiers. In contrast, `.if`/`.elif`/`.else`/`.endif` are compile-time **source-filter controls** handled earlier and explicitly reject instruction-style conditions/effects on their own lines. See [/projects/propan/lexical-and-source-grammar.md](/projects/propan/lexical-and-source-grammar.md) and [/projects/propan/implementation-findings.md](/projects/propan/implementation-findings.md).

## Evidence and limits

- **Implemented:** all names in the table appear in the live semantic dispatch; mode, origin, reservation, data expansion and emission paths were checked.
- **Tested:** `test_suite.zig` and `tests/propan/sema/` contain cases for packing, origin limits, reservation diagnostics, embedded files, data/UTF sequences, and fit/assert behavior. Not every option or edge case has been rerun for this documentation edit.
- **Open:** instruction-specific cross-domain address checks need more coverage; PIC and conditional-source selection edge cases should be tested. The separate nonempty emitted-segment overlap check is now implemented.
