---
type: "Reference"
title: "Propan documentation and validation coverage"
description: "Map from documented Propan language/tooling areas to implementation evidence and current parser, semantic, equivalence, and regression tests."
tags: ["propan", "coverage", "tests", "verification", "documentation"]
status: "draft"
source_confidence: "high"
---
# Propan documentation and validation coverage

This page maps the OKF Propan reference to implementation and test evidence. **Coverage rows not explicitly refreshed after the 2026-10-15 upstream merge still reflect the September baseline and must be revalidated.** It is a maintenance aid, not a claim that every documented edge case has a dedicated regression test.

Test roles differ:

- `tests/propan/parser/` demonstrates frontend acceptance/grammar and may include parser-only forms that are not semantically supported.
- `tests/propan/sema/` exercises semantic evaluation, instruction selection, values, addresses, and selected output metadata.
- `tests/propan/equivalence/` compares Propan encodings against paired Spin2/PASM2 reference material and is especially important for generated P2 instructions, pointer forms, branches, effects, and augmentation.
- `tests/propan/regressions/` contains focused end-to-end regressions for previously observed failures/tool behavior.

A parser fixture alone is never sufficient evidence that a feature is a current semantic language feature.

## Coverage map

| Area | Current OKF reference | Principal implementation/evidence | Current validation examples | Coverage note |
|---|---|---|---|---|
| lexical/source grammar | `/projects/propan/lexical-and-source-grammar.md` | frontend parser and `sema.zig` conditional filter | `parser/values.propan`, `sema/conditional-compilation.propan`, `parser/diagnostics/inactive-conditional-syntax.propan`, `sema/diagnostics/conditional-*.propan` | **Refreshed 2026-10-15:** bracket sequences, `---
type: "Reference"
title: "Propan documentation and validation coverage"
description: "Map from documented Propan language/tooling areas to implementation evidence and current parser, semantic, equivalence, and regression tests."
tags: ["propan", "coverage", "tests", "verification", "documentation"]
status: "draft"
source_confidence: "high"
---
# Propan documentation and validation coverage

This page maps the OKF Propan reference to implementation and test evidence. **Coverage rows not explicitly refreshed after the 2026-10-15 upstream merge still reflect the September baseline and must be revalidated.** It is a maintenance aid, not a claim that every documented edge case has a dedicated regression test.

Test roles differ:

- `tests/propan/parser/` demonstrates frontend acceptance/grammar and may include parser-only forms that are not semantically supported.
- `tests/propan/sema/` exercises semantic evaluation, instruction selection, values, addresses, and selected output metadata.
- `tests/propan/equivalence/` compares Propan encodings against paired Spin2/PASM2 reference material and is especially important for generated P2 instructions, pointer forms, branches, effects, and augmentation.
- `tests/propan/regressions/` contains focused end-to-end regressions for previously observed failures/tool behavior.

A parser fixture alone is never sufficient evidence that a feature is a current semantic language feature.

## Coverage map

| Area | Current OKF reference | Principal implementation/evidence | Current validation examples | Coverage note |
|---|---|---|---|---|
, conditional source selection and phase limits; formatter still needs its own audit |
| expressions/value model | `/projects/propan/expressions.md` | parser + `sema.zig` evaluator, `stdlib/eval.zig` payload types | `sema/array-emission.propan`, `array-constants.propan`, `string-emission.propan`, `diagnostics/unsupported-binary-types.propan` | **Refreshed 2026-10-15:** sequences/repetition, current PC, new address helpers and diagnostics; desired negative `/`/`%` policy remains open |
| function-call grammar | lexical reference + `/projects/propan/stdlib.md` | parser + generated function wrapper metadata | `parser/fncalls.propan`, `sema/stdlib.propan` | active P2 callable namespace documented; generated metadata remains exhaustive source |
| symbols/constants/labels/vars | `/projects/propan/symbols-and-declarations.md` | semantic symbol declaration/evaluation, recursive constant resolver, local scope rebinding | `sema/local-labels.propan`, `sema/diagnostics/constant-cycle.propan`, `test_suite.zig` listing/JSON tests | **Refreshed 2026-10-15:** leading-dot local scope and forward constants supported; label-dependent layout values need separate treatment |
| address model | `/projects/propan/addresses-and-segments.md` | tagged addresses + semantic conversion/encoding | `sema/addressing-modes.propan`, `sema/basic-label-addressing.propan`, `equivalence/absrel_sample.propan`, `equivalence/branching.propan` | current HUB/COG/LUT model and allocation/emission relationship documented; intended `localaddr(register)` policy remains open |
| execution segments/layout | `/projects/propan/addresses-and-segments.md`, `/projects/propan/directives-and-data.md` | `sema.zig` `Cursor`/`assign_locations` | `sema/data-mode.propan`, `sema/spin2-register-origins.propan`, `sema/pack.propan`, origin/reservation diagnostics | **Refreshed 2026-10-15:** also `.regspace`, `.data`, `.org`, `RES`, new origins/packing; instruction-specific transitions need follow-up |
| data/directives | `/projects/propan/directives-and-data.md` | `sema.zig` mnemonic table, data size and emitter; `frontend/imports.zig` | `sema/pack.propan`, `sema/data-mode.propan`, diagnostic fixtures; `test_suite.zig` | **Refreshed 2026-10-15:** string/sequence emission and `FILE` implemented; directive condition/effect policy remains open |
| instruction grammar/selection | `/projects/propan/instruction-syntax.md` | generated variant table, semantic selector and PIC policy | `sema/diagnostics/conditional-nop.propan`, `sema/nop-encoding.propan`, `sema/pic-modes.propan`, `sema/pointer-variant-order.propan` | **Refreshed 2026-10-15:** explicit conditions on NOP rejected; pointer-register comparison typo fixed; residual selection cases remain |
| instruction encoding breadth | instruction reference policy + `/projects/propan/pasm2-differences.md` | generated P2 instruction data | `equivalence/all_instructions.txt` plus paired `.propan`/`.spin2` equivalence groups | broad hardware-encoding comparison; not a handwritten per-instruction language spec |
| effects/flags | `/projects/propan/instruction-syntax.md` | generated effect metadata | `equivalence/flags.propan`, `equivalence/special_effects.propan`, parser condition/effect fixtures | strong encoding-level evidence for normal/special effects |
| augmentation | instruction/address references | semantic AUGD/AUGS emission | `equivalence/aug.propan` | direct equivalence evidence |
| branching/relative addressing | `/projects/propan/pointer-addressing.md`, address/instruction refs | `select_calld_ambiguous_encoding`, `compute_rel`, PIC/address encoder | `sema/pic-modes.propan`, `sema/pointer-variant-order.propan`, `equivalence/ambigious.propan` | **Refreshed 2026-10-15:** current CALLD/PIC preference and cross-segment warnings; edge cases remain open |
| pointer addressing | `/projects/propan/pointer-addressing.md` | `encode_ptr_expr` + instruction emitter | `sema/aug-pointer-update.propan`, `diagnostics/aug-whole-pointer.propan`, `diagnostics/aug-pointer-index-out-of-range.propan`, `equivalence/memory-ptr.propan` | **Refreshed 2026-10-15:** augmented pointer index ranges and whole-pointer rejection covered |
| standard library | `/projects/propan/stdlib.md` | active P2 functions + direct semantic helpers | `sema/builtin-functions.propan`, `alti-*.propan`, `array-emission.propan`, `stdlib.propan` | **Refreshed 2026-10-15:** 32-bit numeric, ALTI, Unicode and register helpers; hardware-facing configuration correctness is not universally revalidated |
| PASM2 migration | `/projects/propan/pasm2-differences.md` | generated mapping + current grammar | equivalence suite broadly | source-form translation summarized; repository examples have been separately classified for current/legacy/unverified status |
| worked examples | `/projects/propan/examples.md` | source/examples + current semantic fixtures | `tests/propan/cases.zig` active examples; `sema/res-labels.propan`, `array-emission.propan`, `diagnostics/conditional-nop.propan` | **Refreshed 2026-10-15:** source examples corrected for RES, NOP, mode-specific alignment; extracted doc-example regression remains open |
| single-root/imports | `/projects/propan/tooling.md` | `src/propan/propan.zig` + `frontend/imports.zig` | `test_suite.zig` root rejection, include precedence, import-once identity and imported source metadata | **Refreshed 2026-10-15:** multi-root input is rejected; imports expand into a single module |
| flat/JSON/Spin2/fill | `/projects/propan/tooling.md` | `src/propan/emit/` and CLI composition | `test_suite.zig` fill-byte, Spin2 readable output and configured FlexSpin round-trip | **Refreshed 2026-10-15:** `spin2` is an output format; do not claim unconditional round-trip coverage |
| list files | `/projects/propan/tooling.md` | `src/propan/listfile.zig` and its unit tests | in-source `listfile.zig` tests | covered at implementation-unit level rather than `tests/propan/` fixture level |
| implementation limitations | `/projects/propan/implementation-findings.md` | code inspection + relevant tests | varies per finding | findings distinguish confirmed behavior/defects from unresolved intent |
| documentation conflicts | `/projects/propan/documentation-discrepancies.md` | comparison of current implementation with README/old docs/examples | parser/sema/equivalence evidence as applicable | prevents parser-only/historical material becoming normative accidentally |

## Known validation/design gaps retained in TODO

The following are intentionally **not** treated as closed merely because adjacent behavior is documented:

- desired negative-operand contract for `/` and `%`;
- desired `localaddr(register)` behavior;
- verifying which label-dependent constants are legal in layout-phase evaluation (ordinary forward constant dependencies now resolve);
- focused verification of nested/UTF string-and-sequence data edge cases (basic emission now implemented);
- conditions/effects on assembler directives;
- remaining CALLD overlapping variant combinations and the defensive two-pointer-register-variant panic (the old comparison typo is corrected);
- intended future status of the broader segment-management fixture;
- intended use of ordinary user constants during layout;
- executable validation of documentation examples;
- documentation link/generated-reference consistency checking;
- post-merge revalidation of remaining directive, expression, segment, instruction and implementation-finding pages.

## Maintenance rule

When a language/tooling behavior changes, update the relevant current-state page and this map if its validation source changes materially. Prefer linking a feature to the smallest authoritative test/evidence family rather than claiming blanket coverage from the existence of a broad suite.

If a parser fixture demonstrates syntax that semantic analysis does not implement, preserve that distinction explicitly rather than marking the semantic feature covered.
