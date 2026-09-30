---
type: "Reference"
title: "Propan documentation and validation coverage"
description: "Map from documented Propan language/tooling areas to implementation evidence and current parser, semantic, equivalence, and regression tests."
tags: ["propan", "coverage", "tests", "verification", "documentation"]
status: "draft"
source_confidence: "high"
---
# Propan documentation and validation coverage

This page maps the current OKF Propan reference to the repository evidence used to validate it. It is a maintenance aid, not a claim that every documented edge case has a dedicated regression test.

Test roles differ:

- `tests/propan/parser/` demonstrates frontend acceptance/grammar and may include parser-only forms that are not semantically supported.
- `tests/propan/sema/` exercises semantic evaluation, instruction selection, values, addresses, and selected output metadata.
- `tests/propan/equivalence/` compares Propan encodings against paired Spin2/PASM2 reference material and is especially important for generated P2 instructions, pointer forms, branches, effects, and augmentation.
- `tests/propan/regressions/` contains focused end-to-end regressions for previously observed failures/tool behavior.

A parser fixture alone is never sufficient evidence that a feature is a current semantic language feature.

## Coverage map

| Area | Current OKF reference | Principal implementation/evidence | Current validation examples | Coverage note |
|---|---|---|---|---|
| lexical/source grammar | `/projects/propan/lexical-and-source-grammar.md` | frontend tokenizer/parser | `parser/comments.propan`, `parser/escape_sequences.propan`, `parser/labels.propan`, `parser/values.propan` | good parser coverage; unknown-escape recovery still lacks a focused verified contract |
| expressions/value model | `/projects/propan/expressions.md` | semantic evaluator/value types | `parser/expressions.propan`, `sema/operators.propan`, `sema/operator-associativity.propan`, `sema/unary-plus.propan`, `sema/value-hint-converter.propan` | core operators covered; intended negative `/` and `%` semantics remain a design question |
| function-call grammar | lexical reference + `/projects/propan/stdlib.md` | parser + generated function wrapper metadata | `parser/fncalls.propan`, `sema/stdlib.propan` | active P2 callable namespace documented; generated metadata remains exhaustive source |
| symbols/constants/labels/vars | `/projects/propan/symbols-and-declarations.md` | semantic symbol declaration/evaluation | `parser/labels.propan`, `sema/basic-constants.propan`, `sema/basic-label-addressing.propan` | current source-order constant behavior documented; forward-constant policy remains open |
| address model | `/projects/propan/addresses-and-segments.md` | tagged addresses + semantic conversion/encoding | `sema/addressing-modes.propan`, `sema/basic-label-addressing.propan`, `equivalence/absrel_sample.propan`, `equivalence/branching.propan` | current HUB/COG/LUT model covered; `localaddr(register)` remains unsettled |
| execution segments/layout | `/projects/propan/addresses-and-segments.md`, `/projects/propan/directives-and-data.md` | semantic location assignment/segments | `sema/align.propan`, `sema/segment_management.propan` | current implemented subset documented; broader segment fixture contains non-current/design material |
| data/directives | `/projects/propan/directives-and-data.md` | semantic directive table + emitter | `parser/directives.propan`, `sema/align.propan`, `regressions/null-in-assert.propan` | supported set documented; string-data and directive condition/effect policy remain open |
| instruction grammar/selection | `/projects/propan/instruction-syntax.md` | generated instruction table + selector | `parser/basic_instruction_layout.propan`, `parser/conditions.propan`, `parser/effects.propan`, `sema/basic-instruction-selection.propan`, `sema/ambigious-selection.propan` | generated table is authority; conditional-NOP intent still needs focused verification |
| instruction encoding breadth | instruction reference policy + `/projects/propan/pasm2-differences.md` | generated P2 instruction data | `equivalence/all_instructions.txt` plus paired `.propan`/`.spin2` equivalence groups | broad hardware-encoding comparison; not a handwritten per-instruction language spec |
| effects/flags | `/projects/propan/instruction-syntax.md` | generated effect metadata | `equivalence/flags.propan`, `equivalence/special_effects.propan`, parser condition/effect fixtures | strong encoding-level evidence for normal/special effects |
| augmentation | instruction/address references | semantic AUGD/AUGS emission | `equivalence/aug.propan` | direct equivalence evidence |
| branching/relative addressing | `/projects/propan/pointer-addressing.md`, address/instruction refs | semantic relative selection and generated PC-relative metadata | `equivalence/branching.propan`, `equivalence/absrel_sample.propan` | current A vs S** distinction documented; CALLD overlap defect remains open |
| pointer addressing | `/projects/propan/pointer-addressing.md` | pointer-expression encoder + canonical P2 data | `equivalence/memory-ptr.propan` | direct/index/update encoding coverage; focused CALLD pointer-overlap regression still needed after fix |
| standard library | `/projects/propan/stdlib.md` | `src/propan/stdlib/` metadata/implementations | `sema/stdlib.propan`, equivalence `hubset.propan` for related configuration use | namespace/signatures documented; hardware-facing helper correctness is not universally revalidated by this map |
| PASM2 migration | `/projects/propan/pasm2-differences.md` | generated mapping + current grammar | equivalence suite broadly | source-form translation summarized; existing examples still need legacy review |
| multi-file/output/JSON/fill | `/projects/propan/tooling.md` | CLI merge loop + emitter | `regressions/multi-file-first.propan`, `multi-file-second.propan`, `check-multi-file-json.zig`, `fill-byte.propan`, `check-flat-output.zig` | direct end-to-end regression coverage |
| list files | `/projects/propan/tooling.md` | `src/propan/listfile.zig` and its unit tests | in-source `listfile.zig` tests | covered at implementation-unit level rather than `tests/propan/` fixture level |
| implementation limitations | `/projects/propan/implementation-findings.md` | code inspection + relevant tests | varies per finding | findings deliberately distinguish confirmed behavior, defects, and unresolved intent |
| documentation conflicts | `/projects/propan/documentation-discrepancies.md` | comparison of current implementation with README/old docs/examples | parser/sema/equivalence evidence as applicable | prevents parser-only/historical material becoming normative accidentally |

## Known validation gaps retained in TODO

The following are intentionally **not** treated as closed merely because adjacent behavior is documented:

- exact regression-verified recovery output for unknown escape sequences;
- desired negative-operand contract for `/` and `%`;
- desired `localaddr(register)` behavior;
- forward references between user constants;
- string data emission policy;
- conditions/effects on assembler directives;
- explicitly conditioned `NOP` intent/hardware semantics;
- focused CALLD pointer-register overlap cases after the selector defect is corrected;
- intended future status of the broader segment-management fixture;
- executable validation of future documentation examples.

## Maintenance rule

When a language/tooling behavior changes, update the relevant current-state page and this map if its validation source changes materially. Prefer linking a feature to the smallest authoritative test/evidence family rather than claiming blanket coverage from the existence of a broad suite.

If a parser fixture demonstrates syntax that semantic analysis does not implement, preserve that distinction explicitly rather than marking the semantic feature covered.
