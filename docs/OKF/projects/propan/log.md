---
type: "Change Log"
title: "Propan documentation log"
description: "Maintenance notes for curated Propan documentation."
tags: ["log", "propan", "maintenance"]
status: "draft"
source_confidence: "high"
---
# Log

## 2026-09-30

- Established the Propan documentation scope.
- Recorded the initial documentation-rework plan in `/TODO.md`.
- Defined current-state documentation/source precedence for the Propan language and assembler.
- Added the first verified lexical/source-grammar reference from the Zig frontend.
- Added a source inventory separating implementation, semantic tests, parser-only tests, generated data, examples, and historical documentation.
- Added an explicit discrepancy register for non-OKF documentation that conflicts with current code.
- Added an implementation finding register for assembler defects/limitations discovered during documentation work.
- Added expression/value and address/segment references and updated TODO progress.
- Removed repeated development-status disclaimers and branch-specific wording from individual reference pages; the scope-level status remains at the Propan OKF entrypoint.
- Restored the segment-management discrepancy entry accidentally removed during cleanup.
- Removed manually maintained front-matter timestamps from OKF documentation; Git history is used for chronology.
- Added the current symbol/declaration model, including flat symbol namespaces, `var` semantics, forward-label behavior, and source-order constant evaluation.
- Added the current directive/data reference for `BYTE`, `WORD`, `LONG`, `.align`, `.assert`, `.cogexec`, `.lutexec`, and `.hubexec`.
- Recorded layout-phase constant limitations and raw string/enumerator data-emission panic paths discovered while documenting directives.
- Updated `/TODO.md` to mark the completed symbol and directive-reference work and retain follow-up tasks for unresolved implementation intent.
- Added the current generated-instruction grammar, complete condition-code table, accepted effect spellings, semantic operand categories, augmentation model, and generic instruction-variant selection behavior.
- Recorded effect aliases accepted by the parser but omitted from the root README.
- Recorded that conditions/effects attached to assembler directives are currently parsed and silently ignored, and added a verification task for explicitly conditioned `NOP`.
- Chose generated/verifiable instruction metadata as the basis for a complete per-instruction reference rather than maintaining a handwritten duplicate of the P2 instruction table.
- Updated `/TODO.md` to mark the completed instruction-language work while retaining CALLD/PC-relative, pointer-addressing, PASM2-difference, and conditioned-NOP follow-up tasks.
- Added a dedicated PTRA/PTRB pointer-addressing reference covering direct, indexed, pre/post-update, and encoded range behavior verified against the equivalence suite and canonical P2 instruction data.
- Documented the distinction between P2 `S**` PC-relative branch operands and `A` relative/absolute address operands, including the scope of `aug()` and `nrel()`.
- Documented the two overlapping CALLD families and expanded the pointer-register ambiguity finding with current generated-table and equivalence-test evidence.
- Updated `/TODO.md` to mark the current pointer-addressing and branch-selection reference work complete while retaining focused regression work for the known CALLD ambiguity defect.
- Added the current standard-library reference, distinguishing active common/P2 symbols from inactive P1 definitions.
- Documented all currently loaded P2 standard-library functions, parameter/default behavior, enumerator domains, diagnostics, and constant families.
- Recorded that the six core address/encoding helpers are semantic builtins rather than generated stdlib functions, and that the current analyzer exposes no additional Spin-style `abs`/rotate/floating-point builtin set.
- Documented the existing `--render-stdlib-docs` path as the preferred exhaustive generated reference mechanism.
- Updated `/TODO.md` to mark the standard-library, function-type, and standard-library enumerator documentation tranche complete.
- Added the current CLI/output reference covering multi-file input, overlay order, flat/JSON formats, fill-byte behavior, list files, diagnostics, and exit status.
- Documented that positional input modules are analyzed independently and later overlapping segment writes overwrite earlier flat-output bytes because cross-module overlap rejection is currently absent.
- Recorded that `.propan` is conventional but not CLI-enforced and that no source-level include/import mechanism is currently implemented.
- Updated `/TODO.md` to mark the source-file/output/tooling tranche and cross-module overlay documentation complete.
- Added a compact PASM2-to-Propan migration reference grounded in the generated P2 instruction mapping and current Propan grammar.
- Documented that generated mnemonics and operand order are inherited from canonical P2 data while immediacy, conditions/effects, address controls, expressions, labels, and assembler directives use Propan syntax/semantics.
- Added a migration checklist and marked the PASM2 relationship/deviation/agent-oriented cheat-sheet tasks complete.
- Added a documentation/validation coverage map connecting current-state pages to parser, semantic, equivalence, regression, and implementation-unit evidence.
- Kept unresolved behavior/design questions explicitly listed as validation gaps rather than treating neighboring test coverage as proof.
- Added a worked-example reference using current syntax for HUB/COG execution, pointer addressing, conditions/effects, REP-relative labels, alignment, and explicit address-domain conversion.
- Marked the planned first-pass worked-example items complete while retaining automated example validation and legacy `examples/*.propan` review as separate follow-up work.
