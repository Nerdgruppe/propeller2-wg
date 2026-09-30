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
