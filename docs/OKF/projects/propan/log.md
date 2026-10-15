---
type: "Change Log"
title: "Propan documentation log"
description: "Maintenance notes for curated Propan documentation."
tags: ["log", "propan", "maintenance"]
status: "draft"
source_confidence: "high"
---
# Log

## 2026-10-15

- Began post-merge revalidation after `main` was merged into `rework-documentation`. The earlier first-pass audit is explicitly limited to its pre-merge code baseline.
- Replaced the obsolete multi-positional-file overlay reference with the actual single-root CLI contract, recursive `.import`/`.import once`, include search paths, preserved imported source locations, `spin2` output, and current CLI options.
- Updated symbol reference for scoped leading-dot labels, import-aware rebinding, recursive forward constant resolution and cycle detection.
- Updated source inventory and coverage entries for the newly verified behavior, and opened explicit follow-up work for the other changed language areas.
- Revalidated and rewrote the directive/data and address/segment references for `.org`, `RES`, `.regspace`, `.data`, `.fit`, `.pack`, `.pic`, `FILE`, expanded origin options, and string/sequence emission.
- Corrected the PASM2 migration summary for source imports and reservation/origin syntax. Explicitly archived pre-merge implementation findings and discrepancy classifications where upstream code superseded them.

## 2026-10-15 — post-merge continuation

- Revalidated expression values and the active P2 standard-library namespace: sequence literals and repetition, Unicode helpers, conditional source directives, `$` current-PC semantics, address conversion helpers, register/ALTI functions and 32-bit numeric/binary32 helpers.
- Replaced obsolete pre-merge claims about binary-operation panics and unsupported stdlib functions; documented the corrected generic `#false` boolean enumerator.
- Verified that explicitly conditioned NOP is now rejected, CALLD compares both pointer-register variant candidates, PIC/LOC relative rules have been extended, and indexed pointer augmentation has separate 20-bit limits.
- Corrected the previously claimed absence of emitted-segment overlap validation: the semantic emitter diagnoses overlapping nonempty HUB ranges.
- Updated worked examples, source/fixture distinctions, source coverage and future audit tasks. Full test execution and formatter audit remain pending.

## 2026-10-15 — formatter follow-up

- Added `/projects/propan/formatting.md` based on the parser-only `--pretty-print` path, `frontend/render.zig`, the golden format fixtures and existing idempotence/semantic byte-equivalence test contracts.
- Documented alignment, label folding, constant groups, expressions, comments and newline handling while distinguishing formatter acceptance from semantic assembler acceptance.
- Linked the new reference from the Propan index/tooling reference and updated coverage and TODO. No formatter tests were executed for this documentation-only edit.

## 2026-10-15 — current finding and discrepancy audit

- Rebuilt the current implementation findings and public documentation discrepancy registers from merged parser, semantic, value-helper, root README, and current tests/examples. Kept obsolete September observations explicitly archived and residual possible limitations separate from proven behavior.
- Reclassified `sumloop.propan` and `propio-client.propan` as registered assembler fixtures and `rgbx.propan` as a support-only example needing a dedicated compilation review; removed historical claims about missing directives and a now-absent `segment_management.propan` file.
- Reconciled the source inventory, coverage reference and historical TODO text. Ran static root-relative link, heading, frontmatter and fence checks across all 27 OKF Markdown files; no issues in those categories. Did not run Zig unit, formatter or example compilation tests.

## 2026-09-30

- Established the Propan documentation scope and current-state source precedence.
- Added lexical/source grammar, expression/value, symbol/declaration, address/segment, directive/data, instruction, and pointer-addressing references.
- Added source inventory, discrepancy tracking, and implementation-finding registers.
- Documented generated instruction conditions/effects, operand categories, augmentation, pointer forms, branch-address selection, and CALLD overlap behavior.
- Added the current standard-library reference and documented the existing generated stdlib HTML reference path.
- Added the current CLI/output reference covering multi-file overlay, flat/JSON output, fill behavior, list files, diagnostics, and exit status.
- Added the PASM2-to-Propan migration reference and compact source-syntax difference table.
- Added the documentation/validation coverage map.
- Added worked examples for HUB/COG execution, pointer access, conditions/effects, REP-relative labels, alignment, and address-domain conversion.
- Reviewed all current `examples/*.propan`: `rgbx.propan` is confirmed legacy against the current semantic directive table, `propio-client.propan` is current-looking but not regression-validated, and `sumloop.propan` remains semantically unverified for packed label expressions.
- Confirmed the exact unknown-escape recovery path: invalid escapes warn, preserve the backslash, and discard the unrecognized escaped character.
- Verified that explicitly conditioned `NOP` does not remain the special all-zero NOP encoding; nonzero condition bits produce a conditional zero-field ROR-form word, so conditional NOP source is documented as an implementation hazard.
- Verified `localaddr(register)` currently always emits an error despite its COG-scope TODO, and recorded that intended COG-only acceptance remains a design/implementation question.
- Verified the unused `TaggedAddress.init()` helper has no repository call site and initializes nonexistent field `.hub` rather than `hub_address`; cleanup is left outside the docs-only scope.
- Documented the current COG/LUT local-address allocation model: local addresses advance from emitted byte count, `var` does not allocate storage, and no non-emitting reservation directive is implemented.
- Added a verified sequential-segment address table from the semantic fixture and a complete mixed COG/LUT/HUB example showing local addresses alongside HUB emission addresses.
- Refreshed the Propan source inventory and coverage map to reflect the completed first-pass references, example review, verified escape/NOP findings, segment allocation work, CLI/output coverage, and remaining narrowly scoped validation/design gaps.
- Marked the broad current-syntax/semantics inventory task complete; remaining TODO items now represent unresolved design choices, focused regressions, validation automation, or cleanup of older non-OKF documentation.
- Completed an OKF-only consistency cleanup: removed stale forward-looking pointer/stdlib wording from the expression reference and updated the scope index so completed example review is no longer listed as remaining work.
- Completed the final first-pass OKF integrity audit: verified internal links and document-set navigation, checked front-matter/status consistency and terminology, corrected remaining stale coordination wording, and marked the first-pass reference set ready for project review.
