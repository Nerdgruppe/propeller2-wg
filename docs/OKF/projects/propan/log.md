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
