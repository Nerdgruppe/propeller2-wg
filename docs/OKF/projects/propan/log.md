---
type: "Change Log"
title: "Propan documentation log"
description: "Maintenance notes for curated Propan documentation."
tags: ["log", "propan", "maintenance"]
status: "draft"
source_confidence: "high"
timestamp: "2026-09-30T12:20:00+02:00"
---
# Log

## 2026-09-30

- Established the Propan documentation scope.
- Recorded the initial documentation-rework plan in `/TODO.md`.
- Defined current-state documentation/source precedence for an actively developed language.
- Added the first verified lexical/source-grammar reference from the Zig frontend.
- Added a source inventory separating implementation, semantic tests, parser-only tests, generated data, examples, and historical documentation.
- Added an explicit discrepancy register for non-OKF documentation that conflicts with current code.
- Added an implementation finding register for assembler defects/limitations discovered during documentation work.
- Added current semantic expression/value documentation, including integer behavior, operator associativity, address-use operators, and encoding-control helpers.
- Added the current HUB/COG/LUT tagged-address and segment model, including implemented exec-mode directives and incomplete segment-safety checks.
- Recorded the mixed implemented/planned status of `tests/propan/sema/segment_management.propan` and the latent `TaggedAddress.init()` field-name defect.
- Updated `/TODO.md` to mark documentation tasks completed by the first two reference tranches and to add follow-up verification tasks discovered during the audit.
