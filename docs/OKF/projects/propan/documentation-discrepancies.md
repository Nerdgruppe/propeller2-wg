---
type: "Reference"
title: "Propan documentation discrepancies"
description: "Current verified conflicts and archived September classifications across public Propan documentation and examples."
tags: ["propan", "documentation", "conflicts", "migration"]
status: "draft"
source_confidence: "high"
---
# Propan documentation discrepancies

This is an audit of documentation and examples **outside** `docs/OKF/` against the Zig assembler on `rework-documentation` as of 2026-10-15. It distinguishes public syntax conflicts, parser-only evidence, design notes, and examples registered for assembly tests. The root README and other prose are not automatically the language authority; see [/projects/propan/status-and-source-precedence.md](/projects/propan/status-and-source-precedence.md).

## Current confirmed documentation differences

### Root README: ternary operator advertised but not parsed

The root `README.md` lists a Propan `? :` ternary expression in its operator comparison. The current tokenizer recognizes `?` and `:` tokens, but `src/propan/frontend/parser.zig` has no ternary expression production. **Classification:** current documentation/implementation conflict. The OKF does not list ternary expressions as supported syntax; implementation or README should eventually be reconciled.

### Root README: octal literal and effect alias coverage

The current tokenizer/number parser recognizes `0o...` octal literals while the README's visible numeric examples omit them. The parser accepts compact effects `:andc`, `:andz`, `:orc`, `:orz`, `:xorc`, `:xorz` and `:wzc` in addition to the longer canonical forms shown in the README. **Classification:** implemented/under-documented, not evidence of a broken parser. The OKF lexical and instruction references include these forms.

### Root README: historical multi-input CLI example

The README's embedded sample usage advertises `<sources...>`, whereas the current CLI accepts **exactly one positional root source** plus recursively expanded `.import` files. The merged test suite covers multiple-positional-input rejection. **Classification:** stale help/example material; see [/projects/propan/tooling.md](/projects/propan/tooling.md). This does not imply the current `--help` output necessarily still uses the old wording.

### Design note: `docs/propan/semantics.md`

This file combines older conceptual notation (`.section` and proposed origin mappings) with subsequently added implementation-oriented details for `.org`, `.fit`, `.pack`, `.pic`, conditional source selection and imports. Treat it as a **mixed design/history document**, not uniformly unsupported or uniformly normative. Verify accepted directive forms against `src/propan/sema.zig` and import handling.

### Parser fixtures do not prove semantic directives

`tests/propan/parser/directives.propan` includes forms such as `.cogfit` and reserved register declarations. A parser fixture proves tokens can be read; it does not establish registration as a supported semantic directive. The current semantic mnemonic table implements `.org`, `RES` (without leading dot), `.fit`, `.data`, `.regspace` and other current forms, but not every parser-only spelling (for example `.cogfit`). **Classification:** validation-scope distinction, not automatically a bug.

## Current example classification

| File | Evidence at current HEAD | Classification and limitations |
|---|---|---|
| `examples/propio-client.propan` | Included in `tests/propan/cases.zig` `examples` group. Uses `const`, `var`, `aug`, smart-pin helpers and current local labels. | **Registered assembly fixture**; not a hardware-runtime certification. |
| `examples/sumloop.propan` | Included in the same `examples` group. Current source uses `alti.state(...)` and `alti.config(...)`, not the old direct `buf_c << 18` address construction. | **Registered assembly fixture**; the old September incompatibility claim is superseded. Hardware execution is a separate question. |
| `examples/rgbx.propan` | Listed in `support_files` rather than the active `examples` group. Uses the now-implemented `.org`, `RES` and `.fit` spellings. | **Needs end-to-end assembly review**; no longer established as legacy merely by those directive names. No full successful assembly run is claimed here. |

Being listed in a test manifest is evidence of coverage intent and test registration. It does not prove the tests were run in this documentation session, nor does it prove runtime behavior on hardware.

## September classifications retained only as history

The September reference reflected an earlier semantic directive table and local-label model. It classified `.org`/`RES`/`.fit`/`.data`/`.regspace` as unsupported, leading-dot names as global, `sumloop.propan` as using unsupported address/bitshift combinations, and the project examples as absent from the active assembler fixture suite.

**Those classifications are obsolete for the current branch.** `tests/propan/sema/segment_management.propan`, previously named in that register, is also **not present at current HEAD**; this missing historical path must not be cited as a passing present-day fixture.

The original historical rationale can be recovered from pre-merge commits and the 2026-09-30 documentation log. It should not be recopied into current references as a claim about the merged compiler.

## Maintenance and unresolved decisions

Keep unresolved design choices explicit: whether to implement the advertised ternary syntax, whether to update public README syntax tables and usage snapshots, and whether to migrate/register `rgbx.propan` as an executable conformance example. A final assembled/hardware-verified classification requires actually running the relevant tools.

Future entries should identify the external claim, source/fixture evidence, current-versus-historical status, and concrete correction or verification required.
