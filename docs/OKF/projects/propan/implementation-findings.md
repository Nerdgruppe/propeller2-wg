---
type: "Finding Register"
title: "Propan implementation findings"
description: "Audited current limitations and archived historical issues in the Propan assembler."
tags: ["propan", "assembler", "findings", "issues"]
status: "draft"
source_confidence: "high"
---
# Propan implementation findings

This register distinguishes **current confirmed behavior**, **possible limitations needing focused reproduction**, and **superseded September 2026 observations**. The current audit is based on source and existing test fixtures from `rework-documentation` as of 2026-10-15. **Tests were inspected, not executed in this documentation session.** Behavior found in source is not automatically a proven hardware/runtime defect.

## Current limitations and open interpretations

### Directive conditions and effects: possible silent no-op

The parser attaches condition/effect fields to any instruction-shaped source line. Unlike ordinary encoded P2 instructions, most directives do not process these modifiers: `select_instruction_mnemonic` assigns their internal mnemonic and `emit_code` handles data/layout without applying the P2 condition/effect encoding. Hence `if(C) BYTE 1` and `.assert 1 :wc` may be accepted without the modifier affecting semantics.

**Exceptions matter:** the `ConditionalFilter` explicitly rejects conditions/effects on `.if`/`.elif`/`.else`/`.endif` control lines, and semantic selection rejects explicitly conditioned `NOP`. Do not generalize this observation to all instruction-shaped lines. The active `examples/sumloop.propan` contains a conditioned `LONG` line, making this a source behavior that warrants a dedicated explicit regression and an intended-semantics decision.

**Evidence:** `src/propan/frontend/parser.zig`, `src/propan/sema.zig` (`ConditionalFilter.directive`, `select_instruction_mnemonic`, `emit_code`). **Status:** current source-level limitation; exact accepted directive combinations should be regression-tested.

### Unknown string escape recovery

`unescape_string` warns on an unknown `\q` escape, emits the **original backslash** and skips the `q`. This is recovery behavior rather than an alternate supported escaping convention.

**Evidence:** `src/propan/frontend/parser.zig` (`unescape_string`, invalid-escape branch). **Status:** current source-confirmed behavior; focused recovery regression still useful.

### Cross-segment and cross-domain executable references

`get_offset_for_exec_mode` returns an execution-address representation but does not universally check segment identity. It warns for HUB↔local execution-domain transitions. COG↔LUT transitions are no longer blanket errors in this helper; they can share a local branch domain. The `@` evaluator does reject mismatched execution modes and warns when segment IDs differ (rather than silently subtracting unrelated HUB addresses).

**Status:** source-confirmed guards plus **unresolved intended policy** for transfers between unrelated local-exec segments or same-mode segment origins. It would be incorrect either to promise complete cross-segment validation or to claim the old total absence of segment warnings. Relevant evidence: `get_offset_for_exec_mode`, `evaluate_expr` (`@`), `tests/propan/sema/spin2-cross-segment-rep.propan`.

### Remaining defensive pointer-variant panic

The `CALLD`/pointer-register variant selector now checks **both** candidates for pointer-register operands and uses `select_calld_ambiguous_encoding` to distinguish ordinary and special CALLD encodings. If **both** candidate variants contain `pointer_reg`, the selector still contains `@panic("incredibly amgigious instructions, should check the setup")`. Ordinary ambiguity produces `err_ambigious_instruction_selection_for`.

**Status:** code-level defensive panic exists; **reachability from current generated variants has not been demonstrated**. This is distinct from the fixed September copy/paste error. Evidence: `src/propan/sema.zig` (`select_instruction_encoding`), `tests/propan/sema/pointer-variant-order.propan` and `tests/propan/equivalence/ambigious.propan`.

### Layout constants depend on evaluation phase

The current assembler recursively resolves forward constant dependencies, including label-independent constants used to determine layout. However, a constant that depends on a **label address** cannot be resolved during the layout pass, before those address values are available. Such dependencies are diagnosed.

**Evidence:** `resolve_constant` (`forbid_label_addresses = ana.in_layout`), `tests/propan/sema/lazy-constants.propan`, `tests/propan/sema/diagnostics/layout-constant-label.propan` and `layout-constant-known-label.propan`. **Status:** current and intentional phase constraint; not a blanket prohibition on user constants in `.org` or `.align`.

## Superseded September observations

The earlier reference captured behavior of a pre-merge Zig implementation. The table preserves the observation and records the current replacement; **none of the historical claims below is a current defect merely because the old code once exhibited it**.

| September observation | Audited merged state | Evidence |
|---|---|---|
| CALLD pointer-register guard compared `any_ptrreg_prev` to itself | Fixed: compares `any_ptrreg_prev` and `any_ptrreg_now`; a distinct double-pointer-register panic remains unproven reachable | `sema.zig` variant selector |
| Binary operations on equal-type address/string operands called `@panic` | Fixed: address/string/sequence invalid binary operations emit typed source diagnostics | `sema.zig` evaluator; `diagnostics/unsupported-binary-types.propan` |
| `BYTE`/`WORD`/`LONG` raw string emission panicked | Fixed: strings expand to elements; integer sequences/repetition and Unicode helpers also work | `sema.zig` data expression handling; `array-emission.propan` |
| All user-defined constants failed in layout directives | Fixed for **label-independent** recursive/forward constants; label-dependent layout values are still restricted | `resolve_constant`; `lazy-constants.propan` |
| Explicitly conditioned `NOP` became a conditional `ROR` word | Fixed: semantic error `err_nop_cannot_have_condition` | `sema.zig`, `diagnostics/conditional-nop.propan` |
| `localaddr(register)` always failed | Fixed: supported with a warning in COG execution context; rejected outside it | `sema.zig` address builtins; `diagnostics/localaddr-register-modes.propan` |
| Segment overlap validation was disabled | Fixed for **nonempty emitted HUB segments**: pairwise range intersection emits a semantic error | `sema.zig` `emit_code` overlap check |
| `.assert` wrong-type message reported the condition type | Fixed: message validation now diagnoses `msg.value` | `sema.zig` `evaluate_asserts`; `diagnostics/assert-needs-string-message.propan` |
| `TaggedAddress.init()` used nonexistent `.hub` field | Fixed: uses `.hub_address`; current layout code calls this constructor | `stdlib/eval.zig`; `sema.zig` layout |
| `@` ignored segment identity and execution domain | Fixed in part: mismatched execution modes diagnose and cross-segment relative references warn; whether to disallow all cross-segment references remains a design decision | `sema.zig` `evaluate_expr`; `spin2-cross-segment-rep.propan` |

Enum values are still not arbitrary integer data: raw enumeration payloads fail numeric type checks unless an instruction/function explicitly consumes the enum.

## Evidence and maintenance rules

- **Official/hardware semantics** belong to canonical Propeller 2 instruction material, not inferred from assembler implementation details.
- **Current source behavior** means the branch implementation was inspected. Cite particular source functions and distinguishing fixtures; do not claim a test passed unless executed.
- **Potential bug** means a code branch or inconsistency exists but its operational reachability or intended behavior is unresolved.
- **Historical** means the finding was valid or plausibly derived from an earlier version, not current verified behavior.

When a limitation is fixed, move it to the superseded section or maintenance log and update the relevant canonical reference. For current instruction and address rules, see [/projects/propan/instruction-syntax.md](/projects/propan/instruction-syntax.md), [/projects/propan/pointer-addressing.md](/projects/propan/pointer-addressing.md), and [/projects/propan/expressions.md](/projects/propan/expressions.md).
