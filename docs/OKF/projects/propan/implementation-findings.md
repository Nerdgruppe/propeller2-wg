---
type: "Finding Register"
title: "Propan implementation findings"
description: "Potential assembler issues and implementation limitations discovered while documenting current Propan behavior."
tags: ["propan", "assembler", "findings", "issues"]
status: "draft"
source_confidence: "high"
---
# Propan implementation findings

This page records potential assembler defects or limitations discovered during documentation work. It is not a substitute for the repository issue tracker. Findings remain here while they materially affect how current behavior should be documented.

## Confirmed code-level findings

### Pointer-register ambiguity check contains a copy/paste defect

Instruction selection computes `any_ptrreg_prev` and `any_ptrreg_now`, but the ambiguity guard currently tests `any_ptrreg_prev` twice:

```zig
if (any_ptrreg_prev == true and any_ptrreg_prev == true) {
    @panic("incredibly amgigious instructions, should check the setup");
}
```

This means the guard does not express the apparent intended condition of both competing variants being pointer-register variants. It can also terminate the assembler with a process panic rather than a source diagnostic if reached.

The generated table makes this relevant to `CALLD`: the ordinary `CALLD D,{#}S**` form overlaps in source type space with `CALLD PA/PB/PTRA/PTRB,#{\}A`. The intended selector preference is to choose a matching `pointer_reg` variant over an ordinary register variant. Existing equivalence material exercises several cases but leaves some special-destination label/numeric forms commented out, so the complete overlap surface is not regression-proven.

**Documentation impact:** the current selection rule and CALLD forms can be described, but pointer-register overlap should be treated as a known current limitation rather than fully hardened behavior. Add focused regression coverage when the ambiguity guard is corrected.

### Binary operations on address values and strings can panic

After type equality is established, binary expression evaluation handles integer/register/enumerator/pointer-expression values but uses `@panic` for `.address` and `.string` operands.

**Documentation impact:** do not imply that all syntactically accepted binary expressions are safely diagnosed. Address/string binary expressions are currently an implementation hazard.

### Raw string and enumerator data emission can panic

The generic value-to-data cast used by `BYTE`, `WORD`, and `LONG` contains `@panic("string emission not supported yet")` for strings and an internal-error panic for unresolved enumerators.

**Documentation impact:** strings are valid expression values but are not currently supported as raw data-emission arguments. Enumerator values require a consuming instruction enumeration context rather than direct emission.

### Layout directives cannot currently use ordinary user constants

Location assignment runs before `evaluate_constant_values()`. `.align` and explicit `.cogexec`/`.lutexec`/`.hubexec` HUB-address operands are evaluated during that earlier layout pass, when user constant symbols exist but their values are still unset.

As a result, a declared user constant cannot currently be used for those layout values. `.align` also maps the resulting `UndefinedSymbol` evaluation failure to a message saying labels cannot be referenced, which is misleading for this case.

**Documentation impact:** directive documentation must distinguish expressions accepted grammatically from values available during layout. This ordering may warrant implementation changes if user constants are intended for layout directives.

### Conditions/effects on assembler directives are silently ignored

The parser stores optional conditions and effects on every instruction-shaped line, including assembler directives. Semantic encoding validates and applies those fields only for generated encoded instructions; directive paths such as `BYTE`, `.align`, and `.assert` bypass that handling.

As a result, source such as `if(C) BYTE 1` or `.assert 1 :wc` can be accepted while the condition/effect has no directive meaning and is ignored.

**Documentation impact:** conditions/effects are documented as generated-instruction syntax only. Directive modifiers should not be recommended until the assembler rejects them or defines explicit semantics.

### Explicitly conditioned `NOP` encodes a different instruction form

The generated `NOP` entry is the special all-zero P2 word. Emission forces condition code `0000` only when the source has no explicit condition. If a condition is supplied, the requested condition code replaces the high four bits while the lower 28 bits remain zero.

Those lower 28 bits are also the base encoding of `ROR D,{#}S` with C/Z/I, D, and S all zero. A source form such as `if(Z) NOP` therefore does not remain a NOP word; it produces a conditionally executed zero-field ROR form (`ROR r0, r0` in register interpretation).

**Documentation impact:** explicitly conditioned NOP must not be recommended. Parser acceptance is an encoding defect/hazard until the assembler rejects such source or models NOP as a fixed complete word that cannot accept a condition.

### Unknown escape recovery drops the unrecognized escaped character

The string unescaper stores the current byte as `char`. When `char` is a backslash it increments the input index to inspect the escaped character. In the invalid-escape branch it emits a warning using that following byte but appends the saved `char`, which is still the original backslash. The loop then advances past the inspected byte.

Thus `\q` currently recovers as a single backslash byte while `q` is discarded.

**Documentation impact:** the exact recovery is now documented, but invalid escapes should not be relied on as intentional language syntax. A regression test would still be useful to lock down the behavior if recovery semantics are intended to remain stable.

### `localaddr(register)` always diagnoses an error

The evaluator has a register branch for `cogaddr()`, `lutaddr()`, and `localaddr()`. `lutaddr(register)` is rejected directly. The `localaddr(register)` branch contains a TODO to check whether execution mode is COG, but currently emits `localaddr() is only valid for registers in a cogexec scope` unconditionally, then also emits the generic warning before returning the numeric register index internally.

Because an error diagnostic marks semantic analysis unsuccessful, `localaddr(register)` is not currently a successful interface even in COG-exec scope.

**Documentation impact:** document register use as unsettled/invalid current behavior. The TODO suggests intended COG-only acceptance, but that intent is not implemented and should not be promoted to language contract without a project decision.

### Segment-overlap validation is disabled

Location assignment contains a commented-out overlap check with a `TODO: Reinclude the overlap check!`. Moving a new HUB segment backwards emits a warning, but the intended overlap error is not active there.

**Documentation impact:** current multi-segment/overlay behavior must be described carefully. Do not state that accidental overlap is rejected until this is implemented and tested.

### `.assert` wrong-type message reports the condition type

When a second `.assert` argument is present but is not a string, the diagnostic formats `@tagName(condition.value)` rather than the type of the message expression.

**Documentation impact:** no language-semantic impact, but diagnostics may be misleading for this error case.

### `TaggedAddress.init()` initializes the wrong field name

`TaggedAddress` contains a `hub_address` field, but its generic `init()` helper currently returns a struct literal using `.hub = hub`. Repository search found no current `TaggedAddress.init(` call site, so Zig's lazy function analysis can allow this latent defect to remain unnoticed while the helper is unused.

**Documentation impact:** none for currently exercised address construction, which uses the specialized `init_hub`/`init_cog`/`init_lut` helpers. The generic helper is verified unused by repository search and malformed as written; it should be corrected or removed before use.

## Findings requiring verification

### `@` relative-offset operator does not validate segment identity

The evaluator contains an explicit TODO to verify that the current location and target address belong to the same segment before calculating the longword delta from HUB addresses.

**Documentation impact:** the current address reference documents the implemented HUB-address calculation and this missing validation separately.

### Cross-local-segment jump checking is incomplete

`get_offset_for_exec_mode()` rejects COG↔LUT execution-domain transitions and warns for HUB↔local transitions, but its own comment notes that segment identity still needs to be incorporated into the protection logic so unrelated local-exec segments cannot be confused merely because their local addresses overlap.

**Documentation impact:** address-domain diagnostics should be described as current safeguards, not as complete segment-safety validation.

## Documentation policy for findings

- Describe current behavior, not desired behavior.
- Mark unexecuted code-reading conclusions as potential until sufficiently determined by explicit control flow or a test/reproducible invocation.
- If a finding is fixed, retain a short historical note only if it explains documentation/version differences; otherwise remove it from the current-state reference and note the change in `log.md`.
