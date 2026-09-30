# Temporary Propan documentation TODO

This file is a temporary worklist for the Propan documentation rework. Items should move into durable documentation, tests, scripts, or normal issue tracking as they are resolved. Remove this file when it is no longer useful as a coordination artifact.

## 1. Establish the authoritative language contract

- [x] Define which documents under `docs/OKF/` are current-state descriptions of Propan behavior and clearly mark design notes, historical notes, and proposals as non-normative.
- [ ] Inventory the current syntax and semantics from `src/propan/`, parser/sema tests, equivalence tests, examples, the root `README.md`, and `docs/propan/semantics.md`.
- [ ] Identify stale syntax in examples or older documentation and decide whether it remains supported, is an alias, or should be removed from documentation.
- [ ] Document the relationship between Propan and PASM2: which mnemonics/operand orders are inherited directly and which syntax intentionally differs.
- [x] State case-sensitivity rules for mnemonics, identifiers, directives, effects, constants, and standard-library names.
- [x] State whitespace/newline rules, including multiline instruction arguments and multiline function calls.
- [x] Document comment syntax and lexical edge cases currently established by the Zig frontend.

## 2. Lexical elements and values

- [x] Document identifier syntax completely, including the current absence of special local-label scoping and special identifier forms.
- [x] Document decimal, hexadecimal, binary, quaternary, and implemented octal integer literals plus `_` separators.
- [x] Document character literals, string literals, escape sequences, and encoding expectations currently implemented by the parser.
- [ ] Document enumerator/value-token syntax such as `#15pF` and explain where such values are defined/usable; encoded-instruction enumerators are now covered, but standard-library enum namespaces/use sites remain.
- [x] Document integer width, signedness model, wrapping arithmetic, shift limits, and narrowing/truncation behavior used by semantic evaluation.
- [x] Document whether boolean values are user-visible language concepts or integer results; current implementation has no separate boolean semantic type.
- [ ] Verify and document the exact recovery result for unknown escape sequences; current code inspection suggests the escaped character may be dropped.

## 3. Expressions

- [x] Produce the current implemented binary-operator table with precedence and associativity.
- [x] Document unary `+`, unary `-`, boolean `!`, bitwise `~`, `@`, `&`, `*`, pointer pre/post increment/decrement, and indexing at the current semantic level.
- [x] Document boolean `and`, `or`, `xor`, comparisons, arithmetic, bitwise operators, shifts, and the current ternary-syntax conflict.
- [ ] Document all builtin/stdlib expression functions such as `abs`, shifts/rotates, bit helpers, floating-point helpers, clock helpers, and target-specific helpers.
- [x] Separate current Propan operators from PASM/Spin spellings used only as comparison material.
- [ ] Define which operand types each standard-library function accepts and how invalid argument combinations are diagnosed.
- [x] Document expression forms that are tokenized/documented externally but not currently implemented semantically.
- [ ] Decide intended negative-operand semantics for `/` and `%` and verify that current `@divFloor`/`@mod` behavior matches the desired Propan contract.

## 4. Symbols, constants, labels, and variables

- [x] Document `const` declarations completely, including current source-order evaluation and forward-reference behavior.
- [x] Document ordinary labels and the current absence of special local-label scoping, including duplicate-name rules.
- [x] Document `var` declarations and the distinction between a symbol, storage allocation, and emitted initialization data.
- [x] Explain symbol namespaces and collisions between constants, labels, variables, standard-library names, functions, and instruction names.
- [x] Document forward-reference behavior and undefined-symbol diagnostics.
- [ ] Decide whether forward references between user constants should remain unsupported or gain dependency-based/repeated evaluation.

## 5. Address model

- [x] Write a dedicated conceptual reference for Propan's HUB, COG, and LUT address domains.
- [x] Define a label's tagged/native address and how its containing segment/execution mode affects interpretation.
- [x] Define the current semantics and legal contexts of `label`, `&label`, `*label`, `@label`, `hubaddr(label)`, `cogaddr(label)`, `lutaddr(label)`, and `localaddr(label)`.
- [x] Explain HUB byte addresses versus COG/LUT long/register addresses and where conversion occurs.
- [ ] Document relative versus absolute branch/call selection and augmentation behavior completely. Generic `address` auto-selection plus `aug()`/`nrel()` are now documented; PC-relative register-or-immediate/CALLD-specific behavior remains.
- [x] Document current warnings/errors for crossing execution modes or segments, including explicitly incomplete segment validation.
- [x] Provide examples that contrast local and HUB interpretations of the same label.
- [ ] Resolve/document the intended behavior of `localaddr(register)`; current code emits an error while also containing a TODO about execution-mode validation.

## 6. Segments, cursors, and execution modes

- [x] Document the current segment model: HUB anchor, segment identity, execution mode, and local program-counter interpretation.
- [x] Document `.hubexec`, `.cogexec`, and `.lutexec` as currently implemented, including optional HUB-address arguments.
- [x] Classify `.huborg`, `.cogorg`, `.lutorg`, `.org`, `.reserve`, `.regspace`, `.data`, and related names as non-current until semantic support is independently verified.
- [x] Document current cursor relocation behavior exposed through exec-mode directives and the warning on backwards HUB movement.
- [ ] Document COG/LUT register allocation and its relationship to HUB emission in enough detail for reserved-register/data workflows.
- [ ] Explain how multiple source files contribute segments and how overlapping emitted ranges are handled across modules.
- [ ] Include one complete worked layout combining COG code, LUT code if supported, HUB data, and reserved registers.
- [ ] Determine the intended future status of the broader segment model described in `tests/propan/sema/segment_management.propan`.
- [ ] Decide whether layout directives should be able to use ordinary user `const` values; current evaluation order makes those values unavailable during layout.

## 7. Directives and data declaration

- [x] Create a complete current directive reference verified against the semantic mnemonic table.
- [x] Document `BYTE`, `WORD`, `LONG`, `.align`, `.assert`, `.cogexec`, `.lutexec`, `.hubexec`, and classify `.regs`, `.res`/`.RES`, `.fit`/`.cogfit`, `.org` and related forms by actual support status.
- [x] For every currently supported directive, document argument grammar, allowed/effective expression types, emitted byte count, cursor effects, alignment behavior, and failure conditions.
- [x] Clarify preferred current syntax and state that no current compatibility aliases for the implemented directive set have been established.
- [x] Document data emission endianness and current string/enumerator emission limitations.
- [ ] Decide intended string-data syntax; current direct `BYTE`/`WORD`/`LONG` string emission reaches a panic rather than expanding or diagnosing the value.
- [ ] Decide whether conditions/effects on assembler directives should be rejected or defined; they are currently parsed and then ignored.

## 8. Instruction syntax

- [x] State the full semantic instruction grammar: optional condition, mnemonic, operands, optional effects.
- [x] Document all condition forms and their exact current P2 condition-code mapping.
- [x] Document all accepted effect suffixes and aliases, the main generated effect classes, and per-variant effect validation.
- [x] Define destination/source operand categories used by Propan and map them to PASM2 terminology.
- [x] Document immediate values, address operands, pointer-expression/pointer-register forms, augmentation, enumerated operands, and special-register selectors at the generic instruction-model level.
- [ ] Document the ambiguous instruction families where selection rules matter, especially CALLD and pointer-register cases.
- [x] Decide that the complete instruction reference should be generated/verified from the assembler instruction database rather than maintained as a handwritten 400+ instruction table.
- [ ] Add or generate a concise list of Propan-specific instruction-form deviations from canonical PASM2.
- [ ] Verify the intended semantics/support of explicitly conditioned `NOP`; no-condition NOP is currently special-cased to condition code `0000`.

## 9. Pointer addressing

- [ ] Document `PTRA`/`PTRB` direct forms.
- [ ] Document `PTRA++`, `PTRA--`, `++PTRA`, `--PTRA` and corresponding PTRB forms.
- [ ] Document indexed pointer forms and legal index ranges: current encoder uses signed 6-bit index for non-updating forms and magnitude `1...16` for updating forms.
- [ ] Explain pre/post update timing in terms of the hardware addressing operation.
- [x] Explain at the expression-model level that pointer syntax becomes a dedicated encoded PTRA/PTRB pointer expression rather than a general arithmetic expression.
- [ ] Reconcile/document pointer-register instruction-selection ambiguity after the existing copy/paste defect is fixed or explicitly accepted as current limitation.

## 10. Standard library

- [ ] Inventory all built-in constants and functions from `src/propan/stdlib/`.
- [ ] Separate common language builtins from P1/P2-target-specific names.
- [ ] Document argument names, allowed arities, default/named arguments if any, return/value types, and edge behavior.
- [ ] Document target-defined constants such as smart-pin modes, clock-mode helpers, register names, and enumerated configuration values.
- [ ] Mark unverified or experimental standard-library extensions as such until checked against authoritative hardware documentation.
- [ ] Prefer generated standard-library reference material if the definitions already contain sufficient metadata/doc text.

## 11. Source-file and assembler behavior

- [ ] Document how multiple input files are assembled and overlaid into a single output.
- [ ] Document flat and JSON output formats and the role of `--fill-byte`.
- [ ] Document list-file behavior sufficiently for users interpreting addresses and segment ownership.
- [ ] Document warning versus error behavior and assembler exit status at a user-facing level.
- [ ] Document the expected file extension and any include/import mechanism if one exists; explicitly state if source inclusion is not supported.
- [ ] Explicitly document that segment-overlap rejection is currently disabled and distinguish intended future validation from current output behavior.

## 12. Examples and migration aids

- [ ] Add a minimal complete HUB-exec program.
- [ ] Add a minimal COG-exec program with HUB-resident data.
- [ ] Add a pointer-memory-access example.
- [ ] Add a conditions/effects example that demonstrates flag flow.
- [ ] Add a REP/relative-label example.
- [ ] Add a data-layout/alignment example.
- [ ] Add a PASM2 → Propan syntax-difference cheat sheet focused on common accidental carry-over from Spin2/PASM2.
- [ ] Review existing `examples/*.propan` and either modernize them or explicitly label legacy/compatibility syntax.

## 13. Agent-oriented usability

- [x] Ensure an agent can determine the current top-level source-line grammar without reading `parser.zig`.
- [x] Ensure an agent can determine the current HUB/COG/LUT address-domain model without reading `sema.zig`.
- [x] Ensure all currently supported assembler directives are discoverable from documentation alone.
- [x] Ensure preferred current directive syntax is explicit where historical/proposed alternatives exist.
- [ ] Add compact tables for pointer encodings and PASM2 differences; grammar, operators, address helpers, directives, conditions, and effects are already tabulated.
- [ ] Keep examples syntactically valid and testable; where practical, make documentation examples part of automated validation.
- [x] Avoid AI-specific prose where ordinary precise language documentation serves both humans and agents better.

## 14. Provenance, verification, and maintenance

- [x] Create a source inventory under `/references/` covering the current implementation, tests, generated instruction data, repository examples, existing Propan docs, and official P2 material used to explain hardware behavior.
- [x] Define source precedence for disagreements between current implementation, tests, design notes, and historical examples.
- [ ] Add a documentation coverage page mapping language features to current-state pages and validation tests.
- [ ] Add link checking and, if useful, generated-reference consistency checks to the normal validation workflow.
- [x] Record meaningful documentation changes in scope-local `log.md` files.
- [x] Move implementation defects into the dedicated `/projects/propan/implementation-findings.md` register instead of leaving them as undifferentiated documentation TODOs; retain specific verification/documentation tasks here when they still block accurate docs.
- [ ] Retire or rewrite `docs/propan/semantics.md` once its still-valid concepts have been incorporated into current-state OKF pages and project intent is confirmed.
- [ ] Rework the Propan portion of the root `README.md` after the OKF language reference is sufficiently complete, keeping the README concise and linking to the current-state reference.
- [ ] Verify whether the unused `TaggedAddress.init()` helper should be fixed or removed; it currently initializes `.hub` although the struct field is named `hub_address`.

## Suggested first-pass document set

Current/planned compact set:

- `/projects/propan/status-and-source-precedence.md` — created
- `/projects/propan/lexical-and-source-grammar.md` — created
- `/projects/propan/expressions.md` — created
- `/projects/propan/symbols-and-declarations.md` — created
- `/projects/propan/addresses-and-segments.md` — created
- `/projects/propan/directives-and-data.md` — created
- `/projects/propan/instruction-syntax.md` — created
- `/projects/propan/stdlib.md`
- `/projects/propan/tooling.md`
- `/projects/propan/pasm2-differences.md`
- `/projects/propan/examples.md`
- `/projects/propan/documentation-discrepancies.md` — created
- `/projects/propan/implementation-findings.md` — created
- `/references/propan-sources.md` — created
- `/references/propan-coverage.md`
