# Temporary Propan documentation TODO

This file is a temporary worklist for the Propan documentation rework. Items should move into durable documentation, tests, scripts, or normal issue tracking as they are resolved. Remove this file when it is no longer useful as a coordination artifact.

## 1. Establish the authoritative language contract

- [ ] Define which documents under `docs/OKF/` are normative descriptions of current Propan behavior and clearly mark design notes, historical notes, and proposals as non-normative.
- [ ] Inventory the current syntax and semantics from `src/propan/`, parser/sema tests, equivalence tests, examples, the root `README.md`, and `docs/propan/semantics.md`.
- [ ] Identify stale syntax in examples or older documentation and decide whether it remains supported, is an alias, or should be removed from documentation.
- [ ] Document the relationship between Propan and PASM2: which mnemonics/operand orders are inherited directly and which syntax intentionally differs.
- [ ] State case-sensitivity rules for mnemonics, identifiers, directives, effects, constants, and standard-library names.
- [ ] State whitespace/newline rules, including multiline instruction arguments and multiline function calls.
- [ ] Document comment syntax and any lexical edge cases.

## 2. Lexical elements and values

- [ ] Document identifier syntax, including local-label syntax and any special identifier forms.
- [ ] Document decimal, hexadecimal, binary, and quaternary integer literals and `_` separators.
- [ ] Document character literals, string literals, escape sequences, and encoding expectations.
- [ ] Document enumerator/value-token syntax such as `#15pF` and explain where such values are defined/usable.
- [ ] Document integer width, signedness model, overflow/wrapping behavior, and conversion rules used by constant evaluation.
- [ ] Document whether null/boolean values are user-visible language concepts or only internal evaluation values.

## 3. Expressions

- [ ] Produce one complete operator table with precedence and associativity.
- [ ] Document unary `+`, unary `-`, boolean `!`, bitwise `~`, `@`, `&`, `*`, pointer pre/post increment/decrement, and indexing.
- [ ] Document boolean `and`, `or`, `xor`, comparisons, arithmetic, bitwise operators, shifts, and ternary expressions.
- [ ] Document builtin expression functions such as `abs`, `hubaddr`, shifts/rotates, bit helpers, floating-point helpers, and any target-specific helpers.
- [ ] Separate normal language operators from PASM/Spin spellings used only as comparison material.
- [ ] Define which operand types each operator/function accepts and how invalid type combinations are diagnosed.
- [ ] Document any expression forms that parse but are intentionally unsupported semantically.

## 4. Symbols, constants, labels, and variables

- [ ] Document `const` declarations completely, including forward references and evaluation order.
- [ ] Document ordinary labels and local labels, including scope and duplicate-name rules.
- [ ] Document `var` declarations and the distinction between a symbol, storage allocation, and emitted initialization data.
- [ ] Explain symbol namespaces and collisions between constants, labels, variables, standard-library names, and instruction names.
- [ ] Document forward-reference behavior and undefined-symbol diagnostics.

## 5. Address model

- [ ] Write a dedicated conceptual reference for Propan's HUB, COG, and LUT address domains.
- [ ] Define a label's native address and how its containing segment/execution mode affects interpretation.
- [ ] Define the exact semantics and legal contexts of `label`, `&label`, `*label`, `@label`, `hubaddr(label)`, and any COG/LUT/local-address helpers.
- [ ] Explain byte addresses versus long/register addresses and where automatic conversions do or do not occur.
- [ ] Document relative versus absolute branch/call selection and augmentation behavior.
- [ ] Document warnings/errors for crossing execution modes or segments, including any current implementation limitations.
- [ ] Provide examples that deliberately contrast the same label viewed in different address domains.

## 6. Segments, cursors, and execution modes

- [ ] Document the current segment model: HUB anchor, emitted span, execution mode, and program-counter interpretation.
- [ ] Document `.hubexec`, `.cogexec`, and `.lutexec` precisely.
- [ ] Document `.huborg`, `.cogorg`, `.lutorg`, `.org`, and any aliases or compatibility forms; identify the preferred spelling for new code.
- [ ] Document rules for moving cursors forward/backward and when a new segment is created.
- [ ] Document COG/LUT register allocation and its relationship to HUB emission.
- [ ] Explain how multiple source files contribute segments and how overlapping emitted ranges are handled.
- [ ] Include one complete worked layout combining COG code, LUT code if supported, HUB data, and reserved registers.

## 7. Directives and data declaration

- [ ] Create a complete directive reference generated or verified against the parser/sema implementation.
- [ ] Document `BYTE`, `WORD`, `LONG`, `.regs`, `.res`/`.RES` if supported, alignment directives, `.fit`/`.cogfit` and related forms, assertions, and section/origin directives.
- [ ] For every directive, document argument grammar, allowed expression types, emitted byte count, cursor effects, alignment behavior, and failure conditions.
- [ ] Clarify preferred modern syntax versus retained compatibility aliases.
- [ ] Document string/data emission behavior and endianness.

## 8. Instruction syntax

- [ ] State the general instruction grammar: optional condition, mnemonic, operands, optional effects.
- [ ] Document all condition forms and their exact P2 condition-code mapping.
- [ ] Document all effect suffixes (`:wc`, `:wz`, `:wcz`, TESTxx effects, etc.) and which instruction classes permit them.
- [ ] Define destination/source operand categories used by Propan and map them to PASM2 terminology.
- [ ] Document immediate values, addresses, pointer-register forms, augmented immediates, and special-register operands.
- [ ] Document the ambiguous instruction families where selection rules matter, especially CALLD and pointer-register cases.
- [ ] Decide whether the instruction reference should be generated from the assembler's instruction database; prefer generated/verifiable material over a manually duplicated 400+ instruction table.
- [ ] Add or generate a concise list of Propan-specific instruction-form deviations from canonical PASM2.

## 9. Pointer addressing

- [ ] Document `PTRA`/`PTRB` direct forms.
- [ ] Document `PTRA++`, `PTRA--`, `++PTRA`, `--PTRA` and corresponding PTRB forms.
- [ ] Document indexed pointer forms and the legal index ranges for each encoding family.
- [ ] Explain pre/post update timing in terms of the hardware addressing operation.
- [ ] Explain when pointer syntax is an encoded P2 pointer operand versus an ordinary expression.

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

- [ ] Ensure an agent can determine valid source-line grammar without reading `parser.zig`.
- [ ] Ensure an agent can determine address-domain semantics without reading `sema.zig`.
- [ ] Ensure all currently supported directives are discoverable from documentation alone.
- [ ] Ensure preferred syntax is explicit where multiple aliases are accepted.
- [ ] Add compact tables for grammar, directives, address operators, conditions, effects, and PASM2 differences because these are high-value retrieval targets.
- [ ] Keep examples syntactically valid and testable; where practical, make documentation examples part of automated validation.
- [ ] Avoid AI-specific prose where ordinary precise language documentation serves both humans and agents better.

## 14. Provenance, verification, and maintenance

- [ ] Create a source inventory under `/references/` covering the current implementation, tests, generated instruction data, repository examples, existing Propan docs, and official P2 material used to explain hardware behavior.
- [ ] Define source precedence for disagreements between current implementation, tests, design notes, and historical examples.
- [ ] Add a documentation coverage page mapping language features to normative pages and validation tests.
- [ ] Add link checking and, if useful, generated-reference consistency checks to the normal validation workflow.
- [ ] Record meaningful documentation changes in scope-local `log.md` files.
- [ ] Move unresolved implementation defects out of documentation TODOs unless they directly block documenting the intended language contract.
- [ ] Retire or rewrite `docs/propan/semantics.md` once its still-valid concepts have been incorporated into normative OKF pages.
- [ ] Rework the Propan portion of the root `README.md` after the OKF language reference is stable, keeping the README concise and linking to the canonical reference.

## Suggested first-pass document set

The exact split should follow the material discovered during the audit, but a likely compact set is:

- `/projects/propan/overview.md`
- `/projects/propan/language-reference.md`
- `/projects/propan/expressions.md`
- `/projects/propan/addresses-and-segments.md`
- `/projects/propan/directives-and-data.md`
- `/projects/propan/instruction-syntax.md`
- `/projects/propan/stdlib.md`
- `/projects/propan/tooling.md`
- `/projects/propan/pasm2-differences.md`
- `/projects/propan/examples.md`
- `/references/propan-sources.md`
- `/references/propan-coverage.md`
