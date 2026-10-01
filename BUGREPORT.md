# Bug Reports

Audit update (2026-10-01): fixed 20 newly identified issues and the two previously unresolved issues below. Added 28 `.propan` fixtures and three Zig regression test files. Original reproduction snippets describe behavior before the fixes; checklist fixtures assert the corrected behavior.

Validation: `zig-0.16.0 build install test -Dwith-flexspin -j4` passed, including 52 unit tests and the parser, semantic, output, and FlexSpin equivalence tests. The regenerated `.coverage/kcov-merged/cov.xml` reports 91.5% line coverage (previously 87.8%). A separate sweep of 364 expression and layout boundary cases found no crashes. `git diff --check` passed.

## Unary plus negated integer expressions

- **Bug:** The evaluator handled unary `+` like unary `-`, so `+5` became `-5`.
- **Reproduction:** `propan --format=none --test-mode=sema` on `.assert +5 == 5` failed with `Value(-5)` versus `Value(5)`.
- **Fix:** Return the original integer value for unary `+`; added a dedicated `tests/propan/sema/unary-plus.propan` behavior test.

## Chained binary operators grouped from the right

- **Bug:** Operators at the same precedence level parsed right to left, so `10 - 3 - 2` became `10 - (3 - 2)` and evaluated to `9`.
- **Reproduction:** `propan --format=none --test-mode=sema` on `.assert 5 == 10 - 3 - 2` failed with `Value(5)` versus `Value(9)`.
- **Fix:** Build binary expression nodes in a loop using the preceding result as the left operand; added a dedicated `tests/propan/sema/operator-associativity.propan` behavior test.

## Invalid `.align` values crashed the assembler

- **Bug:** Zero or non power of two `.align` operands reached `std.mem.alignForward`, which asserts and aborts.
- **Reproduction:** `propan --format=none --test-mode=sema` on `BYTE 1` followed by `.align 3` exited with a panic in `alignForward`.
- **Fix:** Validate alignment before advancing the cursor and emit a semantic error for invalid values; added a test for `.align 0` and `.align 3`.

## Unknown instruction effects were silently discarded

- **Bug:** The parser ignored unrecognized `:effect` names, so `NOP :nonesuch` assembled as plain `NOP`.
- **Reproduction:** `propan --format=none --test-mode=sema` on `NOP :nonesuch` exited successfully.
- **Fix:** Reject unknown effects with a syntax diagnostic; added a dedicated parser unit test.

## Parser diagnostics did not fail assembly

- **Bug:** Nonfatal parser diagnostics, including empty and multi character literals, allowed assembly to complete with exit status 0 and a substituted value.
- **Reproduction:** `propan --format=none --test-mode=sema` on `BYTE ''` printed an error but exited successfully; `BYTE 'ab'` behaved the same way.
- **Fix:** Track parser errors and return `SyntaxError` after parsing; added a dedicated unit test for both literals and an overflowing integer.

## Flat output ignored `--fill-byte`

- **Bug:** The CLI constructed a gap filled with the requested byte, but the flat writer rebuilt the image and filled gaps with zeroes.
- **Reproduction:** `.hubexec 4` followed by `BYTE 0xAA`, assembled with `--fill-byte=255 --format=flat`, produced `00 00 00 00 AA` instead of `FF FF FF FF AA`.
- **Fix:** Write the image already constructed by the CLI; added a dedicated build behavior test that compares flat output bytes.

## Flat output lost bytes from earlier input files

- **Bug:** With multiple source files, the CLI merged each module into an output image, but the flat writer serialized only the last module.
- **Reproduction:** `BYTE 1, 2` in the first file and `BYTE 3` in the second produced a one byte flat image (`03`) instead of the merged image (`03 02`).
- **Fix:** The flat writer now uses the merged CLI image; added a dedicated two file output comparison test.

## JSON output omitted earlier input files

- **Bug:** JSON serialization received only the last module, so its segments, symbols, source lines, and total size were incomplete for multiple source files.
- **Reproduction:** JSON output for the two file fixture reported one segment and `total_size: 1` even though the merged image had two bytes.
- **Fix:** Serialize metadata from every module, remap segment IDs to avoid collisions, and use the merged image size; added a dedicated JSON behavior test.

## Final segment ID did not match its labels

- **Bug:** The emitter generated a new ID for the final segment after labels had already been assigned the current segment's ID.
- **Reproduction:** A label before `BYTE 1` appeared in JSON with `segment_id: 0`, while the emitted segment had `id: 1`.
- **Fix:** Keep the current segment ID when appending the final segment; added a dedicated semantic unit test.

## Invalid address and pointer constants crash after diagnostics

- **Bug:** Semantic analysis reports `err_constants_cannot_store_pointer_expression` or `err_constant_requires_integer_not_offset`, then returns `error.InvalidSymbol` with a stack trace instead of a normal diagnostic result.
- **Reproduction:** `const X = PTRA[0]` followed by `BYTE 1`, or `target:` followed by `const X = target` and `BYTE 1`, assembled with `--format=none --test-mode=sema`.
- **Fix:** Stop analysis after constant evaluation emits errors, before checking internal symbol invariants. The existing pointer/address fixtures now expect normal, matched semantic diagnostics; `invalid-constants.propan` also covers failed evaluation.

## Pointer-operand ambiguity guard checks the same variant twice

- **Bug:** The ambiguity check in `sema.zig` tests `any_ptrreg_prev` on both sides of `and` instead of testing `any_ptrreg_now` on the right. If a pointer-register variant precedes a compatible ordinary variant, this would panic instead of keeping the pointer-register variant.
- **Evidence:** The guard is `if (any_ptrreg_prev == true and any_ptrreg_prev == true)` immediately before the pointer preference checks. Current instruction variants do not appear to trigger the condition from source input.
- **Fix:** Compare the previous and current pointer flags. A unit regression loads CALLD variants in both orders and verifies all four pointer registers select the pointer encoding; `pointer-variant-order.propan` checks emitted words.

## Named function arguments marked the wrong parameter as supplied

- **Bug:** After positional arguments, `map_function_args` marks the named argument's index relative to the remaining parameters, rather than its index in the full parameter list. Required named arguments can be reported missing despite being supplied.
- **Reproduction:** Assemble `.assert bitrange(0, high=3) == 96` in semantic test mode; the `high` parameter is incorrectly reported missing.
- **Fix:** Mark the full parameter index in the supplied-argument bitset. `mixed-function-arguments.propan` covers required named arguments after one and two positionals, reordered keywords, and defaults.

## Register offsets wrap at 511 instead of 512

- **Bug:** `regoffset` reduces register addresses modulo `maxInt(u9)` (511), although the register address space contains 512 entries. Register 511 maps to zero even with offset zero, and negative offsets wrap incorrectly.
- **Reproduction:** Assemble `.assert cogaddr(regoffset(regoffset(PTRB, 6), 0)) == 511` or `.assert cogaddr(regoffset(regoffset(PTRB, 7), -1)) == 511`.
- **Fix:** Compute the signed sum in i11 and reduce modulo 512. `register-offset-wrap.propan` checks zero offsets, both wrap directions, and offset limits.

## Binary operations on addresses or strings crash

- **Bug:** Binary expressions with two addresses or two strings hit explicit `@panic` placeholders in the evaluator instead of reporting unsupported operand types.
- **Reproduction:** Assemble `target:` followed by `.assert target == target`, or `.assert "a" == "a"`, in semantic test mode.
- **Fix:** Emit the existing unsupported-operand diagnostic instead of panicking. `unsupported-binary-types.propan` checks both types with comparison and arithmetic operators.

## Large `ticks` arguments overflow intermediate arithmetic

- **Bug:** Duration conversion, summation, and multiplication by the clock rate use unchecked `u64` arithmetic. Large legal arguments panic before the existing range diagnostic, and a zero clock can still panic despite its result being zero.
- **Reproduction:** Assemble `.assert ticks(1, s=20000000000) == 0`; the duration multiplication aborts with integer overflow. `ticks(4294967295, s=5)` overflows when multiplying by the clock.
- **Fix:** Use u128 for duration and clock arithmetic and diagnostic period counts. This accommodates every accepted input combination, preserves accurate range diagnostics, and allows zero-clock calculations. `ticks-overflow.propan` and `ticks-large-duration.propan` cover maximum inputs, intermediate overflow, and the u32 boundary with WAITX adjustment.

## Failed instruction argument evaluation leaves uninitialized values

- **Bug:** `evaluate_instruction_arguments` catches evaluation errors and continues without initializing the argument. Encoding selection and assertion evaluation then read undefined tagged unions, causing nondeterministic diagnostics or panics.
- **Reproduction:** `.assert bitrange(0, high=3) == 96` (before the named-argument fix) reports a spurious `.assert condition ... found address`; `MOV PTRA, 1 / 0` also enters encoding selection with an undefined argument.
- **Fix:** Initialize failed arguments to an integer placeholder and stop analysis before encoding selection or assertion evaluation when argument evaluation failed. `expression-evaluation-failure.propan` checks division-by-zero in both instructions and assertions without secondary errors.

## Signed division overflow aborts assembly

- **Bug:** Integer division directly uses `@divFloor`, which panics on the minimum signed 64-bit integer divided by -1.
- **Reproduction:** Assemble `.assert (-9223372036854775807 - 1) / -1 == 0`; the assembler aborts with integer overflow.
- **Fix:** Guard minInt(i64) / -1 and return Overflow through the evaluator diagnostic path. `division-overflow.propan` checks that diagnostic.

## Some parser failures silently exit successfully

- **Bug:** The CLI discards parser errors such as `InvalidFlag` and `UnexpectedToken` without ensuring a diagnostic exists. Expression parsing also consumes incomplete arguments and swallows their failures, so `BYTE 1 +` is accepted as an empty data directive.
- **Reproduction:** Assemble `if(Q) NOP`, `BYTE 1 +`, or `BYTE -` with `--format=none --test-mode=sema`; all exit with status zero and no diagnostic.
- **Fix:** Require each nonterminal instruction argument to parse successfully, and convert every non-allocation parser failure into a syntax diagnostic if one was not already emitted. New parser diagnostic fixtures cover incomplete unary/binary expressions, constants, calls, parentheses, and invalid conditions.

## String and enumerator data operands crash emission

- **Bug:** `BYTE`, `WORD`, and `LONG` pass string/enumerator values into conversion code containing panic placeholders.
- **Reproduction:** Assemble `BYTE "hi"` or `BYTE #on`; both abort instead of issuing a type diagnostic.
- **Fix:** Report the existing expected-type diagnostic in shared data conversion instead of panicking. `invalid-data-types.propan` checks BYTE, WORD, and LONG for both unsupported types.

## Signed modulo overflow triggers an arithmetic exception

- **Bug:** `@mod` on the minimum signed 64-bit integer and -1 triggers an arithmetic exception, even though the mathematical remainder is zero.
- **Reproduction:** Assemble `.assert (-9223372036854775807 - 1) % -1 == 0`.
- **Fix:** Return zero for divisor -1 before executing signed modulo. `integer-extremes.propan` also checks divisor 1, ordinary negative division/modulo, and the minimum integer.

## A lone quote crashes the tokenizer

- **Bug:** The string/character token matcher reads index 1 before checking that a closing delimiter or body exists.
- **Reproduction:** Assemble a file containing only `"` or only `'`, with no trailing newline; both abort with an out-of-bounds panic.
- **Fix:** Require at least two bytes before entering the delimiter matcher. Two parser fixtures end at an opening quote without a newline and expect a syntax diagnostic.

## Negating the minimum integer crashes

- **Bug:** Unary minus uses checked negation while binary arithmetic uses wrapping 64-bit arithmetic. Negating the minimum 64-bit integer aborts instead of wrapping consistently.
- **Reproduction:** Assemble `.assert -(-9223372036854775807 - 1) == (-9223372036854775807 - 1)`.
- **Fix:** Use wrapping unary negation, consistent with binary integer arithmetic. `integer-extremes.propan` checks the minimum integer.

## Invalid segment origins still corrupt the layout cursor

- **Bug:** Segment directives diagnose origins beyond hub RAM but still store them in the cursor. Subsequent emission or alignment can overflow `u32` before semantic analysis returns its diagnostics.
- **Reproduction:** `.hubexec 4294967295` followed by `BYTE 1` or `.align 4`, and `.cogexec 4294967295` followed by `NOP`, abort with integer overflow.
- **Fix:** Skip the cursor change after rejecting an origin outside hub RAM. `invalid-origins.propan` checks all segment modes and subsequent data, code, and alignment operations.

## Failed relative-address assertions lose their evaluation context

- **Bug:** When constructing a failed comparison assertion's message, operands are evaluated again with no current address. Relative `@` operands then incorrectly produce a scope error and the message uses a substituted zero.
- **Reproduction:** `.assert @target == 1` followed immediately by `target:` and `NOP` should report only an assertion failure, but also reports that `@` cannot be used in this scope.
- **Fix:** Reevaluate comparison operands using the assertion instruction's end address, matching their initial evaluation. `assert-relative-comparison.propan` expects only the assertion failure.

## Successful assertions skip message type validation

- **Bug:** `.assert` returns early for a true condition before checking that its optional message is a string.
- **Reproduction:** `.assert 1, PTRA` succeeds, while the same non-string message with a false condition emits a type error.
- **Fix:** Validate the optional message before accepting a true condition. `assert-message-with-true-condition.propan` checks the type error.

## Long invalid character literals report out of memory

- **Bug:** Character unescaping used a fixed 32-byte buffer. Longer literals returned `OutOfMemory` rather than the expected diagnostic for multiple characters.
- **Reproduction:** Assemble `BYTE 'abcdefghijklmnopqrstuvwxyz0123456789'` with the original parser.
- **Fix:** Unescape into the parser arena, which already owns literal data, so length does not impose an artificial memory limit. `tests/propan/parser/diagnostics/long-character.propan` checks the character-count diagnostic.

## Binary comparison hides partial-word mismatches

- **Bug:** The comparison renderer reads every incomplete 32-bit word as zero. Differences in the last one to three bytes disappear from the diff, even though the comparison fails.
- **Reproduction:** Compare `BYTE 1` against a one-byte reference containing `02`; the output shows an empty `<diff>` instead of the differing byte.
- **Fix:** Zero-pad the available tail bytes when reading a diff word. `compare-partial-word.propan` is compared with a different one-byte reference and the build checks the differing values appear in the diff.

## Hexadecimal escapes discard the following character

- **Bug:** After reading `\xHH`, the unescaper advances past both hex digits and then increments once more at the loop boundary, skipping the next character.
- **Reproduction:** `BYTE '\x41Z'` is accepted as the single character `A` rather than rejected for containing `AZ`. A string assertion message `"\x41Z"` also loses `Z`.
- **Fix:** Account for the loop increment after decoding the two hex digits. Character and string regression files check preserved ordinary characters and consecutive escapes.

## Pointer conversion in standard-library function wrappers does not compile

- **Bug:** The `PointerExpression` conversion branch repeats `PTRA` instead of handling `PTRB`, and initializes a nonexistent `.inremenet` field. Any wrapped function taking a pointer expression fails compilation when this branch is instantiated.
- **Reproduction:** Define a `define.function` with an `eval.PointerExpression` parameter and invoke it with `Value.register(0x1F8)` or `Value.register(0x1F9)`. Current built-in functions do not instantiate this branch.
- **Fix:** Handle PTRB in its own case and initialize the correct increment field. `src/propan/stdlib/define_tests.zig` instantiates a wrapped pointer function and checks both registers, explicit pointer expressions, and invalid inputs.

## Module metadata borrows caller-owned source paths

- **Bug:** Constant and line-map locations are copied into the returned module without copying their source path. Symbol locations already own their paths. Mutating or freeing the caller's path leaves constant/line metadata corrupted or dangling.
- **Reproduction:** Parse and analyze using a mutable `source.propan` path buffer, then overwrite that buffer. The module's line and constant locations change to the overwritten bytes.
- **Fix:** Copy constant and line-map source paths into the module arena, using the same helper as symbol locations. `src/propan/metadata_tests.zig` first failed when the caller path was overwritten and now verifies all three metadata kinds retain their original path.

## Frontend renderers fail to compile and emit invalid pointer syntax

- **Bug:** Both public frontend renderers omit the enumerator expression variant, so calling them fails compilation. The AST dump also uses outdated formatting for string escapes and ignores non-void writer return values, which Zig rejects. The pretty printer renders pointer modifiers as internal names (`pre_increment`, `post_increment`) and array indexing as `array_index`, producing source that cannot be parsed again.
- **Reproduction:** Parse a source with `#on`, `PTRB++`, or `PTRA[2]`, then call `frontend.render.pretty_print` or `frontend.dump_ast`. Instantiating either renderer exposes the missing union case; printed pointer syntax cannot round-trip.
- **Fix:** Handle enumerators in both renderers, update escape formatting and explicitly discard handled writer byte counts, and print source tokens for pointer modifiers and array indexing. `render-roundtrip.propan` and `src/propan/frontend/render_tests.zig` exercise every expression variant, reparse printed source, compare emitted bytes, and invoke the AST dump.
