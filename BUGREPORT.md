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

---

## Spin2 emitter loses readable zero-operand effects

- **Bug:** `RET :wcz` was emitted as a raw `LONG` because the readable renderer rejected every zero-operand instruction with an effect.
- **Reproduction:** `propan --format=spin2 --output=- examples/sumloop.propan` contained `LONG $FD7C002D` for the return.
- **Fix:** Permit the verified `RET wcz` form while retaining opcode fallback for zero-operand aliases whose FlexSpin encoding differs.

## Spin2 emitter flattens REP end labels and the altered register

- **Bug:** `REP @.end, 8` became a numeric destination and `ADD altered, 0` became `ADD $0, #$0`, hiding both PASM constructs.
- **Reproduction:** Inspect the Spin2 output for `examples/sumloop.propan`.
- **Fix:** Preserve `@label` for the first REP operand, render `altered` as `0-0`, and keep exact binary round-trip coverage.

## Spin2 emitter mangles every label and drops source comments

- **Bug:** Every label was prefixed with `p2_label`, local scope names were flattened, and source comments disappeared.
- **Reproduction:** `quicksum:` and `.loop:` in `examples/sumloop.propan` became `p2_label_quicksum_0` and `p2_label_quicksum_loop_1`; the introductory comment block was absent.
- **Fix:** Emit ordinary global names and scoped `.local` names, add short suffixes for keyword collisions, sanitize nonlocal punctuation, and copy source text into the module for comment emission.

## Spin2 emitter ignores integer spelling and prints zero in hex

- **Bug:** Data and operand zeroes appeared as `$0`, register numbers appeared in hex, and decimal source literals lost their spelling style.
- **Reproduction:** Spin2 output for `examples/sumloop.propan` contained `LONG $0` and `ADD $0, #$0`.
- **Fix:** Print zero as `0`, use decimal for numeric registers, and preserve decimal versus hexadecimal style for direct integer literals.

## Spin2 emitter emits invalid keyword identifiers

- **Bug:** Emitting source names unchanged can produce invalid Spin2 declarations for names such as `COUNT`, `NEXT`, and `END`.
- **Reproduction:** FlexSpin rejected generated output for `tests/propan/sema/array-constants.propan`, `lazy-constants.propan`, and `basic-instruction-selection.propan`.
- **Fix:** Keep readable names by default and append a short suffix only for reserved names or collisions; sanitize punctuation in nonlocal names such as `foo.loop`.

---

## Boolean `#false` arguments are rejected (2026-10-05)

- **Bug:** The shared boolean enumerator table contains `"false "` with a trailing space, so valid `#false` arguments fail with `invalid argument`.
- **Reproduction:** Run `zig-out/bin/propan --format=none --test-mode=sema` on `.assert ticks(1000, ms=3, waitx=#false) == 3`; it exits with an expression-evaluation error.
- **Fix:** Remove the trailing space in the shared conversion table. `tests/propan/sema/boolean-enumerators.propan` checks all six boolean word aliases through `ticks`; it is registered in the build test suite.

## Repeating empty data performs billions of useless iterations (2026-10-05)

- **Bug:** `repeat_value` loops once per repetition even when the input string or array is empty. An empty output can take billions of iterations to assemble.
- **Reproduction:** Assemble `BYTE "" * 4294967295` or `BYTE [] * 4294967295` with `--format=none --test-mode=sema`. Both failed to finish within a two-second timeout; ordinary empty data finishes immediately.
- **Fix:** Return the original empty value after validating the repetition count, before allocating or looping. `tests/propan/sema/empty-repetition.propan` covers both operand orders for strings and arrays and a nested empty string, while checking the resulting image.

## Unknown functions in conditional directives crash assembly (2026-10-05)

- **Bug:** Conditional filtering evaluates function calls before ordinary symbol validation. The evaluator force-unwraps the missing function lookup, aborting on an unknown function in `.if` or `.elif`, including calls through constants.
- **Reproduction:** Assemble `.if missing_function(1)` followed by `BYTE 1` and `.endif` with `--format=none --test-mode=sema`; it panics with `attempt to use null value` in `evaluate_expr`.
- **Fix:** Guard the lookup in the shared expression evaluator and emit the existing unknown-function diagnostic, returning `DiagnosedFailure`. `tests/propan/sema/diagnostics/conditional-unknown-function.propan` checks `.if`, `.elif`, constant indirection, and that inactive branches still skip evaluation.

## `@label` uses storage distance instead of execution distance (2026-10-05)

- **Finding:** The unary `@` evaluator always subtracts hub storage addresses and divides by four, even across separate cog/LUT segments with independent execution origins. Its existing TODO explicitly calls for validating that the addresses belong to the same segment. This can make `REP @target, 1` count storage distance rather than execution distance.
- **Reproduction:** Assemble the following with `zig-out/bin/propan --format=json --output=- --no-warnings`:

  ```propan
  .cogexec 0x100, 0
  REP @target, 1
  .cogexec 0x200, 0
  target:
  NOP
  ```

  Assembly succeeds and emits REP word `0xFCDC7E01`, with first operand 63 (`(0x200 - 0x104) / 4`). Both segments start at cog PC zero; the target's execution PC is zero, not 64.
- **Clarified semantics:** Measure from the execution PC after the instruction. Cog/LUT differences use long indices; hub-to-hub byte differences must be divisible by four and are converted to longs. Cross-segment references warn, while cross-execution-mode references error.
- **Fix:** Subtract execution positions from the PC after the instruction, preserving subregister byte offsets for alignment checks. Require matching execution modes and emit `warn_relative_address_crosses_segments` for different segments. Keep the full post-instruction PC during layout so the final cog/LUT instruction does not wrap its end address. `.data` and `.regspace` have no execution PC. New fixtures cover forward/backward cog/LUT/hub distances, segment warnings, all six execution-mode crossings, unaligned hub distances, augmented instruction length, reserved cog addresses, and final cog/LUT PCs. The six new/updated fixtures passed direct semantic checks using `zig-0.16.0`.

Validation for the initial three 2026-10-05 fixes: `zig-0.16.0 build install test -Dwith-flexspin -j4` passed, including 72 unit tests, diagnostic checks, semantic fixtures, formatter and Spin2 round trips, and FlexSpin equivalence checks. All three new fixtures also passed direct semantic runs; the empty-repetition fixture completed within a three-second timeout. `zig-0.16.0 fmt --check` on changed Zig files and `git diff --check` passed. Existing report content and unrelated `TODO.md` edits were preserved.

## Failed comparison assertions emit operand warnings twice (2026-10-05)

- **Bug:** `evaluate_asserts` reevaluates comparison operands to construct the failure message. The second evaluation emits the same operand warnings again, including cross-segment `@` warnings.
- **Reproduction:** Assemble `.assert ticks(1, ns=1, waitx=#on) == 1` with `--format=none`. The short-WAITX warning appears twice before the assertion failure, although the source contains one call.
- **Fix:** Suppress shared diagnostic emission only while reevaluating operands for the failure message, then restore it before emitting the assertion error. `assert-operand-warning-once.propan` checks exact warning counts and both failure messages for `ticks` and a cross-segment `@` operand.

## Escaped backslashes produce invalid-escape warnings (2026-10-05)

- **Bug:** The source literal unescaper omits the standard `\\` escape, routing it through the invalid-escape warning path even though it produces the intended backslash. Pretty printing also generates this escape for literal backslashes, so round trips introduce warnings.
- **Reproduction:** Assemble `BYTE '\\'` with `--format=none`; it succeeds but warns `invalid escape sequence: \\`.
- **Fix:** Recognize escaped backslashes in the shared single-character escape switch. `backslash-escapes.propan` checks character and string literals, adjacent escapes, exact emitted bytes, and no warnings; the suite also checks formatter round trips.

## Spin2 constants retain invalid punctuation (2026-10-05)

- **Bug:** The Spin2 emitter sanitizes punctuation in label names but writes constant names verbatim. Dotted global constants are valid Propan source and produce invalid Spin2 declarations and operand references.
- **Reproduction:** Assemble `const my.value = 3` followed by `MOV PA, my.value` with `--format=spin2`. The generated declaration is `my.value = 3`; FlexSpin rejects it with a syntax error.
- **Fix:** Use the existing identifier sanitizer for constant declarations and references. `spin2-dotted-constants.propan` checks emitted instructions and exercises a collision between dotted and underscore names; the build suite compiles the Spin2 output with FlexSpin and compares its bytes with the flat image.

## Spin2 symbolic register operands use the wrong execution origin (2026-10-05)

- **Bug:** Spin2 output flattens segments into one DAT image without setting their execution origins, but substitutes label names for numeric register operands. When a cog segment's execution origin differs from its hub storage position, FlexSpin gives those labels different register numbers and silently changes instruction bytes.
- **Reproduction:** Assemble `.cogexec 0x40, 20`, `MOV slot, 1`, `var slot:`, `LONG 0` as flat and Spin2, then compile the Spin2 with FlexSpin. The MOV is `0xF6042A01` in flat output but `0xF6042201` after the round trip: register 21 became 17.
- **Fix:** Substitute a symbolic label only when its DAT register index matches the operand's execution register; otherwise emit the already evaluated number. `spin2-register-origins.propan` checks two independent cog origins, and its generated Spin2 must round-trip byte-for-byte through FlexSpin in the suite.

## Propan accepts conditions on NOP (2026-10-05)

- **Bug:** Propan accepts an explicit condition on NOP and encodes that condition nibble. These nonzero words are ROR instructions, not NOP. The original Spin2 export problem was a symptom of this semantic error; the raw-opcode fallback did not fix it.
- **Reproduction:** Assemble `if(C) NOP` with `--format=flat`: it succeeds with word `0xC0000000`. Spin2 rejects the corresponding conditioned NOP. `return NOP` is also incorrectly accepted, even though its zero condition nibble leaves the word zero.
- **Clarified semantics:** NOP is exactly `0x00000000` and cannot have any explicit condition. `0xF0000000` is unconditional `ROR register(0), register(0)`.
- **Fix:** Reject any explicit condition on NOP during semantic mnemonic selection, including `return`, and keep ordinary NOP's fixed zero encoding. Remove the earlier Spin2 raw-opcode workaround. `diagnostics/conditional-nop.propan` checks five conditions and case variants; `nop-encoding.propan` checks zero-word NOP alongside nonzero ROR words. The renderer fixture now uses a valid `return ROR` spelling for the same zero word.

## Spin2 REP end labels lose cross-segment execution distances (2026-10-05)

- **Bug:** With the clarified `@` semantics, preserving `REP @target` in a flattened DAT image lets FlexSpin recompute the distance using hub storage positions rather than independent cog execution origins.
- **Reproduction:** Assemble `.cogexec 0x100, 10`, `REP @target, 1`, `.cogexec 0x200, 30`, `target:`, `NOP` as flat and Spin2, then compile the Spin2 with FlexSpin. Flat output encodes `0xFCDC2601` (19 longs), while the round trip encodes `0xFCDC7E01` (63 longs).
- **Fix:** Preserve a global REP end label only when its distance in the emitted DAT layout matches the evaluated execution distance; otherwise use the existing raw-opcode fallback. `spin2-cross-segment-rep.propan` checks the warning and exact REP word, with FlexSpin round-trip coverage.

## Ordinary instruction operands accept pointer expressions (2026-10-05)

- **Bug:** Instruction operand compatibility accepts pointer-expression values for ordinary register and register/immediate operands. The emitter converts them to memory-pointer bitfields but uses those bits as register indices for MOV and ADD.
- **Reproduction:** Assemble the following with `zig-out/bin/propan --format=flat --output=pointer.bin`:

  ```propan
  MOV PA, PTRA++
  ADD PB, PTRB[2]
  MOV ++PTRA, 1
  ```

  Assembly succeeds without diagnostics, producing words `0xF603ED61`, `0xF103EF82`, and `0xF6068201`. Those instructions access registers 353, 386, and 321 respectively; they do not update or dereference PTRA/PTRB.
- **Clarified semantics:** Only `Operand.Type.pointer_expr` may accept a pointer-expression value. All other instruction operand types must reject it, including MOV's register and register/immediate operands.
- **Fix:** Reject pointer-expression values for every operand type except `pointer_expr` in the shared `can_assign_from` check. `pointer-expression-operand-types.propan` checks five rejected MOV, ADD, RDLONG, and WRLONG operands. Existing memory-pointer equivalence fixtures cover valid pointer-expression operands.

Validation for the continuation: `zig-0.16.0 build install test -Dwith-flexspin -j4` passed with all fixes and new fixtures, including 72 unit tests, diagnostic and semantic checks, formatter and Spin2 round trips, and FlexSpin equivalence checks. `zig-0.16.0 fmt --check` on changed Zig files and `git diff --check` also passed.

## Spin2 local labels retain invalid punctuation (2026-10-05)

- **Bug:** Local label declarations and REP end-label references bypass identifier sanitization. Dotted local names are valid Propan source but invalid Spin2.
- **Reproduction:** Assemble `start:`, `REP @.one.two, 1`, `MOV PA, *.one.two`, `.one.two:`, `NOP` with `--format=spin2`, then compile the output with FlexSpin. The generated `.one.two` label causes a syntax error.
- **Fix:** Sanitize local label names and disambiguate names that sanitize identically within their parent scope. Resolve REP's target symbol before emission and use the same name conversion as its declaration, while retaining the storage-distance check. `spin2-dotted-local-labels.propan` checks `.one.two` versus `.one_two` and both REP references, including formatter and FlexSpin byte-equivalence checks.

## `localaddr(register)` rejects valid cogexec arguments (2026-10-05)

- **Bug:** The register branch of `localaddr` unconditionally emits `err_localaddr_is_only_valid_for_registers_in_a_cogexec_scope`, with a TODO to check the current execution mode. This rejects registers even inside `.cogexec`.
- **Reproduction:** Assemble `.cogexec` followed by `LONG localaddr(PA)` with `--format=none`. It errors that `localaddr()` is only valid for registers in a cogexec scope, despite being in one. `.cogexec`, `const r = localaddr(PA)`, `LONG r` produces the same error and warning. The evaluator computes register number 502 in both cases before failing analysis.
- **Clarified semantics:** `localaddr` accepts any register only in cogexec; `cogaddr` accepts any register in every mode. Constants use their declaration's execution mode. Preserve the existing advisory warning for register arguments.
- **Fix:** Check the execution mode for register arguments rather than rejecting all `localaddr` register calls. Track declaration modes for constants and active modes during conditional filtering; no PC is provided to conditions or constants. Four fixtures cover all five modes, named and computed registers, constant declaration modes, and inactive mode directives. Existing register warnings remain advisory. Direct semantic checks passed using `zig-0.16.0`.

Validation after the pointer clarification and local-label fix: `zig-0.16.0 build install test -Dwith-flexspin -j4` passed, including 72 unit tests, the new diagnostic and local-label fixtures, valid memory-pointer equivalence checks, formatter round trips, and Spin2/FlexSpin byte comparisons. `zig-0.16.0 fmt --check` on changed Zig files and `git diff --check` passed. Unrelated `TODO.md` edits were preserved.

## Conditional constants emit their warnings twice (2026-10-05)

- **Bug:** A constant used by `.if` is evaluated once by conditional filtering and again by the main analyzer. Both evaluations emit the same warnings at the declaration, even though it appears only once.
- **Reproduction:** Assemble `const reg = cogaddr(PA)`, `.if reg == 502`, `LONG reg`, `.endif` with `--format=none`. The register-argument warning at `cogaddr(PA)` appears twice. The clarified `localaddr(PA)` path reproduces this too.
- **Fix:** Copy valid constant values already evaluated by the conditional probe into the main analyzer's constant cache, preserving their owned string/sequence storage and marking them evaluated. Address and pointer-expression values still undergo ordinary invalid-constant validation. `conditional-constant-warning-once.propan` checks exact counts for cogaddr, localaddr, and short-WAITX warnings, dependent register constants, declaration modes, and emitted values.

## Augmented constants crash instruction emission (2026-10-05)

- **Bug:** A constant can retain the augmentation flag from `aug(...)`, but instruction layout detects augmentation only in operand syntax. Emission writes an extra AUGS/AUGD word without reserving space, so a following instruction overlaps and triggers an assertion.
- **Reproduction:** Assemble `const big = aug(0x12345678)`, `MOV PA, big`, `NOP` using `zig-out/bin/propan --format=json --output=-`. It aborts at `std.debug.assert(hub_offset >= segment_end_hub_offset)` in `emit_code`: MOV was laid out as four bytes but emitted eight.
- **Clarified semantics:** `aug()` must appear directly on an instruction operand. Constants must not retain augmentation flags, and augmentation is invalid in data, layout, assertion, or conditional-compilation expressions.
- **Fix:** Give expression evaluation an explicit instruction-operand context and reject `aug()` elsewhere before retaining an augmentation flag. Return immediately after diagnosing nested `aug()` calls. Four diagnostic fixtures cover constants, data, layout directives, assertions, and conditional compilation. `direct-augmentation.propan` checks exact bytes for valid augmentation of an ordinary constant and a pointer index, each followed by NOP to detect layout overlaps.

Validation after the register-address clarification: `zig-0.16.0 build install test -Dwith-flexspin -j4` passed, including 72 unit tests, all five new semantic/diagnostic fixtures, formatter and Spin2 round trips, and FlexSpin equivalence checks. `zig-0.16.0 fmt --check` on changed Zig files and `git diff --check` passed. At that stage, the augmented-constant crash awaited its language rule; the fix above follows the subsequent clarification.

## Disassembler labels nonzero ROR words as NOP (2026-10-05)

- **Bug:** The comparison-output disassembler masks condition bits for every instruction, including the zero-word NOP alias. Because NOP is checked first, words with only condition bits set are displayed as NOP instead of ROR.
- **Reproduction:** Compare a program containing `ROR register(0), register(0)` against a different reference binary in `--test-mode=compare`. The mismatch's actual instruction is displayed as NOP for word `0xF0000000`.
- **Fix:** Keep every bit significant when matching NOP while continuing to mask variable condition bits for other instructions. A comparison test reuses `nop-encoding.propan` and checks that zero decodes as NOP and `0xF0000000` as ROR in both expected and actual mismatch output.

Validation after the NOP clarification: `zig-0.16.0 build install test -Dwith-flexspin -j4` passed, including semantic rejection of conditional NOP, exact NOP/ROR encodings, mismatch disassembly, formatter and Spin2/FlexSpin round trips, and the 72-test unit suite. `zig-0.16.0 fmt --check` on changed Zig files and `git diff --check` passed. At that stage, discovery stopped at the augmented-constant rule, subsequently clarified and fixed above.

Validation after the augmentation clarification: `zig-0.16.0 build install test -Dwith-flexspin -j4` passed, including all 72 unit tests, the four new augmentation diagnostic fixtures, exact instruction layout, formatter round trips, and Spin2/FlexSpin byte equivalence for valid direct augmentation. Formatting and diff whitespace checks passed.

## Augmentation accepts whole pointer expressions (2026-10-05)

- **Bug:** `aug()` blindly retains its argument's type and adds an augmentation flag, accepting whole pointer expressions despite augmentation being defined on their indices. The preceding regression mistakenly treated this syntax as valid.
- **Reproduction:** Assemble `RDLONG PA, aug(PTRA[256])`, followed by `NOP`, using `zig-out/bin/propan --format=flat`. It succeeds and emits the same instruction words as `RDLONG PA, PTRA[aug(256)]`.
- **Clarified semantics:** `PTRA[aug(256)]` is correct; `aug(PTRA[256])` is forbidden. Pointer increments also keep augmentation on their index.
- **Fix:** Reject pointer-expression arguments in the shared `aug()` evaluator with a diagnostic explaining that the index must be augmented. Correct `direct-augmentation.propan` to use `PTRA[aug(256)]` with the same expected bytes. `aug-whole-pointer.propan` rejects five whole-pointer cases, including pre/post updates and an already augmented index; existing memory-pointer and pointer-update equivalence fixtures retain their valid index syntax.

## Spin2 collision suffixes collide with source identifiers (2026-10-05)

- **Bug:** The exporter resolves sanitized identifier collisions by appending `_p2` and a declaration index without checking whether that generated name is already a valid source identifier. The resulting Spin2 can redeclare a name or reference the wrong declaration.
- **Reproduction:** Export `const a.b = 1`, `const a_b = 2`, `const a_b_p20 = 3`, followed by `MOV PA, a.b`, `MOV PB, a_b`, `ADD PA, a_b_p20`, with `--format=spin2`. It declares `a_b_p20` twice with different values; FlexSpin errors `Redefining a_b_p20 with a different value`. The same suffix scheme is used for global and local labels.
- **Fix:** Reserve a generated-name prefix absent from all sanitized source identifiers, then include the declaration kind and index to distinguish constants and labels. Detect global label/constant collisions in both directions. `spin2-name-collisions.propan` checks exact instruction/data words and exercises original suffix collisions, two occupied generated prefixes, constant/label collisions, global labels, and scoped REP targets; its exported Spin2 compiles with identical bytes in FlexSpin.

## Import-once identity changes with the search path (2026-10-05)

- **Bug:** Import identity is a search-directory index plus its relative path. The same file therefore gets different identities when referenced through an absolute path, an include directory, or its original source directory. `.import once` can emit the file more than once and cycle detection can revisit the root before recognizing a cycle.
- **Reproduction:** Create `/tmp/propan-import-once-alias.propan` containing `.import once`, `.import "/tmp/propan-import-once-alias.propan"`, and `BYTE 1`. Assemble with `--format=flat`: the image contains `01 01` instead of one byte `01`.
- **Fix:** Key the root and imported source cache, active-import set, and import-once set by canonical file paths, while retaining the original display path and directory for diagnostics and relative imports. Preserve stdin's synthetic identity. `import-once-alias.propan` checks equivalent relative paths; a CLI regression checks source-directory, absolute, and include-directory references to the same leaf. The original absolute self-import reproducer now emits one byte.

## Augmentation of register-valued operands leaks into following instructions (2026-10-05)

- **Bug:** `aug()` retains register usage and emission inserts an AUGS/AUGD prefix even when the instruction operand is encoded as a register. That instruction does not consume the queued immediate augmentation, which can change a later instruction's meaning.
- **Reproduction:** Assemble `MOV PA, aug(PB)` followed by `RDLONG PA, PTRA[1]` with `--format=flat`. It succeeds with words `0xFF000000`, `0xF603EDF7`, and `0xFB07ED01`. The MOV uses a register S operand and leaves AUGS queued; the following pointer read is then interpreted as an augmented immediate address `0x101`. The shipped silicon documentation describes the different augmented hub-memory operand layouts.
- **Clarified semantics:** Augmentation is valid only for immediates. Register values and values with register usage, such as `aug(*label)`, must always error, including bare PTRA/PTRB before pointer-expression conversion.
- **Fix:** Require literal usage and an integer or address value in the shared `aug()` evaluator, before applying its flag or converting bare PTRA/PTRB into pointer expressions. `aug-register-operands.propan` rejects named/computed registers, register constants, data labels, explicit register usage, and register-valued pointer indices. `aug-nonnumeric-values.propan` also checks strings, arrays, and enumerators. The positive direct-augmentation fixture now covers immediate code-label addresses alongside integer constants and augmented pointer indices.

Validation for the preceding continuation: `zig-0.16.0 build install test -Dwith-flexspin -j4` passed, including all 72 unit tests, whole-pointer augmentation diagnostics, valid augmented pointer indices, Spin2 name-collision byte equivalence, canonical import-once identities, and all formatter/Spin2 round trips. `zig-0.16.0 fmt --check` on changed Zig files and `git diff --check` passed. Unrelated `TODO.md` changes remain preserved. At that stage, register-valued augmentation awaited the subsequent clarification above.

## Spin2 references collapse case-distinct symbols (2026-10-05)

- **Bug:** Propan resolves symbols case-sensitively, but the Spin2 exporter looks up operand names case-insensitively. It gives case-distinct declarations separate generated names, then selects the first declaration for both references, silently changing instruction bytes.
- **Reproduction:** Export `const Foo = 1`, `const foo = 2`, `MOV PA, Foo`, `MOV PB, foo` with `--format=spin2` and compile it with FlexSpin. Both MOVs refer to the generated name for `Foo`, so the second immediate is 1 instead of 2. The flat binaries differ at byte 5. Label lookup and REP target lookup use the same incorrect name comparison.
- **Fix:** Resolve constant, label, and REP references with exact Propan identifier spelling, while retaining case-insensitive collision detection for Spin2 declarations. Apply the special `altered` spelling only to the exact builtin name, allowing a distinct user `Altered` constant. `spin2-case-sensitive-symbols.propan` checks exact bytes for case-distinct constants, global/local labels, case-distinct parent scopes, mixed constant/label names, and the builtin versus user constant; FlexSpin output matches the flat image.

## Spin2 drops comments after quote character literals (2026-10-05)

- **Bug:** The Spin2 source-comment scanner tracks double-quoted strings but ignores single-quoted character literals. A double quote inside a character literal opens a phantom string, hiding the actual trailing comment.
- **Reproduction:** Export `BYTE '"' // keep this comment` or `BYTE '\"' // keep escaped quote comment` with `--format=spin2`. The BYTE is present but its trailing comment disappears, while a normal comment after a string is retained.
- **Fix:** Track the active quote delimiter, recognizing both strings and character literals and skipping escapes only inside them. `spin2-quoted-comments.propan` checks the character/string bytes and round trips; a CLI output check verifies all five trailing comments survive, including quote, apostrophe, backslash, and embedded `//` cases.

## Spin2 identifier sanitizer misses operand keywords (2026-10-05)

- **Bug:** Exported identifier collision handling recognizes instruction mnemonics and a small directive list, but misses PASM condition and effect keywords. Valid Propan names can therefore become keyword tokens in Spin2 operand positions.
- **Reproduction:** Export `const IF_C = 1` and `MOV PA, IF_C` with `--format=spin2`, then compile it with FlexSpin. The emitted `MOV 502, #IF_C` fails with a syntax error. No Propan error is reported.
- **Fix:** Reserve the PASM condition-name family and effect names, extend the Spin2 language/DAT keyword list, and reuse the standard library's predefined constant and register names. Conflicts use the existing generated-name path, retaining ordinary readable identifiers. `spin2-reserved-names.propan` checks immediate and register operands named after condition/effect words, language tokens, builtins, and the single underscore; its Spin2 output must compile and match the flat bytes in FlexSpin.

Validation after the register-augmentation clarification: `zig-0.16.0 build install test -Dwith-flexspin -j4` passed, including all 72 unit tests, register and nonnumeric augmentation rejection, valid immediate/address and pointer-index augmentation, case-sensitive symbol references, reserved identifier byte equivalence, preserved Spin2 comments, and all formatter/Spin2 round trips. `zig-0.16.0 fmt --check` on changed Zig files and `git diff --check` passed. Unrelated `TODO.md` changes remain preserved.

## Parentheses incorrectly reject direct augmentation (2026-10-05)

- **Bug:** Ordinary parentheses increment expression nesting, so the evaluator treats a wrapped instruction operand as a nested augmentation call.
- **Reproduction:** Assemble `MOV PA, (aug(266))`. It fails with `aug() must be the root of an expression`, while the equivalent unwrapped operand succeeds.
- **Clarified semantics:** Parentheses are transparent wrapping. Wrapped direct operands and pointer indices retain their eligibility for augmentation; actual operators and function arguments still introduce nesting.
- **Fix:** Keep the existing nesting depth when evaluating a parenthesized expression. The direct-augmentation checklist now checks wrapped immediate constants, code-label addresses, pointer operands/indices, and the exact `MOV PA, (aug(266))` example. Negative checks retain rejection inside arithmetic and function arguments, including wrapped nested calls.
- **Validation:** `zig-0.16.0 build install test -Dwith-flexspin -j4` passed, including all 72 unit tests, checklist regressions, formatter round trips, and FlexSpin byte comparisons. `zig-0.16.0 fmt --check src/propan/sema.zig` and `git diff --check` passed. Further bug searching stopped at the user's request after this fix.
