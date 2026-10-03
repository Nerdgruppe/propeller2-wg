# Work Report


---

The goal is to implement a "readable PASM2 mnemonics + labels + directives" output with "--format=spin2" emission in Propan.

This shall be done in the following way:

1. Implement a simple "DAT \ BYTE ..." emission style
2. Use all files that pass semantic analysis in the test suite as a test corpus. Then use propan --format=flat as the reference value and propan --format=spin2 | flexspin -2 as a test miracle to confirm equality.
3. Implement emission of individual parts (segments filled with mnemonics/opcodes, data (long/word/byte) and padding (byte + padding comment)) as the next step of emission equality. This stell shall also already include labels so other spin2 code could use these. The instructions shall use numeric operands for everything.
4. Upgrade the metadata attached to the Module system so we can recover more information
5. Render full spin2 code, including constants, label or constant references in operands, and so on. Render function call results as integer constants. This step is likely best done by annotating the operands AST nodes to recover the syntax.

For each step, create a single commit on the current branch (work/spin2_emission) prefixed with [GPT-6 Sol High] so it's clear it was an AI model doing the work.

Step 5 is allowed to fail and is not required for the goal to succeed, but an attempt shall be done in "best effort". The goal is to make "okayish readable code", not "perfect mirror of propan code".

Report individual steps and decisions to WORKREPORT.md as soon as they are decided.

Also report all bugs encountered in propan into BUGREPORT.md with the typical schema (Bug, Reproduction, Fix).

## Step 1 — byte-only Spin2 output

Decision: `--format=spin2` writes `DAT` followed by `BYTE` rows of at most 16 hexadecimal values from the same merged image used by `--format=flat`. This guarantees identical content, including fill bytes, before introducing semantic rendering. FlexSpin accepts this form and emits a raw DAT binary.

## Step 2 — semantic corpus round-trip

Decision: discover every `.propan` file under `tests/propan`, then retain exactly those that assemble successfully to a flat binary without test-mode checklist handling. For each accepted file, compile the Spin2 output with `flexspin -2` and compare the resulting bytes to the flat image. This includes semantic and equivalence fixtures while naturally excluding parser-only and diagnostic fixtures that fail semantic analysis. A standalone script keeps this check runnable without modifying each existing test case.

## Step 3 — structured byte-exact emission

Decision: attach the emitted kind and source mnemonic to each line record. The Spin2 writer now emits `LONG` opcode words for encoded instructions, typed `BYTE`/`WORD`/`LONG` data, explicit `BYTE` padding with a comment, segment comments, and stable exported `p2_label_N` aliases with original names in comments. Raw opcode words preserve augmentation order and unusual encodings exactly; source mnemonic comments keep them recognizable. The 119-file corpus still round-trips exactly.
