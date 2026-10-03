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

## Step 4 — self-contained instruction metadata

Decision: each emitted instruction line now keeps its evaluated operands, rendered source operand syntax, source expression kind, condition, and effect. Operand syntax and variable-length evaluated values are owned by the returned `Module`; a regression test destroys parser storage and changes its source buffer before reading the metadata. This gives stage 5 readable source material without coupling the emitter to parser lifetime.

## Step 5 — readable mnemonics and references, best effort

Decision: emit real PASM2 mnemonics for long-aligned, single-word instructions when their operand encoding is an ordinary register or immediate and numeric values fit. Preserve conditions and WC/WZ/WCZ effects where FlexSpin round-trips them. Emit safe integer constants in `CON`, use stable sanitized label/constant names in operands, and substitute evaluated integers for function calls. Other instruction forms retain exact `LONG` opcodes with source-like mnemonic and operand comments, including evaluated function results. The fallback is necessary for relative branches, pointer forms, augment sequences, and packed instructions; exact bytes remain the first constraint. A dedicated fixture checks real mnemonic, label, constant, and function-result output.
