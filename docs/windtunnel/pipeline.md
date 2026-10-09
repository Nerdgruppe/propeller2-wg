# Windtunnel instruction pipeline: implementation and validation plan

Status: implemented and acceptance-validated for the supported deterministic
instruction subset, 2026-10-09. The five-stage local pipeline, hub data scheduling,
instruction FIFO, command arbitration, and clocked startup pass the validation
matrix below. Exactness is limited to that subset and the measured oracle device;
collision data, debug execution, and exact pin/serial timing remain outside the
claim. No new instructions have been added.

Windtunnel needs clocked instruction fetch, operand capture, execution, and
writeback, together with hub arbitration and instruction FIFO behavior. A table
of instruction delays cannot reproduce the pipeline's observable behavior.
Implement the pipeline for the existing instruction coverage first.

## Implementation progress

- The native runner's `--characterize` mode assembles and uploads without running
  the simulator, validates WTOR frames, and retains complete decoded register
  snapshots and requested hub observations. `--trace-pipeline` retains edge traces
  with failures and prepared oracle cases.
- Cog/LUT execution has five stage positions, two-clock issue, captured operands,
  deferred result/flag writes, stalls, and branch flush/refill. Hardware agrees on
  GETCT/NOP/cancellation, WAITX, RDLUT/pixel latency, dependencies, LUT execution,
  and the deterministic self-modifying cog-code cases.
- Byte/word/long reads and writes pass all eight cogs, eight banks, eight phases,
  and four byte alignments. Reads have five clocks of grant-to-response latency;
  boundary-crossing transfers use successive slices. SETQ streams one result per
  clock. SETQ2 write source setup needs a third clock before its first grant.
- Hub stack calls/returns pass the same bank/phase/alignment/cog matrix.
  Cog/LUT-to-hub entry and hub-to-cog/LUT return timing pass all eight cogs,
  banks, phases, and four byte alignments. A 19-long instruction
  FIFO stops issuing reads at fifteen occupied words and receives reads after
  five clocks. Throughput and buffered-code edits/refetch pass at all four
  instruction alignments. Unaligned entry assembles across fetch/read stages and
  passes all cogs and alignments. All 64 FIFO/data contention runs also pass on hardware.
- Cog/lock commands use their own round robin (`CT & 7 == cog_id`), distinct from
  RAM slices, with two clocks for returned results. Eight-cog command timing
  probes pass. Command side effects are evaluated before any cog executes that
  edge; pending register/flag results retire later.
- Startup now streams the 504-long image with rotating grants and delayed
  responses. Device characterization covers all target cogs, source banks, and
  command phases and byte alignments for loaded, no-load cog, and no-load hub starts.
- First/middle/last SETQ/SETQ2 beats are visible at their measured edges to a
  second cog. Read and write probes cover all cogs, banks, lengths 1/2/8/9/64,
  and five offsets around each observation boundary.
- Cancellation, all test/decrement/increment branches, `_RET_`, cog/LUT code
  prefetch, local wrapping, software RDFAST/RFBYTE buffering, and shared
  LUT/attention events pass hardware checks. The FIFO continues filling after
  a return to cog execution; mode-transition contention probes verify this.
- Self no-load restart is tested with and without WC in every cog and phase.
  Without WC, one subsequent ordinary instruction commits before reset;
  startup remains timed from the original command grant. Pair startup also
  passes loaded/no-load hub, bank, and phase sweeps.
- Matrix fixtures use named `run:` sections with run-local `cog:` and assembler
  `const:` parameters. 228 wrappers were consolidated into 14 fixtures without
  dropping cases. See the [checklist syntax](check-list.md#runs-and-initial-values).

Execution results distinguish `not_implemented` (missing simulator support),
`trap`, and `illegal` with an explanatory string. Illegal execution stops the
cog, discards its pending pipeline writes, and stops the runner/CLI with that
reason. RDFAST/RFBYTE during hub execution are illegal because instruction
fetching already uses the FIFO. RFBYTE also requires a preceding RDFAST;
conditionally canceled instructions do not fault.

Validation includes the complete project suite, hardware reruns of the
consolidated fixtures, byte-aligned startup, hub address-map and boundary
recovery, and generator preservation. Existing UART support remains functional;
exact smart-pin/serial timing is outside this claim.

## Sources and evidence

The primary source is the [Hardware Manual, Instruction Pipeline](../p2/hardware-manual.md#instruction-pipeline),
including its four diagrams in `word/media` of the
[original DOCX](../p2/original/Hardware_Manual.docx). The Markdown references images
which are absent from its directory; the diagrams were inspected in the DOCX.
The adjacent Execution and Hub RAM chapters are also necessary.

The [original Silicon Documentation](../p2/original/Silicon_Documentation.docx)
requires two intervening instructions after modifying cog code. Its older
pipeline summary says at least four clocks after a branch, while the Hardware
Manual describes a five-clock instruction passage after refill. Use explicit
stage boundaries and hardware measurements to resolve timing, rather than
adding either number as a separate branch penalty.

The checked-in [timing results](../../data/timing-data/test-results.yaml) already
record NOP=2, ADDPIX=7, WAITX=2+D, cog JMP=4, and phase-dependent cog startup.
Their [fixture](../../data/timing-data/fixture.propan) and
[runner](../../data/timing-data/run_tests.py) subtract a two-clock GETCT baseline.
Reuse that measurement method through the existing native P2AAS oracle; the
timing-data runner itself uses a hardcoded serial port and optional Python tools.

Live measurements below used the existing `windtunnel-tests` oracle, the
provided `ws://localhost:12880/` endpoint, the scaffold's 200 MHz clock, and
`zig-0.16.0` from `minimum_zig_version`. The existing local suite passed; the
existing `mov-zero.propan` oracle fixture also passed on the device.

## What the five stages mean

Stages complete on clock edges. A full pipeline starts an ordinary instruction
every two clocks, although that instruction occupies five clocks:

| Clock interval | Instruction A                   | Instruction B                   | Instruction C                   |
| -------------- | ------------------------------- | ------------------------------- | ------------------------------- |
| 1              | Read instruction RAM            |                                 |                                 |
| 2              | Read D/S RAM, latch instruction |                                 |                                 |
| 3              | Latch D/S and instruction       | Read instruction RAM            |                                 |
| 4              | ALU / execution                 | Read D/S RAM, latch instruction |                                 |
| 5              | MUX / write result and C/Z      | Latch D/S and instruction       | Read instruction RAM            |
| 6              |                                 | ALU / execution                 | Read D/S RAM, latch instruction |
| 7              |                                 | MUX / write result and C/Z      | Latch D/S and instruction       |

The table transcribes the manual's diagram; it does not settle internal bypass
wiring. A writes while B captures its operands. Adjacent dependent ALU operations
must see the correct preceding result, even though their RAM-read stages overlap.
Implement the necessary forwarding/capture ordering and test it explicitly.

A conditionally canceled instruction retains its pipeline slot and normal
two-clock throughput. It has no executed side effects, including no wait for the
resource its opcode would otherwise require.

A resource wait freezes the instruction pipeline, including younger instruction
fetches and operand capture. It does not stop CT, other cogs, the hub, or events.
The manual describes wait insertion around the execute/final-store boundary;
retain the captured operands and resource request, wait for readiness, then
complete execution/writeback once. Never rerun one-shot effects on every wait tick.

A taken branch redirects fetch at the execution boundary, discards younger work,
and refills from the target. Preserve the branch's own result/flags/return state.
A branch to the immediately following instruction still refills. A false
condition or untaken test-and-branch does not flush.

Local fetch covers cog `$000..$1F7`, execute-only debug ROM `$1F8..$1FF`,
and LUT `$200..$3FF`; sequential execution
wraps `$3FF` to `$000`. Only a branch enters hub execution. Hub PCs are byte
addresses, instruction longs may be unaligned, and hub execution consumes a
prefetch FIFO. A branch to hub requires FIFO startup and phase-dependent access,
although this device shows no extra entry cost for unaligned instructions.
The adjacent FIFO slice completes the word at the read/decode stage; this stage
placement is an inference constrained by the measurements. Hub fetch cannot be modeled as direct RAM fetch
with a fixed 13-clock delay.

## Measured differences

These are unsigned `end_GETCT - start_GETCT` values, in clocks. Subtract the
empty measurement in the same execution mode to obtain the inserted instruction
cost. Absolute hardware CT values are not comparable to simulator startup CT.

| Probe                              | Original simulator baseline | Hardware | Hardware inserted cost |
| ---------------------------------- | --------------------------: | -------: | ---------------------: |
| Cog: adjacent GETCTs               |                           1 |        2 |                      0 |
| Cog: NOP                           |                           2 |        4 |                      2 |
| Cog: canceled conditional ADD      |                           2 |        4 |                      2 |
| Cog: taken JMP to next instruction |                           3 |        6 |                      4 |
| Cog: WAITX 0                       |                           4 |        4 |                      2 |
| Cog: WAITX 3                       |                           7 |        7 |                      5 |
| Cog: RDLUT                         |                           2 |        5 |                      3 |
| Cog: ADDPIX                        |                           2 |        9 |                      7 |
| Cog to aligned hub `$4000`         |                           3 |       22 |      20 in this sample |
| Hub: adjacent GETCTs               |                           1 |        2 |                      0 |
| Hub: NOP                           |                           2 |        4 |                      2 |
| Hub to cog                         |                           3 |        6 |                      4 |

The WAITX matches are accidental: the current repeated-execution wait and
one-clock ordinary scheduler compensate in these samples. After introducing
two-clock issue, account for WAITX's existing base two clocks only once.
Likewise, five-clock passage through an empty pipeline is not five extra clocks
to add on top of the measured four-clock cog JMP cost.

An aligned RDLONG from bank 0 was also measured at all eight CT phases in cogs
0 and 7. Requested offsets 0..7 yielded these raw GETCT deltas:

| Target cog | Observed start CT modulo 8 | Raw deltas              | RDLONG costs after subtracting 2 |
| ---------- | -------------------------- | ----------------------- | -------------------------------- |
| 0          | 4,5,6,7,0,1,2,3            | 11,18,17,16,15,14,13,12 | 9,16,15,14,13,12,11,10           |
| 7          | 4,5,6,7,0,1,2,3            | 12,11,18,17,16,15,14,13 | 10,9,16,15,14,13,12,11           |

Windtunnel reports 2 for every raw delta. For this particular bank-0 sequence,
the measured read cost fits `9 + ((4 - start_CT - cog_id) & 7)`. This supports
the existing positive cog-ID direction in `ram_slice()`, but the constant 4
includes the GETCT-to-request stage offset. It is not a general arbitration
formula. Sweep target banks and request types before deriving that formula.
The [phase results](pipeline-probes/hub-read-phases.json) retain individual rows.
The [characterization record](pipeline-probes/characterization.json) records
decoded hardware values, uploaded-image hashes, and frame checksums. Two device
sweeps reproduced these timing and phase values. All 19 local probe runs passed;
the hardware self-modifying probe passed and the other 18 runs reported the
expected timing mismatches. Board/silicon identity was not supplied by the frame;
record it alongside the configured clock in future characterization runs.

The [self-modifying probe](pipeline-probes/self-modify.propan) passed locally and
on hardware: adjacent modified code executes its old prefetched word; two NOPs
allow the new word; a taken branch refetches the new word. Adjacent dependent
arithmetic and flushed fall-through code also produced their expected results.
Do not give the one-spacer cog RAM read/write collision a universal old/new
answer: the [community errata](../p2docs.github.io/page/errata.md)
reports device/frequency-dependent bits. Characterize it separately and expose
a collision diagnostic rather than using it as a deterministic conformance test.

## Changes identified in the original code

| Location                                                       | Current behavior                                                   | Required change                                                                                                    |
| -------------------------------------------------------------- | ------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------ |
| `src/windtunnel/sim/Cog.zig:step`                              | Promotes one prefetched instruction and executes it each tick      | Explicit valid stage latches and a two-clock issue phase; freeze/flush rules                                       |
| `Cog.zig:PipelineState`                                        | PC, raw word, ALTx overrides only                                  | Captured operands, effective fields, source execution mode, and pending result/request state                       |
| `Cog.zig:fetch_instruction`                                    | Reads local memory immediately; rounds hub PC down                 | Clocked fetch, raw RAM versus special-register access, byte-correct hub instruction assembly from FIFO             |
| `Cog.zig:jump`                                                 | Clears only `next_instruction`                                     | Redirect fetch, invalidate every younger slot/request, restart hub FIFO when required                              |
| `execute.zig`                                                  | Reads live registers and writes results/flags/other state directly | Separate preparation, one-shot execution, resource progress, and edge-appropriate commit                           |
| `execute.zig:alter`, `alti`, forwarded S operations            | Modify `next_instruction` directly                                 | Address the following instruction's actual pipeline latch; apply field changes before the relevant operand capture |
| `execute.zig:readMemory`, `writeMemory`, pointer calls/returns | Immediate access, one long per wait tick for blocks                | Hub requests, grant and response latency, streaming beats, final flag/result timing                                |
| `Hub.zig:step`                                                 | Sequential cog calls see earlier cog writes in the same tick       | Defined simultaneous edge evaluation/commit and shared-resource arbitration                                        |
| `Hub.zig:start_cog`, `Cog.zig:reset`                           | Loads/starts immediately                                           | Startup/load state and first-fetch timing; clear all in-flight work on stop/restart                                |
| `test_suite.zig`                                               | Checkpoint on prefetched PC; validates local PC as a register      | Checkpoint after older writes drain; validate retired/dispatched code with cog/LUT/hub address mapping             |
| `test_suite.zig` deadline shortcut                             | Jumps CT during a cog-0 wait                                       | Disable initially; restore only with a proof that no pipeline or resource action occurs before the deadline        |
| `windtunnel.zig` logging                                       | Uses current/next instruction slots                                | Distinguish fetch, execute, and retirement events in tracing                                                       |

Inspect all direct register/flag/state writes in the executor, not just ALU
helpers. Existing call/return addresses based on `dispatch_pc` are a useful
foundation; never substitute the speculative fetch PC. Verify instruction fetch
from special-register addresses against hardware instead of assuming operand
read semantics apply to instruction RAM.

Executor regeneration preserves marked bodies but regenerates surrounding
dispatch code. Update `utility/gen_windtunnel.py` if changing generated plumbing;
do not regenerate the executor blindly. Do not regenerate decoding/encoding or
add ISA entries merely to implement pipeline stages.

## Implementation order and acceptance gates

1. **Make characterization independent of simulator success.** Retain source,
   uploaded image, device frame, CRC-checked decoded observations, and simulator
   trace. Add a small explicit hardware-characterization mode to the existing
   runner which assembles/uploads without requiring the local simulation to
   satisfy postconditions first, and reports measurements rather than a
   conformance pass. This is needed for currently trapped unaligned fetch and
   unsupported LUT fixture layouts. Keep the normal oracle strict. Allow an
   observation without inventing a simulator-baseline assertion if useful.
   Gate: a deliberately wrong local result does not prevent collecting hardware
   evidence in characterization mode; corrupt/truncated frames remain failures.

2. **Clock the local cog/LUT pipeline.** Add five stage positions/valid bits and
   two-clock issue phase. Keep the existing semantic ALU implementation. Capture
   operands once; forward preceding results/flags at the appropriate boundary;
   queue result and side effects for their actual commit edge. Capture GETCT at
   its measured stage, including the two-instruction 64-bit capture form.
   Gate: stage traces show isolated five-clock passage and steady two-clock
   issue; dependent D/S/C/Z chains, conditions, augmentation, ALTx/ALTI, and all
   existing functional fixtures pass. NOP/canceled ADD/GETCT timings match hardware.

3. **Add stalls and redirects.** Freeze younger stages, retain latched inputs,
   and make waits idempotent. Redirect on every taken JMP/CALL/RET, event branch,
   test/decrement branch, and `_RET_`; preserve the retiring branch's effects.
   Gate: WAITX 0/1/3/large, event waits/timeouts, cog and LUT branch/call timing,
   untaken branches, branch-to-next-PC, self-modifying code, and canceled slow
   instructions pass. A younger instruction must never execute or consume a
   prefix during a stall/flush. Add existing RDLUT=3 and pixel=7 latency here.

4. **Schedule hub data access.** Compute bank from `(byte_address >> 2) & 7`.
   Model rotating windows, grant/response edges, byte enables, boundary-crossing
   accesses, and SETQ/SETQ2 streams. Preserve existing block and augmentation
   errata. Block writes/read results can become visible on individual beats;
   do not defer an entire block until its final retirement. Gate: reads/writes
   match every bank, phase, alignment, and cog ID; first beat, one-long-per-clock
   continuation, final flags, pointer updates, and waiting instructions' captured
   operands are correct. The Hardware Manual's phrase about the lower three
   address bits refers to slice selection for longs; using byte bits 2..4 is
   consistent with eight slices of 32-bit RAM and requires the bank sweep.

5. **Feed hub instruction fetch through the FIFO.** Implement just the internal
   hubexec FIFO support required by already implemented execution, including
   phase-dependent fill, unaligned byte assembly, occupancy/refill, branch reset,
   sequential wrap, and contention with data accesses/block transfers. The existing limited RDFAST 0/RFBYTE path shares this controller;
   other software FIFO/streamer forms remain outside current coverage. Gate: cog/LUT/hub transition matrix, target banks/phases/alignments,
   straight-line hub throughput, branches while refill is pending, and hub code
   mixed with random data access/block transfers match the device. Self-modifying
   hub code must respect already prefetched words, rather than reading RAM live.

6. **Finish shared edge ordering and lifecycle timing.** Schedule cog startup,
   504-long image loading, no-load restart, stop and lock effects, LUT sharing,
   attention/event delivery, and supported I/O effects. Avoid dependence on the
   host order of cog iteration. Gate: two-cog producer/consumer and stop/restart
   probes, all target cogs 0..7, counter wrap, and existing lifecycle fixtures
   pass. Reconcile functional UART support separately before claiming exact pin
   or serial timing.

Each step should retain the existing local semantic suite and promote stable
hardware measurements into ordinary `.propan` regression fixtures. Calibrate
one shared timing rule per resource, not a special-case delay for each probe.
Startup and FIFO support are part of pipeline exactness even though they are
not additional instructions. Interrupts, REP/SKIP, CORDIC, and streamer are
outside current coverage; a pipeline implementation must leave unsupported
instructions explicit and limit its exactness claim accordingly.

## Hardware validation design

Use existing cog fixtures and WTOR snapshots. Put GETCTs and output registers
inside the DUT; reporting/UART occurs after measurements. Keep timing loops,
preparation, AUGS/AUGD, and branch targets explicit in the assembled instruction
stream. `aug(...)` can add an instruction and alter the phase. Validate the
uploaded encoding and target layout before attributing a difference to hardware.

For a measurement with an inserted sequence, use the empty GETCT pair from the
same mode as its baseline. Repeat device runs and require identical deterministic
results, rather than accepting a range that could hide an off-by-one clock.
For variable hub timing, align a future CT target with ADDCT1/WAITCT1, sweep all
eight offsets, and **record actual GETCT phase**. WAITCT1 itself has a release
offset: the current probe's requested offset 0 yields GETCT phase 4. Align
simulation and hardware by the measured phase and cog/bank identity, not by
wall-clock startup. Compare modulo-32-bit timestamp differences and test wrap.

| Observable               | Required probes                                                                                             |
| ------------------------ | ----------------------------------------------------------------------------------------------------------- |
| Throughput and fill      | Empty/1/2/long ALU sequences in cog, LUT, hub; entry and mode transitions                                   |
| Data and flag forwarding | Immediate producer/consumer D/S chains, ADDX/CMP/conditional consumer, GETCT-to-ADDCT                       |
| Cancellation             | False ALU, false WAITX, false hub read, false branch, canceled augmentation/ALTx                            |
| Redirect                 | Taken/untaken branch families; jump-to-next; call/return flags/address; poisoned fall-through stores        |
| Prefetch visibility      | Cog edits at distances 0/2/3; jump after edit; LUT and hub prefetched-code edits                            |
| Stalls                   | WAITX and event waits; operand/prefix changes by another cog while a resource is pending                    |
| Hub arbitration          | All 8 banks × 8 phases × 8 cog IDs; byte/word/long, offsets 0..3, reads and writes                          |
| Blocks                   | SETQ/SETQ2 lengths 1/2/8/9/long; register wrap, tail RAM, pointer errata, every visible beat                |
| FIFO                     | Every hub target alignment, bank, phase; long straight-line runs, repeated branches, data-access contention |
| Shared/lifecycle         | Concurrent grants, LUT sharing, attention/event set versus clear, start/stop/restart and load timing        |

CT deltas test timing but cannot independently identify every internal edge.
Use self-modifying code, dependencies, poisoned fall-through, and a second cog's
timed observations of writes/events to constrain side-effect placement. Local
stage trace assertions then check the proposed microarchitecture. Successful
black-box probes establish externally observable equivalence for their coverage,
not proof of every hidden stage or every unsupported instruction.

Stop checkpoints capture at the execution slot, after older writes commit and
before the reporter changes DUT state, including flags, Q, and prefix state.

## Calibrated resource rules

These are raw GETCT deltas. Let `p` be the measured starting GETCT phase,
`c` the executing cog ID, `b = (address >> 2) & 7`, and `x` be one when a
byte/word/long transfer crosses a long boundary, otherwise zero.

| Operation | Raw delta |
| --- | --- |
| Ordinary local instruction, including cancellation | `4` |
| Taken local branch | `6` |
| WAITX D | `4 + D` |
| RDLUT / pixel instruction | `5` / `9` |
| Hub data read | `11 + x + ((b + 4 - p - c) & 7)` |
| Hub data write | `5 + x + ((b + 4 - p - c) & 7)` |
| Hub branch entry, all four byte alignments | `15 + ((1 + b - p - c) & 7)` |
| Shared command without / with returned result | `4` / `6`, plus `((6 + c - p) & 7)` |

With an intervening SETQ, an N-long stream adds `N - 1` clocks and uses
phase expression `((b + 2 - p - c) & 7)`, with read/write bases 13/7.
SETQ2 writes need three source-setup clocks: when that expression is zero,
the first window is missed and the delay becomes eight. FIFO contention may
also defer a data grant by another eight clocks; it follows occupancy and
pending return state, rather than a fixed per-instruction penalty.

For command grant clock `g`, startup's first GETCT is `g + 21` for a
no-load cog start, `g + 531 + ((b - g - 17 - target) & 7)` for an image
load, or `g + 30 + ((b - g - 22 - target) & 7)` for a no-load hub start.
Loading requests one long per clock and receives each five clocks later.
Commands grant at `CT & 7 == issuing_cog`; RAM slice access instead uses
`(CT + requesting_cog) & 7`. These are separate arbiters.

| Acceptance gate | Regression evidence |
| --- | --- |
| Characterization independent of simulation; bounded frames | Characterization preparation test, native capture records, existing P2AAS frame validation |
| Five stages, dependencies, flags, augmentation and ALTx | Stage/writeback unit test; ALU/core functional suite; `pipeline-cog`, `pipeline-lut` |
| Stalls, cancellation, redirects and prefetch | `pipeline-branches`, `pipeline-cancellation`, cog/LUT prefetch, core events and calls |
| Hub transfers, stacks, blocks and visible beats | `pipeline-hub-matrix`, `pipeline-hub-stack`, `pipeline-hub-block`, `pipeline-block-visibility` |
| FIFO entry, alignment, buffering, contention and wrap | Hub entry/FIFO, software FIFO/prefetch, contention, mode transition, and hub wrap fixtures |
| Shared edges, startup and lifecycle | Shared events, dual LUT writes, LUT source events, startup/pair/restart fixtures; stop-clearing and counter-wrap unit tests |
| Generator and ordinary CLI | Executor regeneration comparison, CLI pipeline trace; trap/not-implemented/illegal diagnostics and cancellation tests |

Acceptance used `zig-0.16.0 build install test --summary all`: all 19 build
steps succeeded, 32 Windtunnel unit tests and 102 Propan tests passed, and two
existing Propan tests were skipped. All 77 Windtunnel fixtures / 369 runs passed.
A single complete hardware invocation after the review fixes also exited 0:
367 runs passed on P2AAS, and two existing fixtures explicitly disabled the
oracle because they have no UART observation. This includes the 64-run
cog/LUT-to-hub entry and hub-to-cog/LUT return matrix across all cogs, banks,
phases, and byte alignments. Earlier boundary failures and corrected reruns
remain recorded as historical investigation evidence.

The [validation record](pipeline-probes/validation.json) lists every current
fixture, source hash, run name, and hardware result, together with command
summaries and evidence hashes. Executor regeneration is byte-identical after
formatting. CLI tracing and illegal-execution diagnostics were exercised directly.
The [bug report](../../BUGREPORT.md) records seven review findings, their
reproductions and fixes. Counter-rollover regressions additionally cover delayed
events, FIFO/data responses, waits, idle skipping, UART and image startup.

## Device findings and limits

The simulator follows this oracle device's measured behavior where a timing
summary disagrees. In particular, a GETCT WC followed by GETCT returns the low
word at the second instruction's current clock on this device: a preceding
ordinary GETCT measures deltas 4 and 6 for the following two reads. The manual's
coherent 64-bit snapshot description is not established by these measurements;
board/silicon revision is not reported by the oracle. Counter wrap is checked
locally, but a coherent snapshot across wrap is not claimed for this device.

The hub has a 20-bit address map: `$00000..$7FFFF` is RAM,
`$80000..$FBFFF` reads zero and ignores writes, and `$FC000..$FFFFF`
mirrors the final 16 KB. The original model incorrectly aliased the entire
upper half onto RAM. Sequential hub execution across `$7FFFF` therefore
executes zero/NOP words until another cog restarts it; wrapping the 20-bit
address counter from `$FFFFF` continues reading real hub RAM at zero. A
second-cog recovery probe and explicit data reads/writes verify both boundaries.
Ordinary branches to low addresses still select cog/LUT execution.

Cog debug ROM is outside the implemented instruction subset. Data block access
still uses the underlying eight RAM words; instruction fetch marks the ROM and
reports an explicit `not_implemented` fault at execution. Younger ROM fetches flushed
by a branch do not fault. The [debug-ROM experiment](pipeline-probes/debug-rom-entry.propan.in)
is deliberately excluded from the runnable characterization glob: it enters
unsupported native debug context and cannot return a valid WTOR frame.

Concurrent LUT writes to distinct addresses commit together. Same-address
collisions produced different results for different cog pairs; the simulator
records a collision and chooses a deterministic local-write result. Such
collisions and simultaneous instruction-fetch/data-write collisions are exposed
in traces, and their data values are outside the deterministic exactness claim.

The implemented subset excludes interrupts, REP/SKIP, CORDIC, streamer, debug
execution, and general smart-pin timing. This implementation adds no instructions.

## Reproduce this investigation

The [probe directory](pipeline-probes/) is deliberately outside `tests/windtunnel`.
Its timing postconditions describe the original simulator baseline, allowing the
characterization runner to record hardware. Their old timing assertions are
not passing expectations for the new simulator. They are characterization
inputs, not passing pipeline-conformance tests. Their stable measurements are
now represented by the normal pipeline fixtures, using hardware expectations.

```sh
zig-0.16.0 build install test-windtunnel
P2AAS_ENDPOINT=ws://localhost:12880/ zig-out/bin/windtunnel-tests \
  --characterize --artifact-dir .zig-cache/windtunnel-pipeline-evidence \
  docs/windtunnel/pipeline-probes/*.propan
```

Native oracle evidence includes `fixture.propan`, `original.bin`, `uploaded.bin`,
`oracle.propan`, `simulation.json`, `request.json`, `hardware-output.bin`, and
`transport.txt` on a mismatch. The device frame contains all 506 snapshot
registers, even when only some are asserted. Successful conformance runs delete evidence;
`--characterize` retains it, with `hardware.json` containing decoded values. Verify frame magic/version,
payload length/count, and CRC before interpreting register values.

Completion means that the existing supported instruction subset has matching
clock timing **and** side-effect/prefetch behavior across the validation matrix.
The acceptance gates above have passed for that scope. Extend the matrix and
repeat native validation when expanding instruction or device coverage.
