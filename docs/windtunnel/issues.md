# Simulation issues and service errors

`Hub.step()` returns `normal`, `breakpoint` or `reset`. After every edge, including
reset and breakpoint notifications, the caller inspects `Hub.issues.items()` and
any failures in the endpoint or VCD objects it owns and chooses to monitor. Hub
and IO do not choose a host policy, scan caller-owned adapters, or print diagnostics.
An issue does not stop a cog or discard its pipeline. Rejected operations retire
without their rejected effect; unrelated work completes normally.

The CLI and P2AAS server continue after warnings and stop after traps. The raw CLI
renders short warnings inline as `[warning text]`, starts trap details on a new
line, and writes a final CR/LF before restoring the terminal. Logging is used only
outside that raw-terminal scope. The GUI explicitly pauses on any issue. Frame
scheduling and serial boot call the observer supplied by their caller after each
edge. Strict aggregate checks and verbose dumps exist only in `test_support.zig`.

The next step clears issues. A paused host must not call step to refresh the UI.
Firmware reset retains issues collected earlier on its issuing edge; explicit
host initialization starts a fresh collection. Execution breakpoints still stop
before an edge. Data watchpoints retain every match from a completed edge, even
when that edge also reports issues. There is no textual pipeline trace; VCD remains.

## Detector inventory

Bounds below are per completed edge. Writer/reader provenance is captured at
the operation or retained with the pending transfer, never inferred from a later
instruction. Read/write conflicts also identify the actual source: addressed RAM/ROM,
old cog-port data, or the address of the retained LUT sample. Configuration replacements preserve both issuers. Pin command
collisions retain all participating commands, including OR-combined values.

| Issue kind                   | Severity | Trigger and modeled behavior                                                                                                                                                                                                                                                                                               | Bound per edge                                |
| ---------------------------- | -------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------- |
| `invalid_encoding`           | trap     | An executed instruction has no valid decoded operation. Reject and retire it.                                                                                                                                                                                                                                              | 1 per executed instruction/cog                |
| `hub_fifo_conflict`          | trap     | A software FIFO instruction, RDFAST/WRFAST/FBLOCK, or FIFO-based streamer command attempts to use the FIFO during hub execution. Reject and retire it.                                                                                                                                                                     | 1 per executed instruction/cog                |
| `fifo_read_unconfigured`     | trap     | A software RF instruction has no read FIFO configured. Reject and retire it.                                                                                                                                                                                                                                               | 1 per executed instruction/cog                |
| `fifo_write_unconfigured`    | trap     | A software WF instruction has no write FIFO configured. Reject and retire it.                                                                                                                                                                                                                                              | 1 per executed instruction/cog                |
| `streamer_fifo_mismatch`     | trap     | A FIFO-based streamer command is issued without an active FIFO engine/address or with the wrong FIFO direction. Reject the command.                                                                                                                                                                                        | 1 per executed instruction/cog                |
| `xbyte_fifo_unconfigured`    | trap     | XBYTE is requested without a read FIFO configured. Reject and retire it.                                                                                                                                                                                                                                                   | 1 per executed instruction/cog                |
| `hub_skipf`                  | trap     | SKIPF is executed in hub mode. Apply the simulator's explicit rejection policy and retire it.                                                                                                                                                                                                                              | 1 per executed instruction/cog                |
| `streamer_mode`              | trap     | A command decoder rejects a reserved or uncharacterized format, including F/8 and F/9. Reject before publication; preserve active/pending commands.                                                                                                                                                                        | 1 per executed instruction/cog                |
| `pc_domain`                  | warning  | Fetch in hub mode below $400, or cog/LUT mode at or above $400. Preserve actual fetch behavior.                                                                                                                                                                                                                            | 1 per fetch/cog                               |
| `cog_fetch_write`            | warning  | An executed instruction was physically fetched during a write to the same cog RAM long. Discarded or condition-false fetches are silent; retain the original overlap clock, writer, reader and read source.                                                                                                                                                                            | One per matching writer, within 32 writes/cog |
| `lut_read_write`             | warning  | A companion LUT write takes the read port from a subsequent streamer/DDS lookup, even at a different address. Retain the earlier LUT sample and its source address.                                                                                                                                                        | 1 per sampling operation/cog                  |
| `reg_write_conflict`         | warning  | Different captured issuers commit writes to the same cog register/RAM address on one edge. Preserve commit order and both issuers.                                                                                                                                                                                         | Within 32 committed writes/cog                |
| `lut_write_conflict`         | warning  | Different captured issuers write the same receiving LUT address on one edge, including simultaneous local/shared companion writes. Preserve the local-write convention and both issuers.                                                                                                                                   | Within 32 committed writes/cog                |
| `pin_command_collision`      | warning  | Multiple cog commands reach the same pin on one command-delivery edge, including identical commands. Preserve OR combination and every contributor.                                                                                                                                                                        | 1 per pin                                     |
| `pin_buffer_overwrite`       | warning  | A UART/USART/USB pending transmit word is overwritten before consumption. Retain discarded/new words and contributors.                                                                                                                                                                                                     | 1 per pin                                     |
| `usb_receive_error`          | warning  | USB receive error changes from clear to set while a packet/EOP is active or was active on the preceding update. Preserve hardware status.                                                                                                                                                                                  | 1 per pair                                    |
| `smart_dac_clipped`          | warning  | A new Y write to a physical DAC dither mode has low 16 bits above $FF00. Preserve saturation; excludes DAC noise and repository aliases.                                                                                                                                                                                   | 1 per pin                                     |
| `streamer_fifo_lost`         | trap     | An active command requiring a FIFO finds no engine, the wrong direction, or competing hub execution. Skip this sample; retain the active command.                                                                                                                                                                          | 1 per sampling operation/cog                  |
| `streamer_starvation`        | warning  | A FIFO-sourced streamer needs a new source word but none is available. Preserve zero substitution.                                                                                                                                                                                                                         | 1 per source reload/cog                       |
| `streamer_capture_dropped`   | warning  | An automatic capture cannot enqueue its bytes because its write FIFO is full. Repeated nonsequential FBLOCK bank changes can reduce drain bandwidth below the capture rate; steady sequential full-rate capture does not overflow in the model. Preserve the dropped sample; silicon overflow handling remains unmeasured. | 1 per capture/cog                             |
| `streamer_capture_displaced` | warning  | A simultaneous software WFBYTE/WFWORD/WFLONG takes the FIFO write opportunity from automatic capture. Retain both producers; preserve software priority.                                                                                                                                                                   | 1 per capture/cog                             |
| `fifo_configuration_lost`    | warning  | A new FIFO configuration replaces an undelivered request while previous writes are draining. Preserve both issuers and replacement behavior.                                                                                                                                                                               | 1 per cog                                     |
| `cog_start_replaced`         | warning  | A new cog-start request supersedes a scheduled start before delivery. Preserve both issuers and replacement behavior.                                                                                                                                                                                                      | 1 per target cog                              |
| `memory_hole_read`           | warning  | A simulated byte read addresses $80000..$FBFFF after 20-bit wrapping. Preserve the returned zero, including FIFO/block traffic.                                                                                                                                                                                            | Included in 128/cog allowance                 |
| `memory_hole_write`          | warning  | A simulated byte write that is not masked off addresses $80000..$FBFFF after wrapping. Preserve the ignored write.                                                                                                                                                                                                         | Included in 128/cog allowance                 |
| `memory_protected_read`      | trap     | Debug protection blocks a byte read through the lower $7C000..$7FFFF alias. Preserve the returned zero; the upper mirror remains readable.                                                                                                                                                                                 | Included in 128/cog allowance                 |
| `memory_protected_write`     | trap     | Debug protection blocks a byte write to protected RAM: the lower alias, or the upper mirror without debug-origin authorization. Preserve the ignored write.                                                                                                                                                                | Included in 128/cog allowance                 |
| `pad_undervoltage`           | warning  | Any analog substep takes a pad below configured minimum voltage. Aggregate signed voltage extrema by pin.                                                                                                                                                                                                                  | 1 per pin, independent of oversampling        |
| `pad_overvoltage`            | warning  | Any analog substep takes a pad above VIO plus configured margin. Aggregate voltage extrema by pin.                                                                                                                                                                                                                         | 1 per pin, independent of oversampling        |
| `pad_overcurrent`            | warning  | Any analog substep exceeds configured absolute pin-current limit. Aggregate signed current extrema by pin.                                                                                                                                                                                                                 | 1 per pin, independent of oversampling        |
| `unsupported_hubset`         | warning  | HUBSET has an unsupported command tag, or clock validation rejects XI/PLL selection with its input disabled, PLL selection with PLL disabled, or a zero/unrepresentable frequency. Retain the previous configuration and issuer.                                                                                           | 1 per cog                                     |
| `uncharacterized_pll`        | warning  | An accepted clock change enters uncharacterized response coverage, including unsupported reference conditions. Preserve the explicit fallback.                                                                                                                                                                             | 1 per cog                                     |
| `unsafe_postdivider`         | warning  | HUBSET newly flags the known unsafe PLL /2-to-/1 postdivider transition with unchanged ratio, XI divisor 1 and VCO multiplier 16.                                                                                                                                                                                          | 1 per cog                                     |

Ordinary RAM read-after-write ordering, FIFO grant arbitration, command-buffer
backpressure, defined masked writes and deliberate same-instruction register
updates are not issues. Startup/transfer writes participate in actual physical
write reporting. Debugger/inspector reads outside stepping do not produce issues
or watchpoint traffic.

All documented HUBSET command families are present: clock (`$0` high nibble),
reset (`$1`), debug/protection (`$2`), input filters (`$4`), and RNG seed (`$8..$F`).
Command nibbles `$3`, `$5`, `$6`, and `$7` remain unsupported. Invalid clock-source
settings share that diagnostic; limited PLL characterization has its separate kind.

TAQOZ in the shipped ROM executes `HUBSET $3000_0000` on startup after Escape.
Its `WP` word uses the old `%001P` protection encoding. Final silicon documentation
instead defines `%0010_xxxx_xxxx_xxLW_DDDD_DDDD_DDDD_DDDD`; no `$3` command is
documented. This currently emits `unsupported_hubset` and leaves configuration
unchanged. The ROM boot regression expects this exact diagnostic once. It is not
silently mapped to the newer protection command: actual silicon behavior remains
uncharacterized. Clock bits 25..27 are documented as ignored and are accepted.

Hub transfers assert that no two physical reads or writes touch the same long
on one edge, including the upper RAM mirror. Byte lanes of one transfer count once,
including the disjoint trailing/leading pieces of consecutive unaligned block-write
beats from the same instruction. Repeated reads, overlapping writes, different
instructions and different producers cannot share a long. Buffered FIFO consumption
does not claim hub RAM. Startup, FIFO grants, ordinary
memory grants and deferred writes all participate. Arbitration violations are
simulator assertion failures. Hub memory writes have no write-conflict issue kind. Ordinary register operand dependencies use defined writeback/forwarding;
`cog_fetch_write` is emitted once, when a fetched instruction executes, for its earlier physical fetch/write overlap. The payload clock identifies that original overlap; the issue clock identifies execution. A jump that discards a prefetched data word produces no fetch/write issue.

Internal assertions also reject overwriting startup/FIFO read responses or a pending
deferred hub write. FIFO checks reserve space for buffered and in-flight reads and
require queued write bytes to equal ready bytes plus both eligibility stages.
CORDIC checks count all physical input, arithmetic and pending-return entries;
each cog's pending count includes only its tracked entries. Reset-abandoned entries
remain physical work and may still deliver results. Shared commands assert their
owning cog's command slot and at most one issuer per edge. Exact-edge startup,
command and streamer deliveries assert that their deadlines have not passed,
using counter-rollover-aware comparisons.

Bank ownership is not yet asserted globally. Unaligned startup currently assembles
each image word by reading bytes from two adjacent RAM longs on one edge. A strict
one-bank-grant-per-cog check requires resolving that transport model first; it must
not silently change the established startup behavior or reject TAQOZ's unaligned image.

The startup-underflow fixture establishes `streamer_starvation` when a nonblocking
RDFAST is followed by XINIT before the first FIFO response; the initial lookup gets
zero and is not retried when data subsequently arrives.

## Storage and allocation contract

`Issue.Collection.capacity = 8 * (128 + 3) + 64 * (8 + 3)`. The generous per-cog
allowance includes byte accesses, physical writer pairs, fetch, instruction,
streamer and pending-work diagnostics. Per-pin non-electrical producers require
at most four reports, within the allowance of eight. Analog substeps merge extrema.
Insertion checks the bound in every optimization mode and never drops a report.

Physical hub-grant tracking reserves 64 records: eight cogs, four producer phases
(startup, FIFO, ordinary memory and deferred writes), and at most two longs per phase.
Actual committed write tracking reserves 32 entries per cog. Watchpoint storage
reserves 128 accesses per cog per edge per configured watch. Conservative access
accounting per cog: startup 5; FIFO 8; crossing memory reads and consumed lanes 11;
instruction/stack/operand services 32; operand capture 6; fetch 1; writeback/shared
LUT 5; streamer 8. This totals 76, below 128. Execution encounters have eight fixed
slots. Runtime bound checks expose new producer sites that outgrow these contracts.
Watchpoint additions reserve capacity before publishing the definition; removals
retain that capacity. Setup allocation failure remains a host error.

Source/sink adapters contain an endpoint and a closed, sticky error field. A
failed read returns no word; a failed write completes simulation without further
endpoint attempts. `reset()` or replacing the descriptor explicitly re-enables
the adapter. Source readers only consume buffered data. Callback implementations
and writer implementations supplied by callers must not allocate while stepping.
Built-in file/socket writers use fixed buffers; the checklist runner prepares its
output capacity before simulation. VCD capture, quiet-time emission and flushing
likewise retain their first typed export error and stop further emission.

Service errors remain on adapters and VCD, separate from architectural issues.
Hosts print both categories, then stop before another step. A retained service failure
prevents the frame runner from advancing until the endpoint/export is reset or rebound. The GUI Debugger opens,
expands and takes focus on an issue stop; diagnostics remain visible when automatic
inspector UI is hidden.

## Streamer coverage and checklist assertions

Streamer mode data is supplied at runtime. Compilation cannot prove its semantic
coverage. The independent test enumerates all 65,536 upper control words against
literal patterns from the shipped silicon mode table, including variable pin and
configuration bits, and representative counts/start kinds. `$F/$8` and `$F/$9`
receive reserved-mode trap issues rather than a missing-implementation state.

The executor generator uses compile errors for absent handlers and newly generated
placeholder bodies. Exhaustive dispatch detects absent structural cases; runtime
tests establish mode classification and rejection semantics.

Checklist fixtures declare exact total issue counts with `issues: KIND = COUNT`.
The common declaration applies to every run; a run-local declaration replaces the
common count for that kind. Unspecified kinds require zero occurrences. Unexpected
issues print the entire stopping edge and fail immediately; expected issues allow all architectural assertions to run.
Local failure evidence retains completed-edge diagnostics in `issues.txt`.
Rejected reads report once per failure kind per read access, at the first blocked byte. A RDLONG wholly in the hole therefore produces one `memory_hole_read`; an access crossing protected RAM into the hole can produce one of each kind. Rejected writes still identify individual byte lanes. Masked-off zero bytes are silent. The upper
mirror follows its existing debug authorization; address rollover itself is not an issue.
Smart-DAC clipping warnings apply to the two dither modes, where Y supplies a code
plus an eight-bit fraction, and exclude DAC noise output.

Expected simulator rejection policy must use `oracle: off` rather than claiming
the fallback describes undocumented silicon behavior. `issues-hub-skipf.propan`
asserts the explicit hub SKIPF rejection and execution of all following instructions;
the remaining `repeat-skip-modes.propan` runs retain their hardware timing assertions.

Generator regression checks: `.venv/bin/python utility/test_gen_windtunnel.py`. The optional
compiler check selects the versioned binary from `build.zig.zon` and verifies an instantiated
generated placeholder fails for its explicit missing-body guard.

Run suites in Debug or ReleaseSafe. The registered suite command is:

```sh
zig-0.16.0 build install test test-libp2 -Dno-gui=true -Doptimize=ReleaseSafe -Dcoverage=false -Dwindtunnel-jobs=4 -j2
```

The GUI is compiled separately; popup/focus behavior remains a manual acceptance check.
The finite vacant-opcode hardware probes are prepared, but their primary P2AAS endpoint
was unavailable. The invalid-encoding fallback remains explicit pending observations.
