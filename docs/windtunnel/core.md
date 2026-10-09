# Windtunnel core coverage

Windtunnel implements the deterministic ALU and these core operations:

- Conditional branches, relative jumps, register jumps, calls through the hardware stack, calls through PTRA/PTRB hub stacks, and CALLD/LOC.
- Byte, word, and long hub access, unaligned access, masked writes, scaled and augmented pointer expressions, and SETQ/SETQ2 cog/LUT block transfers.
- LUT access and sharing, including Q updates on RDLUT.
- All ALTx register and lane prefixes, ALTI field modification, instruction substitution, and cancellation of result writes.
- Pixel addition, multiplication, blending, and configurable mixing.
- Cog allocation, pair allocation, stop/restart, and retention of cog/LUT RAM on no-load restarts.
- Lock allocation, ownership, release, queries, and automatic release on owner stop/restart.
- GETCT, counter events, attention events, and selectable LUT/lock events, with polls, waits, branches, and SETQ wait timeouts.

SETQ/SETQ2 survive augmentation and ALTx prefixes. The block-pointer-delta and AUGS/ALTx errata described in the repository's [silicon documentation](../p2/original/Silicon_Documentation.docx) are modeled. Block transfers access underlying cog RAM at $1F8..$1FF rather than the special registers, wrap register addresses, and set read flags from the last value transferred.

General software FIFO, streamer (including its colorspace converter), CORDIC, interrupts, debugging, pins, skipping, and REP remain outside this coverage. Existing terminal support is a limited functional UART model. GETRND, BITRND, randomized WAITX, and general HUBSET configuration are also unfinished.

Execution uses a five-stage pipeline with two-clock issue, operand capture, delayed writeback, stalls, and branch refill. Hub accesses use rotating RAM grants, five-clock read responses, boundary-crossing transfers, and streaming blocks. Hub instruction fetch uses a buffered FIFO; cog/lock commands have separate round-robin slots. Cog startup streams the 504-long image before fetching instructions. The existing
blocking RDFAST 0/RFBYTE path shares the same clocked FIFO. Execution reports
`not_implemented` for missing simulator support and `illegal` with a reason for
prohibited use; both stop the runner/CLI. RDFAST/RFBYTE cannot use the instruction
FIFO during hub execution, and RFBYTE requires a preceding RDFAST.

Hardware probes validate local timing and prefetch, hub reads/writes and stacks
across banks/phases/alignments/cogs, block-beat visibility, FIFO entry and
contention, commands, loaded/no-load startup, pairs, and self-restart. Matrix
fixtures use named checklist runs with per-run cog selection and constants.
Acceptance passes 367 local runs and 365 native runs, with two intentional
oracle exclusions. Evidence is recorded in the pipeline plan; collision data, debug ROM,
and exact pin/serial timing are outside the deterministic claim.

The [instruction pipeline plan](pipeline.md) describes the required execution changes, live hardware measurements, and validation gates.

Run the local suite with:

```sh
zig-0.16.0 build install test-windtunnel
```

The core `.propan` fixtures cover instruction interactions and execution across cog, LUT, and hub memory. Zig tests additionally cover cog/lock lifecycle, LUT sharing, counter wrap and capture, memory wrap, and special-register block transfers. Local checks do not establish hardware equivalence; the hardware oracle requires a configured P2AAS endpoint.
