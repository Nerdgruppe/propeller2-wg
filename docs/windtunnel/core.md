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

SETQ/SETQ2 survive augmentation and ALTx prefixes. The block-pointer-delta and AUGS/ALTx errata described in the repository's [silicon documentation](../p2/Silicon_Documentation.docx) are modeled. Block transfers access underlying cog RAM at $1F8..$1FF rather than the special registers, wrap register addresses, and set read flags from the last value transferred.

FIFO, streamer (including its colorspace converter), CORDIC, interrupts, debugging, pins, skipping, and REP remain outside this coverage. Existing terminal support is a limited functional UART model. GETRND, BITRND, randomized WAITX, and general HUBSET configuration are also unfinished.

Execution remains functional rather than cycle-exact. Ordinary instructions use the existing scheduler; block transfers advance one long per step. Initial hub access latency, hub arbitration, startup timing, and exact branch/pixel timing remain unmodeled. GETCT and counter-event values therefore reflect simulator clocks, not exact hardware instruction timing.

Run the local suite with:

```sh
zig-0.16.0 build install test-windtunnel
```

The core `.propan` fixtures cover instruction interactions and execution across cog, LUT, and hub memory. Zig tests additionally cover cog/lock lifecycle, LUT sharing, counter wrap and capture, memory wrap, and special-register block transfers. Local checks do not establish hardware equivalence; the hardware oracle requires a configured P2AAS endpoint.
