# Hub RAM

`Hub.ram` stores `[16384][8][4]u8`: row, RAM slice, byte lane. These are the
512 KiB physical RAM, with explicit little-endian conversion of long values.
The mutable `bytes()` and const `bytesConst()` views expose the same storage in
address order for host loading and inspection; they do not issue hardware accesses.

Twenty address bits decode into lane `[1:0]`, slice `[4:2]`, row `[18:5]` and
upper-map bit `[19]`. Rows `$3E00..$3FFF` hold the last 16 KiB. Their upper alias
is `$FC000..$FFFFF`; other upper-map addresses are the memory hole. Protection
hides the lower alias and prevents application writes through the upper alias,
while debug-authorized writes remain accepted there. Authorization belongs to the
issuing instruction, independent of the cog's state at later writeback.

Clocked producers use `readHubBeat` and `writeHubBeat`. A beat touches one physical
long, checks the rotating slice owner, decodes protection once and selects byte
lanes. The eight `ram_ports` records reject multiple operations on the same
single-port slice, even at different rows. Consecutive unaligned block writes and
FIFO byte fragments may merge disjoint lanes of one write beat. Full sampled
longs and readability metadata travel through alignment latches and response queues.
Returning data and buffered consumption do not use the RAM port again.

An unaligned COGINIT image requires an initial aligned sample followed by 504
aligned samples to assemble its 504 logical image words. Hardware confirms one
additional startup clock for offsets 1..3; aligned and no-load timing is unchanged.

## Split reads

A logical read crosses a physical long when `(address & 3) + size > 4`. Bytes
always fit; words split at offset 3; longs split at offsets 1..3. A sequence of
aligned longs is not treated as an atomic multi-long transaction.

`memory_read_race` is a warning for observed interference, not for unaligned
access alone. Let `t0` and `t1` be the two sampling clocks. A committed write to
selected second-part bytes during `[t0, t1)`, or selected first-part bytes during
`(t0, t1]`, marks the read as raced. Canonical addresses match aliases; same-value
writes count. Rejected writes, masked-off lanes and unselected bytes do not count.

Detection uses the eight current RAM-port records, including accepted write lanes.
After all physical producers run, only unfinished split reads and completions from
that edge are reconciled. A split remains open across any number of FIFO-stolen
slots; no timeout, generation counter, or historical read ring is involved.
The detector descriptor is eight bytes. Source timestamps are retained separately
because they are part of the diagnostic payload, not reference lifetimes.

Startup and ordinary reads share one fragment latch and the same eight-position
five-clock return schedule as FIFO reads. Their byte selection is already known,
so each half retains only its earliest selected writer. Readability is captured
at each sample; later protection changes cannot rewrite that fact.

FIFO hub accesses always sample one aligned 32-bit long. Consumer operations may
combine bytes from adjacent sampled longs. Twenty owned metadata slots cover the
nineteen buffered/in-flight longs and one lower word held by a partial instruction.
Returns, FIFO order and partial fetches move slot IDs, never full payloads. A slot
cannot be reused until its owner releases it. Read and write FIFO storage are a union.

For a future crossing consumer of at most four bytes, only predecessor lanes 1..3
and successor lanes 0..2 can matter. Each slot retains at most six earliest writer
witnesses, populated only when interference occurs. Its source addresses reconstruct
writer byte addresses; the consumer's offset and length reconstruct selected lanes.
Separate RFBYTE operations remain individually atomic across a boundary. Reconfiguration
and discarded partial instructions release their slots explicitly.

Ordinary reads report on completion, startup on register loading, and FIFO and
streamer reads on consumption. Hub instruction fetch reports only if its assembled
instruction executes, once despite execution stalls. Discarded data and discarded
or condition-false instructions remain silent. The payload identifies both source
samples, their clocks and selected lanes, and an interfering writer. Returned data
is unchanged, and a warning does not prove the value lacks an atomic snapshot.

## Storage bounds

On the native 64-bit build, the generation-free model occupies 20,376 bytes per cog
and 1,022,208 bytes per hub, versus 110,944 and 2,289,664 before this refactor.
A FIFO word with all six diagnostic witnesses is 232 bytes; twenty slots reserve
4,640 bytes. Empty witness payloads are not initialized or inspected. The shared
return schedule is 1,016 bytes, including completed direct-read diagnostics.
Compile-time guards limit a cog to 32 KiB, the hub to 1.25 MiB, each FIFO slot to
256 bytes, and its detector descriptor to eight bytes.

The issue collection uses ordered headers and actual-sized payloads, rather than
reserving the largest issue union for every report. Its producer-derived payload
budget is 152,104 bytes; the complete collection occupies 222,608 bytes. Calls to
`at(index)` or a retained iterator’s `next()` reconstruct one full issue at a time. Stepping
allocates nothing and never truncates payloads or participants.

## Validation and throughput

ReleaseSafe validation preserved all 269 fixtures / 5,543 runs without changing
existing vectors. The unit/debugger suites cover delayed grants, clock rollover,
all crossing consumer widths, unreadable halves, slot release/reuse, and full
issue-payload round trips. Debug child-process probes reject released/reused owners,
capacity overflow, orphaned/duplicate owners, occupied return slots, and illegal
RAM grants; valid controls pass. CLI PTY checks cover inline warnings, trap linefeeds,
TAQOZ arithmetic, and exact terminal-state restoration. The GUI is compiled only.

The table uses medians of seven paired native ReleaseSafe runs on logical CPU 15.
Each run excludes initialization and 5,000 warmup clocks, then measures 200,000
clocks with eight active cogs. Baseline/refactored order alternates; every program
checksum matches. These are complete `Hub.step()` measurements, including I/O.

| Workload | Before ns/clock | After ns/clock | Throughput ratio |
| --- | ---: | ---: | ---: |
| Local instruction loop | 8,376 | 5,757 | 1.46× |
| Unaligned hub reads | 8,203 | 5,472 | 1.50× |
| FIFO consumption | 9,152 | 7,596 | 1.20× |
| FIFO/data contention | 8,873 | 7,402 | 1.20× |
| Hub execution | 8,797 | 6,565 | 1.34× |
| Repeated image startup | 8,186 | 5,421 | 1.51× |

The same workloads are available in the existing benchmark executable:

```sh
zig-0.16.0 build build-windtunnel-pads-benchmark -Doptimize=ReleaseSafe -Dcoverage=false -Dno-gui=true
zig-out/bin/windtunnel-pads-benchmark ideal mem_read hub 200000
```

Other workload names are `mem_local`, `mem_fifo`, `mem_contention`, `mem_hub_exec`,
and `mem_startup`. All use the `hub` kernel. Raw paired measurements, checksums,
frozen binaries/source and the validation manifest are retained in
`.tmp/hub-minimal/`; `paired-raw.json` contains every sample and `validation.json`
records the compiler, layout measurements and artifact hashes.
