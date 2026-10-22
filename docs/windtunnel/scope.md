# Scope capture

Windtunnel can retain all 64 physical digital pad levels and all 64 pad voltages
in memory. Capture is independent of visibility: select a pin in the dense I/O
grid, then use Analog and Digital in its properties below the grid, alongside
the H/L/Z drive controls. These checkboxes select displayed traces. Digital means the physical
pad voltage thresholded to low/high, including retained voltage on floating pads;
it is independent of configured receiver routing and cog-visible IN.

The GUI starts recording automatically before simulation or ROM upload. Open
**Windows → Scope** to view it; selecting traces and filling capture storage never
opens the window automatically. Scope uses the shared inspector window wrapper,
including resize logging. **Freeze** retains the data and lets simulation continue
independently. After freezing, select the memory budget and overflow behavior and
click **New capture** to replace the recording. Closing Scope does not stop recording.
Settings apply to the next recording.

The default memory budget is 256 MiB; the minimum is 2 MiB. The budget covers
the entire block array, including its reserved metadata prefix,
rounded down to whole 4 KiB blocks. The small host handle is separate.
Rolling mode reclaims old blocks and advances a common retained start for all
lanes. This is block-granular, so the exact retained duration depends on activity.
Pause mode freezes before the first simultaneous substep that cannot fit, and the
GUI pauses after finishing the simulator step. Detach or start a new capture
before continuing. Constant signals keep their initial value in the channel
descriptor and consume no sample blocks.

Scope shares a physical time axis across lanes. The mouse wheel zooms around the
timestamp under the pointer; left or middle dragging pans continuously at the
current zoom. Navigation disables Follow latest and Fit until selected again.
Left/right clicks select cursors A/B; a left drag does not move cursor A. Hovering
a lane shows its held analog voltage and/or digital voltage and level, with a
clipping indicator where applicable, without placing a cursor. The waveform area
contains controls and measurement data without instructional text.
The hover cursor snaps to the nearest transition on either enabled trace when
it is strictly less than three pixels away. Clicks retain that snapped timestamp;
the tooltip shows signed `t-A` and `t-B` differences for existing cursors.
Capture caches the smallest positive electrical substep seen over its entire
lifetime, including across clock changes, rolling eviction, and resets. Zoom
always spans at least ten of these minimum substeps, padding a short recording
with empty space when necessary. If one such substep occupies more than five
pixels, faint vertical lines show a reference lattice at multiples of that
minimum interval from capture time zero. This is a potential sampling grid based
on the global minimum, rather than a history of changing clock periods.
Each pin has one shared voltage-scaled lane. Analog is drawn first in blue;
digital is drawn over it in green, mapping low/high to 0.0/3.3 V. Either trace
can be shown independently. Cursor values and their signed time difference are
displayed; both traces have separate color-matched readouts in a combined lane.
All displayed timestamps and cursor differences use the exporter's shared integer
formatter: exactly six decimal places with automatic ns/us/ms/s units, never fs.
Analog clipped intervals have red markers and cursor readouts explicitly say CLAMPED. Dense traces retain
per-pixel extrema and digital activity rather than displaying only every nth sample.

Capture observes every configured electrical substep, including clock-acquisition
quiet intervals without CPU edges. Its own timeline survives firmware reset and
GUI reinitialization; reset markers identify the boundary. Reset execution itself
is unchanged: this feature does not start the boot ROM after a firmware reset.
No host elapsed time is added while paused or reinitializing. The existing
fractional-femtosecond accumulator prevents clock-rate-dependent rounding drift;
recorded timestamps have 1 fs resolution. Ordered transitions that quantize to
the same timestamp are preserved.

## CLI

```sh
zig-out/bin/windtunnel --image program.bin --oversample 16 \
  --scope pins.wtscope \
  --scope-memory 256 --scope-overflow stop
```

`--scope` enables capture of all 128 pin streams. The CLI defaults to
`stop` at the memory limit, a successful completion, or can use `rolling`. It saves
the retained recording on normal completion, Ctrl-A/Ctrl-Q, capture-full, and
handled simulation errors. Export failures return an error. Process termination
by an external signal does not guarantee export. Scope is unavailable with
`--serve`. VCD conversion is provided by `windtunnel-export` after the native
capture has been saved.

The GUI saves native captures using Scope's Export path field. Native files
cannot yet be reopened in Scope. The standalone tool's VCD output includes separate
clipping indicators (0=none, 1=low, 2=high) and a reset counter. Timestamps are
rebased to the retained start, whose original femtosecond value is in a comment.

### Inspect and convert saved dumps

`windtunnel-export` reads the native block image without running the simulator:

```sh
zig-0.16.0 build build-windtunnel-export -Doptimize=ReleaseSafe
zig-out/bin/windtunnel-export pins.wtscope
zig-out/bin/windtunnel-export --vcd pins.vcd pins.wtscope
zig-out/bin/windtunnel-export --inspect --vcd pins.vcd pins.wtscope
zig-out/bin/windtunnel-export --vcd - pins.wtscope > pins.vcd
```

Inspection is the default when `--vcd` is omitted; `--inspect` also prints metadata
when converting. With `--vcd -`, VCD goes to stdout and any inspection goes to
stderr. Named output files are replaced atomically after successful conversion;
the input path cannot also be the output path. Invalid images are rejected before
any output is created. Errors return a nonzero exit code.

The report shows retained start/end and duration with exactly six decimal places.
Each time selects ns, us, ms or s automatically; ns is the smallest display unit.
Formatting uses integer rounding, including large timestamps and carries into the
next unit. The summary also lists total stored samples, clipped samples, and the
available channel kinds. The table has Channel, Kind, Samples, Clipped and Blocks
columns, widened automatically to fit the headings and all values. Channels
without blocks are omitted, including the reset stream when no reset occurred.

Samples count all live records in a channel's chain, including initial baselines
and pre-start records retained in the first block of a rolling capture. Clipped
counts stored analog samples with the explicit low/high clipping codes. Digital
rows leave the Clipped cell blank; reset-event streams show zero. Summary counts are the
sums of the table rows. Never-changing pin channels have zero stored samples,
remain visible in Scope when selected, and export their descriptor's initial
value as a constant in VCD. Implicit descriptor baselines, unused slots and stale
records in free blocks do not contribute to sample/clipping counts.

## Core storage and embedding

`windtunnel.Capture.init(allocator, budget_bytes, overflow, initial)` allocates the complete
pool before execution. `initial` is a `Capture.InitialState` with all 64 digital
levels and quantized analog voltages. For a Hub, obtain it using
`hub.io.captureState(hub.io.pads)` at capture creation. Keep the recorder at a stable address, call
`hub.attachCapture(&capture)`, and detach (`hub.capture = null`) before releasing
it. Frozen/full recordings remain readable. `Hub.step` records samples but never
allocates, stops for capture policy, or writes a file. Host callers inspect
`capture.metadataConst().status`; GUI and CLI stop policies are implemented
by their owners. `metadata()` provides mutable access to the header in the first block.
The host handle contains only an allocator and block slice; all capture state is
stored in the image, with no persisted process pointers.

`Capture.readNative(allocator, reader, byte_count)` reads an exact-size image into
4096-byte-aligned owned storage; release it with `deinit()`. The loader validates
the recorder's minimum pool size, header and enum representations, descriptor layout,
complete ownership of every block, link/count/tail/cache consistency, free-list accounting, sample values,
and timestamp order/bounds before returning typed access. It uses an additional
one-bit-per-block ownership bitmap while loading. `channelStatistics(Sample,
channel)` counts a validated channel's blocks, stored samples, and clipped analog samples.

Each channel has only `first_block`, `latest_block`, and `last_value`, packed with
zeroed reserved bits into 16 bytes. Access them through `digitalChannels()`,
`analogChannels()`, and `resetChannel()`, with read-only `Const` variants. Digital and
analog channels have separate typed block chains borrowing from the same pool.
Their descriptors start with both block IDs set to `.none` and `last_value` set
to the initial level. Unchanged observations allocate nothing. On the first
transition, one block receives two samples: the initial value at time zero and
the changed value at the current timestamp. These remain two ordered samples
even when that first transition also occurs at time zero. Later transitions use
the ordinary append path. Reset streams store actual events only.
For a pin channel without blocks, iteration synthesizes its time-zero baseline
from `last_value`; storage statistics count only physically stored records.
Samples are packed, naturally aligned 128-bit records:

| Kind | Low bits | High bits |
| --- | --- | --- |
| Digital | timestamp: u127 | low/high: enum(u1) |
| Analog | timestamp: u112 | quantized voltage: u16 |
| Reset | timestamp: u112 | reason: enum(u16), firmware=0, reinitialize=1 |

Every block, including descriptor blocks, has a packed u32 control word followed
by 12 explicit reserved bytes and 255 sixteen-byte payload records, totaling
4096 bytes. MSB=0 means the lower 31 bits are its count,
including 255. MSB=1 means they are the next BlockId and the block is implicitly
full. Partial blocks never have successors. A full tail retains count=255 until
its initialized successor is linked. Unchanged observations compare cached values
without accessing sample blocks.

The generic `Channel`, `Block`, and `Iterator` types and `recordDigital` /
`recordAnalog` operations are independent of the pad sampler. These operations
accept channels from the recorder's digital/analog arrays, allowing other sources
to reuse the representation and pool without a registration framework. Supplied
timestamps must be chronological within each channel. Observations of unchanged
independent sources also extend the recording end without accessing a sample block.
Iteration and export require that the recorder is not concurrently mutated. The GUI advances and draws on one
thread; the CLI finishes its simulation task before exporting.

Analog voltage uses `1 + round((V + 2) * 10000)` for -2 through +4.5 V inclusive.
Thus valid codes are 1 through 65001, with at most 0.05 mV quantization error.
Code 0 means below -2 V; 65535 means above +4.5 V. Codes 65002 through 65534 are
unused. Clipping loses the excursion's amplitude but preserves its time span.

Rolling eviction drops only a head with a successor and moves the common retained
start to at least that successor's first timestamp. Every pin channel therefore
keeps a value at or before the retained start, either in stored history or its
implicit descriptor baseline. A quiet channel's first transition after rolling
still materializes its initial value at zero. No predecessor cache or channel timing anchors are required.
Reset marker blocks share the budget. No block summaries or per-clock buffers
are stored. Iterators can skip complete blocks when seeking.

Block IDs have an explicit u32 representation: `0xffffffff` means `.none`, while
actual array indices fit in 31 bits. The descriptor occupies a reserved contiguous
prefix of the array. During bootstrap, every slot enters the same index-based free
list and the common allocator borrows the first descriptor blocks. These remain
reserved for the image lifetime. The header in the first block payload stores
the free-list head/count, retention mode, recording status, physical time and
fractional carry, and retained interval. Allocation/freeing updates that header
and the control words in free blocks. The current 2320-byte descriptor uses
145 ordinary 16-byte payload records in one block; its control word carries count=145. Larger descriptors use the same
full-block/successor convention. The prefix size is computed from the record
count. Memory usage counts occupied blocks, including this prefix. Capacity counts all blocks.

## Native format, version 1

A native file is the complete block array, byte for byte. Its size equals the
rounded memory budget: even a quiet 256 MiB recording produces a 256 MiB image.
No blocks are renumbered, and no offset table or separate serialization directory
is generated. Padding, unused samples, and free blocks are included. The entire
array is zeroed on initialization; freed/reused blocks may retain earlier samples
from the same recording. Links and counts determine which samples are live.
The standalone VCD converter writes only the logical retained traces.

The format is fixed little-endian. Capture initialization rejects big-endian
hosts. Persistent fields use explicit-width representations and asserted byte
offsets; runtime allocator/slice representations are not part of the file.
The format version remains 1. This block image replaces the initial compact
serializer; the loader accepts the block image format only. GUI import is not
available yet.

Every block is exactly 4096 bytes and its in-memory address is 4096-byte aligned,
matching `std.heap.page_size_min` on the current target. Descriptor, sample, and
free blocks use the same `Block(T)` layout and allocator. Block zero's control word
is at file offset 0 and its 256-byte header starts in the ordinary payload at file
offset 16. The following offsets are relative to that header:

| Offset | Field | Encoding |
| --- | --- | --- |
| 0 | Magic | 8 bytes: `WTSCOPE` followed by NUL |
| 8 | Version | u32 = 1 |
| 12 | Block size | u32 = 4096 |
| 16 | Total block count | u32 |
| 20 | Reserved descriptor block count | u32, currently 1 |
| 24 | Descriptor byte size | u32, currently 2320 |
| 28 | Channel count | u32 = 129 |
| 32 | Free-list head | BlockId: u32 |
| 36 | Free block count | u32 |
| 40 | Overflow mode | u8: rolling=0, stop=1 |
| 41 | Recording status | u8: recording=0, frozen=1, full=2 |
| 42 | Reserved | 6 zero bytes |
| 48 | Physical elapsed time | u128 whole femtoseconds |
| 64 | Fractional femtoseconds | u64 Q64 remainder |
| 72 | Time reserved | u64 = 0 |
| 80 | Retained start | u128 femtoseconds |
| 96 | Retained end | u128 femtoseconds |
| 112 | Minimum electrical substep | u128 Q64 femtoseconds; 0 until observed |
| 128 | Reserved | 128 zero bytes |

Digital channel descriptors begin at file byte 272, analog descriptors at 1296,
and the reset descriptor at 2320. Each is a packed u128 containing first block ID in
bits 0..31, latest ID in bits 32..63, cached value starting at bit 64, and zero
reserved bits above the value. Digital values use one bit; analog values and reset
kinds use sixteen. Pin channels without blocks use `.none` for both IDs and keep
their initial value in the cache. An empty reset stream uses `.none` for both IDs
and zero for its cached value.
The remainder of the reserved prefix is zero.

For every data/free block, `block_offset = BlockId * 4096`. Its control word is
at offset 0, reserved bytes at 4..15, and 255 packed samples at offset 16. The
existing count-or-next convention and sample bit layouts apply. Free blocks reuse
the control word for their successor; a clear MSB terminates the free list.
Descriptor blocks use the identical control word. Their payload is a sequence of
16-byte metadata records: the header occupies the first 16 records, followed by
129 channel records. In the current image the root is a partial tail with 145
records. Full descriptor blocks link to their successors just like full sample
blocks. The reserved descriptor prefix is allocated in ascending index order.

Images remain relocatable because all references are indices. The descriptor and
all mutable allocation state move with the bytes. Relocation tests copy an image
to another aligned allocation, compare typed iteration and the raw native image,
then append and reclaim blocks identically through the public loading API.
Tool-local tests verify VCD conversion after native reload. Loader tests also
reject malformed headers, invalid enum values, broken/shared/cyclic chains,
inconsistent free lists, bad counts, timestamps, cached values, and truncation.

## Validation and benchmark

Use `zig-0.16.0 build test-windtunnel-unit test-windtunnel-gui -Doptimize=ReleaseSafe`.
The existing pad benchmark accepts an optional `capture` argument for its Hub
kernel, using a 2 MiB rolling pool. Compare repeated paired runs, for example:

```sh
zig-out/bin/windtunnel-pads-benchmark ideal idle hub 200000
zig-out/bin/windtunnel-pads-benchmark ideal idle hub 200000 capture
zig-out/bin/windtunnel-pads-benchmark ideal many hub 200000
zig-out/bin/windtunnel-pads-benchmark ideal many hub 200000 capture
```

Allocation and initial pin sampling are outside the timed section. The busy case
includes rolling reclamation rather than silently freezing at the memory limit.
