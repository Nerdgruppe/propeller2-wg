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

SETQ/SETQ2 block-transfer prefixes survive executed augmentation and ALTx prefixes. The block-pointer-delta and AUGS/ALTx errata described in the repository's [silicon documentation](../p2/original/Silicon_Documentation.docx) are modeled. Block transfers access underlying cog RAM at $1F8..$1FF rather than the special registers, wrap register addresses, and set read flags from the last value transferred.

Conditional cancellation consumes SETQ/SETQ2 prefix state, including a canceled
augmentation. SETQ2 selects LUT words for WRLONG even when D is immediate;
the encoded D field selects the first LUT address. SETQ retains immediate filling.

COGINIT, QDIV, QFRAC and QROTATE clear Q during decode, including when their
condition is false or a taken branch discards them. An executed SETQ, SETQ2 or
CRCNIB in the preceding slot can win that clear. SETQ protection survives
executed augmentation; SETQ2 followed by augmentation does not protect Q.
COGINIT passes the resulting Q to PTRA. These measured side effects do not add
CORDIC execution support. Code that needs Q preserved should initialize the
word fetched past a local return jump, for example with a NOP guard.

The [I/O subsystem](io.md) implements pin instructions, digital pads and routing,
long repository, UART and functional USART modes, plus VCD export. Software FIFO,
CORDIC, interrupts, debugging, skipping, REP and shared RNG behavior are documented
in the [pipeline notes](pipeline.md) and [hardware findings](../accumulated.md).
The [digital streamer](streamer.md) supports timed pin output/capture, FIFO/LUT
transfers, pixel unpacking, TMDS output, events and inspectable DAC bytes.
The [digital color converter](converter.md) transforms those bytes and supports
component, S-Video and composite modes. DDS/Goertzel consumes digital ADC bitstreams;
the electrical frontend supports RC pads, comparators, feedback, DACs, ADC smart modes,
and cog scope/streamer capture.

`Hub.step()` returns `normal`, `breakpoint`, `reset`, or `trap`. Breakpoints stop
before execution or after a complete edge for data watchpoints; traps retain the
fault diagnostic. HUBSET with top nibble `$1` stops the simulation and returns
`reset` after restoring the entire Hub through `init()`, including RAM, ROM, cogs,
pins, counter and clock configuration. It detaches host tracing, terminal and
debugger links. Hosts may explicitly attach them again; boot ROM execution is not
restarted automatically.

The [clock model](clocks.md) supplies shared physical time for CPU edges, pads,
VCD and server pacing. `--sim ideal` uses exact typical values; `--sim realistic`
uses bounded oscillator variation and the characterized PLL responses.
`--rcfast`, `--rcslow`, `--crystal` and `--seed` override these defaults.

The CLI behaves as a raw serial terminal on P63/P62. Startup options are independent:

| Options | Startup |
| --- | --- |
| No image, with or without `--boot` | Regular serial ROM session |
| `--image program.bin` | Load hub RAM and launch the application directly |
| `--boot --image program.bin` | Download and launch through the real ROM serial loader |

For example:

```sh
zig-out/bin/propan -o /tmp/hello.bin examples/hello-world-no-pll.propan
zig-out/bin/windtunnel --boot --baudrate 115200 --image /tmp/hello.bin
zig-out/bin/windtunnel --boot
```

In a ROM session, press **Esc** to enter TAQOZ or **Ctrl-D** to enter the ROM
monitor. Startup sends `> ` to calibrate autobaud at the selected terminal rate.
Input is uncooked: no local echo, line editing, signal handling, CR/LF conversion
or software flow control. Control characters reach the P2 directly, except for
the terminal menu prefix:

| Sequence | Action |
| --- | --- |
| Ctrl-A, Ctrl-A | Send byte `$01` |
| Ctrl-A, Ctrl-H | Show the available commands |
| Ctrl-A, Ctrl-Q | Quit and restore the original TTY settings |

The quit command also interrupts an upload in progress. Redirected input is
supported; EOF disconnects input while queued bytes and the application continue.
Serial bytes go to stdout; the menu and diagnostics go to stderr. The optional
POSIX integration check is `python tests/windtunnel/terminal.py` after building.

ROM startup loads cog zero from `$FC000`, with a 10 kOhm pullup on P59 selecting
serial boot. The external host drives 8N1 waveforms onto physical P63, including
fractional bit periods. The ROM performs its own ADC-based RNG seeding, cog/LUT
setup, autobaud measurements through P0/P1, interrupt-driven reception,
`Prop_Chk` identification, Base64 decoding, checksum validation and application
launch. ROM sessions and interactive input retain UART timing. `--fast-io`
selects virtual application TX after image loading; server RX/TX remain virtual
when requested. `--vcd` also works without an image and includes the boot process.

Uploads accept whole-long, nonempty images up to 512 KiB minus the four-byte
checksum, at 9600..2000000 baud (default 115200). The P59 source is restored after
the upload; the serial host leaves P63 idle high. Applications inherit the baud
dividers calibrated by the ROM; direct loads initialize nominal dividers at the
selected terminal baud rate. The loader consumes its version/checksum replies.
The P2AAS server retains its existing direct-load path.

Embedded callers can use `serial_boot.start()` to enter serial boot,
`serial_boot.Host.write()` to send raw UART commands, or `serial_boot.load(io, ...)`
for the complete checksummed download. Call `Hub.init()` first; these operations
do not model a RESN pulse or launch automatically on HUBSET reset. Flash and SD
boot remain unsupported. The ADC model supplies deterministic calibration bits,
so executing the ROM's seed loop does not add physical entropy.

Loaded COGINIT images retain their source byte alignment. This matters for the
ROM's TAQOZ image at `$FED42`; rounding its source down would load different
instructions. The unaligned-load fixture verifies all four alignments against
hardware, alongside the existing startup/restart timing fixtures.

WFBYTE, WFWORD and WFLONG write the low 1/2/4 operand bytes into the shared FIFO
in little-endian order. GETPTR advances on enqueue; hub memory changes only when
the two-clock write-eligibility stage and rotating RAM-slice grant permit it.
Mixed widths preserve neighboring lanes and split unaligned writes across slices.
The existing `fifo-*` fixtures check these paths against hardware at RCFAST.

Execution uses a five-stage pipeline with two-clock issue, operand capture, delayed writeback, stalls, and branch refill. Hub accesses use rotating RAM grants, five-clock read responses, boundary-crossing transfers, and streaming blocks. Hub instruction fetch uses a buffered FIFO; cog/lock commands have separate round-robin slots. Cog startup streams the 504-long image before fetching instructions. The existing
RDFAST/WRFAST/FBLOCK and RF/WF instructions share the same clocked FIFO. Execution reports
`not_implemented` for missing simulator support and `illegal` with a reason for
prohibited use; both stop the runner/CLI. RDFAST/RFBYTE cannot use the instruction
FIFO during hub execution, and RFBYTE requires a preceding RDFAST.

Hardware probes validate local timing and prefetch, hub reads/writes and stacks
across banks/phases/alignments/cogs, block-beat visibility, FIFO entry and
contention, commands, loaded/no-load startup, pairs, and self-restart. Matrix
fixtures use named checklist runs with per-run cog selection and constants.
The original core acceptance covers 367 local runs and 365 native runs, with two
intentional oracle exclusions. Evidence is recorded in the pipeline plan.
Pin acceptance adds hardware repository/collision/GPIO measurements and pin
instruction fixtures, described in the I/O documentation. Debug ROM and physical
serial waveforms remain outside the deterministic claim.

The [instruction pipeline plan](pipeline.md) describes the required execution changes, live hardware measurements, and validation gates.

Run the local suite with:

```sh
zig-0.16.0 build install test-windtunnel
```

The core `.propan` fixtures cover instruction interactions and execution across cog, LUT, and hub memory. Zig tests additionally cover cog/lock lifecycle, LUT sharing, counter wrap and capture, memory wrap, and special-register block transfers. Local checks do not establish hardware equivalence; the hardware oracle requires a configured P2AAS endpoint.
