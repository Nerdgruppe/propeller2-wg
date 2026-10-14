# Windtunnel I/O

`sim/IO.zig` owns 64 `Pin` instances, each containing a typed WRPIN
configuration, a `SmartPin` from `sim/smart_pin.zig`, and its published outputs.
`SmartPin` owns one shared `Registers` struct and a `Logic` union tagged by
`libp2.types.SmartPinMode`. Repository, digital smart pins, UART TX/RX and USART TX/RX payloads
contain only their private engine state. `configure()` retains the payload for
same-mode digital and serial WRPIN writes and replaces it on mode changes, retaining shared
registers and host-selected receive/transmit modes. `step(Inputs)` updates X/Y, enabled and
acknowledgement state, dispatches the active payload with the shared registers,
and latches the returned result and IN signal. Inputs carry DIR/OUT,
SmartA/SmartB, optional X/Y writes and acknowledgement; outputs carry SmartOut,
IN, the result bus and scheduler signals. IO caches these outputs for pad
resolution and register sampling.

Each simulation frame commits cog writebacks and aggregates OUT/DIR, combines
commands due on that edge, and prepares SmartA/SmartB from delayed pad samples.
It calls every pin's `step()` exactly once with fixed inputs, publishes all
returned outputs together, resolves pads, and updates the input/result pipelines.
The cogs then execute and queue commands for future frames, followed by VCD
capture and counter advancement. Pad feedback may require multiple settling
iterations, but these only read the published outputs and never advance smart
logic. Commands queued outside `Hub.step()` use the same delivery delay; external
driver changes are staged until the next frame.
Unimplemented modes use `DummyMode`, whose step fails with
`UnsupportedSmartPinMode`; IO records a simulation fault with the issuing pin,
cog, PC and instruction, including when DIR is low.

The surrounding `IO.Pin` owns typed `libp2.types.PinConfiguration` WRPIN bit
fields and decodes OUT/OTHER/SMART selection, DIR/TT output enable and data-source
clocked operation. IO owns input/output routing and pad drive
resolution. These configuration decisions are independent of smart-engine outputs.
The normal engine routes SmartA to IN and OUT to SmartOut through the same step
interface as the other engines. `PinIndex` identifies a smart engine/register bit;
`PadIndex` identifies a physical pad. Cog registers remain individual; the hub
aggregates them once per edge after all cog writebacks. Directions combine
with OR, and each cog's output contributes only where that cog enables its
direction. Register writes and cog resets take effect at that shared boundary.

Implemented instructions include every DIR, OUT, FLT and DRV variant, TESTP and
TESTPN with write/AND/OR/XOR flags, and WRPIN, WXPIN, WYPIN, RDPIN, RQPIN and
AKPIN. Mutation flags report the original base bit. Fields wrap inside their
32-pin port; SETQ overrides the field count. Random variants use one hub-owned xoroshiro128** generator advancing every
system clock. Its cog projections preserve measured shared taps and relative
inversions; their numbered output-bit assignment remains approximate.
GETRND (including flags-only), BITRND and randomized WAITX use that same cog
word. GETRND flags select bits 31 and 30; bit/pin mutation flags report the
original base bit. WAITX samples once on entry and clears only requested flags
when its deadline is reached. HUBSET partially seeds the shared state using an
explicitly approximate lane/pipeline model. The retained probes and open mapping
questions are described in [the RNG research notes](../../data/random/README.md).

## Smart modes and endpoints

All twenty digital smart modes (4–23) are implemented: finite pulses and transitions,
frequency/duty NCO, triangle/sawtooth/SMPS PWM, quadrature and gated/up/down
counting, rise/high counting, state/high timing, event timing and missing-event
alarms, fixed-period timing/high measurements, and minimum-window timing/high/
period measurements. They advance once per system clock; `--fast-io` does not
change their behavior. SMPS uses digital A/B voltage/current gates. X/Y writes
acknowledge IN without resetting a running engine. Same-mode WRPIN preserves
private state. Each engine defines its reset, result retention, saturation and
carry behavior separately; state timing returns the previous input level in C.
[Hardware probes, waveforms and limitations](../../data/timing-data/digital-smart-pins/README.md)
provide the validation record.

UART and USART use separate host-selected modes on each `SmartPin`:

| Direction | Mode | Behavior |
| --- | --- | --- |
| RX | `.exact` | Decode routed SmartA/SmartB samples and transitions |
| RX | `.{ .emulated = &source }` | Receive endpoint words with bit timing; USART still requires routed B edges |
| RX | `.{ .virtual = &source }` | Receive at most one word per simulation clock, waiting for acknowledgement |
| TX | `.{ .emulated = &sink }` | Generate serial SmartOut and deliver each completed word to the sink |
| TX | `.{ .virtual = &sink }` | Deliver at most one word per clock without serial transitions |

RX defaults to exact operation. TX defaults to emulated operation with a discard sink.
Attach caller-owned endpoint descriptors, independently of WRPIN:

```zig
var source: IO.DataSource = .{ .reader = reader };
const sink: IO.DataSink = .{ .writer = writer };
hub.io.pins[40].smart.tx_mode = .{ .emulated = &sink };
hub.io.pins[41].smart.rx_mode = .exact;
hub.io.pins[42].smart.rx_mode = .{ .emulated = &source };
hub.io.pins[43].smart.rx_mode = .{ .virtual = &source };
```

Writer adapters truncate completed words to a byte; Reader adapters promote a byte
to a word. Callback endpoints preserve all 32 bits. A source returns `null` when no
word is available. Readers consume only buffered bytes; the host owns refill and
EOF handling outside simulation steps. Keep descriptors and their backing objects
alive while attached. Endpoint errors stop execution with pin and instruction provenance.
No external process or library is needed to use the endpoint API.

Timed receivers share their bit state between exact and emulated operation. Switching
between those modes preserves the partial shift register, receive position and baud
phase. On an exact-to-emulated handoff, the next source value is the **whole current
word**, including bits already received. It stays unmasked until completion, so a
later WXPIN can extend the word. A staged source word survives endpoint replacement
and an intervening exact interval; the next completed word releases it. An empty
emulated source supplies no new word; a partially received emulated word waits for
its source data rather than inventing samples. USART never synthesizes a B clock.

UART validates the start bit, then samples LSB first with a live integer/fractional
baud divider. USART samples the previous or coincident A value on positive B edges
according to X[5]; WRPIN inversion also supports the opposite clock edge. Receive
width changes take effect during the word. Divider changes affect subsequent UART
reloads while retaining the current interval and fractional remainder. Shortening
below the current count can stall reception until reset. Timed reception overwrites
unread results; virtual reception waits for ACK. RX results are MSB justified and
unused low bits can contain earlier samples, including across DIR reset.

UART TX has a shifter and one pending word, start/data/stop signals, an IN indication
when the pending word advances, and busy in C through the stop bit. USART TX uses
routed B edges and supports continuous or start/stop buffering and reset-time priming.
Its output crosses the registered smart-output path. Serial TX shifters feed routed
A samples into their high bits, which is visible through RQPIN; UART exposes its
running shifter and USART latches a result at word boundaries. Both emulated TX
engines deliver completed, width-masked payloads to their current sink. Aborted words
produce no sink delivery. Virtual TX generates the protocol reset level (UART high,
USART low); the ordinary SmartOut propagation, WRPIN output selection and electrical
resolution still apply. Selecting virtual mode does not flush queued output transitions.

Apply host mode changes between simulation clocks. Entering virtual mode aborts
partial transfers and pending TX data. Returning from virtual starts fresh timed
state. Host mode changes preserve completed RX Z/IN; subsequent timed reception retains its normal overwrite behavior. WRPIN
preserves host selections, and same-mode serial WRPIN preserves active logic.
DIR reset aborts private transfers and clears IN while retaining Z; receivers also
retain their sampled shift register. COGSTOP clears that cog's DIR contributions;
other enabled cogs can keep a pin running. Runners do not drain pending output after
shutdown or a fixture checkpoint, so programs must wait for required TX completion.

CLI/server/test runners explicitly attach terminal output to P62. The server attaches
its receive Reader to P63 and refills it from the WebSocket queue outside simulation
steps. The test harness attaches input after its readiness marker. The P2AAS baudrate
query does not override smart-pin X timing.

`windtunnel --fast-io`, including server mode, selects virtual terminal transfers on
P62/P63 only. It preserves cog execution, routing, command delays and explicit waits.
Other pins keep their independently selected modes. Embedded users select the tagged
unions directly; there is no global `IO.fast_io`. Fixtures can retain timed terminal
behavior with `fast-io: off "reason"`. [Serial hardware validation](../../data/timing-data/serial-smart-pins/README.md)
records the measured limits and reproducible checks.

## Digital pads and timing

`io.setExternal(pad_index, signal)` supplies an external drive (`zero`, `one`, `x` or
`z`). Pads resolve internal and external drives, including floating drive modes
and contention. A/B selectors route local/relative pads or the gated OUT bit;
inversion, digital A/B logic, feedback and optional synchronization are modelled.
Digital and Schmitt modes use digital levels. The 2/3/5/8-sample unanimous input
filters and their selectable clock taps feed both GPIO and smart engines. Analog
behavior, DAC/scope instructions, ADC and USB smart modes remain unsupported.
Boolean IN samples `x` and `z` as zero.

Commands distinguish configuration, X/Y writes and acknowledgement. Commands
from all cogs commit together after two clocks. Same-edge commands OR
their data and command selectors; collisions log pin, clock, cog, PC and values.
The shared return pipeline makes repository X-to-Z and acknowledgement observable
after four clocks, and the repository's IN indication after five. Repository Z
survives DIR reset; X writes while held reset update X without changing Z.

GPIO loopback timing depends on frequency. The model places drive changes four
clocks after execution below 170 MHz, or five at higher frequencies. The first
changed TESTP result is at instruction spacing eight/nine clocks respectively,
and an INA/INB operand at nine/ten. The original sweep was stable through 167 MHz and from
170 MHz. A later sweep with a different PLL divider and pad load crossed earlier
at 166/167 MHz; 168/169 MHz also produced mixed samples. The frequency-only model
retains 170 MHz as its deterministic boundary. It does not model electrical jitter
or PLL-divider/load-dependent transitions within that intermediate band. The
loopback measurements do not distinguish output propagation from input propagation.
These timings, command collisions and per-cog output gating were measured on hardware. The 82 probes and
eight phase captures are in `data/timing-data/smart-pin-{cases,results}.yaml`;
the matching simulator/hardware checklist is `state/smart-pin-hardware.propan`.
The [frequency sweep](../../data/timing-data/serial-boot/gpio-frequency.json)
retains both boundary samples and probe sources. The current 82-probe captures
use RCFAST; the earlier five-clock drive model was derived at 200 MHz.
The selected SmartOut/OTHER signal crosses a common registered output pipeline
in every mode. GPIO's SmartOut forwards the registered cog OUT bus; output-generating
engines publish through their output register. Both registrations live in `SmartPin`,
remain clocked across WRPIN changes, and feed the same I/O forwarding path. The model
assigns two clocks to cog OUT transport, one to output-engine publication, and a
common one/two-clock pad path below/at-or-above 170 MHz. This decomposition reproduces
the measured instruction-visible boundaries; it does not identify exact physical
register placement. DIR reaches the output gate separately.
WRPIN changes selection without resetting or filling the pipeline, and virtual TX
uses the same path. Immediate GPIO-to-UART and UART-to-GPIO selection boundaries
were measured at RCFAST and 200 MHz; the retained `output-selection.propan` probe
contains 36 cases and 144 silicon observations in `data/timing-data/serial-smart-pins`.
Clocked output selection adds a further pipeline, which advances once per frame while selected.
Reconfiguration while DIR is low and settled is covered by regression. Active
WRPIN switching of the clocked output mux can expose physical retained-latch
glitches; their exact transient phase is not modeled. Selectable pin events sample
one stage earlier than TESTP/INA, including smart completion events and interrupts.
Smart result reads use the engine's measured return-bus stage, separate from IN.
NCOs separate the base divider, registered accumulator and physical adder output.
The accumulator consumes the preceding divider boundary; publication captures
that registered value at the current boundary when the adder carries. Separate
DIR samples gate reset and publication. These stages share the ordinary digital
result return delay; no divider-specific readback rule is used.
Serial mode timing is functional and has no physical waveform equivalence claim.

## VCD

```sh
zig-out/bin/windtunnel --image program.bin --vcd pins.vcd
```

The CLI supports VCD for image runs. For embedded use, create `sim/Vcd.zig` with
a `*std.Io.Writer`, call `hub.attachVcd(&vcd)` before stepping, and `vcd.finish()`
before closing the Writer. Keep the exporter alive while attached. Export
failures stop the simulator.

Traces contain CT, each cog's OUTA/B, DIRA/B and INA/B, combined I/O registers,
and physical P0–P63 with `0`, `1`, `x` and `z`. Combined OUT is the raw OR of cog
OUT registers; the pads show the direction-gated drive. Initial values and
subsequent changes are emitted after each settled edge. Timestamps remain
monotonic across CT rollover. One synthetic VCD nanosecond means
one simulated system clock, independent of HUBSET frequency or wall time.
Tracing does not change execution timing.
