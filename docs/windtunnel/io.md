# Windtunnel I/O

`sim/IO.zig` owns 64 `Pin` instances, each containing a typed WRPIN
configuration, a `SmartPin` from `sim/smart_pin.zig`, and its published outputs.
`SmartPin` owns one shared `Registers` struct and a `Logic` union tagged by
`libp2.types.SmartPinMode`. Repository, digital smart pins, UART TX/RX and USART TX/RX payloads
contain only their private engine state. `configure()` retains the payload for
same-mode digital, ADC, scope, DAC and serial WRPIN writes and replaces it on mode changes, retaining shared
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
The cogs then execute and queue commands for future frames, followed by counter
advancement and VCD capture of the completed clock. Physical receiver feedback updates within analog substeps; clocked feedback samples
once per system clock. Each RC pad advances through the configured analog substeps per frame. Commands queued outside `Hub.step()` use the same delivery delay; external
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
Measured engine behavior and limitations are recorded in
[Accumulated Propeller 2 Knowledge](../accumulated.md#digital-smart-pins-measured-engine-and-return-bus-behavior).

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

Apply host mode changes between simulation clocks. Each virtual step resets the
serial engine's timed receive/transmit state, abandoning partial words and pending
timed TX data. Virtual TX delivers an enabled WYPIN to its sink on that clock, with
no retained shifter; writes while DIR is low are ignored. Synchronous virtual engines
keep their A/B samples current so a held-high B cannot invent an edge on return.
Returning to timed operation starts from idle without a host transition handler.
Virtual reception preserves unread RX Z/IN until acknowledgement; subsequent timed
reception retains its normal overwrite behavior. WRPIN
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
behavior with `fast-io: off "reason"`.
[Serial hardware findings](../accumulated.md#serial-smart-pins-live-configuration-and-retained-state-2026-10-14)
record the measured limits; regression fixtures live in `tests/windtunnel/`.

## Initialization-time system configuration

The CLI selects a profile for image runs or all fresh server sessions:

```sh
zig-out/bin/windtunnel --sim ideal --image program.bin
zig-out/bin/windtunnel --sim realistic --image program.bin
zig-out/bin/windtunnel --sim realistic --oversample 8 --serve
```

`--sim` accepts only `ideal` or `realistic` and defaults to `ideal`.
`--oversample N` selects a positive integer number of analog samples per system
clock and defaults to 1. It applies to image runs and every fresh server session,
with or without VCD recording.

Every Hub is initialized with an explicit host-side configuration:

```zig
try hub.init(.ideal);     // Default used by the CLI, server and normal fixtures.
try hub.init(.realistic); // Datasheet-based fast outputs and approximate leakage.

var configuration: windtunnel.SystemConfiguration = .realistic;
configuration.vio_voltage = 3.2;
configuration.pad_capacitance = 8e-12;
configuration.oversample = 8;
try hub.init(configuration);
```

`SystemConfiguration.zig` defines both presets and validates all parameters before
replacing Hub state. Nonfinite quantities, nonpositive capacitance/resistances/current
limits, threshold ratios outside `[0, 1]`, unordered/nonpositive curve points and
inconsistent rating bounds return `InvalidConfiguration`. A zero oversampling factor
is also invalid. Inactive model parameters are validated too. Failed initialization
leaves an existing Hub unchanged.

The Hub owns a copy in `system_configuration`; treat it as read-only. There is no
runtime setter: changes require initialization, which clears runtime RAM, cogs,
external sources and electrical snapshots. Firmware-triggered resets reuse the
active system configuration. The configuration is separate from firmware HUBSET;
firmware cannot select a different electrical model.

The desktop Options → System Configuration window shows active settings separately
from an editable draft.
Ideal/Realistic buttons populate the draft; edited values display as Custom. Editing
does not affect simulation. While paused, **Reinitialize with settings** validates
the draft, restores the original loaded image, starts cog 0 and leaves execution
paused. Without an image, all cogs remain stopped. Runtime RAM edits and external
stimuli are discarded, while debugger breakpoint/watchpoint definitions are retained.
All effective electrical parameters are editable in engineering units; no rebuild or
configuration file is needed. **Analog samples per clock** edits the oversampling
draft and displays the active factor separately. Applying it uses the same
reinitialization workflow; firmware resets retain the selected factor.

## RC pads and digital sampling

`sim/Pads.zig` models the 64 physical pads as a SIMD bank, with two current-limited voltage sources per pad
(the internal P2 driver and at most one external source) feeding a capacitor to
ground. Pads have no external electrical connections to one another. IO retains
its existing digital output transport, input routing and sampling pipelines.

```zig
const pad: windtunnel.IO.PadIndex = @enumFromInt(10);
try hub.io.setExternal(pad, .low);
try hub.io.setExternal(pad, .{
    .voltage = 3.3,          // volts
    .resistance = 1_500,     // ohms, strictly positive
    .max_current = 1e-3,     // amperes, unlimited by default
    .drive_enabled = true,   // true by default
    .capacitance = 100e-12,  // farads, zero by default
});
const result = hub.step(); // Handle normal/breakpoint/reset/trap in the runner.
if (hub.io.getExternal(pad)) |state| {
    // state.voltage, state.current and state.internal_current are concrete f64 values.
}
const digital = hub.io.pads[10]; // Signal.low or Signal.high
try hub.io.setExternal(pad, .float); // Release drive; configured leakage may discharge the pad.
```

`setExternal()` validates before replacing the descriptor, and changes take effect
on the next completed hub clock. Nonfinite voltage, nonpositive/nonfinite resistance,
negative/NaN current limits and negative/nonfinite capacitance return `InvalidSource`.
Positive infinity is accepted for an unlimited current. Validation also applies to
disabled sources. `.high` and `.low` use 3.3 V and 0 V respectively, with 1-ohm
resistance, unlimited current and no added capacitance. `.float` disables drive.
Source replacement preserves voltage; changing capacitance does not simulate
charge redistribution. External capacitance remains connected while drive is disabled.

`getExternal()` returns the most recent completed snapshot. Only the snapshot itself
is optional: it is null before the first completed clock following initialization
or reset. Every pad then has a defined voltage and currents, even with no active cogs
or drivers. `current` is positive from the external source into the pad;
`internal_current` is positive from the P2 driver into the pad. Both are instantaneous
currents at the end of the clock. Their sum, minus configured leakage, charges the
capacitor, so they need not cancel during a transient. Reads and staging a source do not advance the model.
Full hub reset clears external descriptors and snapshots and initializes voltage to zero.

The ideal preset retains the simplified RC behavior: 3.3 V VIO, a 0.5 threshold
ratio, 5 pF internal capacitance, 30-ohm fast high/low drive and zero leakage.
Both presets retain 1.5/15/150-kilohm resistor modes and 1 mA/100 microamp/10 microamp
current modes implemented as current-limited 30-ohm rail sources. Their values,
VIO, threshold ratio, capacitance and rating limits are all initialization parameters.
Floating modes and physical output disable disconnect the output driver.

The realistic preset replaces fast drive with piecewise output curves from the
[DC characteristics](../p2/datasheet.md#dc-characteristics):

| Current | Low voltage above ground | High voltage drop below VIO |
| --- | --- | --- |
| 0 mA | 0 mV | 0 mV |
| 1 mA | 15 mV | 6 mV |
| 10 mA | 160 mV | 170 mV |
| 30 mA | 510 mV | 580 mV |

The model interpolates between these points, extends the final segment above 30 mA,
and mirrors each curve for reverse current. Those extensions are assumptions.
Changing VIO shifts the high rail without rescaling the drops; the reference points
apply to nominal 3.3 V I/O conditions. The extracted datasheet labels these rows
`VDD=3.3V`, inconsistent with its separate 1.7–1.9 V core VDD range; this model
interprets the rows as 3.3 V I/O measurements. The table's extracted output rows
also lack their original min/typ/max column layout, so they are used as reference
points rather than a complete transistor characterization.

Realistic mode enables a 33-megohm leakage path to ground, approximating 0.1 microamp
at 3.3 V. The datasheet specifies ±0.1 microamp typical and ±10 microamp maximum
at ground/VIO, without a full leakage curve or direction. A ground-directed resistor
is an explicit approximation, not a measurement. With 5 pF and no external load,
its RC time constant is 165 microseconds. Ideal mode disables leakage and retains
floating charge indefinitely. Configured leakage remains active with output disabled.

For each analog substep, the module solves the piecewise-linear backward-Euler equation
`Ctotal * (Vnew - Vold) / dt = Iinternal(Vnew) + Iexternal(Vnew) - Ileakage(Vnew)`,
where `dt = 1 / clock_frequency / oversample`. Linear branches follow
`I = clamp((Vs - Vpad) / R, -Imax, +Imax)`; piecewise fast branches use the configured
curves. Transients continue with unchanged digital outputs; settled pads skip redundant
solves. This is stable for short RC constants, but coarse clocks distort the continuous
settling curve. No parameters are hardware-calibrated in this implementation.

Digital reads are low below `VIO * threshold_ratio` and high at or above it.
The default ratio is the datasheet's typical 0.5; its 0.3–0.7 range describes threshold
variation and does not become an unknown voltage band. Contention produces a voltage,
not an unknown digital value. `Signal` contains only `low` and `high`; float belongs
to the source's drive enable. Pads advance together from simultaneous voltage snapshots;
asynchronous feedback updates between substeps without zero-delay iterative settling. Relative-pin routing is
logical routing, not an electrical connection between pads.

### Analog oversampling

```sh
zig-out/bin/windtunnel --image program.bin --sim realistic --oversample 8 --vcd pads.vcd
```

Each completed hub clock runs N electrical substeps. Cog/streamer/smart-DAC codes and
registered outputs remain fixed for that clock; asynchronous receiver feedback can
change its selected driver between substeps. Clocked feedback retains one sampled
receiver value throughout the interval. Digital pad observations and architectural
sampling pipelines receive only the final result. ADC modulators, smart engines,
cogs, CT and selectable events advance once per system clock, regardless of N.
A larger factor resolves asynchronous feedback and RC settling more closely; the
default 1 provides a coarse clock-step approximation. Propagation delays are quantized
to substeps, so meaningful subclock behavior requires a sufficiently large factor.

Smaller backward-Euler steps improve the transient approximation and change the
final voltage slightly. Threshold crossings can consequently move between system
clocks. At 20 MHz, 8x samples are 6.25 ns apart; a fast unloaded 30-ohm/5-pF pad's
0.15-ns time constant still needs much finer sampling to resolve its edge. Settled
pads retain their cache fast path. Reported currents and violations describe the
last substep's endpoint; they are not averages over the hub clock.

### SIMD electrical integration

Electrical fields live in separate `@Vector(64, f64)` planes, with packed boolean
masks for connectivity, diagnostics and settled lanes. `Pad.Source` and `Pad.State`
remain scalar host-facing descriptors and snapshots. Staging a source prepares its
numeric data without publishing a new reading. Hub initialization prepares the
immutable low/high curve coefficients.

The solver represents each branch as affine segments `I = b - g*V`. Mirrored fast
curves need five regions; a current-limited external source needs three. It tests
at most fifteen region pairs using vector arithmetic and masks, without sorting
breakpoints or searching until convergence. A bank-wide direct-linear shortcut
handles active unlimited linear/floating branches. Ideal remains a straight-line
model with its configured resistance; using a common piecewise representation
never substitutes Realistic curves for it.

Exactly settled lanes retain their completed state until a driver, source or
timestep changes. An entirely settled bank skips solving. Other banks perform
bounded whole-bank work and mask inactive lanes. This bounds numerical iteration;
it does not promise constant wall-clock latency. Strict f64 arithmetic preserves
accuracy but permits scalar/SIMD rounding differences. Region selection prefers
strictly valid candidates; a closest-boundary fallback allows at most
`32 * epsilon * max(1, abs(V))` interval error, preventing gaps at exact breakpoints.

Build and run the repeatable benchmark with ReleaseSafe and coverage disabled:

```sh
zig-0.16.0 build build-windtunnel-pads-benchmark -Doptimize=ReleaseSafe -Dcoverage=false -Dno-gui
# Or build and run one measurement directly:
zig-0.16.0 build bench-windtunnel-pads -Doptimize=ReleaseSafe -Dcoverage=false -Dno-gui -- ideal many simd 100000
```

For example:

```sh
zig-out/bin/windtunnel-pads-benchmark realistic many hub 100000
zig-out/bin/windtunnel-pads-benchmark realistic many scalar 100000
zig-out/bin/windtunnel-pads-benchmark realistic many scalar_small_sort 100000
zig-out/bin/windtunnel-pads-benchmark realistic many simd 100000
zig-out/bin/windtunnel-pads-benchmark realistic many simd_uniform 100000
```

The kernel modes time only electrical integration plus driver collection, with
outputs toggling every eight clocks. `hub` instead times real GPIO instruction
loops and all Hub work, so its denominator is different. All modes exclude setup,
5000 warmup clocks and printing. Workloads are `idle`, `one`, `many`, `eight`,
`rc`, `limited` and `mixed`; profiles are `ideal` and `realistic`. The `eight`
workload starts eight cogs in Hub mode; kernel modes always process one pad bank.
Output is CSV: profile, workload, kernel, clocks, elapsed nanoseconds, nanoseconds
per clock. Compare sequential paired runs with coverage disabled; CPU affinity
is optional. The scalar reference exists only in tests and this benchmark.
See the [measured SIMD performance](pads-performance.md) for paired full-step results.

### Currents and absolute-rating diagnostics

Snapshots additionally expose `pin_current`, positive from the pad into the chip:
`Cinternal * (Vnew - Vold) / dt + Ileakage - internal_current`. External capacitance
is excluded, so current used to charge an external capacitor is not all counted
as chip-pin current. `current` remains the external source's total branch current.

The `violations` structure reports `undervoltage`, `overvoltage` and `overcurrent`.
Both presets default to the [absolute ratings](../p2/datasheet.md#absolute-maximum-electrical-ratings):
voltage below −0.3 V, voltage above `VIO + 0.3 V`, or absolute `pin_current` above
30 mA. Equality is within the limit. Flags reflect completed-clock endpoints and
clear on recovery; the GUI grid and pin inspector display them. They neither stop
simulation nor clamp voltage/current. These stress ratings do not guarantee correct
operation at their boundaries and do not model damage or cumulative exposure.

The ±10 mA protection-diode rating is reference information only: no diode I–V
characteristic is supplied and no protection diode is simulated. The same applies
to chip-wide ratings: VDD −0.3..2.2 V, VIO −0.3..4.0 V, 120 mA per supply pin,
4 A ground current, 2.5 W dissipation, ambient −40..125 °C and storage −40..150 °C,
and ESD ratings of 4 kV HBM/1 kV CDM. DC operating ranges (VDD 1.7..1.9 V,
VIO 3.15..3.45 V, temperature −40..105 °C) and quiescent currents (40 microamp
VDD, 0.5 microamp VIO under the stated reset conditions) remain reference data.
There is no thermal, supply-power, ESD, inductance or noise model. The selected
XI/XO capacitance figures do not characterize P0–P63 and do not replace the
provisional GPIO capacitance.

### Comparators, feedback, DACs and ADCs

Physical receivers are distinct from `io.pads` (the ordinary thresholded voltage),
`io.receiver_inputs`, routed SmartA/SmartB, and cog IN. Schmitt receivers retain
hysteresis. AB comparison reads the local and paired pad voltages; level comparison
uses an internal code/256 VIO reference and the selected local/adjacent pad.
Level modes force 1.5 kOhm drive and reinterpret GPIO drive/inversion bits as the
reference code. Asynchronous feedback observes propagated receiver state; clocked
feedback retains its sampled state for the clock.

DAC output is a linear Thevenin source. Nominal resistances are 990, 600, 123.75
and 75 ohms; corresponding ranges are VIO, VIO*2/3.3, VIO and VIO*2/3.3. Source
voltage is range*code/256 and resistance is independent of code, including zero.
DIR gates ordinary DAC output; TT gates smart-mode output independently of DIR.
The source mux selects WRPIN's static byte, a cog's final DAC byte (pin modulo
four chooses the channel), repeated BIT_DAC nibbles, or the smart-DAC byte.
Combining a DAC pad with any non-DAC smart mode always selects BIT_DAC,
including TT=01. Noise and dither produce actual per-clock codes before electrical integration.
The shared RNG projection is deterministic but its silicon pin tap mapping remains
approximate. `resolvedDriver()` exposes the last committed electrical driver.

ADC inputs now derive from pad voltage. GIO/VIO/internal-float calibration and
1/3.16/10/31.6/100 gains are decoded independently of GPIO field aliases. Transfer
parameters are initialized with the system; gain amplifies about a configurable
midpoint and density clips to [0,1]. The deterministic error-accumulating modulator
emits one bit per system clock. GIO/VIO/float and 1x calibration predict 96 held-out
higher-gain hardware readings within 0.04 absolute density; default profiles are
nominal, while precision comparisons require channel calibration. Silicon noise, exact modulator dynamics and
per-pin offsets are not reconstructed. ADC observation is nonloading; silicon input
bias currents and charge injection are omitted. Plain DAC modes gate ADC input with OUT;
BIT_DAC and every non-DAC smart mode disable it. Smart DAC modes accumulate routed A over their sample period,
so A/B routing can replace the physical bitstream.

`setAdcOverride(pin, ?density)` explicitly supplies test density or restores electrical
conversion with null. It validates [0,1] and finiteness without resetting phase.
Normal simulations have no override. GPIO output remains available in ADC modes.

The ADC smart modes implement automatic SINC2, wrapping 27-bit SINC2/SINC3
integrators and period differences, arbitrary Y period overrides, externally
clocked B-rise sampling, and raw snapshots with newest bit at bit 31. Scope modes
use measured 68/45/28-tap FIR coefficients and hysteretic arming/triggering.
SETSCP selects a four-aligned block of smart-result low bytes for each cog;
GETSCP and all five ADC streamer formats read this pipe. Disabling SETSCP holds
the last word. Scope and streamer captures preserve existing FIFO and event behavior.
USB remains unsupported.

The Ideal profile uses zero receiver/reference delay. Realistic currently assumes
3 ns receiver delay, 10 ns comparator delay and 50 ns reference settling; the
RCFAST measurements bound rather than isolate these quantities. They are tunable
model assumptions, not separately calibrated silicon timings. Thresholds and ADC
transfer also vary between physical pins/boards. See
[the characterization findings](../accumulated.md#analog-frontend-characterization-2026-10-16).

Reproduce the optional two-board measurements with the compiled Propan/Turboprop
binaries and the existing optional Python WebSocket dependency:

```sh
python utility/characterize-analog.py transfer --artifacts /tmp/analog-transfer
python utility/characterize-analog.py receivers --artifacts /tmp/analog-receivers
python utility/characterize-analog.py dac-feedback --artifacts /tmp/analog-dac
python utility/characterize-analog.py mux --artifacts /tmp/analog-mux
python utility/characterize-analog.py combinations --artifacts /tmp/analog-combinations
```

The observer defaults to P2-EVAL PLX6ZJKWX; the DUT is exclusively P2AAS. The
[monitor firmware](../../utility/analog-monitor.propan) lives beside the host utility,
starts with experiment pins released, preserves RCFAST/calibrated UART, and clears
WRPIN/DIR on completion or after a two-second nominal idle watchdog. Raw captures
stay outside tracked source. Deterministic measurements are retained as registered
`.propan` fixtures and require no external hardware for ordinary regression runs.

## Command and transport timing

Commands distinguish configuration, X/Y writes and acknowledgement. Commands
from all cogs commit together after two clocks. Same-edge commands OR
their data and command selectors; collisions log pin, clock, cog, PC and values.
The shared return pipeline makes repository X-to-Z and acknowledgement observable
after four clocks, and the repository's IN indication after five. Repository Z
survives DIR reset; X writes while held reset update X without changing Z.

Hardware GPIO loopback timing depends on frequency, edge polarity and electrical drive.
The simulator retains optimistic, fixed digital transport: drive changes four clocks
after execution, with unloaded fast-drive TESTP changes at instruction spacing eight
and INA/INB at nine. Clock frequency never selects a different transport pipeline.
The provisional RC model adds load- and clock-dependent voltage settling and threshold
crossings; these assumptions do not reproduce measured electrical behavior.
The former 170 MHz timing branch was an approximation, not a validated hardware threshold.
Polarity-, pin- and current-dependent sampling changes support electrical pad slew,
rather than an extra registered transport stage. Comparator searches also have their
own settling and retention limits. The methods and conclusions are summarized in
[Accumulated Propeller 2 Knowledge](../accumulated.md#gpio-loopback-sampling-crossover-2026-10-14).

Command timing, command collisions and per-cog output gating are covered by the
82 YAML probes and eight-phase captures in
`data/timing-data/smart-pin-{cases,results}.yaml`; the matching simulator checklist
is `tests/windtunnel/state/smart-pin-hardware.propan`.

The selected SmartOut/OTHER signal crosses a common registered output pipeline
in every mode. GPIO's SmartOut forwards the registered cog OUT bus; output-generating
engines publish through their output register. Both registrations live in `SmartPin`,
remain clocked across WRPIN changes, and feed the same I/O forwarding path. The model
assigns two clocks to cog OUT transport, one to output-engine publication, and a
common one-clock pad path at every frequency. This fixed decomposition models the
earlier instruction-visible boundary; it does not identify exact physical register
placement. DIR reaches the output gate separately through a fixed three-clock queue
after writeback; its absolute latency was not isolated by these probes.
WRPIN changes selection without resetting or filling the pipeline, and virtual TX
uses the same path. Immediate GPIO-to-UART and UART-to-GPIO selection boundaries
were measured at RCFAST and 200 MHz in 36 cases with 144 observations;
[the measured boundaries](../accumulated.md#serial-smart-pins-live-configuration-and-retained-state-2026-10-14)
include input sampling and do not establish exact physical register placement.
Clocked output selection adds a fixed three-clock pipeline, which advances once per
frame while selected and retains its history while deselected. HUBSET clock changes
do not retap or flush either output pipeline. Smart-pin oracle fixtures use nominal
20 MHz RCFAST, where the digital timing can be observed without the high-frequency
electrical crossover. Other clock rates require a technical need, such as explicit
high-speed behavior testing.
These delay lines use `sim/pipeline.zig`'s allocation-free `Pipeline(T, max_len)`.
`step(value)` returns the value delayed by the active number of steps, and `current()`
reads that same output without advancing. A two-step delay initialized with zero
returns `0, 0, A` for appends `A, B, C`. `resize()` immediately selects another delay
from zero through `max_len`, retaining the full history; zero passes values through.
`tap(0)` reads the latest append, while `pending()` exposes active queued values
newest first and excludes the emitted output.
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
zig-out/bin/windtunnel --image program.bin --sim realistic --vcd pins.vcd
```

The CLI supports VCD for image runs. For embedded use, create `sim/Vcd.zig` with
a `*std.Io.Writer`, call `hub.attachVcd(&vcd)` before stepping, and `vcd.finish()`
before closing the writer. Keep the exporter alive while attached. Export
failures stop the simulator. Firmware reset detaches the exporter; attach a fresh
exporter for a new trace. Finishing only flushes output and adds no clock edges.

Signals use this hierarchy (waveform viewers may display `/` as `.`):

| Path | Meaning |
|---|---|
| `windtunnel/CT` | 64-bit completed-clock counter |
| `windtunnel/clk` | System clock, initially high, with a 50% duty cycle |
| `windtunnel/cog0/alive` | Cog is running, starting, or waiting; false when stopped |
| `windtunnel/cog0/starting` | Cog startup sequence is still in progress |
| `windtunnel/cog0/DIRA` | Complete 32-bit direction register |
| `windtunnel/cog0/DIRA/DIRA[10]` | Independently selectable direction bit 10 |
| `windtunnel/io/DIRA` | Combined direction vector |
| `windtunnel/io/DIRA/DIRA[10]` | Independently selectable combined direction bit |
| `windtunnel/io/pads/p1/receiver` | Physical receiver output, including hysteresis/comparison |
| `windtunnel/io/pads/p1/adc` | Raw ADC bit, emitted once per system clock |
| `windtunnel/io/pads/p1/state` | Thresholded driven state: `0`, `1`, or disconnected `z` |
| `windtunnel/io/pads/p1/voltage` | VCD real value in volts, including retained charge |

Cog scopes run from `cog0` through `cog7`; pad scopes run from `p0` through `p63`.
Both vectors and individual bits are available for OUTA/B, DIRA/B and INA/B in
every cog scope and the combined I/O scope. Bit numbers are local to the
register: DIRB bit 0 controls P32. Combined OUT is the raw OR of cog OUT registers.
Pending cog starts do not set `alive` until delivered. Debugger pause and waits
do not stop a cog or change its liveness.

Pad `state` uses the drivers committed for its completed clock or analog substep, including
WRPIN drive settings and physical output enable. It becomes `z` only when both
internal and external drive are disconnected; a zero-current source also counts
as disconnected. Capacitance and leakage alone do not constitute drive. A
floating pad may retain a voltage or decay through leakage while its trace state
remains `z`. Binary simulator reads still threshold that voltage normally. Before
the first clock, pads are recorded as `z` and 0 V. Staging an external source does
not change the trace until the next clock begins integrating it.

Voltage is emitted independently of digital transitions, with round-trip precision.
Nominal levels are 0–3.3 V, but values are **not clipped**: custom supplies,
external overvoltage and negative voltages remain visible.

Timestamps represent physical simulation time at a 1 fs resolution, independent
of wall time. Each completed clock emits a falling midpoint and a rising boundary,
then the completed snapshot at that rising boundary. With oversampling enabled,
pad voltage and state changes also appear at `k/N` of the period. CT, cog liveness
and register signals are emitted only at the completed-clock boundary. Odd factors
still place the falling edge at exactly half a period, between neighboring samples.
Unchanged pad samples add no redundant records. The first boundary has CT=1
when starting from reset. The period uses exactly the frequency consumed by that
clock's pad integration, including HUBSET changes. Fractional femtoseconds are
carried across steps and frequency changes; timestamp quantization is below 1 fs
at supported P2 frequencies and time remains monotonic across CT rollover.
Tracing does not change execution timing or pad integration. Intermediate records
stream without allocating buffers proportional to the oversampling factor. Export
errors during a substep are reported when that hub clock completes.

Hub performs full and intermediate captures automatically when attached with
`attachVcd`. Direct callers of
`vcd.capture(hub, clock_frequency)` must supply every consecutive completed clock
and the frequency used for that clock. Skipping clocks returns
`error.NonConsecutiveClock`; repeating an unchanged snapshot emits nothing.
Host-only changes at the same counter are emitted at the existing timestamp
without clock transitions. There are no fabricated transitions while paused.
