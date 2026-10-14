# UART and USART hardware validation

Bench: 2026-10-14. Windtunnel implements per-pin RX exact/emulated/virtual and
TX emulated/virtual modes. Timed receivers keep their live width, divider,
partial shift and sampling state; emulated USART requires the routed B clock.
Both TX modes report completed words to a DataSink. Emulated TX also produces
SmartOut; virtual TX generates the protocol reset level through the ordinary output
pipeline, retaining queued transitions. See [the API and lifecycle rules](../../../docs/windtunnel/io.md).

## Bench and results

The primary board is accessed only through `ws://localhost:12880/`. The secondary
is `/dev/serial/by-id/usb-Parallax_Inc_Propeller_P2-EVAL_PLX6ZJKWX-if00-port0`.
P8–P15 connect 1:1. The independent receiver is P10, selecting secondary-driven
P8/P9 as A/B; its physical output stays disabled. Programs load into RAM and
release test outputs before reporting. Independent-board programs also have
input watchdogs and bounded output polls.

P2AAS has **60 seconds per run**. Native register probes request one second and
finish through the checksummed reporter. The interactive external-board campaign
requests 50 seconds because USB command round trips dominate its runtime.
A runtime timeout describes that request; it does not imply exhausted shared quota.

Validation uses `zig-0.16.0`, required by `build.zig.zon`:

- **294 native silicon cases**, with **2,558 literal register observations**, frozen
  from independent hardware captures before comparison with Windtunnel.
- **294 fresh oracle comparisons passed**: the final campaign includes the
  single-clock UART width-change cutoff and fast synchronous cases.
- **65/65 independent-board checks passed**: all 1–32-bit UART/USART receive widths
  and an active 8-to-16-bit synchronous extension.
- **1,500/1,500 waveform comparisons passed**: UART P4, USART P5 and B clock P6
  in each of 500 complete frames. P0 supplies the independent GPIO markers.
  The clone records four channels at **50 MS/s**. Edge order/count and initial
  levels match, with timing error bounded by two analyzer samples after marker
  calibration. The software and native register probes check faster timing.
- The original campaign's software counts and command results are in `evidence/validation.json`.
  Full fixture, unit and Debug runs include the prior digital smart-pin regressions.

The output-path follow-up removes UART-specific pipeline initialization and virtual
TX flushing/bypass. GPIO, UART and PWM now supply SmartOut to the same I/O path;
engine output registration lives in `SmartPin`. `output-selection.propan` measures
36 GPIO-to-idle-UART-to-GPIO cases (144 observations), including immediate selection
at nine instruction offsets, both prior OUT levels and both clock bands. Original
capture sources/images/snapshots are retained in `evidence/output-selection.tar.gz`,
with a literal-checked index in `output-selection.json`. Follow-up software, hardware
and waveform results are recorded separately in `output-path-validation.json`.

## Measured coverage

All native fixtures are registered in the normal build and run without hardware
against their frozen silicon expectations:

| Fixture suffix | Behavior |
| --- | --- |
| `sync-rx` | Continuous shift history, widths 1/8/32, active shortening/extension |
| `sync-tx` | Reset priming, continuous/start-stop buffering, overwrite before/after first clock |
| `sync-fast` | NCO-driven clock/2, clock/4 and clock/8, both X[5] sample positions, widths 8/16/32 |
| `uart-wave`, `uart-phases`, `uart-widths` | Physical loopback, every width, fractional divider, eight instruction offsets |
| `uart-late-width` | Live width extension after the nominal last sample, exact capture cutoff |
| `uart-live` | Active width changes and 64-to-32/128-clock divider changes |
| `uart-buffered` | Back-to-back odd/even-width frames at 7.25/7.5 clocks, retaining fractional phase |
| `uart-divider` | Minimum supported 3-clock period, 4/5 clocks, fractional cutoff at 64, maximum 65,535 |
| `uart-ready` | RX IN, TX busy, selectable events and acknowledgement at eight offsets |
| `count-wrap` | Shortening below the active count stalls through the bounded long stream |
| `reset`, `restart`, `tx-reset` | Retained Z/shift data, cleared IN, partial TX abort, priming and same-mode WRPIN |

Receiver shift registers retain old bits, including across DIR reset. RDPIN's
configured high bits are the current word; low bits are not guaranteed zero.
UART TX exposes its running shifter through Z. USART captures the shifter at word
boundaries. Both feed routed A back into the high bits; forced-high A probes
separate this from rotating the payload. Detailed findings are recorded in
[Accumulated Propeller 2 Knowledge](../../../docs/accumulated.md).

Simulator tests cover source starvation, full 32-bit callbacks, partial exact/emulated
handoffs, retained staged values, live width extension, per-pin virtual operation,
ACK backpressure, sink replacement, aborts, fractional source cadence, and endpoint
errors. Returning from virtual USART seeds the B history so a held-high clock
cannot invent an edge. Enabling a virtual transmitter without WYPIN cannot invent
a payload.

## Evidence and reproduction

`evidence/characterization.tar.gz` retains each accepted original/uploaded image,
captured source, constants, observation offsets, full hardware snapshot,
checksummed serial report and transport/request metadata. `characterization.json`
indexes the 294 accepted cases and their image/source hashes. The packer checks
formatted source identity and every frozen register value; it uses no simulator
output to manufacture expectations. Final oracle passes retain their logs; the
native runner creates failure artifacts only for mismatches.

The `.sal`, raw CSV, compact physical-pin VCD, exact hardware source/image,
completion record and comparison JSON preserve the waveform experiment. VCD
timestamps are **system clocks at 200 MHz** despite the exporter’s `1ns` timescale;
the comparison reads them accordingly. `waveforms.propan` runs one simulation pass;
the retained hardware image runs 500 finite passes. Configuration and floating
intervals are excluded; the comparison includes the initial idle level and every
edge from 5 to 240 microseconds after the final marker fall.

```sh
zig-0.16.0 build build-windtunnel-tests build-windtunnel -Dcoverage=false -Doptimize=ReleaseFast
zig-0.16.0 build test -Dcoverage=false -Doptimize=ReleaseFast
zig-0.16.0 build test-windtunnel-unit test-windtunnel-debugger -Dcoverage=false -Doptimize=Debug
zig-out/bin/windtunnel-tests --include serial-
P2AAS_ENDPOINT=ws://localhost:12880/ zig-out/bin/windtunnel-tests --oracle --include serial-
.venv/bin/python data/timing-data/serial-smart-pins/external.py
python3 data/timing-data/serial-smart-pins/compare_waveforms.py \
  data/timing-data/serial-smart-pins/evidence/waveforms-50m/digital.csv \
  data/timing-data/serial-smart-pins/evidence/waveforms-simulation.vcd \
  --output /tmp/serial-waveform-comparison.json
```

The external runner is optional bench tooling using the already-installed
`websockets` package, compiled Propan and Turboprop. Windtunnel itself uses its
compiled binary and standard library. Do not run two owners of the primary
P2AAS connection simultaneously.

For new measurements, first collect without running the simulator, then freeze:

```sh
P2AAS_ENDPOINT=ws://localhost:12880/ zig-out/bin/windtunnel-tests --characterize \
  --artifact-dir /tmp/serial-research tests/windtunnel/state/smart-pin-serial-uart-live.propan
python3 data/timing-data/digital-smart-pins/fixtures.py freeze \
  tests/windtunnel/state/smart-pin-serial-uart-live.propan /tmp/serial-research
```

`pack_evidence.py` accepts the characterization directories, requires every current
serial fixture/run, and rebuilds the evidence archive only after validating them.

The measurements establish RCFAST and 200 MHz behavior on this bench. They do not
expand the digital electrical model, validate underspecified timed UART dividers
below three clocks, or claim a full RMII network-driver integration. USB, ADC and
DAC modes remain outside this change.
