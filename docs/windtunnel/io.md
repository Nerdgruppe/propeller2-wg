# Windtunnel I/O

`sim/IO.zig` owns 64 `Pin` instances, each containing a typed WRPIN
configuration, a `SmartPin` from `sim/smart_pin.zig`, and its published outputs.
`SmartPin` owns one shared `Registers` struct and a `Logic` union tagged by
`libp2.types.SmartPinMode`. Repository, UART TX/RX and USART TX/RX payloads
contain only their private transfer state. `configure()` replaces the payload
and retains registers and endpoints. `step(Inputs)` updates X/Y, enabled and
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
participation in idle scheduling. IO owns input/output routing and pad drive
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
32-pin port; SETQ overrides the field count. Random variants use deterministic,
seedable clock-indexed noise rather than the silicon PRNG's bit mapping.

## Smart modes and endpoints

Long repository (smart mode 1, including mode aliases 2/3), UART TX/RX and USART
TX/RX are supported. UART transfers use the configured 1–32-bit word width and
fixed-point frame duration. Transmit has a shifter and one pending word; receive
overwrites unread UART results. Receiver results are MSB justified. COGSTOP
clears the cog's DIR bits; the next shared edge aborts an unfinished transfer
if no other cog keeps that pin enabled. Runners do not drain transmissions
after shutdown or a fixture checkpoint. Programs that require complete output
must wait for UART completion before stopping.

Attach caller-owned `smart_pin.DataSink` and `smart_pin.DataSource` objects:

```zig
var sink: IO.DataSink = .{ .writer = writer };
var source: IO.DataSource = .{ .reader = reader };
hub.io.pins[40].smart.registers.sink = &sink;
hub.io.pins[41].smart.registers.source = &source;
```

Writer adapters truncate to a byte; Reader adapters promote a byte to a 32-bit
word. Callback endpoints preserve all 32 bits. A callback source returns `null`
when no word is available. Readers consume only already buffered bytes: the host
must refill them outside simulation steps. Keep endpoints and their backing
objects alive while attached. Sink/source errors stop execution with pin and
instruction provenance.

Endpoints attach to the smart engine and bypass physical routing and serial
encoding/decoding. USART deliberately transfers whole words without modelling B
clock edges: TX sends on a pin update, RX takes an available word and holds it
until acknowledgement. UART uses frame timing, but active TX pads are unknown
(`x`), rather than a generated serial waveform. The server and test harness
attach their input Reader to P63; CLI/server/test runners attach their output
Writer to P62. IO has no default terminal endpoints or terminal queue. The server
refills its Reader from the WebSocket input queue outside simulation steps,
and the test harness attaches its fixed input after the readiness marker.
UART receive timing comes from the smart pin's X register for every source.
The P2AAS baudrate query remains accepted for protocol compatibility; the
simulator does not apply a separate host baud override.

## Digital pads and timing

`io.setExternal(pad_index, signal)` supplies an external drive (`zero`, `one`, `x` or
`z`). Pads resolve internal and external drives, including floating drive modes
and contention. A/B selectors route local/relative pads or the gated OUT bit;
inversion, digital A/B logic, feedback and optional synchronization are modelled.
Digital and Schmitt modes use digital levels; analog behaviour, input filters,
DAC/scope instructions and remaining smart modes are explicitly unsupported.
Boolean IN samples `x` and `z` as zero.

Commands distinguish configuration, X/Y writes and acknowledgement. Commands
from all cogs commit together after two clocks. Same-edge commands OR
their data and command selectors; collisions log pin, clock, cog, PC and values.
The shared return pipeline makes repository X-to-Z and acknowledgement observable
after four clocks, and the repository's IN indication after five. Repository Z
survives DIR reset; X writes while held reset update X without changing Z.

GPIO drive changes occur five clocks after instruction execution. Input sampling
adds the documented latency: the first changed TESTP result is at instruction
spacing nine clocks, and an INA/INB operand at ten. These timings, command
collisions and per-cog output gating were measured on hardware. The 82 probes and
eight phase captures are in `data/timing-data/smart-pin-{cases,results}.yaml`;
the matching simulator/hardware checklist is `state/smart-pin-hardware.propan`.
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
monotonic across CT rollover and clock skips. One synthetic VCD nanosecond means
one simulated system clock, independent of HUBSET frequency or wall time.
Tracing does not change execution or idle skipping.
