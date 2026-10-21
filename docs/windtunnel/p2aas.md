# P2AAS server

Windtunnel can replace the local P2AAS hardware service for firmware supported by the simulator:

```sh
zig-0.16.0 build install -Doptimize=ReleaseFast
zig-out/bin/windtunnel --serve
# Equivalent, or select another bind address and port:
zig-out/bin/windtunnel --serve --url=ws://127.0.0.1:21591/
```

The default URL is `ws://127.0.0.1:21591/`. Listen URLs accept an IPv4/IPv6 address or `localhost`,
a root path, and the `ws` scheme. An explicitly supplied URL without a port uses port 80.
`--serve` cannot be combined with `--image`. Regular host logs are printed
in server mode; `--verbose` also enables debug logging.
Completed-edge issue and service diagnostics go to stderr; VCD remains the signal export format.
Add `--sim realistic` to initialize every fresh session with datasheet-based pad and [clock](clocks.md)
characteristics; the default is `--sim ideal`. The selected configuration survives
firmware resets. Add `--fast-io` for single-clock serial endpoint throughput in every fresh session;
normal sessions retain UART baud timing. See [serial endpoint behavior](io.md#smart-modes-and-endpoints).
The server uses Zig's standard library and requires no external service, interpreter or package.

## Client contract

One request owns the board at a time; later connections wait. Every accepted image starts with reset
hub/cog/peripheral state and cog 0 loading the initial 504 longs from hub address zero.
Other cogs start stopped. The serial-ROM handoff selects RCFAST (`$F0`), normally
24 MHz in ideal mode, and preserves 2 Mbaud 8N1 dividers derived from the initial
configured RCFAST frequency in pins 62 and 63, with modes and DIR cleared. Restoring the UART modes reuses those dividers without WXPIN or a startup
wait. The initial counter is zero; physical loader execution time and calibration
error are not reproduced. Configured oscillator drift applies during execution.

| Parameter | Default | Validation |
|---|---|---|
| `baudrate` | `115200` | Positive signed 32-bit integer; blank values use the default |
| `timeout_ms` | `2500` | `100..10000`; blank values use the default |
| `code` | Absent | Base64 or base64url; optional padding; at most 512 KiB; word-aligned |

Invalid query parameters and non-WebSocket requests receive HTTP 400 before upgrade.
Unknown query parameters are ignored. Duplicate `code` is rejected. `code=` is valid and selects an
empty URL upload, bypassing the WebSocket length prelude.

Without `code`, clients send a four-byte little-endian image length followed by that many binary bytes.
The length must be nonzero, at most 512 KiB, and divisible by four. Message boundaries and fragmentation
are irrelevant. Runtime bytes following the image in the same frame remain available to the bridge.
Ping/pong and close frames can interrupt a fragmented upload. Upload text is rejected.

The physical loader appends a checksum complement, `0x706f7250 - sum(image longs)`, using wrapping
arithmetic. Windtunnel writes that word immediately after the image. At the 512 KiB boundary the next
address is a hub hole, so this write does not wrap over address zero. The loader text commands and serial
reset/recovery operations are internal to the hardware server and do not need a simulated serial adapter.

After loading, text and binary WebSocket bytes feed a Reader-backed `DataSource` attached to smart pin 63;
completed UART TX frames on pin 62 reach a Writer-backed `DataSink` and become binary WebSocket messages.
Clients must treat output as a byte stream, rather than rely on message boundaries. The server refills
its Reader outside simulation steps. Input buffers are bounded and apply backpressure; bytes remain
buffered while RX is disabled. DIR reset discards an in-flight receive frame, and completed frames can
still overwrite unread results. Receive frame timing follows the firmware's `WXPIN` configuration.
The `baudrate` query is accepted and validated for protocol compatibility; the simulator applies no
separate host baud override.

| Termination | Close status / reason |
|---|---|
| Client close | Normal closure, `1000` |
| HUBSET reset | `1000`, `Simulation stopped.` |
| Upload / frame protocol violation | `1002`, with a reason |
| Runtime quota expired | `1008`, `No time quota left for user code.` |
| Total request quota expired during runtime | `1008`, `No time quota left.` |
| Total request quota expired during upload | `1011`, `The server experienced an unexpected error.` |
| Simulator fault / internal error | `1011`, `The server experienced an unexpected error.` |

The total quota is ten wall-clock seconds. Runtime quota starts after loading. Close handshakes have
an additional two-second cleanup bound. All cogs stopping does not close the session early. Pending
cog starts and peripheral direction changes still receive hub clock edges. HUBSET reset
clears the simulated Hub and closes the session immediately, preserving already-delivered UART output.

## Timing and coverage

Use ReleaseFast for live sessions. Execution is paced so simulated time does not run ahead of wall time;
every system clock is simulated. Heavy firmware can run slower than the physical board and time out
before producing its complete output. [Clock configuration](clocks.md) controls RCFAST, RCSLOW,
the crystal, bounded drift and seed. Ideal defaults are 24 MHz, 20 kHz and 20 MHz.
Realistic mode includes measured PLL responses for the characterized transitions, with
explicit fallback elsewhere. Pacing follows accumulated physical time, including acquisition
without CPU edges. Electrical UART effects, including baud mismatch corruption, remain
outside the functional terminal model.

This is protocol compatibility for supported firmware, not complete coverage of the P2 instruction set
or all smart-pin modes. A completed edge with architectural issues or service errors ends that request with 1011
and prints every diagnostic with its captured provenance. The next request can execute normally.

## Validation

The normal `zig-0.16.0 build test-windtunnel` step includes protocol parser tests, continuous RX/clock
checks, real TCP sessions through the native P2AAS client, simulator-failure close handling, and the
program fixtures. Terminal fixtures use `run:` to share one source across binary and text packets.

Run the existing native oracle client through the simulator by changing its endpoint:

```sh
P2AAS_ENDPOINT=ws://127.0.0.1:21591/ zig-out/bin/windtunnel-tests --oracle \
  tests/windtunnel/program/server-terminal.propan \
  tests/windtunnel/state/pipeline-prefetch.propan \
  tests/windtunnel/state/pipeline-startup.propan
```

An optional Python/WebSocket compatibility matrix exercises both services with the same firmware:

```sh
zig-out/bin/propan -o /tmp/p2aas-terminal.bin tests/windtunnel/program/server-terminal.propan
zig-out/bin/propan -o /tmp/p2aas-loader.bin tests/windtunnel/program/server-loader.propan
.venv/bin/python tests/windtunnel/p2aas_protocol.py \
  --url ws://127.0.0.1:21591/ --terminal /tmp/p2aas-terminal.bin --loader /tmp/p2aas-loader.bin
# Repeat with --url ws://127.0.0.1:12880/ for the physical P2AAS service.
```

The matrix checks HTTP/upload errors, combined and fragmented uploads, standard/base64url uploads,
binary and text input across several chunks, ping/pong, client close, runtime and total deadlines,
reset isolation, checksum placement, a full 512 KiB image, and stopped-cog sessions. The full-size probe
uses a harmless checksum instruction if a loader incorrectly wraps, then reports the memory word at
address zero to detect that error. The following short upload checks that old RAM does not survive reset.
These probes passed on the local hardware service during development on 2026-10-09; hardware assertions
compare byte streams and close statuses, not packet boundaries or absolute loader timestamps.
The native oracle client also passed all 35 runs in the command above against Windtunnel.
The existing compiled .NET `p2aas-run` client passed the same terminal exchange and timeout closure
against both endpoints without changes.

`data/timing-data/run_tests.py` and `utility/p2aas-run.py` also honor `P2AAS_ENDPOINT`. Leaving it unset
preserves their existing hardware transport. The timing-data runner's WebSocket transport needs the
optional Python `websockets` package; its result parser is shared with the serial transport. Existing
CORDIC commands and result reads are implemented; their measured arithmetic, pipeline timing,
and empty-read behavior are described in [Accumulated Propeller 2 Knowledge](../accumulated.md#cordic-computer).
All eight other timing cases matched the hardware service across eight hub phases, including cog
startup. The timing fixture now retains RCFAST and the ROM-calibrated upload
baud rate, so it needs no initial clock or UART settling wait. The serial and
P2AAS timing transports both use 2 Mbaud.
