# Bench wiring verification — 2026-10-13

All stated connections passed slow digital continuity and mapping checks.

| Connection | Result |
|---|---|
| Logic 16 channels 0–7 → P2AAS board P0–P7 | PASS, in order |
| Logic 16 channels 8–15 → P2AAS board P32–P39 | PASS, in order |
| P2AAS board P8–P15 → second board P8–P15 | PASS, all 256 byte patterns |
| Second board P8–P15 → P2AAS board P8–P15 | PASS, all 256 byte patterns |

The second board is
`/dev/serial/by-id/usb-Parallax_Inc_Propeller_P2-EVAL_PLX6ZJKWX-if00-port0`.
The FTDI board was accessed exclusively through `ws://localhost:12880/`.
Both probes were loaded into RAM; flash was not programmed. Test pins were
released before ending the sessions.

## Evidence

- `digital.csv` and `wiring.sal`: complete three-frame walking-one/walking-zero
  sequence over all 16 analyzer channels, captured at 500 kS/s. Stable states
  lasted about 7.746 ms. All 100 stable plateaus, including zero markers,
  matched the expected 16-bit sequence exactly. Brief transitions between
  sequential port writes are excluded by the 1 ms plateau threshold.
- `interboard.json`: 512 expected/observed byte pairs, with no mismatches.
  Each receiver initially read zero under its weak pulldown.
- `probe.propan`: shared RAM probe, using ROM-calibrated UART at 2 Mbaud,
  1.5 kOhm output drive, and a 150 kOhm receiver pulldown with floating high drive.
- `check.py`: inter-board test runner; uses Python's standard-library serial
  facilities and the existing optional `websockets` dependency.

The final Saleae capture used device `B4045E72363CE3C7`, all 16 digital
channels, 500 kS/s, a channel-0 rising trigger, one second after the trigger,
1.2 seconds of retained pretrigger data, and a 1024 MB buffer limit. This
completed successfully. Earlier smaller-buffer attempts were discarded.
The clone's high-bandwidth performance was not tested.

## Repeat the inter-board check

Run from the repository root with host serial-device access:

```sh
zig-out/bin/propan -o data/timing-data/wiring-check/probe.bin data/timing-data/wiring-check/probe.propan
.venv/bin/python data/timing-data/wiring-check/check.py check
```

The runner rewrites `interboard.json`. The observed runtime was 9.340 seconds
inside the P2AAS session's 10-second quota; a substantially slower host may
need the directions checked in separate sessions. The `capture` argument
sends the analyzer patterns instead; arm the Saleae capture beforehand.

Existing compiled Propan and Turboprop binaries were used; no Zig compiler
was invoked for this check.
