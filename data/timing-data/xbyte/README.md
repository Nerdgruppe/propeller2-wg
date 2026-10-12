# XBYTE hardware measurements

Captured on 2026-10-12 through `ws://localhost:12880/`. The probe programs are
registered regressions in `tests/windtunnel/state/xbyte-*.propan`.
`measurements.json` retains the observed register values for their 104 cases.
XBYTE status observations mask off unrelated event-trap bits.

| Fixture | Runs | Evidence |
| --- | ---: | --- |
| `xbyte-lookup` | 8 | All 512 mode values × all 256 bytes, spread across eight cogs; independently calculated address/flag digests and per-dispatch PA/PB/mode checks |
| `xbyte-dispatch` | 20 | Direct address signatures in LUT skip data, flags, PA/PB, Q, stack retention and dispatch timing |
| `xbyte-modes` | 2 | Persistent SETQ, single-dispatch SETQ2, nine-bit masking, ordinary SETQ before RET, implicit and explicit returns |
| `xbyte-control` | 24 | Eight-clock single-instruction loops, explicit RET, cog/LUT handlers, trailing skips, nested calls, POP escape, blocking RDFAST stream changes |
| `xbyte-fifo` | 16 | Nonblocking RDFAST startup across eight gaps on cogs 0 and 7, empty-byte dispatch and pointer retention |
| `xbyte-interrupts` | 34 | CT1 phase sweep, ISR timing, saved PC, PA/PB and GETBRK snapshots on cogs 0 and 7 |

Build and compare with the board:

```sh
zig-0.16.0 build install -Dcoverage=false
zig-out/bin/windtunnel-tests --oracle tests/windtunnel/state/xbyte-*.propan
```

Capture fresh measurements without executing the simulator:

```sh
zig-out/bin/windtunnel-tests --characterize --artifact-dir /tmp/xbyte-capture tests/windtunnel/state/xbyte-*.propan
```

Each characterization artifact includes the source, assembled/uploaded images,
request, binary hardware output, transport log and full register snapshot.
The compact checked-in summary retains the observations used by the regressions;
the full local capture directories are not required for replay.

The mode-table expectations were calculated independently from the silicon
mode descriptions before comparing with the measured digests. The simulator
matches those digests. The fixture records a digest by rotating left seven,
adding the selected LUT address and captured flag bits, then XORing the result
with itself shifted right thirteen. It checks each of the 131,072 PA, PB and mode
results separately. Prefix tests use a register for the 32-bit mode operand:
Propan applies a return condition to generated augmentation instructions too.

See [accumulated knowledge](../../../docs/accumulated.md#xbyte-bytecode-execution-2026-10-12)
for the measured rules. Local PC overflow across $3FF and streamer interactions
remain outside this matrix.
