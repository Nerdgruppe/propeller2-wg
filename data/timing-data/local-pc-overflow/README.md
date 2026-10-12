# Local PC overflow: explicitly open

These are characterization probes, not registered Windtunnel regression tests.
Run them with `zig-out/bin/windtunnel-tests --characterize --artifact-dir /tmp/local-pc-overflow data/timing-data/local-pc-overflow/*.propan`.
The default P2AAS endpoint is `ws://localhost:12880/`.

On 2026-10-12, `pc-stamps.propan`'s `kind2` case branched to LUT address $3FF,
executed its NOP and then a CALLD from the sequential wrap destination. The
saved return PC was **$401**, not $001. `kind1` reached its exit in 12 clocks;
the stamp cases are retained without extrapolating a general fetch/timing rule
from their more complex results. `pc-stamps.json` contains the full hardware
register snapshots and observation offsets.

`rep-cross-wrap.propan` places REP 2,3 at $3FE, an ADD at $3FF and another ADD
at cog address zero, followed by an absolute exit at cog address one.
Windtunnel produces six additions; silicon repeatedly produced **one**. The
original binary reporter frame is retained as `rep-cross-wrap-frame.bin`.
The fixture asserts the hardware result and is intentionally unregistered: a
normal simulator run currently fails it.

Windtunnel's existing local fetch wraps its entire PC at $400. The measured
saved PC proves that this loses upper PC bits. The relation between overflow,
fetch addressing, operand/branch behavior and REP boundaries has not been
recovered. A change to REP's countdown alone would not establish a correct
model. This boundary remains explicitly open; it is not included in the passing
REP/SKIP hardware matrix or its timing claims.
