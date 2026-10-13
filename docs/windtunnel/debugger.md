# Windtunnel inspectors and host debugger

Build with `zig-0.16.0 build build-windtunnel-gui -Dcoverage=false`, then run
`zig-out/bin/windtunnel-gui --image program.bin`. Without an image the hub and
all cogs start stopped. Open inspectors from Control → Windows, or open a pin
from its pad's Inspect button. Closing an inspector retains its view state and
breakpoints for the current process.

Continue advances complete hub clocks for up to 14 ms of monotonic host time
per frame. The deadline is checked between clocks; one clock can exceed the
budget. Step acknowledges the current stop and requests one clock. A new
execution breakpoint or an existing fault can prevent that edge. Execution
shows the current stop, matches, clocks advanced and simulation host time.

Architectural edits are available while paused. Raw RAM/register changes retain
captured pipeline operands and pending writes; a later commit can overwrite an
edit. The PC, CT and scheduler/pipeline latches are read-only. Pin X/Y edits
update the visible register and deliver the corresponding engine write strobe
on the next edge. A simulated pin command on that edge takes priority.
External pad drive buttons work while running, gated by the same physical
output-enable helper used in pad resolution (delayed GPIO DIR, or smart TT).

Local tables show architectural data, underlying tail RAM and execute-only ROM
separately. Disassembly is independent per word, without folding augmentations.
Signed branch immediates show encoded displacements from fallthrough; long
branch displacements are bytes. Annotations identify actual physical fetch
locations and every matching stack slot, even repeated/sticky entries. They do
not infer a call depth. LUT table addresses are relative to LUT RAM; execution
PCs retain the `$200` base.

## Host debugger API

Attach a caller-owned `windtunnel.Debugger` with `hub.debugger = &debugger` and
keep it alive until detached. Call `debugger.deinit()` to release its lists.
Execution breakpoints select cog, execution domain and exact PC. They stop
before any part of the next edge, independently of emulated BRK/COGBRK.
`debugger.acknowledge(hub)` lets the stopped encounters proceed; stalled retries
retain that acknowledgement, and a later encounter at the same PC stops again.

Watch ranges are half-open: local register/LUT longs, or backing hub RAM bytes.
Data scope records operands actually consumed and transfers. Physical scope
also records fetches and preloads, including cancelled instructions. A consumed
latched operand is reported to data watches; its earlier physical preload is
reported to physical watches. FIFO reads retain their original backing addresses
through prefetch and block reloads. Hub mirrors match their backing byte range;
holes and rejected write lanes produce no RAM hit. Accepted same-value writes
still count. Issuing instruction provenance travels through delayed work.

A data hit completes the edge before stopping. `hits` retains every matching
access from that edge, including watch index, clock, owner/issuer, instruction
when available, storage view, address, direction and value. Inspection and host
edits happen outside `Hub.step` and never report watched traffic.

## Validation

- `zig-0.16.0 build test-windtunnel-debugger -Dcoverage=false`
- `zig-0.16.0 build test-windtunnel-gui -Dcoverage=false`
- `zig-0.16.0 build test-windtunnel-unit -Dcoverage=false`
- `zig-0.16.0 build install test -Dcoverage=false`

The GUI test uses an ImGui context without a desktop/OpenGL connection and
renders all 72 per-instance inspectors, closes/reopens them, expands inspection
groups, jumps to the last RAM row, and verifies that rendering preserves state
and produces no watch hits. Desktop checks should also exercise actual mouse
editing, scrolling, window close buttons and both pipeline views.
