# Windtunnel inspectors and host debugger

Build with `zig-0.16.0 build build-windtunnel-gui -Dcoverage=false`, then run
`zig-out/bin/windtunnel-gui --image program.bin`. Without an image the hub and
all cogs start stopped. Open inspectors from the Windows menu, or open a pin
from the selected pin's Inspect button below the I/O matrix. Closing an inspector retains its view state and
breakpoints for the current process. Resizing an inspector or the main desktop
window prints its title and dimensions to the terminal, for example
`Window "I/O": 260 x 290`. Initial window sizes are not logged.

Run advances complete hub clocks for up to 14 ms of monotonic host time
per frame. The deadline is checked between clocks; one clock can exceed the
budget. Step acknowledges the current stop and requests one clock. A new
execution breakpoint or a pending breakpoint can prevent that edge. The bottom status
bar shows the current stop, clocks advanced and simulation host time. The toolbar
below the main menu is ordered Reboot, Reset, Run/Stop, Step.
Reset restores the original loaded image and initialized simulation settings;
Reboot resets the virtual chip, clearing RAM and stopping all cogs. Both pause
execution and preserve host debugger definitions. Windows → Debugger opens execution
breakpoint and data watchpoint editors, with details of the current stop. Pin
windows are grouped into eight submenus of eight pins. File contains Exit and
placeholder Load image/Export VCD entries.

Architectural edits are available while paused. Raw RAM/register changes retain
captured pipeline operands and pending writes; a later commit can overwrite an
edit. The PC, CT and scheduler/pipeline latches are read-only. Pin X/Y edits
update the visible register and deliver the corresponding engine write strobe
on the next edge. A simulated pin command on that edge takes priority.
External pad drive buttons work while running, including when the chip drives
the pad; contention is resolved electrically. Pin inspectors show voltage,
branch currents, chip-pin current and absolute-rating violations. Each pin has a
Configure menu containing WRPIN, followed by Pad and Logic sections. Pad starts
with voltage, current into the chip, threshold level, resolved drive mode and
UV/OV/OC indicators. External and pad source tables appear side by side; their
signed branch currents are positive into the pad. Drive labels describe the last
resolved driver, including delayed output selection. Logic starts with A, B, IN,
cog OUT/DIR, Smart out and physical Drive enable. The I/O overview
is an 8×8 matrix with white pin numbers: red means low and green means high at
the physical pad threshold, before the cog input fabric. Selecting a pin shows
its voltage, L/H/Z external-drive buttons and Inspect below the matrix.

Options → System Configuration opens editable ideal/realistic configuration drafts.
HUBSET and random seed controls remain in Hub; each Cog inspector has its own
Start menu with address and PTRA inputs, followed by Start and Start without load.
Its separate Stop action is enabled for running or scheduled cogs. These controls
also work while simulation is running. Each cog's Options menu controls both
register and LUT views: Tail RAM and Execute word start hidden, while Disassembly
starts visible. Architectural configuration / prefixes follows Events.
Locks use a compact ID/Allocated/Taken/Owner table; Owner uses a bounded drag
control for cog IDs (0–7). Random Generator uses a Register/Value table, and
random state and seeds use hexadecimal only. Hex values use uppercase digits, a `0x` prefix and the
word's full nibble width. Memory dump bytes use `AA BB CC` without prefixes;
its resizable columns fit the address, sixteen bytes and sixteen ASCII characters.
Memory starts at 565 × 435 with the dump filling its body. Its local Jump menu
accepts a byte address; selected byte ranges are edited in Debugger → Data
watchpoints. I/O starts at 260 × 290. Generic inspector blocks have bright
magenta backgrounds. Options → Hide automatic UI hides all generic inspector
blocks while retaining the curated layout. Cog windows start at 400 × 500 and
Hub starts at 320 × 340.
Draft edits never affect an initialized simulation. Reinitialize with settings
applies the validated draft, restores the original image and pauses, preserving
debugger definitions while discarding runtime RAM edits and external stimuli.
See [system configuration](io.md#initialization-time-system-configuration).

Each cog starts with a status table for execution mode, PC, editable C/Z flags,
primed AUGD/AUGS and INT1–INT3. Interrupts show `-` when disabled or stalled,
`ENA` when enabled/pending and `ACT` while servicing an interrupt. Events form
a 4×4 matrix; SE1–SE4 tooltips decode the selected pin, LUT access or lock edge.
The stack labels identify slot 0 as the PUSH/POP top, slot 1 as the next POP,
and slot 7 as the bottom discarded by PUSH.

Local tables show architectural data, underlying tail RAM and execute-only ROM
separately. General-purpose registers have role `GP`. The `BP` column contains
an unlabelled execution-breakpoint checkbox with an ID distinct from its header;
hex data and tail RAM editors fill their cells. Disassembly is independent per
word, without folding augmentations.
Signed branch immediates show encoded displacements from fallthrough; long
branch displacements are bytes. Hover a register or LUT memory row to see its
annotations. Highlighted rows have references or special storage; the tooltips
identify actual physical fetch locations and every matching stack slot, even
repeated/sticky entries. They do not infer a call depth. LUT table addresses are relative to LUT RAM; execution
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
groups, closes/reopens the breakpoint and configuration windows, jumps to the
last RAM row, and verifies that rendering preserves state and produces no watch hits. Desktop checks should also exercise actual mouse
editing, scrolling, window close buttons and both pipeline views.

The [issue collection](issues.md) retains every report from a completed edge. An issue stop opens, expands and focuses Debugger. Its reports remain visible with Hide automatic UI enabled. Run/Step acknowledges the stop; the next edge clears the reports. Service errors pause through the same host path while retaining their adapter state.
