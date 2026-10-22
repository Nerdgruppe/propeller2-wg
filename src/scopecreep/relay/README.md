# Scopecreep remote-display relay

A standalone C#/.NET 10 replacement for the direct `nc` connection. It owns a persistent
connection to `192.168.178.115:8888` and serves one local client on `127.0.0.1:8888`.
Both endpoints are deliberately hardcoded for now.

**Hardware transport and screenshot reconstruction tested.** Generic late clients still
need decoder parameters; the optional screenshot utility supplies the verified parameters.
See the [evaluation](../diagnostics/remote-relay-2026-10-17.md) for evidence.
Do not start it alongside another direct scope connection.
The scope may require its on-device approval prompt when the relay first connects.

## Behavior

- Reads the scope's 8-byte video header exactly, validates magic `55 AA` at offsets 2–3,
  and reads the uint32 little-endian payload length at offset 4. Payloads are limited to
  16 MiB. Unknown header bytes are preserved.
- Forwards each **complete original header and payload** unchanged. This is the same
  framed protocol, not a bare H.264 output stream. Partial packets are never forwarded.
- Continuously drains and discards video when no local client is connected. A client
  arriving during a discarded packet starts at the next packet boundary.
- Reassembles local input into exactly 12-byte events and forwards those bytes verbatim
  through the same scope connection. Event values and coordinates are not interpreted;
  a 12-byte all-zero experimental packet is also forwarded unchanged. An incomplete
  input tail is discarded when its client disconnects.
- Keeps the scope connection open when the local client disconnects. Additional clients
  are immediately closed while one client is active. Clients connecting while the scope
  is disconnected are also closed and must retry later.
- Buffers at most 32 video packets and 32 MiB, including a packet being written. A full
  queue or a local write exceeding two seconds disconnects that client, allowing the
  relay to keep draining the scope. It does not silently drop H.264 packets within a
  client session.
- Retries failed scope connections after five seconds; connection attempts time out
  after five seconds. Upstream failure closes the current local client so two upstream
  streams are never spliced into one downstream session. Invalid video framing also
  closes that scope session rather than guessing a new boundary.
- Does not replay control input on reconnect. An in-progress complete event may finish
  after its local client disconnects. If an upstream write fails or exceeds two seconds,
  that scope connection is closed because it might contain a partial event.
- Ctrl+C and Linux SIGTERM close connections and stop the listener. Logs go to stderr.

No video packets, SPS/PPS parameter sets, or keyframes are cached for later clients.
In a 15-second hardware sample, a late client received three keyframes but no SPS/PPS;
FFmpeg could not decode either the framed stream or the extracted H.264 payloads.
Decoder clients must therefore supply the known parameters or retain them from startup,
then join at a decodable keyframe boundary. The optional utility below supplies them.
The relay contains no decoder, UDP discovery, or approval automation. The
[Scopecreep MCP server](../README.md) is now its integrated video/control client and supplies
the known decoder parameters itself; it replaces the optional screenshot utility below.

## Optional live screenshots

With the relay running and no other local client connected, run from the repository root:

```sh
bash utility/scope-capture.sh
```

This optional developer utility requires Python 3 and FFmpeg. It continuously replaces
`/tmp/micsig.png` atomically. Stop it with Ctrl+C to free the relay's single client slot.
It sends no input events and can be restarted without reconnecting the scope.

`utility/scope-video.py` strips each eight-byte packet header, prefeeds this instrument's
actual SPS/PPS, and skips interframes until the next IDR/keyframe. Its stdout is bare
Annex B H.264, so it can also feed another decoder. Startup may take several seconds
while waiting for a keyframe. Python and FFmpeg are not dependencies of the C# relay.

The verified setup payload, recovered from the initial packet in `/tmp/micsig.log`, is:

```text
000000016742401fa680c8137e540000000168ce3880
```

It describes Constrained Baseline, 800×600, YUV420P video. These bytes are specific to
the observed encoder configuration; replace them if firmware or video settings change.
The helper assumes the observed complete Annex B NAL units within video packets.

An offline framing/startup check needs no scope or decoder:

```sh
python3 utility/test_scope_video.py
```

## Build

From the repository root:

```sh
dotnet build src/scopecreep/relay/relay.csproj
```

For a self-contained single executable:

```sh
dotnet publish src/scopecreep/relay/relay.csproj -c Release -r linux-x64
```

The published executable is
`src/scopecreep/relay/bin/Release/net10.0/linux-x64/publish/scopecreep-relay`.
The relay needs no external utilities or NuGet libraries at runtime.

When ready to replace the existing direct scope connection, stop that connection and
start the published executable. Point the later MCP client at `127.0.0.1:8888`, reading
video packets and writing binary control events on that same socket. Restarting that
client leaves the relay's scope connection alive. A scope reconnect may require renewed
on-device approval.

## Protocol reference

The user's reverse engineering identified the control layout as three uint32 values in
little-endian order: `event`, `x`, `y`. Coordinates address the 800×600 display.

| Event | Value |
|---|---|
| Left down | `0x101` |
| Move with left held | `0x101` |
| Right down | `0x201` |
| Either button up | `0x001` |
| Wheel | `0x401 \| (delta << 16)` |

Input encoding was recovered from `RemoteDisplay.exe`. Left-button dragging is now
hardware verified: moving Y2 down 50 pixels changed its readout from 4.580 V to 3.580 V,
then it was restored to 4.580 V. See the
[before/after evidence](../diagnostics/remote-relay-2026-10-17.md#mouse-movement-proof).
The Windows client's move handler sends `(heldButtons << 8) | 1` while a button is
held; it sends no hover movement. Left down and subsequent held movement therefore
use the same event value. Right-button and wheel behavior remain unverified on hardware.
The relay deliberately forwards input without synthesizing events.

Unused discovery reference: the scope broadcasts 88-byte UDP packets to
`255.255.255.255:12953`, with magic `22 11 55 AA` at offset 0, ASCII model at `0x08`,
ASCII device identifier at `0x28`, and uint32 little-endian width/height at `0x48`/`0x4C`.
No additional TCP authentication or handshake was observed beyond on-device approval.
