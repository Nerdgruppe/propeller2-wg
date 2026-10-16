"""Two-board USB characterization; firmware times probes, and the DUT uses P2AAS only."""

import argparse
import asyncio
import itertools
import json
import os
import select
import struct
import subprocess
import termios
import time
from pathlib import Path

try:
    import websockets
except ImportError:
    websockets = None

ROOT = Path(__file__).resolve().parent.parent
HERE = ROOT / ".zig-cache" / "usb-characterization"
PORT = "/dev/serial/by-id/usb-Parallax_Inc_Propeller_P2-EVAL_PLX6ZJKWX-if00-port0"
ENDPOINT = "ws://localhost:12880/"


def serial_read(fd, length):
    """Receive an exact observer reply with a bounded timeout."""
    data = bytearray()
    deadline = time.monotonic() + 15
    while len(data) < length:
        if not select.select([fd], [], [], max(0, deadline - time.monotonic()))[0]:
            raise TimeoutError(f"observer: {len(data)}/{length} bytes")
        data.extend(os.read(fd, length - len(data)))
    return bytes(data)


class Rig:
    """Own the two board transports and release experiment pins after every campaign."""

    async def __aenter__(self):
        """Assemble the USB-only monitor and load it through each board's designated transport."""
        if websockets is None:
            raise RuntimeError(
                "Hardware characterization needs the existing optional websockets package"
            )
        HERE.mkdir(parents=True, exist_ok=True)
        image_path = HERE / "monitor.bin"
        await asyncio.to_thread(
            subprocess.run,
            [
                str(ROOT / "zig-out/bin/propan"),
                "-o",
                str(image_path),
                str(Path(__file__).with_name("usb-monitor.propan")),
            ],
            check=True,
        )
        self.ws, self.fd = None, None
        self.pending = bytearray()
        try:
            self.fd = os.open(PORT, os.O_RDWR | os.O_NOCTTY | os.O_NONBLOCK)
            await asyncio.to_thread(
                subprocess.run,
                [
                    str(ROOT / "zig-out/bin/turboprop"),
                    "-P",
                    PORT,
                    "-b",
                    "2000000",
                    str(image_path),
                ],
                check=True,
                timeout=10,
            )
            settings = termios.tcgetattr(self.fd)
            settings[0] = settings[1] = settings[3] = 0
            settings[2] = termios.CS8 | termios.CREAD | termios.CLOCAL
            settings[4] = settings[5] = termios.B2000000
            settings[6][termios.VMIN] = settings[6][termios.VTIME] = 0
            termios.tcsetattr(self.fd, termios.TCSANOW, settings)
            assert await asyncio.to_thread(serial_read, self.fd, 1) == b"!"
            self.ws = await websockets.connect(
                ENDPOINT.rstrip("/") + "/?baudrate=2000000&timeout_ms=60000",
                max_size=None,
            )
            image = image_path.read_bytes()
            await self.ws.send(struct.pack("<I", len(image)) + image)
            assert await self.receive(1) == b"!"
            return self
        except BaseException:
            await self.__aexit__(None, None, None)
            raise

    async def receive(self, size):
        """Receive an exact primary reply with a bounded timeout."""
        while len(self.pending) < size:
            chunk = await asyncio.wait_for(self.ws.recv(), 15)
            self.pending.extend(chunk)
        result = bytes(self.pending[:size])
        del self.pending[:size]
        return result

    async def exchange(self, board, data, size):
        """Exchange a binary command through P2AAS or the observer's serial port."""
        if board == 0:
            await self.ws.send(data)
            return await self.receive(size)
        os.write(self.fd, data)
        return await asyncio.to_thread(serial_read, self.fd, size)

    async def call(self, board, command, args=(), size=1):
        """Encode little-endian monitor parameters and validate its reply."""
        data = command.encode() + struct.pack("<" + "I" * len(args), *args)
        result = await self.exchange(board, data, size)
        if size == 1:
            assert result == b".", result
            return None
        return struct.unpack("<" + "I" * (size // 4), result)

    async def config(self, board, pin, mode, x=0, y=0, out=0, dir=0):
        """Configure one pin while initially holding its smart engine in reset."""
        await self.call(board, "C", (pin, mode, x, y, out, dir))

    async def script(self, board, records):
        """Execute register operations in firmware and return cycle-tagged observations."""
        assert 0 < len(records) <= 256
        args = (len(records), *(value for row in records for value in row))
        data = await self.call(board, "E", args, len(records) * 16)
        return [list(data[i : i + 4]) for i in range(0, len(data), 4)]

    async def packet(self, board, values):
        """Feed a buffered packet from firmware using the upper pin's readiness handshake."""
        assert 0 < len(values) <= 256
        data = await self.call(board, "P", (len(values), *values), 16)
        return [list(data[i : i + 4]) for i in range(0, len(data), 4)]

    async def capture(self, board):
        """Retrieve raw GPIO samples and the simultaneous USB status timeline."""
        data = await self.exchange(board, b"H", 8196)
        stamps = struct.unpack("<1024I", data[4100:])
        return {
            "start": struct.unpack("<I", data[:4])[0],
            "wire": list(data[4:4100]),
            "status": [list(stamps[i : i + 4]) for i in range(0, 1024, 4)],
        }

    async def inject(self, board, samples):
        """Play raw pair states at one byte per clock with a shared trigger lead-in."""
        assert 0 < len(samples) <= 16384
        data = (
            b"I"
            + struct.pack("<I", len(samples))
            + bytes(value | 16 for value in samples)
        )
        assert await self.exchange(board, data, 1) == b"."

    async def __aexit__(self, *exc):
        """Release both boards and close transports, including on campaign failures."""
        for board, available in ((0, self.ws is not None), (1, self.fd is not None)):
            if available:
                try:
                    await self.call(board, "F")
                except (
                    OSError,
                    TimeoutError,
                    AssertionError,
                    websockets.ConnectionClosed,
                ) as error:
                    print("cleanup", board, repr(error), flush=True)
        if self.ws is not None:
            await self.ws.close()
        if self.fd is not None:
            os.close(self.fd)


async def usb(rig, board, x, drive=True, dirs=3, modes=None):
    """Reset a pair, configure its mode/rate, then release the selected DIR bits."""
    await rig.call(board, "F")
    modes = modes or [0x76 if drive else 0x36] * 2
    await rig.config(board, 8, modes[0], x)
    await rig.config(board, 9, modes[1])
    records = [(5, 8, 0, 0)] if dirs & 1 else []
    if dirs & 2:
        records.append((5, 9, 0, 0))
    records.append((0, 8, 0, 1024))
    return await rig.script(board, records)


async def smoke():
    """Check transports, baseline status and independent capture of one packet."""
    async with Rig() as rig:
        for board in (0, 1):
            print("startup", board, await usb(rig, board, 0x9000), flush=True)
        await usb(rig, 0, 0x1000)
        await rig.call(1, "F")
        await rig.config(0, 12, 0, dir=1)
        await rig.call(1, "G")
        tx = await rig.packet(0, [0x80, 0, 0xFF, 0x55, 0x80])
        capture = await rig.capture(1)
        (HERE / "smoke.json").write_text(json.dumps({"tx": tx, "capture": capture}))
        print("tx", tx, "wire_levels", sorted(set(capture["wire"])), flush=True)
        print(
            "edges",
            [
                (i, v & 3)
                for i, v in enumerate(capture["wire"])
                if i and v & 3 != capture["wire"][i - 1] & 3
            ][:50],
            flush=True,
        )


def retain(name, record):
    """Save one raw measurement immediately, before running another experiment."""
    (HERE / (name + ".json")).write_text(json.dumps(record))
    print(
        name,
        {
            key: value
            for key, value in record.items()
            if key not in ("captures", "capture", "samples", "observations")
        },
        flush=True,
    )


async def capture_packet(rig, source, observer, values, both=False):
    """Arm independent captures before a firmware-timed USB command sequence."""
    await rig.config(source, 12, 0, dir=1)
    boards = (source, observer) if both else (observer,)
    for board in boards:
        await rig.call(board, "G")
    tx = await rig.packet(source, values)
    captures = {str(board): await rig.capture(board) for board in boards}
    return tx, captures


async def calibrate(rig, source):
    """Measure a 72-bit zero packet in the peer clock domain to match baud generators."""
    observer = 1 - source
    await usb(rig, source, 0x1000)
    await rig.call(observer, "F")
    tx, caps = await capture_packet(rig, source, observer, [0x80] + [0] * 8)
    wire = [v & 3 for v in caps[str(observer)]["wire"]]
    start = next(i for i in range(1, len(wire)) if wire[i] == 2 and wire[i - 1] != 2)
    stop = next(i for i in range(start + 100, len(wire)) if wire[i] == 0)
    period = (stop - start) / 72
    rate = round(65536 / period)
    retain(
        f"clock-{source}",
        {
            "source": source,
            "period": period,
            "receiver_increment": rate,
            "tx": tx,
            "captures": caps,
        },
    )
    return rate


async def basics():
    """Sweep electrical state commands, handshakes, partial configuration and DIR/TT gates."""
    for source in (0, 1):
        async with Rig() as rig:
            await rig.call(1 - source, "F")
            rows = []
            for host in (0, 1):
                for speed in (0, 1):
                    for tt in range(4):
                        for dirs in range(4):
                            x = (host << 15) | (speed << 14) | 0x1000
                            modes = [0x36 | (tt << 6)] * 2
                            startup = await usb(rig, source, x, dirs=dirs, modes=modes)
                            sequence = [
                                (3, 8, value, 512) for value in (0, 1, 2, 3, 4, 0)
                            ]
                            result = await rig.script(source, sequence)
                            rows.append(
                                {
                                    "host": host,
                                    "speed": speed,
                                    "tt": tt,
                                    "dirs": dirs,
                                    "startup": startup,
                                    "result": result,
                                }
                            )
            for modes in ([0x76, 0], [0, 0x76], [0x76, 0x36], [0x36, 0x76]):
                startup = await usb(rig, source, 0x1000, modes=modes)
                result = await rig.script(
                    source, [(3, 8, v, 512) for v in (0, 1, 2, 3, 4, 0)]
                )
                rows.append({"modes": modes, "startup": startup, "result": result})
            retain(f"basics-{source}", {"source": source, "observations": rows})


async def packets():
    """Compare TX waveforms, both-board receivers and cross-byte stuffing at matched rates."""
    values = [
        [],
        [0],
        [255],
        [0x80],
        [0xFF, 0xFF],
        [0x3F, 0xFC],
        [0, 0xFF, 0x55, 0x80],
        [0xD3, 0x17, 0xA9, 0x62],
    ]
    for source in (0, 1):
        for speed in (0, 1):
            for host in (0, 1):
                async with Rig() as rig:
                    rate = await calibrate(rig, source)
                    for index, data in enumerate(values):
                        await usb(rig, source, (host << 15) | (speed << 14) | 0x1000)
                        await usb(
                            rig, 1 - source, ((1 - host) << 15) | (speed << 14) | rate
                        )
                        tx, caps = await capture_packet(
                            rig, source, 1 - source, [0x80] + data, both=True
                        )
                        retain(
                            f"packet-{source}-{speed}-{host}-{index}",
                            {
                                "source": source,
                                "speed": speed,
                                "host": host,
                                "data": data,
                                "rate": rate,
                                "tx": tx,
                                "captures": caps,
                            },
                        )
            async with Rig() as rig:
                for index, data in enumerate(values):
                    await usb(rig, source, (speed << 14) | 0x1000)
                    await rig.call(1 - source, "F")
                    tx, caps = await capture_packet(
                        rig, source, 1 - source, [0x80] + data
                    )
                    retain(
                        f"raw-{source}-{speed}-{index}",
                        {
                            "source": source,
                            "speed": speed,
                            "data": data,
                            "tx": tx,
                            "captures": caps,
                        },
                    )


def encoded(data, speed=0, stuff=True, sync=True):
    """Construct independently specified NRZI line stimuli, optionally omitting stuffed zeros."""
    j = 2 if speed else 1
    state = j
    line = [j] * 16
    if sync:
        line += [j ^ 3, j, j ^ 3, j, j ^ 3, j, j ^ 3, j ^ 3]
        state = j ^ 3
    ones = 1 if sync else 0
    for value in data:
        for bit in range(8):
            high = value & (1 << bit) != 0
            if not high:
                state ^= 3
                ones = 0
            else:
                ones += 1
            line.append(state)
            if stuff and ones == 6:
                state ^= 3
                line.append(state)
                ones = 0
    return line


async def inject_capture(rig, source, receiver, line, period):
    """Capture physical input and USB status on the receiving board for one raw stimulus."""
    await rig.call(source, "F")
    await rig.config(source, 12, 0, dir=1)
    await rig.call(receiver, "G")
    samples = [state for state in line for _ in range(period)]
    await rig.inject(source, samples)
    cap = await rig.capture(receiver)
    return samples, cap


async def receive():
    """Inject legal and malformed packets, steady states and EOP boundaries into both receivers."""
    for source in (0, 1):
        async with Rig() as rig:
            rate = await calibrate(rig, source)
            receiver = 1 - source
            for speed in (0, 1):
                j = 2 if speed else 1
                stimuli = [
                    (f"state-{state}", [j] * 16 + [state] * 12 + [j] * 32)
                    for state in range(4)
                ]
                stimuli += [
                    (f"eop-{length}", encoded([0x5A], speed) + [0] * length + [j] * 32)
                    for length in range(1, 7)
                ]
                stimuli += [
                    (
                        "no-stuff",
                        encoded([0xFF, 0xFF], speed, stuff=False) + [0] * 2 + [j] * 32,
                    )
                ]
                stimuli += [
                    (
                        "valid",
                        encoded([0, 0xFF, 0xFF, 0x55], speed) + [0] * 2 + [j] * 32,
                    )
                ]
                stimuli += [
                    (
                        "bad-sync",
                        [j] * 16
                        + [j ^ 3, j, j ^ 3, j ^ 3, j, j, j ^ 3, j ^ 3]
                        + encoded([0x5A], speed, sync=False)[16:]
                        + [0] * 2
                        + [j] * 32,
                    )
                ]
                for name, line in stimuli:
                    await usb(rig, receiver, (speed << 14) | rate, drive=False)
                    samples, cap = await inject_capture(rig, source, receiver, line, 16)
                    retain(
                        f"rx-{source}-{speed}-{name}",
                        {
                            "source": source,
                            "speed": speed,
                            "rate": rate,
                            "samples": samples,
                            "capture": cap,
                        },
                    )


async def lifecycle():
    """Record ACK behavior, FIFO overflow and active/idle reconfiguration on both boards."""
    for source in (0, 1):
        async with Rig() as rig:
            await rig.call(1 - source, "F")
            rows = []
            for writes in range(1, 7):
                await usb(rig, source, 0x8200)
                sequence = [(3, 8, 0x80, 0)] + [
                    (3, 8, 0x10 + i, 0) for i in range(writes)
                ]
                sequence += [(0, 8, 0, 24000)]
                rows.append(
                    {
                        "kind": "fifo",
                        "writes": writes,
                        "result": await rig.script(source, sequence),
                    }
                )
            for command in (
                (8, 9, 0),
                (2, 9, 123),
                (3, 9, 123),
                (1, 9, 0x76),
                (8, 8, 0),
                (2, 8, 0x9000),
                (3, 8, 0),
                (1, 8, 0x76),
            ):
                await usb(rig, source, 0x9000)
                result = await rig.script(
                    source, [(3, 8, 3, 256), (*command, 256), (0, 8, 0, 256)]
                )
                rows.append({"kind": "ack", "command": command, "result": result})
            for action in (
                (4, 8, 0),
                (4, 9, 0),
                (4, 72, 0),
                (1, 8, 0x76),
                (1, 9, 0x76),
                (1, 72, 0x36),
                (2, 8, 0xD000),
                (2, 9, 0xD000),
            ):
                for active in (False, True):
                    await usb(rig, source, 0x8200 if active else 0x9000)
                    sequence = (
                        [(3, 8, 0x80, 0), (3, 8, 0x5A, 0)]
                        if active
                        else [(3, 8, 3, 256)]
                    )
                    sequence += [(*action, 0), (0, 8, 0, 24000)]
                    rows.append(
                        {
                            "kind": "lifecycle",
                            "action": action,
                            "active": active,
                            "result": await rig.script(source, sequence),
                        }
                    )
            retain(f"lifecycle-{source}", {"source": source, "observations": rows})


async def electrical():
    """Use calibrated ADC observations and known loads to measure USB idle drive strengths."""
    for source in (0, 1):
        async with Rig() as rig:
            observer = 1 - source
            rows = []
            calibration = {}
            for pin in (8, 9):
                for name, mode in (("gio", 0x100030), ("vio", 0x108030)):
                    await rig.config(observer, pin, mode, 13, dir=1)
                    calibration[f"{pin}-{name}"] = (
                        (await rig.call(observer, "M", (pin,), 4))[0] / 64 / 16384
                    )
            for host in (0, 1):
                for speed in (0, 1):
                    for drive in (False, True):
                        await usb(
                            rig,
                            source,
                            (host << 15) | (speed << 14) | 0x1000,
                            drive=drive,
                        )
                        for load in ("float", "down1k5", "up1k5", "down15k", "up15k"):
                            mode = 0x118030
                            if load != "float":
                                mode |= 0x40 | (0x900 if "1k5" in load else 0x1200)
                            for pin in (8, 9):
                                await rig.config(
                                    observer,
                                    pin,
                                    mode,
                                    13,
                                    out=int(load.startswith("up")),
                                    dir=1,
                                )
                            values = [
                                (await rig.call(observer, "M", (pin,), 4))[0]
                                / 64
                                / 16384
                                for pin in (8, 9)
                            ]
                            rows.append(
                                {
                                    "host": host,
                                    "speed": speed,
                                    "drive": drive,
                                    "load": load,
                                    "values": values,
                                }
                            )
            retain(
                f"electrical-{source}",
                {"source": source, "calibration": calibration, "observations": rows},
            )


async def drives():
    """Measure active USB strength and reset-state drive gating with calibrated opposing loads."""
    for source in (0, 1):
        async with Rig() as rig:
            observer = 1 - source
            rows = []
            for host in (0, 1):
                for speed in (0, 1):
                    for dirs in range(4):
                        for state in (0, 1, 2, 3):
                            await usb(
                                rig,
                                source,
                                (host << 15) | (speed << 14) | 0x1000,
                                dirs=dirs,
                            )
                            await rig.script(source, [(3, 8, state, 256)])
                            for load in ("down", "up"):
                                for pin in (8, 9):
                                    await rig.config(
                                        observer,
                                        pin,
                                        0x118030 | 0x940,
                                        13,
                                        out=int(load == "up"),
                                        dir=1,
                                    )
                                values = [
                                    (await rig.call(observer, "M", (pin,), 4))[0]
                                    / 64
                                    / 16384
                                    for pin in (8, 9)
                                ]
                                rows.append(
                                    {
                                        "host": host,
                                        "speed": speed,
                                        "dirs": dirs,
                                        "state": state,
                                        "load": load,
                                        "values": values,
                                    }
                                )
            retain(f"drives-{source}", {"source": source, "observations": rows})


def export_vectors(destination):
    """Compact saved GPIO captures and independent silicon results into offline golden vectors."""
    receive_vectors, transmit_vectors = [], []
    for path in sorted(HERE.glob("*.json")):
        if not path.name.startswith(("rx-", "packet-")):
            continue
        record = json.loads(path.read_text())
        receiver = 1 - record["source"]
        capture = record.get("capture") or record["captures"][str(receiver)]
        wire = [(value >> 2) & 3 for value in capture["wire"]]
        runs = [[len(list(group)), level] for level, group in itertools.groupby(wire)]
        events = [
            value
            for value, _ in itertools.groupby(
                row[1] & 0xFF80 for row in capture["status"]
            )
        ]
        receive_vectors.append(
            {
                "name": path.stem,
                "rate": record["rate"],
                "speed": bool(record["speed"]),
                "wire": runs,
                "bytes": events,
                "final": capture["status"][-1][1] & 0xFFFF,
            }
        )
        if path.name.startswith("packet-"):
            capture = record["captures"][str(record["source"])]
            wire = [(value >> 2) & 3 for value in capture["wire"]]
            k = 1 if record["speed"] else 2
            start = next(
                i for i in range(1, len(wire)) if wire[i] == k and wire[i - 1] != k
            )
            end = next(i for i in range(start + 100, len(wire)) if wire[i] == 0)
            # The source's independent streamer samples its own clock, so NCO=$1000 is exactly 16 samples/bit.
            count = round((end - start) / 16) + 3
            transmit_vectors.append(
                {
                    "name": path.stem,
                    "speed": bool(record["speed"]),
                    "data": record["data"],
                    "symbols": [wire[start + 8 + bit * 16] for bit in range(count)],
                }
            )
    if not receive_vectors or not transmit_vectors:
        raise RuntimeError(
            "Run packets and receive into the same artifacts directory before exporting"
        )
    destination.mkdir(parents=True, exist_ok=True)
    for name, vectors in (
        ("usb-vectors.json", receive_vectors),
        ("usb-transmit-vectors.json", transmit_vectors),
    ):
        text = (
            "[\n"
            + ",\n".join(json.dumps(row, separators=(",", ":")) for row in vectors)
            + "\n]\n"
        )
        (destination / name).write_text(text)
    print(
        f"Exported {len(receive_vectors)} receiver and {len(transmit_vectors)} transmitter vectors"
    )


def boundary_probes():
    """Recreate measured transport discrepancies for further hardware characterization."""
    records = json.loads(
        Path(__file__).with_name("usb-transport-boundaries.json").read_text()
    )
    for record in records:
        fixture = record["fixture"]
        source = ROOT / "tests/windtunnel/state" / (fixture + ".propan")
        body = source.read_text().split("\n\n", 1)[1]
        lines = ["//? WINDTUNNEL CHECK LIST"]
        for run in record["runs"]:
            lines.append("//? run: " + json.dumps(run["name"]))
            lines.extend(
                f"//? const: {name} = {value}"
                for name, value in run["constants"].items()
            )
            lines.extend(
                f"//? post: cog[0].reg[{name}] == {value}"
                for name, value in zip(("result", "upper", "flags"), run["expected"])
            )
        path = HERE / (fixture + "-boundaries.propan")
        path.write_text("\n".join(lines) + "\n\n" + body)
        print(path)
    print(
        "These silicon expectations expose provisional transport limits; use --characterize to remeasure."
    )


def main():
    """Run optional hardware probes and retain captures outside tracked sources."""
    global HERE, PORT, ENDPOINT
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "campaign",
        choices=(
            "smoke",
            "basics",
            "packets",
            "receive",
            "lifecycle",
            "electrical",
            "drives",
            "export",
            "boundaries",
        ),
    )
    parser.add_argument("--artifacts", type=Path, default=HERE)
    parser.add_argument("--observer-port", default=PORT)
    parser.add_argument("--endpoint", default=ENDPOINT)
    parser.add_argument(
        "--golden-directory", type=Path, default=ROOT / "tests/windtunnel"
    )
    args = parser.parse_args()
    HERE, PORT, ENDPOINT = args.artifacts.resolve(), args.observer_port, args.endpoint
    HERE.mkdir(parents=True, exist_ok=True)
    if args.campaign == "export":
        export_vectors(args.golden_directory)
        return
    if args.campaign == "boundaries":
        boundary_probes()
        return
    asyncio.run(
        {
            "smoke": smoke,
            "basics": basics,
            "packets": packets,
            "receive": receive,
            "lifecycle": lifecycle,
            "electrical": electrical,
            "drives": drives,
        }[args.campaign]()
    )


if __name__ == "__main__":
    main()
