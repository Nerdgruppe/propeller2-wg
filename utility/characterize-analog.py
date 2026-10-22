"""Hardware-only two-board analog characterization; primary uses P2AAS exclusively."""

import argparse
import asyncio
import json
import os
import select
import struct
import subprocess
import termios
import time
from pathlib import Path

import websockets

ROOT = Path(__file__).resolve().parent.parent
HERE = ROOT / ".zig-cache" / "analog-characterization"
PORT = "/dev/serial/by-id/usb-Parallax_Inc_Propeller_P2-EVAL_PLX6ZJKWX-if00-port0"
ENDPOINT = "ws://localhost:12880/"


def serial_read(fd, length):
    """Read one exact observer response within ten seconds."""
    data = bytearray()
    deadline = time.monotonic() + 10
    while len(data) < length:
        if not select.select([fd], [], [], max(0, deadline - time.monotonic()))[0]:
            raise TimeoutError(f"observer: {len(data)}/{length} bytes")
        data.extend(os.read(fd, length - len(data)))
    return bytes(data)


def assemble():
    """Build the same monitor for both boards."""
    HERE.mkdir(parents=True, exist_ok=True)
    p = Path(__file__).with_name("analog-monitor.propan")
    subprocess.run(
        [str(ROOT / "zig-out/bin/propan"), "-o", str(HERE / "monitor.bin"), str(p)],
        check=True,
    )
    return (HERE / "monitor.bin").read_bytes()


class Rig:
    """Own two bounded transports with explicit experiment-pin cleanup."""

    async def __aenter__(self):
        """Load the bounded monitor, preserving exclusive P2AAS access to the DUT."""
        image = assemble()
        self.ws = None
        self.fd = None
        self.pending = bytearray()
        self.fd = os.open(PORT, os.O_RDWR | os.O_NOCTTY | os.O_NONBLOCK)
        await asyncio.to_thread(
            subprocess.run,
            [
                str(ROOT / "zig-out/bin/turboprop"),
                "-P",
                PORT,
                "-b",
                "2000000",
                str(HERE / "monitor.bin"),
            ],
            check=True,
            timeout=10,
        )
        a = termios.tcgetattr(self.fd)
        a[0] = a[1] = a[3] = 0
        a[2] = termios.CS8 | termios.CREAD | termios.CLOCAL
        a[4] = a[5] = termios.B2000000
        a[6][termios.VMIN] = a[6][termios.VTIME] = 0
        termios.tcsetattr(self.fd, termios.TCSANOW, a)
        assert await asyncio.to_thread(serial_read, self.fd, 1) == b"!"
        self.ws = await websockets.connect(
            ENDPOINT.rstrip("/") + "/?baudrate=2000000&timeout_ms=60000", max_size=None
        )
        await self.ws.send(struct.pack("<I", len(image)) + image)
        assert await self.receive(1) == b"!"
        return self

    async def receive(self, n):
        """Read exactly n primary UART bytes."""
        while len(self.pending) < n:
            self.pending.extend(await asyncio.wait_for(self.ws.recv(), 10))
        out = bytes(self.pending[:n])
        del self.pending[:n]
        return out

    async def call(self, board, cmd, args=(), size=1):
        """Issue one monitor RPC to either board."""
        data = cmd.encode() + struct.pack("<" + "I" * len(args), *args)
        if board == 0:
            await self.ws.send(data)
            out = await self.receive(size)
        else:
            os.write(self.fd, data)
            out = await asyncio.to_thread(serial_read, self.fd, size)
        if size == 1:
            assert out == b".", out
        return struct.unpack("<" + "I" * (size // 4), out) if size > 1 else None

    async def config(self, b, p, c, x=0, y=0, out=0, dir=0):
        """Configure a pin, beginning with its output disabled."""
        await self.call(b, "C", (p, c, x, y, out, dir))

    async def measure(self, b, p):
        """Average 64 completed automatic SINC2 conversions after three warmups."""
        return (await self.call(b, "M", (p,), 4))[0] / 64 / 16384

    async def __aexit__(self, *exc):
        """Release both boards even after experiment errors."""
        for b in (0, 1):
            try:
                await self.call(b, "F")
            except (
                OSError,
                TimeoutError,
                AssertionError,
                websockets.ConnectionClosed,
            ) as e:
                print("cleanup", b, repr(e), flush=True)
        if self.ws:
            await self.ws.close()
        if self.fd is not None:
            os.close(self.fd)


async def characterize_transfer():
    """Calibrate ADC transfer and DAC voltage/load behavior in both directions."""
    rows = []
    for direction in (0, 1):
        async with Rig() as r:
            src = direction
            obs = 1 - src
            for p in range(8, 12):
                await r.config(obs, p, 0x118030, 13, dir=1)
            for code in (0, 64, 128, 192, 255):
                for p in range(8, 12):
                    await r.config(src, p, 0x160000 | (code << 8), dir=1)
                vals = [await r.measure(obs, p) for p in range(8, 12)]
                rows.append(
                    {
                        "kind": "calibration",
                        "direction": direction,
                        "code": code,
                        "values": vals,
                    }
                )
                print(rows[-1], flush=True)
            for gain in range(8):
                for p in range(8, 12):
                    await r.config(obs, p, 0x100030 | (gain << 15), 13, dir=1)
                for code in (0, 32, 64, 128, 192, 224, 255):
                    for p in range(8, 12):
                        await r.config(src, p, 0x160000 | (code << 8), dir=1)
                    vals = [await r.measure(obs, p) for p in range(8, 12)]
                    rows.append(
                        {
                            "kind": "adc",
                            "direction": direction,
                            "gain": gain,
                            "code": code,
                            "values": vals,
                        }
                    )
                    (HERE / "transfer.json").write_text(json.dumps(rows))
                print("ADC", direction, gain, rows[-1]["values"], flush=True)
            for p in range(8, 12):
                await r.config(obs, p, 0x118030, 13, dir=1)
            for rg in range(4):
                for code in (0, 32, 64, 128, 192, 224, 255):
                    for p in range(8, 12):
                        await r.config(
                            src, p, 0x140000 | (rg << 16) | (code << 8), dir=1
                        )
                    for load in (0, 1, 2):
                        for p in range(8, 12):
                            await r.config(
                                obs,
                                p,
                                0x118030 | 0x900 | (0x40 if load else 0),
                                13,
                                out=(load == 2),
                                dir=1,
                            )
                        vals = [await r.measure(obs, p) for p in range(8, 12)]
                        rows.append(
                            {
                                "kind": "dac",
                                "direction": direction,
                                "range": rg,
                                "code": code,
                                "load": load,
                                "values": vals,
                            }
                        )
                        (HERE / "transfer.json").write_text(json.dumps(rows))
                print("DAC", direction, rg, rows[-1]["values"], flush=True)
        (HERE / "transfer.json").write_text(json.dumps(rows))
    print("rows", len(rows), flush=True)


async def characterize_receivers():
    """Measure receiver hysteresis with a constant-impedance DAC stimulus."""
    rows = []
    for direction in (0, 1):
        async with Rig() as r:
            dut = direction
            stim = 1 - dut
            await r.config(stim, 9, 0x168000, dir=1)
            for mode in (0, 0x60000, 0xC0000, 0x188000, 0x198000):
                await r.config(dut, 8, mode)
                for reverse in (False, True):
                    vals = []
                    for code in range(255, -1, -1) if reverse else range(256):
                        await r.config(stim, 8, 0x160000 | (code << 8), dir=1)
                        vals.append((await r.call(dut, "R", (8,), 8))[1] & 2 != 0)
                    rows.append(
                        {
                            "kind": "receiver",
                            "direction": direction,
                            "mode": mode,
                            "reverse": reverse,
                            "values": vals,
                        }
                    )
                    print(
                        direction,
                        hex(mode),
                        reverse,
                        [i for i in range(1, 256) if vals[i] != vals[i - 1]],
                        flush=True,
                    )
                    (HERE / "receivers.json").write_text(json.dumps(rows))
            for p, x in zip(range(8, 12), (0x12, 0x34, 0x56, 0x78)):
                await r.config(dut, p, 2, dir=1)
                await r.call(dut, "X", (p, x))
            for config in (0x48, 8, 0, 0x48, 0x4C):
                value = (await r.call(dut, "S", (config,), 4))[0]
                rows.append(
                    {
                        "kind": "scope",
                        "direction": direction,
                        "config": config,
                        "value": value,
                    }
                )
                print("scope", hex(config), hex(value), flush=True)
        (HERE / "receivers.json").write_text(json.dumps(rows))


async def characterize_dac_feedback():
    """Measure remaining analog families before any simulator implementation."""
    rows = []
    for direction in (0, 1):
        async with Rig() as r:
            src = direction
            obs = 1 - src
            for p in range(8, 12):
                await r.config(obs, p, 0x118030, 13, dir=1)
            for code in range(256):
                for rg, p in enumerate(range(8, 12)):
                    await r.config(src, p, 0x140000 | (rg << 16) | (code << 8), dir=1)
                vals = [await r.measure(obs, p) for p in range(8, 12)]
                rows.append(
                    {
                        "kind": "all_codes",
                        "direction": direction,
                        "code": code,
                        "values": vals,
                    }
                )
                if code % 32 == 0:
                    print("codes", direction, code, flush=True)
                (HERE / "remaining.json").write_text(json.dumps(rows))
        async with Rig() as r:
            for p in range(8, 12):
                await r.config(obs, p, 0x118030, 13, dir=1)
            for mode in (1, 2, 3):
                for y in (0, 0x1234, 0x8000, 0xFF00, 0xFFFF):
                    await r.config(src, 8, 0x160040 | (mode << 1), 256, y, dir=1)
                    raw = await r.measure(obs, 8)
                    read = await r.call(src, "R", (8,), 8)
                    rows.append(
                        {
                            "kind": "smart_dac",
                            "direction": direction,
                            "mode": mode,
                            "y": y,
                            "value": raw,
                            "read": read,
                        }
                    )
                    print(rows[-1], flush=True)
            for mode in (1, 2, 4, 5, 7, 13, 14, 15):
                for sync in (0, 1):
                    for initial in (0, 1):
                        # Establish the starting charge externally, then relinquish the stimulus.
                        await r.config(src, 10, 0)
                        await r.config(obs, 10, 0x900, out=initial, dir=1)
                        await r.config(
                            src,
                            10,
                            (mode << 17) | (sync << 16) | 0x8000 | 0x1200,
                            dir=1,
                        )
                        await r.config(obs, 10, 0x118030, 13, dir=1)
                        value = await r.measure(obs, 10)
                        rows.append(
                            {
                                "kind": "feedback",
                                "direction": direction,
                                "mode": mode,
                                "sync": sync,
                                "initial": initial,
                                "value": value,
                            }
                        )
                        await r.config(src, 10, 0)
            print("feedback complete", direction, flush=True)
        (HERE / "remaining.json").write_text(json.dumps(rows))


async def characterize_mux():
    """Record source mux and accumulation behavior with an independent ADC observer."""
    rows = []
    async with Rig() as r:
        for p in range(8, 12):
            await r.config(1, p, 0x118030, 13, dir=1)
        await r.call(0, "D", (0xE0A06020,))
        for sel in range(16):
            for p in range(8, 12):
                await r.config(0, p, 0x160040 | (sel << 8), dir=1)
            rows.append(
                {
                    "kind": "select",
                    "sel": sel,
                    "values": [await r.measure(1, p) for p in range(8, 12)],
                }
            )
            print(rows[-1], flush=True)
        for tt in range(4):
            for out in (0, 1):
                for di in (0, 1):
                    await r.config(0, 8, 0x16A300 | (tt << 6), out=out, dir=di)
                    rows.append(
                        {
                            "kind": "bitdac",
                            "tt": tt,
                            "out": out,
                            "dir": di,
                            "value": await r.measure(1, 8),
                        }
                    )
        for mode in (1, 2, 3):
            for a in (0, 0x40000000):
                for out in (0, 1):
                    await r.config(
                        0, 8, 0x160040 | (mode << 1) | a, 256, 0x8000, out=out, dir=1
                    )
                    value = await r.measure(1, 8)
                    read = await r.call(0, "R", (8,), 8)
                    rows.append(
                        {
                            "kind": "accum",
                            "mode": mode,
                            "a": a,
                            "out": out,
                            "value": value,
                            "read": read,
                        }
                    )
                    print(rows[-1], flush=True)
        # The scope pipe retains its last data when disabled, independent of pin mode.
        for p, x in zip(range(8, 12), (0x12, 0x34, 0x56, 0x78)):
            await r.config(0, p, 2, dir=1)
            await r.call(0, "X", (p, x))
        for cfg in (0x48, 8, 0, 0x48, 0x4C):
            rows.append(
                {
                    "kind": "scope",
                    "config": cfg,
                    "value": (await r.call(0, "S", (cfg,), 4))[0],
                }
            )
            print(rows[-1], flush=True)
    (HERE / "selection.json").write_text(json.dumps(rows))


async def characterize_combinations():
    """Measure DAC and ADC routing for non-DAC smart engines across TT and OUT."""
    rows = []
    async with Rig() as r:
        await r.config(1, 8, 0x118030, 13, dir=1)
        for mode in (4, 6, 8, 24, 25, 26, 28, 29, 30, 31):
            for tt in range(4):
                for out in (0, 1):
                    await r.config(0, 8, 0, out=0, dir=1)
                    await r.config(
                        0,
                        8,
                        0x16A300 | (mode << 1) | (tt << 6),
                        x=2 if mode == 26 else 3,
                        out=out,
                        dir=1,
                    )
                    rows.append(
                        {
                            "mode": mode,
                            "tt": tt,
                            "out": out,
                            "voltage": await r.measure(1, 8),
                            "read": await r.call(0, "R", (8,), 8),
                        }
                    )
                    print(rows[-1], flush=True)
                    (HERE / "combinations.json").write_text(json.dumps(rows))


def main():
    """Run an optional hardware campaign without changing simulator state or expectations."""
    global HERE, PORT, ENDPOINT
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "campaign",
        choices=("transfer", "receivers", "dac-feedback", "mux", "combinations"),
    )
    parser.add_argument("--artifacts", type=Path, default=HERE)
    parser.add_argument("--observer-port", default=PORT)
    parser.add_argument("--endpoint", default=ENDPOINT)
    args = parser.parse_args()
    HERE, PORT, ENDPOINT = args.artifacts.resolve(), args.observer_port, args.endpoint
    HERE.mkdir(parents=True, exist_ok=True)
    campaigns = {
        "transfer": characterize_transfer,
        "receivers": characterize_receivers,
        "dac-feedback": characterize_dac_feedback,
        "mux": characterize_mux,
        "combinations": characterize_combinations,
    }
    asyncio.run(campaigns[args.campaign]())


if __name__ == "__main__":
    main()
