#!/usr/bin/env python3
"""Assemble a Propan file and run it on the local P2AAS board."""

import asyncio
import struct
import subprocess
import sys
from pathlib import Path

import websockets


async def run(image: bytes) -> None:
    url = "ws://127.0.0.1:12880/?baudrate=115200&timeout_ms=5000"
    async with websockets.connect(url, max_size=None) as socket:
        await socket.send(struct.pack("<I", len(image)) + image)
        try:
            while True:
                chunk = await socket.recv()
                sys.stdout.buffer.write(chunk if isinstance(chunk, bytes) else chunk.encode())
                sys.stdout.buffer.flush()
        except websockets.ConnectionClosed:
            pass


def main() -> None:
    if len(sys.argv) != 2:
        raise SystemExit("usage: utility/p2aas-run.py FILE.propan")
    root = Path(__file__).resolve().parent.parent
    image = subprocess.check_output([str(root / "zig-out/bin/propan"), "-o", "-", sys.argv[1]])
    asyncio.run(run(image))


if __name__ == "__main__":
    main()
