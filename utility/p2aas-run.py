#!/usr/bin/env python3
"""Assemble a Propan file and run it on the local P2AAS board."""

import asyncio
import os
import struct
import subprocess
import sys
import urllib.parse
from pathlib import Path

import websockets


async def run(image: bytes) -> None:
    endpoint = urllib.parse.urlsplit(os.environ.get("P2AAS_ENDPOINT", "ws://127.0.0.1:12880/"))
    query = dict(urllib.parse.parse_qsl(endpoint.query, keep_blank_values=True))
    if "code" in query:
        raise ValueError("P2AAS_ENDPOINT must not select URL-code upload")
    query.setdefault("baudrate", "115200")
    query.setdefault("timeout_ms", "5000")
    url = urllib.parse.urlunsplit(endpoint._replace(query=urllib.parse.urlencode(query)))
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
