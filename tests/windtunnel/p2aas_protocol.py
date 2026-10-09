#!/usr/bin/env python3
"""Optional differential P2AAS checks. Assemble the two fixtures first; pass either server URL."""
import argparse
import asyncio
import base64
import pathlib
import struct
import time
import urllib.parse

import websockets


def address(endpoint, **parameters):
    url = urllib.parse.urlsplit(endpoint)
    query = dict(urllib.parse.parse_qsl(url.query, keep_blank_values=True))
    query.update(parameters)
    return urllib.parse.urlunsplit(url._replace(query=urllib.parse.urlencode(query)))


async def take(socket, count):
    data = bytearray()
    while len(data) < count:
        part = await asyncio.wait_for(socket.recv(), 15)
        assert isinstance(part, bytes), "UART output must be binary"
        data.extend(part)
    assert len(data) == count, (count, data)
    return bytes(data)


async def check_close(socket, code, reason=None):
    try:
        while True:
            await asyncio.wait_for(socket.recv(), 15)
    except websockets.ConnectionClosed as close:
        assert close.rcvd is not None and close.sent is not None, close
        assert close.rcvd.code == code, close
        if reason is not None:
            assert close.rcvd.reason == reason, close


async def invalid_requests(endpoint):
    queries = (
        {"baudrate": "0"}, {"baudrate": "-1"}, {"baudrate": "2147483648"},
        {"baudrate": "115_200"}, {"timeout_ms": "1_000"},
        {"timeout_ms": "99"}, {"timeout_ms": "10001"}, {"timeout_ms": "no"},
        {"code": "!"}, {"code": "AA=="},
    )
    for query in queries:
        try:
            async with websockets.connect(address(endpoint, **query)):
                raise AssertionError(f"Upgraded invalid request: {query}")
        except websockets.InvalidStatus as error:
            assert error.response.status_code == 400, error
    for length in (0, 1, 3, 524292):
        async with websockets.connect(address(endpoint, timeout_ms="100")) as socket:
            await socket.send(struct.pack("<I", length) + (b"x" * length if length < 4 else b""))
            await check_close(socket, 1002)
    async with websockets.connect(address(endpoint, timeout_ms="100")) as socket:
        await socket.send("not binary")
        await check_close(socket, 1002, "Expected binary websocket messages.")
    print("HTTP validation and upload errors passed", flush=True)


async def terminal_sessions(endpoint, firmware):
    # One fixture covers both alphabets, fragmented frames, separate messages and one-message uploads.
    for mode in ("combined", "messages", "fragmented", "base64", "base64url"):
        query = {"timeout_ms": "1000"}
        if mode.startswith("base64"):
            encoder = base64.b64encode if mode == "base64" else base64.urlsafe_b64encode
            query["code"] = encoder(firmware).decode().rstrip("=")
        async with websockets.connect(address(endpoint, **query), max_size=None) as socket:
            prefix = struct.pack("<I", len(firmware))
            started = time.monotonic()
            if mode == "combined":
                await socket.send(prefix + firmware)
            elif mode == "messages":
                await socket.send(prefix[:2])
                await socket.send(prefix[2:])
                await socket.send(firmware)
            elif mode == "fragmented":
                await socket.send([prefix[:2], prefix[2:] + firmware[:1], firmware[1:]])
            assert await take(socket, 1) == b"!"
            ready = time.monotonic() - started
            await (await socket.ping(b"control traffic"))
            for packet in (b"\x00\x80\xff", b"ABC"):
                # Exercise several incoming chunks while a UART frame is already active.
                await socket.send(packet[:1].decode() if packet == b"ABC" else packet[:1])
                await socket.send(packet[1:])
                assert await take(socket, 3) == packet
            await check_close(socket, 1008, "No time quota left for user code.")
            print(f"{mode}: binary/text packets, pong, timeout handshake passed; ready {ready:.3f}s", flush=True)
    # Client closure must release ownership so a subsequent request can run immediately.
    async with websockets.connect(address(endpoint, timeout_ms="1000")) as socket:
        await socket.send(struct.pack("<I", len(firmware)) + firmware)
        assert await take(socket, 1) == b"!"
        await socket.close()
    print("Client close passed", flush=True)


async def deadlines(endpoint, firmware):
    async with websockets.connect(address(endpoint, timeout_ms="100")) as socket:
        await socket.send(b"\x04\x00")
        await check_close(socket, 1011, "The server experienced an unexpected error.")
    async with websockets.connect(address(endpoint, timeout_ms="10000")) as socket:
        await socket.send(struct.pack("<I", len(firmware)))
        await asyncio.sleep(0.2)  # Ensure total request quota expires before the runtime quota.
        await socket.send(firmware)
        assert await take(socket, 1) == b"!"
        await check_close(socket, 1008, "No time quota left.")
    print("Upload and total request deadline handshakes passed", flush=True)


async def loader_sessions(endpoint, firmware):
    assert len(firmware) % 4 == 0
    checksum = (0x706F7250 - sum(struct.unpack(f"<{len(firmware)//4}I", firmware))) & 0xFFFFFFFF
    full = bytearray(firmware.ljust(512 * 1024, b"\0"))
    struct.pack_into("<I", full, len(firmware), 0x12345678)
    # If checksum storage wrongly wraps, this harmless MOV still lets the probe report address zero.
    complement = (0x706F7250 - sum(struct.unpack(f"<{len(full)//4}I", full))) & 0xFFFFFFFF
    tail = (complement - 0xF6000000) & 0xFFFFFFFF
    struct.pack_into("<I", full, len(full) - 4, tail)
    for image, expected in ((full, (0, 0x12345678, 0, tail)), (firmware, (0, checksum, 0, 0))):
        async with websockets.connect(address(endpoint, timeout_ms="500"), max_size=None) as socket:
            await socket.send(struct.pack("<I", len(image)) + image)
            observed = struct.unpack("<4I", await take(socket, 16))
            assert observed == expected, (observed, expected)
            # All cogs have stopped; physical serial and simulator sessions both stay open.
            await check_close(socket, 1008, "No time quota left for user code.")
        print(f"Loader {len(image)} bytes: checksum, RAM reset, hub hole and stop behavior passed", flush=True)


async def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--url", default="ws://127.0.0.1:21591/")
    parser.add_argument("--terminal", type=pathlib.Path, required=True)
    parser.add_argument("--loader", type=pathlib.Path, required=True)
    args = parser.parse_args()
    await invalid_requests(args.url)
    await terminal_sessions(args.url, args.terminal.read_bytes())
    await loader_sessions(args.url, args.loader.read_bytes())
    await deadlines(args.url, args.terminal.read_bytes())
    print("P2AAS protocol checks passed")


if __name__ == "__main__":
    asyncio.run(main())
