"""Validate serial RX using an independent P2 on the existing P8–P15 connections."""
import asyncio
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import struct
import subprocess
import termios

import websockets

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
PORT = '/dev/serial/by-id/usb-Parallax_Inc_Propeller_P2-EVAL_PLX6ZJKWX-if00-port0'
SPEC = importlib.util.spec_from_file_location('digital_external', HERE.parent / 'digital-smart-pins/external.py')
DIGITAL = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(DIGITAL)


async def main():
    """Run finite full-width UART and USART words, then release both boards even on failure."""
    images = {}
    for name in ('peer', 'external'):
        images[name] = subprocess.check_output(
            [str(ROOT / 'zig-out/bin/propan'), '-o', '-', str(HERE / f'{name}.propan')])
    peer_image = Path('/tmp/p2-serial-peer.bin')
    peer_image.write_bytes(images['peer'])
    fd = os.open(PORT, os.O_RDWR | os.O_NOCTTY | os.O_NONBLOCK)
    rows = []
    try:
        subprocess.run([str(ROOT / 'zig-out/bin/turboprop'), '-P', PORT, '-b', '2000000', str(peer_image)],
                       check=True, timeout=8)
        attrs = termios.tcgetattr(fd)
        attrs[0] = attrs[1] = attrs[3] = 0
        attrs[2] = termios.CS8 | termios.CREAD | termios.CLOCAL
        attrs[4] = attrs[5] = termios.B2000000
        attrs[6][termios.VMIN] = attrs[6][termios.VTIME] = 0
        termios.tcsetattr(fd, termios.TCSANOW, attrs)
        assert await asyncio.to_thread(DIGITAL.read_serial, fd) == b'!'

        async def peer(command):
            """Send one peer command and require its completed acknowledgement."""
            os.write(fd, command)
            assert await asyncio.to_thread(DIGITAL.read_serial, fd) == b'.'

        try:
            async with websockets.connect('ws://localhost:12880/?baudrate=2000000&timeout_ms=50000') as ws:
                pending = bytearray()

                async def primary(command=None, length=1):
                    """Exchange a finite command through P2AAS without opening the primary serial device."""
                    if command is not None:
                        await ws.send(command)
                    while len(pending) < length:
                        pending.extend(await asyncio.wait_for(ws.recv(), 2))
                    result = bytes(pending[:length])
                    del pending[:length]
                    return result

                async def configure(mode, width):
                    """Reset the receiver with its exact physical routing and an explicit live X value."""
                    x = (1024 << 16) | (width - 1) if mode == 31 else width - 1
                    assert await primary(b'C' + bytes([mode]) + struct.pack('<I', x)) == b'.'

                async def observe(label, width, word, ready=1):
                    """Compare meaningful MSB-justified data bits and the ready latch with the intended word."""
                    result, carry, actual_ready = struct.unpack('<IBB', await primary(b'R', 6))
                    mask = (0xffffffff << (32 - width)) & 0xffffffff
                    expected = (word << (32 - width)) & 0xffffffff
                    passed = actual_ready == ready and (not ready or result & mask == expected)
                    rows.append(dict(case=label, width=width, result=result, carry=carry,
                                     ready=actual_ready, expected=expected, passed=passed))

                image = images['external']
                await ws.send(struct.pack('<I', len(image)) + image)
                assert await primary() == b'!'
                try:
                    for mode in (29, 31):
                        for width in range(1, 33):
                            word = 0x89abcdef ^ (width * 0x01010101)
                            await peer(b'W' + bytes([1 if mode == 31 else 0]))
                            await configure(mode, width)
                            if mode == 31:
                                await peer(b'U' + bytes([width]) + struct.pack('<I', word))
                            else:
                                for bit in range(width):
                                    data = (word >> bit) & 1
                                    await peer(b'W' + bytes([data]))
                                    await peer(b'W' + bytes([data | 2]))
                            await observe(f'mode{mode}-width{width}', width, word)
                        print(f'Mode {mode}: checked all 32 widths', flush=True)
                    # RMII-style extension after four samples preserves the partial word.
                    await peer(b'W\x00')
                    await configure(29, 8)
                    word = 0x89abcdef
                    for bit in range(16):
                        if bit == 4:
                            assert await primary(b'X' + struct.pack('<I', 15)) == b'.'
                        data = (word >> bit) & 1
                        await peer(b'W' + bytes([data]))
                        await peer(b'W' + bytes([data | 2]))
                    await observe('live-8-to-16', 16, word)
                finally:
                    try:
                        assert await primary(b'F') == b'.'
                    except websockets.exceptions.ConnectionClosed:
                        # The RAM program's watchdog also releases the receiver after a disconnected host.
                        pass
        finally:
            await peer(b'F')
    finally:
        os.close(fd)
    report = dict(images={name: hashlib.sha256(image).hexdigest() for name, image in images.items()},
                  primary='P2AAS', secondary=PORT, clock_hz=200000000, cases=rows)
    (HERE / 'evidence/external.json').write_text(json.dumps(report, indent=2) + '\n')
    assert all(row['passed'] for row in rows), [row for row in rows if not row['passed']]
    print(f'{len(rows)}/{len(rows)} external serial observations passed; both boards released.')


if __name__ == '__main__':
    asyncio.run(main())
