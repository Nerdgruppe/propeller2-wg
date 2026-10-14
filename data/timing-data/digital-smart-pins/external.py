"""Verify independent external GPIO inputs through the second board, without a shared clock."""
import asyncio
import hashlib
import json
import os
from pathlib import Path
import select
import struct
import subprocess
import termios

import websockets

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
PORT = '/dev/serial/by-id/usb-Parallax_Inc_Propeller_P2-EVAL_PLX6ZJKWX-if00-port0'


def read_serial(fd):
    """Read one peer response with an explicit timeout."""
    if not select.select([fd], [], [], 2)[0]:
        raise TimeoutError('peer response')
    return os.read(fd, 1)


async def main():
    """Check continuous event counts and missing-event alarms at both DUT clocks."""
    subprocess.run([str(ROOT / 'zig-out/bin/propan'), '-o', '/tmp/p2-smart-peer.bin',
                    str(HERE / 'peer.propan')], check=True)
    fd = os.open(PORT, os.O_RDWR | os.O_NOCTTY | os.O_NONBLOCK)
    try:
        subprocess.run([str(ROOT / 'zig-out/bin/turboprop'), '-P', PORT, '-b', '2000000',
                        '/tmp/p2-smart-peer.bin'], check=True, timeout=8)
        attrs = termios.tcgetattr(fd)
        attrs[0] = attrs[1] = attrs[3] = 0
        attrs[2] = termios.CS8 | termios.CREAD | termios.CLOCAL
        attrs[4] = attrs[5] = termios.B2000000
        attrs[6][termios.VMIN] = attrs[6][termios.VTIME] = 0
        termios.tcsetattr(fd, termios.TCSANOW, attrs)
        assert await asyncio.to_thread(read_serial, fd) == b'!'

        async def peer(value=None):
            """Atomically drive a source byte, or float the entire shared bank."""
            os.write(fd, b'F' if value is None else b'W' + bytes([value]))
            assert await asyncio.to_thread(read_serial, fd) == b'.'

        rows = []
        images = []
        try:
            for pll in range(2):
                for mode, y in [(11, 0), (12, 0), (13, 0), (14, 0), (14, 1), (18, 4)]:
                    await peer(0)
                    source = f'const MODE = {mode}\nconst Y = {y}\nconst PLL = {pll}\n' + (HERE / 'external.propan').read_text()
                    image = subprocess.check_output([str(ROOT / 'zig-out/bin/propan'), '-o', '-', '-'], input=source.encode())
                    images.append(dict(pll=pll, mode=mode, y=y, source=source, image_sha256=hashlib.sha256(image).hexdigest()))
                    async with websockets.connect('ws://localhost:12880/?baudrate=2000000&timeout_ms=5000') as ws:
                        pending = bytearray()

                        async def command(data=None, length=1):
                            """Exchange a bounded byte stream through P2AAS."""
                            if data is not None:
                                await ws.send(data)
                            while len(pending) < length:
                                pending.extend(await asyncio.wait_for(ws.recv(), 2))
                            result = bytes(pending[:length])
                            del pending[:length]
                            return result

                        await ws.send(struct.pack('<I', len(image)) + image)
                        marker = await command()
                        assert marker == b'!', (pll, mode, marker.hex())
                        try:
                            if mode == 18:
                                for driven, alarm in [(0, 1), (1, 0), (0, 1)]:
                                    await peer(driven)
                                    assert await command(b'A') == b'.'
                                    await asyncio.sleep(0.1)
                                    value, carry, ready = struct.unpack('<IBB', await command(b'R', 6))
                                    passed = ready == alarm and (value >= 1000000 if alarm else value <= 2)
                                    rows.append(dict(pll=pll, mode=mode, source=driven, value=value,
                                                     carry=carry, ready=ready, passed=passed))
                            else:
                                expected = previous = 0
                                for driven in [0, 1, 3, 2, 0, 2, 3, 1, 0] * 3:
                                    a, b = bool(driven & 1), bool(driven & 2)
                                    old_a, old_b = bool(previous & 1), bool(previous & 2)
                                    rise_a, rise_b = a and not old_a, b and not old_b
                                    if mode == 11:
                                        if a != old_a and b == old_b:
                                            expected += 1 if a != b else -1
                                        elif b != old_b and a == old_a:
                                            expected += 1 if a == b else -1
                                    elif mode == 12:
                                        expected += int(rise_a and b)
                                    elif mode == 13 and rise_a:
                                        expected += 1 if b else -1
                                    elif mode == 14:
                                        expected += int(rise_a) - int(bool(y) and rise_b)
                                    previous = driven
                                    await peer(driven)
                                    value, carry, ready = struct.unpack('<IBB', await command(b'R', 6))
                                    passed = value == expected & 0xffffffff and carry == int(expected < 0)
                                    rows.append(dict(pll=pll, mode=mode, y=y, source=driven,
                                                     expected=expected & 0xffffffff, value=value,
                                                     carry=carry, ready=ready, passed=passed))
                        finally:
                            assert await command(b'F') == b'.'
                    print(f'PLL={pll}, mode={mode}, Y={y}: checked', flush=True)
        finally:
            await peer()
        (HERE / 'evidence/external-images.json').write_text(json.dumps(dict(peer_sha256=hashlib.sha256(Path('/tmp/p2-smart-peer.bin').read_bytes()).hexdigest(), primary=images), indent=2) + '\n')
        (HERE / 'external.json').write_text(json.dumps(rows, indent=2) + '\n')
        failures = [row for row in rows if not row['passed']]
        print(f'{len(rows)-len(failures)}/{len(rows)} external observations passed; both boards released.')
        assert not failures, failures[:10]
    finally:
        os.close(fd)


if __name__ == '__main__':
    asyncio.run(main())
