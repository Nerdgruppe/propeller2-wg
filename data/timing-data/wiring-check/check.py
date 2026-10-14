"""Check the two boards without opening the P2AAS board's physical serial port."""
import asyncio
import json
import os
from pathlib import Path
import select
import struct
import subprocess
import sys
import termios
import time

import websockets

ROOT = Path('/home/felix/projects/nerdgruppe/propeller2-wg')
HERE = Path(__file__).resolve().parent
PORT = '/dev/serial/by-id/usb-Parallax_Inc_Propeller_P2-EVAL_PLX6ZJKWX-if00-port0'


def read_serial(fd):
    """Read a response byte with a bounded wait."""
    if not select.select([fd], [], [], 2)[0]:
        raise TimeoutError('second board did not respond')
    data = os.read(fd, 1)
    if len(data) != 1:
        raise EOFError('second board serial EOF')
    return data


async def main():
    """Load RAM probes and check either analyzer patterns or all inter-board byte values."""
    fd = os.open(PORT, os.O_RDWR | os.O_NOCTTY | os.O_NONBLOCK)
    # Keep the tty open across the loader exit so DTR does not reset the RAM program.
    subprocess.run([str(ROOT / 'zig-out/bin/turboprop'), '-P', PORT, '-b', '2000000',
                    str(HERE / 'probe.bin')], check=True, timeout=8)
    attrs = termios.tcgetattr(fd)
    attrs[0] = attrs[1] = attrs[3] = 0
    attrs[2] = termios.CS8 | termios.CREAD | termios.CLOCAL
    attrs[4] = attrs[5] = termios.B2000000
    attrs[6][termios.VMIN] = 0
    attrs[6][termios.VTIME] = 0
    termios.tcsetattr(fd, termios.TCSANOW, attrs)

    first_secondary_response = True

    async def secondary(command):
        """Exchange one command with the explicitly selected second board."""
        nonlocal first_secondary_response
        os.write(fd, command)
        data = await asyncio.to_thread(read_serial, fd)
        if first_secondary_response and data == b'!':
            data = await asyncio.to_thread(read_serial, fd)
        first_secondary_response = False
        return data

    try:
        assert await secondary(b'F') == b'.'
        image = (HERE / 'probe.bin').read_bytes()
        url = 'ws://localhost:12880/?baudrate=2000000&timeout_ms=10000'
        async with websockets.connect(url, max_size=None) as ws:
            pending = bytearray()

            async def primary(command=None):
                """Exchange bytes through P2AAS, respecting stream message boundaries."""
                if command is not None:
                    await ws.send(command)
                while not pending:
                    pending.extend(await asyncio.wait_for(ws.recv(), 3))
                result = bytes(pending[:1])
                del pending[:1]
                return result

            await ws.send(struct.pack('<I', len(image)) + image)
            assert await primary() == b'!'
            print('Both RAM probes ready at 2 Mbaud.', flush=True)
            started = time.monotonic()
            try:
                if sys.argv[1] == 'capture':
                    assert await primary(b'L') == b'.'
                    print('Three 16-channel walking-one/walking-zero frames sent.', flush=True)
                else:
                    rows = []
                    for label, source, receiver in [
                        ('P2AAS -> second', primary, secondary),
                        ('second -> P2AAS', secondary, primary),
                    ]:
                        assert await primary(b'F') == b'.'
                        assert await secondary(b'F') == b'.'
                        assert await receiver(b'I') == b'.'
                        baseline = (await receiver(b'R'))[0]
                        print(f'{label}: pulldown baseline = 0x{baseline:02x}', flush=True)
                        # Every byte tests walking bits, complements, and every simultaneous combination.
                        for expected in range(256):
                            assert await source(b'W' + bytes([expected])) == b'.'
                            actual = (await receiver(b'R'))[0]
                            rows.append(dict(direction=label, expected=expected, actual=actual))
                        failures = [r for r in rows if r['direction'] == label and r['actual'] != r['expected']]
                        print(f'{label}: {256 - len(failures)}/256 byte patterns matched.', flush=True)
                    (HERE / 'interboard.json').write_text(json.dumps(rows, indent=2) + '\n')
                    failures = [r for r in rows if r['actual'] != r['expected']]
                    print('Mismatches:', json.dumps(failures[:32]), flush=True)
                print(f'Runtime: {time.monotonic() - started:.3f} s', flush=True)
            finally:
                assert await primary(b'F') == b'.'
                assert await secondary(b'F') == b'.'
                print('All tested pins released.', flush=True)
    finally:
        os.close(fd)


if __name__ == '__main__':
    asyncio.run(main())
