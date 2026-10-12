#!/usr/bin/env python3
"""Recover cross-cog RNG tap relationships from retained hardware frames.

Tap numbers in taps.json are anonymous equivalence classes, NOT numbered bits
of a recovered xoroshiro output. No generator or seed hypothesis is assumed.
"""

import argparse
import json
from pathlib import Path
import struct
import zlib


def words(directory):
    """Validate an oracle frame and return its extra hub observation as longs."""
    frame = (directory / "hardware-output.bin").read_bytes()
    request = json.loads((directory / "request.json").read_text())
    assert frame[:8] == b"WTOR\x01\0\0\0"
    length, count = struct.unpack_from("<II", frame, 8)
    assert (length, count) == (request["payload_length"], request["observation_count"])
    assert len(frame) == length + 20
    assert zlib.crc32(frame[:-4]) == struct.unpack_from("<I", frame, len(frame) - 4)[0]
    # Header + 506 registers, Q, two copies each of C and Z.
    extra = frame[2048:-4]
    return struct.unpack(f"<{len(extra) // 4}I", extra)


def capture(directory):
    """Extract synchronized 128-word bursts, grouped by cog and clock stamp."""
    data = words(directory)
    assert len(data) % (8 * 129) == 0
    blocks = len(data) // (8 * 129)
    cogs = [[data[(c * blocks + b) * 129:(c * blocks + b + 1) * 129]
             for b in range(blocks)] for c in range(8)]
    assert all(cogs[c][b][0] == cogs[0][b][0]
               for c in range(8) for b in range(blocks))
    assert all((cogs[0][b][0] - cogs[0][b - 1][0]) & 0xffffffff == 4097
               for b in range(1, blocks))
    return cogs


def recover(cogs):
    """Group bit histories that agree or complement over the entire capture."""
    groups = {}
    for cog, blocks in enumerate(cogs):
        for bit in range(32):
            history = tuple((value >> bit) & 1 for block in blocks for value in block[1:])
            invert = history[0]
            key = tuple(value ^ invert for value in history)
            groups.setdefault(key, []).append([cog, bit, bool(invert)])
    taps = list(groups.values())
    assert len(taps) == 64
    assert all(len(group) == 4 for group in taps)
    assert all(len({c for c, _, _ in group}) == 4 for group in taps)
    return taps


def check(cogs, taps):
    """Verify every measured cross-cog relationship without fitting a state."""
    for group in taps:
        first, bit, invert = group[0]
        for cog, other_bit, other_invert in group[1:]:
            for a, b in zip(cogs[first], cogs[cog]):
                assert all(((x >> bit) & 1) ^ invert == ((y >> other_bit) & 1) ^ other_invert
                           for x, y in zip(a[1:], b[1:]))


def consumer_check(directory, taps):
    """Compare CPU consumer words and GETRND flags to companion-cog taps."""
    data = words(directory)
    names = ("GETRND D", "GETRND flags", "BITRND", "DIRRND", "OUTRND", "FLTRND", "DRVRND", "OUTRND B")
    assert len(data) == 4128
    prior_bit, prior_a, prior_b = 0, 0, 0
    for block in range(4):
        stamp = data[block * 129]
        for operation, name in enumerate(names):
            offset = block * 129 + 1 + operation * 4
            clock, result, carry, zero = data[offset:offset + 4]
            # Each operation follows GETCT directly: both instructions sample
            # their sources on the corresponding execute edge, two clocks apart.
            index = ((clock - stamp) & 0xffffffff) // 2
            value = 0
            for group in taps:
                target = next((entry for entry in group if entry[0] == 0), None)
                if target is None:
                    continue
                _, dst, target_invert = target
                cog, bit, invert = next(entry for entry in group if entry[0] != 0)
                sample = data[cog * 516 + block * 129 + 1 + index]
                value |= (((sample >> bit) & 1) ^ invert ^ target_invert) << dst
            if operation != 1:
                assert result == value, (block, name, hex(result), hex(value))
            if operation < 2:
                assert (carry, zero) == (value >> 31, (value >> 30) & 1)
            else:
                original = 0
                if operation == 2:
                    original, prior_bit = prior_bit, result
                elif operation in (4, 5, 6):
                    original, prior_a = prior_a, result
                elif operation == 7:
                    original, prior_b = prior_b, result
                assert (carry, zero) == (original & 1, original & 1), (block, name, carry, zero, original)
    print(f"{directory.name}: 32 consumer words and flags agree with companion cogs")


def waitx_check(directory, taps):
    """Check randomized WAITX against observers covering both clock parities."""
    data = words(directory)
    masks = (0, 1, 3, 7, 15, 31)
    assert len(data) == 4128
    for block in range(4):
        stamp = data[block * 129]
        for operation, mask in enumerate(masks):
            offset = block * 129 + 1 + operation * 4
            clock, after, carry, zero = data[offset:offset + 4]
            value = 0
            if mask:
                for group in taps:
                    target = next((entry for entry in group if entry[0] == 0), None)
                    if target is None:
                        continue
                    _, dst, target_invert = target
                    choices = []
                    for cog, bit, invert in group:
                        if cog == 0:
                            continue
                        first = stamp + (7 if cog >= 4 else 6)
                        elapsed = (clock + 2 - first) & 0xffffffff
                        if elapsed % 2 == 0 and elapsed // 2 < 128:
                            sample = data[cog * 516 + block * 129 + 1 + elapsed // 2]
                            choices.append(((sample >> bit) & 1) ^ invert ^ target_invert)
                    assert choices and len(set(choices)) == 1
                    value |= choices[0] << dst
            assert (after - clock) & 0xffffffff == 4 + (value & mask)
            # WC, WZ, WCZ repeat: unrequested flags retain their prior values.
            if operation % 3 != 1:
                assert carry == 0
            if operation % 3 != 0:
                assert zero == 0
            else:
                assert zero == (1 if operation == 0 and block == 0 else 0)
            if operation % 3 == 1:
                assert carry == 0
    print(f"{directory.name}: 24 randomized WAITX deadlines and requested flags verified")


def seed_check(directory, taps):
    """Measure cog-0 HUBSET duration where observer and subject parities match."""
    data = words(directory)
    assert len(data) == 8704
    durations = []
    for trial in range(64):
        stamp = data[trial * 17]
        assert all(data[cog * 1088 + trial * 17] == stamp for cog in range(8))
        if stamp & 1:
            continue
        companion = []
        for sample in range(16):
            value = 0
            for group in taps:
                target = next((entry for entry in group if entry[0] == 0), None)
                if target is None:
                    continue
                _, destination, target_invert = target
                cog, bit, invert = next(entry for entry in group if entry[0] != 0)
                other = data[cog * 1088 + trial * 17 + 1 + sample]
                value |= (((other >> bit) & 1) ^ invert ^ target_invert) << destination
            companion.append(value)
        subject = data[trial * 17 + 1:trial * 17 + 17]
        shifts = [shift for shift in range(1, 5) if tuple(companion[shift:]) == subject[:16 - shift]]
        assert len(shifts) == 1
        duration = shifts[0] * 2
        assert duration == 2 + ((6 - stamp) & 7)
        durations.append(duration)
    assert len(durations) == 32 and set(durations) == {2, 4, 6, 8}
    print(f"{directory.name}: 32 HUBSET command durations verified (even CT phases)")


def main():
    """Recover a tap table, then independently replay other retained captures."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("capture", type=Path)
    parser.add_argument("--check", type=Path, nargs="*", default=[])
    parser.add_argument("--consumers", type=Path)
    parser.add_argument("--waitx", type=Path)
    parser.add_argument("--seed", type=Path)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    taps = recover(capture(args.capture))
    if args.output:
        args.output.write_text(json.dumps(taps, indent=2) + "\n")
    for directory in [args.capture, *args.check]:
        cogs = capture(directory)
        check(cogs, taps)
        print(f"{directory.name}: 64 taps, {len(cogs[0]) * 128} words/cog, cross-cog relationships verified")
    if args.consumers:
        consumer_check(args.consumers, taps)
    if args.waitx:
        waitx_check(args.waitx, taps)
    if args.seed:
        seed_check(args.seed, taps)


if __name__ == "__main__":
    main()
