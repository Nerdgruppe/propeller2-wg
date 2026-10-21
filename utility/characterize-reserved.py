#!/usr/bin/env python3
"""Probe vacant $BD/$BE instruction families with a finite independent supervisor."""
import argparse
import asyncio
import hashlib
import json
import struct
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
FIELDS = ("magic", "word", "d", "s", "seed", "completed", "before_c", "before_z",
          "after_c", "after_z", "result_d", "result_s", "clocks")
MAGIC = 0x52535644


def cases():
    """Cover both families, their modifiers, representative operands and all C/Z seeds."""
    rows = [(0, 0, 0, seed) for seed in range(4)]
    for base in (0xFBD00000, 0xFBE00000):
        for modifier in range(4):
            for seed in range(4):
                for d, s in ((0, 0), (1, 1), (0xFFFFFFFF, 0x40), (0x80000000, 0xFFFFFFFF)):
                    field = (s & 511) if modifier & 1 else 101
                    rows.append((base | (modifier << 18) | (100 << 9) | field, d, s, seed))
    return rows


def source(rows):
    """Generate DUT trials supervised by cog zero, with a 20,000-clock deadline."""
    table = "\n".join("LONG " + ", ".join(str(v) for v in row) for row in rows)
    monitor = Path(__file__).with_name("reserved-monitor.propan.in").read_text()
    return monitor.replace("{{COUNT}}", str(len(rows))).replace("{{TABLE}}", table)


async def measure(args):
    """Upload one finite campaign and retain exact source, image and supervisor frames."""
    import websockets  # Optional hardware utility dependency.

    args.output.mkdir(parents=True, exist_ok=True)
    rows = cases()
    path = args.output / "reserved.propan"
    path.write_text(source(rows))
    propan = ROOT / "zig-out/bin/propan"
    path.write_bytes(subprocess.check_output([str(propan), "--pretty-print", str(path)]))
    image_path = path.with_suffix(".bin")
    subprocess.run([str(propan), "-o", str(image_path), str(path)], check=True)
    image = image_path.read_bytes()
    data = bytearray()
    expected = len(rows) * len(FIELDS) * 4
    try:
        async with websockets.connect(args.endpoint + "?baudrate=2000000&timeout_ms=2500",
                                      max_size=None, proxy=None) as ws:
            await ws.send(struct.pack("<I", len(image)) + image)
            while len(data) < expected:
                data.extend(await asyncio.wait_for(ws.recv(), 15))
            await asyncio.sleep(0.02)
    finally:
        (args.output / "uart.bin").write_bytes(data)
    if len(data) != expected:
        raise RuntimeError(f"supervisor frames: {len(data)}/{expected} bytes")
    observed = []
    for index, row in enumerate(struct.iter_unpack("<13I", data)):
        record = dict(zip(FIELDS, row))
        if record["magic"] != MAGIC or tuple(record[key] for key in ("word", "d", "s", "seed")) != rows[index]:
            raise RuntimeError(f"invalid supervisor frame {index}: {record}")
        observed.append(record)
    result = {"endpoint": args.endpoint, "clock": "nominal 20 MHz RCFAST",
              "source_sha256": hashlib.sha256(path.read_bytes()).hexdigest(),
              "image_sha256": hashlib.sha256(image).hexdigest(), "cases": observed}
    (args.output / "results.json").write_text(json.dumps(result, indent=2) + "\n")
    print(f"{len(observed)} supervisor frames; {sum(row['completed'] == 1 for row in observed)} completed DUT trials")


def main():
    """Select the physical endpoint and evidence directory without changing simulator code."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--endpoint", default="ws://localhost:12880/")
    parser.add_argument("--output", type=Path, default=ROOT / ".tmp/reserved-instructions")
    args = parser.parse_args()
    asyncio.run(measure(args))


if __name__ == "__main__":
    main()

