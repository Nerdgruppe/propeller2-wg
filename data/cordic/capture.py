#!/usr/bin/env python3
"""Recapture the frozen CORDIC probes, or replay their archived hardware frames."""

import argparse
import json
import os
from pathlib import Path
import re
import shutil
import struct
import subprocess
import tempfile
import zlib


HERE = Path(__file__).resolve().parent
ROOT = HERE.parent.parent
OPERATIONS = ("QMUL", "QDIV", "QFRAC", "QSQRT", "QROTATE", "QVECTOR", "QLOG", "QEXP")


def extract(cases: list[dict], artifacts: Path) -> list[dict]:
    """Validate raw hardware frames and extract each probe's named X/Y registers."""
    rows = []
    for case in cases:
        folder = artifacts / case["name"]
        if not folder.is_dir():
            matches = list(artifacts.glob(case["name"] + ".propan-*"))
            if len(matches) != 1:
                raise ValueError(f"Expected one capture for {case['name']}, found {len(matches)}")
            folder = matches[0]
        request = json.loads((folder / "request.json").read_text())
        metadata = json.loads((folder / "simulation.json").read_text())
        measured = json.loads((folder / "hardware.json").read_text())
        frame = (folder / "hardware-output.bin").read_bytes()
        if len(frame) < 20:
            raise ValueError(f"{folder}: truncated hardware frame")
        magic, length, count = struct.unpack_from("<8sII", frame)
        if (magic != b"WTOR\x01\x00\x00\x00" or len(frame) != length + 20
                or length != request["payload_length"] or count != request["observation_count"]):
            raise ValueError(f"{folder}: hardware frame header mismatch")
        if zlib.crc32(frame[:-4]) != struct.unpack_from("<I", frame, len(frame) - 4)[0]:
            raise ValueError(f"{folder}: hardware frame CRC mismatch")
        registers = list(struct.unpack_from("<506I", frame, 16))
        if registers != measured["registers"]:
            raise ValueError(f"{folder}: raw registers disagree with hardware.json")
        observations = [o for o in metadata["observations"] if o["kind"] == "reg"]
        if len(observations) != 2 * len(case["inputs"]):
            raise ValueError(f"{folder}: unexpected number of result registers")
        for index, (d, s, q) in enumerate(case["inputs"]):
            x, y = (registers[o["offset"]] for o in observations[2 * index:2 * index + 2])
            rows.append(dict(op=case["op"], d=d, s=s, q=q, x=x, y=y))
    return rows


def check_fixture(rows: list[dict]) -> None:
    """Check that the regression fixture contains exactly the captured Q/D/S/X/Y tables."""
    source = (ROOT / "tests/windtunnel/state/cordic-arithmetic.propan").read_text()
    tables = [[int(value.strip(), 0) for value in group.split(",")]
              for group in re.findall(r"\bLONG\s+\[([^\]]+)\]", source)]
    expected = [[row[key] for key in ("q", "d", "s", "x", "y")]
                for op in OPERATIONS for row in rows if row["op"] == op]
    if tables != expected:
        raise ValueError("cordic-arithmetic.propan tables differ from the captured corpus")


def main() -> int:
    """Run native hardware characterization or offline extraction and verify reproducibility."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--artifacts", type=Path, default=ROOT / ".zig-cache/cordic-capture",
                        help="parent for fresh captures, or existing captures with --extract-only")
    parser.add_argument("--output", type=Path, help="extracted JSON; defaults to ARTIFACTS/vectors.json")
    parser.add_argument("--extract-only", action="store_true", help="replay existing frames without hardware")
    args = parser.parse_args()
    cases = json.loads((HERE / "cases.json").read_text())
    artifacts = args.artifacts.resolve()
    output = args.output.resolve() if args.output else artifacts / "vectors.json"
    if not args.extract_only:
        artifacts.mkdir(parents=True, exist_ok=True)
        artifacts = Path(tempfile.mkdtemp(prefix="capture-", dir=artifacts))
        endpoint = os.environ.get("P2AAS_ENDPOINT", "ws://localhost:12880/")
        print(f"Capturing {len(cases)} batches from {endpoint}; evidence: {artifacts}", flush=True)
        command = [str(ROOT / "zig-out/bin/windtunnel-tests"), "--characterize",
                   "--artifact-dir", str(artifacts)]
        probes = artifacts / "probes"
        probes.mkdir()
        for case in cases:
            path = probes / (case["name"] + ".propan")
            shutil.copyfile(HERE / "captures" / case["name"] / "fixture.propan", path)
            command.append(str(path))
        log = artifacts / "capture.log"
        with log.open("w") as stream:
            result = subprocess.run(command, cwd=ROOT, stdout=stream, stderr=subprocess.STDOUT,
                                    env={**os.environ, "P2AAS_ENDPOINT": endpoint}, check=False)
        if result.returncode:
            print(log.read_text())
            return result.returncode
    rows = extract(cases, artifacts)
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(json.dumps(rows, indent=2) + "\n")
    expected = json.loads((HERE / "vectors.json").read_text())
    if rows != expected:
        print(f"Captured rows differ from the archived corpus; new data retained in {output}")
        return 1
    check_fixture(rows)
    print(f"Verified {len(rows)} hardware rows and regression fixture tables; wrote {output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
