#!/usr/bin/env python3
"""Inspect exported scope samples without converting scope precision into silicon accuracy."""

import argparse
import csv
import hashlib
import json
import statistics
from itertools import pairwise
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent


def read_csv(path):
    """Read Micsig's simultaneous metadata/data columns, validating monotonic timestamps."""
    metadata, samples = {}, []
    with path.open() as source:
        reader = csv.reader(source)
        if next(reader, [])[:2] != ["ProID", "Info"]:
            raise ValueError("Not the qualified Micsig CSV layout")
        for row in reader:
            if len(row) < 5:
                continue
            if row[0]:
                metadata[row[0]] = row[1]
            if row[3]:
                samples.append((float(row[3]), float(row[4])))
    if len(samples) < 3 or any(b[0] <= a[0] for a, b in pairwise(samples)):
        raise ValueError("Missing or nonmonotonic waveform samples")
    declared = int(metadata["Record Length"].split()[0])
    if len(samples) != declared:
        raise ValueError(f"CSV has {len(samples)} samples; header declares {declared}")
    interval = float(metadata["Sample Interval"].split()[0])
    if any(abs((b[0] - a[0]) / interval - 1) > 0.02 for a, b in pairwise(samples)):
        raise ValueError("Sample spacing differs from declared interval")
    return metadata, samples


def summarize(path):
    """Describe a saved waveform and record acquisition settings without claiming calibration."""
    metadata, samples = read_csv(path)
    values = [v for _, v in samples]
    return {
        "source": str(path),
        "sha256": hashlib.sha256(path.read_bytes()).hexdigest(),
        "metadata": metadata,
        "samples": len(samples),
        "interval_s": float(metadata["Sample Interval"].split()[0]),
        "minimum_v": min(values),
        "maximum_v": max(values),
        "mean_v": statistics.mean(values),
        "population_rms_about_mean_v": statistics.pstdev(values),
        "quality": "reported scope voltages; calibration, probe loading and bandwidth additional",
    }


def ramp(path, descending):
    """Recover settled code plateaus from repeated monotonic256-code ramps using physical reset edges."""
    _, samples = read_csv(path)
    result = summarize(path)
    values = [v for _, v in samples]
    q = statistics.quantiles(values, n=10)
    span = q[-1] - q[0]
    if span <= 0:
        raise ValueError("No analog ramp span")
    resets = []
    direction = 1 if descending else -1
    for (ta, a), (tb, b) in pairwise(samples):
        delta = b - a
        if direction * delta > 0.5 * span:
            resets.append((ta + tb) / 2)
    if len(resets) < 3:
        raise ValueError("Need at least two complete cycles; increase horizontal span")
    periods = [b - a for a, b in pairwise(resets)]
    period = statistics.median(periods)
    if max(abs(p / period - 1) for p in periods) > 0.01:
        raise ValueError("Reset periods differ by >1%; reject ramp association")
    groups = [[] for _ in range(256)]
    segment = 0
    for stamp, value in samples:
        while segment + 1 < len(resets) and stamp >= resets[segment + 1]:
            segment += 1
        if segment + 1 >= len(resets) or stamp < resets[segment]:
            continue
        phase = (
            256 * (stamp - resets[segment]) / (resets[segment + 1] - resets[segment])
        )
        code = int(phase)
        if 0 <= code <= 255 and 0.25 <= phase - code <= 0.75:
            groups[255 - code if descending else code].append(value)
    if min(map(len, groups)) < 3:
        raise ValueError("Insufficient settled samples for all256codes")
    points = [
        {
            "code": code,
            "samples": len(group),
            "mean_v": statistics.mean(group),
            "rms_v": statistics.pstdev(group),
        }
        for code, group in enumerate(groups)
    ]
    mean_x, mean_y = 127.5, statistics.mean(p["mean_v"] for p in points)
    slope = sum((p["code"] - mean_x) * (p["mean_v"] - mean_y) for p in points) / sum(
        (i - mean_x) ** 2 for i in range(256)
    )
    intercept = mean_y - slope * mean_x
    residuals = [p["mean_v"] - (intercept + slope * p["code"]) for p in points]
    result.update(
        {
            "cycle_s": period,
            "complete_cycles": len(periods),
            "descending": descending,
            "volts_per_code_fit": slope,
            "intercept_v": intercept,
            "code0_mean_v": points[0]["mean_v"],
            "code255_mean_v": points[255]["mean_v"],
            "max_combined_residual_v": max(map(abs, residuals)),
            "points": points,
            "quality": "coarse DAC+scope+probe transfer; NOT calibrated DAC INL/DNL; scope8bit/unknown INL and gain errors remain",
        }
    )
    return result


def engine(path, levels):
    """Fit repeated full-scale falls and summarize central plateaus in an ascending DAC cycle."""
    _, samples = read_csv(path)
    result = summarize(path)
    values = [v for _, v in samples]
    low, high = statistics.quantiles(values, n=10)[0::8]
    if high - low < 1:
        raise ValueError("Need a full-scale waveform for cycle association")
    threshold = (low + high) / 2
    falls = [
        ta + (threshold - a) * (tb - ta) / (b - a)
        for (ta, a), (tb, b) in pairwise(samples)
        if a > threshold >= b
    ]
    if len(falls) < 5:
        raise ValueError("Need at least five complete full-scale falls")
    periods = [b - a for a, b in pairwise(falls)]
    period = statistics.mean(periods)
    if max(abs(p / period - 1) for p in periods) > 0.05:
        raise ValueError("Inconsistent cycles; reject association")
    center = (len(falls) - 1) / 2
    slope = sum((i - center) * (t - falls[0]) for i, t in enumerate(falls)) / sum(
        (i - center) ** 2 for i in range(len(falls))
    )
    groups, segment = [[] for _ in range(levels)], 0
    for stamp, value in samples:
        while segment + 1 < len(falls) and stamp >= falls[segment + 1]:
            segment += 1
        if segment + 1 >= len(falls) or stamp < falls[segment]:
            continue
        phase = levels * (stamp - falls[segment]) / periods[segment]
        code = int(phase)
        if 0 <= code < levels and 0.3 <= phase - code <= 0.7:
            groups[code].append(value)
    result.update(
        {
            "complete_cycles": len(periods),
            "cycle_period_min_s": min(periods),
            "cycle_period_mean_s": period,
            "cycle_period_max_s": max(periods),
            "cycle_frequency_fit_hz": 1 / slope,
            "sample_update_rate_hz": levels / slope,
            "endpoint_sampling_bound_relative": 2
            * result["interval_s"]
            / (falls[-1] - falls[0]),
            "plateaus": [
                {
                    "index": i,
                    "samples": len(g),
                    "min_v": min(g),
                    "mean_v": statistics.mean(g),
                    "max_v": max(g),
                    "rms_v": statistics.pstdev(g),
                }
                for i, g in enumerate(groups)
            ],
            "quality": "scope timebase, threshold walk, probe and bandwidth uncertainty additional; not intrinsic settling or maximum rate",
        }
    )
    return result


def self_test():
    """Verify ramp association against a synthetic known transfer with a complete reset sequence."""
    import tempfile

    with tempfile.TemporaryDirectory() as directory:
        p = Path(directory) / "ramp.csv"
        count = 5 * 256 * 8
        for descending in (False, True):
            with p.open("w") as output:
                output.write("ProID,Info,,time,Value,\n")
                for i in range(count):
                    name, info = ("", "")
                    if i == 0:
                        name, info = "Record Length", f"{count} Points"
                    if i == 1:
                        name, info = "Sample Interval", "1e-6 s"
                    code = i // 8 % 256
                    if descending:
                        code = 255 - code
                    output.write(f"{name},{info},,{i * 1e-6},{code * 0.01},\n")
            a = ramp(p, descending)
            assert abs(a["volts_per_code_fit"] - 0.01) < 1e-12
            assert a["max_combined_residual_v"] < 1e-12
    print("scope analysis self-test PASS")


def main():
    """Analyze an export, optionally appending its qualified observation immediately to Markdown."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=("stats", "ramp", "engine", "self-test"))
    parser.add_argument("csv", type=Path, nargs="?")
    parser.add_argument("--descending", action="store_true")
    parser.add_argument("--levels", type=int, choices=(2, 4), default=4)
    parser.add_argument("--output", type=Path)
    parser.add_argument("--record", action="store_true")
    args = parser.parse_args()
    if args.command == "self-test":
        self_test()
        return
    if args.csv is None:
        parser.error("CSV path required")
    if args.command == "ramp":
        result = ramp(args.csv, args.descending)
    elif args.command == "engine":
        result = engine(args.csv, args.levels)
    else:
        result = summarize(args.csv)
    encoded = json.dumps(result, indent=2)
    if args.output:
        args.output.write_text(encoded + "\n")
    if args.record:
        with (ROOT / "docs/measurements/physical-findings.md").open("a") as out:
            out.write(
                f"\n## Scope observation: {args.csv}\n\n**Combined instrument/pad observation.** "
                f"{result['samples']} samples at{result['interval_s']}s; "
                f"range{result['minimum_v']:.6f}..{result['maximum_v']:.6f}V. "
                f"{result['quality']}. SHA256`{result['sha256']}`.\n"
            )
            if args.command == "ramp":
                out.write(
                    f"All256code plateaus recovered over{result['complete_cycles']} complete cycles; "
                    f"period{result['cycle_s']:.9f}s, fitted slope{result['volts_per_code_fit']:.9f}V/code, "
                    f"max combined residual{result['max_combined_residual_v']:.6f}V. "
                    "Residual includes scope quantization/nonlinearity and is NOT certified silicon INL. "
                    f"Per-code statistics: `{args.output}`.\n"
                )
            if args.command == "engine":
                out.write(
                    f"{result['complete_cycles']} complete cycles; fitted cycle frequency "
                    f"{result['cycle_frequency_fit_hz']:.6f}Hz, {args.levels}-level update rate "
                    f"{result['sample_update_rate_hz']:.6f}samples/s. Period min/mean/max "
                    f"{result['cycle_period_min_s'] * 1e9:.6f}/"
                    f"{result['cycle_period_mean_s'] * 1e9:.6f}/"
                    f"{result['cycle_period_max_s'] * 1e9:.6f}ns; individual intervals include "
                    "sample/threshold interpolation error and are not a jitter measurement. "
                    f"Central plateau min/mean/max in `{args.output}`. "
                    "Output observed at this setting; not a maximum-rate or calibrated settling claim.\n"
                )
    print(
        encoded
        if args.output is None
        else json.dumps({k: v for k, v in result.items() if k != "points"})
    )


if __name__ == "__main__":
    main()
