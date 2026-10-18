#!/usr/bin/env python3
"""Join a complete ordered analyzer capture to clock trials and document correctness limits."""

import argparse
import csv
import json
from pathlib import Path


def transients(root, trials):
    """Bound settling of 16-period averaged output rates using complete marker windows."""
    windows, edges, previous, start = [], [], [0, 0], None
    with (root / "logic/digital.csv").open() as source:
        for row in csv.DictReader(source):
            stamp = float(row["Time [s]"])
            current = [int(row["Channel 0"]), int(row["Channel 1"])]
            if current[1] and not previous[1]:
                start, edges = stamp, []
            if current[0] and not previous[0] and start is not None:
                edges.append(stamp)
            if previous[1] and not current[1] and start is not None:
                windows.append((start, stamp, edges))
                start = None
            previous = current
    if not trials or len(windows) != len(trials):
        raise ValueError("Transient capture must contain all ordered complete windows")
    results = []
    for trial, (start, end, edges) in zip(trials, windows, strict=True):
        edges = [stamp for stamp in edges if stamp < end - 0.001]
        blocks = []
        reference = trial["cpu_hz_from_clock_div1024"]
        for index in range(0, len(edges) - 16, 16):
            a, b = edges[index], edges[index + 16]
            rate = 16 * 1024 / (b - a)
            blocks.append(
                {
                    "start_s": a - start,
                    "end_s": b - start,
                    "hz": rate,
                    "error_fraction": rate / reference - 1,
                }
            )
        if not blocks:
            raise ValueError("Insufficient complete periods for transient analysis")
        bad = [
            index
            for index, block in enumerate(blocks)
            if abs(block["error_fraction"]) > 0.01
        ]
        settled = max(bad) + 1 if bad else 0
        results.append(
            {
                "case_index": trial["case_index"],
                "kind": trial["kind"],
                "functional_pass": trial["functional_pass"],
                "blocks16": blocks,
                "first_all_following_blocks_within1pct_end_s": blocks[settled]["end_s"]
                if settled < len(blocks)
                else None,
            }
        )
    (root / "transient-blocks.json").write_text(json.dumps(results, indent=2) + "\n")
    with Path("docs/measurements/physical-findings.md").open("a") as output:
        output.write(
            f"\n## Observable PLL frequency settling: {root.name}\n\n"
            "**Coarse independent output-rate bound.** Consecutive non-overlapping16-period "
            "averages of clock/1024 at25MS/s; discard last1ms. Bound is END of first block "
            "after which every retained average stays within1% of the middle-window final rate. "
            "Marker precedes HUBSET by firmware instructions. This is not internal lock, "
            "individual-period settling, jitter or proof of correct transient execution. "
            f"Evidence `{root}/transient-blocks.json` plus originalCSV and firmware.\n\n"
            "| Kind | Trials | Bound µs, min..max |\n| --- | --- | --- |\n"
        )
        for kind in dict.fromkeys(result["kind"] for result in results):
            bounds = [
                result["first_all_following_blocks_within1pct_end_s"]
                for result in results
                if result["kind"] == kind
            ]
            value = (
                f"{min(bounds) * 1e6:.2f}..{max(bounds) * 1e6:.2f}"
                if all(bound is not None for bound in bounds)
                else "not reached in all trials"
            )
            output.write(f"| {kind} | {len(bounds)} | {value} |\n")


def main():
    """Reject incomplete associations and append independently measured frequency evidence."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("directory", type=Path)
    parser.add_argument(
        "campaign", choices=("pll", "cpu", "smoke", "transition", "startup")
    )
    parser.add_argument("--transients", action="store_true")
    args = parser.parse_args()
    root = args.directory
    campaign = args.campaign
    trials = [
        json.loads(s) for s in (root / (campaign + ".jsonl")).read_text().splitlines()
    ]
    trials = [t for t in trials if "error" not in t]
    fits = json.loads((root / "logic-analysis.json").read_text())
    if not trials or len(fits) != len(trials):
        raise ValueError(
            f"Need complete nonempty association: {len(fits)} windows, {len(trials)} trials"
        )
    results = []
    for t, fit in zip(trials, fits, strict=True):
        expected = t.get("nominal_cpu_hz", t.get("nominal_target_hz"))
        measured = fit["cpu_hz_from_clock_div1024"]
        results.append(
            {
                **t,
                **fit,
                "error_ppm_vs_nominal": (measured / expected - 1) * 1e6,
                "rate_within_0_1_percent": abs(measured / expected - 1) < 0.001,
            }
        )
    (root / "verified.json").write_text(json.dumps(results, indent=2))
    passed = sum(r["rate_within_0_1_percent"] for r in results)
    with Path("docs/measurements/physical-findings.md").open("a") as f:
        f.write(
            f"\n## Independent frequency verification: {root.name}\n\n**Measured output rate with uncalibrated analyzer timebase.** {passed}/{len(results)} ordered trial windows within0.1% of nominal; {sum(t['functional_pass'] for t in trials)}/{len(trials)} host-known functional results passed. All retained edges are at least1ms inside trial markers, clock/1024,25MS/s. Timebase calibration, threshold bias and crystal tolerance remain unknown; ppm deviations are observations relative to nominal, not calibrated absolute PLL error. Positional association accepted only because complete marker/trial counts agree.\n\n"
        )
        if all("D" in r for r in results):
            f.write(
                "| D | M | P | Repeats | Nominal MHz | Measured MHz range | Functional | Rate |\n| --- | --- | --- | --- | --- | --- | --- | --- |\n"
            )
            keys = dict.fromkeys((r["D"], r["M"], r["P"]) for r in results)
            for d, m, p in keys:
                group = [r for r in results if (r["D"], r["M"], r["P"]) == (d, m, p)]
                values = [r["cpu_hz_from_clock_div1024"] / 1e6 for r in group]
                f.write(
                    f"| {d} | {m} | {p} | {len(group)} | {group[0]['nominal_cpu_hz'] / 1e6:.6f} | {min(values):.6f}..{max(values):.6f} | {sum(r['functional_pass'] for r in group)}/{len(group)} | {sum(r['rate_within_0_1_percent'] for r in group)}/{len(group)} |\n"
                )
        else:
            f.write(
                "| Kind | Repeats | Final MHz range | Rate |\n| --- | --- | --- | --- |\n"
            )
            for kind in dict.fromkeys(r["kind"] for r in results):
                group = [r for r in results if r["kind"] == kind]
                values = [r["cpu_hz_from_clock_div1024"] / 1e6 for r in group]
                f.write(
                    f"| {kind} | {len(group)} | {min(values):.6f}..{max(values):.6f} | {sum(r['rate_within_0_1_percent'] for r in group)}/{len(group)} |\n"
                )
        f.write(
            f"\nEvidence `{root}/{campaign}.jsonl`, `logic/digital.csv`, `logic-analysis.json`, `verified.json` and firmware hash. These short trials do not prove jitter, endurance, temperature range or valid untested configurations.\n"
        )
    print(
        json.dumps(
            {
                "cases": len(results),
                "rate_pass": passed,
                "functional_pass": sum(t["functional_pass"] for t in trials),
                "error_ppm_min": min(r["error_ppm_vs_nominal"] for r in results),
                "error_ppm_max": max(r["error_ppm_vs_nominal"] for r in results),
            }
        )
    )
    if args.transients:
        if campaign not in ("transition", "startup"):
            parser.error("--transients requires a transition or startup capture")
        transients(root, results)


if __name__ == "__main__":
    main()
