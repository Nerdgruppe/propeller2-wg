#!/usr/bin/env python3
"""Analyze two-channel 100 MS/s PWM/marker captures; report averaged rates, never VCO jitter."""

import argparse
import bisect
import csv
import hashlib
import json
import math
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent


def fit(edges, divider):
    """Fit clock frequency to equally indexed rising edges with a conservative quantization bound."""
    n = len(edges)
    if n < 4:
        raise ValueError("Need at least four rising edges")
    center = (n - 1) / 2
    denominator = n * (n * n - 1) / 12
    slope = (
        sum((i - center) * (t - edges[0]) for i, t in enumerate(edges)) / denominator
    )
    # Each sampled transition is within one 10 ns interval; centered uncertainty is +/-5 ns.
    slope_error = 5e-9 * sum(abs(i - center) for i in range(n)) / denominator
    return {
        "hz": divider / slope,
        "hz_min": divider / (slope + slope_error),
        "hz_max": divider / (slope - slope_error),
        "rising_edges": n,
        "quantization_relative_bound": slope_error / (slope - slope_error),
    }


def select(edges, start, end):
    """Select edges in one half-open timestamp interval without scanning the whole capture."""
    return edges[bisect.bisect_left(edges, start) : bisect.bisect_left(edges, end)]


def phase_score(times, divider, initial, final, tau_us, delay_us, model="first-order"):
    """Compare an integrated frequency response with edge phase, fitting only a phase offset."""
    tau, delay = tau_us * 1e-6, delay_us * 1e-6
    errors = []
    for i, stamp in enumerate(times):
        t = stamp - delay
        if t <= 0:
            phase = initial * t
        else:
            transient = tau * -math.expm1(-t / tau)
            if model == "critical":
                transient = 2 * transient - t * math.exp(-t / tau)
            phase = final * t + (initial - final) * transient
        errors.append((i * divider - phase) / final * 1e9)
    offset = sum(errors) / len(errors)
    residuals = [error - offset for error in errors]
    return math.sqrt(sum(error * error for error in residuals) / len(residuals)), max(
        abs(error) for error in residuals
    )


def fit_phase(directory, model="first-order"):
    """Fit an effective transient candidate; residuals decide whether it is adequate."""
    result = json.loads((directory / "analysis.json").read_text())
    if (
        not result["pulse_count_pass"]
        or not result["functional_pass"]
        or result["initial"] is None
    ):
        raise ValueError(
            "Phase fitting requires independent clock-count and functional qualification"
        )
    initial, final = result["initial"]["hz"], result["final"]["hz"]
    if abs(initial / final - 1) < 0.05:
        raise ValueError("Need a substantial rate change")
    rising, _, _, _ = read_capture(directory / "digital.csv")
    anchor = result["marker_edges"][0][0]
    times = [t - anchor for t in select(rising, anchor - 2e-6, anchor + 80e-6)]
    divider = result["pwm_divider"]
    best = (math.inf, 0, 0, 0)
    for tau_index in range(1, 161):
        for delay_index in range(21):
            tau, delay = tau_index * 0.1, delay_index * 0.1
            rms, peak = phase_score(times, divider, initial, final, tau, delay, model)
            if rms < best[0]:
                best = rms, tau, delay, peak
    _, tau, delay, _ = best
    for tau_index in range(-20, 21):
        for delay_index in range(-20, 21):
            candidate_tau, candidate_delay = (
                tau + tau_index * 0.005,
                delay + delay_index * 0.005,
            )
            if candidate_tau <= 0 or candidate_delay < 0:
                continue
            rms, peak = phase_score(
                times, divider, initial, final, candidate_tau, candidate_delay, model
            )
            if rms < best[0]:
                best = rms, candidate_tau, candidate_delay, peak
    fitted = {
        "case": directory.name,
        "model": model,
        "tau_us": best[1],
        "delay_us": best[2],
        "phase_residual_rms_ns": best[0],
        "phase_residual_peak_ns": best[3],
        "fit_window_us": [-2, 80],
        "edges": len(times),
        "quality": "empirical integrated-frequency fit; parameter grid is not parameter accuracy; includes sampled-edge and marker uncertainty; not measured loop-filter components",
    }
    filename = "phase-fit.json" if model == "first-order" else "phase-fit-critical.json"
    (directory / filename).write_text(json.dumps(fitted, indent=2) + "\n")
    print(json.dumps(fitted))
    return fitted


def read_capture(path):
    """Check ordered 10 ns-grid CSV data and extract actual PWM and marker transitions."""
    rising, falling, marks = [], [], []
    previous = None
    last = -math.inf
    with path.open() as source:
        for row in csv.DictReader(source):
            stamp = float(row["Time [s]"])
            states = (int(row["Channel 0"]), int(row["Channel 1"]))
            if stamp < last or abs(stamp * 1e8 - round(stamp * 1e8)) > 1e-4:
                raise ValueError("Capture is unordered or not on the 100 MS/s grid")
            if previous is not None:
                if states[0] != previous[0]:
                    (rising if states[0] else falling).append(stamp)
                if states[1] != previous[1]:
                    marks.append((stamp, states[1]))
            previous, last = states, stamp
    return rising, falling, marks, last


def analyze(directory, record=False, block_us=8):
    """Verify UART integrity, fit settled rates and bound block-averaged settling after the markers."""
    source = (directory / "firmware.propan").read_text()
    constants = {}
    for line in source.splitlines():
        if line.startswith("const "):
            name, value = line[6:].split("=", 1)
            constants[name.strip()] = int(value.strip(), 0)
    run = json.loads((directory / "run.json").read_text())
    if "content" in run:
        run = json.loads(
            next(item["text"] for item in run["content"] if item["type"] == "text")
        )
    words = run.get("output", {}).get("values", [])
    checksum_ok = len(words) == 4 and words[3] == (
        (sum(words[:3]) & 0xFFFFFFFF) ^ 0x5A5A5A5A
    )
    signature = 0x12345678
    for _ in range(constants["WORK"]):
        signature = ((signature << 5) | (signature >> 27)) & 0xFFFFFFFF
        signature = (((signature ^ 0x9E3779B9) + 0x7F4A7C15) * 2) & 0xFFFFFFFF
    signature_ok = checksum_ok and words[0] == signature and words[1] == 0
    expected_elapsed = (
        constants["HOLD_TICKS"]
        + constants["WORK"] * 40
        + (13 if constants["WORK"] else 12)
    )
    elapsed_ok = checksum_ok and abs(words[2] - expected_elapsed) <= 64
    functional = signature_ok and elapsed_ok
    rising, falling, marks, last = read_capture(directory / "digital.csv")
    if not marks or marks[0][1] != 1:
        raise ValueError("No independent rising trial marker")
    anchor = marks[0][0]
    startup = constants["FROM_MODE"] == 0
    if startup and (len(marks) < 2 or marks[1][1] != 0):
        raise ValueError("Startup requires enable and source-select markers")
    selection = marks[1][0] if startup else anchor
    # Reject the return-to-RC/cleanup tail and require a later inactive capture region.
    end = rising[-1] - 50e-6
    if last - end < 0.5e-3 or end - selection < 2e-3:
        raise ValueError("Incomplete stimulus or insufficient retained stable tail")
    divider = constants["BASE"] * constants["FRAME"]
    exporter = "NCO" if "P_NCO_FREQ" in source else "PWM"
    post_marker_rises = len(select(rising, selection, last))
    pulse_cycle_difference = post_marker_rises * divider - expected_elapsed
    pulse_count_ok = abs(pulse_cycle_difference) <= 256
    final = fit(select(rising, end - 1e-3, end), divider)
    initial_edges = select(rising, anchor - 0.5e-3, anchor - 50e-6)
    if len(initial_edges) < 4:
        initial_edges = select(rising, anchor - 0.5e-3, anchor - 0.2e-6)
    initial = fit(initial_edges, divider) if len(initial_edges) >= 4 else None
    blocks = []
    width = block_us * 1e-6
    for i in range(int((end - selection) / width)):
        start = selection + i * width
        edges = select(rising, start, start + width)
        if len(edges) >= 4:
            block = fit(edges, divider)
            block.update(
                start_us=(start - selection) * 1e6,
                end_us=(start + width - selection) * 1e6,
            )
            block["error_fraction"] = block["hz"] / final["hz"] - 1
            blocks.append(block)
        else:
            blocks.append(
                {
                    "start_us": i * block_us,
                    "end_us": (i + 1) * block_us,
                    "hz": None,
                    "error_fraction": None,
                }
            )
    bounds = {}
    for tolerance in (0.01, 0.005):
        bad = [
            i
            for i, b in enumerate(blocks)
            if b["hz"] is None
            or max(
                abs(b["hz_min"] / final["hz_max"] - 1),
                abs(b["hz_max"] / final["hz_min"] - 1),
            )
            > tolerance
        ]
        index = max(bad) + 1 if bad else 0
        bounds[str(tolerance)] = (
            blocks[index]["end_us"] if index < len(blocks) else None
        )
    # Short profiles expose response shape, but quantization prevents robust narrow-band settling.
    if block_us < 8:
        bounds = {"0.01": None, "0.005": None}
    near = [b["hz"] for b in blocks if b["hz"] is not None and b["start_us"] < 2000]
    result = {
        "case": directory.name,
        "constants": constants,
        "sample_rate": 100000000,
        "block_us": block_us,
        "settling_resolution_qualified": block_us >= 8,
        "pwm_divider": divider,
        "exporter": exporter,
        "transport_checksum_pass": checksum_ok,
        "functional_pass": functional,
        "signature_and_zero_counter_pass": signature_ok,
        "elapsed_range_pass": elapsed_ok,
        "expected_elapsed_ticks": expected_elapsed,
        "pulse_count_pass": pulse_count_ok,
        "post_marker_rising_edges": post_marker_rises,
        "pulse_cycle_difference_vs_command": pulse_cycle_difference,
        "uart_words": words,
        "marker_edges": marks,
        "enable_to_select_us": (selection - anchor) * 1e6,
        "selection_from_trigger_us": (selection - anchor) * 1e6,
        "initial": initial,
        "final": final,
        "settling_end_us": bounds,
        "first2ms_block_min_hz": min(near) if near else None,
        "first2ms_block_max_hz": max(near) if near else None,
        "blocks": blocks,
        "minimum_observed_high_or_low_ns": min(
            b - a
            for a, b in zip(sorted(rising + falling), sorted(rising + falling)[1:])
        )
        * 1e9,
        "quality": f"{block_us}us divided-clock output-rate fits, quantization bounded; uncalibrated timebase, threshold, marker pipeline and divided-clock averaging remain; not internal VCO lock, phase noise or runt-pulse qualification",
        "sha256": {
            p.name: hashlib.sha256(p.read_bytes()).hexdigest()
            for p in (
                directory / "firmware.propan",
                directory / "digital.csv",
                directory / "run.json",
            )
        },
    }
    filename = "analysis.json" if block_us == 8 else f"analysis-{block_us:g}us.json"
    (directory / filename).write_text(json.dumps(result, indent=2) + "\n")
    if record:
        initial_text = (
            f"{initial['hz'] / 1e6:.6f}" if initial is not None else "unqualified"
        )
        text = (
            f"\n## 2026-10-18 Saleae 100 MS/s: {directory.name}\n\n"
            f"**Independent divided-clock observation.** P0 {exporter}/{divider}, P1 marker; "
            f"source {initial_text} MHz, final {final['hz'] / 1e6:.6f} MHz. "
            f"UART checksum={checksum_ok}, seed/workload+elapsed={functional}, "
            f"exported-clock cycle-count consistency={pulse_count_ok}. "
            f"Conservative {block_us} µs-block settling-end bounds after source-select marker: "
            f"1% {bounds['0.01']} µs; 0.5% {bounds['0.005']} µs. "
            f"Enable-to-select marker interval {result['enable_to_select_us']:.3f} µs. "
            f"{result['quality']}. Evidence `{directory.relative_to(ROOT)}/{filename}`, "
            "raw CSV, exact firmware, UART response and hashes. A complete UART response occurs only after explicit P0/P1 cleanup.\n"
        )
        for path in (
            ROOT / "docs/measurements/physical-findings.md",
            ROOT / "docs/accumulated.md",
        ):
            with path.open("a") as output:
                output.write(text)
    print(
        json.dumps(
            {
                k: v
                for k, v in result.items()
                if k not in ("blocks", "sha256", "constants")
            },
            sort_keys=True,
        )
    )
    return result


def self_test():
    """Check fitted timing against synthetic exact and quantized 160 MHz divided-clock edges."""
    exact = [i * 200e-9 for i in range(40)]
    assert math.isclose(fit(exact, 32)["hz"], 160e6, rel_tol=1e-12)
    edges = [round((0.003 + i * 32 / 163.2e6) * 1e8) / 1e8 for i in range(40)]
    measured = fit(edges, 32)
    assert abs(measured["hz"] / 163.2e6 - 1) < measured["quantization_relative_bound"]
    for model in ("first-order", "critical"):
        rms, peak = phase_score(exact, 32, 160e6, 160e6, 1, 0.5, model)
        assert rms < 1e-8 and peak < 1e-8
    print("self-test PASS")


def main():
    """Analyze one retained capture or validate the offline frequency fitter."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("directory", nargs="?", type=Path)
    parser.add_argument("--record", action="store_true")
    parser.add_argument("--self-test", action="store_true")
    parser.add_argument("--fit-phase", action="store_true")
    parser.add_argument("--critical", action="store_true")
    parser.add_argument("--block-us", type=float, default=8)
    args = parser.parse_args()
    if args.self_test:
        self_test()
    elif args.fit_phase:
        fit_phase(
            args.directory.resolve(), "critical" if args.critical else "first-order"
        )
    else:
        if not 1 <= args.block_us <= 64:
            parser.error("block width must be 1..64 us")
        analyze(args.directory.resolve(), args.record, args.block_us)


if __name__ == "__main__":
    main()
