"""Compare bounded serial output frames with a four-channel, 50 MS/s Saleae capture."""
import argparse
import bisect
import importlib.util
import json
from pathlib import Path

HERE = Path(__file__).resolve().parent
SPEC = importlib.util.spec_from_file_location('digital_waveforms', HERE.parent / 'digital-smart-pins/compare_waveforms.py')
DIGITAL = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(DIGITAL)


def window(edges, reference, scale):
    """Include the idle level, UART start/data/stop and both synchronous words within each marker frame."""
    start, end = reference + 5e-6 / scale, reference + 240e-6 / scale
    index = bisect.bisect_right(edges, (start, 'z')) - 1
    assert index >= 0 and edges[index][1] in ('0', '1')
    return [(5e-6, edges[index][1])] + [((clock - reference) * scale, value)
            for clock, value in edges if start < clock < end and value in ('0', '1')]


def main():
    """Calibrate oscillator scale from GPIO markers and compare every captured frame edge."""
    parser = argparse.ArgumentParser(__doc__)
    parser.add_argument('csv', type=Path)
    parser.add_argument('vcd', type=Path)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    hardware = DIGITAL.read_csv(args.csv)
    simulation = DIGITAL.read_vcd(args.vcd, hardware)
    scale = DIGITAL.marker_period(simulation) / DIGITAL.marker_period(hardware)
    simulated_frames = DIGITAL.frames(simulation)
    assert len(simulated_frames) == 1
    reference = simulated_frames[0][1]
    rows = []
    for frame, (_, actual_reference) in enumerate(DIGITAL.frames(hardware)):
        for channel in (4, 5, 6):
            actual = window(hardware[channel], actual_reference, scale)
            expected = window(simulation[channel], reference, 1)
            same = [value for _, value in actual] == [value for _, value in expected]
            error = max((abs(a[0] - b[0]) for a, b in zip(actual, expected)), default=0)
            rows.append(dict(frame=frame, channel=channel, hardware_edges=len(actual) - 1,
                             simulated_edges=len(expected) - 1, max_error_seconds=error,
                             passed=same and error <= 2 / 50000000 + 1e-12))
    assert rows, 'capture contains no complete serial frames'
    args.output.write_text(json.dumps(dict(sample_rate=50000000, clock_scale=scale, cases=rows), indent=2) + '\n')
    failures = [row for row in rows if not row['passed']]
    print(f'{len(rows) - len(failures)}/{len(rows)} serial waveform comparisons passed')
    assert not failures, failures[:10]


if __name__ == '__main__':
    main()
