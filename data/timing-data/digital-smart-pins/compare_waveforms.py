"""Compare framed Saleae pin captures with Windtunnel's clock-indexed VCD."""
import argparse
import bisect
import csv
import json
from pathlib import Path
import statistics


def read_csv(path):
    """Extract actual transitions, omitting repeated end-of-capture samples."""
    with path.open() as stream:
        reader = csv.reader(stream)
        channels = [int(name.split()[-1]) for name in next(reader)[1:]]
        edges = {channel: [] for channel in channels}
        previous = {}
        for row in reader:
            clock = float(row[0])
            for channel, value in zip(channels, row[1:]):
                if value != previous.get(channel):
                    edges[channel].append((clock, value))
                    previous[channel] = value
    return edges


def read_vcd(path, channels):
    """Convert selected physical pin events from system clocks to seconds at 200 MHz."""
    edges = {channel: [] for channel in channels}
    identifiers = {f'p{channel}': channel for channel in channels}
    clock = 0
    with path.open() as stream:
        for line in stream:
            if line.startswith('#'):
                clock = int(line[1:]) / 200_000_000
            elif line and line[0] in '01xz' and line[1:].strip() in identifiers:
                channel = identifiers[line[1:].strip()]
                edges[channel].append((clock, line[0]))
    return edges


def frames(edges):
    """Identify modes from groups of one through seven marker pulses."""
    groups = []
    for clock, value in edges[0]:
        if value == '1':
            if not groups or clock - groups[-1][-1][0] > 30e-6:
                groups.append([])
            groups[-1].append((clock, None))
        elif value == '0' and groups and groups[-1][-1][1] is None:
            rise, _ = groups[-1][-1]
            groups[-1][-1] = (rise, clock)
    return [(len(group) + 3, group[-1][1]) for group in groups if group[-1][1] is not None]


def window(edges, reference, scale=1):
    """Compare the driven interior of each mode, excluding floating configuration intervals."""
    start = reference + 15e-6 / scale
    end = reference + 950e-6 / scale
    index = bisect.bisect_right(edges, (start, 'z')) - 1
    assert index >= 0 and edges[index][1] in ('0', '1'), 'window starts on an unknown level'
    return [(15e-6, edges[index][1])] + [((clock - reference) * scale, value)
            for clock, value in edges if start < clock < end and value in ('0', '1')]


def marker_period(edges):
    """Calibrate the clock from same-polarity GPIO marker edges, independently of DUT outputs."""
    rises = [clock for clock, value in edges[0] if value == '1']
    periods = [b - a for a, b in zip(rises, rises[1:]) if b - a < 30e-6]
    assert periods, 'no marker calibration intervals'
    return statistics.mean(periods)


def compare(hardware, simulation, rate):
    """Require matching edge order/count and at most two samples of timing quantization."""
    simulated_frames = dict(frames(simulation))
    scale = marker_period(simulation) / marker_period(hardware)
    rows = []
    for mode, reference in frames(hardware):
        if mode not in simulated_frames:
            continue
        # The next marker must be present, proving this is a complete captured mode.
        if reference + 0.001 > hardware[0][-1][0]:
            continue
        for channel in hardware:
            if channel == 0:
                continue
            actual = window(hardware[channel], reference, scale)
            expected = window(simulation[channel], simulated_frames[mode])
            same = [value for _, value in actual] == [value for _, value in expected]
            errors = [abs(a[0] - b[0]) for a, b in zip(actual, expected)]
            maximum = max(errors, default=0)
            rows.append(dict(mode=mode, channel=channel, hardware_edges=len(actual) - 1,
                             simulated_edges=len(expected) - 1, initial_level=actual[0][1], max_error_seconds=maximum,
                             hardware_clock_scale=scale,
                             passed=same and maximum <= 2 / rate + 1e-12))
    assert {row['mode'] for row in rows} == set(range(4, 11)), 'capture is missing complete modes'
    return rows


if __name__ == '__main__':
    parser = argparse.ArgumentParser(__doc__)
    parser.add_argument('csv', type=Path)
    parser.add_argument('vcd', type=Path)
    parser.add_argument('--rate', type=int, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    captured = read_csv(args.csv)
    rows = compare(captured, read_vcd(args.vcd, captured), args.rate)
    args.output.write_text(json.dumps(rows, indent=2) + '\n')
    failures = [row for row in rows if not row['passed']]
    print(f'{len(rows) - len(failures)}/{len(rows)} framed pin comparisons passed')
    for row in failures[:20]:
        print(row)
    raise SystemExit(bool(failures))
