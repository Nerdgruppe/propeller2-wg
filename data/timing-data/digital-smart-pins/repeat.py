"""Repeat hardware boundary probes and compare every register with its frozen silicon expectation."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import tarfile

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
FIXTURES = [HERE / 'repeat-boundaries.propan'] + [
    ROOT / f'tests/windtunnel/state/smart-pin-{name}.propan'
    for name in ['synchronous-boundary', 'nco-trace', 'nco-reset', 'events', 'gpio-events', 'clocked-history']
]


def validate(folder):
    """Require literal register observations to equal the captured source's frozen postconditions."""
    rows = []
    for path in sorted(folder.glob('*/hardware.json')):
        metadata = json.loads(path.with_name('simulation.json').read_text())
        hardware = json.loads(path.read_text())
        source = path.with_name('fixture.propan').read_text().splitlines()
        observations = []
        for observation in metadata['observations']:
            if observation['kind'] != 'reg':
                continue
            line = source[observation['line'] - 1]
            expected = int(re.search(r'==\s*(0x[0-9a-fA-F]+)', line)[1], 16)
            actual = hardware['registers'][observation['offset']]
            assert actual == expected, (metadata['run'], line, actual, expected)
            observations.append(dict(offset=observation['offset'], value=actual))
        rows.append(dict(run=metadata['run'], observations=observations,
                         source_sha256=hashlib.sha256(path.with_name('fixture.propan').read_bytes()).hexdigest(),
                         image_sha256=hashlib.sha256(path.with_name('uploaded.bin').read_bytes()).hexdigest()))
    wanted = sum(len(re.findall(r'^//\? run:', path.read_text(), re.M)) for path in FIXTURES)
    assert len(rows) == wanted, (len(rows), wanted)
    return rows


def main():
    """Collect bounded RAM probes serially, validate them, and retain their raw evidence."""
    parser = argparse.ArgumentParser(__doc__)
    parser.add_argument('--count', type=int, default=10)
    parser.add_argument('--artifact-dir', type=Path, required=True)
    args = parser.parse_args()
    assert args.count > 0
    args.artifact_dir.mkdir(parents=True, exist_ok=True)
    environment = dict(os.environ, P2AAS_ENDPOINT='ws://localhost:12880/')
    runs = []
    for iteration in range(args.count):
        folder = args.artifact_dir / f'{iteration + 1:02}'
        log = args.artifact_dir / f'{iteration + 1:02}.log'
        with log.open('wb') as stream:
            subprocess.run([str(ROOT / 'zig-out/bin/windtunnel-tests'), '--characterize',
                            '--artifact-dir', str(folder), *map(str, FIXTURES)],
                           cwd=ROOT, env=environment, stdout=stream, stderr=subprocess.STDOUT, check=True)
        rows = validate(folder)
        runs.append(dict(iteration=iteration + 1, cases=rows))
        (HERE / 'evidence/repeats.json').write_text(json.dumps(runs, indent=2) + '\n')
        print(f'Iteration {iteration + 1}/{args.count}: {len(rows)} hardware runs matched frozen values', flush=True)
    with tarfile.open(HERE / 'evidence/repeats.tar.gz', 'w:gz') as archive:
        archive.add(args.artifact_dir, arcname='repeats')
    print(f'{sum(len(run["cases"]) for run in runs)} hardware runs passed', flush=True)


if __name__ == '__main__':
    main()
