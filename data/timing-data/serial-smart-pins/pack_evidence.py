"""Validate frozen serial observations against raw silicon captures and retain their provenance."""
import argparse
import functools
import hashlib
import json
from pathlib import Path
import re
import subprocess
import tarfile

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
FIXTURES = ROOT / 'tests/windtunnel/state'


@functools.cache
def canonical(source):
    """Compare formatted source identity while allowing measured checklist values to replace placeholders."""
    normalized = re.sub(r'(//\? post:.*==).*', r'\1', source)
    return subprocess.check_output([str(ROOT / 'zig-out/bin/propan'), '--pretty-print', '-'],
                                   input=normalized.encode())


def expectations(source):
    """Index literal register expectations by run and symbol, without using simulator outputs."""
    runs = {}
    current = None
    for line in source.splitlines():
        match = re.search(r'//\? run: "([^"]+)"', line)
        if match:
            current = runs.setdefault(match[1], {})
        match = re.search(r'//\? post:.*\.reg\[([^]]+)\]\s*==\s*(0x[0-9a-fA-F]+|\d+)', line)
        if match:
            current[match[1]] = int(match[2], 0)
    return runs


def main():
    """Require every frozen case to match a captured source and all of its physical register values."""
    parser = argparse.ArgumentParser(__doc__)
    parser.add_argument('artifacts', nargs='+', type=Path)
    args = parser.parse_args()
    fixtures = {path.name: path for path in FIXTURES.glob('smart-pin-serial-*.propan')}
    expected = {name: expectations(path.read_text()) for name, path in fixtures.items()}
    captures = {}
    for artifacts in args.artifacts:
        for hardware_file in sorted(artifacts.glob('*/hardware.json')):
            folder = hardware_file.parent
            name = folder.name.split('.propan')[0] + '.propan'
            if name not in fixtures:
                continue
            source = folder.joinpath('fixture.propan').read_text()
            if canonical(source) != canonical(fixtures[name].read_text()):
                continue
            metadata = json.loads(folder.joinpath('simulation.json').read_text())
            hardware = json.loads(hardware_file.read_text())
            run = metadata['run']
            lines = source.splitlines()
            observations = {}
            for observation in metadata['observations']:
                if observation['kind'] != 'reg':
                    continue
                symbol = re.search(r'\.reg\[([^]]+)\]', lines[observation['line'] - 1])[1]
                value = hardware['registers'][observation['offset']]
                assert value == expected[name][run][symbol], (name, run, symbol, value)
                observations[symbol] = value
            assert observations == expected[name][run], (name, run, 'missing observations')
            key = name, run
            if key in captures:
                assert captures[key][1]['observations'] == observations, ('unstable capture', key)
            image = folder.joinpath('uploaded.bin').read_bytes()
            captures[key] = folder, dict(fixture=name, run=run, observations=observations,
                                        image_sha256=hashlib.sha256(image).hexdigest(),
                                        source_sha256=hashlib.sha256(source.encode()).hexdigest())
    wanted = {(name, run) for name, runs in expected.items() for run in runs}
    assert set(captures) == wanted, ('missing captures', sorted(wanted - set(captures)))
    rows = []
    with tarfile.open(HERE / 'evidence/characterization.tar.gz', 'w:gz') as archive:
        for (name, run), (folder, row) in sorted(captures.items()):
            location = f'characterization/{name}/{run}'
            archive.add(folder, arcname=location)
            rows.append(dict(**row, archive_path=location))
    (HERE / 'evidence/characterization.json').write_text(json.dumps(rows, indent=2) + '\n')
    print(f'{len(rows)} silicon cases and {sum(len(row["observations"]) for row in rows)} register observations validated')


if __name__ == '__main__':
    main()
