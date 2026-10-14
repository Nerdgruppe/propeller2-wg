"""Generate hardware-first smart-pin fixtures and freeze measured register expectations."""
import argparse
import json
from pathlib import Path
import re
import subprocess

ROOT = Path(__file__).resolve().parents[3]
TESTS = ROOT / 'tests/windtunnel/state'

CLOCK_START = '''
// Select the test clock while retaining the ROM-calibrated UART divider for reporting.
.if pll
                HUBSET aug(0x010009f8)
                WAITX aug(200000)
                HUBSET aug(0x010009fb)
                WAITX 64
.endif
'''
CLOCK_END = '''
// Restore RCFAST before the shared reporter uses the ROM-calibrated UART.
                HUBSET 0xF0
                WAITX 64
                JMP nrel(_end)
'''


def write_fixture(name, runs, observations, code):
    """Write named parameter runs with placeholder expectations for characterization."""
    lines = ['//? WINDTUNNEL CHECK LIST', '//? timeout-ms: 1000', '//? max-cycles: 500000']
    for label, values in runs:
        lines.append(f'//? run: "{label}"')
        lines.extend(f'//? const: {key} = {value}' for key, value in values.items())
        lines.extend(f'//? post: cog[*].reg[{symbol}] == 0' for symbol in observations)
    text = '\n'.join(lines) + '\n' + CLOCK_START + code + CLOCK_END
    text += '\n' + '\n'.join(f'var {symbol}: LONG 0' for symbol in observations) + '\n'
    path = TESTS / f'smart-pin-{name}.propan'
    path.write_text(text)
    format_fixture(path)


def format_fixture(path):
    """Apply the repository's canonical Propan formatter."""
    path.write_bytes(subprocess.check_output([str(ROOT / 'zig-out/bin/propan'), '--pretty-print', str(path)]))


def foundation():
    """Probe synchronous output and input routing with known GPIO transitions."""
    runs = [(f'pll{pll}-out{out_sync}-in{in_sync}-phase{phase}',
             dict(pll=pll, out_sync=out_sync, in_sync=in_sync, phase=phase))
            for pll in range(2) for out_sync in range(2) for in_sync in range(2) for phase in range(2)]
    code = '''
// Pin 1 observes the physical pad of pin 0; DIR stays low on the observer.
                DRVL 0
                WRPIN aug(out_sync << 16), 0
                WRPIN aug(0x70000000 | (in_sync << 16)), 1
                WAITX 32
                WAITX phase
                OUTH 0
'''
    code += '                TESTP 1 :wc\n                RCL rising, 1\n' * 16
    code += '                OUTL 0\n'
    code += '                TESTP 1 :wc\n                RCL falling, 1\n' * 16
    code += '                DIRL 0\n'
    write_fixture('synchronous', runs, ['rising', 'falling'], code)


def output(phase_values=range(2), fixture_name='output'):
    """Probe all seven output modes, phase, result, ready, acknowledgement, and reset."""
    runs = []
    configs = {
        4: [(3 | (2 << 16), 3), (5 | (2 << 16), 35), (0, 1)],
        5: [(3, 3), (5, 35), (0, 1)],
        6: [(1, 0x40000000), (3 | (0x1234 << 16), 0x87654321), (0, 0xffffffff)],
        7: [(1, 0x40000000), (3 | (0x1234 << 16), 0x87654321), (0, 0xffffffff)],
        8: [(3 | (7 << 16), y) for y in (0, 1, 3, 7, 8)],
        9: [(3 | (7 << 16), y) for y in (0, 1, 3, 7, 8)],
        10: [(3 | (7 << 16), y) for y in (0, 3, 7)],
    }
    for pll in range(2):
        for mode, pairs in configs.items():
            for x, y in pairs:
                for phase in phase_values:
                    runs.append((f'pll{pll}-mode{mode}-x{x:x}-y{y:x}-phase{phase}',
                                 dict(pll=pll, mode=mode, x=x, y=y, phase=phase)))
    code = '''
// P4 is the DUT; P6 observes its physical pad through the -2 selector.
// A/B come from low P3/P5, making SMPS feedback inactive.
                DRVL 3
                DRVL 5
                WRPIN aug(0x71000000 | (mode << 1) | P_TT_01), 4
                WRPIN aug(0x60000000), 6
                WXPIN aug(x), 4
                WYPIN aug(y), 4
                DIRH 4
                WAITX phase
                WYPIN aug(y), 4
'''
    code += '                TESTP 6 :wc\n                RCL waveform, 1\n' * 32
    code += '''
// Observe result/flag and readiness independently, then acknowledge and reset.
                RQPIN result, 4 :wc
                WRC flag
                TESTP 4 :wc
                WRC ready
                AKPIN 4
                WAITX 8
                TESTP 4 :wc
                WRC acknowledged
                DIRL 4
                WAITX 8
                RQPIN reset_result, 4 :wc
                WRC reset_flag
                TESTP 6 :wc
                WRC reset_output
                DIRL 3
                DIRL 5
'''
    write_fixture(fixture_name, runs, ['waveform', 'result', 'flag', 'ready', 'acknowledged',
                                 'reset_result', 'reset_flag', 'reset_output'], code)


def output_phases():
    """Complete all eight instruction offsets without replacing the initial two-phase evidence."""
    output(range(2, 8), 'output-phases')


def normalize_expectations(source):
    """Compare captured source identity while allowing measured postcondition values to change."""
    return [re.sub(r'(//\? post:.*)==.*', r'\1', line) for line in source]


def counting():
    """Probe all thirteen input engines using known quadrature and simultaneous edges."""
    runs = []
    for pll in range(2):
        for mode in range(11, 24):
            ys = range(8) if mode == 18 else range(4) if mode >= 19 else range(2)
            xs = (0, 32) if mode <= 15 else (1, 3) if mode >= 18 else (0,)
            for x in xs:
                for y in ys:
                    runs.append((f'pll{pll}-mode{mode}-x{x}-y{y}', dict(pll=pll, mode=mode, x=x, y=y)))
    code = '''
// P3/P5 supply physical A/B to P4. Each MOV changes both GPIO outputs together.
                DRVL 3
                DRVL 5
                WRPIN aug(0x71000000 | (mode << 1)), 4
                WXPIN aug(x), 4
                WYPIN aug(y), 4
                WAITX 32
                DIRH 4
'''
    sequence = (0, 8, 40, 32, 0, 32, 40, 8, 0, 40, 0, 8, 0, 32, 0, 0)
    observed = [f'result{i}' for i in range(len(sequence))] + ['flags', 'ready', 'reset_result', 'reset_flag']
    for i, state in enumerate(sequence):
        code += f'''\n// Stimulus {i}: A={bool(state & 8):d}, B={bool(state & 32):d}.
                MOV OUTA, {state}
                WAITX 9
                RQPIN result{i}, 4 :wc
                RCL flags, 1
'''
    code += '''
                TESTP 4 :wc
                WRC ready
                DIRL 4
                WAITX 8
                RQPIN reset_result, 4 :wc
                WRC reset_flag
                DIRL 3
                DIRL 5
'''
    write_fixture('counting', runs, observed, code)



def nco_reset():
    """Probe the running NCO bus and retained Z across reset for every short base period."""
    runs = [(f'pll{pll}-x{x}-phase{phase}', dict(pll=pll, x=x, phase=phase))
            for pll in range(2) for x in range(1, 6) for phase in range(8)]
    code = '''
// Read both the running NCO bus and its final retained value across DIR reset.
                WRPIN (6 << 1), 4
                WXPIN x, 4
                DIRH 4
                WYPIN aug(0x87654321), 4
                WAITX 100
                WAITX phase
                RQPIN running, 4
                DIRL 4
'''
    code += ''.join(f'                RQPIN z{i}, 4\n' for i in range(8))
    write_fixture('nco-reset', runs, ['running'] + [f'z{i}' for i in range(8)], code)


def freeze(path, artifacts):
    """Replace placeholders using independently captured hardware values and observation offsets."""
    lines = path.read_text().splitlines()
    wanted = set(re.findall(r'//\? run: "([^"]+)"', path.read_text()))
    seen = set()
    values = {}
    count = 0
    for metadata in artifacts.glob(f'{path.name}-*/simulation.json'):
        record = json.loads(metadata.read_text())
        hardware = json.loads((metadata.parent / 'hardware.json').read_text())
        captured_lines = (metadata.parent / 'fixture.propan').read_text().splitlines()
        assert normalize_expectations(lines) == normalize_expectations(captured_lines), 'capture source differs from current fixture'
        seen.add(record['run'])
        for observation in record['observations']:
            if observation['kind'] != 'reg':
                continue
            index = observation['line'] - 1
            assert '//? post:' in lines[index], (path, index, lines[index])
            value = hardware['registers'][observation['offset']]
            key = (record['run'], index)
            assert key not in values or values[key] == value, f'unstable hardware observation: {key}'
            values[key] = value
            lines[index] = re.sub(r'==.*', f'== 0x{value:08x}', lines[index])
            count += 1
    assert wanted == seen, f'missing/extra hardware runs: {wanted ^ seen}'
    assert count, f'no observations for {path}'
    path.write_text('\n'.join(lines) + '\n')
    # Assembly was formatted before capture; only literal checklist comment values changed.
    print(f'{path.name}: froze {count} observations from silicon')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(__doc__)
    parser.add_argument('action', choices=['foundation', 'output', 'output-phases', 'counting', 'nco-reset', 'freeze'])
    parser.add_argument('path', nargs='?', type=Path)
    parser.add_argument('artifacts', nargs='?', type=Path)
    args = parser.parse_args()
    if args.action == 'freeze':
        freeze(args.path, args.artifacts)
    else:
        {'foundation': foundation, 'output': output, 'output-phases': output_phases, 'counting': counting, 'nco-reset': nco_reset}[args.action]()
