"""Round-trip every semantically accepted Propan fixture through FlexSpin."""

import pathlib
import subprocess
import sys
import tempfile


def run(*args):
    return subprocess.run(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)


def main():
    propan, flexspin = sys.argv[1:3]
    root = pathlib.Path(__file__).resolve().parents[2]
    checked = 0
    failures = []
    with tempfile.TemporaryDirectory() as directory:
        temp = pathlib.Path(directory)
        flat = temp / "flat.bin"
        spin = temp / "emitted.spin2"
        rebuilt = temp / "rebuilt.bin"
        for source in sorted((root / "tests/propan").rglob("*.propan")):
            flat.unlink(missing_ok=True)
            rebuilt.unlink(missing_ok=True)
            if run(propan, "--format=flat", "-o", str(flat), str(source)).returncode:
                continue
            checked += 1
            emitted = run(propan, "--format=spin2", "-o", str(spin), str(source))
            compiled = run(flexspin, "-2", "-q", "-o", str(rebuilt), str(spin)) if emitted.returncode == 0 else emitted
            readable = True
            if source.name == "spin2-readable.propan" and emitted.returncode == 0:
                text = spin.read_text()
                readable = "MOV p2_label_dst_0, #p2_const_VALUE_0" in text and "MOV p2_label_dst_0, #$3" in text
            if compiled.returncode or not rebuilt.exists() or flat.read_bytes() != rebuilt.read_bytes() or not readable:
                failures.append((source.relative_to(root), (emitted.stderr + compiled.stderr).decode(errors="replace")))
    print(f"Spin2 round-trip: {checked} accepted files, {len(failures)} failures")
    for source, error in failures:
        print(f"  {source}: {error.strip()}", file=sys.stderr)
    return bool(failures) or checked == 0


if __name__ == "__main__":
    sys.exit(main())
