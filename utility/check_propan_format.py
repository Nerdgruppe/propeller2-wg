#!/usr/bin/env python3
"""Check formatting of every repository .propan file without rewriting files."""

import os
import subprocess
from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent
PROPAN = ROOT / "zig-out/bin/propan"


def run(*args: str, source: bytes | None = None, cwd: Path = ROOT) -> subprocess.CompletedProcess[bytes]:
    return subprocess.run(
        [str(PROPAN), "--no-warnings", *args],
        input=source,
        cwd=cwd,
        capture_output=True,
        check=False,
    )


def error(result: subprocess.CompletedProcess[bytes]) -> str:
    lines = result.stderr.decode(errors="replace").splitlines()
    return next((line for line in lines if "error:" in line), lines[0] if lines else f"exit {result.returncode}")


def main() -> int:
    if not PROPAN.is_file():
        print(f"Missing {PROPAN}; build Propan before running this script.")
        return 1

    paths = subprocess.check_output(
        ["git", "-C", str(ROOT), "ls-files", "-z", "--cached", "--others", "--exclude-standard", "--", "*.propan"]
    )
    files = sorted(ROOT / os.fsdecode(path) for path in paths.split(b"\0") if path)
    skipped = semantic_invalid = semantic_valid = failures = 0

    for path in files:
        relative = path.relative_to(ROOT)
        formatted = run("--pretty-print", str(path))
        if formatted.returncode != 0:
            skipped += 1
            print(f"SKIP {relative}: source does not parse")
            continue

        reparsed = run("--pretty-print", "-", source=formatted.stdout, cwd=path.parent)
        if reparsed.returncode != 0:
            failures += 1
            print(f"FAIL {relative}: formatted source does not parse: {error(reparsed)}")
            continue

        original = run("--format=none", str(path))
        # Stdin keeps the formatted text in memory; cwd preserves relative imports and FILE paths.
        checked = run("--format=none", "-", source=formatted.stdout, cwd=path.parent)
        if original.returncode == 0:
            semantic_valid += 1
        else:
            semantic_invalid += 1
        if (original.returncode == 0) != (checked.returncode == 0):
            failures += 1
            print(f"FAIL {relative}: semantic status changed: {error(checked) if checked.returncode else 'now succeeds'}")

    print(
        f"Checked {len(files)} files: {skipped} unparseable originals skipped, "
        f"{semantic_valid} semantically valid, {semantic_invalid} semantically invalid, "
        f"{failures} regressions."
    )
    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
