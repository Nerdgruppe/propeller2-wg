#!/usr/bin/env python3
"""Test a xoroshiro128** hypothesis using an optional installed Z3 executable.

This is research tooling, not part of Windtunnel or its build. A solver timeout
does not reject the hypothesis. A low-tap solution is accepted only if it also
predicts every other output bit in the capture.
"""

import argparse
import json
from pathlib import Path
import re
import subprocess

import analyze

MASK = (1 << 64) - 1


def rotate(value, count):
    """Rotate a 64-bit word left."""
    return ((value << count) | (value >> (64 - count))) & MASK


def step(state, constants):
    """Advance the candidate linear transition, with S0 in the low 64 bits."""
    a, b, c = constants
    first = state & MASK
    mixed = (state >> 64) ^ first
    return (rotate(first, a) ^ mixed ^ ((mixed << b) & MASK)) | (rotate(mixed, c) << 64)


def scramble(value):
    """Apply the reference [5,7,9] scrambler modulo 2^64."""
    return (rotate(value * 5 & MASK, 7) * 9) & MASK


def observations(cogs, taps):
    """Collect the anonymous pool and its elapsed clocks from all bursts."""
    clocks, values = [], []
    for block in range(len(cogs[0])):
        for sample in range(128):
            clocks.append(((cogs[0][block][0] - cogs[0][0][0]) & 0xffffffff) + sample * 2)
            pool = 0
            for index, group in enumerate(taps):
                cog, bit, invert = group[0]
                pool |= (((cogs[cog][block][sample + 1] >> bit) & 1) ^ invert) << index
            values.append(pool)
    return clocks, values


def interleaved_observations(directory, taps):
    """Reconstruct consecutive clocks from the WAITX probe's two observer pairs."""
    data = analyze.words(directory)
    assert len(data) == 4128
    clocks, values = [], []
    for block in range(4):
        stamp = data[block * 129]
        assert all(data[cog * 516 + block * 129] == stamp for cog in range(1, 8))
        for sample in range(256):
            pool = 0
            pair = (2, 3) if sample % 2 == 0 else (4, 5)
            for index, group in enumerate(taps):
                sources = [entry for entry in group if entry[0] in pair]
                assert len(sources) == 1
                cog, bit, invert = sources[0]
                value = data[cog * 516 + block * 129 + 1 + sample // 2]
                pool |= (((value >> bit) & 1) ^ invert) << index
            clocks.append(((stamp - data[0]) & 0xffffffff) + sample)
            values.append(pool)
    return clocks, values


def linear_rows(clocks, constants, word=0):
    """Return observation rows mapping initial state to one state-word low bit."""
    columns = [1 << bit for bit in range(128)]
    clock = 0
    rows = []
    for target in clocks:
        while clock < target:
            columns = [step(value, constants) for value in columns]
            clock += 1
        rows.append(sum(((value >> (word * 64)) & 1) << bit for bit, value in enumerate(columns)))
    return rows


def eliminate(rows):
    """Find a basis and state-independent parity checks for the observations."""
    basis, checks = {}, []
    for index, row in enumerate(rows):
        history = 1 << index
        while row:
            pivot = row.bit_length() - 1
            if pivot not in basis:
                basis[pivot] = row, history
                break
            prior, used = basis[pivot]
            row ^= prior
            history ^= used
        if not row:
            checks.append(history)
    assert len(basis) == 128
    return basis, checks


def constraints(values, checks, timeout):
    """Fit eight low output taps using bit 7 of output * inverse(9) mod 256."""
    lines = [f"(set-option :timeout {timeout * 1000})", "(set-option :sat.anf true)",
             "(set-option :sat.anf.exlin true)", "(set-logic QF_BV)"]
    for bit in range(8):
        lines += [f"(declare-const i{bit} (_ BitVec 6))", f"(declare-const n{bit} (_ BitVec 1))"]
    lines.append("(assert (distinct " + " ".join(f"i{bit}" for bit in range(8)) + "))")
    for index, value in enumerate(values):
        bits = [f"(bvxor n{bit} ((_ extract 0 0) (bvlshr (_ bv{value} 64) ((_ zero_extend 58) i{bit}))))"
                for bit in reversed(range(8))]
        lines += [f"(define-fun o{index} () (_ BitVec 8) (concat " + " ".join(bits) + "))",
                  f"(define-fun r{index} () (_ BitVec 1) ((_ extract 7 7) (bvmul o{index} #x39)))"]
    for check in checks:
        terms = [f"r{index}" for index in range(len(values)) if check >> index & 1]
        lines.append("(assert (= (bvxor " + " ".join(terms) + ") #b0))")
    lines += ["(check-sat)", "(get-value (" + " ".join(f"i{bit} n{bit}" for bit in range(8)) + "))"]
    return "\n".join(lines) + "\n"


def history_constraints(clocks, values, constants, timeout):
    """Fit full state to unordered true/complement output histories."""
    count = len(values)
    a, b, c = constants
    lines = [f"(set-option :timeout {timeout * 1000})", "(set-option :sat.anf true)",
             "(set-option :sat.anf.exlin true)", "(set-logic QF_BV)",
             "(declare-const a0 (_ BitVec 64))", "(declare-const b0 (_ BitVec 64))"]
    clock = 0
    for index, target in enumerate(clocks):
        while clock < target:
            lines += [f"(define-fun q{clock} () (_ BitVec 64) (bvxor a{clock} b{clock}))",
                      f"(define-fun a{clock + 1} () (_ BitVec 64) (bvxor ((_ rotate_left {a}) a{clock}) q{clock} (bvshl q{clock} (_ bv{b} 64))))",
                      f"(define-fun b{clock + 1} () (_ BitVec 64) ((_ rotate_left {c}) q{clock}))"]
            clock += 1
        lines.append(f"(define-fun o{index} () (_ BitVec 64) (bvmul ((_ rotate_left 7) (bvmul a{clock} (_ bv5 64))) (_ bv9 64)))")
    patterns = set()
    for tap in range(64):
        history = sum(((value >> tap) & 1) << index for index, value in enumerate(values))
        patterns.update((history, history ^ ((1 << count) - 1)))
    for bit in range(64):
        history = "(concat " + " ".join(f"((_ extract {bit} {bit}) o{index})" for index in reversed(range(count))) + ")"
        lines += [f"(define-fun h{bit} () (_ BitVec {count}) {history})",
                  "(assert (or " + " ".join(f"(= h{bit} (_ bv{pattern} {count}))" for pattern in sorted(patterns)) + "))"]
    lines += ["(check-sat)", "(get-value (a0 b0))"]
    return "\n".join(lines) + "\n"


def recover_state(basis, low_bits):
    """Solve the triangular observation basis for the candidate initial state."""
    history = sum(bit << index for index, bit in enumerate(low_bits))
    state = 0
    for pivot in sorted(basis):
        row, used = basis[pivot]
        rhs = (history & used).bit_count() & 1
        state |= (rhs ^ ((state & row).bit_count() & 1)) << pivot
    return state


def full_mapping(state, clocks, values, constants, word):
    """Require the recovered state to predict all 64 anonymous tap histories."""
    expected = []
    clock = 0
    for target in clocks:
        while clock < target:
            state = step(state, constants)
            clock += 1
        expected.append(scramble((state >> (word * 64)) & MASK))
    patterns = {}
    for bit in range(64):
        pattern = tuple((value >> bit) & 1 for value in expected)
        patterns[pattern] = [bit, False]
        patterns[tuple(value ^ 1 for value in pattern)] = [bit, True]
    mapping = [patterns.get(tuple((value >> tap) & 1 for value in values)) for tap in range(64)]
    return mapping if all(entry is not None for entry in mapping) and len({entry[0] for entry in mapping}) == 64 else None


def self_check():
    """Check the recovery algebra against a synthetic permuted/inverted stream."""
    constants = (24, 16, 37)
    # Independent seed-1 vectors published by TonyB_ in the P2 discussion p65.
    expected = (0x1680, 0x1696801680, 0xe682e68000001680, 0x800016f099c09692,
                0xdb9ce96699c368c0, 0xacd4a897e816f3bc, 0x736225bd8540f37d, 0xd5ef4bc8db65564a,
                0xe6844f92c4467f20, 0xecbfdd2089c19276, 0x3e3c8f32277dc824, 0x6135641f1dadd2ca,
                0x0695119b0de23cfa, 0x8837ded221b7094d, 0x99f5fdc2c04a92d6, 0x57262a94b712228c)
    state = 1
    for value in expected:
        assert scramble(state & MASK) == value
        state = step(state, constants)
    clocks = list(range(0, 512, 2))
    basis, checks = eliminate(linear_rows(clocks, constants))
    initial = state = 0x123456789abcdef0123456789abcdef0
    clock, outputs, low_bits = 0, [], []
    mapping = [[(tap * 13 + 7) % 64, bool(tap & 1)] for tap in range(64)]
    for target in clocks:
        while clock < target:
            state = step(state, constants)
            clock += 1
        value = scramble(state & MASK)
        low_bits.append((value * 57 >> 7) & 1)
        outputs.append(sum((((value >> bit) & 1) ^ invert) << tap
                           for tap, (bit, invert) in enumerate(mapping)))
    assert recover_state(basis, low_bits) == initial
    history = sum(bit << index for index, bit in enumerate(low_bits))
    assert all((history & check).bit_count() % 2 == 0 for check in checks)
    assert full_mapping(initial, clocks, outputs, constants, 0) == mapping
    print("Synthetic state, parity checks and full tap-map recovery passed")


def main():
    """Save reproducible constraints and accept only a full-capture prediction."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("capture", type=Path, nargs="?")
    parser.add_argument("--constants", nargs=3, type=int, default=(24, 16, 37))
    parser.add_argument("--timeout", type=int, default=600)
    parser.add_argument("--output", type=Path)
    parser.add_argument("--self-check", action="store_true", help="Check recovery algebra without Z3 or hardware")
    parser.add_argument("--interleaved-taps", type=Path, help="Use WAITX observers with a previously calibrated tap table")
    parser.add_argument("--fit-samples", type=int, help="Fit a prefix, but verify every captured word before accepting")
    parser.add_argument("--strategy", choices=("low-taps", "word-histories"), default="low-taps")
    parser.add_argument("--result", type=Path, help="Read a prior Z3 result instead of invoking Z3")
    args = parser.parse_args()
    if args.self_check:
        self_check()
        return
    if args.capture is None or args.output is None:
        parser.error("capture and --output are required unless --self-check is selected")
    if args.interleaved_taps:
        taps = json.loads(args.interleaved_taps.read_text())
        clocks, values = interleaved_observations(args.capture, taps)
    else:
        cogs = analyze.capture(args.capture)
        taps = analyze.recover(cogs)
        clocks, values = observations(cogs, taps)
    full_word = args.strategy == "word-histories"
    count = (16 if full_word else len(values)) if args.fit_samples is None else args.fit_samples
    if not (8 if full_word else 128) <= count <= len(values):
        parser.error("--fit-samples is too small for the selected strategy or exceeds the capture")
    args.output.mkdir(parents=True, exist_ok=True)
    smt = args.output / "constraints.smt2"
    if full_word:
        source = history_constraints(clocks[:count], values[:count], args.constants, args.timeout)
    else:
        basis, checks = eliminate(linear_rows(clocks[:count], args.constants))
        source = constraints(values[:count], checks, args.timeout)
    smt.write_text(source)
    if args.result:
        result = args.result.read_text()
    else:
        result = subprocess.run(["z3", f"-T:{args.timeout + 5}", str(smt)], capture_output=True, text=True, check=False).stdout
    (args.output / "solver.txt").write_text(result)
    if not result.startswith("sat\n"):
        raise SystemExit("No recovered state: " + (result.splitlines()[0] if result else "solver produced no result"))
    fields = {name: int(value[2:], 2 if value.startswith("#b") else 16)
              for name, value in re.findall(r"\(([in]\d|[ab]0)\s+(#[bx][0-9a-f]+)\)", result)}
    if not full_word:
        low_bytes = [sum((((value >> fields[f"i{bit}"]) & 1) ^ fields[f"n{bit}"]) << bit
                         for bit in range(8)) for value in values]
        low_bits = [((value * 57) >> 7) & 1 for value in low_bytes]
    for word in range(1 if full_word else 2):
        if full_word:
            state = fields["a0"] | (fields["b0"] << 64)
        else:
            if word:
                basis, _ = eliminate(linear_rows(clocks[:count], args.constants, word))
            state = recover_state(basis, low_bits)
        mapping = full_mapping(state, clocks, values, args.constants, word)
        if mapping:
            model = {"constants": args.constants, "word": word, "state": [state & MASK, state >> 64],
                     "pool_mapping": mapping, "taps": taps, "capture": str(args.capture)}
            (args.output / "model.json").write_text(json.dumps(model, indent=2) + "\n")
            print(f"Full capture predicted: {len(values)} words/cog, hashed S{word}")
            return
    raise SystemExit("Candidate fit did not predict the full capture; no model accepted")


if __name__ == "__main__":
    main()
