#!/usr/bin/env python3
"""Summarize seed-hammer outliers without assigning canonical RNG bit numbers.

The probe visits baseline/4/5/6/7 seeders, eight observer cogs, eight bursts,
and 64 consecutive GETRND instructions per burst. HUBSET uses 0x80000000:
its payload is zero apart from the mandatory high seed-enable/state bit.
"""

import argparse
from collections import Counter
import json
from pathlib import Path

import analyze


def statistics(values):
    """Describe marginal bias, modal words and the joint biased-bit pattern."""
    count = len(values)
    ones = [sum((value >> bit) & 1 for value in values) for bit in range(32)]
    mask = sum(1 << bit for bit, total in enumerate(ones)
               if min(total, count - total) * 10 <= count)
    mode, frequency = Counter(value & mask for value in values).most_common(1)[0]
    histories = [sum(((value >> bit) & 1) << index for index, value in enumerate(values))
                 for bit in range(32)]
    pair_agreement = [max(total, count - total)
                      for a in range(32) for b in range(a + 1, 32)
                      for total in [(histories[a] ^ histories[b]).bit_count()]]
    return {"samples": count, "ones": ones, "biased_mask": f"{mask:08x}",
            "partial_mode": f"{mode:08x}" if mask else None,
            "partial_mode_count": frequency if mask else None,
            "full_word_mode_count": Counter(values).most_common(1)[0][1],
            "max_pair_agreement": max(pair_agreement),
            "strong_pairs": sum(total * 10 >= count * 9 for total in pair_agreement),
            "max_byte_mode_count": max(Counter((value >> (byte * 8)) & 255
                                               for value in values).most_common(1)[0][1]
                                       for byte in range(4))}


def summarize(data, additional=()):
    """Validate deadlines and pool observer words by actual CT phase across uploads."""
    captures = (data, *additional)
    for capture in captures:
        assert len(capture) == 5 * 8 * 8 * 65
        for burst in range(320):
            assert (capture[burst * 65] - capture[0]) & 0xffffffff == burst * 32769
    result = []
    for config, seeders in enumerate((0, 4, 5, 6, 7)):
        for cog in range(8):
            phases = [[] for _ in range(8)]
            values = []
            for capture in captures:
                for burst in range(8):
                    index = (config * 64 + cog * 8 + burst) * 65
                    stamp = capture[index]
                    for sample, value in enumerate(capture[index + 1:index + 65]):
                        phases[(stamp + 2 + 2 * sample) & 7].append(value)
                        values.append(value)
            result.append({"seeders": seeders, "observer": cog,
                           "overall": statistics(values),
                           "phases": [statistics(values) for values in phases]})
    return result


def self_check():
    """Check phase parsing, stamp wraparound, known outliers and unbiased input."""
    data = []
    for burst in range(320):
        data += [(0xffff0000 + burst * 32769) & 0xffffffff] + [0xaaaaaaaa] * 64
    summary = summarize(data)
    assert len(summary) == 40
    assert all(item["overall"]["biased_mask"] == "ffffffff" for item in summary)
    assert all(phase["samples"] == 64 and phase["partial_mode"] == "aaaaaaaa"
               for item in summary for phase in item["phases"])
    pooled = summarize(data, [data])
    assert all(phase["samples"] == 128 for item in pooled for phase in item["phases"])
    unbiased = statistics([0, 0xffffffff] * 32)
    assert unbiased["biased_mask"] == "00000000"
    assert unbiased["partial_mode_count"] is None
    # Balanced marginals can conceal an exactly repeated pairwise relation.
    assert unbiased["max_pair_agreement"] == 64
    assert unbiased["strong_pairs"] == 496
    print("Seed-hammer analyzer synthetic checks passed.")


def main():
    """Read retained raw frames, check their CRCs and emit phase-wise statistics."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("captures", nargs="*", type=Path)
    parser.add_argument("--self-check", action="store_true")
    parser.add_argument("--output", type=Path)
    parser.add_argument("--pooled-output", type=Path)
    args = parser.parse_args()
    if args.self_check:
        self_check()
        return
    if not args.captures:
        parser.error("provide capture directories or --self-check")
    captures = [analyze.words(path) for path in args.captures]
    output = {str(path): summarize(data) for path, data in zip(args.captures, captures)}
    if args.pooled_output:
        args.pooled_output.write_text(json.dumps(summarize(captures[0], captures[1:]), indent=2) + "\n")
    text = json.dumps(output, indent=2) + "\n"
    if args.output:
        args.output.write_text(text)
    else:
        print(text, end="")


if __name__ == "__main__":
    main()
