#!/usr/bin/env python3
"""Bounded physical experiments; independent captures are required for electrical claims."""

import argparse
import asyncio
import csv
import hashlib
import json
import math
import os
import select
import statistics
import struct
import subprocess
import termios
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
PORT = "/dev/serial/by-id/usb-Parallax_Inc_Propeller_P2-EVAL_PLX6ZJKWX-if00-port0"
ENDPOINT = "ws://localhost:12880/"
FINDINGS = ROOT / "docs/measurements/physical-findings.md"


def pll_mode(divider, multiplier, postdivider):
    """Encode decoded D/M/P values, rejecting invalid fields before touching hardware."""
    if not 1 <= divider <= 64 or not 1 <= multiplier <= 1024:
        raise ValueError("D must be 1..64 and M must be 1..1024")
    if postdivider != 1 and (postdivider % 2 or not 2 <= postdivider <= 30):
        raise ValueError("P must be 1 or an even integer 2..30")
    p = 15 if postdivider == 1 else postdivider // 2 - 1
    return 1 << 24 | (divider - 1) << 18 | (multiplier - 1) << 8 | p << 4 | 0xB


def signature(iterations):
    """Compute the arithmetic/hub trial's expected result independently on the host."""
    value = 0x12345678
    for _ in range(iterations):
        value = ((value << 5) | (value >> 27)) & 0xFFFFFFFF
        value = ((value ^ 0x9E3779B9) + 0x7F4A7C15) & 0xFFFFFFFF
        value = value * 2 & 0xFFFFFFFF
    return value


def serial_exact(fd, length, timeout=15):
    """Read a complete UART response without blocking forever on a failed clock trial."""
    data = bytearray()
    deadline = time.monotonic() + timeout
    while len(data) < length:
        if not select.select([fd], [], [], max(0, deadline - time.monotonic()))[0]:
            raise TimeoutError(f"UART returned {len(data)}/{length} bytes")
        chunk = os.read(fd, length - len(data))
        if not chunk:
            raise EOFError("UART disconnected")
        data.extend(chunk)
    return bytes(data)


class Rig:
    """Own both designated board transports and release all experiment outputs on exit."""

    def __init__(self, directory, endpoint, observer_port):
        """Remember the artifact directory and explicit hardware transport addresses."""
        self.directory, self.endpoint, self.observer_port = (
            directory,
            endpoint,
            observer_port,
        )
        self.ws, self.fd = None, None
        self.pending = bytearray()
        self.original_termios = None

    async def __aenter__(self):
        """Assemble and upload one finite-command monitor, cleaning partial setup failures."""
        import websockets

        self.directory.mkdir(parents=True, exist_ok=True)
        image_path = self.directory / "electrical-monitor.bin"
        await asyncio.to_thread(
            subprocess.run,
            [
                str(ROOT / "zig-out/bin/propan"),
                "-o",
                str(image_path),
                str(Path(__file__).with_name("electrical-monitor.propan")),
            ],
            check=True,
        )
        image = image_path.read_bytes()
        digest = hashlib.sha256(image).hexdigest()
        (self.directory / "firmware-sha256.txt").write_text(digest + "\n")
        (self.directory / f"firmware-{digest}.bin").write_bytes(image)
        (self.directory / f"firmware-{digest}.propan").write_text(
            Path(__file__).with_name("electrical-monitor.propan").read_text()
        )
        try:
            self.fd = os.open(
                self.observer_port, os.O_RDWR | os.O_NOCTTY | os.O_NONBLOCK
            )
            self.original_termios = termios.tcgetattr(self.fd)
            await asyncio.to_thread(
                subprocess.run,
                [
                    str(ROOT / "zig-out/bin/turboprop"),
                    "-P",
                    self.observer_port,
                    "-b",
                    "2000000",
                    str(image_path),
                ],
                check=True,
                timeout=10,
            )
            settings = termios.tcgetattr(self.fd)
            settings[0] = settings[1] = settings[3] = 0
            settings[2] = termios.CS8 | termios.CREAD | termios.CLOCAL
            settings[4] = settings[5] = termios.B2000000
            settings[6][termios.VMIN] = settings[6][termios.VTIME] = 0
            termios.tcsetattr(self.fd, termios.TCSANOW, settings)
            if await asyncio.to_thread(serial_exact, self.fd, 1) != b"!":
                raise RuntimeError("Observer handshake failed")
            self.ws = await websockets.connect(
                self.endpoint.rstrip("/") + "/?baudrate=2000000&timeout_ms=60000",
                max_size=None,
            )
            await self.ws.send(struct.pack("<I", len(image)) + image)
            if await self.receive(1) != b"!":
                raise RuntimeError("Primary handshake failed")
            return self
        except BaseException:
            await self.__aexit__(None, None, None)
            raise

    async def receive(self, length):
        """Accumulate fragmented P2AAS UART bytes to the requested exact length."""
        while len(self.pending) < length:
            self.pending.extend(await asyncio.wait_for(self.ws.recv(), 15))
        data = bytes(self.pending[:length])
        del self.pending[:length]
        return data

    async def call(self, board, command, args=(), size=1):
        """Exchange one binary monitor command; frames and timeouts remain explicit."""
        packet = command.encode() + struct.pack("<" + "I" * len(args), *args)
        if board == 0:
            await self.ws.send(packet)
            return await self.receive(size)
        sent = 0
        while sent < len(packet):
            try:
                sent += os.write(self.fd, packet[sent:])
            except BlockingIOError:
                await asyncio.sleep(0.001)
        return await asyncio.to_thread(serial_exact, self.fd, size)

    async def clock(
        self,
        board,
        mode,
        ticks,
        increment=1 << 22,
        work=1000,
        from_mode=0,
        from_ticks=0,
        settle_ticks=1000000,
    ):
        """Run one clock trial and return a functional result, not a frequency measurement."""
        command = "T" if from_mode or settle_ticks != 1000000 else "K"
        args = (mode, ticks, increment, work)
        if command == "T":
            args = (from_mode, from_ticks, settle_ticks, *args)
        data = await self.call(board, command, args, 17)
        if data[:1] != b"K":
            raise RuntimeError(f"Clock framing error: {data.hex()}")
        observed, failures, elapsed, checksum = struct.unpack("<4I", data[1:])
        if checksum != (((observed + failures + elapsed) & 0xFFFFFFFF) ^ 0x5A5A5A5A):
            raise RuntimeError("Clock response integrity check failed")
        return {
            "mode": mode,
            "work": work,
            "expected_signature": signature(work),
            "observed_signature": observed,
            "hub_failures": failures,
            "trial_ticks": elapsed,
            "functional_pass": observed == signature(work) and failures == 0,
            "transport_additive_checksum": checksum,
        }

    async def __aexit__(self, *_):
        """Attempt cleanup independently on each board and always close/restore transports."""
        from websockets.exceptions import ConnectionClosed

        for board, available in ((0, self.ws is not None), (1, self.fd is not None)):
            if available:
                try:
                    reply = await asyncio.wait_for(self.call(board, "F"), 3)
                    if reply != b".":
                        raise RuntimeError(f"Cleanup reply {reply!r}")
                    print(f"cleanup board{board}: acknowledged", flush=True)
                except (
                    OSError,
                    TimeoutError,
                    RuntimeError,
                    EOFError,
                    ConnectionClosed,
                ) as error:
                    print(f"CLEANUP NOT CONFIRMED board{board}: {error}", flush=True)
        if self.ws is not None:
            await self.ws.close()
        if self.fd is not None:
            if self.original_termios is not None:
                termios.tcsetattr(self.fd, termios.TCSANOW, self.original_termios)
            os.close(self.fd)


def record(directory, name, value):
    """Save each result immediately, including failures; append-only JSONL preserves earlier trials."""
    path = directory / (name + ".jsonl")
    manifest = directory / "firmware-sha256.txt"
    value = {
        "host_utc": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "firmware_sha256": manifest.read_text().strip() if manifest.exists() else None,
        **value,
    }
    with path.open("a") as output:
        output.write(json.dumps(value, sort_keys=True) + "\n")
        output.flush()
    print(json.dumps(value), flush=True)


def finding(title, body):
    """Persist an established observation and its correctness limits before continuing."""
    with FINDINGS.open("a") as output:
        output.write(
            f"\n## {time.strftime('%Y-%m-%d %H:%M:%S UTC', time.gmtime())}: {title}\n\n{body}\n"
        )
        output.flush()


async def rc_campaign(args):
    """Measure each board's oscillators against the other board's direct crystal clock."""
    async with Rig(args.artifacts, args.endpoint, args.observer_port) as rig:
        for source in (0, 1):
            for name, mode, divider, periods, nominal in (
                ("crystal-control", 0xFA, 1024, 1024, 20_000_000),
                ("rcfast", 0xF0, 1024, 2048, 20_000_000),
                ("rcslow", 0xF1, 16, 128, 20_000),
            ):
                for repeat in range(args.repeats):
                    observer = asyncio.create_task(
                        rig.call(1 - source, "O", (periods,), 9)
                    )
                    await asyncio.sleep(0.08)
                    trial = await rig.clock(
                        source, mode, int(nominal * 0.30), (1 << 32) // divider, 0
                    )
                    data = await observer
                    if data[:1] != b"O":
                        raise RuntimeError(f"Observer framing: {data.hex()}")
                    ticks, failed = struct.unpack("<2I", data[1:])
                    item = {
                        "source_board": source,
                        "observer_board": 1 - source,
                        "oscillator": name,
                        "repeat": repeat,
                        "divider": divider,
                        "periods": periods,
                        "observer_crystal_ticks": ticks,
                        "observer_timeout": bool(failed),
                        "source_trial": trial,
                        "reference": "observer direct crystal, nominal20MHz, uncalibrated",
                    }
                    if not failed and ticks:
                        item["frequency_hz_relative_to_nominal_crystal"] = (
                            periods * divider * 20_000_000 / ticks
                        )
                    record(args.artifacts, "rc", item)
                    if failed:
                        raise RuntimeError(
                            "Independent observer timed out; no frequency claimed"
                        )
                    if (
                        name == "crystal-control"
                        and abs(
                            item["frequency_hz_relative_to_nominal_crystal"]
                            / 20_000_000
                            - 1
                        )
                        > 0.001
                    ):
                        raise RuntimeError(
                            "Crystal control differs by >0.1%; reject this RC campaign"
                        )
                    finding(
                        f"Board{source} {name}, observation{repeat + 1}",
                        f"**Measured ratio / bounded absolute accuracy.** {periods} periods of clock/{divider} "
                        f"occupied {ticks} direct-crystal ticks on board{1 - source}. "
                        f"Equivalent frequency at nominal20MHz reference: "
                        f"{item['frequency_hz_relative_to_nominal_crystal']:.3f}Hz. "
                        "Reference crystal tolerance/calibration unknown; polling edge timing and board routing "
                        "are included. This is not an absolute ppm calibration or a jitter measurement. "
                        f"Evidence: `{args.artifacts / 'rc.jsonl'}`; firmware SHA256 recorded alongside it.",
                    )


async def clocks_campaign(args):
    """Emit one marker-framed clock per configuration; independent analyzer capture supplies frequency."""
    if args.command == "smoke":
        cases = [(1, 8, 1), (2, 16, 1), (4, 32, 1), (1, 16, 2), (1, 16, 1)]
    elif args.command == "cpu":
        cases = [(1, f // 20, 1) for f in range(40, args.max_mhz + 1, 20)]
    else:
        cases = []
        for d in args.dividers:
            for f in args.vco_mhz:
                m = round(f * d / 20)
                if 1 <= m <= 1024:
                    p = 1 if f <= 200 else 2
                    cases.append((d, m, p))
    if args.postdividers:
        cases = [(d, m, p) for d, m, _ in cases for p in args.postdividers]
    cases *= args.repeats
    for batch_start in range(0, len(cases), 25):
        async with Rig(args.artifacts, args.endpoint, args.observer_port) as rig:
            for case_index, (d, m, p) in enumerate(
                cases[batch_start : batch_start + 25], batch_start
            ):
                f = 20_000_000 * m / d / p
                mode = pll_mode(d, m, p)
                item = {
                    "board": args.board,
                    "case_index": case_index,
                    "D": d,
                    "M": m,
                    "P": p,
                    "nominal_vco_hz": 20_000_000 * m / d,
                    "nominal_cpu_hz": f,
                    "independent_frequency": "pending analyzer/scope capture",
                    "thermal_monitor": "not connected; short functional trial only",
                }
                if 20_000_000 * m / d > 400_000_000 or f > 400_000_000:
                    raise ValueError("400MHz hard cap exceeded")
                try:
                    result = await rig.clock(
                        args.board, mode, int(f * args.hold_ms / 1000), work=args.work
                    )
                    item.update(result)
                    record(args.artifacts, args.command, item)
                    finding(
                        f"Board{args.board} functional clock trial{case_index}",
                        f"**Functional only.** D={d},M={m},P={p}; nominal CPU{f:.3f}Hz; "
                        f"host-known signature={result['observed_signature']}, "
                        f"hub mismatches={result['hub_failures']}, pass={result['functional_pass']}. "
                        "Independent frequency remains pending capture analysis; short trial is not endurance/thermal validation. "
                        f"Evidence: `{args.artifacts / (args.command + '.jsonl')}`.",
                    )
                    if not result["functional_pass"]:
                        raise RuntimeError(
                            "Host-known signature failed; stop this batch"
                        )
                except Exception as error:
                    record(
                        args.artifacts,
                        args.command,
                        {**item, "error": repr(error), "functional_pass": False},
                    )
                    finding(
                        "Clock configuration did not complete the functional trial",
                        f"**Functional failure, cause unresolved.** D={d},M={m},P={p}; "
                        f"board{args.board}: `{error}`. This is not proof of PLL unlock or a CPU speed boundary. "
                        f"Evidence: `{args.artifacts / (args.command + '.jsonl')}`. Remaining batch cases were not tested.",
                    )
                    raise


async def transition_campaign(args):
    """Exercise independent D/M/P changes and startup waits with externally captured trial markers."""
    transitions = [
        ((1, 8, 1), (1, 10, 1), "M-up"),
        ((1, 10, 1), (1, 8, 1), "M-down"),
        ((1, 8, 1), (2, 8, 1), "D-only"),
        ((1, 8, 1), (2, 16, 1), "D-M-same-frequency"),
        ((1, 16, 2), (1, 16, 1), "P-up"),
        ((1, 16, 1), (1, 16, 2), "P-down"),
    ]
    transitions = [case for case in transitions if case[2] in args.transition_kinds]
    cases = [(before, after, label, 1000000) for before, after, label in transitions]
    if args.command == "startup":
        cases = [
            (None, (1, 8, 1), "startup", wait)
            for wait in (0, 128, 512, 2048, 8192, 32768, 131072, 524288)
        ]
    cases *= args.repeats
    for batch in range(0, len(cases), 25):
        async with Rig(args.artifacts, args.endpoint, args.observer_port) as rig:
            for index, (before, after, label, wait) in enumerate(
                cases[batch : batch + 25], batch
            ):
                d, m, p = after
                target_hz = 20_000_000 * m / d / p
                source_hz = (
                    0
                    if before is None
                    else 20_000_000 * before[1] / before[0] / before[2]
                )
                item = {
                    "case_index": index,
                    "board": args.board,
                    "kind": label,
                    "before": before,
                    "after": after,
                    "startup_wait_rc_ticks": wait,
                    "nominal_target_hz": target_hz,
                    "settling": "pending independent capture",
                }
                if args.command == "startup":
                    # Give crystal/PLL enable a reproducible off interval between trials.
                    await asyncio.sleep(0.05)
                    item["minimum_host_off_interval_s"] = 0.05
                try:
                    item.update(
                        await rig.clock(
                            args.board,
                            pll_mode(d, m, p),
                            int(target_hz * args.hold_ms / 1000),
                            work=0,
                            from_mode=0 if before is None else pll_mode(*before),
                            from_ticks=int(source_hz * 0.01),
                            settle_ticks=wait,
                        )
                    )
                    record(args.artifacts, args.command, item)
                    finding(
                        f"Board{args.board} {label} trial{index}",
                        f"**Functional only.** Origin{before},target{after},wait{wait}RC clocks; "
                        f"returned={item['functional_pass']}. Observable settling is not yet analyzed; "
                        f"internal PLL lock is not observed. Evidence: `{args.artifacts / (args.command + '.jsonl')}`.",
                    )
                except Exception as error:
                    record(args.artifacts, args.command, {**item, "error": repr(error)})
                    finding(
                        "Clock transition failed to return",
                        f"**Combined failure.** {item}; error `{error}`. Stop and reset/reload; "
                        "not an isolated PLL lock-time measurement.",
                    )
                    raise


async def adc_campaign(args):
    """Acquire internal calibration diagnostics, never using them as independent voltage references."""
    for board in (0, 1):
        async with Rig(args.artifacts, args.endpoint, args.observer_port) as rig:
            for source, name in (
                (0, "internal-gio"),
                (1, "internal-vio"),
                (2, "internal-float"),
            ):
                for exponent in args.exponents:
                    configuration = 0x103F30 | source << 15
                    data = await rig.call(
                        board,
                        "A",
                        (configuration, exponent, args.samples),
                        9 + 4 * args.samples,
                    )
                    if data[:1] != b"A":
                        raise RuntimeError("ADC framing error")
                    ticks = struct.unpack("<I", data[1:5])[0]
                    checksum = struct.unpack("<I", data[5:9])[0]
                    values = struct.unpack("<" + "I" * args.samples, data[9:])
                    if checksum != (
                        ((sum(values) + ticks + args.samples) & 0xFFFFFFFF) ^ 0x5A5A5A5A
                    ):
                        raise RuntimeError("ADC payload integrity check failed")
                    name_path = f"adc-board{board}-{name}-n{exponent}.u32"
                    (args.artifacts / name_path).write_bytes(data[9:])
                    item = {
                        "board": board,
                        "input": name,
                        "configuration": configuration,
                        "period_exponent": exponent,
                        "samples": args.samples,
                        "elapsed_rc_ticks": ticks,
                        "transport_additive_checksum": checksum,
                        "mean_counts": statistics.mean(values),
                        "rms_counts": statistics.pstdev(values),
                        "minimum": min(values),
                        "maximum": max(values),
                        "raw": name_path,
                        "quality": "internal calibration diagnostic, not ENOB/linearity",
                    }
                    record(args.artifacts, "adc", item)
                    finding(
                        f"Board{board} {name}, automatic SINC2 X={exponent}",
                        f"**Combined internal diagnostic.** {args.samples} samples: mean "
                        f"{item['mean_counts']:.6f}, population RMS {item['rms_counts']:.6f} raw counts; "
                        f"range{min(values)}..{max(values)}; UART additive checksum verified (not CRC). RC timing, modulator, filter and internal-source "
                        "behavior are combined. No independent voltage, input-referred noise, DC resolution, "
                        f"linearity or ENOB claim. Evidence: `{args.artifacts / name_path}` and `adc.jsonl`.",
                    )


async def dac_campaign(args):
    """Play one finite DAC stimulus for the independent scope, then confirm cleanup."""
    async with Rig(args.artifacts, args.endpoint, args.observer_port) as rig:
        config = 0x140000 | args.range << 16
        reply = await rig.call(
            args.board, "D", (config, args.dwell_ticks, args.cycles, args.pattern)
        )
        record(
            args.artifacts,
            "dac-stimulus",
            {
                "board": args.board,
                "configuration": config,
                "dwell_ticks": args.dwell_ticks,
                "cycles": args.cycles,
                "pattern": args.pattern,
                "reply": reply.decode(errors="replace"),
                "quality": "stimulus only; scope result required",
            },
        )
        if reply != b"D":
            raise RuntimeError("DAC completion missing")


async def dac_engine_campaign(args):
    """Run a bounded DAC engine stimulus; independent scope samples establish any electrical result."""
    async with Rig(args.artifacts, args.endpoint, args.observer_port) as rig:
        config = 0x140000 | args.range << 16
        engine = ("setdacs", "streamer", "pwm", "random").index(args.engine)
        print(f"DAC engine {args.engine} starting", flush=True)
        reply = await rig.call(
            args.board,
            "E",
            (config, engine, args.nco, args.duration_ticks, args.payload),
        )
        record(
            args.artifacts,
            "dac-engine-stimulus",
            {
                "board": args.board,
                "configuration": config,
                "engine": args.engine,
                "nco_increment": args.nco,
                "duration_rc_ticks": args.duration_ticks,
                "payload": args.payload,
                "reply": reply.decode(errors="replace"),
                "quality": "stimulus only; independent scope result required",
            },
        )
        if reply != b"E":
            raise RuntimeError("DAC engine completion missing")


async def load_campaign(args):
    """Run the confirmed resistor fixture for DAC loading or capacitor charge/discharge observations."""
    modes = {
        "external": (0, 0),
        "float": (2, 0),
        "1k5": (1, 0x900),
        "15k": (1, 0x1200),
        "150k": (1, 0x1B00),
        "1ma": (1, 0x2400),
        "100ua": (1, 0x2D00),
        "10ua": (1, 0x3600),
        "dac-chop": (3, 0x140000 | args.range << 16 | args.dac_code << 8),
    }
    pattern, config = modes[args.load_mode]
    async with Rig(args.artifacts, args.endpoint, args.observer_port) as rig:
        print(f"Capacitor fixture {args.load_mode} starting", flush=True)
        reply = await rig.call(
            0, "G", (config, args.load_half_ticks, args.load_cycles, pattern)
        )
        record(
            args.artifacts,
            "load-stimulus",
            {
                "mode": args.load_mode,
                "configuration": config,
                "half_rc_ticks": args.load_half_ticks,
                "cycles": args.load_cycles,
                "reply": reply.decode(errors="replace"),
                "fixture": "P18--4k7--P22; scope CH1 P22, CH2 P18; "
                + (
                    "NO capacitor"
                    if args.load_mode == "dac-chop"
                    else "cap positive P22, negative GND"
                ),
                "quality": "stimulus only; independent scope result required; "
                + (
                    "qualify AC droop and pickup"
                    if args.load_mode == "dac-chop"
                    else "qualify effective capacitance"
                ),
            },
        )
        if reply != b"G":
            raise RuntimeError("Capacitor fixture completion missing")


def analyze_logic(path, nominal_sample_rate):
    """Fit rising edges inside P1 trial windows without including startup and return-to-RC intervals."""
    windows, edges = [], []
    previous = [0, 0]
    start = None
    with path.open() as stream:
        reader = csv.DictReader(stream)
        for row in reader:
            stamp = float(row["Time [s]"])
            current = [int(row["Channel 0"]), int(row["Channel 1"])]
            if current[1] and not previous[1]:
                start, edges = stamp, []
            if current[0] and not previous[0] and start is not None:
                edges.append(stamp)
            if not current[1] and previous[1] and start is not None:
                retained = [x for x in edges if start + 0.001 < x < stamp - 0.001]
                result = {
                    "marker_start_s": start,
                    "marker_end_s": stamp,
                    "rising_edges": len(retained),
                }
                if len(retained) >= 3:
                    n = len(retained)
                    centered = (n - 1) / 2
                    denominator = n * (n * n - 1) / 12
                    slope = (
                        sum(
                            (i - centered) * (x - retained[0])
                            for i, x in enumerate(retained)
                        )
                        / denominator
                    )
                    result["cpu_hz_from_clock_div1024"] = 1024 / slope
                    result["edge_quantization_endpoint_bound_relative"] = (
                        2 / nominal_sample_rate / (retained[-1] - retained[0])
                    )
                    result["quality"] = (
                        "frequency fit; analyzer timebase/threshold uncertainty additional; not VCO jitter"
                    )
                windows.append(result)
                start = None
            previous = current
    return windows


def self_test():
    """Check field encoding, independent signatures and analyzer fitting on a known synthetic trace."""
    import tempfile

    assert pll_mode(1, 10, 1) == 0x010009FB
    assert pll_mode(40, 297, 2) == 0x019D280B
    for invalid in ((0, 1, 1), (65, 1, 1), (1, 1025, 1), (1, 1, 3)):
        try:
            pll_mode(*invalid)
        except ValueError:
            continue
        raise AssertionError(invalid)
    assert signature(0) == 0x12345678
    with tempfile.TemporaryDirectory() as directory:
        p = Path(directory) / "digital.csv"
        with p.open("w") as output:
            output.write("Time [s],Channel 0,Channel 1\n0,0,0\n0.001,0,1\n")
            for i in range(1000):
                t = 0.002 + i * 0.00001
                output.write(f"{t},1,1\n{t + 0.000005},0,1\n")
            output.write("0.013,0,0\n")
        result = analyze_logic(p, 25_000_000)
        assert len(result) == 1
        assert math.isclose(
            result[0]["cpu_hz_from_clock_div1024"], 102_400_000, rel_tol=1e-10
        )
    print("self-test PASS")


def main():
    """Expose explicit campaigns, safe parameter bounds and offline validation without hardware imports."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "command",
        choices=(
            "self-test",
            "rc",
            "smoke",
            "pll",
            "cpu",
            "adc",
            "dac",
            "dac-engine",
            "load",
            "logic",
            "transition",
            "startup",
        ),
    )
    parser.add_argument(
        "--artifacts", type=Path, default=ROOT / "captures/physical/2026-10-17"
    )
    parser.add_argument("--endpoint", default=ENDPOINT)
    parser.add_argument("--observer-port", default=PORT)
    parser.add_argument("--board", type=int, choices=(0, 1), default=0)
    parser.add_argument("--repeats", type=int, default=3)
    parser.add_argument(
        "--transition-kinds",
        nargs="+",
        choices=("M-up", "M-down", "D-only", "D-M-same-frequency", "P-up", "P-down"),
        default=["M-up", "M-down", "D-only", "D-M-same-frequency", "P-down"],
        help="Live P-up caused corruption/hang; select it explicitly only for bounded reproduction",
    )
    parser.add_argument("--max-mhz", type=int, default=320)
    parser.add_argument("--hold-ms", type=float, default=20)
    parser.add_argument("--work", type=int, default=1000)
    parser.add_argument(
        "--dividers", type=int, nargs="+", default=[1, 2, 4, 8, 16, 32, 64]
    )
    parser.add_argument(
        "--vco-mhz",
        type=int,
        nargs="+",
        default=[100, 160, 200, 240, 280, 320, 360, 400],
    )
    parser.add_argument("--postdividers", type=int, nargs="+")
    parser.add_argument("--exponents", type=int, nargs="+", default=[8, 10, 12])
    parser.add_argument("--samples", type=int, default=32768)
    parser.add_argument("--range", type=int, choices=range(4), default=2)
    parser.add_argument("--dwell-ticks", type=int, default=200000)
    parser.add_argument("--cycles", type=int, default=200)
    parser.add_argument("--pattern", type=int, choices=range(5), default=0)
    parser.add_argument(
        "--engine", choices=("setdacs", "streamer", "pwm", "random"), default="streamer"
    )
    parser.add_argument("--nco", type=lambda text: int(text, 0), default=0x80000000)
    parser.add_argument("--payload", type=lambda text: int(text, 0), default=0xFFAA5500)
    parser.add_argument("--duration-ticks", type=int, default=200000000)
    parser.add_argument(
        "--load-mode",
        choices=(
            "external",
            "float",
            "1k5",
            "15k",
            "150k",
            "1ma",
            "100ua",
            "10ua",
            "dac-chop",
        ),
        default="external",
    )
    parser.add_argument("--load-half-ticks", type=int, default=10000000)
    parser.add_argument("--load-cycles", type=int, default=8)
    parser.add_argument("--dac-code", type=int, choices=range(256), default=128)
    parser.add_argument("--csv", type=Path)
    parser.add_argument("--sample-rate", type=int, default=25000000)
    args = parser.parse_args()
    if (
        not 1 <= args.load_cycles <= (100000 if args.load_mode == "dac-chop" else 32)
        or not (1000 if args.load_mode == "dac-chop" else 100000)
        <= args.load_half_ticks
        <= 150000000
    ):
        parser.error("Capacitor fixture parameter bounds exceeded")
    if args.command == "load":
        ticks = (
            (args.load_half_ticks + 30000000)
            if args.load_mode == "float"
            else (2 * args.load_half_ticks * args.load_cycles + 20000000)
        )
        if ticks > 200000000 or (args.load_mode == "float" and args.load_cycles != 1):
            parser.error(
                "Capacitor fixture exceeds bounded duration; float requires --load-cycles 1"
            )
    if not 4 <= args.duration_ticks <= 200000000 or not 1 <= args.nco <= 0x80000000:
        parser.error("DAC engine duration/NCO bounds exceeded")
    if (
        not 0
        <= args.payload
        <= (0xFFFF if args.engine in ("pwm", "random") else 0xFFFFFFFF)
    ):
        parser.error("DAC engine payload exceeds its input width")
    if (
        not 1 <= args.repeats <= 100
        or not 1 <= args.samples <= 32768
        or not 0 <= args.work <= 100000
    ):
        parser.error("repeat/sample/work bounds exceeded")
    if not 1 <= args.max_mhz <= 400 or not 1 <= args.hold_ms <= 500:
        parser.error("CPU400MHz / hold500ms hard caps exceeded")
    if any(n not in (8, 10, 12) for n in args.exponents):
        parser.error("supported ADC exponents:8,10,12")
    if not 1 <= args.cycles <= 2000000 or not 1 <= args.dwell_ticks <= 10000000:
        parser.error("DAC parameter bounds exceeded")
    levels = 256 if args.pattern >= 2 else 4
    if levels * args.cycles * (args.dwell_ticks + 64) > 400000000:
        parser.error("DAC pattern exceeds twenty nominal RC seconds")
    if args.command == "self-test":
        self_test()
    elif args.command == "logic":
        if args.csv is None:
            parser.error("--csv is required")
        print(json.dumps(analyze_logic(args.csv, args.sample_rate), indent=2))
    else:
        actions = {
            "rc": rc_campaign,
            "adc": adc_campaign,
            "dac": dac_campaign,
            "dac-engine": dac_engine_campaign,
            "load": load_campaign,
            "transition": transition_campaign,
            "startup": transition_campaign,
        }
        asyncio.run(actions.get(args.command, clocks_campaign)(args))


if __name__ == "__main__":
    main()
