#!/usr/bin/env python

from dataclasses import dataclass, field
from pathlib import Path
import sys
import asyncio
import io
import os
import struct
import urllib.parse
from tempfile import NamedTemporaryFile
from subprocess import CalledProcessError, run, Popen, PIPE
from contextlib import contextmanager
from typing import BinaryIO, Generic, Iterable, TypeVar

import yaml
from yaml import safe_load

SELF_DIR = Path(__file__).absolute().parent
REPO_ROOT = SELF_DIR.parent.parent

FIXTURE_FILE = SELF_DIR / "fixture.propan"
TESTCASE_FILE = SELF_DIR / "test-cases.yaml"
TESTRESULT_FILE = SELF_DIR / "test-results.yaml"
PROPAN_EXE = REPO_ROOT / "zig-out" / "bin" / "propan"

FIXTURE_PREFIX = FIXTURE_FILE.read_text(encoding="utf-8")

PORT = "/dev/serial/by-id/usb-FTDI_FT231X_USB_UART_DUAB9RPU-if00-port0"


def patch_code(code: str) -> str:

    return "\n".join(
        ("" if line.endswith(":") else " " * 8) + line.rstrip()
        for line in code.splitlines()
    )


def compile_fixture(
    *,
    prepare_code: str,
    dut_code: str,
    data_segment: str,
) -> bytes:

    fixture_text = FIXTURE_PREFIX

    fixture_text = fixture_text.replace(
        "// <<FIXTURE:PREPARE>>", patch_code(prepare_code)
    )
    fixture_text = fixture_text.replace("// <<FIXTURE:DUT>>", patch_code(dut_code))
    fixture_text = fixture_text.replace("// <<FIXTURE:DATA>>", patch_code(data_segment))

    try:
        output = run(
            args=[
                str(PROPAN_EXE),
                "-o",
                "-",
                "-",
            ],
            input=fixture_text.encode('utf-8'),
            capture_output=True,
            text=False,
            check=True,
        )
        return output.stdout
    except CalledProcessError as err:
        if err.stdout:
            print(err.stdout, file=sys.stderr, end="")
        if err.stderr:
            print(err.stderr.decode(), file=sys.stderr, end="")
        print(repr(err), file=sys.stderr)
        # print(f"fixture source: {source_file.name}", file=sys.stderr)
        raise


@dataclass(kw_only=True, frozen=True, slots=True)
class Result:
    launch_ct: int
    "The value of CT[31:0] at the first DUT instruction"

    offset: int
    "The offset of the COG to CT&7"

    duration: int
    "Number of cycles for the DUT code"

    result: int
    "The result written to rt_result, may be useful for additional validation"


def read_results(stdout: BinaryIO) -> list[Result]:
    """Parse the eight phase observations independently of the upload transport."""
    results: list[Result] = list()
    for _ in range(8):
        while True:
            buffer = stdout.read(1)
            if buffer == b"\x03":
                break  # ETX terminates test output
            if len(buffer) == 0:
                raise RuntimeError("Unexpected end of file")
            # print(len(buffer), buffer)

        def read_value() -> int:
            line = stdout.readline()
            try:
                return int(line.removesuffix(b"\n"), 16)
            except:
                sys.stderr.write(f"failed to process {line!r}\r\n")
                raise

        rt_offset = read_value()
        rt_start_ct = read_value()
        rt_stop_ct = read_value()
        rt_result = read_value()

        duration = rt_stop_ct - rt_start_ct

        assert duration >= 2, "GETCT..GETCT must at least take two cycles"
        assert (rt_start_ct + 2) & 0x1FF == rt_offset, (
            "rt_start_ct must be aligned to 0x200!"
        )

        assert rt_offset == (rt_start_ct + 2) & 7, (
            f"{rt_offset=} ~= {(rt_start_ct+2)&7=}"
        )

        results.append(
            Result(
                launch_ct=rt_start_ct + 2,
                offset=rt_offset,
                duration=duration - 2,
                result=rt_result,
            )
        )
    return results


async def run_p2aas(fixture: bytes, endpoint: str) -> list[Result]:
    # The optional transport needs websockets; direct serial use keeps its existing dependencies.
    import websockets

    url = urllib.parse.urlsplit(endpoint)
    query = dict(urllib.parse.parse_qsl(url.query, keep_blank_values=True))
    if "code" in query:
        raise ValueError("P2AAS_ENDPOINT must not select URL-code upload")
    query.setdefault("baudrate", "115200")
    query.setdefault("timeout_ms", "5000")
    address = urllib.parse.urlunsplit(url._replace(query=urllib.parse.urlencode(query)))
    output = bytearray()
    async with websockets.connect(address, max_size=None) as socket:
        await socket.send(struct.pack("<I", len(fixture)) + fixture)
        try:
            while True:
                chunk = await asyncio.wait_for(socket.recv(), 15)
                if not isinstance(chunk, bytes):
                    raise RuntimeError("Expected binary UART output")
                output.extend(chunk)
                if len(output) > 65536:
                    raise RuntimeError("Timing fixture exceeded its output limit")
                # COGSTOP disables TX and can discard the final NUL while it is still shifting.
                # The eighth record's fourth newline is the same completion point used by serial parsing.
                if output.count(b"\x03") == 8 and output.rsplit(b"\x03", 1)[1].count(b"\n") >= 4:
                    break
        except websockets.ConnectionClosed as close:
            raise RuntimeError(f"P2AAS closed before the fixture finished: {close}") from close
    return read_results(io.BytesIO(output))


def run_fixture(fixture: bytes) -> list[Result]:
    endpoint = os.environ.get("P2AAS_ENDPOINT")
    if endpoint:
        return asyncio.run(run_p2aas(fixture, endpoint))

    args = [
        "turboprop", f"--port={PORT}", "--baudrate=115200", "--reset=dtr",
        "--monitor", "--monitor-format=raw", "-",
    ]
    with Popen(args=args, stdin=PIPE, stdout=PIPE) as proc:
        assert proc.stdin is not None and proc.stdout is not None
        proc.stdin.write(fixture)
        proc.stdin.close()
        try:
            return read_results(proc.stdout)
        finally:
            proc.kill()


def execute_fixture(*, measure: str, prepare: str = "", data: str = "") -> list[Result]:

    fixture = compile_fixture(
        prepare_code=prepare,
        dut_code=measure,
        data_segment=data,
    )

    return run_fixture(fixture)


@dataclass(kw_only=True, frozen=True, slots=True)
class TestCase:
    name: str
    measure: str
    expect: int | list[int] | None = field(default=None)
    expect_result: int | list[int] | None = field(default=None)
    prepare: str = field(default="")
    data: str = field(default="")


T = TypeVar("T")


class TerseList(list[T], Generic[T]): ...


class FlowSeqDumper(yaml.SafeDumper): ...


FlowSeqDumper.add_representer(
    TerseList,
    lambda d, x: d.represent_sequence("tag:yaml.org,2002:seq", x, flow_style=True),
)


def compact_list(seq: Iterable[T]) -> TerseList[T] | T:
    args = tuple(seq)
    if len(args) == 0:
        return TerseList()
    if all(arg == args[0] for arg in args):
        return args[0]
    return TerseList(args)


def main():

    testcases: list[TestCase]
    with TESTCASE_FILE.open("rb") as fp:
        yaml_data = safe_load(fp)
        assert isinstance(yaml_data, dict)
        testcases = [TestCase(**item) for item in yaml_data["test-cases"]]
        del yaml_data

    stdout = sys.stdout
    if stdout.isatty():

        @contextmanager
        def wrap_test(name: str):
            stdout.write(f"...  {name}")
            try:
                yield
                stdout.write("\rPASS\n")
            except AssertionError as err:
                stdout.write("\rFAIL\n")
                stdout.write(f"     {type(err).__name__}: {err}\n")
    else:

        @contextmanager
        def wrap_test(name: str):
            stdout.write(f"{name}: ")
            try:
                yield
                stdout.write("PASS\n")
            except AssertionError as err:
                stdout.write(f"FAIL: {err}\n")

    all_ok = True
    test_results: list = list()

    for testcase in testcases:
        test_output = {
            "test": testcase.name,
            "pass": False,
            "results": None,
        }
        test_results.append(test_output)
        with wrap_test(testcase.name):
            results = execute_fixture(
                prepare=testcase.prepare,
                measure=testcase.measure,
                data=testcase.data,
            )

            test_output["results"] = {
                "offsets": compact_list(res.offset for res in results),
                "durations": compact_list(res.duration for res in results),
                "results": compact_list(res.result for res in results),
            }

            if testcase.expect is not None:
                expected_results: list[int]
                if isinstance(testcase.expect, int):
                    expected_results = [testcase.expect] * 8
                else:
                    expected_results = testcase.expect

                for offset, (res, expect) in enumerate(zip(results, expected_results)):
                    assert expect == res.duration, (
                        f"Expected a duration of {expect}, but got {res.duration} for offset {offset}"
                    )

            if testcase.expect_result is not None:
                expected_results: list[int]
                if isinstance(testcase.expect_result, int):
                    expected_results = [testcase.expect_result] * 8
                else:
                    expected_results = testcase.expect_result

                for offset, (res, expect) in enumerate(zip(results, expected_results)):
                    assert expect == res.result, (
                        f"Expected a result of {expect}, but got {res.result} for offset {offset}"
                    )

            test_output["pass"] = True
        if not test_output["pass"]:
            all_ok = False

    with TESTRESULT_FILE.open("w", encoding="utf-8") as fp:
        yaml.dump(
            data={"test-results": test_results},
            stream=fp,
            sort_keys=False,
            Dumper=FlowSeqDumper,
        )

    if not all_ok:
        sys.exit(1)


if __name__ == "__main__":
    main()
