import base64
import sys
import logging
import json

from pathlib import Path
from argparse import ArgumentParser
from typing import Optional, Any

from .parser import parse_file
from .render import render
from .sema import CompileUnit, ConstValue, MemoryAddress, SymbolType, analyze, Scope


def main() -> int:
    _patch_print()

    parser = _create_parser()

    args = parser.parse_args()

    input_file: Path = args.input
    validate_output_file: Optional[Path] = args.validate_output

    logging.info("assembling %s...", input_file)

    program = parse_file(input_file)
    if program is None:
        sys.stderr.write("parse failure!\n")
        return 1

    # render(program=program, file=sys.stdout)

    compile_unit = analyze(program=program)
    if not compile_unit:
        sys.stderr.write("analysis failure\n")
        return 1

    def _print_scope(scope: Scope, indent: str = "  ") -> None:
        for name, value in sorted(scope.symbols.items()):
            print(f"{indent}- {value}")
            _print_scope(value.locals, indent + "  ")

    print("symbol table:")
    _print_scope(compile_unit.globals)

    print("hex dump:")

    img = compile_unit.full_image

    for i in range(0, len(img), 16):
        chunk = img[i : i + 16]
        padding = 16 - len(chunk)

        hex_str = chunk.hex(sep=" ", bytes_per_sep=1)
        hex_str += "   " * padding

        ascii_str: str = ""
        for value in chunk:
            if value < 0x20 or value >= 0x7F:
                value = ord(".")
            ascii_str += chr(value)
        ascii_str += " " * padding

        sys.stdout.write(f"0x{i:06X}: {hex_str} |{ascii_str}|\n")

    if validate_output_file is not None:
        validate_compile_unit(compile_unit, validate_output_file)

    return 0

def validate_compile_unit(compile_unit: CompileUnit, validation_file: Path) :
    ref: dict[str, Any]
    with validation_file.open("r", encoding="utf-8") as fp:
        ref = json.load(fp)

    if "symbols" in ref:
        for name, value in ref["symbols"].items():
            symbol = compile_unit.globals.get(name)
            assert symbol is not None, f"Symbol {name!r} does not exist"

            parts = value.split(":")
            assert len(parts) >= 2

            assert symbol.type.name == parts[0], (
                f"Expected {name!r} to be {parts[0]!r}, but found {symbol.type.name!r}"
            )

            if symbol.type in [SymbolType.code, SymbolType.data]:
                assert isinstance(symbol.value, MemoryAddress)

                hub_address = int(parts[1])
                local_address = int(parts[2])

                assert hub_address == symbol.value.hub_address, (
                    f"Expected {hub_address!r}, but found {symbol.value.hub_address!r}"
                )
                assert local_address == symbol.value.local_address, (
                    f"Expected {local_address!r}, but found {symbol.value.local_address!r}"
                )

            else:
                assert symbol.type == SymbolType.const
                assert isinstance(symbol.value, ConstValue)

                const_value = int(parts[1])
                assert const_value == symbol.value.value, (
                    f"Expected {const_value!r}, but found {symbol.value.value!r}!"
                )

    if "memory" in ref:
        for memspec in ref["memory"]:
            offset = memspec["offset"]
            data_fmt = memspec["format"]
            data_raw = memspec["data"]

            expected_data = _convert_bytes_from(data_raw, data_fmt)

            assert isinstance(offset, int)
            assert isinstance(expected_data, bytes)

            end_pos = offset + len(expected_data)

            assert offset <= len(compile_unit.full_image)
            assert end_pos <= len(compile_unit.full_image)

            actual_data = compile_unit.full_image[offset:end_pos]

            if expected_data != actual_data:
                assert len(actual_data) == len(expected_data)

                for i in range(len(actual_data)):
                    e_chunk = expected_data[i]
                    a_chunk = actual_data[i]

                    if e_chunk != a_chunk:
                        print(
                            f"[0x{offset+i:06X}] Expected {e_chunk:02X}, found {a_chunk:02X}"
                        )

                assert False

def _convert_bytes_from(byte_def: str | list[str], format: str) -> bytes:
    assert isinstance(byte_def, (str, list))

    if isinstance(byte_def, list):
        byte_def = " ".join(b for b in byte_def)

    if format == "hex":
        return bytes.fromhex(byte_def)
    if format == "base64":
        return base64.decodebytes(byte_def)

    assert False


def _create_parser() -> ArgumentParser:
    parser = ArgumentParser()

    parser.add_argument("--validate-output", type=Path, required=False)

    parser.add_argument("input", type=Path)

    return parser


def _patch_print():
    _original_print = __builtins__["print"]

    def _stderr_print(*args, **kwargs):
        kwargs.setdefault("file", sys.stderr)
        _original_print(*args, **kwargs)

    __builtins__["print"] = _stderr_print
