from dataclasses import dataclass, field as ds_field
from pathlib import Path
from enum import Enum
import io
import logging
import json
import sys
from typing import ClassVar, Iterable
import yaml

DATA_ROOT = Path(__file__).parent / ".." / "data" / "encoding"

P2INSTRUCTIONS_JSON = DATA_ROOT / "p2instructions.json"
INSTRUCTIONS_YML = DATA_ROOT / "instructions.yml"


class Flag(Enum):
    NONE = "none"
    ANDC = "ANDC"
    ANDZ = "ANDZ"
    ORC = "ORC"
    ORZ = "ORZ"
    WC = "WC"
    WCZ = "WCZ"
    WZ = "WZ"
    XORC = "XORC"
    XORZ = "XORZ"

    def __lt__(self, other):
        if self == other:
            return False
        if self is Flag.NONE:
            return True
        if other is Flag.NONE:
            return False
        return self.value < other.value


class OpType(Enum):
    ADDRESS = "address"
    AUGMENT = "augment"
    C_REMAP = "c_remap"
    DEST_EITHER = "dest_either"
    DEST_REG = "dest_reg"
    PREG = "preg"
    PTREXPR = "ptrexpr"
    SELECTOR = "selector"
    SRC_EITHER = "src_either"
    SRC_EITHER_PCREL = "src_either_pcrel"
    Z_REMAP = "z_remap"


class BitField(Enum):
    CONDITION = "E"
    POINTER = "W"
    RELATIVE = "R"
    ADDRESS = "A"
    AUGMENT = "n"
    WRITE_C = "C"
    WRITE_Z = "Z"
    IMMEDIATE_S = "I"
    IMMEDIATE_D = "L"
    SOURCE = "S"
    DEST = "D"
    SELECTOR = "N"
    Z_REMAP = "z"
    C_REMAP = "c"

    def __lt__(self, other):
        return self.value < other.value


@dataclass(frozen=True)
class OpMapping:
    field: BitField
    imm: BitField | None = ds_field(kw_only=True, default=None)
    rel: BitField | None = ds_field(kw_only=True, default=None)


FLAG_FIELD: dict[Flag, frozenset[BitField]] = {
    Flag.NONE: set(),
    Flag.ANDC: frozenset((BitField.WRITE_C,)),
    Flag.ANDZ: frozenset((BitField.WRITE_Z,)),
    Flag.ORC: frozenset((BitField.WRITE_C,)),
    Flag.ORZ: frozenset((BitField.WRITE_Z,)),
    Flag.WC: frozenset((BitField.WRITE_C,)),
    Flag.WCZ: frozenset((BitField.WRITE_C, BitField.WRITE_Z)),
    Flag.WZ: frozenset((BitField.WRITE_Z,)),
    Flag.XORC: frozenset((BitField.WRITE_C,)),
    Flag.XORZ: frozenset((BitField.WRITE_Z,)),
}


OP_MAPPING: dict[OpType, OpMapping] = {
    OpType.ADDRESS: OpMapping(BitField.ADDRESS, rel=BitField.RELATIVE),
    OpType.AUGMENT: OpMapping(BitField.AUGMENT),
    OpType.C_REMAP: OpMapping(BitField.C_REMAP),
    OpType.DEST_EITHER: OpMapping(BitField.DEST, imm=BitField.IMMEDIATE_D),
    OpType.DEST_REG: OpMapping(BitField.DEST),
    OpType.PREG: OpMapping(BitField.POINTER),  # can only be first operand
    OpType.PTREXPR: OpMapping(
        BitField.SOURCE, imm=BitField.IMMEDIATE_S
    ),  # can only be second operand
    OpType.SELECTOR: OpMapping(BitField.SELECTOR),
    OpType.SRC_EITHER: OpMapping(BitField.SOURCE, imm=BitField.IMMEDIATE_S),
    OpType.SRC_EITHER_PCREL: OpMapping(BitField.SOURCE, imm=BitField.IMMEDIATE_S),
    OpType.Z_REMAP: OpMapping(BitField.Z_REMAP),
}


OP_DISPLAY_TEXT: dict[OpType, str] = {
    OpType.ADDRESS: "#{\}A",
    OpType.AUGMENT: "#n",
    OpType.C_REMAP: "c",
    OpType.DEST_EITHER: "{#}D",
    OpType.DEST_REG: "D",
    OpType.PREG: "PA/PB/PTRA/PTRB",
    OpType.PTREXPR: "{#}S/P",
    OpType.SELECTOR: "#N",
    OpType.SRC_EITHER: "{#}S",
    OpType.SRC_EITHER_PCREL: "{#}S**",
    OpType.Z_REMAP: "z",
}


@dataclass(kw_only=True, frozen=True)
class Slot:
    offset: int
    length: int

    @property
    def mask(self) -> int:
        return ((1 << self.length) - 1) << self.offset


class Encoding:
    binary: int
    fields: dict[BitField, Slot]

    def __init__(self, mask: str) -> None:
        mask = mask.replace(" ", "")
        if len(mask) != 32:
            raise ValueError("mask must have exactly 32 bits defined")

        bin_mask = mask
        for field in BitField:
            bin_mask = bin_mask.replace(field.value, "0")
        self.binary = int(bin_mask, 2)

        self.fields = dict()

        rev_mask = mask[::-1].replace("1", "0")
        offset = 0
        for group in self.splitgroups(rev_mask):
            if group[0] != "0":
                slot = Slot(offset=offset, length=len(group))
                field = BitField(group[0])
                self.fields[field] = slot
            offset += len(group)

    @property
    def mask(self) -> int:
        m: int = 0
        for slot in self.fields.values():
            m |= slot.mask
        m ^= 0xFFFF_FFFF
        assert (self.binary & m) == self.binary, f"0x{self.binary:08X}, 0x{m:08X}"
        return m

    @property
    def variable_bits(self) -> int:
        return sum(slot.length for slot in self.fields.values())

    def __str__(self) -> str:
        binval = f"{self.binary:0>32b}"

        binval = binval[::-1]  # reverse

        for fld, slot in self.fields.items():
            start = slot.offset
            end = slot.offset + slot.length

            binval = binval[:start] + fld.value * slot.length + binval[end:]
            assert len(binval) == 32, f"{fld} {slot}"

        binval = binval[::-1]  # reverse

        return f"{binval[0:4]} {binval[4:11]} {binval[11:14]} {binval[14:23]} {binval[23:32]}"

    @classmethod
    def splitgroups(cls, mask: str) -> Iterable[str]:
        head = ""
        for c in mask:
            if head[:1] != c:
                if len(head) > 0:
                    yield head
                head = c
            else:
                head += c
        if len(head) > 0:
            yield head


@dataclass(kw_only=True, frozen=True)
class Instruction:
    id: str
    name: str
    flags: set[Flag]
    encoding: Encoding
    operands: list[OpType]
    alias_name: str | None

    @property
    def display_text(self) -> str:
        parts = [self.name]

        if len(self.operands) > 0:
            parts.append(", ".join(OP_DISPLAY_TEXT[op] for op in self.operands))

        if len(self.flags) > 0:
            fstr = "/".join(
                sorted(
                    (flag.name for flag in self.flags if flag is not Flag.NONE),
                    key=lambda f: (len(f), f),
                )
            )

            if len(self.flags) > 1 and Flag.NONE in self.flags:
                fstr = f"{{{fstr}}}"
            parts.append(fstr)

        return " ".join(parts)


def decode_json(path: Path) -> list[Instruction]:
    j_instructions: list[dict]
    with open(path, "rb") as fp:
        j_instructions = json.load(fp)

    instructions: list[Instruction] = list()
    for j_instr in j_instructions:
        instr_id: str = j_instr["id"]
        alias_name: str | None = j_instr["alias"]
        name: str = j_instr["name"]
        flags: set[Flag] = {Flag(v) for v in j_instr["flags"]}
        encoding: Encoding = Encoding(j_instr["enctext"])
        operands: list[OpType] = [OpType(v) for v in j_instr["args"]]

        if name == "<empty>":
            # skip instructions that cannot be properly encoded
            continue

        # print(encoding, name, operands, flags)

        fields_used: set[BitField] = set()
        if BitField.CONDITION in encoding.fields:
            fields_used.add(BitField.CONDITION)

        for flag in flags:
            fields_used.update(FLAG_FIELD[flag])  # flags may overlap!

        def test_field(field: BitField):
            if field not in encoding.fields:
                raise ValueError("Invalid operand")
            if field in fields_used:
                raise ValueError("operand uses field twice!")
            fields_used.add(field)

        for op in operands:
            mapping = OP_MAPPING[op]
            # print("", op, mapping)

            test_field(mapping.field)

            if mapping.rel is not None:
                test_field(mapping.rel)

            if mapping.imm is not None:
                test_field(mapping.imm)

        if fields_used != set(encoding.fields.keys()):
            print("  ", sorted(fields_used))
            print("  ", sorted(encoding.fields.keys()))
            raise ValueError(
                "missing encoding: fields required are not the fields available!"
            )

        instructions.append(
            Instruction(
                id=instr_id,
                name=name,
                flags=flags,
                encoding=encoding,
                operands=operands,
                alias_name=alias_name,
            )
        )

    return instructions


ZIG_KEYWORDS = {
    "and",
    "or",
    "test",
}


def zig_id(v: str) -> str:
    if v in ZIG_KEYWORDS:
        return f'@"{v}"'

    return v


def zig_escape(v: str) -> str:
    v = v.replace("\\", "\\\\")
    v = v.replace('"', '\\"')
    return v
