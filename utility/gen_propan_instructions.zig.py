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
    OpType.PTREXPR: OpMapping(BitField.SOURCE, imm=BitField.IMMEDIATE_S),  # can only be second operand
    OpType.SELECTOR: OpMapping(BitField.SELECTOR),
    OpType.SRC_EITHER: OpMapping(BitField.SOURCE, imm=BitField.IMMEDIATE_S),
    OpType.Z_REMAP: OpMapping(BitField.Z_REMAP),
}

OP_ZIG_TYPE: dict[OpType, str] = {
    OpType.ADDRESS: ".{{ .address = {rel} }}",
    OpType.AUGMENT: ".immediate",
    OpType.C_REMAP: ".{{ .enumeration = modcz_items }}",
    OpType.DEST_EITHER: ".{{ .reg_or_imm = {imm} }}",
    OpType.DEST_REG: ".register",
    OpType.PREG: ".pointer_reg",
    OpType.PTREXPR: ".pointer_expr",
    OpType.SELECTOR: ".immediate",
    OpType.SRC_EITHER: ".{{ .reg_or_imm = {imm} }}",
    OpType.Z_REMAP: ".{{ .enumeration = modcz_items }}",
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
    OpType.Z_REMAP: "z",
}

FLAG_ZIG_FIELD: dict[Flag, str] = {
    Flag.NONE: "none",
    Flag.WC: "wc",
    Flag.WZ: "wz",
    Flag.WCZ: "wcz",
    Flag.ANDC: "and_c",
    Flag.ANDZ: "and_z",
    Flag.ORC: "or_c",
    Flag.ORZ: "or_z",
    Flag.XORC: "xor_c",
    Flag.XORZ: "xor_z",
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
    name: str
    flags: set[Flag]
    encoding: Encoding
    operands: list[OpType]

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


def newslot(slot):
    return f".init({slot.offset}, {slot.length})"


def render_zig(stream: io.IOBase, instructions: list[Instruction]) -> None:
    stream.write("//!\n")
    stream.write("//! This file was auto-generated by <repo>/utility/gen_propan_instructions.zig.py\n")
    stream.write("//!\n")

    stream.write("\n")
    stream.write('const sema = @import("../../sema.zig");\n')
    stream.write("\n")
    stream.write('const modcz_items = @import("constants.zig").modcz_items;\n')
    stream.write("\n")

    stream.write("pub const p2_instructions: []const sema.EncodedInstruction = &.{\n")

    for instr in instructions:
        stream.write(f"    // {instr.display_text}\n")
        stream.write(f"    // {instr.encoding}\n")
        stream.write("    .{\n")

        stream.write(f'        .mnemonic = "{instr.name}",\n')
        stream.write(f"        .binary = 0x{instr.encoding.binary:0>8x},\n")
        stream.write("        .effects = .from_list(&.{ ")

        for i, flag in enumerate(sorted(instr.flags)):
            if i > 0:
                stream.write(", ")
            stream.write(f".{FLAG_ZIG_FIELD[flag]}")

        stream.write(" }),\n")
        stream.write("        .operands = &.{\n")
        for op in instr.operands:
            mapping = OP_MAPPING[op]

            zig_op_type = OP_ZIG_TYPE[op]

            op_slot = instr.encoding.fields[mapping.field]

            type_fmt_slots: dict[str, str] = {}
            if mapping.imm is not None:
                type_fmt_slots["imm"] = newslot(instr.encoding.fields[mapping.imm])
            if mapping.rel is not None:
                type_fmt_slots["rel"] = newslot(instr.encoding.fields[mapping.rel])

            zig_op_type = zig_op_type.format(**type_fmt_slots)

            stream.write(f"            .{{ // {OP_DISPLAY_TEXT[op]}\n")
            stream.write(f"                .slot = {newslot(op_slot)},\n")
            stream.write(f"                .type = {zig_op_type},\n")
            stream.write("            },\n")
        stream.write("        },\n")

        c_effect_slot = instr.encoding.fields.get(BitField.WRITE_C)
        if c_effect_slot is not None:
            stream.write(f"        .c_effect_slot = {newslot(c_effect_slot)},\n")

        z_effect_slot = instr.encoding.fields.get(BitField.WRITE_Z)
        if z_effect_slot is not None:
            stream.write(f"        .z_effect_slot = {newslot(z_effect_slot)},\n")

        stream.write("    },\n")

    stream.write("};\n")


def decode_json(path: Path) -> list[Instruction]:
    j_instructions: list[dict]
    with open(path, "rb") as fp:
        j_instructions = json.load(fp)

    instructions: list[Instruction] = list()
    for j_instr in j_instructions:
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
            raise ValueError("missing encoding: fields required are not the fields available!")

        instructions.append(
            Instruction(
                name=name,
                flags=flags,
                encoding=encoding,
                operands=operands,
            )
        )

    return instructions


def main() -> int | None:
    instructions = decode_json(P2INSTRUCTIONS_JSON)

    logging.info("loaded %d instructions!", len(instructions))

    render_zig(sys.stdout, instructions)


if __name__ == "__main__":
    sys.exit(main() or 0)


# Predefined Instruction Groups:
#   "EEEE _____WW RAA AAAAAAAAA AAAAAAAAA"
#   "EEEE ______N NNI DDDDDDDDD SSSSSSSSS"
#   "EEEE _______ CLI DDDDDDDDD SSSSSSSSS"
#   "EEEE _______ CZI DDDDDDDDD SSSSSSSSS"
#   "EEEE _______ CZL DDDDDDDDD _________"
#   "EEEE _______ CZ_ DDDDDDDDD _________"
#   "EEEE _______ CZ_ _________ _________"
#   "EEEE _______ CZ_ _cccczzzz _________"
#   "EEEE _______ C_I DDDDDDDDD SSSSSSSSS"
#   "EEEE _______ C_L DDDDDDDDD _________"
#   "EEEE _______ C__ DDDDDDDDD _________"
#   "EEEE _______ C__ _cccc____ _________"
#   "EEEE _______ NNI DDDDDDDDD SSSSSSSSS"
#   "EEEE _______ RAA AAAAAAAAA AAAAAAAAA"
#   "EEEE _______ _LI DDDDDDDDD SSSSSSSSS"
#   "EEEE _______ _L_ DDDDDDDDD _________"
#   "EEEE _______ _NI DDDDDDDDD SSSSSSSSS"
#   "EEEE _______ _ZI DDDDDDDDD SSSSSSSSS"
#   "EEEE _______ _Z_ _____zzzz _________"
#   "EEEE _______ __I DDDDDDDDD SSSSSSSSS"
#   "EEEE _______ __I _________ SSSSSSSSS"
#   "EEEE _______ __L DDDDDDDDD _________"
#   "EEEE _______ ___ DDDDDDDDD _________"
#   "EEEE _______ ___ _________ _________"
#   "EEEE _____nn nnn nnnnnnnnn nnnnnnnnn"
#   "____ _______ ___ _________ _________"
