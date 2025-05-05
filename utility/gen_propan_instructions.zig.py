from dataclasses import dataclass
from pathlib import Path
from enum import Enum

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


class OpType(Enum):
    NONE = "none"
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
    COUNT = "N"
    Z_REMAP = "z"
    C_REMAP = "c"


@dataclass(kw_only=True, frozen=True)
class Slot:
    offset: int
    length: int


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

        binval = binval[::-1] # reverse 

        for fld, slot in self.fields.items():
            start = slot.offset
            end = slot.offset + slot.length

            binval = binval[:start] + fld.value * slot.length + binval[end:]
            assert len(binval) == 32 , f"{fld} {slot}"

        binval = binval[::-1] # reverse 

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


        
        


            

def main() -> int | None:
    instructions: list[dict]
    with open(P2INSTRUCTIONS_JSON, "rb") as fp:
        instructions = json.load(fp)

    for j_instr in instructions:
        name: str = j_instr["name"]
        flags: set[Flag] = {Flag(v) for v in j_instr["flags"]}
        encoding: Encoding = Encoding(j_instr["enctext"])
        operands: list[OpType] = [OpType(v) for v in j_instr["args"]]

        print(encoding, name, operands, flags)


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
