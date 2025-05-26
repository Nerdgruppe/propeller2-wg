from dataclasses import dataclass, field
from pathlib import Path
from enum import Enum
import io
import sys
import logging
import sys
import textwrap
import caseconverter

from typing import Iterable

from common import (
    Instruction,
    P2INSTRUCTIONS_JSON,
    P2INSTRUCTIONS_TSV,
    decode_dataset,
    zig_id,
    zig_escape,
)

DATA_ROOT = Path(__file__).parent / ".." / "data" / "encoding"


@dataclass
class Group:
    encoding: str
    name: str
    fields: list[tuple[str, str]] = field(default_factory=list)


def splitgroups(mask: str) -> Iterable[str]:
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
    groups = process_groups()

    instructions = decode_dataset(P2INSTRUCTIONS_JSON, P2INSTRUCTIONS_TSV)

    instructions = [instr for instr in instructions if not instr.is_alias]

    logging.info("loaded %d instructions!", len(instructions))

    if "encoding" in sys.argv:
        render_encoding(sys.stdout, groups.values())

    if "decoder" in sys.argv:
        render_decoder(sys.stdout, instructions, groups)

    if "executor" in sys.argv:
        render_executor_stub(sys.stdout, instructions, groups)


# Predefined Instruction Groups:
INSTRUCTION_GROUPS = {
    "EEEE _____WW RAA AAAAAAAAA AAAAAAAAA": "LocStyle",
    "EEEE ______N NNI DDDDDDDDD SSSSSSSSS": None,
    "EEEE _______ CLI DDDDDDDDD SSSSSSSSS": None,
    "EEEE _______ CZI DDDDDDDDD SSSSSSSSS": None,
    "EEEE _______ CZL DDDDDDDDD _________": None,
    "EEEE _______ CZ_ DDDDDDDDD _________": None,
    "EEEE _______ CZ_ _________ _________": "OnlyFlags",
    "EEEE _______ CZ_ _cccczzzz _________": "UpdateFlags",
    "EEEE _______ C_I DDDDDDDDD SSSSSSSSS": None,
    "EEEE _______ C_L DDDDDDDDD _________": None,
    "EEEE _______ C__ DDDDDDDDD _________": None,
    "EEEE _______ NNI DDDDDDDDD SSSSSSSSS": None,
    "EEEE _______ RAA AAAAAAAAA AAAAAAAAA": "AbsPointer",
    "EEEE _______ _LI DDDDDDDDD SSSSSSSSS": None,
    "EEEE _______ _L_ DDDDDDDDD _________": None,
    "EEEE _______ _NI DDDDDDDDD SSSSSSSSS": None,
    "EEEE _______ _ZI DDDDDDDDD SSSSSSSSS": None,
    "EEEE _______ __I DDDDDDDDD SSSSSSSSS": None,
    "EEEE _______ __I _________ SSSSSSSSS": None,
    "EEEE _______ __L DDDDDDDDD _________": None,
    "EEEE _______ ___ DDDDDDDDD _________": None,
    "EEEE _______ ___ _________ _________": "NoOperands",
    "EEEE _____nn nnn nnnnnnnnn nnnnnnnnn": "Augment",
    "____ _______ ___ _________ _________": "Nop",
}


GROUP_NAMES: dict[str, tuple[str, str]] = {
    "EEEE": ("cond", "Condition"),
    "WW": ("pointer", "PointerReg"),
    "R": ("relative", "bool"),
    "AAAAAAAAAAAAAAAAAAAA": ("address", "u20"),
    "nnnnnnnnnnnnnnnnnnnnnnn": ("augment", "u23"),
    "C": ("c_mod", "FlagModifier"),
    "Z": ("z_mod", "FlagModifier"),
    "I": ("s_imm", "bool"),
    "L": ("d_imm", "bool"),
    "SSSSSSSSS": ("s", "Register"),
    "DDDDDDDDD": ("d", "Register"),
    "N": ("n", "u1"),
    "NN": ("n", "u2"),
    "NNN": ("n", "u3"),
    "zzzz": ("z_value", "FlagExpression"),
    "cccc": ("c_value", "FlagExpression"),
}


def process_groups() -> dict[str, Group]:
    # INSTRUCTION_GROUPS

    groups: dict[str, Group] = dict()

    for src_enc, name in INSTRUCTION_GROUPS.items():
        enc = src_enc.replace(" ", "")

        if name is None:
            s_type = None
            if "S" in enc and "I" in enc:
                s_type = "Simm"
            elif "S" in enc:
                s_type = "S"

            d_type = None
            if "D" in enc and "L_" in enc:
                d_type = "DimmHigh"
            elif "D" in enc and "L" in enc:
                d_type = "Dimm"
            elif "D" in enc:
                d_type = "D"

            f_type = None
            if "C" in enc and "Z" in enc:
                f_type = "Flags"
            elif "C" in enc:
                f_type = "CFlag"
            elif "Z" in enc:
                f_type = "ZFlag"

            n_type = None
            if "NNN" in enc:
                n_type = "N3"
            elif "NN" in enc:
                n_type = "N2"
            elif "N" in enc:
                n_type = "N1"

            prefix = "Only"
            if s_type is not None and d_type is not None:
                prefix = "Both"

            name = "_".join(
                [v for v in (prefix, d_type, s_type, n_type, f_type) if v is not None]
            )

            assert name is not None, repr(enc)

        grp = Group(encoding=enc, name=name)

        offset = 0
        masks = 0
        for group in splitgroups(enc[::-1]):
            if group[0] != "_":
                # named field
                grp.fields.append(GROUP_NAMES[group])
            else:
                # unnamed mask field
                masks += 1
                grp.fields.append((f"_mask{masks}", f"u{len(group)}"))

            offset += len(group)
        assert offset == 32

        groups[src_enc] = grp

    return groups


def render_encoding(stream: io.TextIOBase, groups: Iterable[Group]) -> None:
    stream.write(
        textwrap.dedent(
            """
            //!
            //! AUTOGENERATED CODE!
            //! This code was autogenerated by utility/gen_windtunnel.py encoding!
            //!
            const std = @import("std");

            const enums = @import("enums.zig");

            pub const Register = enums.Register;
            pub const Condition = enums.Condition;
            pub const FlagModifier = enums.FlagModifier;
            pub const FlagExpression = enums.FlagExpression;
            pub const PointerReg = enums.PointerReg;

            comptime {
                std.debug.assert(@bitSizeOf(Instruction) == 32);
            }

            pub const Instruction = packed union {
                raw: u32,
            """
        ).lstrip()
    )

    for grp in sorted(groups, key=lambda g: g.encoding):
        stream.write(f"    {caseconverter.snakecase(grp.name)}: {grp.name},\n")

    stream.write("};\n\n")

    for grp in sorted(groups, key=lambda g: g.encoding):
        stream.write(f"\n\n/// {grp.encoding!r}\n")
        stream.write(f"pub const {grp.name} = packed struct(u32) {{\n")

        for fname, ftype in grp.fields:
            stream.write(f"    {fname}: {ftype},\n")

        stream.write("};\n")


def render_decoder(
    stream: io.TextIOBase,
    instructions: Iterable[Instruction],
    groups: dict[str, Group],
) -> None:
    stream.write(
        textwrap.dedent(
            """
            //!
            //! AUTOGENERATED CODE!
            //! This code was autogenerated by utility/gen_windtunnel.py decoder
            //!
            const std = @import("std");

            const encoding = @import("encoding.zig");

            ///
            /// Enumeration of all valid opcodes.
            ///
            pub const OpCode = enum {
                invalid,
            """
        )
    )

    for instr in sorted(instructions, key=lambda i: i.id):
        opcode = zig_id(caseconverter.snakecase(instr.id))
        stream.write(f"    {opcode},\n")

    stream.write(
        textwrap.dedent("""
            };
            

            ///
            /// A mapping between opcode and the active field of the Instruction union.
            ///
            pub const instruction_type: std.EnumArray(OpCode, []const u8) = blk: {
                @setEvalBranchQuota(10_000);
                break :blk .init(.{
                    .invalid = "",
            """).lstrip()
    )

    for instr in sorted(instructions, key=lambda i: i.id):
        key = str(instr.encoding).replace("1", "_").replace("0", "_")

        grp = groups[key]

        opcode = zig_id(caseconverter.snakecase(instr.id))
        grpname = zig_id(caseconverter.snakecase(grp.name))

        stream.write(f'        .{opcode} = "{grpname}",\n')

    stream.write(
        textwrap.dedent("""
                });
            };
            

            pub fn decode(raw: u32) OpCode
            {
            """).lstrip()
    )

    for instr in sorted(
        instructions, key=lambda i: i.encoding.variable_bits, reverse=True
    ):
        opcode = zig_id(caseconverter.snakecase(instr.id))
        stream.write(
            f"    if((raw & 0x{instr.encoding.mask:08X}) == 0x{instr.encoding.binary:08X})\n"
        )
        stream.write(f"        return .{opcode};\n")

    stream.write("    return .invalid;\n\n")
    stream.write("}\n\n")


def render_executor_stub(
    stream: io.TextIOBase,
    instructions: Iterable[Instruction],
    groups: dict[str, Group],
) -> None:
    stream.write(
        textwrap.dedent(
            """
            const std = @import("std");

            const encoding = @import("encoding.zig");
            const Cog = @import("Cog.zig");

            """
        )
    )

    last_grp = None

    for instr in sorted(instructions, key=lambda i: (i.group, i.iid, i.id)):
        if instr.group != last_grp:
            stream.write(
                textwrap.dedent(
                    f"""


                    //
                    // GROUP: {instr.group}
                    //


                    """
                )
            )
            last_grp = instr.group

        key = str(instr.encoding).replace("1", "_").replace("0", "_")

        grp = groups[key]

        opcode = caseconverter.snakecase(instr.id)
        grpname = zig_id(grp.name)

        stream.write("\n\n")
        stream.write(f"/// {instr.display_text}\n")
        stream.write(f"/// {instr.encoding}\n")
        stream.write("///\n")
        stream.write(f"/// description: {instr.description}\n")
        stream.write(f"/// cog timing:  {instr.cog_timing}\n")
        stream.write(f"/// hub timing:  {instr.hub_timing}\n")
        stream.write(
            f"/// access:      mem={instr.memory_access}, reg={instr.register_access}, stack={instr.stack_access}\n"
        )
        stream.write(
            f"pub fn {zig_id(opcode)}(cog: *Cog, args: encoding.{grpname}) Cog.ExecResult {{\n"
        )
        stream.write("    _ = cog;\n")
        stream.write("    _ = args;\n")
        stream.write(
            f'    @panic("{zig_escape(instr.display_text)} is not implemented yet!");\n'
        )
        stream.write("    // return .next;\n")
        stream.write("}\n")


if __name__ == "__main__":
    sys.exit(main() or 0)
