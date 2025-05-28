from dataclasses import dataclass, field
from pathlib import Path
from enum import Enum
import re
import io
import sys
import logging
import sys
import textwrap
import caseconverter

from typing import Iterable, Callable

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
    logging.basicConfig()
    groups = process_groups()

    instructions = decode_dataset(P2INSTRUCTIONS_JSON, P2INSTRUCTIONS_TSV)

    instructions = [instr for instr in instructions if not instr.is_alias]

    logging.info("loaded %d instructions!", len(instructions))

    if "encoding" in sys.argv:
        render_encoding(sys.stdout, groups.values())

    if "decoder" in sys.argv:
        render_decoder(sys.stdout, instructions, groups)

    if "executor" in sys.argv:
        _render_incremental(Path(sys.argv[2]), lambda s: render_executor_stub(s, instructions, groups))


def _render_incremental(path: Path, render: Callable[[io.TextIOBase], None]) -> None:
    original_text: str
    try:
        original_text = path.read_text(encoding="utf-8")
    except FileNotFoundError:
        original_text = ""

    _, previous_sections = _exchange_patches(original_text, None)

    new_text: str
    with io.StringIO() as fp:
        render(fp)
        new_text = fp.getvalue()

    output, _ = _exchange_patches(new_text, previous_sections)

    path.write_text(output, encoding="utf-8")


_PATCH_PATTERN = re.compile(r"^\s*//\s*codegen:\s*(?P<cmd>\w+)(?::(?P<args>\S+))\s*$")


def _exchange_patches(source: str, lut_in: dict[str, list[str]] | None) -> tuple[str, dict[str, list[str]]]:
    lut_out: dict[str, list[str]] = dict()

    current_tag: str | None = None
    current_stash: list[str] | None = None

    output_lines: list[str] = list()
    for line in source.splitlines():
        try:
            match = _PATCH_PATTERN.fullmatch(line)
            if match is not None:
                cmd = match.group("cmd").strip()
                arg = match.group("args").strip()

                output_lines.append(line)
                match cmd:
                    case "begin":
                        if current_tag is not None:
                            raise ValueError(f"missing end cmd for codegen section {current_tag!r}")
                        assert current_stash is None

                        if arg in lut_out:
                            raise ValueError(f"duplicate stashing for codegen section {arg!r}")

                        current_tag = arg
                        current_stash = list()

                        lut_out[arg] = current_stash

                        if lut_in is not None:
                            previous = lut_in.get(arg, None)
                            if previous is not None:
                                output_lines.extend(previous)
                            else:
                                logging.warning("patch section %r has currently no replacement", arg)
                                # this will render the new lines into the output:
                                current_stash = output_lines

                    case "end":
                        if current_tag is None:
                            raise ValueError(f"missing start cmd for codegen section {arg!r}")
                        if current_tag != arg:
                            raise ValueError(f"mismatching start cmd for codegen section {current_tag!r} => {arg!r}")
                        assert current_stash is not None
                        assert current_tag in lut_out
                        current_tag = None
                        current_stash = None

                    case _:
                        raise ValueError(f"unknown codegen cmd: {cmd!r}")

            elif current_stash is not None:
                assert current_tag is not None
                current_stash.append(line)
            else:
                output_lines.append(line)
        except ValueError:
            logging.error("error in line %r", line)
            raise

    if current_tag is not None:
        assert current_stash is not None
        raise ValueError(f"unterminated codegen section: {current_tag!r}")
    else:
        assert current_stash is None

    return "\n".join(output_lines), lut_out


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

            name = "_".join([v for v in (prefix, d_type, s_type, n_type, f_type) if v is not None])

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

        stream.write("\n")
        stream.write(
            f"    pub fn format(grp: {grp.name}, fmt: []const u8, opt: std.fmt.FormatOptions, writer: anytype) !void {{\n"
        )
        stream.write("    _ = fmt;\n")
        stream.write("    _ = opt;\n")
        stream.write(f'    try writer.print("{grp.name}(')

        first = True
        for fname, ftype in reversed(grp.fields):
            if fname.startswith("_mask"):
                continue

            if not first:
                stream.write(", ")
            stream.write(f"{fname}={{}}")
            first = False
        stream.write(')", .{')

        for fname, ftype in reversed(grp.fields):
            if fname.startswith("_mask"):
                continue
            stream.write(f"grp.{fname},\n")

        stream.write("});\n")

        if first:
            stream.write("    _ = grp;\n")

        stream.write("}\n")

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
                if(raw == 0x00000000)
                    return .nop;
            """).lstrip()
    )

    for instr in sorted(instructions, key=lambda i: i.encoding.variable_bits, reverse=True):
        if instr.encoding.binary == 0:
            # skip special-cased NOP
            continue
        opcode = zig_id(caseconverter.snakecase(instr.id))
        stream.write(f"    if((raw & 0x{instr.encoding.mask:08X}) == 0x{instr.encoding.binary:08X})\n")
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
            const logger = std.log.scoped(.execute);

            const decode = @import("decode.zig");
            const encoding = @import("encoding.zig");
            const Cog = @import("Cog.zig");

            pub fn execute_instruction(cog: *Cog, state: Cog.PipelineState) Cog.ExecResult {
                const opcode = decode.decode(state.instr);

                const enc: encoding.Instruction = .{ .raw = state.instr };

                switch (opcode) {
                    .invalid => return .trap,
            """
        )
    )

    for instr in instructions:
        if "simple_exec" not in instr.tags:
            continue

        opcode = caseconverter.snakecase(instr.id)

        grp_key = str(instr.encoding).replace("1", "_").replace("0", "_")
        grp = groups[grp_key]

        grpname = zig_id(grp.name)

        stream.write(
            f"inline .{zig_id(opcode)} => |opc| return execute_simple(cog, state, encoding.{grpname}, enc.{caseconverter.snakecase(grpname)}, @tagName(opc)),\n"
        )

    stream.write(
        textwrap.dedent(
            """
                    inline else => |opc| {
                        @setEvalBranchQuota(10_000);
                        const field = comptime decode.instruction_type.get(opc);
                        const params = @field(enc, field);

                        logger.info("0x{X:0>5}: 0x{X:0>8} {s}: {}", .{ state.pc, state.instr, @tagName(opc), params });

                        return @field(@This(), @tagName(opc))(cog, params);
                    },
                }
            }

            const SimpleResult = struct {
                result: u32,
                c: bool,
                z: bool,

                pub fn simple(result: u32, c: bool, z: bool) SimpleResult {
                    return .{ .result = result, .c = c, .z = z };
                }

                pub fn autoz(result: u32, c: bool) SimpleResult {
                    return .{ .result = result, .c = c, .z = (result == 0) };
                }

                pub fn autoc(result: u32, z: bool) SimpleResult {
                    return .{ .result = result, .c = (result & 0x8000_0000) != 0, .z = z };
                }
                
                pub fn autocz(result: u32) SimpleResult {
                    return .{ .result = result, .c = (result & 0x8000_0000) != 0, .z = (result == 0) };
                }
            };

            // codegen: begin:globalcode
            // TODO: Implement global stuff here
            // codegen: end:globalcode
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

        if "simple_exec" in instr.tags:
            grp_key = str(instr.encoding).replace("1", "_").replace("0", "_")
            grp = groups[grp_key]

            field_keys = [k for k, _ in grp.fields]
            assert "cond" in field_keys

            has_d = "d" in field_keys
            if has_d:
                pass
            else:
                assert "d_imm" not in field_keys

            has_s = "s" in field_keys
            if has_s:
                pass
            else:
                assert "s_imm" not in field_keys

            mods_c = "c_mod" in field_keys
            mods_z = "z_mod" in field_keys

            stream.write(f"pub fn {zig_id(opcode)}(cog: *Cog")
            if has_d:
                stream.write(", d: u32")
            if has_s:
                stream.write(", s: u32")

            stream.write(") SimpleResult {\n")
            stream.write(f"    // codegen: begin:{zig_id(opcode)}\n")
            stream.write("    _ = cog;\n")
            if has_d:
                stream.write("    _ = d;\n")
            if has_s:
                stream.write("    _ = s;\n")

            stream.write(f'    @panic("{zig_escape(instr.display_text)} is not implemented yet!");\n')

            if "z_is_reszero" in instr.tags and "c_is_resmsb" in instr.tags:
                stream.write("    // return .autocz(result);\n")
            elif "z_is_reszero" in instr.tags:
                stream.write("    // return .autoz(result, c);\n")
            elif "c_is_resmsb" in instr.tags:
                stream.write("    // return .autoc(result, z);\n")
            else:
                stream.write("    // return .simple(result, c, z);\n")

            stream.write(f"    // codegen: end:{zig_id(opcode)}\n")
            stream.write("}\n")

        else:
            stream.write(f"pub fn {zig_id(opcode)}(cog: *Cog, args: encoding.{grpname}) Cog.ExecResult {{\n")
            stream.write(f"    // codegen: begin:{zig_id(opcode)}\n")
            stream.write("    _ = cog;\n")
            stream.write("    _ = args;\n")
            stream.write(f'    @panic("{zig_escape(instr.display_text)} is not implemented yet!");\n')
            stream.write("    // return .next;\n")
            stream.write(f"    // codegen: end:{zig_id(opcode)}\n")
            stream.write("}\n")


if __name__ == "__main__":
    sys.exit(main() or 0)
