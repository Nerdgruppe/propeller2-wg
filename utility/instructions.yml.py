
import re
import sys

from enum import StrEnum
from abc import ABC
from pathlib import Path
from dataclasses import dataclass, field
from typing import Optional

SELF_PATH = Path(__file__).parent

class HubAccess(StrEnum):
    read = "read"
    write = "write"
    
class StackAccess(StrEnum):
    push = "push"
    pop = "pop"

class RegisterAccess(StrEnum):
    D = 'D'
    PA = 'PA'
    PB = 'PB'
    D_if_WC =  "D if reg and WC"
    D_if_nWC =  "D if reg and !WC"
    DIR = "DIRx"
    OUT = "OUTx"
    DIR_OUT = "DIRx* + OUTx"
    
    # register access is one of PA|PB|PTRA|PTRB
    selectable = "Per W"

class InterruptShield(StrEnum):
    next = '✔'
    loop_body = '✔+code' 
    selectable = '✔ if WC'

regw_lut = {
    '': None,
    'D': RegisterAccess.D,
    'PA': RegisterAccess.PA,
    'PB': RegisterAccess.PB,
    "D if reg and WC": RegisterAccess.D_if_WC,
    "D if reg and !WC": RegisterAccess.D_if_nWC,
    "DIRx": RegisterAccess.DIR,
    "OUTx": RegisterAccess.OUT,
    "DIRx* + OUTx": RegisterAccess.DIR_OUT,
    "Per W": RegisterAccess.selectable,
}


hubrw_lut = {
    "": None,
    "Write": HubAccess.write,
    "Read": HubAccess.read,
}

stackrw_lut = {
    "": None,
    "Push": StackAccess.push,
    "Pop": StackAccess.pop,
}

shield_lut = {
    "": None,
    '✔': InterruptShield.next,
    '✔+code': InterruptShield.loop_body,
    '✔ if WC': InterruptShield.selectable,
}

alias_lut = {
    ".": False,
    "alias": True,
}

group_to_tags_lut = {
    "Branch A - Call": {"control", "branch"},
    "Branch A - Jump": {"control", "branch"},
    "Branch D - Call": {"control", "branch"},
    "Branch D - Call+Skip": {"control", "branch"},
    "Branch D - Jump": {"control", "branch"},
    "Branch D - Jump+Skip": {"control", "branch"},
    "Branch D - Skip": {"control", "branch"},
    "Branch Repeat": {"control", "branch"},
    "Branch Return": {"control", "branch"},
    "Branch S - Call": {"control", "branch"},
    "Branch S - Mod & Test": {"control", "branch"},
    "Branch S - Resume": {"control", "branch"},
    "Branch S - Return": {"control", "branch"},
    "Branch S - Test": {"control", "branch"},
    "Color Space Converter": {"colors"},
    "CORDIC Solver": {"math", "cordic"},
    "Events - Attention": {"events"},
    "Events - Branch": {"events"},
    "Events - Configuration": {"events"},
    "Events - Poll": {"events"},
    "Events - Wait": {"events"},
    "Hub Control - Cogs": {"hub"},
    "Hub Control - Locks": {"hub"},
    "Hub Control - Multi": {"hub"},
    "Hub FIFO": {"fifo","hubram"},
    "Hub FIFO - New Block": {"fifo","hubram"},
    "Hub FIFO - New Read": {"fifo","hubram"},
    "Hub FIFO - New Write": {"fifo","hubram"},
    "Hub FIFO - Read": {"fifo","hubram"},
    "Hub FIFO - Write": {"fifo","hubram"},
    "Hub RAM - Read": {"hubram"},
    "Hub RAM - Write": {"hubram"},
    "Interrupts": {"interrupts"},
    "Lookup Table": {"lut"},
    "Math and Logic": {"math"},
    "Miscellaneous": {"misc"},
    "Pins": {"pins"},
    "Pixel Mixer": {"colors"},
    "Register Indirection": {"control"},
    "Smart Pins": {"pins","smartpins"},
    "Streamer": {"fifo", "streamer"},
}

TIMESPEC_PAT = re.compile(
    r"^(?P<min>\d+)(?:\.\.\.(?P<max>\d+)\s*(?P<longcross>\*)?)?$"
)

ENCODING_PAT = re.compile(
    r"^[ 01ECZIDSLczARWnN]+$"
)

class ExecutionTime(ABC):
    pass 

@dataclass(frozen=True, kw_only=True)
class BoundedExecutionTime(ExecutionTime):
    min: Optional[int]
    max: Optional[int]
    unaligned_access_delay: bool

class WellDefinedExecutionTime(ExecutionTime):
    """
    Can only have predefined constants!
    """

@dataclass(frozen=True, kw_only=True)
class VariableExecutionTime(ExecutionTime):
    options: set[ExecutionTime]
    
@dataclass(frozen=True, kw_only=True)
class SummedExecutionTime(ExecutionTime):
    parts: tuple[ExecutionTime]

SAME_EXECUTION_TIME = WellDefinedExecutionTime()
NO_APPLICABLE_TIME = WellDefinedExecutionTime()
ILLEGAL_TIME = WellDefinedExecutionTime()
EXECUTION_TIME_FROM_D = WellDefinedExecutionTime()
WRFAST_FINISH_TIME = WellDefinedExecutionTime()

PREDEFINED_TIMINGS = {
    "same": SAME_EXECUTION_TIME,
    "FIFO IN USE": NO_APPLICABLE_TIME,
    "ILLEGAL": ILLEGAL_TIME,
    "2+": BoundedExecutionTime(min=2,max=None,unaligned_access_delay=False),
    "2 or 4": VariableExecutionTime(
        options = {
            BoundedExecutionTime(min=2,max=2,unaligned_access_delay=False),
            BoundedExecutionTime(min=4,max=4,unaligned_access_delay=False),
        }
    ),
    "2 or 13...20": VariableExecutionTime(
        options = {
            BoundedExecutionTime(min=2,max=2,unaligned_access_delay=False),
            BoundedExecutionTime(min=13,max=20,unaligned_access_delay=False),
        }
    ),
    "2 + D": SummedExecutionTime(
        parts = [
            BoundedExecutionTime(min=2,max=2,unaligned_access_delay=False),
            EXECUTION_TIME_FROM_D,
        ]
    ),
    "2 or WRFAST finish + 10...17": VariableExecutionTime(
        options = {
            BoundedExecutionTime(min=2,max=2,unaligned_access_delay=False),
            SummedExecutionTime(parts = (
                WRFAST_FINISH_TIME,
                BoundedExecutionTime(min=10,max=17,unaligned_access_delay=False),
            )),
        },
    ),
    "2 or WRFAST finish + 3": VariableExecutionTime(
        options = {
            BoundedExecutionTime(min=2,max=2,unaligned_access_delay=False),
            SummedExecutionTime(parts = (
                WRFAST_FINISH_TIME,
                BoundedExecutionTime(min=3,max=3,unaligned_access_delay=False),
            )),
        },
    ),
    '2...9, +2 if result': VariableExecutionTime(
        options = {
            BoundedExecutionTime(min=2,max=9,unaligned_access_delay=False),
            BoundedExecutionTime(min=4,max=11,unaligned_access_delay=False),
        },
    ),
}

def parse_exec_time(spec: str) -> ExecutionTime:
    timing = PREDEFINED_TIMINGS.get(spec)
    if timing is not None:
        return timing
    

    match = TIMESPEC_PAT.match(spec)
    if match is not None:
        min = match.group("min")
        max = match.group("max") or min
        longcross = match.group("longcross") is not None 
        return BoundedExecutionTime(
            min = int(min),
            max = int(max),
            unaligned_access_delay = longcross,
        )
        
    assert False, repr(spec)

@dataclass(kw_only=True)
class Instruction:
    mnemonic: str
    group: str
    tags: set[str]
    encoding: str
    alias: bool
    description: str
    intr_shield: Optional[InterruptShield]
    cogexec: ExecutionTime
    hubexec: ExecutionTime
    reg_access: Optional[RegisterAccess]
    hub_access: Optional[HubAccess]
    stack_access: Optional[StackAccess]

instructions: list[Instruction] = list()

with (SELF_PATH / "instructions.tsv").open() as fp:
    for line in fp:
        try:
            cols = tuple(line.removesuffix("\n").split('\t'))
            assert len(cols) == 14
            
            _, syntax, group, encoding, alias, description, shield, cogexec, hubexec, _, _, regw, hubrw, stackrw = cols

            assert ENCODING_PAT.match(encoding), repr(encoding)

            alias = alias_lut[alias]

            shield = shield_lut[shield]

            cogexec = parse_exec_time(cogexec)
            hubexec = parse_exec_time(hubexec)

            regw = regw_lut[regw]
            hubrw = hubrw_lut[hubrw]
            stackrw = stackrw_lut[stackrw]

            tags = group_to_tags_lut[group]

            # print(syntax, group, encoding, alias, description, shield, cogexec, hubexec, regw, hubrw, stackrw)

            instructions.append(Instruction(
                mnemonic=syntax,
                group=group,
                tags=tags,
                encoding=encoding,
                alias=alias,
                description=description,
                intr_shield=shield,
                cogexec=cogexec,
                hubexec=hubexec,
                reg_access=regw,
                hub_access=hubrw,
                stack_access=stackrw,
            ))
        except:
            print("failed to process", repr(line), file=sys.stderr)
            raise

output = sys.stdout

def write(*args):
    output.write("".join(args))

def writeln(*args):
    write(*args, "\n")


writeln("---")
writeln()
writeln("instructions:")

for instr in instructions:
    writeln("  - mnemonic: '", instr.mnemonic, "'")
    writeln("    encoding: ", instr.encoding)
    writeln("    brief: |")
    writeln("      ", instr.description.strip())

    writeln("    group: ", instr.group)
    if len(instr.tags) > 0:
        writeln("    tags:")
        for tag in sorted(instr.tags):
            writeln("      - ", tag)

    if instr.reg_access is not None:
        writeln("    reg_access: ", instr.reg_access.name)

    if instr.hub_access is not None:
        writeln("    hub_access: ", instr.hub_access.name)

    if instr.stack_access is not None:
        writeln("    stack_access: ", instr.stack_access.name)

    writeln("    is_alias: ", "true" if instr.alias else "false")
    if instr.intr_shield is not None:
        writeln("    intr_shield: ", instr.intr_shield.name)

    def write_timing(timing: ExecutionTime, I:str = "        "):
        assert timing is not  SAME_EXECUTION_TIME
            
        if timing is NO_APPLICABLE_TIME:
            return write("fifo active")
        if timing is ILLEGAL_TIME:
            return write("illegal")
        if timing is EXECUTION_TIME_FROM_D:
            return write("D")
        if timing is WRFAST_FINISH_TIME:
            return write("WRFAST finished")
        
        if isinstance(timing, BoundedExecutionTime):
            if timing.min == timing.max and not timing.unaligned_access_delay and timing.min is not None:
                return write(str(timing.min))
            
            writeln()
            writeln(I, "min: ", str(timing.min))
            writeln(I, "max: ", str(timing.max))
            if timing.unaligned_access_delay:
                writeln(I, "unaligned_delay: true")

        elif isinstance(timing, VariableExecutionTime):
            writeln()
            writeln(I, "type: one-of")
            writeln(I, "options:")
            for opt in timing.options:
                write(I + "  - ")
                write_timing(opt, I + "    ")
                writeln()
            
        elif isinstance(timing, SummedExecutionTime):
            writeln()
            writeln(I, "type: sum-of")
            writeln(I, "summands:")
            for opt in timing.parts:
                write(I + "  - ")
                write_timing(opt, I + "    ")
                writeln()
          
        else:
            assert False, repr(type(timing))




    
    writeln("    cycles:")
    write("      cog: ")
    write_timing(instr.cogexec)
    writeln()
    write("      hub: ")
    write_timing(instr.hubexec if instr.hubexec is not SAME_EXECUTION_TIME else instr.cogexec)
    writeln()
    
    writeln()