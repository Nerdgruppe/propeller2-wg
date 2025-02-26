import re

from enum import StrEnum, Enum
from typing import Optional, Any, ClassVar
from pathlib import Path
from dataclasses import dataclass, field


import yaml


struct = dataclass(frozen=True, kw_only=True)


INSTRUCTIONS_YML = Path(__file__).parent / "instructions.yml"


@struct
class Timings:
    pass


@struct
class Encoding:
    bitmask: str

    @property
    def size(self) -> int:
        return len(self.bitmask)


@struct
class Condition:
    name: str
    encoding: Encoding
    description: str


class Effect(StrEnum):
    opt_wc = "{WC}"
    opt_wc_wz_wcz = "{WC/WZ/WCZ}"
    opt_wcz = "{WCZ}"
    opt_wz = "{WZ}"
    andc_andz = "ANDC/ANDZ"
    orc_orz = "ORC/ORZ"
    wc_wz = "WC/WZ"
    wc_wz_wcz = "WC/WZ/WCZ"
    xorc_xorz = "XORC/XORZ"

    @property
    def is_optional(self) -> bool:
        return self in (
            Effect.opt_wc,
            Effect.opt_wc_wz_wcz,
            Effect.opt_wcz,
            Effect.opt_wz,
        )


class Immediate(Enum):
    no = ""
    optional = "{#}"
    yes = "#"


@struct
class Parameter:
    name: str

    immediate: Immediate
    absolute: Immediate


@struct
class Instruction:
    name: str
    mnemonic: str
    encoding: Encoding
    brief: str
    group: str
    tags: frozenset[str]
    is_alias: bool
    description: str
    hubexec_timings: Timings
    cogexec_timings: Timings

    parameters: tuple[Parameter]
    effect: Optional[Effect]

    def __str__(self) -> str:
        return self.mnemonic


@struct
class InstructionSet:
    _instance: ClassVar[Optional["InstructionSet"]] = None

    instructions: list[Instruction] = field(default_factory=list)
    conditions: dict[str, str] = field(default_factory=dict)

    instruction_groups: dict[str, frozenset[Instruction]] = field(default_factory=dict)

    def __post_init__(self):
        for instr in self.instructions:
            group = self.instruction_groups.setdefault(instr.name, set())
            group.add(instr)

        for key, value in list(self.instruction_groups.items()):
            self.instruction_groups[key] = frozenset(value)

    def get_group(self, name: str) -> frozenset[Instruction] | None:
        return self.instruction_groups.get(name)


def _load_encoding(value: str, bits: int) -> Encoding:
    assert isinstance(value, str)

    value = value.replace(" ", "").replace("_", "")
    assert len(value) == bits

    for char in value:
        assert char in ["0", "1", "E", "I", "D", "S", "C", "Z", "N", "L", "c", "z", "R", "A", "W", "n"], repr(char)

    return Encoding(bitmask=value)


def _assert_type(value: Any, T: type):
    assert isinstance(value, T)
    return value


def _load_timing(value: dict[str, Any] | int) -> Timings:
    return Timings()


EFFECTS = {
    "{WC}": Effect.opt_wc,
    "{WC/WZ/WCZ}": Effect.opt_wc_wz_wcz,
    "{WCZ}": Effect.opt_wcz,
    "{WZ}": Effect.opt_wz,
    "ANDC/ANDZ": Effect.andc_andz,
    "ORC/ORZ": Effect.orc_orz,
    "WC/WZ": Effect.wc_wz,
    "WC/WZ/WCZ": Effect.wc_wz_wcz,
    "XORC/XORZ": Effect.xorc_xorz,
}


def _parse_param(spec: str) -> Parameter:
    imm: Immediate
    if spec.startswith("#"):
        spec = spec.removeprefix("#")
        imm = Immediate.yes
    elif spec.startswith("{#}"):
        spec = spec.removeprefix("{#}")
        imm = Immediate.optional
    else:
        imm = Immediate.no

    abs: Immediate
    if spec.startswith("\\"):
        spec = spec.removeprefix("\\")
        abs = Immediate.yes
    elif spec.startswith("{\\}"):
        spec = spec.removeprefix("{\\}")
        abs = Immediate.optional
    else:
        abs = Immediate.no

    assert len(spec) == 1 or spec in ("S/P", "PA/PB/PTRA/PTRB"), repr(spec)

    return Parameter(
        name=spec,
        immediate=imm,
        absolute=abs,
    )


def _classify_mnemonic(mnemonic: str) -> tuple[list[Parameter], Effect | None]:
    components: list[str] = re.split(r",| +", mnemonic)
    assert len(components) >= 1
    assert len(components) <= 5

    name = components[0]
    params: list[str] = list()
    effect: Effect | None = None
    for item in components[1:]:
        if item in EFFECTS:
            assert effect is None, f"Duplicate effect: {item!r}"
            effect = EFFECTS[item]
        else:
            assert effect is None, f"Param {item!r} after effect"
            params.append(
                _parse_param(item),
            )

    assert len(params) <= 3

    return tuple(params), effect


def _load_instruction(value: dict[str, Any]) -> Instruction:
    assert isinstance(value, dict)

    mnemonic: str = _assert_type(value["mnemonic"], str)
    name: str = mnemonic.split(" ")[0]

    params, effect = _classify_mnemonic(mnemonic)

    return Instruction(
        name=name.upper(),
        mnemonic=mnemonic,
        parameters=params,
        effect=effect,
        encoding=_load_encoding(value["encoding"], 32),
        brief=_assert_type(value["brief"], str),
        group=_assert_type(value["group"], str),
        tags=frozenset(_assert_type(tag, str) for tag in _assert_type(value["tags"], list)),
        is_alias=_assert_type(value.get("is_alias", False), bool),
        description=_assert_type(value["brief"], str),
        hubexec_timings=_load_timing(value["cycles"]["cog"]),
        cogexec_timings=_load_timing(value["cycles"]["hub"]),
    )


def load() -> InstructionSet:
    if InstructionSet._instance is not None:
        return InstructionSet._instance

    data_root: dict[str, Any]
    with INSTRUCTIONS_YML.open("r", encoding="utf-8") as fp:
        data_root = yaml.safe_load(fp)

    assert isinstance(data_root, dict)

    conditions_root: dict[str, Any] = data_root["conditions"]
    assert isinstance(conditions_root, dict)

    instructions_root: list[dict[str, Any]] = data_root["instructions"]
    assert isinstance(instructions_root, list)

    isa: InstructionSet = InstructionSet(
        conditions={
            name: Condition(
                name=_assert_type(name, str),
                encoding=_load_encoding(value["encoding"], 4),
                description=_assert_type(value["description"], str),
            )
            for name, value in conditions_root.items()
        },
        instructions={_load_instruction(value) for value in instructions_root},
    )

    InstructionSet._instance = isa

    return isa
