import re

from dataclasses import dataclass as dataclass_raw, field as field_raw
from enum import StrEnum
from dataclasses_json import dataclass_json, config as json_config


def _field(*args, encoder=None, decoder=None, **kwargs):
    return field_raw(
        *args,
        **kwargs,
        metadata=json_config(
            decoder=decoder,
            encoder=encoder,
        ),
    )


def _dataclass(cls):
    return dataclass_json(
        undefined="RAISE",
    )(
        dataclass_raw(
            kw_only=True,
        )(cls),
    )


class BitMask:
    PATTERN = re.compile(r"^[01x_]+$")

    def __init__(self, mask: str):
        assert self.PATTERN.match(mask) is not None, repr(mask)
        self._mask = mask

    @classmethod
    def from_json(cls, data: str) -> "BitMask":
        if isinstance(data, str):
            return cls(data)
        if isinstance(data, list):
            return [cls.from_json(item) for item in data]
        assert False, repr(data)

    def __str__(self):
        return repr(self._mask)

    def __repr__(self):
        return f"BitMask({self._mask!r})"


class Unused(StrEnum):
    unused = "unused"


@_dataclass
class BitField:
    bitsize: int
    name: str
    description: str
    enum: dict[int, str]
    unused: bool
    description: str | None = _field(default=None)


@_dataclass
class Register:
    format: list[BitField]
    description: str | None = _field(default=None)


@_dataclass
class Registers:
    X: Register | Unused
    Y: Register | Unused
    Z: Register | Unused = _field(default=Unused.unused)


@_dataclass
class InputOutput:
    IN: str | None = _field(default=None)
    OUT: str | None = _field(default=None)


class DacMode(StrEnum):
    disabled = "disabled"
    enabled = "enabled"


@_dataclass
class Condition:
    dac: DacMode | None = _field(default=None, decoder=DacMode)
    X: BitMask | None = _field(default=None, decoder=BitMask.from_json)
    Y: BitMask | None = _field(default=None, decoder=BitMask.from_json)


@_dataclass
class SmartMode:
    name: str
    encoding: BitMask | list[BitMask] = _field(decoder=BitMask.from_json)
    registers: Registers | None = _field(default=None)
    io: InputOutput | None = _field(default=None)
    condition: Condition | None = _field(default=None)
    description: str | None = _field(default=None)


@_dataclass
class Document:
    smartmodes: dict[str, SmartMode]
