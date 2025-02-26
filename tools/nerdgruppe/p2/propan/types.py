import inspect
import logging
import io

from enum import Enum
from dataclasses import dataclass, field
from typing import Optional, Callable, TypeAlias
from abc import ABC, abstractmethod


class SymbolType(Enum):
    code = "code"  # <name>:
    data = "data"  # var <name>:
    const = "const"  # const <name> = ...


class AddressingMode(Enum):
    cogexec = "cogexec"
    lutexec = "lutexec"
    hubexec = "hubexec"


class ExecutionCluster:
    """
    A cluster is everything between the switch of the addressing modes.

    You can only refer directly to local symbols in the same execution cluster.
    """

    hub_base: int


class BinaryStream:
    def __init__(self):
        self.io = io.BytesIO()

    def tell(self) -> int:
        return self.io.tell()

    def write(self, data: bytes):
        self.io.write(data)

    def write_int(self, val: int, bits: int, signed: bool):
        self.write(val.to_bytes(length=bits // 8, byteorder="little", signed=signed))

    def write_u8(self, val: int):
        self.write_int(val, 8, False)

    def write_u16(self, val: int):
        self.write_int(val, 32, False)

    def write_u32(self, val: int):
        self.write_int(val, 32, False)

    def write_i8(self, val: int):
        self.write_int(val, 8, True)

    def write_i16(self, val: int):
        self.write_int(val, 32, True)

    def write_i32(self, val: int):
        self.write_int(val, 32, True)

    def get_bytes(self) -> bytes:
        return self.io.getvalue()


@dataclass(kw_only=True)
class AssemblerState:
    # Total hub address, ever incrementing
    hub_address: int = field(default=0)

    # Local address, either HUB, COG or LUT
    local_address: int = field(default=0)

    # Defines what local_address refers to and how instructions are executed.
    addressing_mode: AddressingMode = field(default=AddressingMode.cogexec)

    # The current lut/cog exec instruction cluster
    current_cluster: ExecutionCluster | None = field(default_factory=ExecutionCluster)


class Value(ABC):
    """
    A generic "value" which can resolve into a 32-bit constant,
    a hub-relative offset or a cog-relative offset.
    """

    @abstractmethod
    def get_value(self, state: AssemblerState) -> int: ...

    @abstractmethod
    def resolve(self, state: AssemblerState) -> "Value": ...


class ConstValue(Value):
    """
    A value that evaluates to a constant value.
    """

    value: int

    def __init__(self, value: int):
        self.value = value

    def __str__(self):
        return str(self.value)

    def __repr__(self):
        return f"ConstValue({self.value})"

    def get_value(self, state: AssemblerState) -> int:
        return self.value

    def resolve(self, state: AssemblerState) -> "Value":
        return self


class PoisonValue(Value):
    """
    A value which cannot be resolved, but has all properties of a value.
    """

    def __init__(self):
        pass

    def __str__(self):
        return "Poison"

    def __repr__(self):
        return "Poison"

    def get_value(self, state: AssemblerState) -> int:
        raise ValueError("A poison value cannot be resolved.")

    def resolve(self, state: AssemblerState) -> "Value":
        return self


class IncompleteValue(Value):
    """
    A value which has properties of the actual value, but it is not available yet.
    """

    compute: Callable

    def __init__(self, compute: Callable):
        self.compute = compute

    def __str__(self):
        return "Incomplete"

    def __repr__(self):
        return "Incomplete"

    def get_value(self, state: AssemblerState) -> int:
        raise ValueError("An incomplete value cannot be resolved.")

    def resolve(self, state: AssemblerState) -> "Value":
        return self.compute(state)

    def wrap(self, wrapper: Callable):
        return IncompleteValue(
            lambda state: wrapper(self.resolve(state), state),
        )


class MemoryAddress(Value):
    """
    A memory address is a value which refers to a hub address,
    and optionally a cog offset.
    """

    hub_address: int
    local_address: int
    cluster: ExecutionCluster | None

    def __init__(self, *, cluster: ExecutionCluster | None, hub: int, local: int):
        assert isinstance(hub, int)
        assert isinstance(local, int)

        self.cluster = cluster
        self.hub_address = hub
        self.local_address = local

    def get_value(self, state: AssemblerState) -> int:
        return self.local_address

    def __str__(self):
        if self.cluster is not None:
            return f"0x{self.hub_address:06X}:0x{self.local_address:03X}"
        else:
            assert self.hub_address == self.local_address
            return f"0x{self.hub_address:06X}"

    def __repr__(self):
        if self.cluster is not None:
            return f"MemoryAddress(hub=0x{self.hub_address:06X}, local=0x{self.local_address:03X})"
        else:
            return f"MemoryAddress(hub=0x{self.hub_address:06X}"

    def resolve(self, state: AssemblerState) -> "Value":
        return self
