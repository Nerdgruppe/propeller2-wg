import logging

from ..library import Library, Namespace, function
from ..types import ConstValue, MemoryAddress, Value


class BuiltinFunctions(Library):
    @function
    def aug(value: Value) -> Value:
        return value

    @function
    def hubaddr(value: MemoryAddress) -> ConstValue:
        if not isinstance(value, MemoryAddress):
            raise ValueError(
                f"Unexpected value of type {type(value)}. Expected MemoryAddress!"
            )
        return ConstValue(value.hub_address)

    @function
    def localaddr(value: Value):
        print(type(value), repr(value))
        return value
