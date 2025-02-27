import logging

from ..library import Library, Namespace, function
from ..types import ConstValue, MemoryAddress


class BuiltinFunctions(Library):
    @function
    def aug(value):
        return value

    @function
    def hubaddr(value):
        if not isinstance(value, MemoryAddress):
            raise ValueError(f"Unexpected value of type {type(value)}. Expected MemoryAddress!")
        return ConstValue(value.hub_address)

    @function
    def localaddr(value):
        print(type(value), repr(value))
        return value
