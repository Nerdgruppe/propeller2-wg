import logging

from typing import Callable, Optional

from .types import ConstValue, IncompleteValue, MemoryAddress


class Library:
    functions: dict[str, Callable]

    def __init__(self):
        self.functions = dict()

    def get_function(self, name: str) -> Optional[Callable]:
        return self.functions.get(name)

    def add_function(self, name: str, func: Callable) -> None:
        assert name not in self.functions
        self.functions[name] = func

    def register_function(self, func) -> None:
        assert func.__name__ is not None
        self.add_function(func.__name__, func)

    def register_named_function(self, name: str) -> Callable:
        def _wrap(func):
            self.add_function(name, func)

        return _wrap


def StandardLibrary() -> Library:
    lib = Library()

    function = lib.register_function
    named_function = lib.register_named_function

    @function
    def aug(value):
        return value

    @function
    def hubaddr(value):
        if not isinstance(value, MemoryAddress):
            raise ValueError(
                f"Unexpected value of type {type(value)}. Expected MemoryAddress!"
            )
        return ConstValue(value.hub_address)

    @function
    def localaddr(value):
        print(type(value), repr(value))
        return value

    @named_function("Hub.clockMode")
    def clockMode(
        pll: str,
        in_div: int,
        mul: int,
        out_div: int,
        xi: str,
        sysclk: str,
    ) -> int:
        logging.error("implement Hub.clockMode()")
        return 0

    @named_function("SmartPin.UartTx.mode")
    def uart_tx_mode(dir: str) -> int:
        logging.error("implement SmartPin.UartTx.mode()")
        return 0

    @named_function("SmartPin.UartTx.config")
    def uart_tx_config(baud: int, bits: int, clk: int) -> int:
        logging.error("implement SmartPin.UartTx.config()")
        return 0

    @named_function("SmartPin.UartRx.mode")
    def uart_rx_mode() -> int:
        logging.error("implement SmartPin.UartRx.mode()")
        return 0

    @named_function("SmartPin.UartRx.config")
    def uart_rx_config(baud: int, bits: int, clk: int) -> int:
        logging.error("implement SmartPin.UartRx.config()")
        return 0

    return lib
