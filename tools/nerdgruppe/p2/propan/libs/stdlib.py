import logging

from ..library import Library, Namespace, function
from ..types import ConstValue, IncompleteValue, MemoryAddress


class StandardLibrary(Library):
    class Hub(Namespace):
        @function
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

    class SmartPin(Namespace):
        class UartTx(Namespace):
            @function
            def mode(dir: str) -> int:
                logging.error("implement SmartPin.UartTx.mode()")
                return 0

            @function
            def config(baud: int, bits: int, clk: int) -> int:
                logging.error("implement SmartPin.UartTx.config()")
                return 0

        class UartRx(Namespace):
            @function
            def mode() -> int:
                logging.error("implement SmartPin.UartRx.mode()")
                return 0

            @function
            def config(baud: int, bits: int, clk: int) -> int:
                logging.error("implement SmartPin.UartRx.config()")
                return 0
