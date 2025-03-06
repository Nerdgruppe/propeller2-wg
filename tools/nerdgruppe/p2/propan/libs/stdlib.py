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

        @function
        def reboot() -> int:
            """
            Hard reset, reboots chip
            """
            # %0001_xxxx_xxxx_xxxx_xxxx_xxxx_xxxx_xxxx
            return 0x1000_0000

        @function
        def setFilter(r: int, length: int, tap: int) -> int:

        @function
        def seedRng(seed: int) -> int:
            """
            Seed Xoroshiro128** PRNG with `seed`
            """

            assert (seed & ~0x7FFF_FFFF) == 0
            return seed | 0x8000_0000

    class SmartPin(Namespace):
        class UartTx(Namespace):
            @function
            def mode(dir: str) -> int:
                logging.error("implement SmartPin.UartTx.mode()")
                return 0

            @function
            def config(baud: int, clk: int, bits: int = 8) -> int:
                assert bits >= 1 and bits <= 32, "bits must be between 1 and 32!"

                # X[31:16] establishes the number of clocks in a bit period, and in case X[31:26] is zero, X[15:10]
                # establishes the number of fractional clocks in a bit period. The X bit period value can be simply computed
                # as: (clocks * $1_0000) & $FFFFFC00. For example, 7.5 clocks would be $00078000, and 33.33 clocks
                # would be $00215400.

                # Use float here to support fractional divisions:
                clocks: float = clk / baud

                # Cast back after multiplying with the hex value:
                config_long: int = int(clocks * 0x1_0000) & 0xFFFFFC00

                # Add number of bits:
                config_long += bits - 1

                return config_long

        class UartRx(Namespace):
            @function
            def mode() -> int:
                logging.error("implement SmartPin.UartRx.mode()")
                return 0

            @function
            def config(baud: int, clk: int, bits: int = 8) -> int:
                return StandardLibrary.SmartPin.UartTx.config(baud=baud, bits=bits, clk=clk)
