import logging
from enum import Enum

from nerdgruppe.utils.structs import packed_struct, field

from ..library import Library, Namespace, function
from ..types import (
    ConstValue,
    IncompleteValue,
    MemoryAddress,
    u5,
    u16,
    u31,
    MinMaxInt,
    LookUpTable,
)


class FilterName(Enum):
    filt0 = 0
    filt1 = 1
    filt2 = 2
    filt3 = 3


@packed_struct
class _FilterConfig:
    """
    %0100_xxxx_xxxx_xxxx_xxxx_xxxR_RLLT_TTTT
    """

    tap = field(bits=5)
    length = field(bits=2)
    filter = field(bits=2)
    _padding = field(bits=19, default=0)
    tag = field(bits=4, default=0b0100)


@packed_struct
class _DebugConfig:
    """
    %0010_xxxx_xxxx_xxLW_DDDD_DDDD_DDDD_DDDD
    """

    debug_mask = field(bits=16)
    write_protect = field(bits=1)
    locked = field(bits=1)
    _padding = field(bits=10, default=0)
    tag = field(bits=4, default=0b0010)


class CrystalMode(LookUpTable):
    @classmethod
    def get_mapping(cls) -> dict[str, int]:
        return {
            "float": 0,
            "nocap": 1,
            "15pF": 2,
            "30pF": 3,
        }


class ClockSource(Enum):
    rcfast = 0b00
    rcslow = 0b01
    xi = 0b10
    pll = 0b11


@packed_struct
class _ClockMode:
    """
    # %0000_000E_DDDD_DDMM_MMMM_MMMM_PPPP_CCSS
    """
    clock_src = field(bits=2)
    crystal = field(bits=2)
    vco_div = field(bits=4)
    vco_mul = field(bits=10)
    xi_div = field(bits=6)
    pll_on = field(bits=1)
    _padding = field(bits=3, default=0)
    tag = field(bits=4, default=0b0000)


class StandardLibrary(Library):
    class Hub(Namespace):
        @function
        def clockMode(
            pll: bool,
            in_div: MinMaxInt(1, 64),
            mul: MinMaxInt(1, 1024),
            out_div: int,
            xi: CrystalMode,
            sysclk: ClockSource,
        ) -> int:
            _VCO_DIVS = (
                2,  # 0 = VCO / 2
                4,  # 1 = VCO / 4
                6,  # 2 = VCO / 6
                8,  # 3 = VCO / 8
                10,  # 4 = VCO / 10
                12,  # 5 = VCO / 12
                14,  # 6 = VCO / 14
                16,  # 7 = VCO / 16
                18,  # 8 = VCO / 18
                20,  # 9 = VCO / 20
                22,  # 10 = VCO / 22
                24,  # 11 = VCO / 24
                26,  # 12 = VCO / 26
                28,  # 13 = VCO / 28
                3,  # 14 = VCO / 3
                1,  # 15 = VCO / 1
            )
            assert out_div in _VCO_DIVS, (
                f"out_div must be one of {', '.join(_VCO_DIVS)}, but found {out_div!r}"
            )

            # print("pll     = ", pll, ":", type(pll))
            # print("in_div  = ", in_div, ":", type(in_div))
            # print("mul     = ", mul, ":", type(mul))
            # print("out_div = ", out_div, ":", type(out_div))
            # print("xi      = ", xi, ":", type(xi))
            # print("sysclk  = ", sysclk, ":", type(sysclk))

            return _ClockMode(
                clock_src=sysclk.value,
                crystal=xi,
                vco_div=_VCO_DIVS.index(out_div ),
                vco_mul=mul - 1,
                xi_div=in_div - 1,
                pll_on=pll,
            ).to_int()

        @function
        def reboot() -> int:
            """
            Hard reset, reboots chip
            """
            # %0001_xxxx_xxxx_xxxx_xxxx_xxxx_xxxx_xxxx
            return 0x1000_0000

        @function
        def setFilter(filter: FilterName, length: int, tap: u5) -> int:
            """
            Set `filter` to fifo count `length` (2,3,5 or 8) and `tap` bit (0..31).
            """
            valid_options = (2, 3, 5, 8)
            assert length in valid_options, (
                f"length must be one of [{', '.join(valid_options)}], but was {length!r}"
            )
            assert tap >= 0 and tap <= 31, (
                f"tap must be between 0 and 31, but was {tap!r}"
            )

            return ConstValue(
                _FilterConfig(
                    filter=filter.value,
                    length=valid_options.index(length),
                    tap=tap,
                ).to_int()
            )

        @function
        def seedRng(seed: u31) -> int:
            """
            Seed Xoroshiro128** PRNG with `seed`
            """

            assert (seed & ~0x7FFF_FFFF) == 0
            return seed | 0x8000_0000

        @function
        def debugConfig(
            debug_enable: u16, write_protect: bool = False, lock: bool = False
        ) -> int:
            """
            - `debug_enable`: Debug interrupt enables for cogs 15..0, respectively
            - `write_protect`: Write-protect last 16KB of hub RAM
            - `lock`: Lock W and `write_protect` bit settings until next reset
            """

            return _DebugConfig(
                debug_mask=debug_enable,
                write_protect=write_protect,
                locked=lock,
            ).to_int()

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
                return StandardLibrary.SmartPin.UartTx.config(
                    baud=baud, bits=bits, clk=clk
                )
