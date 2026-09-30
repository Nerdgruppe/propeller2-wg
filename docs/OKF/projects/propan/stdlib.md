---
type: "Reference"
title: "Propan standard library"
description: "Current predefined constants, builtin expression helpers, and P2 configuration functions exposed by Propan."
tags: ["propan", "stdlib", "constants", "functions", "p2"]
status: "draft"
source_confidence: "high"
---
# Propan standard library

This page describes the standard-library symbols that are loaded by the current P2 assembler. The implementation is the authority for availability. Hardware-facing configuration helpers are documented here as current Propan behavior; unless a helper is explicitly cross-checked elsewhere against canonical P2 documentation, its packed result should not be treated as an independent hardware specification.

## Active library composition

Current semantic analysis loads, in order:

1. common predefined constants;
2. P2 predefined constants;
3. the P2 standard-library function namespace;
4. generated P2 instructions.

The repository also contains P1 constant/function files, but the current `analyze()` path does not load them. They are therefore implementation material, not active P2-language symbols.

The common function source currently contains only commented sketches. The six core helpers `hubaddr`, `cogaddr`, `lutaddr`, `localaddr`, `aug`, and `nrel` are registered directly by semantic analysis rather than through the generated standard-library function namespace. Their address/encoding semantics are documented in [/projects/propan/expressions.md](/projects/propan/expressions.md) and [/projects/propan/addresses-and-segments.md](/projects/propan/addresses-and-segments.md).

There are no additional current builtins named `abs`, generic shifts/rotates, floating-point helpers, or similar Spin-style expression functions. The active callable set is the six semantic helpers plus the P2 functions listed below.

## Calls, parameters, defaults, and diagnostics

Generated standard-library functions carry parameter metadata: parameter name, semantic type, documentation text, and an optional default value. Calls may use positional arguments and named arguments; defaults fill omitted optional parameters.

The wrapper converts Propan values to the declared implementation type before invoking the helper. The currently represented parameter classes are integer, string, address, register, pointer expression, enumerator, and unconstrained `Value`. Integer implementation widths act as range constraints, and some parameters add explicit minimum/maximum checks.

Invalid calls can fail as an argument-count error, type mismatch, invalid argument/range, or integer overflow. Helpers with an evaluation context may also emit their own warning/error diagnostic. The semantic layer converts ordinary evaluation failures into source diagnostics.

Enumerator arguments use Propan enumerator values such as `#nearest`, `#pll`, or `#15pF`; they are not global integer constants. Enumerator acceptance is specific to the parameter's implementation enum.

Boolean parameters accept integer values and the boolean enumerator spellings recognized by the conversion layer. Current accepted enumerator spellings are `#true`, `#yes`, `#on`, `#1`, `#no`, `#off`, and `#0`. The conversion table also contains `false ` with a trailing space rather than `false`; consequently `#false` is not currently accepted by this generic conversion path. This is an implementation quirk, not recommended syntax.

## Common predefined constants

These symbols are active for P2 assembly:

| Name | Value/type | Notes |
|---|---:|---|
| `TRUE` | `0xffffffff` | integer truth constant |
| `FALSE` | `0` | integer false constant |
| `POSX` | `0x7fffffff` | maximum positive signed 32-bit value |
| `NEGX` | `-0x80000000` | minimum signed 32-bit value |
| `PI` | `0x40490fdb` | IEEE-754 single-precision bit pattern for pi, stored as an integer value |

## P2 predefined symbols

### Core and hardware registers

`CHIPVER` is the integer `2`. `altered` is a special register-valued symbol for register index 0, intended for operands affected by `ALT*` instructions.

The active hardware-register symbols are:

```text
IJMP3 IRET3 IJMP2 IRET2 IJMP1 IRET1
PA PB PTRA PTRB DIRA DIRB OUTA OUTB INA INB
```

They map to P2 register indices `$1F0...$1FF` in that order.

### MODCZ/condition enumerator symbols

The following predefined symbols are enumerator values, not integers:

```text
_CLR
_NC_AND_NZ _NZ_AND_NC _GT
_NC_AND_Z _Z_AND_NC
_NC _GE
_C_AND_NZ _NZ_AND_C
_NZ _NE
_C_NE_Z _Z_NE_C
_NZ_OR_NC _NC_OR_NZ
_C_AND_Z _Z_AND_C
_C_EQ_Z _Z_EQ_C
_Z _E
_Z_OR_NC _NC_OR_Z
_C _LT
_C_OR_NZ _NZ_OR_C
_Z_OR_C _C_OR_Z _LE
_SET
```

The leading underscore is part of the predefined symbol name. The stored enumerator text omits it, for example `_GT` yields enumerator `#GT` when consumed by an enumerated operand.

### Smart-pin integer constants

The active `P_*` constants are:

```text
P_TRUE_A P_INVERT_A P_LOCAL_A P_PLUS1_A P_PLUS2_A P_PLUS3_A
P_OUTBIT_A P_MINUS3_A P_MINUS2_A P_MINUS1_A
P_TRUE_B P_INVERT_B P_LOCAL_B P_PLUS1_B P_PLUS2_B P_PLUS3_B
P_OUTBIT_B P_MINUS3_B P_MINUS2_B P_MINUS1_B
P_PASS_AB P_AND_AB P_OR_AB P_XOR_AB P_FILT0_AB P_FILT1_AB P_FILT2_AB P_FILT3_AB
P_LOGIC_A P_LOGIC_A_FB P_LOGIC_B_FB P_SCHMITT_A P_SCHMITT_A_FB P_SCHMITT_B_FB
P_COMPARE_AB P_COMPARE_AB_FB
P_ADC_GIO P_ADC_VIO P_ADC_FLOAT P_ADC_1X P_ADC_3X P_ADC_10X P_ADC_30X P_ADC_100X
P_DAC_990R_3V P_DAC_600R_2V P_DAC_124R_3V P_DAC_75R_2V
P_LEVEL_A P_LEVEL_A_FBN P_LEVEL_B_FBP P_LEVEL_B_FBN
P_ASYNC_IO P_SYNC_IO P_TRUE_IN P_INVERT_IN
P_TRUE_OUTPUT P_TRUE_OUT P_INVERT_OUTPUT P_INVERT_OUT
P_HIGH_FAST P_HIGH_1K5 P_HIGH_15K P_HIGH_150K P_HIGH_1MA P_HIGH_100UA P_HIGH_10UA P_HIGH_FLOAT
P_LOW_FAST P_LOW_1K5 P_LOW_15K P_LOW_150K P_LOW_1MA P_LOW_100UA P_LOW_10UA P_LOW_FLOAT
P_TT_00 P_TT_01 P_TT_10 P_TT_11 P_OE P_CHANNEL P_BITDAC
P_NORMAL P_REPOSITORY P_DAC_NOISE P_DAC_DITHER_RND P_DAC_DITHER_PWM
P_PULSE P_TRANSITION P_NCO_FREQ P_NCO_DUTY
P_PWM_TRIANGLE P_PWM_SAWTOOTH P_PWM_SMPS P_QUADRATURE P_REG_UP P_REG_UP_DOWN
P_COUNT_RISES P_COUNT_HIGHS
P_STATE_TICKS P_HIGH_TICKS P_EVENTS_TICKS P_PERIODS_TICKS P_PERIODS_HIGHS
P_COUNTER_TICKS P_COUNTER_HIGHS P_COUNTER_PERIODS
P_ADC P_ADC_EXT P_ADC_SCOPE P_USB_PAIR P_SYNC_TX P_SYNC_RX P_ASYNC_TX P_ASYNC_RX
```

These are raw integer configuration fields. Composing them with ordinary bitwise operators remains the caller's responsibility unless a higher-level helper below covers the intended configuration.

### Streamer integer constants

The active `X_*` constants are:

```text
X_IMM_32X1_LUT X_IMM_16X2_LUT X_IMM_8X4_LUT X_IMM_4X8_LUT
X_IMM_32X1_1DAC1 X_IMM_16X2_2DAC1 X_IMM_16X2_1DAC2
X_IMM_8X4_4DAC1 X_IMM_8X4_2DAC2 X_IMM_8X4_1DAC4
X_IMM_4X8_4DAC2 X_IMM_4X8_2DAC4 X_IMM_4X8_1DAC8
X_IMM_2X16_4DAC4 X_IMM_2X16_2DAC8 X_IMM_1X32_4DAC8
X_RFLONG_32X1_LUT X_RFLONG_16X2_LUT X_RFLONG_8X4_LUT X_RFLONG_4X8_LUT
X_RFBYTE_1P_1DAC1 X_RFBYTE_2P_2DAC1 X_RFBYTE_2P_1DAC2
X_RFBYTE_4P_4DAC1 X_RFBYTE_4P_2DAC2 X_RFBYTE_4P_1DAC4
X_RFBYTE_8P_4DAC2 X_RFBYTE_8P_2DAC4 X_RFBYTE_8P_1DAC8
X_RFWORD_16P_4DAC4 X_RFWORD_16P_2DAC8 X_RFLONG_32P_4DAC8
X_RFBYTE_LUMA8 X_RFBYTE_RGBI8 X_RFBYTE_RGB8 X_RFWORD_RGB16 X_RFLONG_RGB24
X_1P_1DAC1_WFBYTE X_2P_2DAC1_WFBYTE X_2P_1DAC2_WFBYTE
X_4P_4DAC1_WFBYTE X_4P_2DAC2_WFBYTE X_4P_1DAC4_WFBYTE
X_8P_4DAC2_WFBYTE X_8P_2DAC4_WFBYTE X_8P_1DAC8_WFBYTE
X_16P_4DAC4_WFWORD X_16P_2DAC8_WFWORD X_32P_4DAC8_WFLONG
X_1ADC8_0P_1DAC8_WFBYTE X_1ADC8_8P_2DAC8_WFWORD
X_2ADC8_0P_2DAC8_WFWORD X_2ADC8_16P_4DAC8_WFLONG X_4ADC8_0P_4DAC8_WFLONG
X_DDS_GOERTZEL_SINC1 X_DDS_GOERTZEL_SINC2
X_DACS_OFF X_DACS_0_0_0_0 X_DACS_X_X_0_0 X_DACS_0_0_X_X
X_DACS_X_X_X_0 X_DACS_X_X_0_X X_DACS_X_0_X_X X_DACS_0_X_X_X
X_DACS_0N0_0N0 X_DACS_X_X_0N0 X_DACS_0N0_X_X X_DACS_1_0_1_0
X_DACS_X_X_1_0 X_DACS_1_0_X_X X_DACS_1N1_0N0 X_DACS_3_2_1_0
X_PINS_OFF X_PINS_ON X_WRITE_OFF X_WRITE_ON X_ALT_OFF X_ALT_ON
```

### Cog-start and event constants

```text
COGEXEC COGEXEC_NEW HUBEXEC HUBEXEC_NEW COGEXEC_NEW_PAIR HUBEXEC_NEW_PAIR NEWCOG
EVENT_INT INT_OFF EVENT_CT1 EVENT_CT2 EVENT_CT3
EVENT_SE1 EVENT_SE2 EVENT_SE3 EVENT_SE4
EVENT_PAT EVENT_FBW EVENT_XMT EVENT_XFI EVENT_XRO EVENT_XRL EVENT_ATN EVENT_QMT
```

## P2 standard-library functions

The following table is the current callable namespace loaded from `src/propan/stdlib/p2/functions.zig`. Integer types below describe the implementation width/range; `enum(...)` lists the accepted enumerator domain.

| Function | Parameters | Result / behavior |
|---|---|---|
| `regoffset` | `reg: register`, `offset: i10` | register; wraps the register index using the current implementation arithmetic |
| `bitrange` | `low: u5`, `high: u5` | `u10` packed bit-field; rejects `high < low` |
| `pinrange` | `start: u6`, `end: u6`, `wrap=#unset` | `u11` packed pin field; pins must be in one 32-pin group; wrapping can warn, be accepted, or be rejected |
| `ticks` | `clk: u32`, `s=0`, `ms=0`, `us=0`, `ns=0`, `waitx=false`, `round=#nearest` | `u32` clock count; rounding is `#floor`, `#ceil`, or `#nearest`; `waitx` subtracts two clocks and may warn for very short delays |
| `Hub.reboot` | none | `0x10000000` HUBSET reset word |
| `Hub.seedRng` | `seed: u31` | HUBSET RNG-seed word |
| `Hub.debugConfig` | `debug_enable: u16`, `write_protect=false`, `lock=false` | packed HUBSET debug word |
| `Hub.setFilter` | `filter: enum(filt0..filt3)`, `length`, `tap: u5` | packed HUBSET filter word; length must be 2, 3, 5, or 8 |
| `Hub.clockMode` | `pll`, `in_div: 1..64`, `mul: 1..1024`, `out_div`, `xi`, `sysclk` | packed HUBSET clock word; validates supported output divisors and source/PLL combinations |
| `Hub.fifoConfig` | `blocks: u14`, `no_wait=false` | RDFAST/WRFAST D configuration word |
| `SmartPin.Pulse.config` | `base: u16`, `threshold: u16` | packed X value |
| `SmartPin.Pwm.config` | `base: u16`, `frame: u16` | packed X value |
| `SmartPin.Nco.config` | `base: u16`, `phase: u16` | packed X value |
| `SmartPin.SyncTx.config` | `bits: 1..32`, `start_stop=false` | synchronous-TX X value |
| `SmartPin.SyncRx.config` | `bits: 1..32`, `on_edge=false` | synchronous-RX X value |
| `SmartPin.Adc.config` | `mode`, `period_exp: u4` | ADC X value; maximum period exponent depends on mode |
| `SmartPin.Scope.config` | `b: u6`, `a: u6`, `filter` | ADC-scope X value |
| `SmartPin.Scope.pipeConfig` | `base_pin: u6`, `enabled=true` | SETSCP D value; `base_pin` must be divisible by 4 |
| `SmartPin.Usb.config` | `baud: u32`, `clk: u32`, `host=false`, `full_speed=false` | USB-pair X value; rejects zero/impossible clock ratios |
| `SmartPin.Measure.eventY` | `sensitivity`, `timeout=false` | P_EVENTS_TICKS Y value |
| `SmartPin.Measure.periodY` | `a_edge=false`, `b_edge=false` | P_PERIODS/P_COUNTER Y value |
| `SmartPin.UartTx.config` | `baud: u64`, `clk: u64`, `bits: 1..32 = 8` | asynchronous-UART X value |
| `SmartPin.UartRx.config` | `baud: u64`, `clk: u64`, `bits: 1..32 = 8` | asynchronous-UART X value |
| `Event.pin` | `pin: u6`, `kind` | SETSE event selector for pin rise/fall/change/low/high |
| `Event.lock` | `lock: u4`, `kind` | SETSE hub-lock rise/fall/change selector |
| `Event.lut` | `address: $1FC..$1FF`, `write=false`, `companion=false` | SETSE LUT event selector |

### Function enumerator domains

| Domain/use | Accepted enumerators |
|---|---|
| `ticks.round` | `#floor`, `#ceil`, `#nearest` |
| `Hub.setFilter.filter` | `#filt0`, `#filt1`, `#filt2`, `#filt3` |
| `Hub.clockMode.xi` | `#float`, `#nocap`, `#15pF`, `#30pF` |
| `Hub.clockMode.sysclk` | `#rcfast`, `#rcslow`, `#xi`, `#pll` |
| `SmartPin.Adc.config.mode` | `#sample`, `#sinc2`, `#sinc3`, `#bits` |
| `SmartPin.Scope.config.filter` | `#tukey68`, `#tukey45`, `#hann28` |
| `SmartPin.Measure.eventY.sensitivity` | `#high`, `#rise`, `#edge` |
| `Event.pin.kind` | `#rise`, `#fall`, `#change`, `#low`, `#high` |
| `Event.lock.kind` | `#rise`, `#fall`, `#change` |

`#15pF` is therefore a concrete example of an enumerator/value token whose meaning is determined by the receiving function parameter, not by a global symbol table.

## P1 definitions present in the repository

The inactive P1 constants file defines `CHIPVER=1`, the clock-mode names `RCFAST`, `RCSLOW`, `XINPUT`, `XTAL1`, `XTAL2`, `XTAL3`, `PLL1X`, `PLL2X`, `PLL4X`, `PLL8X`, and `PLL16X`, plus register-address integers `PAR`, `CNT`, `INA`, `INB`, `OUTA`, `OUTB`, `DIRA`, `DIRB`, `CTRA`, `CTRB`, `FRQA`, `FRQB`, `PHSA`, `PHSB`, `VCFG`, and `VSCL`.

The P1 functions file currently defines no functions. None of these P1-only definitions are loaded by the current P2 analyzer.

## Generated reference support

The CLI already exposes `--render-stdlib-docs <path>` (or `-` for standard output). It renders the active P2 constant map and generated P2 function metadata to HTML. That generated output should remain the exhaustive machine-derived reference for names, values, parameters, defaults, and implementation documentation; this page should focus on language-facing semantics, grouping, caveats, and links.

When standard-library definitions change, prefer validating or regenerating their reference from the metadata rather than hand-maintaining a second independent list.
