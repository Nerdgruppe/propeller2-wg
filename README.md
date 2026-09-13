# Propan - Alternative Propeller 2 Assembler

Propan is an alternative assembler and assembly syntax that is catered for a more strict and well-defined behaviour.

## Building

Install [Zig 0.16](https://ziglang.org/download/#release-0.16.0), then build the project:

```sh-session
[user@machine propeller2-wg]$ zig build -Doptimize=ReleaseSafe
[user@machine propeller2-wg]$
[user@machine propeller2-wg]$ ls ./zig-out/bin/
propan  turboprop  windtunnel
[user@machine propeller2-wg]$ ./zig-out/bin/propan --help
Usage: ./zig-out/bin/propan [-h] [-o <output>] <sources...>

Propan is an assembler for the Propeller 2 architecture.

Options:
  -h, --help         Prints this help text
  -o, --output       Sets the path of the output file.
      --test-mode    <internal use only>
      --compare-to   <internal use only>
  -v, --verbose      Enables debug logging
  -f, --format       Selects the binary format to use
  -F, --fill-byte    The byte value which is used to fill empty/undefined space in the binary. Defaults to 0x00.
      --list-file    Writes a list file to the given path. Use '-' to write to stdout.
[user@machine propeller2-wg]$ 
```

If you want to also have the tools [loadp2](https://github.com/totalspectrum/loadp2) and [flexspin](https://github.com/totalspectrum/flexprop), you can use `-Dwith-flexspin` on the `zig build` invocation:

```sh-session
[user@machine propeller2-wg]$ zig-0.16.0 build -Dwith-flexspin -Doptimize=ReleaseSafe
install
└─ install flexspin
   └─ compile exe flexspin Debug native
      └─ run exe byacc (cgram) w
./.zig-cache/o/acb1656cfd74dec78085048b417c27f6/byacc: 4 shift/reduce conflicts.
failed command: ./.zig-cache/o/acb1656cfd74dec78085048b417c27f6/byacc -s -p cgramyy -t -l -b /home/felix/projects/nerdgruppe/propeller2-wg/.zig-cache/o/26f59ccb55c72d171707406b90d1ecfc/cgram -d /home/felix/projects/nerdgruppe/propeller2-wg/zig-pkg/N-V-__8AAOUErQDX6Zwe1aeK_COII7E566nhOgG8bC4qX88S/frontends/c/cgram.y

install
└─ install flexspin
   └─ compile exe flexspin Debug native
      └─ run exe byacc (basic) w
./.zig-cache/o/acb1656cfd74dec78085048b417c27f6/byacc: 52 shift/reduce conflicts.
failed command: ./.zig-cache/o/acb1656cfd74dec78085048b417c27f6/byacc -s -p basicyy -t -l -b /home/felix/projects/nerdgruppe/propeller2-wg/.zig-cache/o/21a5fbee1365a1862eba4373e37552fb/basic -d /home/felix/projects/nerdgruppe/propeller2-wg/zig-pkg/N-V-__8AAOUErQDX6Zwe1aeK_COII7E566nhOgG8bC4qX88S/frontends/basic/basic.y

install
└─ install flexspin
   └─ compile exe flexspin Debug native
      └─ run exe byacc (spin) w
./.zig-cache/o/acb1656cfd74dec78085048b417c27f6/byacc: 44 shift/reduce conflicts.
failed command: ./.zig-cache/o/acb1656cfd74dec78085048b417c27f6/byacc -s -p spinyy -t -l -b /home/felix/projects/nerdgruppe/propeller2-wg/.zig-cache/o/7d79a20a5a7e5cb0883d16882f4252b7/spin -d /home/felix/projects/nerdgruppe/propeller2-wg/zig-pkg/N-V-__8AAOUErQDX6Zwe1aeK_COII7E566nhOgG8bC4qX88S/frontends/spin/spin.y

[user@machine propeller2-wg]$ ls zig-out/bin/
flexspin  loadp2  propan  turboprop  windtunnel
[user@machine propeller2-wg]$ ./zig-out/bin/flexspin -h
Propeller Spin/PASM Compiler 'FlexSpin' (c) 2011-2025 Total Spectrum Software Inc. and contributors
Version 7.1.1 Compiled on: Sep 13 2026
usage: ./zig-out/bin/flexspin [options] filename.spin | filename.bas
  [ -h ]              display this help
  [ -L or -I <path> ] add a directory to the include path
  [ -o <name> ]      set output filename to <name>
  [ -b ]             output binary file format
  [ -e ]             output eeprom file format
  [ -c ]             output only DAT sections
  [ -l ]             output DAT as a listing file
  [ -f ]             output list of file names
  [ -g ]             enable debug statements
  [ -q ]             quiet mode (suppress banner and non-error text)
  [ -p ]             disable the preprocessor
  [ -D <define> ]    add a define
  [ -u ]             ignore for openspin compatibility (unused method elimination always enabled)
  [ -2 ]             compile for Prop2
  [ -2nu ]           compile for Prop2 with Nu interpreter
  [ -O# ]            set optimization level:
          -O0 = no optimization
          -O1 = basic optimization
          -O2 = all optimization
  [ -H nnnn ]        set starting hub address
  [ -E ]             skip initial coginit code (usually used with -H)
  [ -w ]             compile for COG with Spin wrappers
  [ -Wall ]          enable warnings for language extensions and other features
  [ -Werror ]        make warnings into errors
  [ -Wabs-paths ]    print absolute paths for file names in errors/warnings
  [ -Wmax-errors=N ] allow at most N errors in a pass before stopping
  [ -C ]             enable case sensitive mode
  [ -x ]             capture program exit code (for testing)
  [ --charset=xxx ]  set character set for runtime
           xxx is one of utf8, latin1, shiftjis, or parallax
  [ --code=cog ]     compile for COG mode instead of LMM
  [ --compress ]     compress output binary for faster download
  [ --interp=rom ]   compile bytecodes for P1 ROM interpreter (alpha feature!)
  [ --interp=nu ]    compile bytecodes for NuCode interpreter (alpha feature!)
  [ --fcache=N ]     set FCACHE size to N (0 to disable)
  [ --fixedreal ]    use 16.16 fixed point in place of floats
  [ --lmm=xxx ]      use alternate LMM implementation for P1
           xxx = orig uses original flexspin LMM
           xxx = slow uses traditional (slow) LMM
  [ --nostdlib]      skip searching in the standard library location for include files
  [ --sizes]         print code and interpreter sizes
  [ --tabs=N ]       assume tabs are set every N spaces for indentation purposes
  [ --verbose ]      print additional diagnostic messages (for debugging the compiler)
  [ --version ]      just show compiler version
  [ --zip ]          create zip archive of source files
[user@machine propeller2-wg]$ ./zig-out/bin/loadp2
Must specify a file name or -t or -x
loadp2 - a loader for the propeller 2 - version 0.075 Sep 13 2026
usage: loadp2
         [ -p port ]               serial port
         [ -b baud ]               user baud rate (default is 115200)
         [ -l baud ]               loader baud rate (default is 2000000)
         [ -f clkfreq ]            clock frequency (default is 80000000)
         [ -m clkmode ]            clock mode in hex (default is ffffffff)
         [ -s address ]            starting address in hex (default is 0)
         [ -t ]                    enter terminal mode after running the program
         [ -T ]                    enter PST-compatible terminal mode
         [ -v ]                    enable verbose mode
         [ -k ]                    wait for user input before exit
         [ -q ]                    quiet mode: also checks for exit sequence
         [ -n ]                    no reset; skip any hardware reset
         [ -9 dir ]                serve 9p remote filesystem from dir
         [ -FIFO bytes]            modify serial FIFO size (default is 2048 bytes)
         [ -? ]                    display a usage message and exit
         [ -DTR ]                  use DTR for reset (default)
         [ -RTS ]                  use RTS for reset
         [ -xDEBUG ]               enter ROM debug monitor
         [ -xTAQOZ ]               enter ROM version of TAQOZ
         [ -xTERM ]                enter terminal, avoid reset
         [ -NOZERO ]               do not clear memory before download (default)
         [ -ZERO ]                 clear memory before download
         [ -PATCH ]                patch in clock frequency and serial parms
         [ -SINGLE ]               set load mode for single stage
         [ -FLASH ]                program application to SPI flash
         [ -NOEOF ]                ignore EOF on input
         [ -HIMEM=flash ]          addresses 0x8000000 and up refer to flash
         filespec                  file to load
         [ -e script ]             send a sequence of characters after starting P2
         [ -a arg1 [arg2 ...] ]    put arguments for program into memory

In -CHIP mode, filespec may optionally be multiple files with address
specifiers, such as:
    @ADDR=file1,@ADDR=file2,@ADDR+file3
Here ADDR is a hex address at which to load the next file, followed by = or +
If it is followed by + then the size of the file is put in memory followed by
the file data. This feature is useful for loading data that a program wishes
to act on. For example, a VGA program which displays data from $1000 may be
loaded with:
    @0=vgacode.bin,@1000=picture.bmp
The main executable code must always be specified first
[user@machine propeller2-wg]$ 
```

## Syntax

### Constants

Constants create a name associated with an integer value:

```propan
const MY_CONSTANT = 10
```

### Conditionals

| Encoding | Propan                 | PASM                                                     | C | Op  | Z  | Description                                                              |
|----------|------------------------|----------------------------------------------------------|---|-----|----|--------------------------------------------------------------------------|
| `0b0000` | `return`               | `_RET_`                                                  | - |     | -  | always; execute instruction then return if no branch; no context restore |
| `0b0001` | `if(!C & !Z)`, `if(>)` | `IF_00`, `IF_A`, `IF_GT`, `IF_NC_AND_NZ`, `IF_NZ_AND_NC` | 0 | and | 0  | if comparison/subtraction was greater than (C = 0 and Z = 0)             |
| `0b0010` | `if(!C & Z)`           | `IF_01`, `IFwo_NC_AND_Z`, `IF_Z_AND_NC`                  | 0 | and | 1  | if C clear and Z set (C = 0 and Z = 1)                                   |
| `0b0011` | `if(!C)`,`if(>=)`      | `IF_0X`, `IF_AE`, `IF_GE`, `IF_NC`                       | 0 |     | -  | if comparison/subtraction was above or equal (C = 0)                     |
| `0b0100` | `if(C & !Z)`           | `IF_10`, `IF_C_AND_NZ`, `IF_NZ_AND_C`                    | 1 | and | 0  | if C set and Z clear (C = 1 and Z = 0)                                   |
| `0b0101` | `if(!Z)`,`if(!=)`      | `IF_X0`, `IF_NE`, `IF_NZ`                                | - |     | 0  | if comparison/subtraction was not equal (Z = 0)                          |
| `0b0110` | `if(C != Z)`           | `IF_DIFF`, `IF_C_NE_Z`, `IF_Z_NE_C`                      | x | and | !x | if C not equal to Z (C = 0 and Z = 1 *or* C = 1 and Z = 0)               |
| `0b0111` | `if(!C \| !Z)`         | `IF_NOT_11`, `IF_NC_OR_NZ`, `IF_NZ_OR_NC`                | 0 | or  | 0  | if C clear or Z clear (C = 1 or Z = 0)                                   |
| `0b1000` | `if(C & Z)`            | `IF_11`, `IF_C_AND_Z`, `IF_Z_AND_C`                      | 1 | and | 1  | if C set and Z set (C = 1 and Z = 1)                                     |
| `0b1001` | `if(C == Z)`           | `IF_SAME`, `IF_C_EQ_Z`, `IF_Z_EQ_C`                      | x | and | x  | if C equal to Z (C = 0 and Z = 0 *or* C = 1 and Z = 1)                   |
| `0b1010` | `if(Z)`, `if(==)`      | `IF_X1`, `IF_E`, `IF_Z`                                  | - |     | 1  | if comparison/subtraction was equal (Z = 1)                              |
| `0b1011` | `if(!C \| Z)`          | `IF_NC_OR_Z`, `IF_NOT_10`, `IF_Z_OR_NC`                  | 0 | or  | 1  | if C clear or Z set (C = 0 or Z = 1)                                     |
| `0b1100` | `if(C)`, `if(<)`       | `IF_1X`, `IF_B`, `IF_C`, `IF_LT`                         | 1 |     | -  | if comparison/subtraction was less than (C = 1)                          |
| `0b1101` | `if(C \| !Z)`          | `IF_C_OR_NZ`, `IF_NOT_01`, `IF_NZ_OR_C`                  | 1 | or  | 0  | if C set or Z clear (C = 1 or Z = 0)                                     |
| `0b1110` | `if(C \| Z)`, `if(<=)` | `IF_BE`, `IF_C_OR_Z`, `IF_LE`, `IF_NOT_00`, `IF_Z_OR_C`  | 1 | or  | 1  | if comparison/subtraction was less than or equal (C = 1 or Z = 1)        |
| `0b1111` |                        |                                                          | - |     | -  | always; this is the default, no condition expressed                      |

### Effects

| Propan   | PASM   | Description                                                    |
|----------|--------|----------------------------------------------------------------|
| `:and_c` | `ANDC` | AND tested bit/pin into current C; used on TESTxx instructions |
| `:and_z` | `ANDZ` | AND tested bit/pin into current Z; used on TESTxx instructions |
| `:or_c`  | `ORC`  | OR tested bit/pin into current C; used on TESTxx instructions  |
| `:or_z`  | `ORZ`  | OR tested bit/pin into current Z; used on TESTxx instructions  |
| `:xor_c` | `XORC` | XOR tested bit/pin into current C; used on TESTxx instructions |
| `:xor_z` | `XORZ` | XOR tested bit/pin into current Z; used on TESTxx instructions |
| `:wc`    | `WC`   | Write C arg; used on many instructions                         |
| `:wcz`   | `WCZ`  | Write both C and Z args; used on many instructions             |
| `:wz`    | `WZ`   | Write Z arg; used on many instructions                         |

### Unary Operators

| Propan      | PASM                    | Description                                        |
|-------------|-------------------------|----------------------------------------------------|
| `!`         | `!!`                    | Boolean: NOT (0 => TRUE, else => FALSE)            |
| `~`         | `!`                     | Bitwise: NOT                                       |
| `+`         | `+`                     | Positive (+X) unary form of Add                    |
| `-`         | `-`                     | Negate (−X); unary form of Subtract                |
| `@`         | `@` (inside `REP`)      | PC-relative offset to label                        |
| `hubaddr()` | `@` (when not in `REP`) | Absolute hub address of label                      |
| `*`         | *n.a.*                  | Derference code label                              |
| `&`         | "`#`"                   | Address of data label                              |
| `abs()`     | `ABS`                   | Absolute value                                     |
| `fabs()`    | `FABS`                  | Floating-point absolute value (clears MSB)         |
| `encod()`   | `ENCOD`                 | Encode MSB, 0..31                                  |
| `decod()`   | `DECOD`                 | Decode, 1 << (x & $1F)                             |
| `bmask()`   | `BMASK`                 | Bitmask, (2 << (x & $1F)) - 1                      |
| `popcnt()`  | `ONES`                  | Sum all '1' bits, 0..32                            |
| `sqrt()`    | `SQRT`                  | Square root of unsigned value                      |
| `fsqrt()`   | `FSQRT`                 | Floating-point square root                         |
| `qlog()`    | `QLOG`                  | Unsigned value to logarithm {5'whole, 27'fraction} |
| `qexp()`    | `QEXP`                  | Logarithm to unsigned value                        |

### Binary Operators

| Precedence Group | Propan       | PASM      | Description                                            |
|------------------|--------------|-----------|--------------------------------------------------------|
| 0                | `and`        | `&&`      | Boolean: AND                                           |
| 0                | `or`         | `\|\|`    | Boolean: OR                                            |
| 0                | `xor`        | `^^`      | Boolean: XOR                                           |
| 1                | `==`         | `==`      | Boolean: Is equal                                      |
| 1                | `!=`         | `<>`      | Boolean: Is not equal                                  |
| 1                | `<=>`        | `<=>`     | Signed comparison (<, =, > returns -1, 0, 1)           |
| 1                | `<`          | `<`       | Boolean: Is less than (signed)                         |
| 1                | *n.a.*       | `+<`      | Boolean: Is less than (unsigned)                       |
| 1                | `>`          | `>`       | Boolean: Is greater than (signed)                      |
| 1                | *n.a.*       | `+>`      | Boolean: Is greater than (unsigned)                    |
| 1                | `<=`         | `<=`      | Boolean: Is less than or equal (signed)                |
| 1                | *n.a.*       | `+<=`     | Boolean: Is less than or equal (unsigned)              |
| 1                | `>=`         | `>=`      | Boolean: Is greater than or equal (signed)             |
| 1                | *n.a.*       | `+>=`     | Boolean: Is greater than or equal (unsigned)           |
| 2                | `+`          | `+`       | Add                                                    |
| 2                | `-`          | `-`       | Subtract                                               |
| 2                | `\|`         | `\|`      | Bitwise: OR                                            |
| 2                | `^`          | `^`       | Bitwise: XOR                                           |
| 4                | `>>`         | `>>`      | Bitwise: shift x right by y bits, insert 0's           |
| 4                | `<<`         | `<<`      | Bitwise: shift x left by y bits, insert 0's            |
| 3                | `&`          | `&`       | Bitwise: AND                                           |
| 3                | `*`          | `*`       | Multiply and return lower 32 bits (signed)             |
| 3                | `/`          | `/`       | Divide and return quotient (signed)                    |
| 3                | *n.a.*       | `+/`      | Divide and return quotient (unsigned)                  |
| 3                | *n.a.*       | `//`      | Divide and return remainder (signed)                   |
| 3                | `%`          | `+//`     | Divide and return remainder (unsigned)                 |
| -                | `smin()`     | `#>`      | Limit minimum (signed)                                 |
| -                | `smax()`     | `<#`      | Limit maximum (signed)                                 |
| -                | `sar()`      | `SAR`     | Shift x right by y bits, insert MSB's                  |
| -                | `ror()`      | `ROR`     | Rotate x right by y bits                               |
| -                | `rol()`      | `ROL`     | Rotate x left by y bits                                |
| -                | `rev()`      | `REV`     | Reverse order of bits 0..y of x and zero-extend        |
| -                | `zerox()`    | `ZEROX`   | Zero-extend above bit y                                |
| -                | `signx()`    | `SIGNX`   | Sign-extend from bit y                                 |
| -                | `sca()`      | `SCA`     | Unsigned scale, (x * y) >> 32                          |
| -                | `scas()`     | `SCAS`    | Signed scale, (x * y) >> 30                            |
| -                | `frac()`     | `FRAC`    | Unsigned fraction, (x << 32) / y                       |
| -                | `bitrange()` | `ADDBITS` | Make bit field                                         |
| -                | `pinrange()` | `ADDPINS` | Make pin field                                         |
| -                | `fmul()`     | `*.`      | Floating-point multiply                                |
| -                | `fdiv()`     | `/.`      | Floating-point divide                                  |
| -                | `fadd()`     | `+.`      | Floating-point add                                     |
| -                | `fsub()`     | `-.`      | Floating-point subtract                                |
| -                | `flt()`      | `<.`      | Floating-point less than (returns 0 or -1)             |
| -                | `fle()`      | `<=.`     | Floating-point less than or equal (returns 0 or -1)    |
| -                | `==`         | `==.`     | Floating-point equal (returns 0 or -1)                 |
| -                | `!=`         | `<>.`     | Floating-point not equal (returns 0 or -1)             |
| -                | `fgt()`      | `>=.`     | Floating-point greater than or equal (returns 0 or -1) |
| -                | `fge()`      | `>.`      | Floating-point greater than (returns 0 or -1)          |

### Ternary Operators

| Propan  | PASM    | Description                                   |
|---------|---------|-----------------------------------------------|
| `?` `:` | `?` `:` | Ternary: return 2nd or 3rd value based on 1st |
