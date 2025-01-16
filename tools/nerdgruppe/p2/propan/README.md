# Propan - Alternative Propeller 2 Assembler

Propan is an alternative assembler and assembly syntax that is catered for a more strict and well-defined behaviour.

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
| `0b0010` | `if(!C & Z)`           | `IF_01`, `IF_NC_AND_Z`, `IF_Z_AND_NC`                    | 0 | and | 1  | if C clear and Z set (C = 0 and Z = 1)                                   |
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

| Propan | PASM | Description                             |
|--------|------|-----------------------------------------|
| `!`    | `!!` | Boolean: NOT (0 => TRUE, else => FALSE) |
| `~`    | `!`  | Bitwise: NOT                            |
| `+`    | `+`  | Positive (+X) unary form of Add         |
| `-`    | `-`  | Negate (−X); unary form of Subtract     |
| `@`    | -    | PC-relative offset to label             |
| `*`    | -    | Derference code label                   |
| `&`    | `#`  | Address of data label                   |

### Binary Operators

| Precedence Group | Propan   | PASM   | Description                                  |
|------------------|----------|--------|----------------------------------------------|
| 0                | `and`    | `&&`   | Boolean: AND                                 |
| 0                | `or`     | `\|\|` | Boolean: OR                                  |
| 0                | `xor`    | `^^`   | Boolean: XOR                                 |
| 1                | `==`     | `==`   | Boolean: Is equal                            |
| 1                | `!=`     | `<>`   | Boolean: Is not equal                        |
| 1                | `<=>`    | `<=>`  | Signed comparison (<, =, > returns -1, 0, 1) |
| 1                | `<`      | `<`    | Boolean: Is less than (signed)               |
| 1                |          | `+<`   | Boolean: Is less than (unsigned)             |
| 1                | `>`      | `>`    | Boolean: Is greater than (signed)            |
| 1                |          | `+>`   | Boolean: Is greater than (unsigned)          |
| 1                | `<=`     | `<=`   | Boolean: Is less than or equal (signed)      |
| 1                |          | `+<=`  | Boolean: Is less than or equal (unsigned)    |
| 1                | `>=`     | `>=`   | Boolean: Is greater than or equal (signed)   |
| 1                |          | `+>=`  | Boolean: Is greater than or equal (unsigned) |
| 2                | `+`      | `+`    | Add                                          |
| 2                | `-`      | `-`    | Subtract                                     |
| 2                | `\|`     | `\|`   | Bitwise: OR                                  |
| 2                | `^`      | `^`    | Bitwise: XOR                                 |
| 4                | `>>`     | `>>`   | Bitwise: Shift right                         |
| 4                | `<<`     | `<<`   | Bitwise: Shift left                          |
| 3                | `&`      | `&`    | Bitwise: AND                                 |
| 3                | `*`      | `*`    | Multiply and return lower 32 bits (signed)   |
| 3                | `/`      | `/`    | Divide and return quotient (signed)          |
| 3                |          | `+/`   | Divide and return quotient (unsigned)        |
| 3                | `%`      | `//`   | Divide and return remainder (signed)         |
| 3                |          | `+//`  | Divide and return remainder (unsigned)       |
| -                | `smin()` | `#>`   | Limit minimum (signed)                       |
| -                | `smax()` | `<#`   | Limit maximum (signed)                       |

### Ternary Operators

| Propan  | PASM    | Description                                   |
|---------|---------|-----------------------------------------------|
| `?` `:` | `?` `:` | Ternary: return 2nd or 3rd value based on 1st |
