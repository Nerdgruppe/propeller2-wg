# Propan Check Lists

These checklists can be embedded inside `.propan` files to
add assertions for the Propan test suite.

Each test file that supports tests lists, starts with `//? PROPAN CHECK LIST<LF>`.

After that, each following line that starts with `//?` is part of the
test list. The first line not starting with `//?` terminates the test list block.

Test lists are then parsed as if `//?` is not present.

The features supported are:

## Diagnostic Checks

Assert the exact diagnostics emitted by a test:

```c
//? err: err_unknown_mnemonic
//? err: err_unknown_mnemonic
//? err: warn_symbol_has_no_references
```

```c
err: <code>
```

`<code>` must be a tag of `diagnostics.Kind` at any level: error, warning, or
info. The `err:` keyword is kept for compatibility; it checks diagnostics at
all three levels. Each `err:` line expects one occurrence, so repeated lines
check the count. Order, message details, and source locations do not matter;
missing or unexpected diagnostics fail the test. A matching test exits successfully
without printing the matched diagnostics. Diagnostic checks work in parser,
semantic, and comparison test modes. If compilation stops on expected errors,
symbol, segment, and memory checks are skipped, as is binary comparison. Checklist
check failures can also be matched with `err:`; an expected check failure skips
binary comparison.

## Symbol Checks

Checks for the existence of labels or other symbols.

Example:

```c
//? sym: _start0 code:0:0
//? sym: _end0   code:12:3
//? sym: _start1 code:20:0
```

Rough syntax idea:

```c
sym: <symbol> <spec>
```

- `<symbol>` is the symbol name
- `<spec>` is one of the following:
  - `<type>:<hub>`: Asserts that `<symbol>` is located at the given hub address.
  - `<type>:<hub>:<local>`: Asserts that `<symbol>` is located at the given hub address and has the given `<local>` address.
  - `<type>` is `code`, `data`, `constant`, `builtin`.
  - `<hub>` and `<local>` are either decimal, hexadecimal or `-` for absent/null

## Segment Checks

Checks for the existence of a given segment

Example:

```c
//? seg: 0x2000 100
//? seg: 0x2000 100 cogexec
//? seg: 0x2000 100 lutexec
//? seg: 0x2000 data
```

Syntax idea:

```c
//? seg: <start-address> [<length>] [<type>]
```

- `<start-address>`> is the "identifier" of the segment and encodes the start address.
- `[<length>]` is the optional length of the segment. hexadecimal or decimal.
- `[<type>]` checks the exec_mode of our segment. Can be one of:
  - `cogexec`
  - `lutexec`
  - `hubexec`
  - `regspace`
  - `data`

## Memory Checks

Compare the memory of the generated binary against a memory block.

Example:

```c
//? mem: 0x00000 == u32 [
//?      100,  200,  300,  3, 12,
//?      400,  500,  600,  3, 32,
//?      700,  800,  900,  3, 52,
//?     1000, 1100, 1200, 72, 72
//? ]
```

Rough syntax idea:

```c
mem: <start-address> <compare-mode> <data-block>
```

- `<start-address>` is a decimal or hexadecimal address where the memory starts
- `<compare-mode>` is either
  - `==` for requiring that the memory block is the *whole* memory. full program length is asserted to be equal
  - `<-` for just asserting that the block of memory matches. full program length is not tested
- `<data-block>` is one of the following things:
  - `u8 [ <numbers> ]`, `u16 [ <numbers> ]`, `u32 [ <numbers> ]`
    - `<numbers>` is a list of (comma or whitespace) separated numbers
    - Each number is either a decimal number from `minInt(i32` to `maxInt(u32` which means
      that we can write both positive and negative numbers.
  - `hex [ <bytes> ]` is a sequence of hex bytes.
    - `<bytes>` is a sequence of bytes where each byte is a two-character hex number.
      numbers can be upper/lower case, space or comma separated or joined together.
