# TODO-List

## Random Task Collection

- Configuration File for analyzer options
- `COGBRK #S` seems to be unsupported in flexspin
- <https://github.com/totalspectrum/spin2cpp/issues/485>
- `.address => @panic("TODO: Implement binary operators on offsets."),`
- `.string => @panic("TODO: Implement binary operators on strings."),`
- Plan to allow access to local labels somehow?
- Regular `.cogexec` must auto-fit into 496, 502 or 506 instead of 512 registers
- Warning for `.hubexec` below `$400` (would be PC inside LUT/cog)
- Enable "-Dx=y" on the CLI
- Implement warning/error for `EncodedInstruction.Flags.wcz_not_used`
- Fully define the compatibility matrix for implicit jumping/referencing between segments.

## Priority Fixes / Tasks

- `TEST D {WC/WZ/WCZ}`
  - Add the one operand alias for TEST D,D, including its flag effects.
    The instruction table specifies this encoding. FlexSpin emitted identical
    words for both spellings; Propan currently rejects the one operand form because
    its TEST definition (line 1348) requires two operands.
- Resolve ambigious instruction encoding
  - `CALLD D,{#}S {WC/WZ/WCZ}`
  - `CALLD PA/PB/PTRA/PTRB,#{\}A`
  - `tests/propan/equivalence/ambigious.spin2`
  - These instructions can have two different selections and are ambigious.
    provide configuration for this.
- Implement single-operand instruction aliases (rolnib, ..)
- Explicit register syntax/predefined registers (`register(X)`)
  - Consider if `const magic = register(13)` is a good idea


## New Features

### File Inclusion

tl;dr: `#include` is missing.

Implement:
`.import "<filepath>"` should behave as if the file was pasted at this location.
This must invoke a "sub-parser". This is best implemented *before* semantic analysis, and
just paste the AST nodes of `<filepath>` into the AST of the enclosing file.

Files can be included multiple times unless they are declared `.import once`
(not with a string but a identifier/word).

Drop multi-file-support on the CLI (just plain reject it)

### Groups

Groups are similar to ELF sections with garbage collection.

A group is only emitted into the binary if a symbol from the group is referenced from the outside. Groups can transitively
depend on other groups and form a graph.

The default code is not in a group.

- `.group`: Starts a new group.
- `.endgroup`: Ends the current group and swaps back to "always emitted" context.

### Relocations

Required for the Ashet Home Computer:

Allows creation of metadata that is attached to a memory location. This metadata can encode "relocations" which
can later be applied to dynamically link the resulting binary to another location, or use other pins.

```propan

.relocation #base     pin
.relocation #offset   hub

RDPIN dst, reloc(#base, 10)     // emits the info that this field must be patched by the "base"

RDLONG dst, reloc(#offset, 256) // reads from #offset+256
```

- `.relocation <tag> <type>`: Declares a new relocation key named `<tag>` of the given `<type>` hint.
  - `<tag>` is an enumerator which uniquely identifies the relocation
  - `<type>` is one of:
    - `bit`: The relocation is assumed to be a pin number (0…31).
    - `pin`: The relocation is assumed to be a pin number (0…63).
    - `hub`: The relocation is assumed to be a hub address (0…0x7FFFF).
    - `lut`: The relocation is assumed to be a lut address (0…511).
    - `cog`: The relocation is assumed to be a cog address/register (0…511).
    - `int`: The relocation is assumed to be any kind of 32 bit integer.
- `reloc(<tag>, <value>)`: Attaches relocation metadata to `<value>`, such that `<value>` can later be patched by the
  relocation identified by `<tag>`.

## Under Consideration

### SPIN DEBUG

Currently, Propan does not support the "debug()" syntax from SPIN2


### Structures

Potentially, we could define something like our own struct type for data emission?

```c
.typedef Vec3 [ x: word, y: word, z: word ]


STRUCT Vec3(x=10, y=20, z=30)
```