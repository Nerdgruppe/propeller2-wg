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
- wordoffset must error/warn on byteoffset() == 1/3

## Priority Fixes / Tasks

- Change `.reserve` to `RES` (makes code more uniform and allows better code formatting)

## New Features

### `propan fmt`

Implement auto-formatting for propan code.

This needs further specification.

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

### PASM2/SPIN2 emission

Implement `propan --format=spin2` which emits a text file that is compatible with
the SPIN ecosystem.

This file should not contain just a long `BYTE` block, but preferrably textual
instructions, data blocks, and labels.

For this, we need extensive flexspin-oracle testing.
