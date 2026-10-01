# TODO-List

- `TEST D {WC/WZ/WCZ}`
- Consider if `const magic = register(13)` is a good idea
- Resolve ambigious instruction encoding
  - `CALLD D,{#}S {WC/WZ/WCZ}`
  - `CALLD PA/PB/PTRA/PTRB,#{\}A`
  - `tests/propan/equivalence/ambigious.spin2`
- annotate Offset/Label with segment id, so it can be detected if labels are used cross-segment
- Configuration File for analyzer options
- `COGBRK #S` seems to be unsupported in flexspin
- <https://github.com/totalspectrum/spin2cpp/issues/485>
- implement instruction aliases (rolnib, ..)
- function for "ALTI state" and "ALTI config":
  - `(cogaddr(buf_c) << 18) | (cogaddr(buf_b) << 9) | (cogaddr(buf_a) << 0)`
  - `// increment R, D, S with wrap-8,`
    `// substitue each field into next instr`
    `LONG 0b110_110_110_111_111_111`

## New Features

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
