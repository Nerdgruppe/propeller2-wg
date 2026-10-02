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
- Implement single-operand instruction aliases (rolnib, ..)
- `BYTE "foo"` and friends.
- `.address => @panic("TODO: Implement binary operators on offsets."),`
- `.string => @panic("TODO: Implement binary operators on strings."),`
- Explicit register syntax/predefined registers (`register(X)`)
- Plan to allow access to local labels somehow?
- Regular `.cogexec` must auto-fit into 496, 502 or 506 instead of 512 registers
- Warning for `.hubexec` below `$400` (would be PC inside LUT/cog)

## New Features

### File Inclusion

tl;dr: `#include` is missing.

### Conditional Compilation

Right now, everything is always analyzed. This yields potential issues when having configurable code.

Concept:

We have an evaluation stack, which prevents code evaluation if at least a single "false" is on the stack.

Stack modification can be done with:

```c
.if <cond>    // Evaluates <cond> and pushes <cond> on the stack.
.elif <cond>  // Replaces stack top with <cond>.
.else         // Logically inverts stack top
.endif        // Pops stack top.
```

If any of these are encountered while in a disabled code area, `<cond>` is not evaluated, but only a `false`
is pushed/replaced. This means we keep the "code disabled" feature while also not requiring to
evaluate `<cond>`.

### Array Emission

PASM2 has the ability to emit arrays of values: `LONG 10[15]` will emit 15 times the value `10`.

Propan does not support this right now, but should.

Concept:

```c
LONG 10[15]                 // would work syntactically right now
LONG 15 * [10,20,30]        // might be an alternative solution
                            // would also work with strings:
BYTE 5 * "NaN", " Batman!"  // "NaNNaNNaNNaNNaN Batman!"
```

Potentially a clearer syntax shall be used.

#### Subfeature: Structures

Potentially, we could define something like our own struct type for data emission?

```c
.typedef Vec3 [ x: word, y: word, z: word ]


STRUCT Vec3(x=10, y=20, z=30)
```

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
