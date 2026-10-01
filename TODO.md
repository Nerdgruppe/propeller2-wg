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