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
- local labels
- implement instruction aliases (rolnib, ..)
