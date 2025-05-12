# TODO-List

- `TEST D {WC/WZ/WCZ}`
- `aug()` => `AUGS`, `AUGD`
- `@` operator
- Consider if `const magic = register(13)` is a good idea
- Resolve ambigious instruction encoding
  - `CALLD D,{#}S {WC/WZ/WCZ}`
  - `CALLD PA/PB/PTRA/PTRB,#{\}A`
- `cursed.spin2`
- Implement enum literal parser
- annotate Offset/Label with segment id, so it can be detected if labels are used cross-segment
- Configuration File for analyzer options
- Custom binary format which contains
  - code
  - symbol info
  - debug info
