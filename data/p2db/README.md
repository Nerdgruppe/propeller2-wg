# P2DB - A Propeller 2 instruction database

The goal of the P2DB project is to create a single source of truth for knowledge about the Propeller 2 instruction set.

This database shall be usable for:

- Documentation Generation
- Assemblers
- Disassemblers
- Compilers
- Simulators
- Static Code Analyzers
- ...

This means that the data needs to be modelled in a formal, reusable way that can be consumed by many different applications and languages.

## Schema

The schema (`p2db.cue`) for the database is written in [CUE](https://cuelang.org/), a language for precise data schema descriptions including validators.

`p2db.cue` contains both a formal description of the database schema, but also a lot of comments that add additional knowledge and intent to the actual data schema.

## Database

The database is authored as YAML files in `db/*.yaml` files and these files are meant to be fused together into a single JSON database.

This is done with `cue export --out json p2db.cue db/*.yaml`.
