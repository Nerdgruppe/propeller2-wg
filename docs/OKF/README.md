# Open Knowledge Format documentation

`docs/OKF/` contains curated, retrieval-oriented project knowledge. It is intended to be useful both to human contributors and to automated coding/documentation agents.

The bundle is organized by engineering concept rather than by source-document layout. Source code, tests, existing documentation, generated data, and external technical references may all contribute evidence, but processed OKF pages should capture the resulting knowledge in a focused form.

## Layout conventions

- `index.md` is the navigation entry point for a scope.
- `log.md` records meaningful documentation-maintenance changes for that scope.
- Topic directories contain focused documents and may have their own `index.md` and `log.md`.
- `references/` records source inventories, provenance, coverage notes, and other material used to support processed documentation.
- Links beginning with `/` are relative to the `docs/OKF/` root.
- Do not add manually maintained timestamps to OKF front matter; use Git history for creation and modification chronology.

The initial structure intentionally stays small. Add new top-level topics when durable documentation needs them rather than creating speculative empty hierarchies.

## Evidence and status

Documentation should make important provenance distinctions explicit. In particular, do not blur together:

- behavior defined by the current implementation;
- behavior demonstrated by tests;
- behavior stated by external or official documentation;
- conclusions derived from those sources;
- open questions, known discrepancies, and unverified assumptions.

When sources conflict, preserve the discrepancy until the project deliberately resolves or normalizes it.

## Current scope

The first documentation-rework target is Propan. See [/projects/propan/index.md](/projects/propan/index.md) and the temporary [/TODO.md](/TODO.md).
