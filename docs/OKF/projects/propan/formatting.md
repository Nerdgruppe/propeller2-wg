---
type: "Reference"
title: "Propan source formatting"
description: "Current pretty-printer behavior, layout rules, syntax boundaries and formatter test coverage."
tags: ["propan", "formatter", "pretty-print", "syntax", "tooling"]
status: "draft"
source_confidence: "high"
---
# Propan source formatting

This reference describes the formatter on `rework-documentation` following the 2026-10-15 upstream merge. It is grounded in `src/propan/frontend/render.zig`, the `--pretty-print` path in `src/propan/propan.zig`, `src/propan/frontend/render_tests.zig`, the formatting fixtures under `tests/propan/format/`, and `src/propan/test_suite.zig`. The pre-existing `docs/propan/formatting.md` provides additional explanatory context; implementation and tests take precedence if they disagree.

## CLI behavior and scope

```sh
propan --pretty-print program.propan
propan --pretty-print -
```

`--pretty-print <path>` reads **one** source file and writes formatted source to standard output; `-` reads standard input. It is an alternative CLI path, not an output format (`--format spin2` is a separate feature). The tool invokes the frontend parser and AST renderer, then returns **before** normal import expansion, symbol resolution, assembler semantics or binary emission. Therefore:

- Source must be syntactically parseable; a parser failure makes formatting fail.
- A syntactically valid but semantically unsupported mnemonic may still be formatted. Formatting success is **not** assembly validation.
- `.import` lines are formatted as source lines; their referenced files are **not expanded** by this CLI path.
- Supplying additional positional root files in pretty-print mode is rejected.

The command emits text to standard output. It does not rewrite the input file in place. The CLI doesn't enforce a `.propan` extension.

## Columns and case

Indentation is measured in spaces, counting from the left edge. The formatter uses a four-column grid for assembly blocks:

| Item | Nominal column |
|---|---:|
| Labels and mode/directive lines | 0 |
| Instruction condition | 4 |
| Mnemonic | 16 |
| First operand | 24 |
| Wrapped operand-call arguments | 28 |

Later operand columns, effect suffixes and trailing comments are aligned within a formatting block. Columns can move to a later four-space boundary when earlier text would overlap them, so the numbers above are **defaults**, not inflexible positions for every long label or operand.

Instruction and data mnemonic spellings become uppercase (`mOv` → `MOV`, `lOnG` → `LONG`). `const`/`var` remain lowercase. The source spellings of symbols, labels, function names, literals, dot directive names and comment text are retained by the formatter.

A new non-dot label or execution/data mode directive starts a new assembly layout block. Leading-dot local labels remain in the containing block. See [/projects/propan/symbols-and-declarations.md](/projects/propan/symbols-and-declarations.md) for the corresponding semantic scoping rule; formatter grouping itself is a presentation choice, not an additional namespace.

## Label folding and constant alignment

Short leading-dot labels can share a line with a following unconditioned instruction, provided they fit before the mnemonic column. Short ordinary labels can share a line with `BYTE`, `WORD`, `LONG`, `FILE` or `RES`, subject to the same available-column test. `var` counts toward the width. Labels before directives or conditional instructions remain separate; intervening comments prevent folding.

```propan
loop:
.next:          ADD     PTRA,   1
.conditional:
    if(C)       MOV     PTRB,   PTRA
buffer:         LONG    1,  2,  3
```

Adjacent `const` assignments form an aligned group. Their `=` column is determined by the longest identifier in the group (rather than rounded to four-space stops). Blank lines, independent comments, labels and instructions end a constant group. A blank separation between groups is retained as a single blank line.

```propan
const A         = 1
const LONG_NAME = 2
const SUM       = A + LONG_NAME

const B = 3
```

## Expressions, calls and comments

Binary operators have spaces around them; unary operators are separated when writing them together would change tokenization (for example, `- -1`). The expression printer preserves or introduces necessary parentheses to retain binary precedence and associativity. Indexed pointer syntax and named arguments use compact forms.

Calls stay on one line when practical. A call containing comments or a trailing comma can be expanded to one argument per line, with a trailing comma and consistent indentation. Nested calls increase indentation; a comment in an empty call stays inside the parentheses.

```propan
                LONG    ticks(
                            1000,
                            ms=3,
                        )
```

Standalone comment blocks are preserved. A block directly preceding a non-dot label or `const` declaration is aligned at column zero; before instructions it normally aligns to the mnemonic column unless the surrounding label structure requires otherwise. Other standalone comment blocks use their first comment's original indentation rounded upward to a multiple of four. Inline and expression comments remain associated with their original source expressions.

The formatter normalizes runs of blank lines between statements (typically at most two), preserves a deliberate separation between constant groups and emits LF line endings. It ensures an empty final line, including for an empty or previously unterminated input. `//?` test-checklist blocks are kept separate from subsequent declarations.

## Verification evidence

The following are **existing test contracts inspected in source**, not tests executed as part of this documentation change:

- `src/propan/frontend/render_tests.zig` tests expression formatting with parentheses removed from the AST, source/AST rendering, comment handling, newline behavior and parse → render → reparse → semantic byte equivalence.
- `src/propan/test_suite.zig` tests golden-format fixtures and **idempotence** by formatting the resulting output a second time. It also includes malformed-input rejection and a formatter-vs-original assembly comparison over semantic fixtures.
- `tests/propan/format/input.propan` / `expected.propan`, `layout-input.propan` / `layout-expected.propan`, and `constants.propan` / `constants-expected.propan` contain concrete expected layouts.

These tests provide more than cosmetic snapshots: several compare resulting module segment bytes/offsets after reformatting. They are not a guarantee that every conceivable source or external tool round-trips, and no newly executed test result is claimed here.

The related command-line contract is documented in [/projects/propan/tooling.md](/projects/propan/tooling.md).
