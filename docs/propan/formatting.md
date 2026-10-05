# Propan formatting

Run `propan --pretty-print path/to/file.propan` to format one file to standard
output. Use `propan --pretty-print -` to read from standard input. Formatting
requires valid syntax; semantic validation is a separate operation.

## Columns and spelling

Indentation is measured in spaces from the left edge, starting at zero.
Instruction columns advance to multiples of four:

| Element                              | Indentation |
| ------------------------------------ | ----------- |
| Labels and directives                | 0           |
| Condition codes                      | 4           |
| Mnemonics                            | 16          |
| First operand                        | 24          |
| Parameters of a wrapped operand call | 28          |

Later operands align in shared columns within an assembly block. Each operand
column is sized using preceding operands only from instructions that reach that
column. A final operand never widens the next operand's column. Effects and
trailing comments also align to multiples of four. A column moves to the next
four-space boundary when the preceding text would overlap it. Nonlocal labels
and execution or data mode directives start new assembly blocks; local labels
stay in the surrounding block.

Mnemonics are uppercase: `mOv` becomes `MOV` and `lOnG` becomes `LONG`.
The spelling of dot directives, labels, identifiers, function names, literals,
and comment text is preserved. The keywords `const` and `var` are lowercase.

## Labels

A local label can share a line with the next mnemonic when its complete text,
including `:` and at least one following space, fits before indentation 16.
A nonlocal label can share a line with `LONG`, `WORD`, `BYTE`, `FILE`, or `RES` under
the same limit. The `var ` prefix counts towards this limit for variable labels.

Folding can skip intervening blank lines. A comment after the label prevents
folding, including a comment on the label's own line. Labels before directives
stay on their own lines. A label never shares a line with both a condition code
and a mnemonic.

```propan
loop:
.next:          ADD     PTRA,   1
.conditional:
    if(C)       MOV     PTRB,   PTRA
buffer:         LONG    1,  2,  3
```

## Constants

Consecutive `const` declarations form a constant block. A blank line, standalone
comment, label, or instruction ends that block. The longest identifier determines
the shared `=` column. Padding goes before `=`, with at least one space after each
identifier and exactly one space after `=`. Constant assignment columns are not
rounded to multiples of four.

```propan
const A         = 1
const LONG_NAME = 2
const SUM       = A + LONG_NAME

const B = 3
```

When blank lines separate two constant blocks, the formatter keeps exactly one.
Multiline constant values continue from their value's starting column.

## Expressions and wrapped calls

Binary operators have surrounding spaces. Indexing, prefix and postfix operators,
and named arguments use their normal compact syntax. Necessary parentheses are
preserved or added to keep the expression's meaning. Adjacent unary operators are
separated when concatenating them would form a different token, as in `- -1`.

A function call stays on one line unless it contains a comment or has a trailing
comma. Calls with arguments that wrap put each argument on its own line, with a
trailing comma. Parameters are indented four spaces beyond the operand's starting
column; each nested call adds another four spaces. The closing `)` returns to the
call's surrounding indentation. An empty call's comment stays inside its
parentheses, with `)` on the following line.

```propan
                LONG    ticks(
                            1000,
                            ms=3,
                        )
```

## Comments and blank lines

Consecutive standalone line comments form a comment block. With blank lines on
both sides, the block keeps the first comment's original indentation, rounded up
to a multiple of four. All comments in the block share that indentation.

Other comment blocks follow the next semantic element:

- Before a label, directive, or constant declaration, the block starts at
  indentation 0.
- Before a mnemonic, the block starts at indentation 16.
- Without a following semantic element, the block likewise keeps its original
  indentation rounded up to a multiple of four.

Comments inside expressions remain inside those expressions. Comment text and
source order are preserved.

Runs of blank lines between statements are limited to two, with the exception
of the single blank line between constant blocks. Every file ends with an empty
line; existing one or two trailing empty lines are retained. An empty input is
formatted as one empty line. Structural line endings use LF.
