---
type: "Reference"
title: "Propan lexical and source grammar"
description: "Current lexical rules and top-level source grammar implemented by the Zig Propan frontend."
tags: ["propan", "syntax", "grammar", "lexer", "parser"]
status: "draft"
source_confidence: "high"
---
# Propan lexical and source grammar

## Top-level line forms

The parser currently recognizes these source-line forms:

```text
<blank line>
var <designator>
<label-designator>
const <identifier> = <expression>
if(<condition>) <mnemonic> [<arguments>] [<effect>]
return <mnemonic> [<arguments>] [<effect>]
<mnemonic> [<arguments>] [<effect>]
```

A designator is an identifier immediately followed by `:`. `var` therefore uses forms such as `var data:` while an ordinary code label uses `label:`.

Instruction mnemonics are lexically ordinary identifiers. Semantic analysis later decides whether a mnemonic is a real P2 instruction or a supported assembler directive/pseudo-instruction.

## Whitespace and line breaks

Spaces and horizontal whitespace are ignored between tokens. A linefeed normally terminates a source line.

Instruction argument lists may continue over a single line break immediately after a comma. Function-call argument parsing treats linefeeds as whitespace while inside parentheses, so function calls can be formatted across multiple lines.

Examples:

```propan
ADD dst,
    src

const value = max(
    a=1,
    b=2,
)
```

## Comments

`//` starts a comment that continues to the end of the physical line.

No block-comment syntax is currently defined by the Zig tokenizer.

## Identifiers

Ordinary identifiers use:

```text
first character:  _ . A-Z a-z
later characters: _ . A-Z a-z 0-9
```

A bare `-` is an operator, not an identifier character in the current Zig tokenizer despite an outdated inline token comment suggesting otherwise.

The tokenizer derives these related forms from the identifier character set:

- `name:` → designator;
- `:name` → instruction effect token;
- `#name` → enumerator token.

## Keywords and case sensitivity

The lexical keywords are currently:

```text
const var if return and or xor
```

They are matched as literal words by the tokenizer and are therefore case-sensitive in the current frontend.

Other categories differ:

- P2 instruction/directive mnemonic lookup is case-insensitive.
- instruction effect names are matched case-insensitively.
- condition flags `C` and `Z` are matched case-insensitively.
- user-defined symbol names are stored in the normal symbol table without case folding and should therefore be treated as case-sensitive.
- function-name lookup is not case-folded; builtin/stdlib function spellings should be treated as case-sensitive unless a specific function definition establishes otherwise.

## Integer literals

The tokenizer and parser currently implement:

| Form | Base | Example |
|---|---:|---|
| decimal | 10 | `1234` |
| binary | 2 | `0b1101` |
| quaternary | 4 | `0q0123` |
| octal | 8 | `0o755` |
| hexadecimal | 16 | `0xDEADBEEF` |

Underscores are accepted inside numeric literals and are passed to Zig's integer parser. Literal magnitude is parsed as an unsigned value constrained to the positive range of `u63`; unary minus is a separate expression operator.

### Documentation conflict: octal literals

The root syntax documentation currently discusses decimal, binary, quaternary, and hexadecimal examples but does not document octal literals. The current tokenizer and integer parser explicitly implement the `0o` prefix. Treat octal support as **implemented but externally under-documented**.

## Character and string literals

Character literals use single quotes and string literals use double quotes.

The current unescaper recognizes:

- `\e` — ESC;
- `\r` — carriage return;
- `\n` — line feed;
- `\t` — horizontal tab;
- `\"` — double quote;
- `\'` — single quote;
- `\xHH` — one byte encoded by two hexadecimal digits;
- `\u{...}` — Unicode code point encoded as UTF-8.

Character literals evaluate to the Unicode code point of exactly one character. Empty character literals and character literals containing more than one Unicode code point are diagnosed.

Raw control characters are rejected inside string/character literals.

### Potential implementation issue: unknown escape handling

For an unrecognized escape sequence the current implementation emits a warning but appends the original backslash rather than the escaped character. For example, the recovery path for `\q` appears to preserve `\` while dropping `q`. This behavior should be verified with a regression test before it is intentionally documented as language behavior.

## Enumerators

`#name` is tokenized as an enumerator expression. Enumerator meaning is resolved later by semantic analysis and standard-library/instruction operand handling. This page does not yet define the complete enumerator namespace or legal use sites.

## Expressions: parser-level precedence

The current parser groups binary operators from lowest to highest precedence as:

1. `and`, `or`, `xor`
2. `==`, `!=`, `<=>`, `<`, `>`, `<=`, `>=`
3. `+`, `-`, `|`, `^`
4. `&`, `*`, `/`, `%`
5. `<<`, `>>`
6. unary operators / value expressions

Binary operators within each group are parsed left-associatively.

Unary operators currently parsed are:

```text
- + ~ ! @ * & ++ --
```

Postfix `++`/`--` and one `[...]` indexing operation are accepted on identifier expressions.

## Function calls

Function calls use:

```propan
name()
name(value)
name(a=1, b=2)
```

Named arguments are syntactically supported. A trailing comma is accepted. Function-call contents may span lines.

Semantic validity, parameter names, and argument types depend on the resolved builtin/stdlib function.

## Known syntax-documentation conflict: ternary operator

The root README currently lists `? :` as a Propan ternary operator. The Zig tokenizer has `?` and `:` tokens, but the current expression parser has no ternary-expression production. Consequently this is **not currently established as supported Zig-frontend syntax** and should be treated as an external-documentation conflict until implementation or tests demonstrate otherwise.

## Still to document

This page intentionally does not yet claim completeness for:

- semantic expression types and conversions;
- complete condition grammar;
- address operators and segment-dependent label semantics;
- directive semantics;
- stdlib function signatures;
- instruction operand selection.

Those areas are tracked in `/TODO.md`.
