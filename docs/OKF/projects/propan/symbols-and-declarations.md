---
type: "Reference"
title: "Propan symbols and declarations"
description: "Current semantics for constants, labels, variables, symbol namespaces, and references."
tags: ["propan", "symbols", "constants", "labels", "variables"]
status: "draft"
source_confidence: "high"
---
# Propan symbols and declarations

This page describes the current semantic model for user-defined symbols. Lexical identifier rules are documented in [/projects/propan/lexical-and-source-grammar.md](/projects/propan/lexical-and-source-grammar.md), and address interpretation is documented in [/projects/propan/addresses-and-segments.md](/projects/propan/addresses-and-segments.md).

## Declaration forms

| Form | Symbol kind | Storage/emission |
|---|---|---|
| `name:` | code label | none; records the current address |
| `var name:` | data label | none; records the current address |
| `const name = expression` | constant | none; stores the evaluated semantic value |

A declaration by itself never emits bytes. Data storage is emitted separately with `BYTE`, `WORD`, or `LONG`; see [/projects/propan/directives-and-data.md](/projects/propan/directives-and-data.md).

## One flat user-symbol namespace

Labels, `var` declarations, user constants, and predefined/builtin constants share one case-sensitive symbol table.

Consequences:

- the same spelling cannot be reused for two labels/constants/variables;
- a user declaration cannot replace a predefined constant of the same spelling;
- code labels and data labels are not separate namespaces;
- spelling case matters for symbols.

Functions and instruction/directive mnemonics use separate lookup tables. A symbol name can therefore coincide with a function or mnemonic without being the same entity. Function lookup is case-sensitive; mnemonic lookup is case-insensitive.

## Labels

A code label records the current tagged address and evaluates with the default usage hint `literal`.

```propan
start:
    LONG 0
```

A label emits no data and consumes no address space. Its address is the cursor position at the declaration.

### No special local-label scope is currently implemented

`.` is a legal identifier character, but semantic analysis stores the complete identifier string in the same flat symbol table as every other user symbol. There is currently no separate parent/local-label scope mechanism in the Zig semantic layer.

For example, `.loop:` is a symbol whose name is literally `.loop`; it is not automatically scoped beneath the preceding non-dot label.

## `var` declarations

`var` uses a designator:

```propan
var data:
```

The declaration creates a **data symbol at the current address**. It does not allocate a register, reserve bytes, or emit initialization data.

The practical semantic difference from an ordinary code label is its default usage hint:

- code label → literal/address-oriented use;
- `var` data label → register-oriented use.

To associate emitted storage with a `var` symbol, place a data directive after it:

```propan
var value:
    LONG 0
```

Here `value` denotes the address at which the `LONG` begins; the `LONG`, not `var`, contributes four emitted bytes.

## Constants

A constant declaration has the form:

```propan
const name = expression
```

All constant names are declared during the initial symbol-declaration pass, but their values are evaluated later **in source order**.

Current accepted constant result categories are:

- integer;
- string;
- enumerator;
- register value (currently accepted, with an implementation TODO questioning whether this should remain allowed).

A constant may not retain:

- a tagged address directly; use an explicit address helper such as `hubaddr()` or `cogaddr()` to turn the address into an integer;
- a pointer expression.

For example:

```propan
label:
    LONG 0
const address = hubaddr(label)
```

Label locations have already been assigned by the time user constants are evaluated, so explicit address conversion can use label values.

### Constant dependency order

Because constant values are evaluated in source order, a constant can use a constant that has already been evaluated:

```propan
const a = 10
const b = a + 5
```

A reference to a later constant is different. Its name is known from the declaration pass, so it is not reported as an unknown symbol, but its value is still unset when the earlier constant is evaluated:

```propan
const a = b + 1
const b = 10
```

The current evaluator therefore fails to evaluate `a`. Forward references between user constants are not currently resolved through dependency ordering or repeated evaluation.

Circular constant dependencies likewise have no resolution mechanism.

## Forward label references

Labels are declared before reference validation, and all label locations are assigned before ordinary instruction arguments and constants are finally evaluated. Therefore normal references to labels declared later in the file are supported by the semantic pipeline.

This differs from user constants: label addresses are assigned in a dedicated layout pass, while constant values are evaluated sequentially afterward.

## Undefined references

Semantic analysis recursively checks symbol references before layout/evaluation.

A name that has no declaration or builtin entry produces an `undefined reference to symbol ...` diagnostic. References inside wrapped expressions, unary/binary expressions, function calls, constants, and instruction arguments are all checked.

Undefined function names are checked separately against the function namespace.

## Duplicate declarations

When a label, `var`, or constant is declared with a symbol name that is already defined, semantic analysis emits a duplicate-symbol diagnostic at the later declaration.

Because builtin constants are loaded before user declarations, they also reserve their spellings in the symbol namespace.

## Unused-symbol warnings

The analyzer tracks whether symbols are referenced. User labels/data symbols and user constants can produce a `symbol ... has no references` warning when they remain unused. Predefined/builtin symbols are marked referenced when loaded so the assembler does not warn about the complete builtin set.

## Relationship to instruction/directive names

Instruction and directive names are not declarations in the user-symbol namespace. Their separate mnemonic table currently contains encoded P2 instructions plus the assembler-defined names documented in [/projects/propan/directives-and-data.md](/projects/propan/directives-and-data.md).

Do not infer a symbol collision merely because a user symbol has the same letters as a mnemonic; the semantic namespaces are distinct.
