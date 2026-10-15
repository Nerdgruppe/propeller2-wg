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

## Global user symbols and scoped local labels

Ordinary labels, `var` declarations, user constants, and predefined/builtin constants share a case-sensitive global symbol table. Leading-dot local label declarations and references are additionally qualified by lexical scope; their internal keys cannot collide with identically spelled global names.

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

### Leading-dot local labels

**Implemented and tested:** a leading-dot declaration such as `.loop:` is local to the most recent non-dot label (`foo:`, `var storage:`, etc.). Another non-dot declaration opens a new scope. Mode directives (`.cogexec`, `.lutexec`, `.hubexec`, `.regspace`, `.data`) end the previous local scope. Local labels at the beginning of a source scope also have their own implicit scope. References to `.loop` bind in the active scope, and a later `bar:` may declare a different `.loop:`.

`foo.loop:`, by contrast, does **not** begin with a dot: it is a literal global identifier, distinct from `.loop:` under `foo:`.

```propan
foo:
    LONG .loop
.loop:
    LONG 0

bar:
    LONG .loop
.loop:
    LONG 0

foo.loop:
    LONG 0
```

The analyzer internally namespaces local labels by scope and emits qualified display names such as `foo:loop` and `bar:loop` in JSON/listing metadata. Imports are expanded before local scopes are rebound, so the effective lexical sequence can span source files.

**Evidence:** `src/propan/frontend/parser.zig` (scope changes), `src/propan/sema.zig` (`get_label_info` and import rebind), `tests/propan/sema/local-labels.propan` and `src/propan/test_suite.zig` local-label/listing tests.

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

All constant names are declared before ordinary evaluation. Constant values are then **resolved recursively on demand** with cached results and explicit cycle detection, including dependencies on constants declared later in the source.

Current accepted constant result categories are:

- integer;
- string;
- integer sequence;
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

### Constant dependencies and cycles

Both earlier and later constant definitions can be referenced:

```propan
const a = b + 1
const b = 10
```

**Current result:** `a` resolves to `11`. The evaluator walks dependencies recursively (`resolve_constant`), records evaluation state and diagnoses cyclic dependencies instead of relying on source-order availability. The current implementation also permits string/sequence constants.

There is an important **layout-phase qualification**: directives such as `.align` and `.org` need values while locations are being assigned. Constants whose evaluation depends on label addresses not yet assigned can fail during that phase, even though ordinary constant evaluation succeeds after layout. Do not treat dependency resolution as permission to use arbitrary label-dependent expressions in layout directives.

**Evidence:** `src/propan/sema.zig` (`resolve_constant`, `evaluate_constant_values`, `layout_integer`); `tests/propan/sema/diagnostics/constant-cycle.propan`.


## Forward label references

Labels are declared before reference validation, and all label locations are assigned before ordinary instruction arguments and constants are finally evaluated. Therefore normal references to labels declared later in the file are supported by the semantic pipeline.

Label addresses are assigned in a dedicated layout pass. Constant dependencies can be resolved recursively; expressions that require not-yet-known label addresses during layout remain restricted.

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
