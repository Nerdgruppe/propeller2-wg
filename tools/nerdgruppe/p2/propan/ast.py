from abc import ABC 
from dataclasses import dataclass, field
from enum import IntEnum, StrEnum

class Effect(StrEnum):
    and_c = ":and_c"
    and_z = ":and_z"
    or_c = ":or_c"
    or_z = ":or_z"
    xor_c = ":xor_c"
    xor_z = ":xor_z"
    wc = ":wc"
    wcz = ":wcz"
    wz = ":wz"

class ConditionStyle(StrEnum):
    comparison = "operation"
    boolean = "boolean"
    _return = "return"
    
class ConditionOp(StrEnum):
    bool_and = "and"
    bool_or = "or"
    equals = "equals"
    differs = "differs"

class NumberFormat(IntEnum):
    binary = 2
    quaternary = 4
    decimal = 10
    hexadecimal = 16

class UnaryOperator(StrEnum):
    bitwise_invert = "~"
    logical_invert = "!"
    plus = "+"
    negate = "-"

class Identifier(str):
    pass

class Expression(ABC):
    pass

@dataclass(kw_only=True, frozen=True)
class Argument:
    value: Expression
    name: Identifier | None

@dataclass(kw_only=True, frozen=True)
class UnaryExpression(Expression):
    operator: UnaryOperator
    value: Expression


@dataclass(kw_only=True, frozen=True)
class NumericExpression(Expression):
    format: NumberFormat
    value: int

@dataclass(kw_only=True, frozen=True)
class FunctionCallExpression(Expression):
    function: Identifier
    arguments: list[Argument]


@dataclass(kw_only=True, frozen=True)
class RelativeAddressExpression(Expression):
    target: Identifier
    
@dataclass(kw_only=True, frozen=True)
class AddressOfExpression(Expression):
    target: Identifier
    
@dataclass(kw_only=True, frozen=True)
class ValueOfExpression(Expression):
    target: Identifier

@dataclass(kw_only=True, frozen=True)
class ArrayExpression(Expression):
    value: Expression
    count: Expression

@dataclass(kw_only=True, frozen=True)
class Condition:
    style: ConditionStyle
    op: ConditionOp
    c_state: bool | None
    z_state: bool | None

@dataclass(kw_only=True, frozen=True)
class Constant:
    identifier: Identifier
    value: Expression

@dataclass(kw_only=True, frozen=True)
class Label:
    identifier: Identifier
    is_variable: bool


@dataclass(kw_only=True, frozen=True)
class Instruction:
    label: Label | None
    condition: Condition | None 
    mnemonic: Identifier
    arguments: list[Argument]
    effect: Effect | None

@dataclass(kw_only=True, frozen=True)
class Program:

    lines: list[Instruction | Constant | Label]