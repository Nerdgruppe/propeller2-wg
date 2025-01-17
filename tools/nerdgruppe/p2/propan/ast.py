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
    character = 0
    binary = 2
    quaternary = 4
    decimal = 10
    hexadecimal = 16


class UnaryOperator(StrEnum):
    bitwise_invert = "~"
    logical_invert = "!"
    plus = "+"
    negate = "-"


class BinaryOperator(StrEnum):
    logical_and = "and"
    logical_or = "or"
    logical_xor = "xor"

    inequal = "!="
    less_than = "<"
    less_or_equal = "<="
    compare = "<=>"
    equals = "=="
    greater_than = ">"
    greater_or_equal = ">="

    subtract = "-"
    add = "+"

    multiply = "*"
    divide = "/"
    modulus = "%"

    bitwise_or = "|"
    bitwise_and = "&"
    bitwise_xor = "^"
    shift_left = "<<"
    shift_right = ">>"


class Identifier(str):
    pass


class Expression(ABC):
    pass


@dataclass(kw_only=True, frozen=True)
class Comment:
    text: str


@dataclass(kw_only=True, frozen=True)
class Argument:
    value: Expression
    name: Identifier | None
    comment: Comment | None = field(default=None)


@dataclass(kw_only=True, frozen=True)
class ArgumentList:
    multiline: bool
    items: tuple[Argument]

    def __len__(self) -> int:
        return len(self.items)

    def __iter__(self):
        return iter(self.items)


@dataclass(kw_only=True, frozen=True)
class WrappingExpression(Expression):
    value: Expression


@dataclass(kw_only=True, frozen=True)
class UnaryExpression(Expression):
    operator: UnaryOperator
    value: Expression


@dataclass(kw_only=True, frozen=True)
class BinaryExpression(Expression):
    operator: BinaryOperator
    lhs: Expression
    rhs: Expression


@dataclass(kw_only=True, frozen=True)
class NumericExpression(Expression):
    format: NumberFormat
    value: int
    written: str | None


@dataclass(kw_only=True, frozen=True)
class StringExpression(Expression):
    value: bytes
    written: str | None


@dataclass(kw_only=True, frozen=True)
class FunctionCallExpression(Expression):
    function: Identifier
    arguments: ArgumentList


@dataclass(kw_only=True, frozen=True)
class SymbolicExpression(Expression):
    name: Identifier


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
class Line:
    comment: Comment | None = field(default=None)


@dataclass(kw_only=True, frozen=True)
class Constant(Line):
    identifier: Identifier
    value: Expression


@dataclass(kw_only=True, frozen=True)
class Label(Line):
    identifier: Identifier
    is_variable: bool


@dataclass(kw_only=True, frozen=True)
class Instruction(Line):
    label: Label | None
    condition: Condition | None
    mnemonic: Identifier
    arguments: ArgumentList
    effect: Effect | None


@dataclass(kw_only=True, frozen=True)
class Program:
    lines: tuple[Line]
