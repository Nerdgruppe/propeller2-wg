import sys

from pathlib import Path
from dataclasses import replace

from lark import Lark, Transformer, v_args, GrammarError, Token
from lark.exceptions import UnexpectedInput, UnexpectedCharacters
from lark.visitors import Discard

from .grammar import PROPAN_GRAMMAR
from .ast import (
    Program,
    Expression,
    NumericExpression,
    NumberFormat,
    UnaryOperator,
    UnaryExpression,
    BinaryOperator,
    BinaryExpression,
    FunctionCallExpression,
    Constant,
    Identifier,
    Line,
    EmptyLine,
    Label,
    Comment,
    Instruction,
    Effect,
    Condition,
    ConditionStyle,
    ConditionOp,
    Argument,
    ArgumentList,
    RelativeAddressExpression,
    AddressOfExpression,
    ValueOfExpression,
    ArrayExpression,
    SymbolicExpression,
    WrappingExpression,
    StringExpression,
)


class _SentinelType:
    def __init__(self, tag: str):
        self.tag = tag

    def __repr__(self):
        return f"<{self.tag}>"


COMMA = _SentinelType("COMMA")
EOL = _SentinelType("EOL")


def _debug_print(ctx, args):
    print("debug(", repr(ctx), len(args), ")")
    for index, arg in enumerate(args):
        print(f"  [{index}]:{type(arg)} = {arg!r}")
    print(flush=True)


def _debug(fun):
    def _wrap(self, *args):
        _debug_print(fun.__name__, args)
        return fun(self, *args)

    return _wrap


OP_CONDITIONS = {
    #     (C, Z, Op)
    ">=": (0, None, None),
    "<=": (1, 1, "or"),
    "==": (None, 1, None),
    "!=": (None, 0, None),
    "<": (1, None, None),
    ">": (0, 0, "and"),
}


@v_args(inline=True)  # Affects the signatures of the methods
class PropanTransformer(Transformer):
    #
    # Constructs
    #

    def program(self, *sequence) -> Program:
        return Program(
            lines=[part for part in sequence if not isinstance(part, Token)],
        )

    def line(self, data: Line, comment: Comment | None, eol) -> Line:
        assert data.comment is None
        if comment is not None:
            return replace(data, comment=comment)
        return data

    def empty_line(self, comment: Comment | None, eol) -> Line:
        return EmptyLine(comment=comment)

    def comment(self, text: Token) -> Comment:
        return Comment(text=text)

    def const_decl(self, identifier: Identifier, value: Expression) -> Constant:
        return Constant(
            identifier=identifier,
            value=value,
        )

    def label(self, identifier: Identifier) -> Label:
        return Label(
            identifier=identifier,
            is_variable=False,
        )

    def var_label(self, identifier: Identifier) -> Label:
        return Label(
            identifier=identifier,
            is_variable=True,
        )

    def instruction(
        self,
        label: Label | None,
        condition: Condition | None,
        mnemonic: Identifier,
        arguments: list[Argument],
        effect: Effect | None,
    ) -> Instruction:
        return Instruction(
            label=label,
            condition=condition,
            mnemonic=mnemonic,
            arguments=arguments,
            effect=effect,
        )

    def ident(self, token: Token) -> Identifier:
        return Identifier(token)

    def arglist(self, *args) -> ArgumentList:
        assert all(arg is EOL or arg is COMMA or isinstance(arg, Argument) for arg in args)

        # multiline = any(arg is EOL for arg in args)
        multiline = (args[-1] == COMMA) or any(arg.comment is not None for arg in args if isinstance(arg, Argument))

        return ArgumentList(multiline=multiline, items=tuple(arg for arg in args if isinstance(arg, Argument)))

    def commented_arg(self, arg: Argument, comment: Comment | None) -> Argument:
        assert arg.comment is None
        if comment is not None:
            return replace(arg, comment=comment)
        return arg

    def positional_arg(self, value: Expression) -> Argument:
        return Argument(
            name=None,
            value=value,
        )

    def named_arg(self, name: Identifier, value: Expression) -> Argument:
        return Argument(
            name=name,
            value=value,
        )

    #
    # Atomic Expressions
    #

    def wrapping_expr(self, expr: Expression) -> WrappingExpression:
        return WrappingExpression(
            value=expr,
        )

    def function_call(self, name: Identifier, args: ArgumentList, _eol: _SentinelType = EOL) -> FunctionCallExpression:
        assert _eol is EOL
        return FunctionCallExpression(
            function=name,
            arguments=args or tuple(),
        )

    def relative_op(self, identifier: Identifier) -> RelativeAddressExpression:
        return RelativeAddressExpression(target=identifier)

    def deref_op(self, identifier: Identifier) -> ValueOfExpression:
        return ValueOfExpression(target=identifier)

    def addrof_op(self, identifier: Identifier) -> AddressOfExpression:
        return AddressOfExpression(target=identifier)

    def array_ctor(self, value: Expression, count: Expression) -> ArrayExpression:
        return ArrayExpression(
            value=value,
            count=count,
        )

    def symbol_ref(self, identifier: Identifier) -> SymbolicExpression:
        return SymbolicExpression(name=identifier)

    def string_literal(self, string: str) -> StringExpression:
        return StringExpression(
            value=None,  # TODO(fqu): Parse string!
            written=string,
        )

    #
    # Binary Operators
    #

    def binary_op(self, lhs: Expression, op_token: Token, rhs: Expression) -> BinaryExpression:
        op = BinaryOperator(op_token)
        return BinaryExpression(
            lhs=lhs,
            rhs=rhs,
            operator=op,
        )

    #
    # Unary Operators
    #

    def unary_minus(self, value: Expression) -> UnaryExpression:
        return UnaryExpression(
            operator=UnaryOperator.negate,
            value=value,
        )

    def unary_plus(self, value: Expression) -> UnaryExpression:
        return UnaryExpression(
            operator=UnaryOperator.plus,
            value=value,
        )

    def unary_not(self, value: Expression) -> UnaryExpression:
        return UnaryExpression(
            operator=UnaryOperator.bitwise_invert,
            value=value,
        )

    def unary_inverse(self, value: Expression) -> UnaryExpression:
        return UnaryExpression(
            operator=UnaryOperator.logical_invert,
            value=value,
        )

    #
    # Number Literals
    #

    def character_literal(self, text: str) -> NumericExpression:
        char_text = text[1:-1]

        assert len(char_text) == 1, repr(char_text)

        return NumericExpression(
            format=NumberFormat.character,
            value=ord(char_text),
            written=text,
        )

    def bin_number(self, value: str) -> NumericExpression:
        return NumericExpression(
            format=NumberFormat.binary,
            value=int(value, 2),
            written=value,
        )

    def quad_number(self, value: str) -> NumericExpression:
        return NumericExpression(
            format=NumberFormat.quaternary,
            value=int(value, 4),
            written=value,
        )

    def dec_number(self, value: str) -> NumericExpression:
        return NumericExpression(
            format=NumberFormat.decimal,
            value=int(value, 10),
            written=value,
        )

    def hex_number(self, value: str) -> NumericExpression:
        return NumericExpression(
            format=NumberFormat.hexadecimal,
            value=int(value, 16),
            written=value,
        )

    #
    # Conditions
    #

    def condition(self, cond: Condition) -> Condition:
        return cond

    def return_condition(self) -> Condition:
        return Condition(
            style=ConditionStyle._return,
            c_state=None,
            z_state=None,
        )

    def compare_condition(self, flag: Token) -> Condition:
        c, z, op = OP_CONDITIONS[flag]
        return Condition(
            style=ConditionStyle.comparison,
            op=_map_none(ConditionOp, op),
            c_state=c,
            z_state=z,
        )

    def _op_condition(self, lhs: Condition, op: ConditionOp, rhs: Condition):
        assert lhs.style == ConditionStyle.boolean
        assert lhs.op is None

        assert rhs.style == ConditionStyle.boolean
        assert rhs.op is None

        assert (lhs.c_state is None) != (rhs.c_state is None)
        assert (lhs.z_state is None) != (rhs.z_state is None)

        return Condition(
            style=ConditionStyle.boolean,
            op=op,
            c_state=lhs.c_state if lhs.c_state is not None else rhs.c_state,
            z_state=lhs.z_state if lhs.z_state is not None else rhs.z_state,
        )

    def op_condition_or(self, lhs: Condition, rhs: Condition) -> Condition:
        return self._op_condition(lhs, ConditionOp.bool_or, rhs)

    def op_condition_and(self, lhs: Condition, rhs: Condition) -> Condition:
        return self._op_condition(lhs, ConditionOp.bool_and, rhs)

    def op_condition_eq(self, lhs: Condition, rhs: Condition) -> Condition:
        return self._op_condition(lhs, ConditionOp.equals, rhs)

    def op_condition_neq(self, lhs: Condition, rhs: Condition) -> Condition:
        return self._op_condition(lhs, ConditionOp.differs, rhs)

    def true_flag(self, flag: Token) -> Condition:
        if flag.lower() == "c":
            return Condition(style=ConditionStyle.boolean, op=None, c_state=True, z_state=None)
        else:
            return Condition(style=ConditionStyle.boolean, op=None, c_state=None, z_state=True)

    def inv_flag(self, flag: Token) -> Condition:
        if flag.lower() == "c":
            return Condition(style=ConditionStyle.boolean, op=None, c_state=False, z_state=None)
        else:
            return Condition(style=ConditionStyle.boolean, op=None, c_state=None, z_state=False)

    #
    # Effect Literals
    #

    def effect_andc(self, _: Token) -> Effect:
        return Effect.and_c

    def effect_andz(self, _: Token) -> Effect:
        return Effect.and_z

    def effect_orc(self, _: Token) -> Effect:
        return Effect.or_c

    def effect_orz(self, _: Token) -> Effect:
        return Effect.or_z

    def effect_xorc(self, _: Token) -> Effect:
        return Effect.xor_c

    def effect_xorz(self, _: Token) -> Effect:
        return Effect.xor_z

    def effect_wc(self, _: Token) -> Effect:
        return Effect.wc

    def effect_wcz(self, _: Token) -> Effect:
        return Effect.wcz

    def effect_wz(self, _: Token) -> Effect:
        return Effect.wz

    #
    # Utilities
    #

    def eol(self, _: Token):
        return EOL

    def comma(self):
        return COMMA

    def discard(self, _: Token):
        return Discard

    def __default__(self, data, children, meta):
        print("unhandled subtree of type %r: %r" % (str(data), children))
        return super().__default__(data, children, meta)


def _map_none(callable, value):
    if value is None:
        return None
    return callable(value)


def parse_file(path: str | Path) -> Program:
    parser = Lark(PROPAN_GRAMMAR)

    transformer = PropanTransformer()

    try:
        source = Path(path).read_text()
        parse_tree = parser.parse(source)
    except UnexpectedInput as err:
        sys.stderr.write(f"{err}\n")
        sys.exit(1)

    program = transformer.transform(parse_tree)

    return program
