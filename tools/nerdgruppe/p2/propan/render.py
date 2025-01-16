import io

from dataclasses import dataclass, field
from typing import Callable

from .ast import (
    Program,
    Expression,
    NumericExpression,
    NumberFormat,
    UnaryOperator,
    UnaryExpression,
    FunctionCallExpression,
    Constant,
    Identifier,
    Label,
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
)

@dataclass
class _ConstantGroup:
    items: list[Constant] = field(default_factory=list)
    width: int = field(default=0)

def render(program: Program, *, file: io.IOBase):

    mnemonic_width = 0
    condition_width = 0

    constant_groups: dict[Constant, _ConstantGroup] = dict()

    last_line_const = None
    for line in program.lines:
        if isinstance(line, Instruction):
            mnemonic_width = max(mnemonic_width, len(line.mnemonic))

            if line.condition is not None:
                buffer = io.StringIO()
                render_condition(line.condition, file=buffer)
                condition_width = max(condition_width, len(buffer.getvalue()))
        
        if isinstance(line, Constant):

            group = constant_groups.get(last_line_const, _ConstantGroup())
            assert group is not None

            group.width = max(group.width, len(line.identifier))

            constant_groups[line] = group

            last_line_const = line
        else:
            last_line_const = None

    for line in program.lines:

        constant_width = None
        try:
            group = constant_groups.get(line, None)
            if group is not None:
                constant_width = group.width
        except:
            pass 

        render_line(
            line, 
            file=file,
            condition_width=condition_width,
            mnemonic_width=mnemonic_width,
            constant_width=constant_width,
        )

def render_line(
    line: Instruction | Constant | Label | None, 
    *, 
    file: io.IOBase, 
    mnemonic_width: int | None = None,
    condition_width: int | None = None,
    constant_width: int | None = None,
):
    if line is None:
        file.write("\n")
        return

    indent = "  "
    if condition_width is not None:
        indent = " " * (condition_width + 3)
    
    with _LineEndTrimmer(file) as file:

        label: Label | None = None
        if isinstance(line, Label):
            label = line
        elif isinstance(line, Instruction):
            label = line.label
        
        if label is not None:
            if label.is_variable:
                file.write("var ")
            file.write(label.identifier)
            file.write(": ")

        if isinstance(line, Instruction):

            
            if line.label is None:
                file.write("  ")

            if line.condition:

                with _WidthJustifier(file, condition_width or 0) as wrapper:
                    render_condition(line.condition, file=wrapper)
                file.write(" ")
            else:
                if condition_width is not None:
                    file.write(" " * (condition_width + 1))

            mnemonic: str = line.mnemonic.upper()
            if mnemonic_width is not None:
                mnemonic = mnemonic.ljust(mnemonic_width)

            file.write(mnemonic)

            if len(line.arguments) > 0:
                file.write(" ")
                render_argument_list(line.arguments, indent=indent, file=file)

            if line.effect:
                file.write(" ")
                file.write(line.effect.value)

        elif isinstance(line, Constant):
            identifier = line.identifier
            if constant_width is not None:
                identifier = identifier.ljust(constant_width)

            file.write("const ")
            file.write(identifier)
            file.write(" = ")
            render_expression(line.value, indent="", file=file)

        # Instruction | Constant | Label

        file.write("\n")

CONDITION_LUT = {
    # C, Z, Op
    (0, None, None): ">=",
    (1, 1, "or"): "<=",
    (None, 1, None): "==",
    (None, 0, None): "!=",
    (1, None, None): "<",
    (0, 0, "and"): ">",
}

def render_condition(cond: Condition, *, file: io.IOBase):
    if cond.style == ConditionStyle._return:
        assert condition.c_state is None
        assert condition.z_state is None
        file.write("return")
        return
    
    file.write("if(")
    if cond.style == ConditionStyle.boolean:
        if cond.op is None:
            _render_condition_atom(cond.c_state, cond.z_state, file=file)
        else:
            _render_condition_atom(cond.c_state, None, file=file)
            file.write(" ")
            if cond.op == ConditionOp.bool_and:
                file.write("&")
            elif cond.op == ConditionOp.bool_or:
                file.write("|")
            elif cond.op == ConditionOp.equals:
                file.write("==")
            else:
                assert cond.op == ConditionOp.differs
                file.write("==")
            file.write(" ")
            _render_condition_atom(None, cond.z_state, file=file)
    else:
        assert cond.style == ConditionStyle.comparison

        key = (cond.c_state, cond.z_state, cond.op )

        file.write(CONDITION_LUT[key])


    file.write(")")

def _render_condition_atom(c_state: bool|None,z_state: bool|None, *, file : io.IOBase):
    assert (c_state is None) != (z_state is None), f"C={c_state} Z={z_state}"

    if z_state is not None:
        if not z_state:
            file.write("!")
        file.write("Z")
    else:
        assert c_state is not None
        if not c_state:
            file.write("!")
        file.write("C")

def render_argument_list(args: ArgumentList, *, indent: str, file: io.IOBase):
    if len(args) == 0:
        return

    if args.multiline:       
        file.write("\n")

        for i, arg in enumerate(args):
            file.write(indent + "  ")
            render_argument(arg, indent=indent + "  ",file=file)
            file.write(",\n")
        
        file.write(indent)

    else:
        for i, arg in enumerate(args):
            if i > 0:
                file.write(", ")
            render_argument(arg, indent=indent, file=file)

def render_argument(arg: Argument, *, indent: str, file: io.IOBase):

    if arg.name is not None:
        file.write(arg.name)
        file.write("=")

    render_expression(arg.value, indent=indent, file=file)


def render_expression(expr: Expression, *, indent: str, file: io.IOBase):

    handlers: dict[type, Callable] = dict()
    def handler(T: type):
        def _wrap(fun):
            assert handlers.get(T) is None, f"Duplicate handler: {T}"
            handlers[T] = fun
            return fun
        return _wrap

    @handler(WrappingExpression)
    def wrapper(expr: WrappingExpression):
        file.write("(")
        render_expression(expr.value, indent=indent, file=file)
        file.write(")")
    
    @handler(UnaryExpression)
    def unary(expr: UnaryExpression):
        assert False

    @handler(NumericExpression)
    def numeric(expr: NumericExpression):
        if expr.written is not None:
            file.write(expr.written)
            return

        if expr.format == NumberFormat.character:
            file.write("'")

            if expr.value >= 0x20 and expr.value < 0x7F:
                # printable ascii range is always
                file.write(chr(expr.value))
            elif expr.value <= 0xFF:
                # use hex escaping
                file.write(f"\\x{expr.value:02X}")
            else:
                # use unicode escaping
                file.write(f"\\U{{{expr.value:X}}}")

            file.write("'")
            return
        
        MARKERS = {
            NumberFormat.binary: "b",
            NumberFormat.quaternary: "q",
            NumberFormat.decimal: None,
            NumberFormat.hexadecimal: "x",
        }
        DIGITS = "0123456789ABCDEF"

        marker = MARKERS[expr.format]

        if marker is not None:
            file.write("0" + marker)
        
        if expr.value == 0:
            file.write("0")
        
        s = ""
        v = expr.value
        while v > 0:
            i = v % expr.format
            v /= expr.format
            s += DIGITS[i]
        
        file.write(s.reverse())
        
    @handler(FunctionCallExpression)
    def function_call(expr: FunctionCallExpression):
        
        file.write(expr.function)
        file.write("(")
        render_argument_list(expr.arguments, indent=indent, file=file)
        file.write(")")

    @handler(SymbolicExpression)
    def symbol_ref(expr: SymbolicExpression):
        file.write(expr.name)

    @handler(RelativeAddressExpression)
    def relative_address(expr: RelativeAddressExpression):
        file.write(f"@{expr.target}")

    @handler(AddressOfExpression)
    def address_of(expr: AddressOfExpression):
        file.write(f"&{expr.target}")

    @handler(ValueOfExpression)
    def value_of(expr: ValueOfExpression):
        file.write(f"*{expr.target}")

    @handler(ArrayExpression)
    def array(expr: ArrayExpression):
        
        render_expression(expr.value, indent=indent, file=file)
        file.write("[")
        render_expression(expr.count, indent=indent, file=file)
        file.write("]")

    handler = handlers.get(type(expr))
    if handler is None:
        print("unhandled expression type: ", type(expr))
        file.write("<DUMMY>")
        return

    handler(expr)

class _WidthJustifier:
    _file: io.IOBase
    _written: int
    _width: int

    def __init__(self, file: io.IOBase, width: int):
        self._file = file 
        self._written = 0
        self._width = width
    
    def __enter__(self):

        return self

    def __exit__(self, *args):
        if self._written < self._width:
            self.write(" " * (self._width - self._written))
        assert self._written >= self._width

    def write(self, text: str):
        self._file.write(text)
        self._written += len(text)


class _LineEndTrimmer:
    _file: io.IOBase
    _line_buffer: str
    
    def __init__(self, file: io.IOBase):
        self._file = file 
        self._line_buffer = ""
    
    def __enter__(self):
        return self

    def __exit__(self, *args):
        pass

    def write(self, text: str):
        self._line_buffer += text

        while True:
            line, sep, suffix = self._line_buffer.partition("\n")
            if sep == "":
                # no end of line found
                return
            self._file.write(line.rstrip() + "\n")
            self._line_buffer = suffix
