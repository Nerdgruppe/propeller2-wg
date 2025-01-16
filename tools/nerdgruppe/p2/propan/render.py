import io

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
    RelativeAddressExpression,
    AddressOfExpression,
    ValueOfExpression,
    ArrayExpression,
)

def render(program: Program, *, file: io.IOBase):

    mnemonic_width = 0
    condition_width = 0
    for line in program.lines:
        if isinstance(line, Instruction):
            mnemonic_width = max(mnemonic_width, len(line.mnemonic))

            if line.condition is not None:
                buffer = io.StringIO()
                render_condition(line.condition, file=buffer)
                condition_width = max(condition_width, len(buffer.getvalue()))

    for line in program.lines:
        render_line(
            line, 
            file=file,
            condition_width=condition_width,
            mnemonic_width=mnemonic_width,
        )

def render_line(
    line: Instruction | Constant | Label | None, 
    *, 
    file: io.IOBase, 
    mnemonic_width: int | None = None,
    condition_width: int | None = None,
):
    if line is None:
        file.write("\n")
        return

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

        mnemonic: str = line.mnemonic.upper()
        if mnemonic_width is not None:
            mnemonic = mnemonic.ljust(mnemonic_width)

        file.write(mnemonic)

        if line.arguments:
            file.write(" <ARGS>")

        if line.effect:
            file.write(" ")
            file.write(line.effect.value)

    elif isinstance(line, Constant):
        file.write("const ")
        file.write(line.identifier)
        file.write(" = ")
        file.write("<EXPR>")

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



class _WidthJustifier:

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
    