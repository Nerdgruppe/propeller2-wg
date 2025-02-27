import inspect
import logging
import io

from enum import Enum
from dataclasses import dataclass, field
from typing import Optional, Callable, TypeAlias
from abc import ABC, abstractmethod

from nerdgruppe.p2.specs import instructions

from .stdlib import Library, StandardLibrary

from .ast import (
    Effect,
    ConditionStyle,
    ConditionOp,
    NumberFormat,
    UnaryOperator,
    BinaryOperator,
    Identifier,
    Expression,
    Comment,
    Argument,
    ArgumentList,
    WrappingExpression,
    UnaryExpression,
    BinaryExpression,
    NumericExpression,
    StringExpression,
    FunctionCallExpression,
    SymbolicExpression,
    RelativeAddressExpression,
    AddressOfExpression,
    ValueOfExpression,
    ArrayExpression,
    Condition,
    Line,
    Constant,
    Label,
    Instruction,
    Program,
)

from .types import (
    SymbolType,
    Value,
    ConstValue,
    MemoryAddress,
    PoisonValue,
    IncompleteValue,
    BinaryStream,
    AssemblerState,
    AddressingMode,
)


def TODO(msg: str, /, *args):
    logging.error("TODO: " + msg, *args)


class Symbol:
    type: SymbolType
    name: Identifier
    value: Value | None
    locals: "Scope"

    def __init__(
        self,
        container: "Scope",
        type: SymbolType,
        name: Identifier,
        value: int | Value | None = None,
    ):
        self.locals = Scope(parent=container)
        self.type = type
        self.name = name
        if isinstance(value, int):
            self.value = ConstValue(value)
        else:
            self.value = value

    def __str__(self) -> str:
        prefix = {
            SymbolType.code: "",
            SymbolType.const: "const ",
            SymbolType.data: "var ",
        }[self.type]
        return f"{prefix}{self.name} = {self.value or '<unset>'}"

    def __repr__(self) -> str:
        return f"Symbol(name={self.name!r}, type={self.type.name}, value={self.value or '<unset>'})"


class Scope:
    parent: Optional["Scope"]
    symbols: dict[str, Symbol]

    def __init__(self, *, parent: Optional["Scope"] = None):
        self.symbols = dict()
        self.parent = parent

    def add_symbol(self, symbol: Symbol):
        if symbol.name in self.symbols:
            raise ValueError(f"duplicate symbol: {symbol.name}")
        self.symbols[symbol.name] = symbol

    def get(self, name: str) -> Optional[Symbol]:
        local = self.symbols.get(name)
        if local is not None:
            return local
        if self.parent is not None:
            return self.parent.get(name)
        return None


class CompileUnit:
    globals: Scope

    full_image: bytes

    def __init__(self):
        self.globals = Scope()


EvalResult: TypeAlias = Value | Symbol | Expression


class ByteCode(ABC):
    """
    A code is something that can be emitted.
    """

    argv: list[Value]

    def __init__(self, argv: list[Value]):
        self.argv = argv

    def resolve_values(self, state: AssemblerState) -> None:
        for i, value in enumerate(self.argv):
            self.argv[i] = value.resolve(state)

    @abstractmethod
    def get_size(self) -> int: ...

    @abstractmethod
    def serialize(self, asm: AssemblerState, stream: BinaryStream) -> None: ...


class InstructionByteCode(ByteCode):
    instruction: instructions.Instruction

    def __init__(self, instruction: instructions.Instruction, argv: list[Value]) -> None:
        super().__init__(argv)
        self.instruction = instruction

    def get_size(self) -> int:
        TODO("check for AUG")
        return 4

    def serialize(self, asm: AssemblerState, stream: BinaryStream) -> None:
        pass


class DataByteCode(ByteCode):
    size_per_item: int

    values: list[Value]

    def __init__(self, size: int, values: list[Value]):
        super().__init__(values)
        self.size_per_item = size

    def get_size(self) -> int:
        return self.size_per_item * len(self.argv)

    def serialize(self, asm: AssemblerState, stream: BinaryStream) -> None:
        for value in self.argv:
            integer: int = value.get_value(asm)
            stream.write_int(integer, 8 * self.size_per_item, False)


class ControlByteCode(ByteCode):
    def get_size(self) -> int:
        return 0

    def serialize(self, asm: AssemblerState, stream: BinaryStream) -> None:
        self.execute(asm)

    @abstractmethod
    def execute(self, asm: AssemblerState) -> None: ...


class SwitchAddressingModeCode(ControlByteCode):
    addressing_mode: AddressingMode

    def __init__(self, addressing_mode: AddressingMode, argv: list[Value]):
        super().__init__(argv)
        self.addressing_mode = addressing_mode

    def execute(self, asm: AssemblerState) -> None:
        asm.switch_addressing_mode(self.addressing_mode)


def unwrap(cv: Value) -> int:
    assert isinstance(cv, ConstValue)
    return cv.value


BUILTIN_CODES: dict[str, Callable] = {
    "LONG": lambda argv: DataByteCode(4, argv),
    "WORD": lambda argv: DataByteCode(2, argv),
    "BYTE": lambda argv: DataByteCode(1, argv),
    ".COGEXEC": lambda argv: SwitchAddressingModeCode(AddressingMode.cogexec, argv),
    ".LUTEXEC": lambda argv: SwitchAddressingModeCode(AddressingMode.lutexec, argv),
    ".HUBEXEC": lambda argv: SwitchAddressingModeCode(AddressingMode.hubexec, argv),
}


class Analyzer:
    lib: Library
    isa: instructions.InstructionSet
    program: Program
    cu: CompileUnit

    symbol_lut: dict[Line, Symbol]
    code_lut: dict[Line, ByteCode]

    byte_code: list[ByteCode]

    def __init__(self, program: Program):
        self.isa = instructions.load()
        self.lib = StandardLibrary()

        self.program = program
        self.cu = CompileUnit()

        self.symbol_lut = dict()
        self.code_lut = dict()
        self.byte_code = list()

    def find_symbols(self):
        current_locals: Scope | None = None

        for line in self.program.lines:
            label = line.get_label()

            symbol: Symbol | None = None
            if label is not None:
                assert not isinstance(line, Constant)

                if label.is_variable:
                    symbol = Symbol(self.cu.globals, SymbolType.data, label.identifier)
                else:
                    symbol = Symbol(self.cu.globals, SymbolType.code, label.identifier)

            if isinstance(line, Constant):
                symbol = Symbol(self.cu.globals, SymbolType.const, line.identifier)

            if symbol is not None:
                self.symbol_lut[line] = symbol
                if symbol.name.startswith("."):
                    assert current_locals is not None
                    current_locals.add_symbol(symbol)
                else:
                    self.cu.globals.add_symbol(symbol)
                    current_locals = symbol.locals

    def resolve_constants(self):
        for line in self.program.lines:
            if not isinstance(line, Constant):
                continue
            symbol = self.symbol_lut.get(line)
            assert symbol is not None
            assert symbol.type is SymbolType.const

            value = self.evaluate_expression(
                line.value,
                allow_partial=False,
                scope=self.cu.globals,
                expected_symbol=SymbolType.const,
            )
            assert isinstance(value, Value)

            symbol.value = value

    def assemble_first_pass(self):
        """
        Creates the basic byte codes, and fills them with all necessary
        information so the second pass can resolve all addresses.
        """
        active_symbol: Symbol = None
        for instr in self.program.lines:
            active_symbol = self.symbol_lut.get(instr, active_symbol)
            if not isinstance(instr, Instruction):
                continue

            code = instr.mnemonic.upper()
            bytecode: ByteCode

            argv = [
                self.evaluate_expression(
                    arg.value,
                    allow_partial=True,
                    scope=active_symbol.locals if active_symbol is not None else self.cu.globals,
                    expected_symbol=None,
                )
                for arg in instr.arguments or []
            ]

            if code in BUILTIN_CODES:
                ctor = BUILTIN_CODES[code]

                bytecode = ctor(argv)

            else:
                # instruction

                isa_instr = self.select_instruction(instr, argv)

                bytecode = InstructionByteCode(isa_instr)

            self.code_lut[instr] = bytecode
            self.byte_code.append(bytecode)

    def assemble_second_pass(self):
        """
        Fully analyzes the byte code and assigns addresses and labels
        """

        asm = AssemblerState()
        stream = BinaryStream()

        active_symbol: Symbol = None
        for instr in self.program.lines:
            line_symbol = self.symbol_lut.get(instr)
            if line_symbol is not None:
                active_symbol = line_symbol

                if line_symbol.type != SymbolType.const:
                    assert line_symbol.value is None
                    line_symbol.value = MemoryAddress(
                        cluster=asm.current_cluster,
                        hub=asm.hub_address,
                        local=asm.local_address,
                    )

            if not isinstance(instr, Instruction):
                continue

            bytecode: ByteCode = self.code_lut[instr]

            bytecode.resolve_values(asm)

            size = bytecode.get_size()

            bytecode.serialize(asm, stream)

            asm.hub_address += size
            if asm.addressing_mode != AddressingMode.hubexec:
                assert (size % 4) == 0, "line does not emit a multiple of 4 bytes!"
                asm.local_address += size // 4
            else:
                asm.local_address += size

        self.cu.full_image = stream.get_bytes()

    def select_instruction(self, instr: Instruction, argv: list) -> instructions.Instruction:
        group = self.isa.get_group(instr.mnemonic.upper())
        if group is None:
            raise ValueError(f"{instr.mnemonic} is not a valid mnemonic")
        assert len(group) > 0

        argc = len(argv)

        matches: list[instructions.Instruction] = list()
        for isa_instr in group:
            if len(isa_instr.parameters) != argc:
                continue

            if instr.effect not in COMPATIBLE_ISA_EFFECTS[isa_instr.effect]:
                continue

            ok = True
            for param, arg in zip(isa_instr.parameters, argv):
                if isinstance(arg, Symbol):
                    if param.immediate == instructions.Immediate.yes:
                        ok = arg.type == SymbolType.code
                    elif param.immediate == instructions.Immediate.no:
                        ok = arg.type == SymbolType.data

            if not ok:
                continue

            matches.append(isa_instr)

        if len(matches) == 0:
            raise ValueError(f"Could not find matching instruction for {instr.mnemonic}")

        assert len(matches) == 1, "Ambigious matches: " + ", ".join(m.mnemonic for m in matches)

        return matches[0]

    def evaluate_expression(
        self,
        value: Expression,
        *,
        allow_partial: bool,
        scope: Scope,
        expected_symbol: SymbolType,
    ) -> EvalResult:
        _handlers: dict[type, Callable] = dict()

        def handler(fun) -> Callable:
            props = inspect.getfullargspec(fun)
            assert props.args == ["expr"]
            arg_type: type = props.annotations["expr"]

            assert arg_type not in _handlers
            _handlers[arg_type] = fun

        def recurse(val: Expression) -> EvalResult:
            return self.evaluate_expression(
                val,
                allow_partial=allow_partial,
                scope=scope,
                expected_symbol=expected_symbol,
            )

        @handler
        def _(expr: WrappingExpression) -> EvalResult:
            return recurse(expr.value)

        @handler
        def _(expr: NumericExpression) -> EvalResult:
            return ConstValue(expr.value)

        @handler
        def _(expr: SymbolicExpression) -> EvalResult:
            sym = scope.get(expr.name)
            if sym is not None:
                if sym.value is None:
                    assert allow_partial, "Undefined symbol used in non-partial context"

                    def _resolve(state) -> Value:
                        assert sym.value is not None, f"{sym.name!r} could not be resolved."
                        return sym.value

                    return IncompleteValue(_resolve)

                return sym.value
            else:
                # How to resolve that?
                return expr.name

        @handler
        def _(expr: UnaryExpression) -> EvalResult:
            value = recurse(expr.value)
            op = {
                UnaryOperator.bitwise_invert: lambda v: ~v,
                UnaryOperator.logical_invert: lambda v: 0 if v != 0 else 1,
                UnaryOperator.plus: lambda v: v,
                UnaryOperator.negate: lambda v: -v,
            }
            return op[expr.operator](value)

        @handler
        def _(expr: BinaryExpression) -> EvalResult:
            lhs = unwrap(recurse(expr.lhs))
            rhs = unwrap(recurse(expr.rhs))

            op = {
                BinaryOperator.logical_and: lambda l, r: int(bool(l) and bool(r)),
                BinaryOperator.logical_or: lambda l, r: int(bool(l) or bool(r)),
                BinaryOperator.logical_xor: lambda l, r: int(bool(l) != bool(r)),
                BinaryOperator.inequal: lambda l, r: int(l != r),
                BinaryOperator.less_than: lambda l, r: int(l < r),
                BinaryOperator.less_or_equal: lambda l, r: int(l <= r),
                BinaryOperator.compare: lambda l, r: -1 if l < r else 1 if l > r else 0,
                BinaryOperator.equals: lambda l, r: int(l == r),
                BinaryOperator.greater_than: lambda l, r: int(l > r),
                BinaryOperator.greater_or_equal: lambda l, r: int(l >= r),
                BinaryOperator.subtract: lambda l, r: l - r,
                BinaryOperator.add: lambda l, r: l + r,
                BinaryOperator.multiply: lambda l, r: l * r,
                BinaryOperator.divide: lambda l, r: l / r,
                BinaryOperator.modulus: lambda l, r: l % r,
                BinaryOperator.bitwise_or: lambda l, r: l | r,
                BinaryOperator.bitwise_and: lambda l, r: l & r,
                BinaryOperator.bitwise_xor: lambda l, r: l ^ r,
                BinaryOperator.shift_left: lambda l, r: l << r,
                BinaryOperator.shift_right: lambda l, r: l >> r,
            }
            return ConstValue(op[expr.operator](lhs, rhs))

        @handler
        def _(expr: StringExpression) -> EvalResult:
            TODO("String expressions")
            assert False, "StringExpression"

        @handler
        def _(expr: FunctionCallExpression) -> EvalResult:
            func = self.lib.get_function(expr.function)
            if func is None:
                raise KeyError(f"Function {expr.function!r} does not exist!")

            args = [recurse(arg.value) for arg in expr.arguments if arg.name is None]
            kwargs = {arg.name: recurse(arg.value) for arg in expr.arguments if arg.name is not None}

            if any(isinstance(arg, IncompleteValue) for arg in args) or any(
                isinstance(arg, IncompleteValue) for arg in kwargs.values()
            ):
                assert allow_partial, "detected incomplete value in non-partial context"

                def _eval(state: AssemblerState) -> Value:
                    true_args = [arg.resolve(state) for arg in args]
                    true_kwargs = {name: arg.resolve(state) for name, arg in kwargs.items()}
                    return func(*true_args, **true_kwargs)

                return IncompleteValue(_eval)

            return func(*args, **kwargs)

        @handler
        def _(expr: RelativeAddressExpression) -> EvalResult:
            sym = scope.get(expr.target)
            if sym is None:
                raise KeyError(f"A label named {expr.target!r} does not exist!")
            if sym.type == SymbolType.const:
                raise KeyError(f"A {expr.target!r} refers to a constant, but label expected!")

            return ("relative", sym)

        @handler
        def _(expr: AddressOfExpression) -> EvalResult:
            sym = scope.get(expr.target)
            if sym is None:
                raise KeyError(f"A label named {expr.target!r} does not exist!")
            if sym.type == SymbolType.const:
                raise KeyError(f"A {expr.target!r} refers to a constant, but label expected!")

            return ("immediate", sym)

        @handler
        def _(expr: ValueOfExpression) -> EvalResult:
            sym = scope.get(expr.target)
            if sym is None:
                raise KeyError(f"A label named {expr.target!r} does not exist!")
            if sym.type == SymbolType.const:
                raise KeyError(f"A {expr.target!r} refers to a constant, but label expected!")

            return ("register", sym)

        @handler
        def _(expr: ArrayExpression) -> EvalResult:
            value = recurse(expr.value)
            count = recurse(expr.count)

            TODO("implement array values: %r[%r]", value, count)

            return value

        processor = _handlers[type(value)]
        return processor(value)


COMPATIBLE_ISA_EFFECTS: dict[instructions.Effect, frozenset[Effect | None]] = {
    None: frozenset({None}),
    instructions.Effect.opt_wc: frozenset({None, Effect.wc}),
    instructions.Effect.opt_wc_wz_wcz: frozenset({None, Effect.wc, Effect.wz, Effect.wcz}),
    instructions.Effect.opt_wcz: frozenset({None, Effect.wcz}),
    instructions.Effect.opt_wz: frozenset({None, Effect.wz}),
    instructions.Effect.andc_andz: frozenset({Effect.and_c, Effect.and_z}),
    instructions.Effect.orc_orz: frozenset({Effect.or_c, Effect.or_z}),
    instructions.Effect.wc_wz: frozenset({Effect.wc, Effect.wz}),
    instructions.Effect.wc_wz_wcz: frozenset({Effect.wc, Effect.wz, Effect.wcz}),
    instructions.Effect.xorc_xorz: frozenset({Effect.xor_c, Effect.xor_z}),
}


def analyze(program: Program) -> CompileUnit:
    analyzer = Analyzer(program)

    analyzer.find_symbols()

    analyzer.resolve_constants()

    analyzer.assemble_first_pass()

    analyzer.assemble_second_pass()

    return analyzer.cu
