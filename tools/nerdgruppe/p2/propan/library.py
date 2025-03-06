import logging
import textwrap
import inspect

from enum import Enum
from typing import Callable, Optional, Any

from nerdgruppe.utils import SortedConstDict

from .types import IntWithRange, Value, ConstValue, LookUpTable
from .ast import Identifier


class _UNSET_TYPE:
    def __str__(self) -> str:
        return "<unset>"

    def __repr__(self) -> str:
        return "<unset>"


UNSET = _UNSET_TYPE()

_OK_TYPES = [
    int,
    str,
    bool,
    Value,
    Enum,
    LookUpTable,
]
_TRUTHY_VALUES = (".yes", ".true", ".on")
_FALSY_VALUES = (".no", ".false", ".off")


def _check_type_support(param_type: type) -> bool:
    mro = param_type.mro()

    for ok_type in _OK_TYPES:
        if ok_type in mro:
            return True

    return False


class Parameter:
    name: str
    param_type: type
    default_value: Any

    def __init__(self, name: str, param_type: type, default_value: Any) -> None:
        assert param_type is not UNSET

        assert isinstance(name, str)
        assert isinstance(param_type, type)

        assert _check_type_support(param_type), (
            f"unsupported parameter type: {param_type}"
        )

        self.name = name
        self.param_type = param_type
        self.default_value = default_value

    def coerce(self, src_value: Any):
        unwrapped_value: Any

        if isinstance(src_value, ConstValue):
            unwrapped_value = src_value.value
        if isinstance(src_value, Value):
            unwrapped_value = src_value.get_value(
                None
            )  # TODO: This will eventually fail!
        else:
            unwrapped_value = src_value

        if Enum in self.param_type.mro():
            assert isinstance(unwrapped_value, Identifier), (
                f"expected an identifier, but found {unwrapped_value!r} ({type(unwrapped_value)})"
            )
            assert unwrapped_value.startswith("."), (
                f"expected an enum literal, but found {unwrapped_value!r}"
            )

            try:
                return self.param_type[unwrapped_value[1:]]
            except ValueError:
                print(
                    f"invalid enumeration value {repr(unwrapped_value[1:])}. valid options are:"
                )
                for val in self.param_type:
                    print(f"- {val.name!r}")
                raise

        if LookUpTable in self.param_type.mro():
            assert isinstance(unwrapped_value, Identifier), (
                f"expected an identifier, but found {unwrapped_value!r} ({type(unwrapped_value)})"
            )
            assert unwrapped_value.startswith("."), (
                f"expected an enum literal, but found {unwrapped_value!r}"
            )

            try:
                return self.param_type.get(unwrapped_value[1:])
            except ValueError:
                print(
                    f"invalid enumeration value {repr(unwrapped_value[1:])}. valid options are:"
                )
                for val in self.param_type:
                    print(f"- {val.name!r}")
                raise

        if IntWithRange in self.param_type.mro():
            assert isinstance(unwrapped_value, int), (
                f"expected integer, but found {type(unwrapped_value)} ({unwrapped_value!r})"
            )

            minval, maxval = self.param_type.get_range()

            assert unwrapped_value >= minval and unwrapped_value <= maxval, (
                f"{self.param_type.__name__} must be between {minval} and {maxval}, but found {unwrapped_value}"
            )

            return self.param_type(unwrapped_value)

        if bool in self.param_type.mro():
            if isinstance(unwrapped_value, int):
                return unwrapped_value != 0
            if isinstance(unwrapped_value, Identifier):
                if unwrapped_value in _TRUTHY_VALUES:
                    return True
                elif unwrapped_value in _FALSY_VALUES:
                    return False
                else:
                    assert False, (
                        f"expected one of {', '.join(_TRUTHY_VALUES + _FALSY_VALUES)}, but got {unwrapped_value!r}"
                    )

        assert isinstance(unwrapped_value, self.param_type), (
            f"{unwrapped_value} ({type(unwrapped_value)} is not of type {self.param_type}"
        )

        if isinstance(unwrapped_value, (str, int, bool)):
            return unwrapped_value
        assert False, repr(type(src_value))

    def __repr__(self) -> str:
        return f"Parameter(name={self.name!r}, param_type={self.param_type!r}, default_value={self.default_value!r})"


class Function:
    name: str
    function: Callable
    docs: str | None
    params: SortedConstDict[str, Parameter]

    def __init__(self, name: str, function: Callable):
        self.name = name
        self.function = function
        self.docs = textwrap.dedent(function.__doc__ or "").strip()

        argspec = inspect.getfullargspec(function)

        assert argspec.varargs is None, "not supported yet!"
        assert argspec.varkw is None, "not supported yet!"
        assert len(argspec.kwonlyargs) == 0, "not supported yet!"
        assert argspec.kwonlydefaults is None, "not supported yet!"

        default_values = (
            {
                argspec.args[
                    len(argspec.args) - len(argspec.defaults) + i
                ]: default_value
                for i, default_value in enumerate(argspec.defaults or tuple())
            }
            if argspec.defaults is not None
            else dict()
        )

        self.params = SortedConstDict(
            items=[
                (
                    argname,
                    Parameter(
                        name=argname,
                        param_type=argspec.annotations.get(argname, UNSET),
                        default_value=default_values.get(argname, UNSET),
                    ),
                )
                for argname in argspec.args
            ]
        )


    def __call__(self, *args, **kwargs):
        return self.function(*args, **kwargs)


class Namespace:
    name: str
    functions: dict[str, Callable]

    def __init__(self):
        self.name = self.__class__.__name__
        self.functions = dict()

        for name, item in vars(self.__class__).items():
            if isinstance(item, Function):
                assert item.name == name
                self.functions[name] = item
            elif isinstance(item, type) and Namespace in item.__mro__:
                item = item()  # create instance of namespace type
                assert item.name == name
                self.functions.update(
                    {
                        f"{item.name}.{name}": func
                        for name, func in item.functions.items()
                    }
                )
            else:
                pass  # print(name, type(item), item)


class Library(Namespace):
    def get_function(self, name: str) -> Optional[Callable]:
        return self.functions.get(name)


class LibraryCollection:
    functions: dict[str, Function]

    def __init__(self, *libs: Library):
        self.functions = dict()
        for lib in libs:
            self.functions.update(lib.functions)

    def get_function(self, name: str) -> Optional[Callable]:
        return self.functions.get(name)


def function(fun):
    return Function(fun.__name__, fun)
