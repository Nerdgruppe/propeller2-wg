import logging

from typing import Callable, Optional


class Function:
    name: str
    function: Callable

    def __init__(self, name: str, function: Callable):
        self.name = name
        self.function = function

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
                self.functions.update({f"{item.name}.{name}": func for name, func in item.functions.items()})
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
