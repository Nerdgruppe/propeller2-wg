from typing import TypeVar, Generic

TValue = TypeVar("TKey")
TKey = TypeVar("TValue")


class SortedConstDict(Generic[TKey, TValue]):
    _ordered: tuple[TValue]
    _lut: dict[TKey, TValue]

    def __init__(self, *, items: list[tuple[TKey, TValue]]) -> None:
        self._ordered = tuple(val for _, val in items)
        self._lut = {key: value for key, value in items}

    def __iter__(self):
        return iter(self._ordered)

    def __getitem__(self, index: int | TKey) -> TValue:
        if isinstance(index, int):
            return self._ordered[index]
        else:
            return self._lut[index]
