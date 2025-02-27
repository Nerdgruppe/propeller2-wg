class _PackedStructBase:
    _FIELDS: list["StructField"]
    _FIELD_BY_NAME: dict[str, "StructField"]

    bit_size: int
    byte_size: int
    _raw: int 

class StructField:
    _total_count: int = 0

    index: int
    bits: int
    signed: bool

    # assigned later:
    name: str
    offset: int

    def __init__(self, bits: int, signed: bool = False) -> None:
        self.bits = bits
        self.signed = signed
        self.index = StructField._total_count
        StructField._total_count += 1

    @property
    def shifted_mask(self) -> int:
        return self.unshifted_mask << self.offset

    @property
    def unshifted_mask(self) -> int:
        return (1 << self.bits) - 1

    def __get__(self, instance: "_PackedStructBase", owner: type | None = None, /) -> int:
        return (instance._raw >> self.offset) & self.unshifted_mask

    def __set__(self, instance: "_PackedStructBase", value: int, /) -> None:
        if (value & ~self.unshifted_mask) != 0:
            raise ValueError("out of range")
        instance._raw = (instance._raw & ~self.shifted_mask) | (value << self.offset)


def field(*args, **kwargs) -> StructField:
    return StructField(*args, **kwargs)


def packed_struct(cls: type) -> type:
    named_fields = [
        (name, field)
        for name, field in vars(cls).items()
        if isinstance(field, StructField)
    ]

    named_fields.sort(key=lambda nf: nf[1].index)

    total_bits = 0
    for i, (name, field) in enumerate(named_fields):
        field.index = i
        field.name = name
        field.offset = total_bits
        total_bits += field.bits

    fields = [field for (name, field) in named_fields]

    assert total_bits == sum(f.bits for f in fields)

    _mask: int = (1 << total_bits) - 1

    class PackedStruct(cls, _PackedStructBase):
        _FIELDS: list[StructField] = fields
        _FIELD_BY_NAME: dict[str, StructField] = {f.name: f for f in fields}

        bit_size: int = total_bits
        byte_size: int = (total_bits + 7) // 8

        _raw: int

        def __init__(self, **kwargs):
            self._raw = 0
            for key, value in kwargs.items():
                self._FIELDS[key].__set__(self,key, value)

        @classmethod
        def from_int(cls, value: int):
            if (value & ~_mask) != 0:
                raise ValueError("value out of range")

            struct = cls()
            struct._raw = value
            return struct

        def to_int(self) -> int:
            assert (self._raw & ~_mask) == 0
            return self._raw

        @classmethod
        def offset_of(cls, field_name: str) -> int:
            return cls._FIELD_BY_NAME[field_name].offset

        @classmethod
        def size_of(cls, field_name: str) -> int:
            return cls._FIELD_BY_NAME[field_name].bits

        @classmethod
        def mask_of(cls, field_name: str) -> int:
            return  cls._FIELD_BY_NAME[field_name].shifted_mask

    return PackedStruct
