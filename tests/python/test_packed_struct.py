from nerdgruppe.utils.structs import packed_struct, field as sfield


def test_properties():
    @packed_struct
    class TwoBytes:
        a = sfield(bits=8)
        b = sfield(bits=8)

    assert TwoBytes.bit_size == 16
    assert TwoBytes.byte_size == 2
    assert TwoBytes.offset_of("a") == 0
    assert TwoBytes.offset_of("b") == 8
    assert TwoBytes.size_of("a") == 8
    assert TwoBytes.size_of("b") == 8
    assert TwoBytes.mask_of("a") == 0x00FF
    assert TwoBytes.mask_of("b") == 0xFF00


def test_access():
    @packed_struct
    class OneTwoThree:
        i1 = sfield(bits=1)
        i2 = sfield(bits=2)
        i3 = sfield(bits=3)

    value = OneTwoThree.from_int(0b110_01_1)
    assert isinstance(value, OneTwoThree)

    assert value.to_int() == 0b110_01_1

    assert value.i1 == 0b1
    assert value.i2 == 0b01
    assert value.i3 == 0b110

    value.i1 = 0
    value.i2 = 0b10
    value.i3 = 0b001

    assert value.to_int() == 0b001_10_0
    assert value.i1 == 0
    assert value.i2 == 0b10
    assert value.i3 == 0b001
