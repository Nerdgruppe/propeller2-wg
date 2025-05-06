from pathlib import Path

OUT_DIR = Path(__file__).absolute().parent

instructions = [
    "ROR",  #  D,{#}S {WC/WZ/WCZ}
    "ROL",  #  D,{#}S {WC/WZ/WCZ}
    "SHR",  #  D,{#}S {WC/WZ/WCZ}
    "SHL",  #  D,{#}S {WC/WZ/WCZ}
    "RCR",  #  D,{#}S {WC/WZ/WCZ}
    "RCL",  #  D,{#}S {WC/WZ/WCZ}
    "SAR",  #  D,{#}S {WC/WZ/WCZ}
    "SAL",  #  D,{#}S {WC/WZ/WCZ}
    "ADD",  #  D,{#}S {WC/WZ/WCZ}
    "ADDX",  #  D,{#}S {WC/WZ/WCZ}
    "ADDS",  #  D,{#}S {WC/WZ/WCZ}
    "ADDSX",  #  D,{#}S {WC/WZ/WCZ}
    "SUB",  #  D,{#}S {WC/WZ/WCZ}
    "SUBX",  #  D,{#}S {WC/WZ/WCZ}
    "SUBS",  #  D,{#}S {WC/WZ/WCZ}
    "SUBSX",  #  D,{#}S {WC/WZ/WCZ}
    "CMP",  #  D,{#}S {WC/WZ/WCZ}
    "CMPX",  #  D,{#}S {WC/WZ/WCZ}
    "CMPS",  #  D,{#}S {WC/WZ/WCZ}
    "CMPSX",  #  D,{#}S {WC/WZ/WCZ}
    "CMPR",  #  D,{#}S {WC/WZ/WCZ}
    "CMPM",  #  D,{#}S {WC/WZ/WCZ}
    "SUBR",  #  D,{#}S {WC/WZ/WCZ}
    "CMPSUB",  #  D,{#}S {WC/WZ/WCZ}
    "FGE",  #  D,{#}S {WC/WZ/WCZ}
    "FLE",  #  D,{#}S {WC/WZ/WCZ}
    "FGES",  #  D,{#}S {WC/WZ/WCZ}
    "FLES",  #  D,{#}S {WC/WZ/WCZ}
    "SUMC",  #  D,{#}S {WC/WZ/WCZ}
    "SUMNC",  #  D,{#}S {WC/WZ/WCZ}
    "SUMZ",  #  D,{#}S {WC/WZ/WCZ}
    "SUMNZ",  #  D,{#}S {WC/WZ/WCZ}
    # "TESTB",  #  D,{#}S WC/WZ
    # "TESTBN",  #  D,{#}S WC/WZ
    # "TESTB",  #  D,{#}S ANDC/ANDZ
    # "TESTBN",  #  D,{#}S ANDC/ANDZ
    # "TESTB",  #  D,{#}S ORC/ORZ
    # "TESTBN",  #  D,{#}S ORC/ORZ
    # "TESTB",  #  D,{#}S XORC/XORZ
    # "TESTBN",  #  D,{#}S XORC/XORZ
    # "BITL",  #  D,{#}S {WCZ}
    # "BITH",  #  D,{#}S {WCZ}
    # "BITC",  #  D,{#}S {WCZ}
    # "BITNC",  #  D,{#}S {WCZ}
    # "BITZ",  #  D,{#}S {WCZ}
    # "BITNZ",  #  D,{#}S {WCZ}
    # "BITRND",  #  D,{#}S {WCZ}
    # "BITNOT",  #  D,{#}S {WCZ}
    "AND",  #  D,{#}S {WC/WZ/WCZ}
    "ANDN",  #  D,{#}S {WC/WZ/WCZ}
    "OR",  #  D,{#}S {WC/WZ/WCZ}
    "XOR",  #  D,{#}S {WC/WZ/WCZ}
    "MUXC",  #  D,{#}S {WC/WZ/WCZ}
    "MUXNC",  #  D,{#}S {WC/WZ/WCZ}
    "MUXZ",  #  D,{#}S {WC/WZ/WCZ}
    "MUXNZ",  #  D,{#}S {WC/WZ/WCZ}
    "MOV",  #  D,{#}S {WC/WZ/WCZ}
    "NOT",  #  D,{#}S {WC/WZ/WCZ}
    # "NOT",  #  D {WC/WZ/WCZ}
    "ABS",  #  D,{#}S {WC/WZ/WCZ}
    # "ABS",  #  D {WC/WZ/WCZ}
    "NEG",  #  D,{#}S {WC/WZ/WCZ}
    # "NEG",  #  D {WC/WZ/WCZ}
    "NEGC",  #  D,{#}S {WC/WZ/WCZ}
    # "NEGC",  #  D {WC/WZ/WCZ}
    "NEGNC",  #  D,{#}S {WC/WZ/WCZ}
    # "NEGNC",  #  D {WC/WZ/WCZ}
    "NEGZ",  #  D,{#}S {WC/WZ/WCZ}
    # "NEGZ",  #  D {WC/WZ/WCZ}
    "NEGNZ",  #  D,{#}S {WC/WZ/WCZ}
    # "NEGNZ",  #  D {WC/WZ/WCZ}
    "INCMOD",  #  D,{#}S {WC/WZ/WCZ}
    "DECMOD",  #  D,{#}S {WC/WZ/WCZ}
    "ZEROX",  #  D,{#}S {WC/WZ/WCZ}
    "SIGNX",  #  D,{#}S {WC/WZ/WCZ}
    "ENCOD",  #  D,{#}S {WC/WZ/WCZ}
    # "ENCOD",  #  D {WC/WZ/WCZ}
    "ONES",  #  D,{#}S {WC/WZ/WCZ}
    # "ONES",  #  D  {WC/WZ/WCZ}
    "TEST",  #  D,{#}S {WC/WZ/WCZ}
    # "TEST",  #  D {WC/WZ/WCZ}
    "TESTN",  #  D,{#}S {WC/WZ/WCZ}
]

conditions = [
    ("_RET_", "return"),
    ("IF_NC_AND_NZ", "if(!c & !z)"),
    ("IF_NZ_AND_NC", "if(!z & !c)"),
    ("IF_GT", "if(>)"),
    ("IF_A", "if(>)"),
    ("IF_00", "if(!c & !z)"),
    ("IF_NC_AND_Z", "if(!c & z)"),
    ("IF_Z_AND_NC", "if(z & !c)"),
    ("IF_01", "if(!c & z)"),
    ("IF_NC", "if(!c)"),
    ("IF_GE", "if(>=)"),
    ("IF_AE", "if(>=)"),
    ("IF_0X", "if(!c)"),
    ("IF_C_AND_NZ", "if(c & !z)"),
    ("IF_NZ_AND_C", "if(!z & c)"),
    ("IF_10", "if(c & !z)"),
    ("IF_NZ", "if(!z)"),
    ("IF_NE", "if(!=)"),
    ("IF_X0", "if(!=)"),
    ("IF_C_NE_Z", "if(c != z)"),
    ("IF_Z_NE_C", "if(z != c)"),
    ("IF_DIFF", "if(c != z)"),
    ("IF_NC_OR_NZ", "if(!c | !z)"),
    ("IF_NZ_OR_NC", "if(!c | !z)"),
    ("IF_NOT_11", "if(!c | !z)"),
    ("IF_C_AND_Z", "if(c & z)"),
    ("IF_Z_AND_C", "if(z & c)"),
    ("IF_11", "if(c & z)"),
    ("IF_C_EQ_Z", "if(c == z)"),
    ("IF_Z_EQ_C", "if(z == c)"),
    ("IF_SAME", "if(c == z)"),
    ("IF_Z", "if(z)"),
    ("IF_E", "if(z)"),
    ("IF_X1", "if(z)"),
    ("IF_NC_OR_Z", "if(!c | z)"),
    ("IF_Z_OR_NC", "if(z | !c)"),
    ("IF_NOT_10", "if(!c | z)"),
    ("IF_C", "if(c)"),
    ("IF_LT", "if(<)"),
    ("IF_B", "if(<)"),
    ("IF_1X", "if(c)"),
    ("IF_C_OR_NZ", "if(c | !z)"),
    ("IF_NZ_OR_C", "if(!z | c)"),
    ("IF_NOT_01", "if(!z | c)"),
    ("IF_C_OR_Z", "if(c | z)"),
    ("IF_Z_OR_C", "if(z | c)"),
    ("IF_LE", "if(<=)"),
    ("IF_BE", "if(<=)"),
    ("IF_NOT_00", "if(<=)"),
]

label_count = 9


def gen_lrefs(make_imm, make_immnum):
    make_immnum = make_immnum or make_imm
    i = 0
    while True:
        for idx in range(label_count):
            name = f"lbl{idx}"
            if (i % 4) == 3:
                yield make_immnum(i)
            elif (i % 8) == 7:
                yield make_imm(name)
            else:
                yield name
            i += 1


def gen_sequence(items):
    while True:
        for item in items:
            yield item


with open(OUT_DIR / "arithmetic_group.propan", "w") as fp:
    effect = gen_sequence(("", " :wc", " :wz", " :wcz"))
    cond = gen_sequence([second for _, second in conditions])
    label = gen_lrefs(lambda n: f"&{n}", lambda n: f"{n}")

    for i, instr in enumerate(instructions):
        fp.write(f"{next(cond)} {instr} {next(label)}, {next(label)}{next(effect)} // {4 * i:05X}\n")

    fp.write("\n")

    for i in range(label_count):
        fp.write(f"var lbl{i}: LONG {i}\n")

with open(OUT_DIR / "arithmetic_group.spin2", "w") as fp:
    effect = gen_sequence(("", " WC", " WZ", " WCZ"))
    cond = gen_sequence([first for first, _ in conditions])
    label = gen_lrefs(lambda n: f"#{n}", None)

    fp.write("DAT\n")

    for i, instr in enumerate(instructions):
        fp.write(f"{next(cond)} {instr} {next(label)}, {next(label)}{next(effect)} ' {4 * i:05X}\n")

    fp.write("\n")

    for i in range(label_count):
        fp.write(f"lbl{i} LONG {i}\n")
