import lark

from pathlib import Path

from lark import Lark, Transformer, v_args

from .grammar import PASM_GRAMMAR

@v_args(inline=True)    # Affects the signatures of the methods
class PasmTransformer(Transformer):
    pass 

parser = Lark(PASM_GRAMMAR)

transformer=PasmTransformer()

parse_tree = parser.parse(Path("data/codesamples/misoyume_lower.spin2").read_text())

ast_tree = transformer.transform(parse_tree)
