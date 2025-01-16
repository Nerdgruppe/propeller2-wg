from lark.load_grammar import load_grammar

PROPAN_GRAMMAR, _ = load_grammar(
    r"""
    
    ?start: program

    program     : line*

    ?line       : const_decl eol    -> line
                | instruction eol   -> line
                | label eol         -> line 
                | eol               -> empty_line
 
    const_decl  : "const" ident "=" expr

    instruction : [label] [condition] ident [arglist] [effect]

    label       : "var" ident ":" -> var_label
                | ident ":"       -> label

    condition   : "if" "(" conditional  ")" -> condition
                | "return"                  -> return_condition

    ?conditional : COND_OP                     -> compare_condition
                | flag_spec "|" flag_spec   -> op_condition_or
                | flag_spec "&" flag_spec   -> op_condition_and
                | flag_spec "==" flag_spec  -> op_condition_eq
                | flag_spec "!=" flag_spec  -> op_condition_neq
                | flag_spec

    flag_spec   : FLAG                      -> true_flag
                | "!" FLAG                  -> inv_flag

    arglist     : arg ("," arg)*            -> arglist

    arglist_nl  : eol* arg ( eol* "," eol* arg)* eol* (comma eol*)? -> arglist

    arg         : expr              -> positional_arg
                | ident "=" expr    -> named_arg    

    effect      : EFFECT_ANDC   -> effect_andc
                | EFFECT_ANDZ   -> effect_andz
                | EFFECT_ORC    -> effect_orc
                | EFFECT_ORZ    -> effect_orz
                | EFFECT_XORC   -> effect_xorc
                | EFFECT_XORZ   -> effect_xorz
                | EFFECT_WC     -> effect_wc
                | EFFECT_WCZ    -> effect_wcz
                | EFFECT_WZ     -> effect_wz

    ?expr       : expr_l0

    ?expr_l0    : expr_l1 BINOP_L0 expr_l0      -> binary_op
                | expr_l1

    BINOP_L0    : "and"
                | "or"
                | "xor"

    ?expr_l1     : expr_l2 BINOP_L1 expr_l1     -> binary_op
                | expr_l2

    BINOP_L1    : "<=>"
                | "=="
                | "!="
                | ">="
                | "<="
                | ">"
                | "<"

    ?expr_l2     : expr_l3 BINOP_L2 expr_l2     -> binary_op
                | expr_l3
    
    BINOP_L2    : "+"
                | "-"
                | "|"
                | "^"

    ?expr_l3    : expr_l4 BINOP_L3 expr_l3      -> binary_op
                | expr_l4

    BINOP_L3    : "*"
                | "/"
                | "%"
                | "&"

    ?expr_l4    : expr_l5 BINOP_L4 expr_l4      -> binary_op
                | expr_l5

    BINOP_L4    : "<<"
                | ">>"

    ?expr_l5     : expr_unary

    ?expr_unary  : "-" expr_unary -> unary_minus
                | "+" expr_unary -> unary_plus
                | "!" expr_unary -> unary_not
                | "~" expr_unary -> unary_inverse
                | expr_atomic

    ?expr_atomic : expr_atomic "[" expr "]"       -> array_ctor
                | ident "(" [arglist_nl] eol* ")" -> function_call
                | ident                           -> symbol_ref
                | "@" ident                       -> relative_op
                | "*" ident                       -> deref_op
                | "&" ident                       -> addrof_op
                | number
                | CHARLITERAL                     -> character_literal
                | STRINGLITERAL                   -> string_literal
                | ENUMLITERAL                     -> symbol_ref
                | "(" expr "?" expr ":" expr ")"
                | "(" expr ")"                    -> wrapping_expr

    ident       : IDENTIFIER
    eol         : EOL
    comma       : ","

    number      : BIN_NUMBER -> bin_number
                | QUAD_NUMBER -> quad_number
                | DEC_NUMBER -> dec_number
                | HEX_NUMBER -> hex_number

    COMMENT     : "//" /[^\n]*/
    IDENTIFIER  : /\.?[a-z_][a-z0-9_]*(\.[a-z_][a-z0-9_]*)*/i

    BIN_NUMBER  : /\b0b[01_]+\b/
    QUAD_NUMBER : /\b0q[01234_]+\b/
    DEC_NUMBER  : /\b[0-9_]+\b/
    HEX_NUMBER  : /\b0x[0-9a-fA-F_]+\b/

    CHARLITERAL     : /'(\.|[^'])*'/
    STRINGLITERAL   : /"(\.|[^"])*"/

    ENUMLITERAL : /\.[a-zA-Z0-9_]+/

    FLAG        : "C"i | "Z"i

    COND_OP : ">=" | "<=" | "==" | "!=" | "<" | ">"

    EFFECT_ANDC : /:and_?c/i
    EFFECT_ANDZ : /:and_?z/i
    EFFECT_ORC  : /:or_?c/i
    EFFECT_ORZ  : /:or_?z/i
    EFFECT_XORC : /:xor_?c/i
    EFFECT_XORZ : /:xor_?z/i
    EFFECT_WC   : /:wc/i
    EFFECT_WCZ  : /:wcz/i
    EFFECT_WZ   : /:wz/i

    WS  : /[ \t]+/
    EOL : /[ \t]*\r?\n/

    %ignore WS
    %ignore COMMENT

""",
    source=__file__,
    import_paths=None,
    global_keep_all_tokens=False,
)
