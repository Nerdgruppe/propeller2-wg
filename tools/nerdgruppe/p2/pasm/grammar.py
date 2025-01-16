
PASM_GRAMMAR = r'''
    ?start: program

    program : eol* block*
    
    block : con_block 
          | dat_block

    eol: blockcomment* [linecomment] EOL

    linecomment : COMMENT
    blockcomment : "{" blockcomment "}"
                 | "{" [BLOCKCOMMENT_BODY] "}"

    con_block : "CON" eol con_line*
    dat_block : "DAT" eol dat_line*

    con_line : [con_instr] eol
    dat_line : [dat_instr] eol

    con_instr : common_instr
              | IDENT "=" expr

    dat_instr : common_instr
              | sequence
              | conditional
              | debug_instr

    common_instr : "#include" string
                 | "#ifdef" ident
                 | "#ifndef" ident
                 | "#elseifdef" ident
                 | "#else"
                 | "#endif" 
    
    debug_instr : "debug" "(" [arglist] ")"

    conditional : "if" expr
                | "end"

    sequence : ident+ arglist? ident*

    arglist : arg ("," arg)*

    arg    : expr

    expr   : number
           | ident
           | string
           | "$"
           | "#" expr
           | "@" expr
           | "\\" expr
           | "-" expr
           | "(" expr ")"
           | expr "[" expr "]"
           | expr "-" expr
           | expr "+" expr
           | expr "*" expr
           | expr "/" expr
           | expr ">>" expr
           | expr "<<" expr
           | expr "<" expr
           | expr ">" expr
           | expr "|" expr
           | expr "&" expr
           | ident ":" ident
           | ident "(" [arglist] ")"
           | ident "++"
           | expr "addpins" expr
           | expr "addbits" expr
           | expr "?" expr ":" expr

    ident  : IDENT
    string : STRING
    number : DEC_NUMBER
           | BIN_NUMBER
           | HEX_NUMBER
           | QUAD_NUMBER


    DEC_NUMBER   : /[0-9_]+/
    HEX_NUMBER   : /\$[0-9a-zA-Z_]+/
    BIN_NUMBER   : /\%[01_]+/
    QUAD_NUMBER   : /\%%[0123_]+/
    IDENT        : /\.?[A-Za-z_][A-Za-z0-9_]*/
    STRING       : /"(\\[^\n]|[^\"])*"/
    COMMENT      : /'[^\r\n]*/
    BLOCKCOMMENT_BODY : /[^}]+/
    EOL          : "\n"
    WS           : /[ \t]/

    %ignore WS

'''