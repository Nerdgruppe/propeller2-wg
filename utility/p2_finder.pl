%%% ===========================================================
%%% p2_finder.pl — Propeller 2 Instruction Sequence Finder
%%% A stripped-down superoptimizer sketch in SWI-Prolog
%%% ===========================================================
%%%
%%% Architecture overview:
%%%
%%%   Layer 1 — Symbolic bit domain
%%%     Per-bit values: 0, 1, bit(Reg,N), u(Id,N), compound terms
%%%     Register values: bv([B0..B31])  |  expr(Tree)
%%%     Flags: same domain as bits, plus c_of(...), z_of(...)
%%%
%%%   Layer 2 — Instruction semantics
%%%     15 P2 instructions modeled (MOV ADD SUB SUBR AND OR XOR
%%%     SHL SHR NOT NEG CMP TESTB MUXC ZEROX)
%%%     Flag modifiers: none / wc / wz / wcz
%%%
%%%   Layer 3 — Iterative-deepening search + goal matching
%%%     Structural unification for bitwise-only sequences
%%%     Concretization fallback for arithmetic expressions
%%%
%%% Conventions:
%%%   - Bitvectors are LSB-first: bv([B0, B1, ..., B31])
%%%   - bit(Reg, N) = bit N of named input register Reg at t=0
%%%   - u(Id, N)    = bit N of unknown temp register Id at t=0
%%%   - x in goals  = don't care (matches anything)

%%% ===========================================================
%%% USER MANUAL — How to write queries
%%% ===========================================================
%%%
%%% BASIC QUERY STRUCTURE
%%% ---------------------
%%% All queries use find_sequence/2:
%%%
%%%   find_sequence(
%%%       config{
%%%           inputs:  [a, b],        % named input registers
%%%           temps:   1,             % number of scratch registers (t0, t1, ...)
%%%           max_len: 3,             % max instruction count to try
%%%           max_imm: 31,            % max immediate value (0..511)
%%%           goal:    [Constraints], % what the output should look like
%%%           n_vectors: 50,          % (optional) random test vectors, default 50
%%%           flag_policy: full,      % (optional) override flag search policy
%%%           flag_tail: 2            % (optional) only last N instrs search flags
%%%       },
%%%       Seq                         % output: list of instructions
%%%   ).
%%%
%%% After getting Seq, pretty-print with:
%%%   print_sequence(Seq).
%%%
%%% To find ALL solutions (backtrack), use:
%%%   find_sequence(Config, Seq), print_sequence(Seq), fail ; true.
%%%
%%%
%%% REGISTERS
%%% ---------
%%% Registers come from two sources:
%%%
%%%   inputs: [a, b, mask]   →  Registers a, b, mask exist with symbolic
%%%                              initial values. You can reference their
%%%                              initial state in goals.
%%%
%%%   temps: 2               →  Registers t0, t1 exist with unknown initial
%%%                              values. Use these as scratch space for the
%%%                              search to write intermediate results into.
%%%
%%% IMPORTANT: A register must be in inputs OR created as a temp to be used.
%%% Writing `reg(b, ...)` in a goal when b is not in inputs will error.
%%%
%%%
%%% GOAL CONSTRAINTS
%%% ----------------
%%% The goal is a list of constraints. Each constraint is one of:
%%%
%%%   reg(Name, Expr)   — register Name must hold value described by Expr
%%%   c(Expr)           — C flag must equal Expr
%%%   z(Expr)           — Z flag must equal Expr
%%%
%%% EVERY register that exists must appear in the goal list.
%%% Use x (don't care) for registers/flags you don't care about.
%%%
%%%
%%% GOAL EXPRESSIONS (the Expr part)
%%% --------------------------------
%%% These describe what value you want. They get compiled against the
%%% initial state before the search begins.
%%%
%%%   BASIC VALUES:
%%%     x                 don't care — matches anything
%%%     0                 concrete bit zero  (for flags only: c(0), z(0))
%%%     1                 concrete bit one   (for flags only: c(1), z(1))
%%%     imm(V)            concrete 32-bit value as a bitvector
%%%                       Examples: imm(0), imm(1), imm(255), imm(0xFF)
%%%
%%%   REFERENCING INITIAL STATE:
%%%     in(a)             "the value register a had at the start"
%%%     in_c              "the C flag value at the start"
%%%     in_z              "the Z flag value at the start"
%%%     bit(a, 7)         "bit 7 of register a's initial value"
%%%                       (single bit — use in c() or z() goals)
%%%
%%%   BITWISE OPERATIONS ON GOALS:
%%%     not(G)            bitwise NOT of G
%%%     and(G1, G2)       bitwise AND
%%%     or(G1, G2)        bitwise OR
%%%     xor(G1, G2)       bitwise XOR
%%%     shl(G, N)         shift left by constant N  (N = 0..31)
%%%     shr(G, N)         shift right by constant N (N = 0..31)
%%%     zerox(G, N)       zero-extend above bit N
%%%
%%%   ARITHMETIC OPERATIONS ON GOALS:
%%%     add(G1, G2)       addition (validated by concrete test vectors)
%%%     sub(G1, G2)       subtraction
%%%     neg(G)            two's complement negation
%%%
%%%   COMPARISON OPERATORS (produce single-bit 0/1 result):
%%%     eq(G1, G2)        1 if G1 == G2, else 0  (unsigned)
%%%     neq(G1, G2)       1 if G1 != G2, else 0  (unsigned)
%%%     lt(G1, G2)        1 if G1 <  G2, else 0  (unsigned)
%%%     gte(G1, G2)       1 if G1 >= G2, else 0  (unsigned)
%%%     lts(G1, G2)       1 if G1 <  G2, else 0  (signed)
%%%     gtes(G1, G2)      1 if G1 >= G2, else 0  (signed)
%%%
%%%   Comparisons work on full 32-bit values. Use inside c() or z()
%%%   to express flag goals like "C = (A[31:24] == 0x01)":
%%%
%%%     c(eq(shr(in(a), 24), imm(1)))
%%%
%%%   They also work inside reg() if you want a 0/1 register result:
%%%
%%%     reg(t0, eq(in(a), in(b)))    % t0 = 1 if a==b, else 0
%%%
%%%   NOTE: Comparisons are always validated by concretization
%%%   (test vectors), never by structural matching. They cannot
%%%   be proven symbolically — only tested probabilistically.
%%%
%%%
%%% COMMON PATTERNS / COOKBOOK
%%% -------------------------
%%%
%%% "Zero register b":
%%%   reg(b, imm(0))
%%%
%%% "Copy a to b, leave a unchanged":
%%%   reg(a, in(a)), reg(b, in(a))
%%%
%%% "Swap a and b":
%%%   reg(a, in(b)), reg(b, in(a))
%%%
%%% "Invert all bits of a":
%%%   reg(a, not(in(a)))
%%%
%%% "a = a AND mask":
%%%   reg(a, and(in(a), in(mask)))
%%%
%%% "t0 = a + b*2":
%%%   reg(t0, add(in(a), shl(in(b), 1)))
%%%
%%% "C flag = bit 0 of a":
%%%   c(bit(a, 0))
%%%
%%% "Z flag = 0 (always clear)":
%%%   z(0)
%%%
%%% "Z flag = bit 31 of a":
%%%   z(bit(a, 31))
%%%
%%% "C = 1 if top byte of a is 0x01":
%%%   c(eq(shr(in(a), 24), imm(1)))
%%%
%%% "C = 1 if a < b (unsigned compare)":
%%%   c(lt(in(a), in(b)))
%%%
%%% "C = 1 if a >= 0 (signed, i.e. bit31 == 0)":
%%%   c(gtes(in(a), imm(0)))
%%%
%%% "C flag = whatever, I don't care":
%%%   c(x)
%%%
%%% "Register t0 = whatever, I don't care":
%%%   reg(t0, x)
%%%
%%%
%%% ERROR REPORTING
%%% ---------------
%%% The goal is validated before the search begins. If something
%%% is wrong, you get an error like:
%%%
%%%   *** GOAL ERROR ***
%%%   Register 'b' not found. Available: [a,t0]
%%%
%%%   *** GOAL ERROR ***
%%%   Unknown constraint: b(in(a))
%%%     Valid forms: reg(Name, Expr), c(Expr), z(Expr)
%%%     Did you forget reg(...)? e.g. write reg(b, in(a)) not b(in(a))
%%%
%%%   *** GOAL ERROR ***
%%%   Unknown goal expression: foo(42) (in reg(a))
%%%     Valid expressions:
%%%       x, 0, 1, in(Reg), in_c, in_z, imm(Value), bit(Reg,N)
%%%       not(E), and(E1,E2), or(E1,E2), xor(E1,E2)
%%%       shl(E,N), shr(E,N), zerox(E,N)
%%%       add(E1,E2), sub(E1,E2), neg(E)
%%%       eq(E1,E2), neq(E1,E2), lt(E1,E2), gte(E1,E2)
%%%       lts(E1,E2), gtes(E1,E2)
%%%
%%% Use find_sequence_safe/2 instead of find_sequence/2 if you
%%% want errors to be printed and fail gracefully instead of
%%% throwing an exception.
%%%
%%%
%%% COMPLETE EXAMPLE QUERIES
%%% ------------------------
%%%
%%% Find XOR-swap of a and b (3 instructions, no immediates):
%%%
%%%   find_sequence(
%%%       config{inputs: [a,b], temps: 0, max_len: 3, max_imm: 0,
%%%              goal: [reg(a, in(b)), reg(b, in(a)), c(x), z(x)]},
%%%       Seq),
%%%   print_sequence(Seq).
%%%
%%% Find "set Z = LSB of a" (1 instruction):
%%%
%%%   find_sequence(
%%%       config{inputs: [a], temps: 0, max_len: 1, max_imm: 31,
%%%              goal: [reg(a, in(a)), c(x), z(bit(a,0))]},
%%%       Seq),
%%%   print_sequence(Seq).
%%%
%%% Find "t0 = a + b*2" (3 instructions, allow shifts):
%%%
%%%   find_sequence(
%%%       config{inputs: [a,b], temps: 1, max_len: 3, max_imm: 1,
%%%              goal: [reg(t0, add(in(a), shl(in(b),1))),
%%%                     reg(a, x), reg(b, x), c(x), z(x)],
%%%              n_vectors: 100},
%%%       Seq),
%%%   print_sequence(Seq).
%%%
%%% Find "extract bit 7 to C, leave a unchanged":
%%%
%%%   find_sequence(
%%%       config{inputs: [a], temps: 0, max_len: 1, max_imm: 31,
%%%              goal: [reg(a, in(a)), c(bit(a,7)), z(x)]},
%%%       Seq),
%%%   print_sequence(Seq).
%%%
%%%
%%% FLAG POLICY / PERFORMANCE TIPS
%%% ------------------------------
%%% Flag modifiers (WC/WZ/WCZ) multiply the search space by ~4x.
%%% The search auto-detects when flags matter:
%%%
%%%   - If ALL flag goals are x     →  "minimal" policy: no flag ops,
%%%                                     no flag modifiers. Fastest.
%%%
%%%   - If SOME flags constrained   →  "tail(K)" policy: only the last
%%%                                     K instructions get full flag
%%%                                     enumeration. Earlier ones use minimal.
%%%
%%% Override with:
%%%   flag_policy: full       % all positions get all flag variants
%%%   flag_policy: minimal    % no flag ops anywhere
%%%   flag_tail: 1            % only last instruction gets flag search
%%%
%%% Reducing max_imm also helps a lot. Use max_imm: 0 when no
%%% immediates are needed, max_imm: 1 for shifts by 1, etc.
%%%
%%% ===========================================================

:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(random)).
:- use_module(library(aggregate)).

%%% ===========================================================
%%% SECTION 1: Symbolic Bit Simplification
%%% ===========================================================
%%% These operate on single symbolic bits.
%%% They apply basic Boolean identities eagerly to keep terms
%%% small. Anything that can't be simplified stays as a compound.

sym_and(0, _, 0) :- !.
sym_and(_, 0, 0) :- !.
sym_and(1, X, X) :- !.
sym_and(X, 1, X) :- !.
sym_and(X, X, X) :- !.
sym_and(A, B, sym_and(A, B)).

sym_or(1, _, 1) :- !.
sym_or(_, 1, 1) :- !.
sym_or(0, X, X) :- !.
sym_or(X, 0, X) :- !.
sym_or(X, X, X) :- !.
sym_or(A, B, sym_or(A, B)).

sym_xor(0, X, X) :- !.
sym_xor(X, 0, X) :- !.
sym_xor(1, 1, 0) :- !.
sym_xor(1, X, R) :- !, sym_not(X, R).
sym_xor(X, 1, R) :- !, sym_not(X, R).
sym_xor(X, X, 0) :- !.
sym_xor(A, B, sym_xor(A, B)).

sym_not(0, 1) :- !.
sym_not(1, 0) :- !.
sym_not(sym_not(X), X) :- !.
sym_not(A, sym_not(A)).

%%% Mux: if Sel=1 then TrueVal else FalseVal
sym_mux(1, T, _, T) :- !.
sym_mux(0, _, F, F) :- !.
sym_mux(S, T, F, R) :-
    %% mux(S,T,F) = (S & T) | (!S & F)
    sym_and(S, T, P1),
    sym_not(S, NS),
    sym_and(NS, F, P2),
    sym_or(P1, P2, R).

%%% ===========================================================
%%% SECTION 2: Bitvector Construction & Access
%%% ===========================================================

%% make_input_bv(+Name, -BV)
%%   Symbolic bitvector for a named input register.
make_input_bv(Name, bv(Bits)) :-
    numlist(0, 31, Ns),
    maplist({Name}/[N, bit(Name,N)]>>true, Ns, Bits).

%% make_imm_bv(+Value, -BV)
%%   Concrete bitvector for an immediate value (0..511 or any 32-bit).
make_imm_bv(Value, bv(Bits)) :-
    numlist(0, 31, Ns),
    maplist({Value}/[N, B]>>(B is (Value >> N) /\ 1), Ns, Bits).

%% make_unknown_bv(+Id, -BV)
%%   Bitvector with unknown bits (for temp registers).
make_unknown_bv(Id, bv(Bits)) :-
    numlist(0, 31, Ns),
    maplist({Id}/[N, u(Id,N)]>>true, Ns, Bits).

%% make_zero_bv(-BV)
make_zero_bv(bv(Bits)) :-
    length(Bits, 32),
    maplist(=(0), Bits).

%% bv_bit(+BV, +N, -Bit)
bv_bit(bv(Bits), N, Bit) :-
    nth0(N, Bits, Bit).

%% bv_all_concrete(+BV)
%%   True if every bit is 0 or 1.
bv_all_concrete(bv(Bits)) :-
    maplist([B]>>(B == 0 ; B == 1), Bits).

%% bv_to_integer(+BV, -Value)
%%   Convert a fully concrete bitvector to an integer.
bv_to_integer(bv(Bits), Value) :-
    bv_to_integer_(Bits, 0, 0, Value).

bv_to_integer_([], _, Acc, Acc).
bv_to_integer_([B|Bs], Pos, Acc, Val) :-
    NewAcc is Acc + B * (1 << Pos),
    Pos1 is Pos + 1,
    bv_to_integer_(Bs, Pos1, NewAcc, Val).

%%% ===========================================================
%%% SECTION 3: Bitvector Operations (bitwise, precise)
%%% ===========================================================

bv_and(bv(As), bv(Bs), bv(Rs)) :-
    maplist([A, B, R]>>sym_and(A, B, R), As, Bs, Rs).

bv_or(bv(As), bv(Bs), bv(Rs)) :-
    maplist([A, B, R]>>sym_or(A, B, R), As, Bs, Rs).

bv_xor(bv(As), bv(Bs), bv(Rs)) :-
    maplist([A, B, R]>>sym_xor(A, B, R), As, Bs, Rs).

bv_not(bv(As), bv(Rs)) :-
    maplist([A, R]>>sym_not(A, R), As, Rs).

%% bv_shl(+BV, +Amt, -Result, -CarryOut)
%%   Logical shift left by concrete amount 0..31.
%%   C = last bit shifted out (bit 32-Amt of original, or bit 31 if Amt=0).
bv_shl(bv(Bits), 0, bv(Bits), C) :-
    !,
    nth0(31, Bits, C).       % C = D[31] when shift=0
bv_shl(bv(Bits), Amt, bv(Result), C) :-
    integer(Amt), Amt >= 1, Amt =< 31,
    %% Insert Amt zeros at bottom, take first 32 bits
    length(Zeros, Amt),
    maplist(=(0), Zeros),
    append(Zeros, Bits, Extended),
    length(Result, 32),
    append(Result, _, Extended),
    %% C = last bit shifted out = original bit at position (32 - Amt)
    CIdx is 32 - Amt,
    nth0(CIdx, Bits, C).

%% bv_shr(+BV, +Amt, -Result, -CarryOut)
%%   Logical shift right by concrete amount 0..31.
%%   C = last bit shifted out (bit Amt-1 of original, or bit 0 if Amt=0).
bv_shr(bv(Bits), 0, bv(Bits), C) :-
    !,
    nth0(0, Bits, C).        % C = D[0] when shift=0
bv_shr(bv(Bits), Amt, bv(Result), C) :-
    integer(Amt), Amt >= 1, Amt =< 31,
    %% Drop Amt bits from bottom, pad with Amt zeros at top
    length(Drop, Amt),
    append(Drop, Remaining, Bits),
    length(Zeros, Amt),
    maplist(=(0), Zeros),
    append(Remaining, Zeros, Result),
    %% C = last bit shifted out = original bit at position (Amt - 1)
    CIdx is Amt - 1,
    nth0(CIdx, Bits, C).

%% bv_zerox(+BV, +BitPos, -Result)
%%   Zero-extend: keep bits 0..BitPos, set bits BitPos+1..31 to 0.
bv_zerox(bv(Bits), BitPos, bv(Result)) :-
    integer(BitPos), BitPos >= 0, BitPos =< 31,
    Keep is BitPos + 1,
    length(KeptBits, Keep),
    append(KeptBits, _, Bits),
    ZeroCount is 32 - Keep,
    length(Zeros, ZeroCount),
    maplist(=(0), Zeros),
    append(KeptBits, Zeros, Result).

%% bv_neg(+BV, -Result)
%%   Two's complement negation. Only precise when input is fully concrete.
%%   Otherwise produces expr(neg(BV)).
bv_neg(BV, Result) :-
    ( bv_all_concrete(BV)
    -> bv_to_integer(BV, V),
       NV is ((-V) /\ 0xFFFFFFFF),
       make_imm_bv(NV, Result)
    ;  Result = expr(neg(BV))
    ).

%%% Symbolic Z flag: is the result all-zeros?
symbolic_z_of_bv(bv(Bits), Z) :-
    ( maplist(==(0), Bits)
    -> Z = 1                        % definitely zero
    ; ( member(1, Bits)             % at least one known '1'
      -> Z = 0
      ;  Z = z_of(bv(Bits))        % can't tell — symbolic
      )
    ).

%%% Symbolic parity (for AND/OR/XOR C flag): XOR-reduction of all bits.
%%% We just leave it symbolic unless the result is fully concrete.
symbolic_parity(bv(Bits), C) :-
    ( maplist([B]>>(B == 0 ; B == 1), Bits)
    -> foldl([B, Acc, New]>>(New is Acc xor B), Bits, 0, C)
    ;  C = c_of(parity(bv(Bits)))
    ).

%%% ===========================================================
%%% SECTION 4: Value Coercion Helpers
%%% ===========================================================
%%% When a bitwise op receives an expr(...), we collapse bits to
%%% symbolic "extraction" terms.  When arithmetic receives a bv,
%%% we wrap it in an expression.

%% ensure_bv(+Val, -BV)
%%   Coerce to bitvector form. expr(...) produces opaque bit terms.
ensure_bv(bv(Bits), bv(Bits)) :- !.
ensure_bv(expr(E), bv(Bits)) :-
    numlist(0, 31, Ns),
    maplist({E}/[N, expr_bit(E, N)]>>true, Ns, Bits).

%% ensure_expr(+Val, -Expr)
%%   Coerce to expression form.
ensure_expr(expr(E), expr(E)) :- !.
ensure_expr(BV, expr(bv_word(BV))) :- BV = bv(_).

%%% ===========================================================
%%% SECTION 5: Machine State
%%% ===========================================================
%%% state(Regs, C, Z)
%%%   Regs: assoc RegName -> Value (bv or expr)
%%%   C, Z: symbolic bit values

make_state(Regs, C, Z, state(Regs, C, Z)).

get_reg(state(Regs, _, _), Name, Val) :-
    get_assoc(Name, Regs, Val).

set_reg(state(Regs, C, Z), Name, Val, state(Regs1, C, Z)) :-
    put_assoc(Name, Regs, Val, Regs1).

get_c(state(_, C, _), C).
get_z(state(_, _, Z), Z).

set_c(state(R, _, Z), C, state(R, C, Z)).
set_z(state(R, C, _), Z, state(R, C, Z)).
set_cz(state(R, _, _), C, Z, state(R, C, Z)).

%% init_state(+InputRegs, +TempCount, -State)
%%   Build initial symbolic state.
%%   Input registers get symbolic bitvectors bit(Name, N).
%%   Temp registers get unknown bitvectors u(Id, N).
%%   C and Z start as u(c_init) and u(z_init).
init_state(InputRegs, TempCount, State) :-
    %% Build temp register names: t0, t1, ...
    ( TempCount > 0
    -> numlist_(0, TempCount, TempNums),
       maplist([I, Name]>>(atom_concat(t, I, Name)), TempNums, TempNames)
    ;  TempNames = []
    ),
    %% Create register assoc
    append(InputRegs, TempNames, AllRegs),
    maplist({InputRegs}/[Name, Name-Val]>>(
        ( memberchk(Name, InputRegs)
        -> make_input_bv(Name, Val)
        ;  make_unknown_bv(Name, Val)
        )
    ), AllRegs, Pairs),
    list_to_assoc(Pairs, Regs),
    make_state(Regs, u(c_init), u(z_init), State).

numlist_(Start, Count, List) :-
    End is Start + Count - 1,
    ( Start =< End
    -> numlist(Start, End, List)
    ;  List = []
    ).

%% all_reg_names(+State, -Names)
all_reg_names(state(Regs, _, _), Names) :-
    assoc_to_keys(Regs, Names).

%%% ===========================================================
%%% SECTION 6: Instruction Operand Resolution
%%% ===========================================================

%% resolve_src(+Src, +State, -Value)
%%   reg(R)  -> look up R in state
%%   imm(V)  -> make concrete bitvector
resolve_src(reg(R), State, Val) :-
    get_reg(State, R, Val).
resolve_src(imm(V), _, Val) :-
    make_imm_bv(V, Val).

%% try_concrete_shift(+Val, -Amt)
%%   Try to extract a concrete shift amount (bits 4:0) from a value.
try_concrete_shift(bv(Bits), Amt) :-
    numlist(0, 4, Idxs),
    maplist({Bits}/[I, B]>>(nth0(I, Bits, B), (B == 0 ; B == 1)), Idxs, ShiftBits),
    bv_to_integer_(ShiftBits, 0, 0, Amt).
try_concrete_shift(expr(_), _) :- fail.

%%% ===========================================================
%%% SECTION 7: Instruction Semantics
%%% ===========================================================
%%% Each instruction computes:
%%%   (ResultVal, NewC, NewZ, WritesD)
%%% Then flag modifiers decide what gets written.

%% apply_flags(+FlagMod, +NewC, +NewZ, +OldC, +OldZ, -OutC, -OutZ)
apply_flags(none, _,    _,    C, Z, C, Z).
apply_flags(wc,   NewC, _,    _, Z, NewC, Z).
apply_flags(wz,   _,    NewZ, C, _, C, NewZ).
apply_flags(wcz,  NewC, NewZ, _, _, NewC, NewZ).

%% execute_instr(+Instr, +StateIn, -StateOut)
%%   Instr = instr(Op, D, Src, FlagMod)
execute_instr(instr(Op, D, Src, FlagMod), SIn, SOut) :-
    get_reg(SIn, D, DVal),
    resolve_src(Src, SIn, SVal),
    get_c(SIn, OldC),
    get_z(SIn, OldZ),
    compute(Op, DVal, SVal, OldC, OldZ, ResultVal, NewC, NewZ, WritesD),
    apply_flags(FlagMod, NewC, NewZ, OldC, OldZ, OutC, OutZ),
    ( WritesD == yes
    -> set_reg(SIn, D, ResultVal, S1)
    ;  S1 = SIn
    ),
    set_cz(S1, OutC, OutZ, SOut).

%% compute(+Op, +DVal, +SVal, +C, +Z, -Result, -NewC, -NewZ, -WritesD)

%%% --- MOV D, S ---
%%% D = S.  C = S[31].  Z = (S == 0).
compute(mov, _DVal, SVal, _C, _Z, Result, NewC, NewZ, yes) :-
    ensure_bv(SVal, BV),
    BV = bv(Bits),
    Result = BV,
    nth0(31, Bits, NewC),
    symbolic_z_of_bv(BV, NewZ).

%%% --- ADD D, S ---
%%% D = D + S.  C = carry.  Z = (result == 0).
compute(add, DVal, SVal, _C, _Z, Result, NewC, NewZ, yes) :-
    ensure_expr(DVal, expr(DE)),
    ensure_expr(SVal, expr(SE)),
    Result = expr(add(DE, SE)),
    NewC = c_of(add(DE, SE)),
    NewZ = z_of(add(DE, SE)).

%%% --- SUB D, S ---
%%% D = D - S.  C = borrow.  Z = (result == 0).
compute(sub, DVal, SVal, _C, _Z, Result, NewC, NewZ, yes) :-
    ensure_expr(DVal, expr(DE)),
    ensure_expr(SVal, expr(SE)),
    Result = expr(sub(DE, SE)),
    NewC = c_of(sub(DE, SE)),
    NewZ = z_of(sub(DE, SE)).

%%% --- SUBR D, S ---
%%% D = S - D.  C = borrow.  Z = (result == 0).
compute(subr, DVal, SVal, _C, _Z, Result, NewC, NewZ, yes) :-
    ensure_expr(DVal, expr(DE)),
    ensure_expr(SVal, expr(SE)),
    Result = expr(sub(SE, DE)),
    NewC = c_of(sub(SE, DE)),
    NewZ = z_of(sub(SE, DE)).

%%% --- AND D, S ---
%%% D = D & S.  C = parity of result.  Z = (result == 0).
compute(and, DVal, SVal, _C, _Z, Result, NewC, NewZ, yes) :-
    ensure_bv(DVal, DBV),
    ensure_bv(SVal, SBV),
    bv_and(DBV, SBV, Result),
    symbolic_parity(Result, NewC),
    symbolic_z_of_bv(Result, NewZ).

%%% --- OR D, S ---
compute(or, DVal, SVal, _C, _Z, Result, NewC, NewZ, yes) :-
    ensure_bv(DVal, DBV),
    ensure_bv(SVal, SBV),
    bv_or(DBV, SBV, Result),
    symbolic_parity(Result, NewC),
    symbolic_z_of_bv(Result, NewZ).

%%% --- XOR D, S ---
compute(xor, DVal, SVal, _C, _Z, Result, NewC, NewZ, yes) :-
    ensure_bv(DVal, DBV),
    ensure_bv(SVal, SBV),
    bv_xor(DBV, SBV, Result),
    symbolic_parity(Result, NewC),
    symbolic_z_of_bv(Result, NewZ).

%%% --- SHL D, S ---
%%% D = D << S[4:0].  C = last bit shifted out.  Z = (result == 0).
compute(shl, DVal, SVal, _C, _Z, Result, NewC, NewZ, yes) :-
    ensure_bv(DVal, DBV),
    ( try_concrete_shift(SVal, Amt)
    -> bv_shl(DBV, Amt, Result, NewC),
       symbolic_z_of_bv(Result, NewZ)
    ;  %% Non-concrete shift: fall back to expression
       ensure_expr(DVal, expr(DE)),
       ensure_expr(SVal, expr(SE)),
       Result = expr(shl(DE, SE)),
       NewC = c_of(shl(DE, SE)),
       NewZ = z_of(shl(DE, SE))
    ).

%%% --- SHR D, S ---
compute(shr, DVal, SVal, _C, _Z, Result, NewC, NewZ, yes) :-
    ensure_bv(DVal, DBV),
    ( try_concrete_shift(SVal, Amt)
    -> bv_shr(DBV, Amt, Result, NewC),
       symbolic_z_of_bv(Result, NewZ)
    ;  ensure_expr(DVal, expr(DE)),
       ensure_expr(SVal, expr(SE)),
       Result = expr(shr(DE, SE)),
       NewC = c_of(shr(DE, SE)),
       NewZ = z_of(shr(DE, SE))
    ).

%%% --- NOT D, S ---
%%% D = !S.  C = !S[31].  Z = (result == 0).
compute(not, _DVal, SVal, _C, _Z, Result, NewC, NewZ, yes) :-
    ensure_bv(SVal, SBV),
    bv_not(SBV, Result),
    Result = bv(Bits),
    nth0(31, Bits, NewC),
    symbolic_z_of_bv(Result, NewZ).

%%% --- NEG D, S ---
%%% D = -S.  C = MSB of result.  Z = (result == 0).
compute(neg, _DVal, SVal, _C, _Z, Result, NewC, NewZ, yes) :-
    ensure_bv(SVal, SBV),
    bv_neg(SBV, Result),
    ( Result = bv(Bits)
    -> nth0(31, Bits, NewC),
       symbolic_z_of_bv(Result, NewZ)
    ;  %% expr fallback
       ensure_expr(SVal, expr(SE)),
       NewC = c_of(neg(SE)),
       NewZ = z_of(neg(SE))
    ).

%%% --- CMP D, S ---
%%% No write to D.  C = borrow of (D - S).  Z = (D == S).
compute(cmp, DVal, SVal, _C, _Z, _Result, NewC, NewZ, no) :-
    ensure_expr(DVal, expr(DE)),
    ensure_expr(SVal, expr(SE)),
    NewC = c_of(sub(DE, SE)),
    NewZ = z_of(sub(DE, SE)).

%%% --- TESTB D, #S  (WC or WZ) ---
%%% Flag = D[S[4:0]].  No write to D.
%%% Note: FlagMod determines whether C or Z is written; both get
%%% the same computed value here. apply_flags routes it correctly.
compute(testb, DVal, SVal, _C, _Z, _Result, BitVal, BitVal, no) :-
    ensure_bv(DVal, DBV),
    ( try_concrete_shift(SVal, BitPos)
    -> bv_bit(DBV, BitPos, BitVal)
    ;  BitVal = bit_of(DVal, SVal)     % opaque if S not concrete
    ).

%%% --- MUXC D, S ---
%%% D = (!S & D) | (S & {32{C}}).
%%% For each bit i: if S[i]=1 then C else D[i].
%%% C = parity of result.  Z = (result == 0).
compute(muxc, DVal, SVal, C, _Z, Result, NewC, NewZ, yes) :-
    ensure_bv(DVal, bv(DBits)),
    ensure_bv(SVal, bv(SBits)),
    maplist({C}/[SB, DB, RB]>>sym_mux(SB, C, DB, RB), SBits, DBits, RBits),
    Result = bv(RBits),
    symbolic_parity(Result, NewC),
    symbolic_z_of_bv(Result, NewZ).

%%% --- ZEROX D, #S ---
%%% Zero-extend D above bit S[4:0].
%%% C = MSB of result.  Z = (result == 0).
compute(zerox, DVal, SVal, _C, _Z, Result, NewC, NewZ, yes) :-
    ensure_bv(DVal, DBV),
    ( try_concrete_shift(SVal, BitPos)
    -> bv_zerox(DBV, BitPos, Result),
       Result = bv(RBits),
       nth0(31, RBits, NewC),
       symbolic_z_of_bv(Result, NewZ)
    ;  ensure_expr(DVal, expr(DE)),
       Result = expr(zerox(DE, SVal)),
       NewC = c_of(zerox(DE, SVal)),
       NewZ = z_of(zerox(DE, SVal))
    ).

%%% ===========================================================
%%% SECTION 8: Instruction Table & Enumeration
%%% ===========================================================

%% p2_op_info(Op, WritesD, SrcType, FlagType)
%%   WritesD:  yes | no
%%   SrcType:  any | bit_pos
%%   FlagType: optional | required
p2_op_info(mov,   yes, any,     optional).
p2_op_info(add,   yes, any,     optional).
p2_op_info(sub,   yes, any,     optional).
p2_op_info(subr,  yes, any,     optional).
p2_op_info(and,   yes, any,     optional).
p2_op_info(or,    yes, any,     optional).
p2_op_info(xor,   yes, any,     optional).
p2_op_info(shl,   yes, any,     optional).
p2_op_info(shr,   yes, any,     optional).
p2_op_info(not,   yes, any,     optional).
p2_op_info(neg,   yes, any,     optional).
p2_op_info(cmp,   no,  any,     required).
p2_op_info(testb, no,  bit_pos, required).
p2_op_info(muxc,  yes, any,     optional).
p2_op_info(zerox, yes, bit_pos, optional).

%% candidate_instr(+RegNames, +MaxImm, +FlagPolicy, -Instr)
%%   FlagPolicy = full | minimal
%%     full    — enumerate all valid flag mods
%%     minimal — skip flag-only ops, restrict to flagmod=none
%%               (safe when goal has c(x) and z(x) and MUXC is not needed)
candidate_instr(RegNames, MaxImm, FlagPolicy, instr(Op, D, Src, FlagMod)) :-
    p2_op_info(Op, WritesD, SrcType, FlagType),
    %% Under minimal policy, skip instructions that only set flags
    ( FlagPolicy == minimal, WritesD == no -> fail ; true ),
    member(D, RegNames),
    valid_src(SrcType, RegNames, MaxImm, Src),
    valid_flagmod(FlagType, FlagPolicy, FlagMod),
    prune_ok(Op, D, Src, FlagMod).

valid_src(any, RegNames, _MaxImm, reg(R)) :- member(R, RegNames).
valid_src(any, _RegNames, MaxImm, imm(V)) :- between(0, MaxImm, V).
valid_src(bit_pos, _RegNames, _, imm(V)) :- between(0, 31, V).

%% valid_flagmod(+FlagType, +FlagPolicy, -FlagMod)
valid_flagmod(optional, minimal, none) :- !.
valid_flagmod(optional, full, none).
valid_flagmod(optional, full, wc).
valid_flagmod(optional, full, wz).
valid_flagmod(optional, full, wcz).
valid_flagmod(required, _, wc).
valid_flagmod(required, _, wz).
valid_flagmod(required, _, wcz).

%% prune_ok(+Op, +D, +Src, +FlagMod)
%%   Fail for obviously useless instructions.
prune_ok(mov, D, reg(S), none) :- !, D \= S.   % MOV R,R without flags = NOP
prune_ok(or, D, reg(S), none) :- !, D \= S.     % OR R,R without flags = NOP
prune_ok(and, D, reg(S), none) :- !, D \= S.    % AND R,R without flags = NOP
prune_ok(add, _, imm(0), none) :- !, fail.       % ADD R,#0 none = NOP
prune_ok(sub, _, imm(0), none) :- !, fail.       % SUB R,#0 none = NOP
prune_ok(shl, _, imm(0), none) :- !, fail.       % SHL R,#0 none = NOP
prune_ok(shr, _, imm(0), none) :- !, fail.       % SHR R,#0 none = NOP
prune_ok(or, _, imm(0), none) :- !, fail.        % OR R,#0 none = NOP
prune_ok(xor, _, imm(0), none) :- !, fail.       % XOR R,#0 none = NOP
prune_ok(_, _, _, _).

%%% ===========================================================
%%% SECTION 9: Sequence Execution
%%% ===========================================================

%% execute_sequence(+Instrs, +StateIn, -StateOut)
execute_sequence([], S, S).
execute_sequence([I|Is], SIn, SOut) :-
    execute_instr(I, SIn, SMid),
    execute_sequence(Is, SMid, SOut).

%%% ===========================================================
%%% SECTION 10: Goal Matching
%%% ===========================================================
%%% bit_match(+Computed, +Goal)
%%%   Succeeds if computed value satisfies goal constraint.
%%%   x = don't care, otherwise structural equality.

bit_match(_, x) :- !.
bit_match(V, V) :- !.
%% Fallback: structurally equal terms
bit_match(A, B) :- A == B.

%% val_match(+ComputedVal, +GoalVal)
%%   Match register-level values. x = don't care.
val_match(_, x) :- !.
val_match(bv(As), bv(Bs)) :- !,
    maplist(bit_match, As, Bs).
val_match(A, B) :- A == B.

%%% ===========================================================
%%% SECTION 11: Concretization & Validation
%%% ===========================================================
%%% For goals involving expr(...) terms, we fall back to
%%% concrete multi-vector testing.

%% collect_bit_symbols(+Term, -Symbols)
%%   Walk a term tree and collect all bit(Reg,N) and u(Id,N) atoms.
collect_bit_symbols(Term, Symbols) :-
    collect_bit_symbols_(Term, Bag, []),
    sort(Bag, Symbols).

collect_bit_symbols_(bit(R,N)) --> !, [bit(R,N)].
collect_bit_symbols_(u(Id,N)) --> !, [u(Id,N)].
collect_bit_symbols_(Term) -->
    { compound(Term), !, Term =.. [_|Args] },
    collect_bit_symbols_list_(Args).
collect_bit_symbols_(_) --> [].   % atoms, numbers

collect_bit_symbols_list_([]) --> [].
collect_bit_symbols_list_([H|T]) -->
    collect_bit_symbols_(H),
    collect_bit_symbols_list_(T).

%% random_bindings(+Symbols, -Bindings)
%%   Assign random 0/1 to each symbol. Returns assoc.
random_bindings(Symbols, Bindings) :-
    maplist([Sym, Sym-V]>>(random_between(0, 1, V)), Symbols, Pairs),
    list_to_assoc(Pairs, Bindings).

%% concretize_bit(+SymBit, +Bindings, -ConcreteBit)
concretize_bit(0, _, 0) :- !.
concretize_bit(1, _, 1) :- !.
concretize_bit(bit(R,N), Bindings, V) :- !,
    get_assoc(bit(R,N), Bindings, V).
concretize_bit(u(Id,N), Bindings, V) :- !,
    get_assoc(u(Id,N), Bindings, V).
concretize_bit(sym_and(A,B), Bindings, V) :- !,
    concretize_bit(A, Bindings, VA),
    concretize_bit(B, Bindings, VB),
    V is VA /\ VB.
concretize_bit(sym_or(A,B), Bindings, V) :- !,
    concretize_bit(A, Bindings, VA),
    concretize_bit(B, Bindings, VB),
    V is VA \/ VB.
concretize_bit(sym_xor(A,B), Bindings, V) :- !,
    concretize_bit(A, Bindings, VA),
    concretize_bit(B, Bindings, VB),
    V is VA xor VB.
concretize_bit(sym_not(A), Bindings, V) :- !,
    concretize_bit(A, Bindings, VA),
    V is 1 - VA.
concretize_bit(expr_bit(E, N), Bindings, V) :- !,
    concretize_val(expr(E), Bindings, Word),
    V is (Word >> N) /\ 1.
concretize_bit(bit_of(DVal, SVal), Bindings, V) :- !,
    concretize_val(DVal, Bindings, DWord),
    concretize_val(SVal, Bindings, SWord),
    BitPos is SWord /\ 31,
    V is (DWord >> BitPos) /\ 1.
%% Goal comparison terms (from eq/neq/lt/gte/lts/gtes)
concretize_bit(goal_cmp(Op, V1, V2), Bindings, Bit) :- !,
    concretize_val(V1, Bindings, W1),
    concretize_val(V2, Bindings, W2),
    eval_cmp(Op, W1, W2, Bit).
%% Flag terms
concretize_bit(c_of(Expr), Bindings, V) :- !,
    concretize_flag_c(Expr, Bindings, V).
concretize_bit(z_of(Expr), Bindings, V) :- !,
    concretize_flag_z(Expr, Bindings, V).
concretize_bit(c_of(parity(BV)), Bindings, V) :- !,
    concretize_val(BV, Bindings, Word),
    popcount_parity(Word, V).

%% concretize_val(+Value, +Bindings, -Word)
%%   Evaluate a register value to a concrete 32-bit integer.
concretize_val(bv(Bits), Bindings, Word) :- !,
    maplist({Bindings}/[B, CB]>>concretize_bit(B, Bindings, CB), Bits, CBits),
    bv_to_integer_(CBits, 0, 0, Word).
concretize_val(expr(E), Bindings, Word) :- !,
    concretize_expr(E, Bindings, Word).
concretize_val(goal_cmp(Op, V1, V2), Bindings, Word) :- !,
    concretize_val(V1, Bindings, W1),
    concretize_val(V2, Bindings, W2),
    eval_cmp(Op, W1, W2, Word).

%% concretize_expr(+Expr, +Bindings, -Word)
concretize_expr(add(A, B), Bindings, W) :- !,
    concretize_expr(A, Bindings, WA),
    concretize_expr(B, Bindings, WB),
    W is (WA + WB) /\ 0xFFFFFFFF.
concretize_expr(sub(A, B), Bindings, W) :- !,
    concretize_expr(A, Bindings, WA),
    concretize_expr(B, Bindings, WB),
    W is (WA - WB) /\ 0xFFFFFFFF.
concretize_expr(neg(A), Bindings, W) :- !,
    concretize_expr(A, Bindings, WA),
    W is ((-WA) /\ 0xFFFFFFFF).
concretize_expr(shl(A, B), Bindings, W) :- !,
    concretize_expr(A, Bindings, WA),
    concretize_expr(B, Bindings, WB),
    Amt is WB /\ 31,
    W is (WA << Amt) /\ 0xFFFFFFFF.
concretize_expr(shr(A, B), Bindings, W) :- !,
    concretize_expr(A, Bindings, WA),
    concretize_expr(B, Bindings, WB),
    Amt is WB /\ 31,
    W is WA >> Amt.
concretize_expr(zerox(A, S), Bindings, W) :- !,
    concretize_expr(A, Bindings, WA),
    concretize_val(S, Bindings, SV),
    BitPos is SV /\ 31,
    Mask is (1 << (BitPos + 1)) - 1,
    W is WA /\ Mask.
concretize_expr(bv_word(BV), Bindings, W) :- !,
    concretize_val(BV, Bindings, W).

%% Concrete flag computation
concretize_flag_c(add(A, B), Bindings, C) :- !,
    concretize_expr(A, Bindings, WA),
    concretize_expr(B, Bindings, WB),
    Sum is WA + WB,
    ( Sum > 0xFFFFFFFF -> C = 1 ; C = 0 ).
concretize_flag_c(sub(A, B), Bindings, C) :- !,
    concretize_expr(A, Bindings, WA),
    concretize_expr(B, Bindings, WB),
    ( WA < WB -> C = 1 ; C = 0 ).    % borrow
concretize_flag_c(neg(A), Bindings, C) :- !,
    concretize_expr(A, Bindings, WA),
    NV is ((-WA) /\ 0xFFFFFFFF),
    C is (NV >> 31) /\ 1.             % MSB of result
concretize_flag_c(parity(BV), Bindings, C) :- !,
    concretize_val(BV, Bindings, Word),
    popcount_parity(Word, C).
concretize_flag_c(shl(A, B), Bindings, C) :- !,
    concretize_expr(A, Bindings, WA),
    concretize_expr(B, Bindings, WB),
    Amt is WB /\ 31,
    ( Amt > 0
    -> CIdx is 32 - Amt,
       C is (WA >> CIdx) /\ 1
    ;  C is (WA >> 31) /\ 1
    ).
concretize_flag_c(shr(A, B), Bindings, C) :- !,
    concretize_expr(A, Bindings, WA),
    concretize_expr(B, Bindings, WB),
    Amt is WB /\ 31,
    ( Amt > 0
    -> CIdx is Amt - 1,
       C is (WA >> CIdx) /\ 1
    ;  C is WA /\ 1
    ).

concretize_flag_z(Expr, Bindings, Z) :-
    concretize_expr(Expr, Bindings, W),
    Masked is W /\ 0xFFFFFFFF,
    ( Masked =:= 0 -> Z = 1 ; Z = 0 ).

popcount_parity(Word, Parity) :-
    popcount_parity_(Word, 32, 0, Parity).
popcount_parity_(_, 0, Acc, Acc) :- !.
popcount_parity_(W, N, Acc, Par) :-
    Bit is W /\ 1,
    NewAcc is Acc xor Bit,
    W1 is W >> 1,
    N1 is N - 1,
    popcount_parity_(W1, N1, NewAcc, Par).

%% eval_cmp(+Op, +W1, +W2, -Bit)
%%   Evaluate comparison operators on 32-bit unsigned words.
%%   Returns 1 if true, 0 if false.
eval_cmp(eq,   W1, W2, B) :- ( W1 =:= W2                     -> B = 1 ; B = 0 ).
eval_cmp(neq,  W1, W2, B) :- ( W1 =\= W2                     -> B = 1 ; B = 0 ).
eval_cmp(lt,   W1, W2, B) :- ( W1 < W2                        -> B = 1 ; B = 0 ).
eval_cmp(gte,  W1, W2, B) :- ( W1 >= W2                       -> B = 1 ; B = 0 ).
eval_cmp(lts,  W1, W2, B) :- to_signed(W1,S1), to_signed(W2,S2),
                              ( S1 < S2                        -> B = 1 ; B = 0 ).
eval_cmp(gtes, W1, W2, B) :- to_signed(W1,S1), to_signed(W2,S2),
                              ( S1 >= S2                       -> B = 1 ; B = 0 ).

%% to_signed(+Unsigned32, -Signed)
to_signed(W, S) :-
    ( W >= 0x80000000
    -> S is W - 0x100000000
    ;  S = W
    ).

%%% ===========================================================
%%% SECTION 12: Goal Expression Compiler & Checking
%%% ===========================================================
%%% Goal values are expressions that get compiled against the
%%% initial state into symbolic values.
%%%
%%% Supported goal expressions:
%%%   x                   — don't care
%%%   in(Reg)             — initial value of register Reg
%%%   in_c / in_z         — initial C/Z flag value
%%%   imm(V)              — concrete 32-bit value
%%%   bit(Reg, N)         — single symbolic bit (for flag goals)
%%%   not(G)              — bitwise NOT
%%%   and(G1, G2)         — bitwise AND
%%%   or(G1, G2)          — bitwise OR
%%%   xor(G1, G2)         — bitwise XOR
%%%   shl(G, N)           — shift left by constant N
%%%   shr(G, N)           — shift right by constant N
%%%   zerox(G, N)         — zero-extend above bit N
%%%   add(G1, G2)         — arithmetic add  (produces expr tree)
%%%   sub(G1, G2)         — arithmetic sub  (produces expr tree)
%%%   neg(G)              — arithmetic negate (produces expr tree)
%%%   eq(G1, G2)          — 1 if G1 == G2, else 0  (bit result)
%%%   neq(G1, G2)         — 1 if G1 != G2, else 0  (bit result)
%%%   lt(G1, G2)          — 1 if G1 <  G2 unsigned, else 0
%%%   gte(G1, G2)         — 1 if G1 >= G2 unsigned, else 0
%%%   lts(G1, G2)         — 1 if G1 <  G2 signed,   else 0
%%%   gtes(G1, G2)        — 1 if G1 >= G2 signed,   else 0
%%%   bv(...) / expr(...)  — passthrough (already compiled)

%% -------------------------------------------------------
%% Goal Validation — run BEFORE search, gives clear errors
%% -------------------------------------------------------

%% validate_goal(+Goal, +InitState)
%%   Check that every constraint is well-formed and all referenced
%%   registers exist. Throws informative errors on failure.
validate_goal(Goal, InitState) :-
    ( is_list(Goal) -> true
    ; throw(goal_error("Goal must be a list of constraints, got: ~w", [Goal]))
    ),
    all_reg_names(InitState, RegNames),
    maplist({InitState, RegNames}/[C]>>validate_constraint(C, InitState, RegNames), Goal).

validate_constraint(reg(Name, Expr), InitState, RegNames) :- !,
    ( atom(Name) -> true
    ; throw(goal_error("Register name must be an atom, got: ~w", [Name]))
    ),
    ( member(Name, RegNames) -> true
    ; throw(goal_error("Register '~w' not found. Available: ~w", [Name, RegNames]))
    ),
    validate_expr(Expr, InitState, RegNames, reg(Name)).
validate_constraint(c(Expr), InitState, RegNames) :- !,
    validate_expr(Expr, InitState, RegNames, c).
validate_constraint(z(Expr), InitState, RegNames) :- !,
    validate_expr(Expr, InitState, RegNames, z).
validate_constraint(Bad, _, _) :-
    throw(goal_error(
        "Unknown constraint: ~w~n  Valid forms: reg(Name, Expr), c(Expr), z(Expr)~n  Did you forget reg(...)? e.g. write reg(b, in(a)) not b(in(a))",
        [Bad])).

%% validate_expr(+Expr, +InitState, +RegNames, +Context)
%%   Context = reg(Name) | c | z   — for error messages.
validate_expr(x, _, _, _) :- !.
validate_expr(0, _, _, _) :- !.
validate_expr(1, _, _, _) :- !.
validate_expr(in_c, _, _, _) :- !.
validate_expr(in_z, _, _, _) :- !.
validate_expr(in(R), _, RegNames, Ctx) :- !,
    ( atom(R) -> true
    ; throw(goal_error("in(~w): register name must be an atom", [R]))
    ),
    ( member(R, RegNames) -> true
    ; throw(goal_error("in(~w): register '~w' not found in ~w (in ~w)",
                        [R, R, RegNames, Ctx]))
    ).
validate_expr(imm(V), _, _, Ctx) :- !,
    ( integer(V) -> true
    ; throw(goal_error("imm(~w): value must be an integer (in ~w)", [V, Ctx]))
    ).
validate_expr(bit(R, N), _, RegNames, Ctx) :- !,
    ( atom(R) -> true
    ; throw(goal_error("bit(~w,~w): register name must be an atom (in ~w)",
                        [R, N, Ctx]))
    ),
    ( member(R, RegNames) -> true
    ; throw(goal_error("bit(~w,~w): register '~w' not found in ~w (in ~w)",
                        [R, N, R, RegNames, Ctx]))
    ),
    ( integer(N), N >= 0, N =< 31 -> true
    ; throw(goal_error("bit(~w,~w): bit index must be 0..31 (in ~w)",
                        [R, N, Ctx]))
    ).
%% Unary operators
validate_expr(not(G), Init, Regs, Ctx) :- !,
    validate_expr(G, Init, Regs, Ctx).
validate_expr(neg(G), Init, Regs, Ctx) :- !,
    validate_expr(G, Init, Regs, Ctx).
%% Binary operators (bitwise)
validate_expr(and(G1,G2), Init, Regs, Ctx) :- !,
    validate_expr(G1, Init, Regs, Ctx),
    validate_expr(G2, Init, Regs, Ctx).
validate_expr(or(G1,G2), Init, Regs, Ctx) :- !,
    validate_expr(G1, Init, Regs, Ctx),
    validate_expr(G2, Init, Regs, Ctx).
validate_expr(xor(G1,G2), Init, Regs, Ctx) :- !,
    validate_expr(G1, Init, Regs, Ctx),
    validate_expr(G2, Init, Regs, Ctx).
%% Binary operators (arithmetic)
validate_expr(add(G1,G2), Init, Regs, Ctx) :- !,
    validate_expr(G1, Init, Regs, Ctx),
    validate_expr(G2, Init, Regs, Ctx).
validate_expr(sub(G1,G2), Init, Regs, Ctx) :- !,
    validate_expr(G1, Init, Regs, Ctx),
    validate_expr(G2, Init, Regs, Ctx).
%% Shift/extend with constant
validate_expr(shl(G,N), Init, Regs, Ctx) :- !,
    ( integer(N), N >= 0, N =< 31 -> true
    ; throw(goal_error("shl(_,~w): shift amount must be integer 0..31 (in ~w)",
                        [N, Ctx]))
    ),
    validate_expr(G, Init, Regs, Ctx).
validate_expr(shr(G,N), Init, Regs, Ctx) :- !,
    ( integer(N), N >= 0, N =< 31 -> true
    ; throw(goal_error("shr(_,~w): shift amount must be integer 0..31 (in ~w)",
                        [N, Ctx]))
    ),
    validate_expr(G, Init, Regs, Ctx).
validate_expr(zerox(G,N), Init, Regs, Ctx) :- !,
    ( integer(N), N >= 0, N =< 31 -> true
    ; throw(goal_error("zerox(_,~w): bit position must be integer 0..31 (in ~w)",
                        [N, Ctx]))
    ),
    validate_expr(G, Init, Regs, Ctx).
%% Comparison operators (produce single-bit result)
validate_expr(eq(G1,G2), Init, Regs, Ctx) :- !,
    validate_expr(G1, Init, Regs, Ctx),
    validate_expr(G2, Init, Regs, Ctx).
validate_expr(neq(G1,G2), Init, Regs, Ctx) :- !,
    validate_expr(G1, Init, Regs, Ctx),
    validate_expr(G2, Init, Regs, Ctx).
validate_expr(lt(G1,G2), Init, Regs, Ctx) :- !,
    validate_expr(G1, Init, Regs, Ctx),
    validate_expr(G2, Init, Regs, Ctx).
validate_expr(gte(G1,G2), Init, Regs, Ctx) :- !,
    validate_expr(G1, Init, Regs, Ctx),
    validate_expr(G2, Init, Regs, Ctx).
validate_expr(lts(G1,G2), Init, Regs, Ctx) :- !,
    validate_expr(G1, Init, Regs, Ctx),
    validate_expr(G2, Init, Regs, Ctx).
validate_expr(gtes(G1,G2), Init, Regs, Ctx) :- !,
    validate_expr(G1, Init, Regs, Ctx),
    validate_expr(G2, Init, Regs, Ctx).
% Passthrough
validate_expr(bv(_), _, _, _) :- !.
validate_expr(expr(_), _, _, _) :- !.
%% Catch-all: unknown expression
validate_expr(Bad, _, _, Ctx) :-
    throw(goal_error(
        "Unknown goal expression: ~w (in ~w)~n  Valid expressions:~n    x, 0, 1, in(Reg), in_c, in_z, imm(Value), bit(Reg,N)~n    not(E), and(E1,E2), or(E1,E2), xor(E1,E2)~n    shl(E,N), shr(E,N), zerox(E,N)~n    add(E1,E2), sub(E1,E2), neg(E)~n    eq(E1,E2), neq(E1,E2), lt(E1,E2), gte(E1,E2)~n    lts(E1,E2), gtes(E1,E2)",
        [Bad, Ctx])).

%% -------------------------------------------------------
%% Goal Expression Compiler
%% -------------------------------------------------------

%% compile_goal_val(+GoalExpr, +InitState, -Value)
compile_goal_val(x, _, x) :- !.
compile_goal_val(in(R), Init, Val) :- !,
    get_reg(Init, R, Val).
compile_goal_val(in_c, Init, C) :- !,
    get_c(Init, C).
compile_goal_val(in_z, Init, Z) :- !,
    get_z(Init, Z).
compile_goal_val(imm(V), _, Val) :- !,
    make_imm_bv(V, Val).
compile_goal_val(bit(R, N), _, bit(R, N)) :- !.
compile_goal_val(not(G), Init, Val) :- !,
    compile_goal_val(G, Init, V),
    ensure_bv(V, BV),
    bv_not(BV, Val).
compile_goal_val(and(G1, G2), Init, Val) :- !,
    compile_goal_val(G1, Init, V1),
    compile_goal_val(G2, Init, V2),
    ensure_bv(V1, BV1), ensure_bv(V2, BV2),
    bv_and(BV1, BV2, Val).
compile_goal_val(or(G1, G2), Init, Val) :- !,
    compile_goal_val(G1, Init, V1),
    compile_goal_val(G2, Init, V2),
    ensure_bv(V1, BV1), ensure_bv(V2, BV2),
    bv_or(BV1, BV2, Val).
compile_goal_val(xor(G1, G2), Init, Val) :- !,
    compile_goal_val(G1, Init, V1),
    compile_goal_val(G2, Init, V2),
    ensure_bv(V1, BV1), ensure_bv(V2, BV2),
    bv_xor(BV1, BV2, Val).
compile_goal_val(shl(G, N), Init, Val) :- !,
    compile_goal_val(G, Init, V),
    ensure_bv(V, BV),
    bv_shl(BV, N, Val, _).
compile_goal_val(shr(G, N), Init, Val) :- !,
    compile_goal_val(G, Init, V),
    ensure_bv(V, BV),
    bv_shr(BV, N, Val, _).
compile_goal_val(zerox(G, N), Init, Val) :- !,
    compile_goal_val(G, Init, V),
    ensure_bv(V, BV),
    bv_zerox(BV, N, Val).
compile_goal_val(add(G1, G2), Init, Val) :- !,
    compile_goal_val(G1, Init, V1),
    compile_goal_val(G2, Init, V2),
    ensure_expr(V1, expr(E1)), ensure_expr(V2, expr(E2)),
    Val = expr(add(E1, E2)).
compile_goal_val(sub(G1, G2), Init, Val) :- !,
    compile_goal_val(G1, Init, V1),
    compile_goal_val(G2, Init, V2),
    ensure_expr(V1, expr(E1)), ensure_expr(V2, expr(E2)),
    Val = expr(sub(E1, E2)).
compile_goal_val(neg(G), Init, Val) :- !,
    compile_goal_val(G, Init, V),
    ensure_expr(V, expr(E)),
    Val = expr(neg(E)).
%% --- Comparison operators ---
%% These produce a single-bit value (0 or 1), suitable for c()/z() goals.
%% They compile into goal_cmp(...) terms evaluated during concretization.
compile_goal_val(eq(G1, G2), Init, Val) :- !,
    compile_goal_val(G1, Init, V1),
    compile_goal_val(G2, Init, V2),
    Val = goal_cmp(eq, V1, V2).
compile_goal_val(neq(G1, G2), Init, Val) :- !,
    compile_goal_val(G1, Init, V1),
    compile_goal_val(G2, Init, V2),
    Val = goal_cmp(neq, V1, V2).
compile_goal_val(lt(G1, G2), Init, Val) :- !,
    compile_goal_val(G1, Init, V1),
    compile_goal_val(G2, Init, V2),
    Val = goal_cmp(lt, V1, V2).
compile_goal_val(gte(G1, G2), Init, Val) :- !,
    compile_goal_val(G1, Init, V1),
    compile_goal_val(G2, Init, V2),
    Val = goal_cmp(gte, V1, V2).
compile_goal_val(lts(G1, G2), Init, Val) :- !,
    compile_goal_val(G1, Init, V1),
    compile_goal_val(G2, Init, V2),
    Val = goal_cmp(lts, V1, V2).
compile_goal_val(gtes(G1, G2), Init, Val) :- !,
    compile_goal_val(G1, Init, V1),
    compile_goal_val(G2, Init, V2),
    Val = goal_cmp(gtes, V1, V2).
% Passthrough for already-compiled values
compile_goal_val(Val, _, Val) :- Val = bv(_), !.
compile_goal_val(Val, _, Val) :- Val = expr(_), !.
%% Bare atoms 0, 1 for flag goals
compile_goal_val(0, _, 0) :- !.
compile_goal_val(1, _, 1) :- !.
%% Catch-all — should have been caught by validate, but just in case
compile_goal_val(Bad, _, _) :-
    throw(goal_error("Cannot compile goal expression: ~w", [Bad])).

check_goal(Constraints, InitState, FinalState) :-
    check_goal(Constraints, InitState, FinalState, 50).

check_goal(Constraints, InitState, FinalState, NVectors) :-
    resolve_goal_constraints(Constraints, InitState, Resolved),
    ( try_structural_match(Resolved, FinalState)
    -> true
    ;  validate_concrete(Resolved, FinalState, NVectors)
    ).

%% resolve_goal_constraints — now uses compile_goal_val
resolve_goal_constraints([], _, []).
resolve_goal_constraints([C|Cs], Init, [R|Rs]) :-
    resolve_one_constraint(C, Init, R),
    resolve_goal_constraints(Cs, Init, Rs).

resolve_one_constraint(reg(Name, GoalExpr), Init, reg(Name, Val)) :- !,
    compile_goal_val(GoalExpr, Init, Val).
resolve_one_constraint(c(GoalExpr), Init, c(Val)) :- !,
    compile_goal_val(GoalExpr, Init, Val).
resolve_one_constraint(z(GoalExpr), Init, z(Val)) :- !,
    compile_goal_val(GoalExpr, Init, Val).

%% try_structural_match(+Resolved, +FinalState)
try_structural_match([], _).
try_structural_match([Constraint|Rest], Final) :-
    match_one(Constraint, Final),
    try_structural_match(Rest, Final).

match_one(reg(Name, GoalVal), Final) :-
    get_reg(Final, Name, ActualVal),
    val_match(ActualVal, GoalVal).
match_one(c(GoalVal), Final) :-
    get_c(Final, ActualC),
    bit_match(ActualC, GoalVal).
match_one(z(GoalVal), Final) :-
    get_z(Final, ActualZ),
    bit_match(ActualZ, GoalVal).

%% validate_concrete(+Resolved, +FinalState, +NVectors)
%%   Fall back: concretize both sides with vectors and compare.
%%   Uses structured seed vectors first (corner cases), then random ones.
%%   This catches sparse conditions like eq(shr(A,24), imm(1)) that
%%   random-only testing misses with high probability.
validate_concrete(Resolved, FinalState, NVectors) :-
    collect_bit_symbols((Resolved, FinalState), AllSyms),
    %% Partition symbols into registers and unknowns
    partition_symbols(AllSyms, RegBitMap, UnknownSyms),
    %% Generate seed vectors from corner-case register values
    seed_bindings(RegBitMap, UnknownSyms, SeedBindingsList),
    length(SeedBindingsList, NSeed),
    %% Validate all seed vectors first
    validate_binding_list(SeedBindingsList, Resolved, FinalState),
    %% Then random vectors
    NRandom is max(0, NVectors - NSeed),
    validate_n_vectors(NRandom, AllSyms, Resolved, FinalState).

%% partition_symbols(+AllSyms, -RegBitMap, -UnknownSyms)
%%   Group bit(Reg,N) by register name. Collect u(...) separately.
partition_symbols([], [], []).
partition_symbols([bit(R,N)|Rest], RegMap, Unknowns) :- !,
    partition_symbols(Rest, RegMap0, Unknowns),
    add_to_reg_map(R, N, RegMap0, RegMap).
partition_symbols([U|Rest], RegMap, [U|Unknowns]) :-
    partition_symbols(Rest, RegMap, Unknowns).

add_to_reg_map(R, N, [], [R-[N]]).
add_to_reg_map(R, N, [R-Ns|Rest], [R-[N|Ns]|Rest]) :- !.
add_to_reg_map(R, N, [Other|Rest], [Other|Rest1]) :-
    add_to_reg_map(R, N, Rest, Rest1).

%% seed_bindings(+RegBitMap, +UnknownSyms, -BindingsList)
%%   Generate corner-case bindings. For each register, try a set of
%%   interesting 32-bit values. Cross-product across registers.
%%   Unknown symbols (temps) get random values in each seed vector.
seed_bindings(RegBitMap, UnknownSyms, BindingsList) :-
    %% Corner-case values: chosen to trigger boundary conditions
    %% in shifts, comparisons, byte-lane operations
    SeedValues = [
        0x00000000, 0x00000001, 0x000000FF, 0x00000100,
        0x0000FFFF, 0x00010000, 0x01000000, 0x01010101,
        0x7FFFFFFF, 0x80000000, 0xFFFF0000, 0xFFFFFFFF
    ],
    reg_seed_combos(RegBitMap, SeedValues, RegCombosList),
    maplist({UnknownSyms}/[RegBindings, FullBindings]>>(
        %% Add random values for unknown symbols
        maplist([Sym, Sym-V]>>(random_between(0, 1, V)), UnknownSyms, UPairs),
        append(RegBindings, UPairs, AllPairs),
        list_to_assoc(AllPairs, FullBindings)
    ), RegCombosList, BindingsList).

%% reg_seed_combos(+RegBitMap, +Values, -ComboList)
%%   Cross-product of seed values for each register.
%%   Each combo is a flat list of bit(R,N)-V pairs.
%%   Limit: if >2 registers, only use a subset of values to avoid explosion.
reg_seed_combos([], _, [[]]).
reg_seed_combos([R-BitNs|Rest], Values, Combos) :-
    length([_|Rest], NRemaining),
    %% Limit values per register to keep combos manageable
    ( NRemaining > 1
    -> take_n(6, Values, UseValues)    % 6 values for 3+ registers
    ;  UseValues = Values              % all values for 1-2 registers
    ),
    reg_seed_combos(Rest, Values, RestCombos),
    findall(
        Combined,
        ( member(V, UseValues),
          member(RC, RestCombos),
          word_to_bit_pairs(R, BitNs, V, Pairs),
          append(Pairs, RC, Combined)
        ),
        Combos
    ).

%% word_to_bit_pairs(+Reg, +BitNs, +Word, -Pairs)
%%   Convert a 32-bit word to bit(Reg,N)-BitVal pairs.
word_to_bit_pairs(_, [], _, []).
word_to_bit_pairs(R, [N|Ns], Word, [bit(R,N)-B|Rest]) :-
    B is (Word >> N) /\ 1,
    word_to_bit_pairs(R, Ns, Word, Rest).

take_n(_, [], []) :- !.
take_n(0, _, []) :- !.
take_n(N, [H|T], [H|R]) :- N > 0, N1 is N - 1, take_n(N1, T, R).

%% validate_binding_list(+BindingsList, +Resolved, +FinalState)
validate_binding_list([], _, _).
validate_binding_list([B|Bs], Resolved, FinalState) :-
    validate_one_vector(Resolved, FinalState, B),
    validate_binding_list(Bs, Resolved, FinalState).

validate_n_vectors(0, _, _, _) :- !.
validate_n_vectors(N, Syms, Resolved, FinalState) :-
    N > 0,
    random_bindings(Syms, Bindings),
    validate_one_vector(Resolved, FinalState, Bindings),
    N1 is N - 1,
    validate_n_vectors(N1, Syms, Resolved, FinalState).

validate_one_vector([], _, _).
validate_one_vector([Constraint|Rest], Final, Bindings) :-
    concrete_match_one(Constraint, Final, Bindings),
    validate_one_vector(Rest, Final, Bindings).

concrete_match_one(reg(Name, GoalVal), Final, Bindings) :-
    ( GoalVal == x -> true
    ; get_reg(Final, Name, ActualVal),
      concretize_val(ActualVal, Bindings, AWord),
      concretize_val(GoalVal, Bindings, GWord),
      AWord =:= GWord
    ).
concrete_match_one(c(GoalVal), Final, Bindings) :-
    ( GoalVal == x -> true
    ; get_c(Final, ActualC),
      concretize_bit(ActualC, Bindings, AC),
      concretize_bit(GoalVal, Bindings, GC),
      AC =:= GC
    ).
concrete_match_one(z(GoalVal), Final, Bindings) :-
    ( GoalVal == x -> true
    ; get_z(Final, ActualZ),
      concretize_bit(ActualZ, Bindings, AZ),
      concretize_bit(GoalVal, Bindings, GZ),
      AZ =:= GZ
    ).

%%% ===========================================================
%%% SECTION 13: Search Engine
%%% ===========================================================

%% find_sequence(+Config, -Sequence)
%%   Config = config{
%%     inputs: [a, b, ...],      % named input registers
%%     temps:  N,                 % number of temp registers
%%     max_len: M,                % maximum sequence length
%%     max_imm: I,                % max immediate value (default 511)
%%     goal: [Constraints],       % goal constraints
%%     n_vectors: V,              % concretization vectors (default 50)
%%     flag_policy: full|minimal  % override auto-detected flag policy
%%     flag_tail: K               % override: last K instructions get full flags
%%   }

find_sequence(Config, Sequence) :-
    _{inputs: Inputs, temps: TempCount, max_len: MaxLen, goal: Goal} :< Config,
    ( get_dict(max_imm, Config, MaxImm) -> true ; MaxImm = 511 ),
    ( get_dict(n_vectors, Config, NVec) -> true ; NVec = 50 ),
    %% Determine flag policy (accept both flag_policy and flag_mode)
    ( ( get_dict(flag_policy, Config, FPOverride)
      ; get_dict(flag_mode, Config, FPOverride) )
    -> FlagMode = flat(FPOverride)
    ; get_dict(flag_tail, Config, FTail)
    -> FlagMode = tail(FTail)
    ;  auto_flag_mode(Goal, FlagMode)
    ),
    init_state(Inputs, TempCount, InitState),
    all_reg_names(InitState, RegNames),
    %% Validate goal before starting expensive search
    validate_goal(Goal, InitState),
    format("~nFlag mode: ~w~n", [FlagMode]),
    format("Registers: ~w~n", [RegNames]),
    between(0, MaxLen, Len),
    format("--- Searching length ~d ---~n", [Len]),
    length(Sequence, Len),
    generate_sequence(Sequence, Len, RegNames, MaxImm, FlagMode),
    execute_sequence(Sequence, InitState, FinalState),
    check_goal(Goal, InitState, FinalState, NVec),
    format("  Found: ~w~n", [Sequence]).

%% Wrapper that catches goal_error exceptions and prints them nicely.
find_sequence_safe(Config, Sequence) :-
    catch(
        find_sequence(Config, Sequence),
        goal_error(Fmt, Args),
        ( format("~n*** GOAL ERROR ***~n"),
          format(Fmt, Args),
          nl,
          fail
        )
    ).

%% auto_flag_mode(+Goal, -FlagMode)
%%   Analyze goal constraints to determine the smartest flag policy.
%%   - All flags are x       → flat(minimal)  — no flag ops at all
%%   - Some flags constrained → tail(N)        — only last N instrs
%%     get full flag enumeration; earlier instrs use minimal
auto_flag_mode(Goal, FlagMode) :-
    ( goal_flags_are_dontcare(Goal)
    -> FlagMode = flat(minimal)
    ;  count_flag_constraints(Goal, N),
       FlagMode = tail(N)
    ).

goal_flags_are_dontcare([]).
goal_flags_are_dontcare([c(x)|Rest]) :- !, goal_flags_are_dontcare(Rest).
goal_flags_are_dontcare([z(x)|Rest]) :- !, goal_flags_are_dontcare(Rest).
goal_flags_are_dontcare([reg(_, _)|Rest]) :- !, goal_flags_are_dontcare(Rest).
goal_flags_are_dontcare([_|_]) :- fail.

%% count_flag_constraints(+Goal, -N)
%%   Count how many non-x flag constraints exist. At most 2 (C and Z).
count_flag_constraints(Goal, N) :-
    include(is_nontrivial_flag, Goal, FlagGoals),
    length(FlagGoals, N0),
    N is max(N0, 1).    % at least 1 tail instruction gets full

is_nontrivial_flag(c(V)) :- V \== x.
is_nontrivial_flag(z(V)) :- V \== x.

%% generate_sequence(+SeqHoles, +Len, +RegNames, +MaxImm, +FlagMode)
%%   FlagMode = flat(Policy) | tail(K)
%%   For tail(K): positions 0..Len-K-1 use minimal, Len-K..Len-1 use full.
generate_sequence([], _, _, _, _).
generate_sequence([Instr|Rest], Len, RegNames, MaxImm, FlagMode) :-
    length(Rest, Remaining),
    position_flag_policy(FlagMode, Len, Remaining, Policy),
    candidate_instr(RegNames, MaxImm, Policy, Instr),
    generate_sequence(Rest, Len, RegNames, MaxImm, FlagMode).

%% position_flag_policy(+FlagMode, +TotalLen, +Remaining, -Policy)
%%   Remaining = number of instructions AFTER this one.
%%   So position index = TotalLen - Remaining - 1.
%%   For tail(K): positions in the last K get full, rest get minimal.
position_flag_policy(flat(P), _, _, P) :- !.
position_flag_policy(tail(K), _TotalLen, Remaining, Policy) :-
    ( Remaining < K
    -> Policy = full
    ;  Policy = minimal
    ).

%%% ===========================================================
%%% SECTION 14: Pretty Printing
%%% ===========================================================

print_sequence([]) :- nl.
print_sequence([instr(Op, D, Src, FlagMod)|Rest]) :-
    format_src(Src, SrcStr),
    format_flags(FlagMod, FlagStr),
    upcase_atom(Op, OpUp),
    format("  ~w~t~20|~w, ~w~t~40|~w~n", [OpUp, D, SrcStr, FlagStr]),
    print_sequence(Rest).

format_src(reg(R), R).
format_src(imm(V), S) :- format(atom(S), "#~d", [V]).

format_flags(none, '').
format_flags(wc,   'WC').
format_flags(wz,   'WZ').
format_flags(wcz,  'WCZ').

%%% ===========================================================
%%% SECTION 15: Example Goals & Queries
%%% ===========================================================

%% Example 1: Swap registers A and B
%%   XOR swap: 3 instructions, no temps, no immediates needed.
%%   Flag policy auto-detects to 'minimal' since c(x)/z(x).
example_swap(Seq) :-
    find_sequence(
        config{
            inputs: [a, b],
            temps:  0,
            max_len: 3,
            max_imm: 0,
            goal: [
                reg(a, in(b)),
                reg(b, in(a)),
                c(x),
                z(x)
            ]
        },
        Seq
    ).

%% Example 2: Set Z to LSB of register A
%%   Expected: TESTB A, #0 WZ  (1 instruction)
example_z_lsb(Seq) :-
    find_sequence(
        config{
            inputs: [a],
            temps:  0,
            max_len: 1,
            max_imm: 31,
            goal: [
                reg(a, in(a)),
                c(x),
                z(bit(a, 0))
            ]
        },
        Seq
    ).

%% Example 3: Bitwise NOT of register A
%%   Expected: NOT A  (1 instruction)
%%   Demonstrates goal expression compiler: not(in(a))
example_not(Seq) :-
    find_sequence(
        config{
            inputs: [a],
            temps:  0,
            max_len: 1,
            max_imm: 0,
            goal: [
                reg(a, not(in(a))),
                c(x),
                z(x)
            ]
        },
        Seq
    ).

%% Example 4: R = A + B * 2  (with one temp)
%%   Goal uses expression compiler: add(in(a), shl(in(b), 1))
%%   Expected: MOV t0,b / SHL t0,#1 / ADD t0,a (or similar)
%%   max_imm: 1 keeps the search tractable (only #0 and #1 needed)
example_add_b_times_2(Seq) :-
    find_sequence(
        config{
            inputs: [a, b],
            temps:  1,
            max_len: 3,
            max_imm: 1,
            goal: [
                reg(t0, add(in(a), shl(in(b), 1))),
                reg(a, x),
                reg(b, x),
                c(x),
                z(x)
            ],
            n_vectors: 100
        },
        Seq
    ).

%% Example 5: Extract bit 7 of A into C flag
%%   Expected: TESTB A, #7 WC  (1 instruction)
example_bit7_to_c(Seq) :-
    find_sequence(
        config{
            inputs: [a],
            temps:  0,
            max_len: 1,
            max_imm: 31,
            goal: [
                reg(a, in(a)),
                c(bit(a, 7)),
                z(x)
            ]
        },
        Seq
    ).

%% Example 6: Zero the low byte of A  (AND with 0xFFFFFF00)
%%   Uses a mask register: AND a, mask
%%   mask = imm(0x1FF) won't work for 0xFFFFFF00, so we need
%%   a register holding the mask. We provide it as input.
example_zero_low_byte(Seq) :-
    find_sequence(
        config{
            inputs: [a, mask],
            temps:  0,
            max_len: 1,
            max_imm: 511,
            goal: [
                reg(a, and(in(a), in(mask))),
                reg(mask, in(mask)),
                c(x),
                z(x)
            ]
        },
        Seq
    ).

%% Example 7: C = 1 if A[31:24] == 0x01
%%   Uses comparison operator: shift right 24 bits, compare to 1.
%%   Expected: SHR into temp, CMP temp with #1 or similar.
example_top_byte_eq_01(Seq) :-
    find_sequence(
        config{
            inputs: [a],
            temps:  1,
            max_len: 3,
            max_imm: 31,
            goal: [
                reg(a, in(a)),
                reg(t0, x),
                c(eq(shr(in(a), 24), imm(1))),
                z(x)
            ],
            n_vectors: 100
        },
        Seq
    ).

%%% ===========================================================
%%% SECTION 16: Quick-Run Helpers
%%% ===========================================================

run_example(swap) :-
    format("=== Finding: swap A <-> B ===~n"),
    ( example_swap(Seq)
    -> format("~nSolution:~n"), print_sequence(Seq)
    ;  format("~nNo solution found.~n")
    ).

run_example(z_lsb) :-
    format("=== Finding: Z = A[0] ===~n"),
    ( example_z_lsb(Seq)
    -> format("~nSolution:~n"), print_sequence(Seq)
    ;  format("~nNo solution found.~n")
    ).

run_example(not_a) :-
    format("=== Finding: A = NOT A ===~n"),
    ( example_not(Seq)
    -> format("~nSolution:~n"), print_sequence(Seq)
    ;  format("~nNo solution found.~n")
    ).

run_example(add_b2) :-
    format("=== Finding: T0 = A + B*2 ===~n"),
    ( example_add_b_times_2(Seq)
    -> format("~nSolution:~n"), print_sequence(Seq)
    ;  format("~nNo solution found.~n")
    ).

run_example(bit7c) :-
    format("=== Finding: C = A[7] ===~n"),
    ( example_bit7_to_c(Seq)
    -> format("~nSolution:~n"), print_sequence(Seq)
    ;  format("~nNo solution found.~n")
    ).

run_example(top_byte) :-
    format("=== Finding: C = (A[31:24] == 0x01) ===~n"),
    ( example_top_byte_eq_01(Seq)
    -> format("~nSolution:~n"), print_sequence(Seq)
    ;  format("~nNo solution found.~n")
    ).

%% run_all: run all simple examples
run_all :-
    run_example(z_lsb),
    run_example(not_a),
    run_example(bit7c),
    run_example(swap).

%%% ===========================================================
%%% SECTION 17: Operator Helpers for Concise Goal Syntax
%%% ===========================================================

%% goal_bit(+InitState, +Reg, +BitN, -BitVal)
goal_bit(Init, Reg, N, Bit) :-
    get_reg(Init, Reg, bv(Bits)),
    nth0(N, Bits, Bit).
