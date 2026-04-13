Let me research the specific instructions mentioned before writing the new model.The key discovery on block timing: footnote 1 on line 8482 says **"+1 if crosses hub long"** — not "+8" or "+16". That means `block_per_long` as I had it in v2 was completely fabricated. The correct extra factor is `+1 clock (cog or hub) if the access crosses a hub long boundary`. The base range 3..10 already encodes the hub-window phase wait; the +1 is a one-time crossing penalty. I do not see any per-long block-transfer timing in the official documentation, which means the per-long cost must be exactly 1 clock each (since the CORDIC/hub bus can sustain one long per clock once in-phase).

Now let me write the revised model v3.

---

## Model v3

Here's what each of your 20 items resolved to, then the full revised schema and examples.

---

### Resolution notes before the schema

**1. sub_fields as list.** Every operand now has `sub_fields: []` — a list of `{bits, name, description}` objects. Used wherever a field has semantic sub-structure (ALTI's S has 6 named 3-bit chunks; MOVBYTS's S has four 2-bit byte-selector nibbles).

**2 & 3. Operands as ordered list with `name` + `stored_in`.** `operands` is now a sequence. Each item has `name` (the assembly-visible label: `D`, `S`, `N`, `A`, `n`, `PA/PB/PTRA/PTRB`), `stored_in` (which encoding field holds it: `D_field`, `S_field`, `N_field`, `A_field`, `WW_field`, `n_field`), and the rest of the descriptor.

**4. Missing roles.** Added: `reg_or_imm9_or_ptrexpr` (WMLONG S), `imm23` (AUGS/AUGD payload), `address` (JMP/CALLD-imm20 `#{\}A`).

**5. Flags unchanged.** Confirmed correct.

**6. registers: no K, include IJMPn/IRETn.** K is removed from `registers`; `stack` is the model for that. IJMP1/IRET1 etc. are valid named registers.

**7. registers.written: no inline annotations.** Conditionality is now a structured `condition` enum per written-register entry.

**8. Q register semantics.** Q is a general-purpose "prefix value" register. SETQ stores a value *and* sets `q_state: regular`; SETQ2 sets `q_state: alternative`. Instructions that consume Q care about the state. Q is consumed by: RDLONG/WRLONG/WMLONG (block count), MUXQ (bit mask), CRCNIB (poly shift), COGINIT (param), QDIV/QFRAC/QROTATE/WAITxxx (timeout/input). The `q_effect` section now distinguishes `sets_q_state` (SETQ/SETQ2), `reads_q_value` (MUXQ etc.), and `behavior_per_q_state` (RDLONG/WMLONG).

**9. q_effect.behavior broader.** Replaced with `q_consumed` (bool), `q_state_required` (null / `regular` / `alternative` / `any`), and `q_behavior` list keyed on state.

**10 & 13. Timing correction.** The footnote says `+1 if crosses hub long` — not +8/+16. The correct additional factor type is `hub_long_cross` (cost: 1 cog, 1 hub). Block transfers cost +1 per additional long (hub bus can do one long per clock). `hub_phase` added as an open topic with explanation of the determinism property.

**11. stack.operation: push|pop|none.**

**12. branch.conditional removed** (redundant with type==branch).

**13. return_context destination/source** reduced to enum: `D`, `stack`, `PA`, `PB`, `PTRA`, `PTRB`.

**14. pipeline deferred as open question.**

**15. interrupt_shield: `self_if_wc`** added.

**16. smart_pin operation fixed.** AKPIN is an alias for `write_X` with `acknowledges: true`. `acknowledge` is not a standalone operation.

**17. operand.format field.** Added: `raw | unsigned | signed | pcstate | ptrexpr | vec4_byte_sel | nits | nibs | ...`

**18. Semantics: R vs D, immediate checks.** `R` = result destination (usually same as D, but ALTR can redirect it). `$is_imm(D)` / `$is_imm(S)` predicates. `n` for selector literals.

**19. encoding.fields: only non-fixed patterns.** Fixed constant fields (opcode constants) are removed from the fields list; the `pattern` string encodes them fully.

**20. ignores_condition: bool** added.

---



# ───────────────────────────────────────────────────────────────────
# OPEN QUESTIONS (v3)
# ───────────────────────────────────────────────────────────────────
#
# 1. Hub phase tracking.
#    hub_phase factor costs 0..7 cog clocks — but after any instruction
#    that incurred a hub_phase wait, the cog's phase relative to the hub
#    is deterministic. A timing analyser could track this and give exact
#    (not range) cycle counts for sequences of hub-bound instructions.
#    Proposed addition: a `hub_phase_output: bool` field on instructions
#    that synchronise phase, allowing tools to propagate phase state.
#    Decision needed: is this in the ISA DB or a separate analyser concern?
#
# 2. Pipeline section completeness.
#    The ALT family (ALTS/ALTD/ALTR/ALTB/ALTI/ALTSB/ALTSN/ALTGB/ALTGN/
#    ALTGW/ALTGN), SCA, and SCAS all modify the following instruction's
#    pipeline. The `next_instr_modifier` field names the slot but does not
#    describe the *value* substituted or the auto-indexer mechanism (ALTR/ALTS
#    D post-increment). A full `pipeline.substitution` sub-object is needed.
#    AUGS/AUGD are `next_imm_S/D` which is a different category: they modify
#    how the next instruction's immediate is decoded, not the register field.
#    Needs its own sub-field separate from ALT modifiers.
#
# 3. Semantics language: R vs D disambiguation.
#    ALTR redirects the *result* of the next instruction to a different
#    register than D. In the semantics language, `R` denotes the result
#    destination and `D` the source operand; they diverge when ALTR is active.
#    For instructions where R == D always (the vast majority), using just `R =`
#    is unambiguous. A note to this effect should be added to every entry that
#    writes D in the normal case, to prepare for ALTR support.
#
# 4. block_per_long source confirmation.
#    The PASM2 manual (timing table footnote, line 8482) states "+1 if crosses
#    hub long" for single-long hub accesses. It does not explicitly state the
#    per-long cost for block transfers. The value of 1 clock per additional long
#    is derived from the fact that the P2 hub bus runs at 1 long per hub slot
#    (8 cog clocks per hub revolution ÷ 8 cogs = 1 slot per cog per revolution,
#    but the bus itself can sustain 1 long/clock once a cog holds the bus).
#    This should be verified against the silicon documentation or community
#    measurements before treating block_per_long = 1 as authoritative.
#
# 5. AUGS/AUGD: interrupt-shielded self.
#    The existing `shield: true` in p2instructions.json for AUGS/AUGD indicates
#    the instruction following an AUGS/AUGD is shielded. The current model
#    puts this under `interrupt_shield.type: loop_body` which is wrong —
#    it should be `next_instr_shielded: bool` as a sibling field to
#    `interrupt_shield`, since these protect the *next* instruction, not
#    themselves or a loop. Also applies to ALTS/ALTD/ALTR/ALTI/SCA/SCAS/ALTSB/
#    ALTSN etc. (all pipeline-modifying instructions shield their successor).
#
# 6. Conditional writes: if_reg_D_and_wc and similar.
#    The current condition enum covers the known cases. If a future instruction
#    has a condition not in the enum (e.g. "if block mode"), the enum must be
#    extended. The enum should be reviewed against all ~376 instructions before
#    freezing.