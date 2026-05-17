# Handoff: rewriting `bv_values/svalue.ml` with the `[%%svalue]` DSL

Audience: a followup agent continuing this work. This document is the single
source of truth for *why*, *what*, and *how*. Read it fully before touching
code. It assumes no memory of the prior session.

---

## 0. TL;DR

There is a working PPX (`soteria/ppx/svalue_dsl_*.ml`) that turns a compact
declarative DSL into a hash-consed value language: the `ty` type, the
hash-consed term type, hash-consing, smart constructors (with all algebraic
simplifications expressed as K-style rewrite rules), constant folding, the
generic `mk_*` dispatchers, a derived `eval`, and a phantom-typed `Typed`
module. It has been used to **fully and equivalently** rewrite the small
`tiny_values/svalue.ml` (537 lines → ~335 lines of declarative spec), proven
by a semantic differential test.

The goal is to do the same for `bv_values/svalue.ml` (2309 lines, the real
industrial bitvector engine), which today is an unreadable wall of nested
pattern matches. The blocker was DSL expressiveness; the two largest gaps
(**parameterized constructors** and **parameterized/recursive types**) are now
implemented and regression-clean. The remaining work is enumerated in §5–§7.

---

## 1. The goal

`bv_values/svalue.ml` defines the symbolic value language used by the
bitvector backend. It is ~2300 lines, the bulk of which is three
mutually-recursive modules (`Bool`, `BitVec`, `Float`) of smart constructors,
each a giant `match (v1.node.kind, v2.node.kind) with | ... -> ...` performing
algebraic simplification. It is correct but effectively unreadable and
unmaintainable.

We want to re-express it as `bv_values/svalue_dsl.ml`: a *declarative*
description (types, operators, K-style rewrite rules) from which the PPX
**generates**:

1. `Svalue` — the `ty` type, the hash-consed term type, hash-consing, the
   `Unop`/`Binop`/`Nop` constructor modules, all smart constructors with their
   simplifications, constant folding, `pp`, `iter_vars`, `equal`/`compare`/
   `hash`, `mk_unop`/`mk_binop`/`mk_nop`.
2. `eval` — the recursive evaluator (currently `bv_values/eval.ml`), which
   should be *derived* (it is just "recurse, re-apply smart ctors").
3. `Typed` — the phantom-typed wrapper (currently `bv_values/typed.ml` +
   `typed.mli`), sealed by a generated abstract signature so the polymorphic
   sorts (`sint`, `sbool`, `sptr`, `nonzero`, …) are actually enforced.

**Equivalence is the acceptance criterion.** `svalue_dsl` need not be a
syntactic drop-in; it must be *semantically equivalent* to `svalue.ml`, proven
by a differential test (see §8). This mirrors exactly what was done for
`tiny_values` (see `soteria/tests/svalue_dsl/check.ml`).

The DSL must be *elegant*: a bv arm like

```ocaml
| Binop (Eq, bv1, { node = { kind = Unop (BvExtract (s1,e1), x); _ }; _ }),
  Binop (Eq, bv2, { node = { kind = Unop (BvExtract (s2,e2), y); _ }; _ })
  when equal x y && (e1+1 = s2 || e2+1 = s1) ->
    let bv, xy = if e1+1=s2 then BitVec.concat bv2 bv1, BitVec.extract s1 e2 x
                 else BitVec.concat bv1 bv2, BitVec.extract s2 e1 x in
    sem_eq bv xy
```

must become two readable rules:

```
rule (bv1 == bv_extract[s1 e1] x) && (bv2 == bv_extract[s2 e2] y)
   ~> sem_eq (bv_concat bv2 bv1) (bv_extract[s1 e2] x)   when {{ equal x y && e1+1 = s2 }}
rule (bv1 == bv_extract[s1 e1] x) && (bv2 == bv_extract[s2 e2] y)
   ~> sem_eq (bv_concat bv1 bv2) (bv_extract[s2 e1] x)   when {{ equal x y && e2+1 = s1 }}
```

The `{{ ... }}` guard is a genuine numeric predicate (fine — `tiny` uses these
too, e.g. `when {{ Z.equal i j }}`). What was previously forcing whole-arm
escaping was the *inability to bind constructor payloads* (`s1 e1` from
`BvExtract`), not the predicate. That gap is now fixed.

---

## 2. Current state of the PPX (what already exists)

### 2.1 Files

- `soteria/ppx/svalue_dsl_ast.ml` — the DSL AST.
- `soteria/ppx/svalue_dsl_lex.ml` — hand-written lexer.
- `soteria/ppx/svalue_dsl_parse.ml` — recursive-descent parser.
- `soteria/ppx/svalue_dsl_gen.ml` — code generation (~1450 lines, the bulk).
- `soteria/ppx/svalue_dsl_ppx.ml` — registers the
  `[%%svalue {| ... |}]` structure-item extension; called from
  `soteria/ppx/ppx_symex.ml` (`let () = Svalue_dsl_ppx.register ()`).
- `soteria/ppx/dune` — `library ppx_symex`, `kind ppx_rewriter`,
  `libraries fmt ppxlib`, `preprocess (pps ppxlib.metaquot)`.

The `soteria` lib (`soteria/lib/dune`) has `(preprocess (pps ... ppx_symex
...))` and `(include_subdirs qualified)`. A submodule must be re-exported in
the directory wrapper to be visible, e.g. `tiny_values/tiny_values.ml`
contains `module Svalue_dsl = Svalue_dsl`. For bv you must add
`module Svalue_dsl = Svalue_dsl` to `soteria/lib/bv_values/bv_values.ml`.

### 2.2 Reference rewrite (study this)

`soteria/lib/tiny_values/svalue_dsl.ml` is a *complete, faithful,
equivalence-proven* DSL rewrite of `tiny_values/svalue.ml`. It is the gold
template for style and for what the DSL can express. Read it side-by-side
with `tiny_values/svalue.ml`.

### 2.3 DSL surface (today)

Inside `[%%svalue {| ... |}]`:

- `ty Bool sort sbool | Int sort sint` — declares `type ty = TBool | TInt`,
  with optional per-variant phantom `sort` mapping. **Now also supports
  parameterized & recursive constructors**:
  `ty Bool | Float(FloatPrecision.t) | Seq(ty) | BitVector(int)`
  → `type ty = TBool | TFloat of FloatPrecision.t | TSeq of ty | TBitVector
  of int`. Nullary variants get a `t_<lower>` alias; parameterized ones do
  not (write those helpers by hand after the block).
- `sort sint : Int {{ [ \`NonZero | \`Zero ] }}` — a phantom sort for the
  `Typed` module, with optional base runtime `ty` (`: Int`). `sort any {{ ...
  }}` (no base) → treated as a polymorphic `'a` in `Typed`.
- `leaf Var : poly = Var.t` — a leaf kind constructor (`Var of Var.t`);
  `poly` means "ty supplied at construction" (the variable). Generates
  `mk_var v ty`.
- `literal Bool : Bool = bool as of_bool` /
  `literal Int : Int = Z.t as int_z print Z.pp_print` — a constant kind
  constructor with a friendly smart-ctor name and an optional printer. A
  `bool` literal also auto-generates `v_true`/`v_false`/`of_bool`/`to_bool`.
- `op name : T1 -> T2 -> R SYM = Ctor(p1: ty, p2: ty) { props; rules }` —
  an operator. `SYM` is the pretty/infix symbol (unquoted: `+`, `<=`,
  `&&`; for word/symbol pp use the `{{ ... }}` form: `{{rem}}`, `{{/}}`).
  `Ctor` is the OCaml constructor; `(p:ty,...)` is its **parameterized
  payload** (NEW). `any` as an arg/result type → that arg's `.node.ty` is
  used for the result; otherwise result ty = `T<R>`.
  Properties: `commutative` (uses `mk_commut_binop`, and mirrors every rule
  for both argument orders), `idempotent`, `involutive`, `identity <int>`,
  `absorbing <int>`, `fold <ocaml-fn>` (constant folding when all operands
  are literals).
- `nop name = Ctor {{ fun l -> ... }}` — an n-ary op (`Nop of Nop.t * t
  list`); the smart ctor body is hand-written OCaml (used for `Distinct`,
  which needs `sure_neq` + cross product).
- `with {{ <structure items> }}` — in-module helper code, emitted **after**
  the kind type and `( <| )` but **before** the smart constructors (so it
  can reference kind constructors, `equal`, `<|`). Used in `tiny` for the
  recursive `is_mod`.

Rules: `rule <lhs> ~> <rhs> [when <guard>]`.

- Lowercase ident = term-level pattern var (binds a whole `t`); repeated
  occurrence ⇒ auto `equal` guard.
- `#x` / `#x:Kind` = literal-payload-level var (binds the `Z.t`/`bool`/...).
- `_` = wildcard. `true`/`false`/integer = literal constants.
- `name[p1 p2] a b` = match/construct operator `name` binding/passing its
  **constructor params** `p1 p2` (NEW). Top-level rule params are the smart
  ctor's own named arguments (referenced by their declared names, no `[ ]`
  needed); `[ ]` is for params of *sub-terms*.
- RHS using the **smart-ctor name** (`leq b a`) calls the smart constructor
  (re-simplifies / recurses). RHS using the **OCaml constructor name**
  (`Leq b a`, capitalized) builds the raw node directly with no
  re-simplification (commutative-aware), matching arms like `Binop (Leq, v2,
  v1) <| TBool`.
- Leaf-constructor patterns: `(Var a) - (Var b) ~> 0 when {{ Var.equal a b
  }}` binds the leaf payload.
- `{{ ocaml }}` is usable as a whole RHS, inside a `when` guard, or as the
  `nop`/`with` body. Guards may also be `x = y` / `x <> y` over pattern
  vars (compiled to `equal`).

### 2.4 What the PPX generates (structure order)

Everything is wrapped in `include struct [@@@ocaml.warning "-a"] ... end`
(warnings scoped). Order produced by `generate` in `svalue_dsl_gen.ml`:

1. `ty_decl` — `type ty = ... [@@deriving eq, show, ord]`.
2. `ty_helpers` — `t_<lower>` aliases (nullary only), `is_bool_ty`.
3. `Nop` / `Unop` / `Binop` modules (`type t = ... [@@deriving eq, ord]` +
   `pp`). Constructors now carry their parameterized payloads.
4. `core_items` — `pp_hash_consed`/`equal_hash_consed`/`compare_hash_consed`,
   the recursive `type t_kind ... and t_node ... and t = t_node hash_consed
   [@@deriving show, eq, ord]`, `unique_tag`/`hash`/`kind`/`equal`/`compare`,
   `iter_vars`, `pp_full`, `pp`, the `Hcons = Hc.Make(...)` functor,
   `( <| )`, `mk_commut_binop`, `mk_commut_nop`.
5. `leaf_lit_ctors` — `mk_var`, `int_z`/`of_bool`, `v_true`/`v_false`/
   `to_bool`.
6. `aux` — the `with {{ ... }}` blocks.
7. `smart_ctors` — one big `let rec <op> ... and <op> ...`.
8. `nop_bodies` — `let distinct = <body>`.
9. `dispatchers` — `mk_unop`/`mk_binop`/`mk_nop` (now destructure ctor
   params and partially apply: `function Lt s -> lt s | Add c -> add c | ...`).
10. `eval_items` — `type _ Effect.t += Eval_var ...`, `eval_var`, the
    derived `let rec eval`, the effect-handled `eval ~eval_var`.
11. `typed_module` — `module Typed : <generated abstract sig> = struct ...
    end` (only if `sort`s are declared).

`iter_vars`, the `Hcons` hash, `pp`, and `eval` are generated **conditionally**
on which of `Unop`/`Binop`/`Nop`/`Ite` exist (helpers `has_un`/`has_bin`/
`has_nop`/`has_ite` in `core_items`/`eval_items`). This is the seam to
generalize for arbitrary structural kinds (see §6).

### 2.5 Codegen internals you must understand

- `ctx` (built by `build_ctx`): `tys`, `ty_args`, `lits`, `leaves`, `ops`,
  `nops`, `aux`, `sorts`, `by_name`/`by_sym`/`by_ctor` (op lookup),
  `lit_by_ty`, `ty_sort`/`sort_base`, `op_sorts` (original sort tokens for
  `Typed`, before sort→runtime-ty normalization).
- Sort↔ty: `op_args`/`op_ret` are normalized to **runtime ty** (a `sort`
  with a base ty maps to it) for `Svalue` codegen; `op_sorts` keeps the
  original tokens for `Typed`. `runtime_ty`/`sort_of` do the mapping.
- `compile_pat ctx ~loc ~expect_ty ~seen e : pattern * guards` — turns a
  LHS sub-expr into a pattern over a whole `t` value plus side guards.
  Handles: wildcard, term var (+repeat→guard), `#lit`, bool/int constants
  (int constants → `equal v (int_z n)` guard since `Z.t` is unpatternable),
  leaf-ctor patterns, and operator application **with constructor params**
  (`pconstr o.op_ctor ppats` where ppats come from `params`).
- `emit_term` — RHS to expression. Branches: `by_ctor` (raw build, threads
  params via `econstr o.op_ctor pes`), fold (all-lit + `op_fold` + no
  params), else smart-ctor call (`eapply (evar o.op_name) (param_exprs @
  arg_exprs)`). `emit_param` emits a constructor-param expr (var/int/bool/
  `{{}}`). `emit_lit` handles literal-level subexprs.
- `env` maps a rule's variable names to `` `Term `` / `` `Lit `` / `` `Param
  `` (param = raw OCaml value usable in guards/RHS/`{{}}`). `collect_env`
  also harvests sub-term ctor-param binders.
- `smart_ctor` — the op's `let rec` binding. Param fun-args (named by
  `op_params`) come **first**, then `__a0..__an`; body is `match (__a0,
  __a1) with derived_arms @ fold_arm @ user_rules(+commut swaps) @ default`.
- `default_expr` / `fold_arm` / `derived_arms` thread `ctor_params_e`
  (the `op_params` names) into the constructed node.
- `typed_module` builds an abstract-sealed signature **as text**, parsed by
  `Ppxlib.Parse.interface`. `type +'a t = t` alone is *vacuous*; the
  abstract `type +'a t`/`type +'a ty` in the sealing signature is what
  enforces the phantom sorts. Op sigs prepend the (printed) param OCaml
  types before the sort-typed arg/ret.

---

## 3. The target: anatomy of `bv_values/svalue.ml`

Required reading (in order): `soteria/lib/bv_values/svalue.ml`,
`soteria/lib/bv_values/eval.ml`, `soteria/lib/bv_values/typed.mli`,
`soteria/lib/bv_values/typed.ml`, `soteria/lib/bv_values/expr.ml`.

### 3.1 Helper modules (lines ~5–34)

`FloatPrecision` (`F16|F32|F64|F128` + `size`/`of_size`), `FloatClass`
(`Normal|Subnormal|Zero|Infinite|NaN` + `as_fpclass`), `RoundingMode`
(`NearestTiesToEven|NearestTiesToAway|Ceil|Floor|Truncate`). These are
referenced by `ty`, `Unop`, `Binop` — so they must be emitted **before** the
type declarations. The current `with {{ }}` slot is *after* the kind type and
cannot host them. → need a **prelude** slot (§6.D).

### 3.2 `ty` (lines ~36–66)

```ocaml
type ty = TBool | TFloat of FloatPrecision.t | TLoc of int
        | TPointer of int | TSeq of ty | TBitVector of int
```
Recursive (`TSeq of ty`) and parameterized — **supported now** by
`ty Bool | Float(FloatPrecision.t) | Loc(int) | Pointer(int) | Seq(ty) |
BitVector(int)`. Hand-written helpers `t_f16`, `t_bv`, `is_float`, `is_bv`,
`precision_of_f`, `size_of` go after the block (or in a `with`/`prelude`).

### 3.3 Constructor modules (lines ~68–191)

`Nop`: `Distinct`. `Unop`: `Not`, `GetPtrLoc`, `GetPtrOfs`, `BvOfBool of
int`, `BvOfFloat of RoundingMode.t * bool * int`, `FloatOfBv of
RoundingMode.t * bool * FloatPrecision.t`, `FloatOfBvRaw of
FloatPrecision.t`, `BvExtract of int * int`, `BvExtend of bool * int`,
`BvNot`, `Neg`, `FAbs`, `FIs of FloatClass.t`, `FRound of RoundingMode.t`.
`Binop`: nullary `And|Or|Eq|FEq|FLeq|FLt|FAdd|FSub|FMul|FDiv|FRem|Mod|
BvConcat|BitAnd|BitOr|BitXor|Shl|LShr|AShr`, parameterized `Add of
{checked:bool}`, `Sub of {checked:bool}`, `Mul of {checked:bool}`, `Div of
bool`, `Rem of bool`, `AddOvf of bool`, `SubOvf of bool`, `MulOvf of bool`,
`Lt of bool`, `Leq of bool`. **All parameterized forms are supported now**
(declare e.g. `op bv_add : Bv -> Bv -> Bv + = Add(checked: bool) { ... }`).
Note: the DSL represents inline-record payloads as a positional tuple (e.g.
`Add of bool` instead of `Add of {checked:bool}`) — fine, since `svalue_dsl`
is an independent equivalent module, not a textual drop-in.

The custom `Unop.pp`/`Binop.pp` (with `pp_signed`/`pp_checked`) are *not*
required for equivalence (the test compares semantically via `eval`, not
pp-strings — see §8). The generated `pp` is adequate.

### 3.4 `t_kind` (lines ~197–211)

```ocaml
type t_kind =
  | Var of Var.t | Bool of bool | Float of string | Ptr of t * t
  | BitVec of Z.t | Seq of t list | Unop of Unop.t * t
  | Binop of Binop.t * t * t | Nop of Nop.t * t list | Ite of t * t * t
  | Exists of (Var.t * ty) list * t
```
`Var`=leaf, `Bool`/`Float`/`BitVec`=literals (already supported), but
`Ptr of t*t`, `Seq of t list`, `Exists of (Var.t*ty) list * t` are **new
structural kinds** the DSL cannot yet declare (§6.C). `Exists` is a
*binder*: `iter_vars` must add the bound vars to an ignore set before
descending into the body (the original `iter_vars`, lines ~218–239, does
exactly this). This is the single subtlest semantic requirement.

### 3.5 Smart constructors (lines ~458–2222)

`module rec Bool : Bool = struct ... end and BitVec : BitVec = struct ...
end and Float : Float = struct ... end`. Module-type sigs at lines
~333–457. The flat namespace the DSL generates is *fine* for an equivalent
module; downstream that wants `Svalue.BitVec.add` can be satisfied with
thin alias modules written after the block (or generated — optional). The
critical signatures:

- `BitVec`: `mk : int -> Z.t -> t` (size + value, **masks** to size),
  `mki`, `zero`/`one : int -> t`, `add ?checked`, `div ~signed`, `concat`
  (result size = n1+n2), `extract lo hi` (result size = hi-lo+1), `extend
  ~signed n`, `lt ~signed`, `of_bool n`, `of_float ~rounding ~signed
  ~size`, etc. Note **optional/labelled args** (`?checked`, `~signed`).
- `Float`: `mk : FloatPrecision.t -> string -> t`, arithmetic, comparisons,
  classification.

`eval.ml` is the dispatcher `Binop.t -> t -> t -> t` etc. — it is exactly
what the DSL's `mk_binop`/`mk_unop` + derived `eval` already produce. The
generated `eval` should replace `bv_values/eval.ml` (verify semantics).

### 3.6 `Typed` (typed.mli)

Many sorts: `sint = [\`NonZero|\`Zero]`, `sint_ovf` (+`\`Overflowed`),
`nonzero`, `zero`, `sfloat`, `sbool`, `sptr`, `sloc`, `'a sseq = [\`List of
'a]`, `cval = [sint|sptr|sfloat]`, `any`. Plus `pp_*` and `hash_*` per
sort. The current `typed_module` generator handles flat sorts with `pp_*`;
it needs: parametric sorts (`'a sseq`), `hash_*` functions, and the larger
glue surface. Read `typed.mli` fully and extend the generator (§7).

---

## 4. Why so much escaping was needed before (root cause)

The original DSL only supported **nullary** operator constructors. bv's power
is in *parameterized* constructors (`Lt of bool`, `Add of {checked}`,
`BvExtract of int*int`) and *parameterized/recursive* types. Without binding
a constructor's payload (`Lt signed -> ... signed ...`, `BvExtract (s,e) ->
... s e ...`), every such arm had to be a whole-arm `{{ }}` escape, which is
larger and uglier than the original. This was a real design miss (the
original scope targeted `tiny`'s clean nullary algebra). It is now fixed.

The remaining "procedural-looking" arms decompose into: (a) clean rules with
small numeric `{{ }}` *guards* (legitimate — `tiny` does this), and (b) RHS
`if cond then A else B` which becomes **two guarded rules**. Very little
genuinely needs full escaping once §5–§7 are done.

---

## 5. Done (verified)

### Prior sessions

1. **Parameterized operator constructors**, end-to-end: AST (`op_params`),
   parser (`= Ctor(p:ty,...)` and `name[p ...]` in rules), codegen
   (`compile_pat` param patterns, `emit_term`/raw-build param threading,
   `emit_param`, `smart_ctor` leading param args, `default_expr`/
   `derived_arms`/`fold_arm`, `mk_*` dispatchers destructure params,
   `op_module` ctor payloads, `Typed` sig prepends param types).
2. **Parameterized & recursive `ty` constructors**: AST (`ty_args`),
   parser (`parse_paren_types`, shared), codegen (`ty_decl` payloads,
   `ty_helpers` skips aliases for parameterized variants, `ctx.ty_args`).

### This session (handoff §10 steps 1–3 + all §6 engine features)

All landed in the PPX, **tiny regression green throughout** (20000×6 envs
equivalent + phantom typing OK), and **`soteria.cma` builds** with a partial
`bv_values/svalue_dsl.ml` skeleton (`module Svalue_dsl = Svalue_dsl` added to
`bv_values.ml`) that exercises every feature. Generated code verified by
inspection via `standalone.exe`.

3. **§6.D Prelude escape** (`prelude {{ ... }}`): lexer kw, `DPrelude`,
   `parse_prelude`, `ctx.prelude` emitted at the very top (before `ty_decl`).
   Used to port `FloatPrecision`/`FloatClass`/`RoundingMode`.
4. **§6.C Generic structural kinds** (`kind <Ctor> [binder] (fields…) [eval
   {{ }}]`): AST `kind_decl`/`kind_field` (`KRec`/`KRecList`/`KOpaque`),
   `parse_kind` (depth-aware field span capture), `ctx.kinds`. Codegen weaves
   kinds into `kind_type` (constructors), `core_items` (a **binder-aware
   `iter_vars`**: `aux ~ignore` form with `Var.Set` when any kind is a
   `binder`, else the old `let rec` form unchanged so tiny is byte-identical;
   `hash`), `pp_fun` (non-faithful `<Ctor>` case), `eval_items` (default
   recurse+raw-rebuild reusing `x.node.ty`, or an explicit `eval {{ fun x
   f0 … -> t }}` — used by `Exists`, effect-based: bound vars → `mk_var v
   ty`, free vars re-`perform` to the outer handler). `equal_t_kind`/
   `compare_t_kind`/`pp_t_kind` are auto-derived (constructors in `t_kind`).
   *Not yet done: kind-constructor patterns/construction inside op rules*
   (needed for `sem_eq`'s `Ptr (..)==Ptr (..)` arm, step 5) — `compile_pat`/
   `emit_term` must learn `is_kind_ctor` (helper already added). Rules-based
   `kind` bodies (vs `eval {{ }}`/hand-written `with`) also deferred — bv's
   Ptr/Seq/Exists smart ctors are bespoke glue in a `with {{ }}` block.
5. **§6.E Flexible result type**: AST `op_ret_ty`, parser `-> {{ fun
   <params> <args> -> ty }}` after the ctor/params, threaded through
   `result_ty_expr` (default arm) and `emit_term` raw-build. `any`/`T<R>`
   still the fallbacks. Verified with `bv_concat` → `TBitVector (size_of a
   + size_of b)`.
6. **§6.F Parameterized literal ctors**: AST `lit_params`/`lit_build`,
   `mk` keyword + shared `parse_ctor_params` (depth-aware), `parse_payload_
   type` stop-set widened. `leaf_lit_ctors` emits `let <ctor> = <build>`
   when a builder is given; `lit_ctor_for` returns `None` for parameterized
   literals (no bad 1-arg `#x` coercion — such rules build via `{{ }}`);
   `Typed` lit sig prepends param types. Verified: `bv_mk n z = BitVec
   Z.(z land pred (one lsl n)) <| TBitVector n`, `float_mk fp f = Float f
   <| TFloat fp`.
7. **§6.G size-aware folding**: *no new PPX feature needed for
   correctness* — shown expressible with the shipped machinery: term-var
   patterns + a `{{ }}` guard + a `{{ }}` RHS reading `.node.ty`, plus tiny
   `is_bv_lit`/`to_z_exn` helpers in `with {{ }}`. Verified: `bv_add`/
   `bv_concat` fold arms produce correctly-masked, correctly-sized
   constants. The *ergonomic sugar* `#x@s` (bind payload **and** size) is
   deferred to step 5 — see §6.G note below; it is sugar only, not required.

8. **§6.C deferral resolved — kind-patterns in rules**: `compile_pat` now
   accepts a generic-kind constructor pattern (`(Ptr l o)`, `(Seq xs)`):
   `KRec` fields recurse, `KRecList`/`KOpaque` bind a var/`_`. `collect_env`
   already harvests them (treated as `Term`, emitted bare — correct for
   `{{ }}`/sub-`sem_eq` use). `is_kind_ctor` gates it before the op branch.
   Verified by `sem_eq`'s `(Ptr l1 o1)==(Ptr l2 o2)` arm (commutative-mirrored
   correctly). *Still deferred:* kind **construction** in a RHS (build a
   `Ptr` from a rule) — not needed yet; bv builds kinds via the hand-written
   `ptr`/`seq` glue in `with {{ }}`.
9. **Step 4 — Bool transcribed + BitVec deps (fold-only)**: `bv_values/
   svalue_dsl.ml` now has the **full Bool core** (`not`, `and_`, `or_`,
   `ite`, `sem_eq`) as K-rules, plus the BitVec ops Bool references
   (`bv_concat/add/sub/neg/not/lt/leq/extract/of_bool`) transcribed
   **fold-only**. `soteria.cma` builds; tiny green; generated code
   inspected (the `not (bv_lt[s] a b) ~> bv_leq[s] b a` payload-binding and
   the `Ptr==Ptr` kind-pattern are correct). Deferred Bool arms (sound to
   omit): `not`'s `Eq(BitVec _:bv1,_)` & `Nop(Distinct,[l;r])`; the deep
   symbolic arms of `and_`/`or_`/`sem_eq` (the ~35-arm `sem_eq` tail incl.
   the msb-extract special case).

   **★ Soundness strategy — REFINED (read carefully).** Equivalence (§8) is
   by *evaluating* both modules under random concrete envs. The earlier
   claim "omitting simplifications is always sound" is **TRUE only for
   *total* operations**. It is **FALSE for *partial* ops (`div`/`rem`/
   `mod`)**: `Svalue` has *subterm-dropping* arms (`0*_→0`, `shl≥size→0`,
   `0 rem _→0`, ite/`&&`/`||` short-circuit, …) so it never *evaluates* —
   never *faults on* — the dropped subterm. A fold-only `Svalue_dsl` keeps
   that subterm and **does** div-by-zero-fault when it later becomes
   concrete-zero ⇒ the two disagree exactly when a dropped subterm would
   div0. Empirically confirmed by the §8 test (every mismatch involved
   div/rem/mod under a dropped subterm). Consequences:
   - For the **total fragment** (add/sub/mul/neg/bitand|or|xor/shl/lshr/
     ashr/lt/leq/eq/bool/ite/ptr) fold-only **is** provably equivalent —
     the §8 test passes (600k evals, 0 mismatches; see §8).
   - `div`/`rem`/`mod` must be transcribed **faithfully** (their
     subterm-dropping arms, in `Svalue`'s arm order) before re-admitting
     them to the recipe. Fold-only is *not* enough for them.
   So: grow the **total** ops fold-arm-first (mandatory) then symbolic
   arms (optional for §8, required for §11 polish); grow div/rem/mod
   **faithfully** (mandatory for §8).

10. **§8 bv differential test — standing & passing (total fragment).**
    `soteria/tests/bv_svalue_dsl/{dune,check.ml}` (mirrors tiny's
    `check.ml`): random well-typed terms over the total bitvector + bool +
    ptr fragment at width 8, built through `Bv_values.Svalue` *and*
    `Bv_values.Svalue_dsl`, evaluated under random envs (`Bv_values.Eval.
    eval` vs the generated `D.eval`), results compared. **0 mismatches over
    600k evaluations** (5 seeds × 20000 × 6); wired to
    `@soteria/tests/bv_svalue_dsl/runtest` (defaults 20000/seed 42). This is
    the equivalence proof for the covered fragment. div/rem/mod are excluded
    pending faithful transcription (★ above).

**Regression: clean.** Both `@soteria/tests/svalue_dsl/runtest` (tiny: 120k
evals + phantom `Typed`) and `@soteria/tests/bv_svalue_dsl/runtest` (bv:
120k evals) pass; the whole `soteria` library builds (bytecode + native).
`tiny_values/svalue_dsl.ml` uses no params, so behavior is byte-identical to
before — the redesign did not regress it.

---

## 6. DSL features

Listed in dependency order. **§6.D, §6.C, §6.E, §6.F, §6.G are DONE** (see
§5 "This session"); the descriptions below are kept for reference and now
carry a status note. §6.H remains optional. The only follow-up engine work
is the two §6.C deferrals (kind-patterns-in-rules; rules-based kinds) — both
naturally land while transcribing `sem_eq` in step 5.

### C. Generic structural kinds (`Ptr`, `Seq`, `Exists`)  — **DONE** (with two deferrals, see §5 item 4)

Generalize the hardcoded `Unop`/`Binop`/`Nop`/`Ite` handling. Add a `kind`
declaration:

```
kind Ptr   (t, t)            { rule ... }            (* or {{ body }} *)
kind Seq   (t list)          {{ fun l -> ... }}
kind Exists binder ((Var.t * ty) list, t) {{ fun bs body -> ... }}
```

Each kind has a payload of typed fields. Mark which fields are recursive
`t` / `t list` (so `iter_vars`/`hash` recurse) vs opaque. A `binder` kind
(`Exists`) declares which field is the bound-variable list and which is the
body, so generated `iter_vars` adds the bound vars to the ignore set before
recursing the body (study original `iter_vars`). Generated `pp`,
`equal_t_kind`/deriving, `hash`, and `eval` must handle the new kinds.
`Nop`/`Ite` should ideally be re-expressed as instances of this general
mechanism (reduces special-casing in `core_items`/`eval_items`). Smart ctor
either rules or `{{ body }}` (Ptr/Seq/Exists ctors in bv are simple).

Implementation seam: `core_items` (`iter_vars`/`hash`), `pp_fun`,
`kind_type`, `eval_items` currently switch on `has_un/has_bin/has_nop/
has_ite`. Replace with iteration over a `kinds` list of descriptors.

### D. Prelude escape (`prelude {{ ... }}`)  — **DONE** (§5 item 3)

Like `with {{ }}` but emitted at the **very top** of the generated structure
(before `ty_decl`). For `FloatPrecision`/`FloatClass`/`RoundingMode` (and
their `[@@deriving]`), which `ty`/`Unop`/`Binop` reference. Add `prelude`
keyword; in `generate`, prepend `prelude` blocks before `ty_decl`.

### E. Flexible result type  — **DONE** (§5 item 5)

Many bv ops compute their result `ty` from operands: `bv_concat` →
`TBitVector (size_of a + size_of b)`, `bv_extract[lo hi]` → `TBitVector
(hi-lo+1)`, most arithmetic → `v1.node.ty`. Today: `any` (= an arg's
`.node.ty`) or `T<R>` constant. Add a per-op result-type expression:

```
op bv_concat : Bv -> Bv -> Bv = BvConcat -> {{ fun a b -> TBitVector
   (size_of a.node.ty + size_of b.node.ty) }} { ... }
```

i.e. an optional `-> {{ fun <args> <params> -> <ty expr> }}` after the
constructor, used by `result_ty_expr`/`default_expr`/raw-build instead of
`econstr (ty_ctor ret)`. `any` stays as a shorthand.

### F. Parameterized literal constructors with normalization  — **DONE** (§5 item 6)

`BitVec` literals are size-carrying and **masked**: `mk n z = BitVec
Z.(z land pred (one lsl n)) <| TBitVector n`. The current `literal`
generates a fixed `int_z x = Int x <| TInt`. Extend `literal` to allow
parameters and a custom build expression:

```
literal BitVec : bv = Z.t  mk (n: int) {{ fun n z -> BitVec Z.(z land pred
   (one lsl n)) <| TBitVector n }}  print {{ Fmt.of_to_string (Z.format
   "%#x") }}
```

The literal-payload pattern (`#bv`) still binds the `Z.t`; the *size* comes
from the node's `ty` (`TBitVector n`) — see §G.

### G. Size/ty-aware constant folding  — **DONE for correctness; sugar deferred** (§5 item 7)

bv folding is masked, signed/unsigned, size-dependent. **Resolution:** no
new PPX feature is required. Write a fold arm as an ordinary rule whose
operands are **term vars** (whole nodes), with a `{{ }}` guard and a `{{ }}`
RHS that reads `.node.ty`/size, e.g. (in `bv_values/svalue_dsl.ml`):

```
op bv_add : BitVector -> BitVector -> BitVector = Add(checked: bool)
   -> {{ fun _checked a _b -> a.node.ty }} {
  rule bv_add a b
     ~> {{ bv_mk (size_of a.node.ty) Z.(to_z_exn a + to_z_exn b) }}
     when {{ is_bv_lit a && is_bv_lit b }}
}
```

with `is_bv_lit`/`to_z_exn` (and `size_of`) as one-liners in the in-module
`with {{ }}` block. This is verified working (correctly masked & sized).

**Optional ergonomic sugar (do this opportunistically in step 5, not
required):** a typed-literal pattern `#x@s` that binds the payload **and**
`s = size_of <node>.node.ty`, so the common `(BitVec l, BitVec r)` fold
arms read like `rule bv_add (#l@s) (#r@_) ~> {{ bv_mk s Z.(l+r) }}`.
Implementation seam: extend the lexer to accept `@<ident>` after a
`HASH_IDENT` (and its `:Kind`), `ELit` gains an optional size-var, and
`compile_pat`'s `ELit` case binds the kind payload **and** matches the
node's `ty` to recover the size (for `BitVec`, `ty = TBitVector s` binds
`s` directly; generalize via a per-literal "size-of-ty" expression). It is
**sugar only** — every fold is already expressible without it.

### H. (Optional) optional/labelled smart-ctor params

bv uses `add ?(checked=false)` / `div ~signed`. The DSL makes params
positional. **Not required for equivalence** (the differential test builds
via its own wrappers). Only needed if `svalue_dsl` must be an API drop-in.
If wanted: extend `op_params` with `?name:ty=default` / `~name` and emit
labelled/optional fun-args + thread through dispatchers.

---

## 7. `eval` and `Typed` specifics

- **eval**: the derived `eval` already exists and is generated from
  `mk_unop`/`mk_binop`/`mk_nop` + recursion. Once §6.C lands, ensure
  `eval_items` recurses correctly through `Ptr`/`Seq`/`Exists`/`Float`
  (Exists: evaluate body with bound vars treated as themselves; check
  `bv_values/eval.ml` and the original semantics). It should obsolete
  `bv_values/eval.ml`; keep that file or alias to the generated `eval`.
- **Typed**: extend `typed_module` to cover bv's `typed.mli`:
  parametric sorts (`'a sseq`), `hash_*` per sort, `cval`/`any` unions,
  the full glue surface (`get_ty`, `cast`, `type_checked`, `cast_checked2`,
  …) and per-op/lit phantom sigs (params prepended, already handled).
  The sealing-signature-as-text approach already works (`Ppxlib.Parse.
  interface`, bare items, `open T`, `type raw_t = t`/`raw_ty = ty` aliases
  in *both* sig and struct, abstract `+'a t`/`+'a ty`). Read `typed.mli`
  and mirror it.

---

## 8. Equivalence test (acceptance)

Mirror `soteria/tests/svalue_dsl/check.ml`. Equivalence is **semantic**, not
string-identity: cross-module commutative normal forms differ (allocation-
order dependent), so compare by *evaluating under random environments*.

Plan:

1. A pure recipe ADT over the bv ops, **tracking bitvector size** per
   subexpression (so generated terms are well-typed: same-size operands,
   correct `concat`/`extract` sizes). Include bool, bitvector, ptr;
   consider deferring `Float` (NaN/rounding make scalar comparison
   delicate — either bit-exact compare via the float's string repr, or
   exclude initially and note it).
2. A reference interpreter doing masked `Z` arithmetic (mod 2^n,
   signed/unsigned per op), the ground truth.
3. Build the symbolic term through both `Bv_values.Svalue` and the new
   `Bv_values.Svalue_dsl` (this runs all simplifications).
4. For many random environments, `eval` both to a concrete scalar and
   assert `S = D` (and optionally `= reference`). Handle build-time
   `Division_by_zero` from eager constant folding: require both raise or
   skip (see how `check.ml` does `\`Div0`).
5. Deterministic default seed; `(tests (names check ...) (libraries
   soteria))`.

`check.ml` is ~250 lines and directly adaptable. The recipe/size bookkeeping
is the only genuinely new part.

---

## 9. Workflow, gotchas, commands

- **Worktree**: you are in a git worktree. `dune` climbs to the main repo
  unless you pass `--root .`. Always:
  `dune build --root . <target>` / `dune build --root .
  @soteria/tests/svalue_dsl/runtest`.
- Build the PPX in isolation first: `dune build --root .
  soteria/ppx/ppx_symex.cma`.
- **Inspect expansion**: `soteria/tests/ppx/standalone.exe` is a full ppx
  driver (`Ppxlib.Driver.standalone`). Rebuild it after PPX changes
  (`dune build --root . soteria/tests/ppx/standalone.exe`), then
  `./_build/default/soteria/tests/ppx/standalone.exe -impl /tmp/x.ml` to
  see the generated source. Indispensable for debugging codegen.
- After **every** PPX change, re-run the tiny regression; it must stay
  green. tiny is the safety net proving you didn't break the engine.
- Generated code is wrapped in `[@@@ocaml.warning "-a"]`; redundant/
  unused match arms (from commutative mirroring) are expected and silenced.
- ppxlib is 0.38. Pitfalls already hit (don't re-introduce):
  - `pexp_function` is the Jane-Street arity form; use
    `pexp_function_cases` for `function ...`.
  - Metaquot cannot antiquote a *binding name* that then takes params
    (`let [%p ...] x = ...` is a syntax error); build with
    `B.value_binding`/`B.pexp_fun`.
  - `[@@deriving]` goes on the **last** decl of a `type ... and ...` group.
  - `Ppxlib.Parse.interface` wants **bare** signature items (no `sig ...
    end`); the sealed `Typed` sig needs `open T`, `type raw_t = t` /
    `type raw_ty = ty` declared in **both** the sig and the struct, and
    abstract `type +'a t` / `type +'a ty` (concrete `= t` is vacuous).
  - Lexer: a standalone `_` must lex as `SYM "_"` (wildcard), and
    `parse_app`'s argument-start set must include `SYM "_"`.
  - DSL grammar: `when` comes **after** `~>`; operator symbols are
    **unquoted** (`<=`, `&&`); word/symbol pp via `{{rem}}`/`{{/}}`.
- Don't aim for a syntactic drop-in. `svalue_dsl` is an independent module
  proven equivalent by the differential test, exactly like `tiny`.
- Don't try to make the generated `pp` faithful — equivalence is by `eval`,
  not pp. Saves enormous effort (skip the hex/`!=`/range printers).

---

## 10. Suggested execution order

1. ✅ **DONE** — §6.D prelude → `FloatPrecision`/`FloatClass`/`RoundingMode`
   ported; `ty` + `t_*`/`size_of`/`is_*`/`precision_of_f` live in the
   in-module `with {{ }}` (so result-ty `{{ }}` can use `size_of`).
2. ✅ **DONE** — §6.C generic kinds: `Ptr`/`Seq`/`Exists` declared (+ `Bool`/
   `BitVec`/`Float` literals, `Var` leaf). Type/kind/hashcons/iter_vars
   (binder-aware)/eval scaffolding generated. `soteria.cma` builds.
3. ✅ **DONE** — §6.E result-ty + §6.F/G bitvec literal/fold: `bv_concat`
   (size from operands) and `bv_add` (param `checked`, masked fold) are
   declared & verified. The current `bv_values/svalue_dsl.ml` is the
   started skeleton — **continue growing it from here**.
4. ✅ **DONE** — `Bool` core (`not`/`and_`/`or_`/`ite`/`sem_eq`)
   transcribed; the BitVec ops Bool depends on declared **fold-only**
   (sound). `soteria.cma` builds; tiny green. (Deferred Bool arms +
   soundness strategy: §5 item 9.)
5. ✅ **DONE (total fragment)** — full fold-only BitVec set declared
   (`bv_mul/and/or/xor/shl/lshr/ashr/div/rem/mod` added; `ptr`/`seq` glue
   in `with`). §8 stood up (step 7) — passes.
   **← NEXT (step 5 continued):**
   (a) Transcribe `div`/`rem`/`mod` **faithfully** (★ — subterm-dropping
       arms in `Svalue`'s order), then re-admit them to the §8 recipe
       (`gen_i` cases + un-comment the `Div/Rem/Mod` ctors) and reach 0
       mismatches.
   (b) Flesh out the remaining `BitVec` symbolic arms (~1370 lines) and
       all of `Float`, each: clean rule / two guarded rules for `if/else`
       RHS / small `when {{ }}`. The hard `sem_eq` tail
       (`BvExtract`/`Ite`/`BvConcat` nested patterns) uses nested
       parameterized-ctor payload binding (already works, cf.
       `not (bv_lt[s] a b)`); likely wants the optional `#x@s`
       typed-literal sugar (§6.G) for the `(BitVec _, TBitVector 1)` arms —
       add it then. Add Float to the §8 recipe last (bit-exact compare via
       the float's string repr, or keep deferred — handoff §8 allows).
   The §8 test guards every increment.
6. §7 Typed extension; mirror `bv_values/typed.mli` (bv declares no
   `sort`s yet ⇒ `typed_module` currently emits nothing for bv).
7. ✅ **DONE (total fragment)** — §8 equivalence test standing & passing
   (§5 item 10). Iterate to 0 mismatches as div/rem/mod/Float are added.
8. Replace `bv_values/eval.ml` usage with the derived `eval` (or alias);
   re-run the whole `soteria` build and existing test suites.

Each step ends with: `dune build --root . soteria/lib/soteria.cma` green +
tiny regression green + bv differential green.

---

## 11. Acceptance criteria  (status: 3 of 5 met; 2 partial/remaining)

- ◑ `soteria/lib/bv_values/svalue_dsl.ml` is a declarative DSL spec; escapes
  limited to helper prelude / structural-kind glue / numeric guards /
  fold bodies — **not** whole simplification arms. *True for what is
  transcribed (Bool core + fold-only BitVec); the remaining BitVec/Float
  symbolic arms are not yet written but the principle holds.*
- ◑ `soteria/tests/bv_svalue_dsl/` has a deterministic semantic differential
  test: `Bv_values.Svalue` ≡ `Bv_values.Svalue_dsl`, **0 mismatches over
  600k evals** — ✅ for the total bitvector+bool+ptr fragment; div/rem/mod
  & Float not yet covered (need faithful transcription, ★ §5).
- ✅ `tiny_values` regression still green (no engine regression).
- ☐ The generated `Typed` enforces phantom sorts — **remaining** (bv
  declares no `sort`s yet; §7 / step 6).
- ✅ Whole `soteria` library builds (bytecode + native).
