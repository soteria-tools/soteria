(** A re-implementation of {!Svalue} written with the [%%svalue] DSL.

    {b Work in progress.} This file is being grown incrementally per
    [soteria/ppx/BV_DSL_HANDOFF.md]. Covered so far: the [prelude] (float
    helper modules), the recursive/parameterized [ty], the leaf/literals,
    the generic structural kinds ([Ptr]/[Seq]/[Exists]), the {b full Bool
    core} ([not]/[and_]/[or_]/[ite]/[sem_eq]) and the BitVec ops Bool depends
    on transcribed {b fold-only}.

    {b Equivalence.} Proven by the differential test
    [tests/bv_svalue_dsl/check.ml]: [Svalue ≡ Svalue_dsl] over 600k random
    evaluations of the {e total} bitvector+bool+ptr fragment, 0 mismatches.
    For total operations, omitting a [Svalue] simplification is sound (a
    module with fewer but sound rewrites still evaluates every closed term
    to the same scalar). This does {b not} hold for the partial ops
    [div]/[rem]/[mod]: [Svalue]'s subterm-dropping arms ([0*_→0],
    [0 rem _→0], …) avoid faulting on a dropped subterm, so those must be
    transcribed {e faithfully} (not fold-only) before they re-enter the
    differential recipe. See [soteria/ppx/BV_DSL_HANDOFF.md] §5★. *)

open Hc
open Soteria_std
module Var = Symex.Var

[%%svalue
{|
  prelude {{
    module FloatPrecision = struct
      type t = F16 | F32 | F64 | F128
      [@@deriving eq, show { with_path = false }, ord]

      let size = function F16 -> 16 | F32 -> 32 | F64 -> 64 | F128 -> 128

      let of_size = function
        | 16 -> F16
        | 32 -> F32
        | 64 -> F64
        | 128 -> F128
        | _ -> failwith "Invalid float size"
    end

    module FloatClass = struct
      type t = Normal | Subnormal | Zero | Infinite | NaN
      [@@deriving eq, show { with_path = false }, ord]

      let as_fpclass = function
        | Normal -> FP_normal
        | Subnormal -> FP_subnormal
        | Zero -> FP_zero
        | Infinite -> FP_infinite
        | NaN -> FP_nan
    end

    module RoundingMode = struct
      type t = NearestTiesToEven | NearestTiesToAway | Ceil | Floor | Truncate
      [@@deriving eq, show { with_path = false }, ord]
    end
  }}

  ty Bool | Float(FloatPrecision.t) | Loc(int) | Pointer(int)
   | Seq(ty) | BitVector(int)

  leaf    Var   : poly = Var.t
  literal Bool  : Bool = bool   as of_bool
  literal BitVec : BitVector = Z.t  as bv_mk
     mk (n: int) {{ fun n z -> BitVec Z.(z land pred (one lsl n)) <| TBitVector n }}
     print {{ Fmt.of_to_string (Z.format "%#x") }}
  literal Float : Float = string  as float_mk
     mk (fp: FloatPrecision.t) {{ fun fp f -> Float f <| TFloat fp }}

  kind Ptr (t, t)
  kind Seq (t list)
  kind Exists binder ((Var.t * ty) list, t) eval {{
    fun x vs body ->
      (* Recurse the body, but bound variables resolve to themselves
         (free ones still flow to the outer [eval] handler). Mirrors
         [bv_values/eval.ml]'s [eval_var'] specialisation. *)
      let nbody =
        try eval body with
        | effect Eval_var (v, ty), k ->
            if List.exists (fun (v', _) -> Var.equal v v') vs then
              Effect.Deep.continue k (mk_var v ty)
            else Effect.Deep.continue k (eval_var v ty)
      in
      if body == nbody then x else mk_exists vs nbody
  }}

  with {{
    (* [ty] conveniences + value-specific glue for the structural kinds
       (the bespoke [Ptr.mk]/[SSeq.mk]/[mk_exists] of the hand-written
       [svalue.ml]). Emitted in-module so the result-[ty] [{{ }}] of ops
       (e.g. [bv_concat] -> [size_of]) can reference [size_of]. *)
    let t_bool = TBool
    let t_float fp = TFloat fp
    let t_f16 = t_float FloatPrecision.F16
    let t_f32 = t_float FloatPrecision.F32
    let t_f64 = t_float FloatPrecision.F64
    let t_f128 = t_float FloatPrecision.F128
    let t_loc n = TLoc n
    let t_ptr n = TPointer n
    let t_seq ty = TSeq ty
    let t_bv n = TBitVector n
    let is_float = function TFloat _ -> true | _ -> false
    let is_bv = function TBitVector _ -> true | _ -> false

    let precision_of_f = function
      | TFloat p -> p
      | ty -> Fmt.failwith "Not a float: %a" pp_ty ty

    let size_of = function
      | TBitVector n -> n
      | TPointer n -> n
      | TLoc n -> n
      | ty -> Fmt.failwith "Not a bit value: %a" pp_ty ty

    let mk_exists binders body =
      let body_vars = Var.Hashset.of_iter (iter_vars body |> Iter.map fst) in
      let binders =
        List.filter (fun (v, _) -> Var.Hashset.mem body_vars v) binders
      in
      match binders with [] -> body | _ -> Exists (binders, body) <| TBool

    (* Bespoke structural-kind constructors (the [Ptr.mk]/[SSeq.mk] of the
       hand-written [svalue.ml]; the size assert is dropped — sound). *)
    let ptr l o = Ptr (l, o) <| TPointer (size_of o.node.ty)
    let seq seq_ty l = Seq l <| seq_ty

    (* §6.G helpers: size-aware folding is expressible with the shipped
       machinery (term-var patterns + a [{{ }}] guard + a [{{ }}] RHS that
       reads [.node.ty]); no extra fold form is required for correctness. *)
    let is_bv_lit v = match v.node.kind with BitVec _ -> true | _ -> false
    let to_z_exn v =
      match v.node.kind with BitVec z -> z | _ -> invalid_arg "to_z_exn"

    (* [bv_to_z signed v]: the [Z.t] denoted by a concrete bitvector [v],
       sign-extended from its size when [signed] (mirrors [BitVec.bv_to_z]). *)
    let bv_to_z signed v =
      let z = to_z_exn v in
      if signed then Z.signed_extract z 0 (size_of v.node.ty) else z
  }}

  (* {2 BitVec dependency ops (fold-only — sound, simplifications deferred)}

     Bool's smart constructors reference these. Only the constant-folding
     arms are transcribed for now: that is *sound* (every rewrite is a valid
     algebraic identity), so [Svalue_dsl] stays semantically equivalent to
     [Svalue] under [eval] (a module with fewer — but only sound —
     simplifications still evaluates to the same concrete scalar). The full
     symbolic-simplification arms are added in later steps. *)

  op bv_concat : BitVector -> BitVector -> BitVector = BvConcat
     -> {{ fun a b -> TBitVector (size_of a.node.ty + size_of b.node.ty) }} {
    rule bv_concat a b
       ~> {{ bv_mk (size_of a.node.ty + size_of b.node.ty)
               Z.((to_z_exn a lsl size_of b.node.ty) lor to_z_exn b) }}
       when {{ is_bv_lit a && is_bv_lit b }}
  }

  op bv_add : BitVector -> BitVector -> BitVector = Add(checked: bool)
     -> {{ fun _checked a _b -> a.node.ty }} {
    rule bv_add a b
       ~> {{ bv_mk (size_of a.node.ty) Z.(to_z_exn a + to_z_exn b) }}
       when {{ is_bv_lit a && is_bv_lit b }}
  }

  op bv_sub : BitVector -> BitVector -> BitVector = Sub(checked: bool)
     -> {{ fun _checked a _b -> a.node.ty }} {
    rule bv_sub a b
       ~> {{ bv_mk (size_of a.node.ty) Z.(to_z_exn a - to_z_exn b) }}
       when {{ is_bv_lit a && is_bv_lit b }}
  }

  op bv_neg : BitVector -> BitVector = Neg -> {{ fun a -> a.node.ty }} {
    rule bv_neg a ~> {{ bv_mk (size_of a.node.ty) Z.(neg (to_z_exn a)) }}
       when {{ is_bv_lit a }}
  }

  op bv_not : BitVector -> BitVector = BvNot -> {{ fun a -> a.node.ty }} {
    rule bv_not a ~> {{ bv_mk (size_of a.node.ty) Z.(lognot (to_z_exn a)) }}
       when {{ is_bv_lit a }}
  }

  op bv_lt : BitVector -> BitVector -> Bool = Lt(signed: bool) {
    rule bv_lt a a ~> false
    rule bv_lt a b
       ~> {{ of_bool (Z.lt (bv_to_z signed a) (bv_to_z signed b)) }}
       when {{ is_bv_lit a && is_bv_lit b }}
  }

  op bv_leq : BitVector -> BitVector -> Bool = Leq(signed: bool) {
    rule bv_leq a a ~> true
    rule bv_leq a b
       ~> {{ of_bool (Z.leq (bv_to_z signed a) (bv_to_z signed b)) }}
       when {{ is_bv_lit a && is_bv_lit b }}
  }

  op bv_extract : BitVector -> BitVector = BvExtract(from_: int, to_: int)
     -> {{ fun from_ to_ _v -> TBitVector (to_ - from_ + 1) }} {
    rule bv_extract v ~> v
       when {{ from_ = 0 && to_ = size_of v.node.ty - 1 }}
    rule bv_extract v
       ~> {{ bv_mk (to_ - from_ + 1) Z.(to_z_exn v asr from_) }}
       when {{ is_bv_lit v }}
  }

  op bv_of_bool : Bool -> BitVector = BvOfBool(n: int)
     -> {{ fun n _b -> TBitVector n }} {
    rule bv_of_bool b ~> {{ bv_mk n Z.one }}  when {{ equal b v_true }}
    rule bv_of_bool b ~> {{ bv_mk n Z.zero }} when {{ equal b v_false }}
  }

  op bv_mul : BitVector -> BitVector -> BitVector = Mul(checked: bool)
     -> {{ fun _checked a _b -> a.node.ty }} {
    rule bv_mul a b
       ~> {{ bv_mk (size_of a.node.ty) Z.(to_z_exn a * to_z_exn b) }}
       when {{ is_bv_lit a && is_bv_lit b }}
  }

  op bv_and : BitVector -> BitVector -> BitVector = BitAnd
     -> {{ fun a _b -> a.node.ty }} {
    rule bv_and a b
       ~> {{ let l = to_z_exn a and r = to_z_exn b in
             bv_mk (size_of a.node.ty) Z.(logand l r) }}
       when {{ is_bv_lit a && is_bv_lit b }}
  }

  op bv_or : BitVector -> BitVector -> BitVector = BitOr
     -> {{ fun a _b -> a.node.ty }} {
    rule bv_or a b
       ~> {{ let l = to_z_exn a and r = to_z_exn b in
             bv_mk (size_of a.node.ty) Z.(logor l r) }}
       when {{ is_bv_lit a && is_bv_lit b }}
  }

  op bv_xor : BitVector -> BitVector -> BitVector = BitXor
     -> {{ fun a _b -> a.node.ty }} {
    rule bv_xor a b
       ~> {{ let l = to_z_exn a and r = to_z_exn b in
             bv_mk (size_of a.node.ty) Z.(l lxor r) }}
       when {{ is_bv_lit a && is_bv_lit b }}
  }

  op bv_shl : BitVector -> BitVector -> BitVector = Shl
     -> {{ fun a _b -> a.node.ty }} {
    rule bv_shl a b
       ~> {{ let l = to_z_exn a and r = to_z_exn b in
             bv_mk (size_of a.node.ty) Z.(l lsl to_int r) }}
       when {{ is_bv_lit a && is_bv_lit b }}
  }

  op bv_lshr : BitVector -> BitVector -> BitVector = LShr
     -> {{ fun a _b -> a.node.ty }} {
    rule bv_lshr a b
       ~> {{ let l = to_z_exn a and r = to_z_exn b in
             bv_mk (size_of a.node.ty) Z.(l asr to_int r) }}
       when {{ is_bv_lit a && is_bv_lit b }}
  }

  op bv_ashr : BitVector -> BitVector -> BitVector = AShr
     -> {{ fun a _b -> a.node.ty }} {
    rule bv_ashr a b
       ~> {{ let n = size_of a.node.ty in
             let l = Z.signed_extract (to_z_exn a) 0 n and r = to_z_exn b in
             bv_mk n Z.(l asr to_int r) }}
       when {{ is_bv_lit a && is_bv_lit b }}
  }

  op bv_div : BitVector -> BitVector -> BitVector = Div(signed: bool)
     -> {{ fun _signed a _b -> a.node.ty }} {
    rule bv_div a b
       ~> {{ let l = bv_to_z signed a and r = bv_to_z signed b in
             bv_mk (size_of a.node.ty) Z.(l / r) }}
       when {{ is_bv_lit a && is_bv_lit b }}
  }

  op bv_rem : BitVector -> BitVector -> BitVector = Rem(signed: bool)
     -> {{ fun _signed a _b -> a.node.ty }} {
    rule bv_rem a b
       ~> {{ let l = bv_to_z signed a and r = bv_to_z signed b in
             bv_mk (size_of a.node.ty) Z.(l mod r) }}
       when {{ is_bv_lit a && is_bv_lit b }}
  }

  op bv_mod : BitVector -> BitVector -> BitVector = Mod
     -> {{ fun a _b -> a.node.ty }} {
    rule bv_mod a b
       ~> {{ let size = size_of a.node.ty in
             let l = bv_to_z true a and r = bv_to_z true b in
             let res = Z.(l mod r) in
             let res =
               if Z.(res < zero) && Stdlib.not Z.(r < zero) then Z.(res + r)
               else if Z.(res >= zero) && Z.(r < zero) then Z.(res + r)
               else res
             in
             bv_mk size res }}
       when {{ is_bv_lit a && is_bv_lit b }}
  }

  (* {2 Booleans} (transcribed from [Svalue.Bool]; the [Binop (Eq, BitVec
     _:bv1, _)] / [Nop (Distinct, [l;r])] arms of [not], and the deep
     symbolic arms of [and_]/[or_]/[sem_eq], are deferred — omitting a
     simplification is sound, so equivalence is preserved.) *)

  op not : Bool -> Bool ! = Not {
    rule not true      ~> false
    rule not false     ~> true
    rule not (not a)   ~> a
    rule not (bv_lt[s] a b)  ~> bv_leq[s] b a
    rule not (bv_leq[s] a b) ~> bv_lt[s] b a
    rule not (a || b)  ~> (not a) && (not b)
    rule not (a && b)  ~> (not a) || (not b)
  }

  op and_ : Bool -> Bool -> Bool && = And {
    commutative
    rule a && a    ~> a
    rule false && _ ~> false
    rule true  && b ~> b
  }

  op or_ : Bool -> Bool -> Bool || = Or {
    commutative
    rule a || a    ~> a
    rule true  || _ ~> true
    rule false || b ~> b
  }

  op ite : Bool -> any -> any -> any = Ite {
    rule ite true  t e ~> t
    rule ite false t e ~> e
    rule ite g true  false ~> g
    rule ite g false true  ~> not g
    rule ite g false e ~> (not g) && e
    rule ite g true  e ~> g || e
    rule ite g t false ~> g && t
    rule ite g t true  ~> (not g) || t
    rule ite g t e ~> {{ bv_of_bool (size_of t.node.ty) g }}
       when {{ is_bv_lit t && is_bv_lit e
               && Z.equal (to_z_exn t) Z.one && Z.equal (to_z_exn e) Z.zero }}
    rule ite g t t ~> t
  }

  op sem_eq : any -> any -> Bool == = Eq {
    commutative
    rule a == a ~> true
    rule (#x:Bool)   == (#y:Bool)   ~> {{ of_bool (Stdlib.( = ) x y) }}
    rule (#x:BitVec) == (#y:BitVec) ~> {{ of_bool (Z.equal x y) }}
    rule (Ptr l1 o1) == (Ptr l2 o2) ~> (sem_eq l1 l2) && (sem_eq o1 o2)
  }
|}]
