open Soteria_std
open Hc
module Var = Soteria_symex.Var

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

module FloatRoundingMode = struct
  type t = NearestTiesToEven | NearestTiesToAway | Ceil | Floor | Truncate
  [@@deriving eq, show { with_path = false }, ord]
end

type ty =
  | TBool
  | TInt
  | TFloat of FloatPrecision.t
  | TLoc
  | TPointer
  | TSeq of ty
  | TBitVector of int
[@@deriving eq, show { with_path = false }, ord]

let t_bool = TBool
let t_int = TInt
let t_f fp = TFloat fp
let t_f16 = t_f F16
let t_f32 = t_f F32
let t_f64 = t_f F64
let t_f128 = t_f F128
let t_loc = TLoc
let t_ptr = TPointer
let t_seq ty = TSeq ty

(* NOTE: Currently, we do not have BitVectors as values, and we do not expose them to the
   interpreter. BVs are merely an artefact of the fact we can't do all operations we wish to
   only on Int+Float. As such, we must ensure that BV-typed values never leak! They may only
   exist as intermediate values. *)
let t_bv n = TBitVector n
let is_float = function TFloat _ -> true | _ -> false

let precision_of_f = function
  | TFloat p -> p
  | ty -> Fmt.failwith "Not a float: %a" pp_ty ty

let size_of_bv = function TBitVector n -> n | _ -> failwith "Not a bitvector"

module Nop = struct
  type t = Distinct [@@deriving eq, show { with_path = false }, ord]
end

module Unop = struct
  let equal_fpclass = ( = )
  let compare_fpclass = compare

  type t =
    | Not
    | FAbs
    | GetPtrLoc
    | GetPtrOfs
    | IntOfBool
    | BvOfFloat of int (* target bitvec size *)
    | BvOfInt of bool * int (* signed * target bitvec size *)
    | FloatOfBv of FloatPrecision.t
    | IntOfBv of bool (* signed *)
    | BvExtract of int * int (* from idx (incl) * to idx (incl) *)
    | BvExtend of bool * int (* signed * by N bits *)
    | BvNot
    | BvNegOvf
    | FIs of FloatClass.t
    | FRound of FloatRoundingMode.t
  [@@deriving eq, ord]

  let pp_signed ft b = Fmt.string ft (if b then "s" else "u")

  let pp ft = function
    | Not -> Fmt.string ft "!"
    | FAbs -> Fmt.string ft "abs."
    | GetPtrLoc -> Fmt.string ft "loc"
    | GetPtrOfs -> Fmt.string ft "ofs"
    | IntOfBool -> Fmt.string ft "b2i"
    | BvOfFloat n -> Fmt.pf ft "f2bv(%d)" n
    | BvOfInt (signed, n) -> Fmt.pf ft "i2bv[%a%d]" pp_signed signed n
    | FloatOfBv p -> Fmt.pf ft "bv2f[%a]" FloatPrecision.pp p
    | IntOfBv _ -> Fmt.string ft "bv2i"
    | BvExtract (from, to_) -> Fmt.pf ft "extract[%d-%d]" from to_
    | BvExtend (signed, by) -> Fmt.pf ft "extend[%a%d]" pp_signed signed by
    | BvNot -> Fmt.string ft "!bv"
    | BvNegOvf -> Fmt.string ft "-bv_ovf"
    | FIs fc -> Fmt.pf ft "fis(%a)" FloatClass.pp fc
    | FRound mode -> Fmt.pf ft "fround(%a)" FloatRoundingMode.pp mode
end

module Binop = struct
  type t =
    (* Bool *)
    | And
    | Or
    (* Comparison *)
    | Eq
    | Leq
    | Lt
    (* Arith *)
    | Plus
    | Minus
    | Times
    | Div
    | Rem
    | Mod (* Modulo, not remainder *)
    (* Float comparison *)
    | FEq
    | FLeq
    | FLt
    (* Float arith *)
    | FPlus
    | FMinus
    | FTimes
    | FDiv
    | FRem
    (* BitVector operators *)
    | BvPlus
    | BvMinus
    | BvTimes
    | BvDiv of bool (* signed *)
    | BvRem of bool (* signed *)
    | BvMod of bool (* signed *)
    | BvPlusOvf of bool (* signed *)
    | BvTimesOvf of bool (* signed *)
    | BvLt of bool (* signed *)
    | BvLeq of bool (* signed *)
    | BvConcat
    | BitAnd
    | BitOr
    | BitXor
    | BitShl
    | BitLShr
    | BitAShr
  [@@deriving eq, show { with_path = false }, ord]

  let pp_signed ft b = Fmt.string ft (if b then "s" else "u")

  let pp ft = function
    | And -> Fmt.string ft "&&"
    | Or -> Fmt.string ft "||"
    | Eq -> Fmt.string ft "=="
    | Leq -> Fmt.string ft "<="
    | Lt -> Fmt.string ft "<"
    | Plus -> Fmt.string ft "+"
    | Minus -> Fmt.string ft "-"
    | Times -> Fmt.string ft "*"
    | Div -> Fmt.string ft "/"
    | Rem -> Fmt.string ft "rem"
    | Mod -> Fmt.string ft "mod"
    | FEq -> Fmt.string ft "==."
    | FLeq -> Fmt.string ft "<=."
    | FLt -> Fmt.string ft "<."
    | FPlus -> Fmt.string ft "+."
    | FMinus -> Fmt.string ft "-."
    | FTimes -> Fmt.string ft "*."
    | FDiv -> Fmt.string ft "/."
    | FRem -> Fmt.string ft "rem."
    | BvPlus -> Fmt.string ft "+b"
    | BvMinus -> Fmt.string ft "-b"
    | BvTimes -> Fmt.string ft "*b"
    | BvDiv s -> Fmt.pf ft "/%ab" pp_signed s
    | BvRem s -> Fmt.pf ft "rem%ab" pp_signed s
    | BvMod s -> Fmt.pf ft "mod%ab" pp_signed s
    | BvPlusOvf s -> Fmt.pf ft "+%ab_ovf" pp_signed s
    | BvTimesOvf s -> Fmt.pf ft "*%ab_ovf" pp_signed s
    | BvLt s -> Fmt.pf ft "<%ab" pp_signed s
    | BvLeq s -> Fmt.pf ft "<=%ab" pp_signed s
    | BvConcat -> Fmt.string ft "++"
    | BitAnd -> Fmt.string ft "&"
    | BitOr -> Fmt.string ft "|"
    | BitXor -> Fmt.string ft "^"
    | BitShl -> Fmt.string ft "<<"
    | BitLShr -> Fmt.string ft "l>>"
    | BitAShr -> Fmt.string ft "a>>"
end

let pp_hash_consed pp_node ft t = pp_node ft t.node
let equal_hash_consed _ t1 t2 = Int.equal t1.tag t2.tag
let compare_hash_consed _ t1 t2 = Int.compare t1.tag t2.tag

type t_kind =
  | Var of Var.t
  | Bool of bool
  | Int of Z.t [@printer Fmt.of_to_string Z.to_string]
  | Float of string
  | Ptr of t * t
  | BitVec of Z.t [@printer Fmt.of_to_string (Z.format "%#x")]
  | Seq of t list
  | Unop of Unop.t * t
  | Binop of Binop.t * t * t
  | Nop of Nop.t * t list
  | Ite of t * t * t

and t_node = { kind : t_kind; ty : ty }
and t = t_node hash_consed [@@deriving show { with_path = false }, eq, ord]

let hash t = t.tag
let kind t = t.node.kind

let rec iter_vars (sv : t) (f : Var.t * ty -> unit) : unit =
  match sv.node.kind with
  | Var v -> f (v, sv.node.ty)
  | Bool _ | Int _ | Float _ | BitVec _ -> ()
  | Ptr (l, r) | Binop (_, l, r) ->
      iter_vars l f;
      iter_vars r f
  | Unop (_, sv) -> iter_vars sv f
  | Nop (_, l) | Seq l -> List.iter (fun sv -> iter_vars sv f) l
  | Ite (c, t, e) ->
      iter_vars c f;
      iter_vars t f;
      iter_vars e f

let pp_full ft t = pp_t_node ft t.node

let rec pp ft t =
  let open Fmt in
  match t.node.kind with
  | Var v -> pf ft "V%a" Var.pp v
  | Bool b -> pf ft "%b" b
  | Int z when Z.(z > of_int 2048 || z < of_int (-2048)) ->
      pf ft "%s" (Z.format "%#x" z)
  | Int z -> pf ft "%a" Z.pp_print z
  | Float f -> pf ft "%sf" f
  | BitVec bv -> pf ft "%s" (Z.format "%#x" bv)
  | Ptr (l, o) -> pf ft "&(%a, %a)" pp l pp o
  | Seq l -> pf ft "%a" (brackets (list ~sep:comma pp)) l
  | Ite (c, t, e) -> pf ft "(%a ? %a : %a)" pp c pp t pp e
  | Unop (Not, { node = { kind = Binop (Eq, v1, v2); _ }; _ }) ->
      pf ft "(%a != %a)" pp v1 pp v2
  | Unop (op, v) -> pf ft "%a(%a)" Unop.pp op pp v
  | Binop (op, v1, v2) -> pf ft "(%a %a %a)" pp v1 Binop.pp op pp v2
  | Nop (op, l) -> (
      let rec aux = function
        | acc, [] -> acc
        | Some l, { node = { kind = Var v; _ }; _ } :: rest ->
            aux (Some (Var.to_int v :: l), rest)
        | _, _ -> None
      in
      let range = aux (Some [], l) in
      let range =
        Option.bind range (fun l ->
            let l = List.sort Int.compare l in
            let min = List.hd l in
            let max = List.hd @@ List.rev l in
            if max - min + 1 = List.length l then Some (min, max) else None)
      in
      match range with
      | Some (min, max) -> pf ft "%a(V|%d-%d|)" Nop.pp op min max
      | None -> pf ft "%a(%a)" Nop.pp op (list ~sep:comma pp) l)

let[@inline] equal a b = Int.equal a.tag b.tag
let[@inline] compare a b = Int.compare a.tag b.tag

let rec sure_neq a b =
  (not (equal_ty a.node.ty b.node.ty))
  ||
  match (a.node.kind, b.node.kind) with
  | Int a, Int b | BitVec a, BitVec b -> not (Z.equal a b)
  | Bool a, Bool b -> a <> b
  | Ptr (la, oa), Ptr (lb, ob) -> sure_neq la lb || sure_neq oa ob
  | _ -> false

module Hcons = Hc.Make (struct
  type t = t_node

  let equal = equal_t_node

  (* We could do a lot more efficient in terms of hashing probably,
     if this ever becomes a bottleneck. *)
  let hash { kind; ty } =
    let hty = Hashtbl.hash ty in
    match kind with
    | Var _ | Bool _ | Int _ | Float _ | BitVec _ -> Hashtbl.hash (kind, hty)
    | Ptr (l, r) -> Hashtbl.hash (l.tag, r.tag, hty)
    | Seq l -> Hashtbl.hash (List.map (fun sv -> sv.tag) l, hty)
    | Unop (op, v) -> Hashtbl.hash (op, v.tag, hty)
    | Binop (op, l, r) -> Hashtbl.hash (op, l.tag, r.tag, hty)
    | Nop (op, l) -> Hashtbl.hash (op, List.map (fun sv -> sv.tag) l, hty)
    | Ite (c, t, e) -> Hashtbl.hash (c.tag, t.tag, e.tag, hty)
end)

let ( <| ) kind ty : t = Hcons.hashcons { kind; ty }
let mk_var v ty = Var v <| ty

(** We put commutative binary operators in some sort of normal form where
    element with the smallest id is on the LHS, to increase cache hits. *)
let mk_commut_binop op l r =
  if l.tag <= r.tag then Binop (op, l, r) else Binop (op, r, l)

(* TODO: substitution will break normal forms. *)
let rec subst subst_var sv =
  match sv.node.kind with
  | Var v -> mk_var (subst_var v) sv.node.ty
  | Bool _ | Int _ | Float _ | BitVec _ -> sv
  | Ptr (l, r) ->
      let l' = subst subst_var l in
      let r' = subst subst_var r in
      if equal l l' && equal r r' then sv else Ptr (l', r') <| TPointer
  | Seq l ->
      let changed = ref false in
      let l' =
        List.map
          (fun sv ->
            let new_sv = subst subst_var sv in
            if not (equal new_sv sv) then changed := true;
            new_sv)
          l
      in
      if !changed then Seq l' <| sv.node.ty else sv
  | Unop (op, v) ->
      let v' = subst subst_var v in
      if equal v v' then sv else Unop (op, v') <| sv.node.ty
  | Binop (op, l, r) ->
      let l' = subst subst_var l in
      let r' = subst subst_var r in
      if equal l l' && equal r r' then sv else Binop (op, l', r') <| sv.node.ty
  | Nop (op, l) ->
      let changed = ref false in
      let l' =
        List.map
          (fun sv ->
            let new_sv = subst subst_var sv in
            if not (equal new_sv sv) then changed := true;
            new_sv)
          l
      in
      if !changed then Nop (op, l') <| sv.node.ty else sv
  | Ite (c, t, e) ->
      let c' = subst subst_var c in
      let t' = subst subst_var t in
      let e' = subst subst_var e in
      if equal c c' && equal t t' && equal e e' then sv
      else Ite (c', t', e') <| sv.node.ty

(** {2 Booleans} *)

let v_true = Bool true <| TBool
let v_false = Bool false <| TBool

let as_bool t =
  if equal t v_true then Some true
  else if equal t v_false then Some false
  else None

let bool b =
  (* avoid re-alloc and re-hashconsing *)
  if b then v_true else v_false

let and_ v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, _ when equal v1 v2 -> v1
  | Bool false, _ | _, Bool false -> v_false
  | Bool true, _ -> v2
  | _, Bool true -> v1
  | _ -> mk_commut_binop And v1 v2 <| TBool

let or_ v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Bool true, _ | _, Bool true -> v_true
  | Bool false, _ -> v2
  | _, Bool false -> v1
  | _ -> mk_commut_binop Or v1 v2 <| TBool

let conj l = List.fold_left and_ v_true l

let rec not sv =
  if equal sv v_true then v_false
  else if equal sv v_false then v_true
  else
    match sv.node.kind with
    | Unop (Not, sv) -> sv
    | Binop (Lt, v1, v2) -> Binop (Leq, v2, v1) <| TBool
    | Binop (Leq, v1, v2) -> Binop (Lt, v2, v1) <| TBool
    | Binop (Or, v1, v2) -> mk_commut_binop And (not v1) (not v2) <| TBool
    | Binop (And, v1, v2) -> mk_commut_binop Or (not v1) (not v2) <| TBool
    | _ -> Unop (Not, sv) <| TBool

let rec split_ands (sv : t) (f : t -> unit) : unit =
  match sv.node.kind with
  | Binop (And, s1, s2) ->
      split_ands s1 f;
      split_ands s2 f
  | _ -> f sv

let distinct l =
  (* [Distinct l] when l is empty or of size 1 is always true *)
  match l with
  | [] | [ _ ] -> v_true
  | l ->
      let cross_product = List.to_seq l |> Seq.self_cross_product in
      let sure_distinct =
        Seq.for_all (fun (a, b) -> sure_neq a b) cross_product
      in
      if sure_distinct then v_true else Nop (Distinct, l) <| TBool

let ite guard if_ else_ =
  match guard.node.kind with
  | Bool true -> if_
  | Bool false -> else_
  | _ -> Ite (guard, if_, else_) <| if_.node.ty

(** {2 Integers} *)

let int_z z = Int z <| TInt
let int i = int_z (Z.of_int i)

let nonzero_z z =
  if Z.equal Z.zero z then raise (Invalid_argument "nonzero_z") else int_z z

let nonzero x = if x = 0 then raise (Invalid_argument "nonzero") else int x
let zero = int_z Z.zero
let one = int_z Z.one

let int_of_bool b =
  if equal v_true b then one
  else if equal v_false b then zero
  else Unop (IntOfBool, b) <| TInt

let rec plus v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, _ when equal v1 zero -> v2
  | _, _ when equal v2 zero -> v1
  | Int i1, Int i2 -> int_z (Z.add i1 i2)
  | Binop (Plus, v1, { node = { kind = Int i2; _ }; _ }), Int i3 ->
      plus v1 (int_z (Z.add i2 i3))
  | Binop (Plus, { node = { kind = Int i1; _ }; _ }, v2), Int i3 ->
      plus (int_z (Z.add i1 i3)) v2
  | Int i1, Binop (Plus, v1, { node = { kind = Int i2; _ }; _ }) ->
      plus (int_z (Z.add i1 i2)) v1
  | Int i1, Binop (Plus, { node = { kind = Int i2; _ }; _ }, v2) ->
      plus (int_z (Z.add i1 i2)) v2
  | _ -> mk_commut_binop Plus v1 v2 <| TInt

let rec minus v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, _ when equal v2 zero -> v1
  | Int i1, Int i2 -> int_z (Z.sub i1 i2)
  | Var v1, Var v2 when Var.equal v1 v2 -> zero
  | Binop (Minus, { node = { kind = Int i2; _ }; _ }, v1), Int i1 ->
      minus (int_z (Z.sub i2 i1)) v1
  | Binop (Minus, v1, { node = { kind = Int i2; _ }; _ }), Int i1 ->
      minus v1 (int_z (Z.sub i2 i1))
  | Binop (Plus, x, y), _ when equal x v2 -> y
  | Binop (Plus, x, y), _ when equal y v2 -> x
  | _, Binop (Plus, x, y) when equal x v1 -> y
  | _, Binop (Plus, x, y) when equal x v1 -> y
  | _ -> Binop (Minus, v1, v2) <| TInt

let times v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, _ when equal v1 zero || equal v2 zero -> zero
  | _, _ when equal v1 one -> v2
  | _, _ when equal v2 one -> v1
  | Int i1, Int i2 -> int_z (Z.mul i1 i2)
  | _ -> mk_commut_binop Times v1 v2 <| TInt

let div v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, _ when equal v2 one -> v1
  | Int i1, Int i2 -> int_z (Z.div i1 i2)
  | Binop (Times, v, { node = { kind = Int i; _ }; _ }), Int j
  | Int j, Binop (Times, v, { node = { kind = Int i; _ }; _ })
    when Z.equal i j ->
      v
  | Binop (Times, { node = { kind = Int i; _ }; _ }, v), Int j
  | Int j, Binop (Times, { node = { kind = Int i; _ }; _ }, v)
    when Z.equal i j ->
      v
  | _ -> Binop (Div, v1, v2) <| TInt

let rec is_mod v n =
  match v.node.kind with
  | Int i1 -> Z.equal (Z.( mod ) i1 n) Z.zero
  | Binop (Plus, v2, v3) -> is_mod v2 n && is_mod v3 n
  | Binop (Minus, v2, v3) -> is_mod v2 n && is_mod v3 n
  | Binop (Times, v2, v3) -> is_mod v2 n || is_mod v3 n
  | _ -> false

let rec rem v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, Int i2 when is_mod v1 i2 -> zero
  | Int i1, Int i2 -> int_z (Z.rem i1 i2)
  | Binop (Times, v1, n), Binop (Times, v2, m) when equal n m ->
      times n (rem v1 v2)
  | Binop (Times, n, v1), Binop (Times, m, v2) when equal n m ->
      times n (rem v1 v2)
  | _ -> Binop (Rem, v1, v2) <| v1.node.ty

let rec mod_ v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, _ when equal v2 one -> zero
  | _, Int i2 when is_mod v1 i2 -> zero
  | Int i1, Int i2 ->
      (* OCaml's mod computes the remainer... *)
      let rem = Z.( mod ) i1 i2 in
      if Z.lt rem Z.zero then int_z (Z.add rem i2) else int_z rem
  | Binop (Mod, x, { node = { kind = Int i1; _ }; _ }), Int i2
    when Z.geq i1 i2 && Z.divisible i1 i2 ->
      mod_ x v2
  | _ -> Binop (Mod, v1, v2) <| TInt

let neg v =
  match (v.node.ty, v.node.kind) with
  | TInt, Int i -> int_z (Z.neg i)
  | _ -> minus zero v

(** {2 Bit vectors}

    Bit vector operations; all these expect values of type [TInt], and apply
    bit-vector conversions under the hood as needed. This allow bit operations
    to be seamlessly used on [TInt], without worrying about the underlying
    representation.*)
module BitVec = struct
  let bool_or = or_
  let bool_and = and_
  let bool_not = not

  (** Raw operations for values of type [TBitVector]; be careful when using
      these, so as to provide properly typed values. Prefer using [BitVec]
      directly when possible, as it may also provide more reductions. *)
  module Raw = struct
    let mk n bv =
      assert (Z.(leq bv (one lsl n)));
      BitVec bv <| t_bv n

    let mk_masked n bv = mk n Z.(bv land pred (one lsl n))

    let mk_like other bv =
      assert (Z.(leq bv (one lsl size_of_bv other.node.ty)));
      BitVec bv <| other.node.ty

    (** [bv_to_z signed bits z] parses a BitVector [z], for a given bitwidth
        [bits], with [signed], into an integer. *)
    let bv_to_z signed bits z =
      let z = Z.(z land pred (one lsl bits)) in
      if signed then
        let bits_m_1 = bits - 1 in
        let max = Z.(pred (one lsl bits_m_1)) in
        if Z.leq z max then z else Z.(z - (one lsl bits))
      else z

    (** [max_for signed n] is the inclusive maximum for a bitvector of size [n]
        when it is [signed] *)
    let max_for signed n =
      let n = if signed then n - 1 else n in
      Z.(pred (one lsl n))

    (** [min_for signed n] is the inclusive minimum for a bitvector of size [n]
        when it is [signed] *)
    let min_for signed n =
      if signed then Z.(neg (one lsl Stdlib.( - ) n 1)) else Z.zero

    (** [is_2pow z] is [true] if [z] is a power of 2 {b greater than one}. The
        [> 1] check is done to ensure the mask for that number (ie. [z - 1]) is
        nonzero. *)
    let is_2pow z = Z.(z > one && popcount z = 1)

    (** [is_left_mask bits z] is true if [z] is of the form [1+0+] and its
        length is [bits] *)
    let is_left_mask bits z =
      if Z.(z <= one) then false
      else
        let size = Z.log2up z in
        let zeroes = size - Z.popcount z in
        if zeroes = 0 then false
        else if size <> bits then false
        else
          let mask = Z.pred @@ Z.shift_left Z.one zeroes in
          Z.(equal (z land mask) zero)

    (** [is_right_mask z] is true if [z] is of the form [0*1+] *)
    let is_right_mask z = Z.(z > zero && popcount (succ z) = 1)

    (** [right_mask_size z] is, for a [z] of the form [0*1{n}], [n]. If [z] is
        not of the form [0*1{n}], the result is undefined, so use
        [is_right_mask] before. *)
    let right_mask_size z = Z.(log2 (succ z))

    (** [covers_bitwidth bits z] is true if [z] is of the form [1+] and covers
        the whole bitwidth of size [bits]. *)
    let covers_bitwidth bits z = is_right_mask z && right_mask_size z = bits

    let rec plus v1 v2 =
      match (v1.node.kind, v2.node.kind) with
      | BitVec l, BitVec r -> mk_masked (size_of_bv v1.node.ty) Z.(l + r)
      | _, BitVec z when Z.equal z Z.zero -> v1
      | BitVec z, _ when Z.equal z Z.zero -> v2
      (* only propagate down ites if we know it's concrete *)
      | Ite (b, l, r), BitVec x | BitVec x, Ite (b, l, r) ->
          let x = mk_like v1 x in
          ite b (plus l x) (plus r x)
      | _ -> mk_commut_binop BvPlus v1 v2 <| v1.node.ty

    let rec minus v1 v2 =
      match (v1.node.kind, v2.node.kind) with
      | BitVec l, BitVec r -> mk_masked (size_of_bv v1.node.ty) Z.(l - r)
      | _, BitVec z when Z.equal z Z.zero -> v1
      (* only propagate down ites if we know it's concrete *)
      | Ite (b, l, r), BitVec _ -> ite b (minus l v2) (minus r v2)
      | BitVec _, Ite (b, l, r) -> ite b (minus v1 l) (minus v1 r)
      | _ -> Binop (BvMinus, v1, v2) <| v1.node.ty

    let rec times v1 v2 =
      match (v1.node.kind, v2.node.kind) with
      | BitVec l, BitVec r -> mk_masked (size_of_bv v1.node.ty) Z.(l * r)
      (* only propagate down ites if we know it's concrete *)
      | Ite (b, l, r), BitVec x | BitVec x, Ite (b, l, r) ->
          let x = mk_like v1 x in
          ite b (times l x) (times r x)
      | _ -> mk_commut_binop BvTimes v1 v2 <| v1.node.ty

    let div signed v1 v2 = Binop (BvDiv signed, v1, v2) <| v1.node.ty
    let rem signed v1 v2 = Binop (BvRem signed, v1, v2) <| v1.node.ty
    let mod_ signed v1 v2 = Binop (BvMod signed, v1, v2) <| v1.node.ty

    let rec not v =
      match v.node.kind with
      | BitVec bv ->
          let n = size_of_bv v.node.ty in
          mk_masked n Z.(lognot bv)
      | Ite (b, l, r) -> ite b (not l) (not r)
      | _ -> Unop (BvNot, v) <| v.node.ty

    let rec and_ v1 v2 =
      let n = size_of_bv v1.node.ty in
      assert (n == size_of_bv v2.node.ty);
      match (v1.node.kind, v2.node.kind) with
      | BitVec l, BitVec r -> mk n Z.(logand l r)
      | BitVec mask, _ when Z.(equal mask zero) -> v1
      | _, BitVec mask when Z.(equal mask zero) -> v2
      | BitVec mask, _ when covers_bitwidth n mask -> v2
      | _, BitVec mask when covers_bitwidth n mask -> v1
      (* i2bv[N](x) & 0x0*1{M} can become extend[unsigned, N](i2bv[M](x)) *)
      | Unop (BvOfInt (s, _), v1), BitVec mask when is_right_mask mask ->
          let mask_size = right_mask_size mask in
          let bv_of_int = Unop (BvOfInt (s, mask_size), v1) <| t_bv mask_size in
          extend false (n - mask_size) bv_of_int
      (* For (x >> s) & m, the mask is irrelevant if it entirely covers [bitsize - s] *)
      | ( (Binop
             ((BitLShr | BitAShr), _, { node = { kind = BitVec shift; _ }; _ })
           as base),
          BitVec mask )
      | ( BitVec mask,
          (Binop
             ((BitLShr | BitAShr), _, { node = { kind = BitVec shift; _ }; _ })
           as base) )
        when let bitwidth = n - Z.to_int shift in
             let low_mask = Z.(pred (one lsl bitwidth)) in
             Z.(equal (mask land low_mask) low_mask) ->
          base <| t_bv n
      | BitVec _, Ite (b, l, r) -> ite b (and_ v1 l) (and_ v1 r)
      | Ite (b, l, r), BitVec _ -> ite b (and_ l v2) (and_ r v2)
      (* if it's a right mask, it's usually beneficial to propagate it *)
      | (BitVec mask, Binop (BitAnd, l, r) | Binop (BitAnd, l, r), BitVec mask)
        when is_right_mask mask ->
          let mask = mk_like v1 mask in
          and_ (and_ mask l) (and_ mask r)
      | ( Ite (b1, l1, { node = { kind = BitVec r1; _ }; _ }),
          Ite (b2, l2, { node = { kind = BitVec r2; _ }; _ }) )
        when Z.(equal r1 zero) && Z.(equal r2 zero) ->
          ite (bool_and b1 b2) (and_ l1 l2) (mk_like v1 Z.zero)
      | _, _ -> mk_commut_binop BitAnd v1 v2 <| t_bv n

    and or_ v1 v2 =
      match (v1.node.kind, v2.node.kind) with
      | BitVec l, BitVec r -> mk_like v1 Z.(l lor r)
      | BitVec z, _ when Z.equal z Z.zero -> v2
      | _, BitVec z when Z.equal z Z.zero -> v1
      (* 0x0..0X..X | (0x0..0Y..Y << N) when N = |X..X| ==> 0x0..0Y..YX..X  *)
      | ( Unop (BvExtend (false, nx), base),
          Binop
            ( BitShl,
              { node = { kind = Unop (BvExtend (false, _), tail); _ }; _ },
              { node = { kind = BitVec shift; _ }; _ } ) )
        when Z.to_int shift = size_of_bv base.node.ty ->
          let tail_size = size_of_bv tail.node.ty in
          if nx = tail_size then concat tail base
          else if nx > tail_size then
            let new_base = concat tail base in
            extend false (nx - tail_size) new_base
          else
            let new_tail = extract 0 (nx - 1) tail in
            concat new_tail base
      | ( Ite
            ( b1,
              { node = { kind = BitVec l1; _ }; _ },
              { node = { kind = BitVec r1; _ }; _ } ),
          Ite
            ( b2,
              { node = { kind = BitVec l2; _ }; _ },
              { node = { kind = BitVec r2; _ }; _ } ) )
        when Z.(equal l1 one)
             && Z.(equal l2 one)
             && Z.(equal r1 zero)
             && Z.(equal r2 zero) ->
          ite (bool_or b1 b2) (mk_like v1 Z.one) (mk_like v1 Z.zero)
      | _ -> mk_commut_binop BitOr v1 v2 <| v1.node.ty

    and xor v1 v2 =
      match (v1.node.kind, v2.node.kind) with
      | BitVec l, BitVec r -> mk_like v1 Z.(l lxor r)
      | BitVec z, _ when Z.equal z Z.zero -> v2
      | _, BitVec z when Z.equal z Z.zero -> v1
      | ( Ite
            ( b1,
              { node = { kind = BitVec l1; _ }; _ },
              { node = { kind = BitVec r1; _ }; _ } ),
          Ite
            ( b2,
              { node = { kind = BitVec l2; _ }; _ },
              { node = { kind = BitVec r2; _ }; _ } ) )
        when Z.(equal l1 one)
             && Z.(equal l2 one)
             && Z.(equal r1 zero)
             && Z.(equal r2 zero) ->
          ite
            (bool_not (mk_commut_binop Eq b1 b2 <| TBool))
            (mk_like v1 Z.one) (mk_like v1 Z.zero)
      | _ -> mk_commut_binop BitXor v1 v2 <| v1.node.ty

    and extract from_ to_ v =
      let prev_size = size_of_bv v.node.ty in
      assert (0 <= from_ && from_ <= to_ && to_ < prev_size);
      let size = to_ - from_ + 1 in
      match v.node.kind with
      | BitVec bv -> mk_masked size Z.(bv asr from_)
      | Binop (((BitAnd | BitOr | BitXor) as bop), v1, v2) -> (
          let v1 = extract from_ to_ v1 in
          let v2 = extract from_ to_ v2 in
          match bop with
          | BitAnd -> and_ v1 v2
          | BitOr -> or_ v1 v2
          | BitXor -> xor v1 v2
          | _ -> failwith "unreachable binop")
      | Binop (BitLShr, v1, { node = { kind = BitVec x; _ }; _ }) ->
          (* we have to be careful to not extract bits that are out of bounds *)
          let shift = Z.to_int x in
          if to_ + shift < prev_size then
            (* 1. we can just shift the extraction *)
            extract (from_ + shift) (to_ + shift) v1
          else if from_ + shift >= prev_size then
            (* 2. the full shift is out of bounds! so 0 *)
            mk size Z.zero
          else
            (* 3. it's an in between - for now, we don't do anything *)
            Unop (BvExtract (from_, to_), v) <| t_bv size
      | Unop (BvOfInt (signed, _), v) when from_ = 0 ->
          Unop (BvOfInt (signed, size), v) <| t_bv size
      | Ite (b, l, r) ->
          let l = extract from_ to_ l in
          let r = extract from_ to_ r in
          ite b l r
      | Unop (BvExtend (false, by), _) when from_ >= prev_size - by ->
          (* zero extension, and we're extracting only the extended bits *)
          mk size Z.zero
      | Unop (BvExtend (_, by), v) when from_ = 0 && to_ = prev_size - by - 1 ->
          (* extracting exactly the original bits *)
          v
      | Unop (BvExtend (_, by), v) when to_ <= prev_size - by - 1 ->
          (* extracting from original bits *)
          extract from_ to_ v
      | Unop (BvExtract (prev_from_, _), v) ->
          Unop (BvExtract (prev_from_ + from_, prev_from_ + to_), v)
          <| t_bv size
      | _ -> Unop (BvExtract (from_, to_), v) <| t_bv size

    and extend signed extend_by v =
      let size = size_of_bv v.node.ty in
      let to_ = size + extend_by in
      assert (extend_by > 0);
      match v.node.kind with
      | BitVec bv ->
          let to_' = to_ + 1 in
          let bv = Z.(bv land pred (one lsl to_')) in
          mk to_ bv
      | Ite (b, l, r) ->
          let l = extend signed extend_by l in
          let r = extend signed extend_by r in
          ite b l r
      (* unlike with extract, we don't want to propagate extend within the expression for &, |, ^,
         as that will require a more expensive bit-blasting. *)
      (* We also note the following reduction is *not valid*, as some upper bits may be set;
         e.g. given i2bv[3](8) = 0b000, extend[1](i2bv[3](8)) = 0b0000, whereas
              i2bv[4](8) = 0b1000
      | Unop (BvOfInt, v) -> Unop (BvOfInt, v) <| t_bv signed to_ *)
      | _ -> Unop (BvExtend (signed, extend_by), v) <| t_bv to_

    (** [concat v1 v2] for [v1] of size [n] and [v2] of size [m] is a bitvector
        of size [n + m] where the first [m] bits are [v2] and the following [n]
        are [v1] *)
    and concat v1 v2 =
      let n1 = size_of_bv v1.node.ty in
      let n2 = size_of_bv v2.node.ty in
      match (v1.node.kind, v2.node.kind) with
      | BitVec l, BitVec r -> mk_masked (n1 + n2) Z.(r + shift_left l n2)
      | Unop (BvExtract (from1, to1), v1), Unop (BvExtract (from2, to2), v2)
        when to2 + 1 = from1 && equal v1 v2 ->
          extract from2 to1 v1
      (* We order (extract A ++ (X ++ extract )) *)
      | ( Unop (BvExtract (_, _), _),
          Binop
            ( BvConcat,
              ({ node = { kind = Unop (BvExtract _, _); _ }; _ } as left),
              right ) ) ->
          concat (concat v1 left) right
      | _, _ -> Binop (BvConcat, v1, v2) <| t_bv (n1 + n2)

    let rec shl v1 v2 =
      match (v1.node.kind, v2.node.kind) with
      | BitVec l, BitVec r ->
          mk_masked (size_of_bv v1.node.ty) Z.(l lsl to_int r)
      | _, BitVec s when Z.equal s Z.zero -> v1
      | _, BitVec s when Z.geq s (Z.of_int (size_of_bv v1.node.ty)) ->
          mk_like v1 Z.zero
      | Binop (BitShl, v, { node = { kind = BitVec s1; _ }; _ }), BitVec s2 ->
          shl v (mk_like v1 Z.(s1 + s2))
      | _ -> Binop (BitShl, v1, v2) <| v1.node.ty

    let rec lshr v1 v2 =
      match (v1.node.kind, v2.node.kind) with
      | BitVec l, BitVec r ->
          mk_masked (size_of_bv v1.node.ty) Z.(l asr to_int r)
      | _, BitVec s when Z.equal s Z.zero -> v1
      | _, BitVec s when Z.geq s (Z.of_int (size_of_bv v1.node.ty)) ->
          mk_like v1 Z.zero
      | Binop (BitLShr, v, { node = { kind = BitVec s1; _ }; _ }), BitVec s2 ->
          lshr v (mk_like v1 Z.(s1 + s2))
      | _ -> Binop (BitLShr, v1, v2) <| v1.node.ty

    let ashr v1 v2 =
      match (v1.node.kind, v2.node.kind) with
      | BitVec l, BitVec r ->
          mk_masked (size_of_bv v1.node.ty) Z.(l asr to_int r)
      | _, BitVec s when Z.equal s Z.zero -> v1
      | Binop (BitAShr, v, { node = { kind = BitVec s1; _ }; _ }), BitVec s2 ->
          lshr v (mk_like v1 Z.(s1 + s2))
      | _ -> Binop (BitAShr, v1, v2) <| v1.node.ty

    let rec lt signed v1 v2 =
      let bits = size_of_bv v1.node.ty in
      match (v1.node.kind, v2.node.kind) with
      | BitVec l, BitVec r ->
          bool @@ Z.lt (bv_to_z signed bits l) (bv_to_z signed bits r)
      | _, BitVec x when Stdlib.not signed && Z.(equal x one) ->
          (* unsigned x < 1 is x == 0 *)
          mk_commut_binop Eq v1 (mk_like v1 Z.zero) <| TBool
      | BitVec x, _ when Z.equal (bv_to_z signed bits x) (max_for signed bits)
        ->
          v_false
      | _, BitVec x when Z.equal (bv_to_z signed bits x) (min_for signed bits)
        ->
          v_false
      | Ite (b, l, r), _ -> ite b (lt signed l v2) (lt signed r v2)
      | _, Ite (b, l, r) -> ite b (lt signed v1 l) (lt signed v1 r)
      | _ -> Binop (BvLt signed, v1, v2) <| TBool

    let leq signed v1 v2 =
      let bits = size_of_bv v1.node.ty in
      match (v1.node.kind, v2.node.kind) with
      | BitVec l, BitVec r ->
          bool @@ Z.leq (bv_to_z signed bits l) (bv_to_z signed bits r)
      | BitVec x, _ when Z.equal (bv_to_z signed bits x) (min_for signed bits)
        ->
          v_true
      | _, BitVec x when Z.equal (bv_to_z signed bits x) (max_for signed bits)
        ->
          v_true
      | _ -> Binop (BvLeq signed, v1, v2) <| TBool

    let gt signed v1 v2 = lt signed v2 v1
    let geq signed v1 v2 = leq signed v2 v1
    let plus_overflows signed v1 v2 = Binop (BvPlusOvf signed, v1, v2) <| TBool
    let neg_overflows v = Unop (BvNegOvf, v) <| TBool

    let times_overflows signed v1 v2 =
      Binop (BvTimesOvf signed, v1, v2) <| TBool
  end

  (** [Some (signed, size)] if the given value contains somewhere some bitvector
      operations; [None] otherwise. *)
  let contains_bvs v =
    let exception Found of bool * int in
    let rec aux v =
      match v.node.kind with
      (* Found! *)
      | Unop (IntOfBv signed, bv) ->
          raise_notrace (Found (signed, size_of_bv bv.node.ty))
      (* These cases "kill" the search, as converting them to BVs has no advantage *)
      | Unop (IntOfBool, _) -> ()
      (* Iterate *)
      | BitVec _ | Var _ | Bool _ | Int _ | Float _ -> ()
      (* note here we ignore the condition of ite *)
      | Ptr (x, y) | Binop (_, x, y) | Ite (_, x, y) ->
          aux x;
          aux y
      | Unop (_, x) -> aux x
      | Nop (_, xs) | Seq xs -> List.iter aux xs
    in
    try
      aux v;
      None
    with Found (signed, size) -> Some (signed, size)

  let contains_bvs_2 v1 v2 =
    match contains_bvs v1 with Some _ as res -> res | None -> contains_bvs v2

  let of_float n v =
    match (v.node.ty, v.node.kind, n) with
    | TFloat _, Unop (FloatOfBv p, v), _ when FloatPrecision.size p = n -> v
    | TFloat F32, Float f, 32 ->
        let z = Z.of_int32 (Int32.bits_of_float (Float.of_string f)) in
        Raw.mk n z
    | TFloat F64, Float f, 64 ->
        let z = Z.of_int64 (Int64.bits_of_float (Float.of_string f)) in
        Raw.mk n z
    | _, _, _ -> Unop (BvOfFloat n, v) <| t_bv n

  (** [of_int signed size v] converts the value [v] of type [TInt] into an
      equivalent value of type [TBitVector size], assuming the representation is
      [signed]. *)
  let rec of_int s n v =
    let of_int = of_int s n in
    match v.node.kind with
    | Unop (IntOfBv _, v) ->
        let size = size_of_bv v.node.ty in
        if size = n then v
        else if size > n then Raw.extract 0 (n - 1) v
        else Raw.extend s (n - size) v
    | Int z ->
        (* need to mask otherwise we'll encode a value bigger than the bitwidth *)
        let mask = Z.pred @@ Z.shift_left Z.one n in
        let z = Z.(z land mask) in
        Raw.mk n z
    | Binop (Mod, v, { node = { kind = Int mask; _ }; _ })
      when Z.gt mask (Raw.max_for s n) ->
        of_int v
    | Binop (Mod, v, { node = { kind = Int mask; _ }; _ }) when Raw.is_2pow mask
      ->
        Raw.and_ (of_int v) (Raw.mk n (Z.pred mask))
    | Binop (Times, { node = { kind = Int mask; _ }; _ }, v)
      when Raw.is_2pow mask ->
        let fac = Raw.mk n (Z.of_int (Z.log2 mask)) in
        Raw.shl (of_int v) fac
    | Binop (Div, v, { node = { kind = Int mask; _ }; _ }) when Raw.is_2pow mask
      ->
        let fac = Raw.mk n (Z.of_int (Z.log2 mask)) in
        if s then Raw.ashr (of_int v) fac else Raw.lshr (of_int v) fac
    | Binop (Plus, l, r) -> Raw.plus (of_int l) (of_int r)
    | Binop (Minus, l, r) -> Raw.minus (of_int l) (of_int r)
    | Binop (Times, l, r) -> Raw.times (of_int l) (of_int r)
    | Binop (Div, l, r) -> Raw.div s (of_int l) (of_int r)
    | Binop (Rem, l, r) -> Raw.rem s (of_int l) (of_int r)
    | Binop (Mod, l, r) -> Raw.mod_ s (of_int l) (of_int r)
    | Unop (IntOfBool, v) -> Ite (v, Raw.mk n Z.one, Raw.mk n Z.zero) <| t_bv n
    | Ite (b, t, e) -> Ite (b, of_int t, of_int e) <| t_bv n
    | _ -> Unop (BvOfInt (s, n), v) <| t_bv n

  let rec to_int signed v =
    match v.node.kind with
    | BitVec z -> int_z @@ Raw.bv_to_z signed (size_of_bv v.node.ty) z
    | Binop (BitShl, l, { node = { kind = Int n; _ }; _ }) ->
        let pow = int_z @@ Z.shift_left Z.one (Z.to_int n) in
        times l pow
    | Binop (BitAShr, l, { node = { kind = Int n; _ }; _ }) when signed ->
        let pow = int_z @@ Z.shift_right Z.one (Z.to_int n) in
        div l pow
    | Binop (BitLShr, l, { node = { kind = Int n; _ }; _ })
      when Stdlib.not signed ->
        let pow = int_z @@ Z.shift_right Z.one (Z.to_int n) in
        div l pow
    | Binop (BitAnd, v, { node = { kind = BitVec mask; _ }; _ })
      when Raw.is_left_mask (size_of_bv v.node.ty) mask ->
        (* left mask, of the form 1+0+. e.g. for 1111 1000, this is equivalent to dividing by 2^3,
         and then re-multiplying, avoiding the bitvector.  *)
        let zeroes = Z.log2 mask - Z.popcount mask in
        let pow = int_z @@ Z.shift_left Z.one zeroes in
        let v = to_int signed v in
        times (div v pow) pow
    | Ite
        ( b,
          { node = { kind = BitVec l; _ }; _ },
          { node = { kind = BitVec r; _ }; _ } )
      when Z.equal l Z.one && Z.equal r Z.zero ->
        int_of_bool b
    | Ite
        ( b,
          { node = { kind = BitVec l; _ }; _ },
          { node = { kind = BitVec r; _ }; _ } )
      when Z.equal l Z.zero && Z.equal r Z.one ->
        int_of_bool (not b)
    | Ite (b, t, e) -> ite b (to_int signed t) (to_int signed e)
    | _ -> Unop (IntOfBv signed, v) <| t_int

  let to_float v =
    match v.node.kind with
    | Unop (BvOfFloat _, v) -> v
    | _ ->
        let size = size_of_bv v.node.ty in
        let fp = FloatPrecision.of_size size in
        Unop (FloatOfBv fp, v) <| t_f fp

  let and_ ~size ~signed v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | Int i1, Int i2 -> int_z (Z.( land ) i1 i2)
    | Bool b1, Bool b2 -> bool (b1 && b2)
    | _ ->
        let v1_bv = of_int signed size v1 in
        let v2_bv = of_int signed size v2 in
        let bv = Raw.and_ v1_bv v2_bv in
        to_int signed bv

  let or_ ~size ~signed v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | Int i1, Int i2 -> int_z (Z.( lor ) i1 i2)
    | Bool b1, Bool b2 -> bool (b1 || b2)
    | _ ->
        let v1_bv = of_int signed size v1 in
        let v2_bv = of_int signed size v2 in
        let bv = Raw.or_ v1_bv v2_bv in
        to_int signed bv

  let xor ~size ~signed v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | Int i1, Int i2 -> int_z (Z.( lxor ) i1 i2)
    | Bool b1, Bool b2 -> bool (b1 <> b2)
    | _ ->
        let v1_bv = of_int signed size v1 in
        let v2_bv = of_int signed size v2 in
        let bv = Raw.xor v1_bv v2_bv in
        to_int signed bv

  let not ~size ~signed v = of_int signed size v |> Raw.not |> to_int signed

  let[@inline] wrap_binop f =
   fun ~size ~signed v1 v2 ->
    let v1 = of_int signed size v1 in
    let v2 = of_int signed size v2 in
    let r = f v1 v2 in
    to_int signed r

  let shl = wrap_binop Raw.shl
  let ashr = wrap_binop Raw.ashr
  let lshr = wrap_binop Raw.lshr
  let wrap_plus = wrap_binop Raw.plus
  let wrap_minus = wrap_binop Raw.minus
  let wrap_times = wrap_binop Raw.times

  let[@inline] apply_binop_signed f =
   fun ~size ~signed v1 v2 ->
    let v1 = of_int signed size v1 in
    let v2 = of_int signed size v2 in
    f signed v1 v2

  let plus_overflows = apply_binop_signed Raw.plus_overflows
  let times_overflows = apply_binop_signed Raw.times_overflows
  let lt_as_bv = apply_binop_signed Raw.lt
  let leq_as_bv = apply_binop_signed Raw.leq

  let minus_overflows =
    apply_binop_signed @@ fun signed l r ->
    let size = size_of_bv l.node.ty in
    let neg_overflows = Raw.neg_overflows r in
    let r_neg = Raw.minus (Raw.mk size Z.zero) r in
    let add_overflows = Raw.plus_overflows signed l r_neg in
    bool_or neg_overflows add_overflows
end

(** {2 Floating point} *)
module Float = struct
  let mk fp f = Float f <| t_f fp
  let mk_f fp f = Float (Float.to_string f) <| t_f fp
  let like v f = Float (Float.to_string f) <| v.node.ty

  let fp_of v =
    match v.node.ty with
    | TFloat fp -> fp
    | _ -> Fmt.failwith "Unsupported float type"

  let f16 f = mk_f F16 f
  let f32 f = mk_f F32 f
  let f64 f = mk_f F64 f
  let f128 f = mk_f F128 f
  let eq v1 v2 = mk_commut_binop FEq v1 v2 <| TBool

  let lt v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | Float f1, Float f2 -> bool (float_of_string f1 < float_of_string f2)
    | _ -> Binop (FLt, v1, v2) <| TBool

  let leq v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | Float f1, Float f2 -> bool (float_of_string f1 <= float_of_string f2)
    | _ -> Binop (FLeq, v1, v2) <| TBool

  let gt v1 v2 = lt v2 v1
  let geq v1 v2 = leq v2 v1
  let plus v1 v2 = mk_commut_binop FPlus v1 v2 <| v1.node.ty
  let minus v1 v2 = Binop (FMinus, v1, v2) <| v1.node.ty
  let div v1 v2 = Binop (FDiv, v1, v2) <| v1.node.ty
  let times v1 v2 = mk_commut_binop FTimes v1 v2 <| v1.node.ty
  let rem v1 v2 = Binop (FRem, v1, v2) <| v1.node.ty

  let abs v =
    match v.node.kind with
    | Unop (FAbs, _) -> v
    | _ -> Unop (FAbs, v) <| v.node.ty

  let neg v =
    let fp = fp_of v in
    Binop (FMinus, mk fp "0.0", v) <| v.node.ty

  (* FIXME: all of these reductions are unsound for floats that aren't F64, I think *)
  let is_floatclass fc =
   fun sv ->
    match sv.node.kind with
    | Float f ->
        bool (FloatClass.as_fpclass fc = classify_float (float_of_string f))
    | _ -> Unop (FIs fc, sv) <| TBool

  let is_normal = is_floatclass Normal
  let is_subnormal = is_floatclass Subnormal
  let is_infinite = is_floatclass Infinite
  let is_nan = is_floatclass NaN
  let is_zero = is_floatclass Zero
  let round rm sv = Unop (FRound rm, sv) <| sv.node.ty

  let of_int fp v =
    match (v.node.kind, fp) with
    (* We force the integer to a float, to account for precision loss.
     Ideally we should do this for every float precision, but we would need support for
     f16, f32 and f128 in OCaml.  *)
    | Int i, FloatPrecision.F64 -> mk fp (string_of_float (Z.to_float i))
    | _ -> BitVec.to_float (BitVec.of_int true (FloatPrecision.size fp) v)

  let to_int n v =
    match v.node.kind with
    | Float f -> int_z (Z.of_float (Float.of_string f))
    | _ -> BitVec.to_int true (BitVec.of_float n v)
end

(* {2 Equality, comparison, int-bool conversions} *)

let rec lt v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Int i1, Int i2 -> bool (Z.lt i1 i2)
  | _, _ when equal v1 v2 -> v_false
  | _, Binop (Plus, v2, v3) when equal v1 v2 -> lt zero v3
  | _, Binop (Plus, v2, v3) when equal v1 v3 -> lt zero v2
  | Binop (Plus, v1, v3), _ when equal v1 v2 -> lt v3 zero
  | Binop (Plus, v1, v3), _ when equal v3 v2 -> lt v1 zero
  | Binop (Plus, v1, v2), Binop (Plus, v3, v4) when equal v1 v3 -> lt v2 v4
  | Binop (Plus, v1, v2), Binop (Plus, v3, v4) when equal v2 v3 -> lt v1 v4
  | Binop (Plus, v1, v2), Binop (Plus, v3, v4) when equal v1 v4 -> lt v2 v3
  | Binop (Plus, v1, v2), Binop (Plus, v3, v4) when equal v2 v4 -> lt v1 v3
  | Binop (Plus, v1, { node = { kind = Int x; _ }; _ }), Int y
  | Binop (Plus, { node = { kind = Int x; _ }; _ }, v1), Int y ->
      lt v1 (int_z @@ Z.sub y x)
  | Int y, Binop (Plus, v1, { node = { kind = Int x; _ }; _ })
  | Int y, Binop (Plus, { node = { kind = Int x; _ }; _ }, v1) ->
      lt (int_z @@ Z.sub y x) v1
  | Binop (Minus, v1, { node = { kind = Int x; _ }; _ }), Int y ->
      lt v1 (int_z @@ Z.add y x)
  | Binop (Minus, { node = { kind = Int x; _ }; _ }, v1), Int y ->
      lt (int_z @@ Z.sub x y) v1
  | Int y, Binop (Minus, v1, { node = { kind = Int x; _ }; _ }) ->
      lt (int_z @@ Z.add y x) v1
  | Int y, Binop (Minus, { node = { kind = Int x; _ }; _ }, v1) ->
      lt v1 (int_z @@ Z.sub x y)
  | Int y, Binop (Times, { node = { kind = Int x; _ }; _ }, v1')
  | Int y, Binop (Times, v1', { node = { kind = Int x; _ }; _ }) ->
      if Z.equal Z.zero x then bool (Z.lt y Z.zero)
      else
        let op = if Z.divisible y x || Z.(y > zero) then lt else leq in
        if Z.(zero < x) then op (int_z Z.(y / x)) v1'
        else op v1' (int_z Z.(y / x))
  | Binop (Times, v1', { node = { kind = Int x; _ }; _ }), Int y
  | Binop (Times, { node = { kind = Int x; _ }; _ }, v1'), Int y ->
      if Z.equal Z.zero x then bool (Z.lt Z.zero y)
      else
        let op = if Z.divisible y x || Z.(y < zero) then lt else leq in
        if Z.(zero < x) then op v1' (int_z Z.(y / x))
        else op (int_z Z.(y / x)) v1'
  | Binop ((Mod | Rem), _, { node = { kind = Int x; _ }; _ }), Int y
    when Z.leq x y ->
      v_true
  | Int y, Binop ((Mod | Rem), _, { node = { kind = Int x; _ }; _ })
    when Z.lt y (Z.neg (Z.abs x)) ->
      v_true
  | _ -> (
      match BitVec.contains_bvs_2 v1 v2 with
      | Some (signed, size) -> BitVec.lt_as_bv ~size ~signed v1 v2
      | None -> Binop (Lt, v1, v2) <| TBool)

and leq v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Int i1, Int i2 -> bool (Z.leq i1 i2)
  | _, _ when equal v1 v2 -> v_true
  | _, Binop (Plus, v2, v3) when equal v1 v2 -> leq zero v3
  | _, Binop (Plus, v2, v3) when equal v1 v3 -> leq zero v2
  | Binop (Plus, v1, v2), Binop (Plus, v3, v4) when equal v1 v3 -> leq v2 v4
  | Binop (Plus, v1, v2), Binop (Plus, v3, v4) when equal v2 v3 -> leq v1 v4
  | Binop (Plus, v1, v2), Binop (Plus, v3, v4) when equal v1 v4 -> leq v2 v3
  | Binop (Plus, v1, v2), Binop (Plus, v3, v4) when equal v2 v4 -> leq v1 v3
  | Binop (Plus, v1, { node = { kind = Int x; _ }; _ }), Int y
  | Binop (Plus, { node = { kind = Int x; _ }; _ }, v1), Int y ->
      leq v1 (int_z @@ Z.sub y x)
  | Int y, Binop (Plus, v1, { node = { kind = Int x; _ }; _ })
  | Int y, Binop (Plus, { node = { kind = Int x; _ }; _ }, v1) ->
      leq (int_z @@ Z.sub y x) v1
  | Binop (Minus, v1, { node = { kind = Int x; _ }; _ }), Int y ->
      leq v1 (int_z @@ Z.add y x)
  | Binop (Minus, { node = { kind = Int x; _ }; _ }, v1), Int y ->
      leq (int_z @@ Z.sub x y) v1
  | Int y, Binop (Minus, v1, { node = { kind = Int x; _ }; _ }) ->
      leq (int_z @@ Z.add y x) v1
  | Int y, Binop (Minus, { node = { kind = Int x; _ }; _ }, v1) ->
      leq v1 (int_z @@ Z.sub x y)
  | Int y, Binop (Times, { node = { kind = Int x; _ }; _ }, v1')
  | Int y, Binop (Times, v1', { node = { kind = Int x; _ }; _ }) ->
      if Z.equal Z.zero x then bool (Z.lt y Z.zero)
      else
        let op = if Z.divisible y x || Z.(y < zero) then leq else lt in
        if Z.(zero < x) then op (int_z Z.(y / x)) v1'
        else op v1' (int_z Z.(y / x))
  | Binop (Times, v1', { node = { kind = Int x; _ }; _ }), Int y
  | Binop (Times, { node = { kind = Int x; _ }; _ }, v1'), Int y ->
      if Z.equal Z.zero x then bool (Z.lt y Z.zero)
      else
        let op = if Z.divisible y x || Z.(y > zero) then leq else lt in
        if Z.(zero < x) then op v1' (int_z Z.(y / x))
        else op (int_z Z.(y / x)) v1'
  | Binop ((Mod | Rem), _, { node = { kind = Int x; _ }; _ }), Int y
    when Z.leq x y ->
      v_true
  | Int y, Binop ((Mod | Rem), _, { node = { kind = Int x; _ }; _ })
    when Z.lt y (Z.neg (Z.abs x)) ->
      v_true
  | _ -> (
      match BitVec.contains_bvs_2 v1 v2 with
      | Some (signed, size) -> BitVec.leq_as_bv ~signed ~size v1 v2
      | None -> Binop (Leq, v1, v2) <| TBool)

let geq v1 v2 = leq v2 v1
let gt v1 v2 = lt v2 v1

let rec sem_eq v1 v2 =
  if equal v1 v2 && Stdlib.not (is_float v1.node.ty) then v_true
  else
    match (v1.node.kind, v2.node.kind) with
    | Int z1, Int z2 -> bool (Z.equal z1 z2)
    | Bool b1, Bool b2 -> bool (b1 = b2)
    | Ptr (l1, o1), Ptr (l2, o2) -> and_ (sem_eq l1 l2) (sem_eq o1 o2)
    | _, Binop (Plus, v2, v3) when equal v1 v2 -> sem_eq v3 zero
    | _, Binop (Plus, v2, v3) when equal v1 v3 -> sem_eq v2 zero
    | Binop (Plus, v1, v3), _ when equal v1 v2 -> sem_eq v3 zero
    | Binop (Plus, v1, v3), _ when equal v3 v2 -> sem_eq v1 zero
    | Binop (Plus, v1, v2), Binop (Plus, v3, v4) when equal v1 v3 ->
        sem_eq v2 v4
    | Binop (Plus, v1, v2), Binop (Plus, v3, v4) when equal v1 v4 ->
        sem_eq v2 v3
    | Binop (Plus, v1, v2), Binop (Plus, v3, v4) when equal v2 v3 ->
        sem_eq v1 v4
    | Binop (Plus, v1, v2), Binop (Plus, v3, v4) when equal v2 v4 ->
        sem_eq v1 v3
    | Binop (Plus, v1, { node = { kind = Int x; _ }; _ }), Int y
    | Binop (Plus, { node = { kind = Int x; _ }; _ }, v1), Int y
    | Int y, Binop (Plus, v1, { node = { kind = Int x; _ }; _ })
    | Int y, Binop (Plus, { node = { kind = Int x; _ }; _ }, v1) ->
        sem_eq v1 (int_z @@ Z.sub y x)
    | Int y, Binop (Times, { node = { kind = Int x; _ }; _ }, v1)
    | Int y, Binop (Times, v1, { node = { kind = Int x; _ }; _ })
    | Binop (Times, v1, { node = { kind = Int x; _ }; _ }), Int y
    | Binop (Times, { node = { kind = Int x; _ }; _ }, v1), Int y ->
        if Z.equal Z.zero x then bool (Z.equal Z.zero y)
        else if Z.(equal zero (rem y x)) then sem_eq v1 (int_z Z.(y / x))
        else v_false
    | Unop (IntOfBool, v1), Int z -> if Z.equal Z.zero z then not v1 else v1
    (* Reduce  (X & #x...N) = #x...M to (X & #xN) = #xM *)
    | Binop (BitAnd, _, _), _ | _, Binop (BitAnd, _, _) -> (
        let rec msb_of v =
          match v.node.kind with
          | BitVec v when Z.(v > zero) -> Some (Z.log2up v)
          | BitVec v when Z.(equal v zero) -> Some 0
          | Binop (BitAnd, bv1, bv2) ->
              Option.merge min (msb_of bv1) (msb_of bv2)
          | _ -> None
        in
        let current_size = size_of_bv v1.node.ty in
        let msb = Option.map2 max (msb_of v1) (msb_of v2) in
        match msb with
        | Some msb when msb <> current_size - 1 ->
            let v1 = BitVec.Raw.extract 0 msb v1 in
            let v2 = BitVec.Raw.extract 0 msb v2 in
            sem_eq v1 v2
        | _ ->
            (* regular sem_eq *)
            if equal v1 v2 then v_true else mk_commut_binop Eq v1 v2 <| TBool)
    | Unop (IntOfBv _, bv1), Unop (IntOfBv _, bv2) -> sem_eq bv1 bv2
    | Unop (IntOfBv _, bv), Int n | Int n, Unop (IntOfBv _, bv) ->
        let size = size_of_bv bv.node.ty in
        let n = if Z.geq n Z.zero then n else Z.neg n in
        sem_eq bv (BitVec n <| t_bv size)
    | _ -> (
        let bit_vec =
          match (v1.node.ty, v2.node.ty) with
          | TInt, TInt -> BitVec.contains_bvs_2 v1 v2
          | _ -> None
        in
        match bit_vec with
        | Some (signed, n) ->
            let v1 = BitVec.of_int signed n v1 in
            let v2 = BitVec.of_int signed n v2 in
            sem_eq v1 v2
        | None -> mk_commut_binop Eq v1 v2 <| TBool)

let sem_eq_untyped v1 v2 =
  if equal_ty v1.node.ty v2.node.ty then sem_eq v1 v2 else v_false

(** Negates a boolean that is in integer form (i.e. 0 for false, anything else
    is true) *)
let not_int_bool sv =
  match sv.node.kind with
  | Int z -> if Z.equal z Z.zero then one else zero
  | Unop (IntOfBool, sv') -> int_of_bool (not sv')
  | _ -> int_of_bool (sem_eq sv zero)

let bool_of_int sv =
  match sv.node.kind with
  | Int z -> bool (Stdlib.not (Z.equal z Z.zero))
  | Unop (IntOfBool, sv') -> sv'
  | _ -> not (sem_eq sv zero)

(** {2 Pointers} *)

module Ptr = struct
  let mk l o = Ptr (l, o) <| TPointer

  let loc p =
    match p.node.kind with Ptr (l, _) -> l | _ -> Unop (GetPtrLoc, p) <| TLoc

  let null_loc = Int Z.zero <| TLoc
  let is_null_loc l = sem_eq l null_loc
  let loc_of_z z = Int z <| TLoc
  let loc_of_int i = loc_of_z (Z.of_int i)

  let ofs p =
    match p.node.kind with Ptr (_, o) -> o | _ -> Unop (GetPtrOfs, p) <| TInt

  let decompose p =
    match p.node.kind with Ptr (l, o) -> (l, o) | _ -> (loc p, ofs p)

  let add_ofs p o =
    let loc, ofs = decompose p in
    mk loc (plus ofs o)

  let null = mk null_loc zero
  let is_null p = sem_eq p null
  let is_at_null_loc p = is_null_loc (loc p)
end

(** {2 Sequences} *)

module SSeq = struct
  let mk ~seq_ty l = Seq l <| seq_ty

  let inner_ty ty =
    match ty with TSeq ty -> ty | _ -> failwith "Expected a sequence type"
end

(** {2 Infix operators} *)

module Infix = struct
  let int_z = int_z
  let int = int
  let ptr = Ptr.mk
  let seq = SSeq.mk
  let ( ==@ ) = sem_eq
  let ( ==?@ ) = sem_eq_untyped
  let ( >@ ) = gt
  let ( >=@ ) = geq
  let ( <@ ) = lt
  let ( <=@ ) = leq
  let ( &&@ ) = and_
  let ( ||@ ) = or_
  let ( +@ ) = plus
  let ( -@ ) = minus
  let ( ~- ) = neg
  let ( *@ ) = times
  let ( /@ ) = div
  let ( %@ ) = mod_
  let ( ==.@ ) = Float.eq
  let ( >.@ ) = Float.gt
  let ( >=.@ ) = Float.geq
  let ( <.@ ) = Float.lt
  let ( <=.@ ) = Float.leq
  let ( +.@ ) = Float.plus
  let ( -.@ ) = Float.minus
  let ( *.@ ) = Float.times
  let ( /.@ ) = Float.div
end

module Syntax = struct
  module Sym_int_syntax = struct
    let mk_nonzero = nonzero
    let zero = zero
    let one = one
  end
end
