open Hc
open Soteria_std
module Var = Symex.Var

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

type ty =
  | TBool
  | TFloat of FloatPrecision.t
  | TLoc of int
  | TPointer of int (* size of location and offset *)
  | TSeq of ty
  | TBitVector of int
[@@deriving eq, show { with_path = false }, ord]

let t_bool = TBool
let t_f fp = TFloat fp
let t_f16 = t_f F16
let t_f32 = t_f F32
let t_f64 = t_f F64
let t_f128 = t_f F128
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

module Nop = struct
  type t = Distinct [@@deriving eq, show { with_path = false }, ord]
end

module Unop = struct
  let equal_fpclass = ( = )
  let compare_fpclass = compare

  type t =
    | Not
    | GetPtrLoc
    | GetPtrOfs
    | BvOfBool of int (* target bitvec size *)
    | BvOfFloat of RoundingMode.t * bool * int (* signed * target bitvec size *)
    | FloatOfBv of
        RoundingMode.t * bool * FloatPrecision.t (* signed * precision *)
    | BvExtract of int * int (* from idx (incl) * to idx (incl) *)
    | BvExtend of bool * int (* signed * by N bits *)
    | BvNot
    | Neg
    | FAbs
    | FIs of FloatClass.t
    | FRound of RoundingMode.t
  [@@deriving eq, ord]

  let pp_signed ft b = Fmt.string ft (if b then "s" else "u")

  let pp ft = function
    | Not -> Fmt.string ft "!"
    | FAbs -> Fmt.string ft "abs."
    | GetPtrLoc -> Fmt.string ft "loc"
    | GetPtrOfs -> Fmt.string ft "ofs"
    | BvOfBool n -> Fmt.pf ft "b2bv[%d]" n
    | BvOfFloat (rm, signed, n) ->
        Fmt.pf ft "f2%abv[%a,%d]" pp_signed signed RoundingMode.pp rm n
    | FloatOfBv (rm, signed, p) ->
        Fmt.pf ft "%abv2f[%a,%a]" pp_signed signed RoundingMode.pp rm
          FloatPrecision.pp p
    | BvExtract (from, to_) -> Fmt.pf ft "extract[%d-%d]" from to_
    | BvExtend (signed, by) -> Fmt.pf ft "extend[%a%d]" pp_signed signed by
    | BvNot -> Fmt.string ft "!bv"
    | Neg -> Fmt.string ft "-"
    | FIs fc -> Fmt.pf ft "fis(%a)" FloatClass.pp fc
    | FRound mode -> Fmt.pf ft "fround(%a)" RoundingMode.pp mode
end

module Binop = struct
  type t =
    (* Bool *)
    | And
    | Or
    (* Comparison *)
    | Eq
    (* Float comparison *)
    | FEq
    | FLeq
    | FLt
    (* Float arith *)
    | FAdd
    | FSub
    | FMul
    | FDiv
    | FRem
    (* BitVector arithmetic *)
    | Add of bool (* was overflow checked? for optimisations only *)
    | Sub of bool (* was overflow checked? for optimisations only *)
    | Mul of bool (* was overflow checked? for optimisations only *)
    | Div of bool (* signed *)
    | Rem of bool (* signed *)
    | Mod
    | MulOvf of bool (* signed *)
    | Lt of bool (* signed *)
    | Leq of bool (* signed *)
    (* Bitvector bit operations *)
    | BvConcat
    | BitAnd
    | BitOr
    | BitXor
    | Shl
    | LShr
    | AShr
  [@@deriving eq, show { with_path = false }, ord]

  let pp_signed ft b = Fmt.string ft (if b then "s" else "u")
  let pp_checked ft b = if b then Fmt.string ft "ck"

  let pp ft = function
    | And -> Fmt.string ft "&&"
    | Or -> Fmt.string ft "||"
    | Eq -> Fmt.string ft "=="
    | FEq -> Fmt.string ft "==."
    | FLeq -> Fmt.string ft "<=."
    | FLt -> Fmt.string ft "<."
    | FAdd -> Fmt.string ft "+."
    | FSub -> Fmt.string ft "-."
    | FMul -> Fmt.string ft "*."
    | FDiv -> Fmt.string ft "/."
    | FRem -> Fmt.string ft "rem."
    | Add c -> Fmt.pf ft "+%a" pp_checked c
    | Sub c -> Fmt.pf ft "-%a" pp_checked c
    | Mul c -> Fmt.pf ft "*%a" pp_checked c
    | Div s -> Fmt.pf ft "/%a" pp_signed s
    | Rem s -> Fmt.pf ft "rem%a" pp_signed s
    | Mod -> Fmt.string ft "mod"
    | MulOvf s -> Fmt.pf ft "*%a_ovf" pp_signed s
    | Lt s -> Fmt.pf ft "<%a" pp_signed s
    | Leq s -> Fmt.pf ft "<=%a" pp_signed s
    | BvConcat -> Fmt.string ft "++"
    | BitAnd -> Fmt.string ft "&"
    | BitOr -> Fmt.string ft "|"
    | BitXor -> Fmt.string ft "^"
    | Shl -> Fmt.string ft "<<"
    | LShr -> Fmt.string ft "l>>"
    | AShr -> Fmt.string ft "a>>"
end

let pp_hash_consed pp_node ft t = pp_node ft t.node
let equal_hash_consed _ t1 t2 = Int.equal t1.tag t2.tag
let compare_hash_consed _ t1 t2 = Int.compare t1.tag t2.tag

type t_kind =
  | Var of Var.t
  | Bool of bool
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

let iter =
  let rec aux (f : t -> unit) (sv : t) : unit =
    f sv;
    match sv.node.kind with
    | Var _ | Bool _ | Float _ | BitVec _ -> ()
    | Ptr (l, r) | Binop (_, l, r) ->
        aux f l;
        aux f r
    | Unop (_, sv) -> aux f sv
    | Nop (_, l) | Seq l -> List.iter (aux f) l
    | Ite (c, t, e) ->
        aux f c;
        aux f t;
        aux f e
  in
  Fun.flip aux

let iter_vars (sv : t) (f : Var.t * ty -> unit) : unit =
  iter sv @@ fun sv ->
  match sv.node.kind with Var v -> f (v, sv.node.ty) | _ -> ()

let pp_full ft t = pp_t_node ft t.node

let rec pp ft t =
  let open Fmt in
  match t.node.kind with
  | Var v -> pf ft "V%a" Var.pp v
  | Bool b -> pf ft "%b" b
  | Float f -> pf ft "%sf" f
  | BitVec bv ->
      let size = size_of t.node.ty in
      if size mod 4 <> 0 then
        pf ft "0b%s" (Z.format ("0" ^ string_of_int size ^ "b") bv)
      else pf ft "0x%s" (Z.format ("0" ^ string_of_int (size / 4) ^ "x") bv)
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
  | BitVec a, BitVec b -> not (Z.equal a b)
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
    | Var _ | Bool _ | Float _ | BitVec _ -> Hashtbl.hash (kind, hty)
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
  | Bool _ | Float _ | BitVec _ -> sv
  | Ptr (l, r) ->
      let l' = subst subst_var l in
      let r' = subst subst_var r in
      if equal l l' && equal r r' then sv else Ptr (l', r') <| sv.node.ty
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

(** {2 Operator declarations} *)

module type Bool = sig
  val v_true : t
  val v_false : t
  val as_bool : t -> bool option
  val bool : bool -> t
  val and_ : t -> t -> t
  val or_ : t -> t -> t
  val conj : t list -> t
  val not : t -> t
  val split_ands : t -> t Iter.t
  val distinct : t list -> t
  val ite : t -> t -> t -> t
  val sem_eq : t -> t -> t
  val sem_eq_untyped : t -> t -> t
end

module type BitVec = sig
  (* constructor *)
  val mk : int -> Z.t -> t
  val mk_masked : int -> Z.t -> t
  val mki : int -> int -> t
  val zero : int -> t
  val one : int -> t
  val bv_to_z : bool -> int -> Z.t -> Z.t
  val to_z : t -> Z.t option
  val msb_of : t -> int

  (* arithmetic *)
  val add : ?checked:bool -> t -> t -> t
  val sub : ?checked:bool -> t -> t -> t
  val mul : ?checked:bool -> t -> t -> t
  val div : signed:bool -> t -> t -> t
  val rem : signed:bool -> t -> t -> t
  val mod_ : t -> t -> t
  val neg : t -> t

  (* overflow checks *)
  val add_overflows : signed:bool -> t -> t -> t
  val sub_overflows : signed:bool -> t -> t -> t
  val mul_overflows : signed:bool -> t -> t -> t
  val neg_overflows : t -> t

  (* inequalities *)
  val lt : signed:bool -> t -> t -> t
  val leq : signed:bool -> t -> t -> t
  val gt : signed:bool -> t -> t -> t
  val geq : signed:bool -> t -> t -> t

  (* bitvec manipulation *)
  val concat : t -> t -> t
  val extend : signed:bool -> int -> t -> t
  val extract : int -> int -> t -> t

  (* bitwise operations *)
  val and_ : t -> t -> t
  val or_ : t -> t -> t
  val xor : t -> t -> t
  val shl : t -> t -> t
  val lshr : t -> t -> t
  val ashr : t -> t -> t
  val not : t -> t

  (* bool-bv conversions *)
  val of_bool : int -> t -> t
  val to_bool : t -> t
  val not_bool : t -> t

  (* float-bv conversions *)
  val of_float : rounding:RoundingMode.t -> signed:bool -> size:int -> t -> t

  val to_float :
    rounding:RoundingMode.t -> signed:bool -> fp:FloatPrecision.t -> t -> t
end

module type Float = sig
  (* constructors *)
  val mk : FloatPrecision.t -> string -> t
  val f16 : float -> t
  val f32 : float -> t
  val f64 : float -> t
  val f128 : float -> t
  val like : t -> float -> t
  val fp_of : t -> FloatPrecision.t

  (* arithmetic *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t
  val abs : t -> t
  val neg : t -> t
  val round : RoundingMode.t -> t -> t

  (* comparisons *)
  val eq : t -> t -> t
  val lt : t -> t -> t
  val leq : t -> t -> t
  val gt : t -> t -> t
  val geq : t -> t -> t

  (* classification *)
  val is_floatclass : FloatClass.t -> t -> t
  val is_normal : t -> t
  val is_subnormal : t -> t
  val is_zero : t -> t
  val is_infinite : t -> t
  val is_nan : t -> t
end

(** {2 Booleans} *)

module rec Bool : Bool = struct
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
    | Binop (Eq, l1, r1), Binop (Eq, l2, r2)
      when (equal l1 l2 && sure_neq r1 r2)
           || (equal l1 r2 && sure_neq r1 l2)
           || (equal r1 l2 && sure_neq l1 r2)
           || (equal r1 r2 && sure_neq l1 l2) ->
        v_false
    | ( Binop (Eq, bv1, { node = { kind = Unop (BvExtract (s1, e1), x); _ }; _ }),
        Binop (Eq, bv2, { node = { kind = Unop (BvExtract (s2, e2), y); _ }; _ })
      )
      when equal x y && (e1 + 1 = s2 || e2 + 1 = s1) ->
        let bv, xy =
          if e1 + 1 = s2 then (BitVec.concat bv2 bv1, BitVec.extract s1 e2 x)
          else (BitVec.concat bv1 bv2, BitVec.extract s2 e1 x)
        in
        mk_commut_binop Eq bv xy <| TBool
    | _ -> mk_commut_binop And v1 v2 <| TBool

  let conj l = List.fold_left and_ v_true l

  let rec or_ v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | _, _ when equal v1 v2 -> v1
    | Bool true, _ | _, Bool true -> v_true
    | Bool false, _ -> v2
    | _, Bool false -> v1
    | Binop (Lt s1, l1, r1), Binop (Lt s2, l2, r2)
      when s1 = s2 && equal l1 r2 && equal r1 l2 ->
        not (mk_commut_binop Eq l1 r1 <| TBool)
    | Binop (Or, v1, v1'), _ when equal v1 v2 || equal v1' v2 -> v1
    | _, Binop (Or, v2, v2') when equal v1 v2 || equal v1 v2' -> v2
    | _ -> mk_commut_binop Or v1 v2 <| TBool

  and not sv =
    if equal sv v_true then v_false
    else if equal sv v_false then v_true
    else
      match sv.node.kind with
      | Unop (Not, sv) -> sv
      | Binop (Lt s, v1, v2) -> Binop (Leq s, v2, v1) <| TBool
      | Binop (Leq s, v1, v2) -> Binop (Lt s, v2, v1) <| TBool
      | Binop (Or, v1, v2) -> and_ (not v1) (not v2)
      | Binop (And, v1, v2) -> or_ (not v1) (not v2)
      | Binop (Eq, { node = { kind = BitVec bv; ty = TBitVector 1 }; _ }, v)
      | Binop (Eq, v, { node = { kind = BitVec bv; ty = TBitVector 1 }; _ }) ->
          mk_commut_binop Eq (BitVec.mk 1 Z.(one - bv)) v <| TBool
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
    match (guard.node.kind, if_.node.kind, else_.node.kind) with
    | Bool true, _, _ -> if_
    | Bool false, _, _ -> else_
    | _, Bool true, Bool false -> guard
    | _, Bool false, Bool true -> not guard
    | _, Bool false, _ -> and_ (not guard) else_
    | _, Bool true, _ -> or_ guard else_
    | _, _, Bool false -> and_ guard if_
    | _, _, Bool true -> or_ (not guard) if_
    | _, BitVec o, BitVec z when Z.(equal o one) && Z.equal z Z.zero ->
        BitVec.of_bool (size_of if_.node.ty) guard
    | _ when equal if_ else_ -> if_
    | _ -> Ite (guard, if_, else_) <| if_.node.ty

  let rec sem_eq v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | _ when equal v1 v2 -> v_true
    | Bool b1, Bool b2 -> bool (b1 = b2)
    | Ptr (l1, o1), Ptr (l2, o2) -> and_ (sem_eq l1 l2) (sem_eq o1 o2)
    | BitVec b1, BitVec b2 -> bool (Z.equal b1 b2)
    (* Arithmetics *)
    | BitVec _, Unop (Neg, v2) -> sem_eq (BitVec.neg v1) v2
    | Unop (Neg, v1), BitVec _ -> sem_eq v1 (BitVec.neg v2)
    | BitVec _, Binop (Add _, ({ node = { kind = BitVec _; _ }; _ } as l), r)
    | BitVec _, Binop (Add _, r, ({ node = { kind = BitVec _; _ }; _ } as l)) ->
        sem_eq (BitVec.sub v1 l) r
    | Binop (Add _, ({ node = { kind = BitVec _; _ }; _ } as l), r), BitVec _
    | Binop (Add _, r, ({ node = { kind = BitVec _; _ }; _ } as l)), BitVec _ ->
        sem_eq (BitVec.sub v2 l) r
    | BitVec _, Binop (Sub _, l, ({ node = { kind = BitVec _; _ }; _ } as r)) ->
        sem_eq (BitVec.add v1 r) l
    | BitVec _, Binop (Sub _, ({ node = { kind = BitVec _; _ }; _ } as l), r) ->
        sem_eq (BitVec.sub l v1) r
    | Binop (Sub _, l, ({ node = { kind = BitVec _; _ }; _ } as r)), BitVec _ ->
        sem_eq (BitVec.add v2 r) l
    | Binop (Sub _, ({ node = { kind = BitVec _; _ }; _ } as l), r), BitVec _ ->
        sem_eq (BitVec.sub l v2) r
    | BitVec n, Binop (Mul true, { node = { kind = BitVec m; _ }; _ }, x)
    | BitVec n, Binop (Mul true, x, { node = { kind = BitVec m; _ }; _ })
    | Binop (Mul true, { node = { kind = BitVec m; _ }; _ }, x), BitVec n
    | Binop (Mul true, x, { node = { kind = BitVec m; _ }; _ }), BitVec n ->
        if Z.(equal m zero) then bool (Z.equal n Z.zero)
        else if Z.(equal n zero) then sem_eq x (BitVec.zero (size_of x.node.ty))
        else if Z.(divisible n m) then
          sem_eq x (BitVec.mk (size_of x.node.ty) Z.(n / m))
        else v_false
    | ( Binop (Mul _, { node = { kind = BitVec a; _ }; _ }, b),
        Binop (Mul _, { node = { kind = BitVec c; _ }; _ }, d) )
      when Z.(equal a c && Stdlib.not (equal a zero)) ->
        sem_eq b d
    | ( Binop (Mul _, b, { node = { kind = BitVec a; _ }; _ }),
        Binop (Mul _, d, { node = { kind = BitVec c; _ }; _ }) )
      when Z.(equal a c && Stdlib.not (equal a zero)) ->
        sem_eq b d
    | ( Binop (Mul _, { node = { kind = BitVec a; _ }; _ }, b),
        Binop (Mul _, d, { node = { kind = BitVec c; _ }; _ }) )
      when Z.(equal a c && Stdlib.not (equal a zero)) ->
        sem_eq b d
    | ( Binop (Mul _, b, { node = { kind = BitVec a; _ }; _ }),
        Binop (Mul _, { node = { kind = BitVec c; _ }; _ }, d) )
      when Z.(equal a c && Stdlib.not (equal a zero)) ->
        sem_eq b d
    (* Bitvectors *)
    | (BitVec _ as z), Binop (BvConcat, l, r)
    | Binop (BvConcat, l, r), (BitVec _ as z) ->
        let z = z <| v1.node.ty in
        let size_r = size_of r.node.ty in
        let size_l = size_of l.node.ty in
        let z_r = BitVec.extract 0 (size_r - 1) z in
        let z_l = BitVec.extract size_r (size_r + size_l - 1) z in
        and_ (sem_eq l z_l) (sem_eq r z_r)
    (* ite(b, A::B, C::D) == l :: r <=> ite(b, A, C) == l && ite(b, B, D) == r *)
    | ( Ite
          ( b,
            ({ node = { kind = BitVec _; _ }; _ } as t),
            ({ node = { kind = BitVec _; _ }; _ } as e) ),
        Binop (BvConcat, l, r) )
    | ( Binop (BvConcat, l, r),
        Ite
          ( b,
            ({ node = { kind = BitVec _; _ }; _ } as t),
            ({ node = { kind = BitVec _; _ }; _ } as e) ) ) ->
        let size_r = size_of r.node.ty in
        let size_l = size_of l.node.ty in
        let t_r = BitVec.extract 0 (size_r - 1) t in
        let t_l = BitVec.extract size_r (size_r + size_l - 1) t in
        let e_r = BitVec.extract 0 (size_r - 1) e in
        let e_l = BitVec.extract size_r (size_r + size_l - 1) e in
        and_ (sem_eq (ite b t_l e_l) l) (sem_eq (ite b t_r e_r) r)
    | Binop (BvConcat, l1, r1), Binop (BvConcat, l2, r2)
      when size_of l1.node.ty = size_of l2.node.ty ->
        and_ (sem_eq l1 l2) (sem_eq r1 r2)
    (* BvOfBool and If-then-elses *)
    | Ite (b, l, t), (BitVec _ | Bool _) -> ite b (sem_eq l v2) (sem_eq t v2)
    | (BitVec _ | Bool _), Ite (b, l, t) -> ite b (sem_eq v1 l) (sem_eq v1 t)
    | Bool false, _ -> not v2
    | _, Bool false -> not v1
    | Bool true, _ -> v2
    | _, Bool true -> v1
    | Unop (BvOfBool _, b), Unop (BvOfBool _, c) -> sem_eq b c
    | Unop (Not, b), Unop (Not, c) -> sem_eq b c
    | Unop (BvOfBool _, b), BitVec z | BitVec z, Unop (BvOfBool _, b) ->
        if Z.equal z Z.one then b
        else if Z.equal z Z.zero then not b
        else v_false
    (* special case: for BVs, check if we can infer the most significant set bits and extract *)
    | _ when is_bv v1.node.ty && is_bv v2.node.ty ->
        let current_size = size_of v1.node.ty in
        let msb = max (BitVec.msb_of v1) (BitVec.msb_of v2) in
        if 0 < msb && msb < current_size - 1 then
          let v1' = BitVec.extract 0 msb v1 in
          let v2' = BitVec.extract 0 msb v2 in
          sem_eq v1' v2'
        else
          (* regular sem_eq *)
          mk_commut_binop Eq v1 v2 <| TBool
    | _ -> mk_commut_binop Eq v1 v2 <| TBool

  let sem_eq_untyped v1 v2 =
    if equal_ty v1.node.ty v2.node.ty then sem_eq v1 v2 else v_false
end

(** {2 Bit vectors} *)
and BitVec : BitVec = struct
  let mk n bv =
    assert (Z.(zero <= bv && bv < one lsl n));
    BitVec bv <| t_bv n

  let mk_masked n bv = mk n Z.(bv land pred (one lsl n))
  let mki n i = mk n (Z.of_int i)
  let zero n = mk n Z.zero
  let one n = mk n Z.one

  (** [bv_to_z signed bits z] parses a BitVector [z], for a given bitwidth
      [bits], with [signed], into an integer. *)
  let bv_to_z signed bits z =
    let z = Z.(z land pred (one lsl bits)) in
    if signed then
      let bits_m_1 = bits - 1 in
      let max = Z.(pred (one lsl bits_m_1)) in
      if Z.leq z max then z else Z.(z - (one lsl bits))
    else z

  let to_z v = match v.node.kind with BitVec z -> Some z | _ -> None

  (** [max_for signed n] is the inclusive maximum for a bitvector of size [n]
      when it is [signed] *)
  let max_for signed n =
    let n = if signed then n - 1 else n in
    Z.(pred (one lsl n))

  (** [min_for signed n] is the inclusive minimum for a bitvector of size [n]
      when it is [signed] *)
  let min_for signed n =
    if signed then Z.(neg (one lsl Stdlib.( - ) n 1)) else Z.zero

  (** [is_right_mask z] is true if [z] is of the form [0*1+] *)
  let is_right_mask z = Z.(z > zero && popcount (succ z) = 1)

  (** [right_mask_size z] is, for a [z] of the form [0*1{n}], [n]. If [z] is not
      of the form [0*1{n}], the result is undefined, so use [is_right_mask]
      before. *)
  let right_mask_size z = Z.(log2 (succ z))

  (** [covers_bitwidth bits z] is true if [z] is of the form [1+] and covers the
      whole bitwidth of size [bits]. *)
  let covers_bitwidth bits z = is_right_mask z && right_mask_size z = bits

  let is_pow2 z = Z.(gt z zero && popcount z = 1)

  (** [lsb z] returns the least significant bit of z that is set, or 128 if z is
      0. *)
  let lsb z = if Z.equal z Z.zero then 128 else Z.(logand z (neg z) |> log2)

  let rec msb_of v =
    match v.node.kind with
    | BitVec v when Z.(v > zero) -> Z.log2 v
    | BitVec v when Z.(equal v zero) -> 0
    | Binop (BitAnd, bv1, bv2) -> min (msb_of bv1) (msb_of bv2)
    | Ite (_, l, r) -> max (msb_of l) (msb_of r)
    | Unop (BvExtend (false, __), v) -> msb_of v
    | _ -> size_of v.node.ty - 1

  let overflows ~signed n l r op =
    let minz = min_for signed n in
    let maxz = max_for signed n in
    let l = bv_to_z signed n l in
    let r = bv_to_z signed n r in
    let res = op l r in
    Z.Compare.(res < minz || res > maxz)

  let ovf_check ~signed n l r op = Bool.bool @@ overflows ~signed n l r op

  let of_bool n b =
    if equal Bool.v_true b then one n
    else if equal Bool.v_false b then zero n
    else Unop (BvOfBool n, b) <| TBitVector n

  let to_bool v =
    match v.node.kind with
    | BitVec z -> Bool.bool (Stdlib.not (Z.equal z Z.zero))
    | Unop (BvOfBool _, sv') -> sv'
    | _ -> Bool.not (Bool.sem_eq v (zero (size_of v.node.ty)))

  let not_bool v =
    let n = size_of v.node.ty in
    match v.node.kind with
    | BitVec z -> if Z.equal z Z.zero then one n else zero n
    | Unop (BvOfBool n, g) -> of_bool n (Bool.not g)
    | _ -> of_bool n (Bool.sem_eq v (zero n))

  let rec add ?(checked = false) v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | BitVec l, BitVec r -> mk_masked (size_of v1.node.ty) Z.(l + r)
    | Unop (Neg, v1), _ -> sub v2 v1
    | _, Unop (Neg, v2) -> sub v1 v2
    | _, BitVec z when Z.equal z Z.zero -> v1
    | BitVec z, _ when Z.equal z Z.zero -> v2
    | (BitVec z, Unop (BvNot, v) | Unop (BvNot, v), BitVec z)
      when Z.equal z Z.one ->
        neg v
    | Binop (Add _, ({ node = { kind = BitVec _; _ }; _ } as c1), r), BitVec _
    | Binop (Add _, r, ({ node = { kind = BitVec _; _ }; _ } as c1)), BitVec _
      ->
        add (add c1 v2) r
    | Binop (Sub _, l, ({ node = { kind = BitVec _; _ }; _ } as c1)), BitVec _
      ->
        add l (sub v2 c1)
    | Binop (Sub _, ({ node = { kind = BitVec _; _ }; _ } as c1), r), BitVec _
      ->
        sub (add c1 v2) r
    | _, Binop (Sub _, l, r) when equal r v1 -> l
    | Binop (Sub _, l, r), _ when equal r v2 -> l
    | Binop (Mul _, l1, r1), Binop (Mul _, l2, r2)
      when equal l1 l2 || equal l1 r2 || equal r1 l2 || equal r1 r2 ->
        (* FIXME: remove true; this is not actually checked, this would rather depend on
           if the addition is checked (which we dont track) *)
        if equal l1 l2 then Binop (Mul true, l1, add r1 r2) <| v1.node.ty
        else if equal l1 r2 then Binop (Mul true, l1, add r1 l2) <| v1.node.ty
        else if equal r1 l2 then Binop (Mul true, r1, add l1 r2) <| v1.node.ty
        else Binop (Mul true, r1, add l1 l2) <| v1.node.ty
    (* only propagate down ites if we know it's concrete *)
    | Ite (b, l, r), BitVec x | BitVec x, Ite (b, l, r) ->
        let n = size_of v1.node.ty in
        let x = mk n x in
        Bool.ite b (add l x) (add r x)
    | _ -> mk_commut_binop (Add checked) v1 v2 <| v1.node.ty

  and sub ?(checked = false) v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | BitVec l, BitVec r -> mk_masked (size_of v1.node.ty) Z.(l - r)
    | _, BitVec z when Z.equal z Z.zero -> v1
    | BitVec z, _ when Z.equal z Z.zero -> neg v2
    (* BAD PERF:!!!! *)
    | _, Unop (Neg, v2) -> add v1 v2
    | Binop (Sub _, ({ node = { kind = BitVec _; _ }; _ } as c1), s), BitVec _
      ->
        sub (sub c1 v2) s
    | Binop (Sub _, s, ({ node = { kind = BitVec _; _ }; _ } as c1)), BitVec _
      ->
        sub s (add c1 v2)
    | BitVec _, Binop (Add _, ({ node = { kind = BitVec _; _ }; _ } as r), c)
    | BitVec _, Binop (Add _, c, ({ node = { kind = BitVec _; _ }; _ } as r)) ->
        sub (sub v1 r) c
    | Binop (Add _, ({ node = { kind = BitVec _; _ }; _ } as r), c), BitVec _
    | Binop (Add _, c, ({ node = { kind = BitVec _; _ }; _ } as r)), BitVec _ ->
        add c (sub r v2)
    (* only propagate down ites if we know it's concrete *)
    | Ite (b, l, r), BitVec _ -> Bool.ite b (sub l v2) (sub r v2)
    | BitVec _, Ite (b, l, r) -> Bool.ite b (sub v1 l) (sub v1 r)
    | Unop (BvOfBool n, b), BitVec _ -> Bool.ite b (sub (one n) v2) (neg v2)
    | BitVec _, Unop (BvOfBool n, b) -> Bool.ite b (sub v1 (one n)) v1
    | _ -> Binop (Sub checked, v1, v2) <| v1.node.ty

  and neg v =
    (* let n = size_of v.node.ty in
    match v.node.kind with
    | BitVec bv -> mk_masked n Z.(neg bv)
    | _ -> sub (zero n) v *)
    let n = size_of v.node.ty in
    match v.node.kind with
    | BitVec bv -> mk_masked n Z.(neg bv)
    | Unop (Neg, v) -> v
    | Ite (b, l, r) -> Bool.ite b (neg l) (neg r)
    | Unop (BvOfBool n, b) -> Bool.ite b (neg (one n)) (zero n)
    | _ -> Unop (Neg, v) <| v.node.ty

  (** [mod_ v1 v2] is the signed remainder of [v1 / v2], which takes the sign of
      the divisor [v2] if [signed]. For an unsigned version, use
      [rem ~signed:false]. *)
  let mod_ v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | BitVec l, BitVec r ->
        let size = size_of v1.node.ty in
        let l = bv_to_z true size l in
        let r = bv_to_z true size r in
        let res = Z.(l mod r) in
        let res =
          if Z.(res < zero) && not Z.(r < zero) then Z.(res + r)
          else if Z.(res >= zero) && Z.(r < zero) then Z.(res + r)
          else res
        in
        mk_masked size res
    | _ -> Binop (Mod, v1, v2) <| v1.node.ty

  (** [rem ~signed v1 v2] is the remainder of [v1 / v2], which takes the sign of
      the dividend [v1] if [signed]. *)
  let rec rem ~signed v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | BitVec l, BitVec r ->
        let size = size_of v1.node.ty in
        let l = bv_to_z signed size l in
        let r = bv_to_z signed size r in
        mk_masked size Z.(l mod r)
    | _, BitVec r when Stdlib.not signed && Z.(equal r one) ->
        zero (size_of v1.node.ty)
    | _, BitVec r when Stdlib.not signed && is_pow2 r ->
        let size = size_of v1.node.ty in
        let bitwidth = Z.log2 r in
        let lower = extract 0 (bitwidth - 1) v1 in
        extend ~signed:false (size - bitwidth) lower
    | Binop (Add _, { node = { kind = BitVec l; _ }; _ }, r), BitVec d
      when Stdlib.not signed && Z.(equal l d) ->
        rem ~signed r v2
    | Binop (Add _, r, { node = { kind = BitVec l; _ }; _ }), BitVec d
      when Stdlib.not signed && Z.(equal l d) ->
        rem ~signed r v2
    | _ -> Binop (Rem signed, v1, v2) <| v1.node.ty

  and not v =
    match v.node.kind with
    | BitVec bv ->
        let n = size_of v.node.ty in
        mk_masked n Z.(lognot bv)
    | Ite (b, l, r) -> Bool.ite b (not l) (not r)
    | _ -> Unop (BvNot, v) <| v.node.ty

  and and_ v1 v2 =
    let n = size_of v1.node.ty in
    assert (n == size_of v2.node.ty);
    match (v1.node.kind, v2.node.kind) with
    | BitVec l, BitVec r -> mk n Z.(logand l r)
    | BitVec mask, _ when Z.(equal mask zero) -> v1
    | _, BitVec mask when Z.(equal mask zero) -> v2
    | BitVec mask, _ when covers_bitwidth n mask -> v2
    | _, BitVec mask when covers_bitwidth n mask -> v1
    (* For (x >> s) & m, the mask is irrelevant if it entirely covers [bitsize - s] *)
    | ( (Binop ((LShr | AShr), _, { node = { kind = BitVec shift; _ }; _ }) as
         base),
        BitVec mask )
    | ( BitVec mask,
        (Binop ((LShr | AShr), _, { node = { kind = BitVec shift; _ }; _ }) as
         base) )
      when let bitwidth = n - Z.to_int shift in
           let low_mask = Z.(pred (one lsl bitwidth)) in
           Z.(equal (mask land low_mask) low_mask) ->
        base <| t_bv n
    | BitVec _, Ite (b, l, r) -> Bool.ite b (and_ v1 l) (and_ v1 r)
    | Ite (b, l, r), BitVec _ -> Bool.ite b (and_ l v2) (and_ r v2)
    (* if it's a right mask, it's usually beneficial to propagate it *)
    | (BitVec mask, Binop (BitAnd, l, r) | Binop (BitAnd, l, r), BitVec mask)
      when is_right_mask mask ->
        let n = size_of v1.node.ty in
        let mask = mk n mask in
        and_ (and_ mask l) (and_ mask r)
    | BitVec o, Unop (BvOfBool _, _) when Z.equal o Z.one -> v2
    | Unop (BvOfBool _, _), BitVec o when Z.equal o Z.one -> v1
    | Unop (BvOfBool _, b1), Unop (BvOfBool _, b2) ->
        of_bool n (Bool.and_ b1 b2)
    | ( Ite (b1, l1, { node = { kind = BitVec r1; _ }; _ }),
        Ite (b2, l2, { node = { kind = BitVec r2; _ }; _ }) )
      when Z.(equal r1 zero) && Z.(equal r2 zero) ->
        let n = size_of v1.node.ty in
        Bool.ite (Bool.and_ b1 b2) (and_ l1 l2) (zero n)
    | _, _ -> mk_commut_binop BitAnd v1 v2 <| t_bv n

  and or_ v1 v2 =
    assert (match v1.node.ty with TBitVector _ -> true | _ -> false);
    assert (match v2.node.ty with TBitVector _ -> true | _ -> false);
    match (v1.node.kind, v2.node.kind) with
    | BitVec l, BitVec r ->
        let n = size_of v1.node.ty in
        mk n Z.(l lor r)
    | BitVec z, _ when Z.equal z Z.zero -> v2
    | _, BitVec z when Z.equal z Z.zero -> v1
    (* 0x0..0X..X | (0x0..0Y..Y << N) when N = |X..X| ==> 0x0..0Y..YX..X  *)
    | ( Unop (BvExtend (false, nx), base),
        Binop
          ( Shl,
            { node = { kind = Unop (BvExtend (false, _), tail); _ }; _ },
            { node = { kind = BitVec shift; _ }; _ } ) )
      when Z.to_int shift = size_of base.node.ty ->
        let tail_size = size_of tail.node.ty in
        if nx = tail_size then concat tail base
        else if nx > tail_size then
          let new_base = concat tail base in
          extend ~signed:false (nx - tail_size) new_base
        else
          let new_tail = extract 0 (nx - 1) tail in
          concat new_tail base
    | Unop (BvOfBool n, b1), Unop (BvOfBool _, b2) -> of_bool n (Bool.or_ b1 b2)
    | _ -> mk_commut_binop BitOr v1 v2 <| v1.node.ty

  and xor v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | BitVec l, BitVec r ->
        let n = size_of v1.node.ty in
        mk n Z.(l lxor r)
    | BitVec z, _ when Z.equal z Z.zero -> v2
    | _, BitVec z when Z.equal z Z.zero -> v1
    | Unop (BvOfBool n, b1), Unop (BvOfBool _, b2) ->
        of_bool n (Bool.not (Bool.sem_eq b1 b2))
    | _ -> mk_commut_binop BitXor v1 v2 <| v1.node.ty

  (** [extract from_ to_ v] returns a bitvector covering bits from index
      [from_], to index [to_], inclusive. [0 <= from_ <= to_ < size_of v] must
      hold. *)
  and extract from_ to_ v =
    let prev_size = size_of v.node.ty in
    assert (0 <= from_ && from_ <= to_ && to_ < prev_size);
    let size = to_ - from_ + 1 in
    match v.node.kind with
    | BitVec bv -> mk_masked size Z.(bv asr from_)
    | _ when from_ = 0 && to_ = prev_size - 1 -> v
    | Binop (((BitAnd | BitOr | BitXor) as bop), v1, v2) -> (
        let v1 = extract from_ to_ v1 in
        let v2 = extract from_ to_ v2 in
        match bop with
        | BitAnd -> and_ v1 v2
        | BitOr -> or_ v1 v2
        | BitXor -> xor v1 v2
        | _ -> failwith "unreachable binop")
    | Binop (Shl, v1, v2) ->
        (* this only holds under the assumption v2 < BITS(v1), which should be true, since
         it usually is UB otherwise *)
        let v1 = extract from_ to_ v1 in
        let v2 = extract from_ to_ v2 in
        shl v1 v2
    | Binop (LShr, v1, { node = { kind = BitVec x; _ }; _ }) ->
        (* we have to be careful to not extract bits that are out of bounds *)
        let shift = Z.to_int x in
        if to_ + shift < prev_size then
          (* 1. we can just shift the extraction *)
          extract (from_ + shift) (to_ + shift) v1
        else if from_ + shift >= prev_size then
          (* 2. the full shift is out of bounds! so 0 *)
          zero size
        else
          (* 3. it's an in between - for now, we don't do anything *)
          Unop (BvExtract (from_, to_), v) <| t_bv size
    | Ite (b, l, r) ->
        let l = extract from_ to_ l in
        let r = extract from_ to_ r in
        Bool.ite b l r
    | Unop (BvExtend (false, by), _) when from_ >= prev_size - by ->
        (* zero extension, and we're extracting only the extended bits *)
        zero size
    | Unop (BvExtend (true, by), v) when from_ >= prev_size - by && from_ = to_
      ->
        (* sign extension, and we're extracting an extended bit: msb of the prev value *)
        let prev_size = size_of v.node.ty in
        extract (prev_size - 1) (prev_size - 1) v
    | Unop (BvExtend (signed, _), v) when from_ = 0 ->
        (* extracting from the beginning of an extended value *)
        let orig_size = size_of v.node.ty in
        if to_ = orig_size - 1 then
          (* extracting exactly the original bits *)
          v
        else if to_ < orig_size then
          (* extracting subset of original bits *)
          extract from_ to_ v
        else
          (* we can reduce the extend *)
          extend ~signed (to_ - orig_size + 1) v
    | Unop (BvExtend (_, by), v) when to_ <= prev_size - by - 1 ->
        (* extracting from original bits *)
        extract from_ to_ v
    | Unop (BvExtract (prev_from_, _), v) ->
        extract (prev_from_ + from_) (prev_from_ + to_) v
    | Binop (BvConcat, l, r) ->
        let size_r = size_of r.node.ty in
        if from_ >= size_r then extract (from_ - size_r) (to_ - size_r) l
        else if to_ < size_r then extract from_ to_ r
        else
          let r' = extract from_ (size_r - 1) r in
          let l' = extract 0 (to_ - size_r) l in
          concat l' r'
    | Binop (Add _, l, r) when from_ = 0 ->
        let l_low = extract from_ to_ l in
        let r_low = extract from_ to_ r in
        add l_low r_low
    | Binop (Add _, { node = { kind = BitVec n; _ }; _ }, x) when to_ < lsb n ->
        extract from_ to_ x
    | Binop (Add _, x, { node = { kind = BitVec n; _ }; _ }) when to_ < lsb n ->
        extract from_ to_ x
    | Binop (Mul _, { node = { kind = BitVec n; _ }; _ }, _)
      when is_pow2 n && to_ < Z.log2 n ->
        zero size
    | Binop (Mul _, l, r) when from_ = 0 ->
        (* i think the extraction is necessarily unchecked? *)
        let l_low = extract from_ to_ l in
        let r_low = extract from_ to_ r in
        mul ~checked:false l_low r_low
    | Binop (Rem false, l, { node = { kind = BitVec n; _ }; _ })
      when from_ = 0 && Z.log2 n < to_ ->
        (* extract[0,N](X % M) when M < 2^M can be pushed down *)
        let l = extract from_ to_ l in
        rem ~signed:false l (mk (to_ + 1) n)
    | _ -> Unop (BvExtract (from_, to_), v) <| t_bv size

  and extend ~signed extend_by v =
    let n = size_of v.node.ty in
    let to_ = n + extend_by in
    assert (extend_by > 0);
    match v.node.kind with
    | BitVec bv ->
        if signed && Z.testbit bv (n - 1) then
          (* Sign extend: replicate the MSB *)
          let mask = Z.(pred (one lsl extend_by) lsl n) in
          mk to_ Z.(bv lor mask)
        else
          (* Zero extend: just mask to ensure no extra bits *)
          mk to_ bv
    | Ite (b, l, r) ->
        let l = extend ~signed extend_by l in
        let r = extend ~signed extend_by r in
        Bool.ite b l r
    (* can't extend if signed && n == 1, as it should be all 1s *)
    | Unop (BvOfBool n, b) when Stdlib.not signed || n > 1 -> of_bool to_ b
    (* | Unop (BvExtract (_, t), v)
      when Stdlib.not signed && to_ = size_of v.node.ty && t = to_ - 1 ->
        (* extend[N](extract[L-N, L-1] X) == X >> N, where L = size_of X *)
        lshr v (mki to_ extend_by) *)
    (* unlike with extract, we don't want to propagate extend within the expression for &, |, ^,
         as that will require a more expensive bit-blasting. *)
    (* We also note the following reduction is *not valid*, as some upper bits may be set;
         e.g. given i2bv[3](8) = 0b000, extend[1](i2bv[3](8)) = 0b0000, whereas
              i2bv[4](8) = 0b1000
      | Unop (BvOfInt, v) -> Unop (BvOfInt, v) <| t_bv signed to_ *)
    | _ -> Unop (BvExtend (signed, extend_by), v) <| t_bv to_

  (** [concat v1 v2] for [v1] of size [n] and [v2] of size [m] is a bitvector of
      size [n + m] where the first [m] bits are [v2] and the following [n] are
      [v1] *)
  and concat v1 v2 =
    let n1 = size_of v1.node.ty in
    let n2 = size_of v2.node.ty in
    match (v1.node.kind, v2.node.kind) with
    | BitVec l, BitVec r -> mk_masked (n1 + n2) Z.(r + shift_left l n2)
    | Unop (BvExtract (from1, to1), v1), Unop (BvExtract (from2, to2), v2)
      when to2 + 1 = from1 && equal v1 v2 ->
        extract from2 to1 v1
    (* We re-order (extract A ++ (extract B ++ X)) to ((extract A ++ extract B) ++ X) *)
    | ( Unop (BvExtract _, x),
        Binop
          ( BvConcat,
            ({ node = { kind = Unop (BvExtract _, y); _ }; _ } as left),
            right ) )
      when equal x y ->
        concat (concat v1 left) right
    (* We re-order ((X ++ extract A) ++ extract B) to (X ++ (extract A ++ extract B)) *)
    | ( Binop
          ( BvConcat,
            left,
            ({ node = { kind = Unop (BvExtract _, x); _ }; _ } as right) ),
        Unop (BvExtract _, y) )
      when equal x y ->
        concat left (concat right v2)
    | Ite (b1, l1, r1), Ite (b2, l2, r2) when equal b1 b2 ->
        Bool.ite b1 (concat l1 l2) (concat r1 r2)
    | _, _ -> Binop (BvConcat, v1, v2) <| t_bv (n1 + n2)

  and shl v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | BitVec l, BitVec r -> mk_masked (size_of v1.node.ty) Z.(l lsl to_int r)
    | _, BitVec s when Z.equal s Z.zero -> v1
    | _, BitVec s when Z.geq s (Z.of_int (size_of v1.node.ty)) ->
        zero (size_of v1.node.ty)
    | Binop (Shl, v, { node = { kind = BitVec s1; _ }; _ }), BitVec s2 ->
        let n = size_of v1.node.ty in
        shl v (mk n Z.(s1 + s2))
    | _ -> Binop (Shl, v1, v2) <| v1.node.ty

  and lshr v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | BitVec l, BitVec r -> mk_masked (size_of v1.node.ty) Z.(l asr to_int r)
    | _, BitVec s when Z.equal s Z.zero -> v1
    | _, BitVec s when Z.geq s (Z.of_int (size_of v1.node.ty)) ->
        zero (size_of v1.node.ty)
    | Binop (LShr, v, { node = { kind = BitVec s1; _ }; _ }), BitVec s2 ->
        let n = size_of v1.node.ty in
        lshr v (mk n Z.(s1 + s2))
    | _ -> Binop (LShr, v1, v2) <| v1.node.ty

  and ashr v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | BitVec l, BitVec r -> mk_masked (size_of v1.node.ty) Z.(l asr to_int r)
    | _, BitVec s when Z.equal s Z.zero -> v1
    | Binop (AShr, v, { node = { kind = BitVec s1; _ }; _ }), BitVec s2 ->
        let n = size_of v1.node.ty in
        lshr v (mk n Z.(s1 + s2))
    | _ -> Binop (AShr, v1, v2) <| v1.node.ty

  and mul ?(checked = false) v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | BitVec l, BitVec r -> mk_masked (size_of v1.node.ty) Z.(l * r)
    | _, BitVec z when Z.equal z Z.one -> v1
    | BitVec z, _ when Z.equal z Z.one -> v2
    | _, BitVec z when Z.equal z Z.zero -> zero (size_of v1.node.ty)
    | BitVec z, _ when Z.equal z Z.zero -> zero (size_of v1.node.ty)
    | Binop (Mul true, { node = { kind = BitVec n; _ }; _ }, x), BitVec m
    | Binop (Mul true, x, { node = { kind = BitVec n; _ }; _ }), BitVec m
    | BitVec m, Binop (Mul true, { node = { kind = BitVec n; _ }; _ }, x)
    | BitVec m, Binop (Mul true, x, { node = { kind = BitVec n; _ }; _ })
      when checked ->
        mul ~checked:true x (mk (size_of v1.node.ty) Z.(n * m))
    (* only propagate down ites if we know it's concrete *)
    | Ite (b, l, r), BitVec x | BitVec x, Ite (b, l, r) ->
        let n = size_of v1.node.ty in
        let x = mk n x in
        Bool.ite b (mul l x) (mul r x)
    | _ -> mk_commut_binop (Mul checked) v1 v2 <| v1.node.ty

  let rec div ~signed v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | BitVec l, BitVec r ->
        let size = size_of v1.node.ty in
        let l = bv_to_z signed size l in
        let r = bv_to_z signed size r in
        let res = Z.(l / r) in
        mk_masked size res
    | _, BitVec r when Z.equal r Z.one -> v1
    (* this case shouldn't happen but it avoids conflicts for the next two patterns *)
    | ( Binop
          ( Mul checked,
            ({ node = { kind = BitVec _; _ }; _ } as l),
            ({ node = { kind = BitVec _; _ }; _ } as r) ),
        BitVec _ ) ->
        div ~signed (mul ~checked l r) v2
    | Binop (Mul true, { node = { kind = BitVec n; _ }; _ }, x), BitVec d
    | Binop (Mul true, x, { node = { kind = BitVec n; _ }; _ }), BitVec d
      when Stdlib.not signed && Z.(divisible n d) ->
        (* (x * n) / d = x * (n / d) when n % d == 0 *)
        mul ~checked:true x (mk (size_of v1.node.ty) Z.(n / d))
    | Binop (Mul true, { node = { kind = BitVec n; _ }; _ }, x), BitVec d
    | Binop (Mul true, x, { node = { kind = BitVec n; _ }; _ }), BitVec d
      when Stdlib.not signed && Z.(divisible d n) ->
        (* (x * n) / d = x / (d / n) when d % n == 0 *)
        let divisor = Z.(d / n) in
        div ~signed x (mk (size_of v1.node.ty) divisor)
    | Binop (Div s, x, { node = { kind = BitVec n; _ }; _ }), BitVec d
      when s = signed
           && (Stdlib.not @@ overflows ~signed (size_of v1.node.ty) n d Z.mul)
      ->
        (* (x / n) / d = x / (n * d) (if n * d doesn't overflow) *)
        div ~signed x (mk (size_of v1.node.ty) Z.(n * d))
    | _ -> Binop (Div signed, v1, v2) <| v1.node.ty

  let rec lt ~signed v1 v2 =
    let bits = size_of v1.node.ty in
    match (v1.node.kind, v2.node.kind) with
    | BitVec l, BitVec r ->
        Bool.bool @@ Z.lt (bv_to_z signed bits l) (bv_to_z signed bits r)
    | BitVec _, Binop (Add true, ({ node = { kind = BitVec _; _ }; _ } as r), x)
    | BitVec _, Binop (Add true, x, ({ node = { kind = BitVec _; _ }; _ } as r))
      ->
        lt ~signed (sub ~checked:true v1 r) x
    | Binop (Add true, ({ node = { kind = BitVec _; _ }; _ } as l), x), BitVec _
    | Binop (Add true, x, ({ node = { kind = BitVec _; _ }; _ } as l)), BitVec _
      ->
        lt ~signed x (sub ~checked:true v2 l)
    | _, BitVec x when Stdlib.not signed && Z.(equal x one) ->
        (* unsigned x < 1 is x == 0 *)
        Bool.sem_eq v1 (zero bits)
    | _, BitVec x when signed && Z.(equal x zero) ->
        let lt_zero v =
          Binop (Lt signed, v, zero (size_of v.node.ty)) <| TBool
        in
        let not_eq_0 v = Bool.not (Bool.sem_eq v1 (zero (size_of v.node.ty))) in
        (* this function returns if this node is negative if we can tell,
           and otherwise the node that represents the sign bit *)
        let rec aux_lt_zero v =
          match v.node.kind with
          | Unop (BvExtend (true, _), v) -> aux_lt_zero v
          | Unop (BvExtend (false, _), _) -> Bool.v_false
          | Binop (Mod, _, r) -> Bool.and_ (aux_lt_zero r) (not_eq_0 v)
          | Binop (Rem true, l, _) -> Bool.and_ (aux_lt_zero l) (not_eq_0 v)
          | Binop (Div true, l, r) ->
              Bool.and_ (not_eq_0 v)
                (Bool.not (Bool.sem_eq (aux_lt_zero l) (aux_lt_zero r)))
          | Binop (BvConcat, l, _) -> aux_lt_zero l
          | Unop (BvNot, v) -> not (aux_lt_zero v)
          | Unop (BvOfBool n, _) when n > 1 -> Bool.v_false
          | Ite (_, l, r) ->
              let pos_l = aux_lt_zero l in
              let pos_r = aux_lt_zero r in
              if pos_l = pos_r then pos_l else lt_zero v
          | _ -> lt_zero v
        in
        aux_lt_zero v1
    | BitVec x, _ when Z.equal (bv_to_z signed bits x) (max_for signed bits) ->
        Bool.v_false
    | _, BitVec x when Z.equal (bv_to_z signed bits x) (min_for signed bits) ->
        Bool.v_false
    | BitVec x, _ when Z.equal (bv_to_z signed bits x) (min_for signed bits) ->
        Bool.not (Bool.sem_eq v1 v2)
    | _, BitVec x when Z.equal (bv_to_z signed bits x) (max_for signed bits) ->
        Bool.not (Bool.sem_eq v1 v2)
    | ( BitVec c2,
        Binop (Mul true, x, ({ node = { kind = BitVec c1; _ }; _ } as v2)) )
    | ( BitVec c2,
        Binop (Mul true, ({ node = { kind = BitVec c1; _ }; _ } as v2), x) ) ->
        (* PROOF FOR c2 < x * c1
        (assert (and
          (not (bvsmulo x c1))
          (not (= c1 #x00))
          (let
            (
              (c1neg (bvslt c1 #x00))
              (c2neg (bvslt c2 #x00))
              (divs (= (bvsrem c2 c1) #x00)))
            (not
              (= (bvslt c2 (bvmul x c1))
                (ite (or divs (not c2neg))
                  (ite c1neg
                    (or (and (= c1 #xff) (= c2 #x80))
                      (bvslt x (bvsdiv c2 c1)))
                    (bvslt (bvsdiv c2 c1) x))
                  (ite c1neg
                    (bvsle x (bvsdiv c2 c1))
                    (bvsle (bvsdiv c2 c1) x)))))))) *)
        let c1 = bv_to_z signed bits c1 in
        let c2 = bv_to_z signed bits c2 in
        (* be careful bc c1 = v2 and c2 = v1 in this case *)
        if Z.divisible c2 c1 || Z.geq c2 Z.zero then
          if Z.lt c1 Z.zero then
            if
              signed
              && Z.equal c1 (Z.of_int (-1))
              && Z.equal c2 (min_for signed bits)
            then Bool.v_true
            else lt ~signed x (div ~signed v1 v2)
          else lt ~signed (div ~signed v1 v2) x
        else if Z.lt c1 Z.zero then leq ~signed x (div ~signed v1 v2)
        else leq ~signed (div ~signed v1 v2) x
    | ( Binop (Mul true, x, ({ node = { kind = BitVec c1; _ }; _ } as v1)),
        BitVec c2 )
    | ( Binop (Mul true, ({ node = { kind = BitVec c1; _ }; _ } as v1), x),
        BitVec c2 ) ->
        (* PROOF FOR : x * c1 < c2
          (assert (and
            (not (bvsmulo x c1))
            (not (= c1 #x00))
            (let
              (
                (c1neg (bvslt c1 #x00))
                (c2neg (bvslt c2 #x00))
                (divs (= (bvsrem c2 c1) #x00)))
              (not
                (= (bvslt (bvmul x c1) c2)
                  (ite (or divs c2neg)
                    (ite c1neg
                      (and (not (and (= c1 #xff) (= c2 #x80)))
                        (bvslt (bvsdiv c2 c1) x))
                      (bvslt x (bvsdiv c2 c1)))
                    (ite c1neg
                      (bvsle (bvsdiv c2 c1) x)
                      (bvsle x (bvsdiv c2 c1))))))))) *)
        let c1 = bv_to_z signed bits c1 in
        let c2 = bv_to_z signed bits c2 in
        if Z.divisible c2 c1 || Z.lt c2 Z.zero then
          if Z.lt c1 Z.zero then
            if
              signed
              && Z.equal c1 (Z.of_int (-1))
              && Z.equal c2 (min_for signed bits)
            then Bool.v_false
            else lt ~signed (div ~signed v2 v1) x
          else lt ~signed x (div ~signed v2 v1)
        else if Z.lt c1 Z.zero then leq ~signed (div ~signed v2 v1) x
        else leq ~signed x (div ~signed v2 v1)
    | Binop (Mul true, l1, r1), Binop (Mul true, l2, r2)
      when equal l1 l2 || equal l1 r2 || equal r1 l2 || equal r1 r2 ->
        if equal l1 l2 then lt ~signed r1 r2
        else if equal l1 r2 then lt ~signed r1 l2
        else if equal r1 l2 then lt ~signed l1 r2
        else lt ~signed l1 l2
    (* x < ite(b, 1, 0) => ite(b, x < 1, x < 0) => ite(b, x = 0, false) => b && x = 0 *)
    | _, Unop (BvOfBool n, b) when Stdlib.not signed ->
        Bool.and_ b (Bool.sem_eq v1 (zero n))
    | Ite (b, l, r), _ -> Bool.ite b (lt ~signed l v2) (lt ~signed r v2)
    | _, Ite (b, l, r) -> Bool.ite b (lt ~signed v1 l) (lt ~signed v1 r)
    | _ -> Binop (Lt signed, v1, v2) <| TBool

  and leq ~signed v1 v2 =
    let bits = size_of v1.node.ty in
    match (v1.node.kind, v2.node.kind) with
    | _ when equal v1 v2 -> Bool.v_true
    | BitVec l, BitVec r ->
        Bool.bool @@ Z.leq (bv_to_z signed bits l) (bv_to_z signed bits r)
    | BitVec _, Binop (Add true, ({ node = { kind = BitVec _; _ }; _ } as r), x)
    | BitVec _, Binop (Add true, x, ({ node = { kind = BitVec _; _ }; _ } as r))
      ->
        leq ~signed (sub ~checked:true v1 r) x
    | Binop (Add true, ({ node = { kind = BitVec _; _ }; _ } as l), x), BitVec _
    | Binop (Add true, x, ({ node = { kind = BitVec _; _ }; _ } as l)), BitVec _
      ->
        leq ~signed x (sub ~checked:true v2 l)
    | BitVec x, _ when Z.equal (bv_to_z signed bits x) (min_for signed bits) ->
        Bool.v_true
    | _, BitVec x when Z.equal (bv_to_z signed bits x) (max_for signed bits) ->
        Bool.v_true
    | ( BitVec c2,
        Binop (Mul true, x, ({ node = { kind = BitVec c1; _ }; _ } as v2)) )
    | ( BitVec c2,
        Binop (Mul true, ({ node = { kind = BitVec c1; _ }; _ } as v2), x) ) ->
        (* PROOF FOR : c2 <= x * c1
        (assert (and
          (not (bvsmulo x c1))
          (not (= c1 #x00))
          (let
            (
              (c1neg (bvslt c1 #x00))
              (c2neg (bvslt c2 #x00))
              (divs (= (bvsrem c2 c1) #x00)))
              (not
                (= (bvsle c2 (bvmul x c1) )
                  (ite divs
                    (ite c1neg
                      (or (and (= c1 #xff) (= c2 #x80))
                        (bvsle x (bvsdiv c2 c1)))
                      (bvsle (bvsdiv c2 c1) x))
                    (ite c1neg
                      (ite c2neg
                        (bvsle x (bvsdiv c2 c1))
                        (bvslt x (bvsdiv c2 c1)))
                      (ite c2neg
                        (bvsle (bvsdiv c2 c1) x)
                        (bvslt (bvsdiv c2 c1) x))))))))) *)
        let c1 = bv_to_z signed bits c1 in
        let c2 = bv_to_z signed bits c2 in
        (* be careful bc c1 = v2 and c2 = v1 in this case *)
        if Z.divisible c2 c1 then
          if Z.lt c1 Z.zero then
            if
              signed
              && Z.equal c1 (Z.of_int (-1))
              && Z.equal c2 (min_for signed bits)
            then Bool.v_true
            else leq ~signed x (div ~signed v1 v2)
          else leq ~signed (div ~signed v1 v2) x
        else if Z.lt c1 Z.zero then
          if Z.lt c2 Z.zero then leq ~signed x (div ~signed v1 v2)
          else lt ~signed x (div ~signed v1 v2)
        else if Z.lt c2 Z.zero then leq ~signed (div ~signed v1 v2) x
        else lt ~signed (div ~signed v1 v2) x
    | ( Binop (Mul true, x, ({ node = { kind = BitVec c1; _ }; _ } as v1)),
        BitVec c2 )
    | ( Binop (Mul true, ({ node = { kind = BitVec c1; _ }; _ } as v1), x),
        BitVec c2 ) ->
        (* PROOF FOR : x * c1 <= c2
          (assert (and
            (not (bvsmulo x c1))
            (not (= c1 #x00))
            (let
              (
                (c1neg (bvslt c1 #x00))
                (c2neg (bvslt c2 #x00))
                (divs (= (bvsrem c2 c1) #x00)))
              (not
                (= (bvsle (bvmul x c1) c2)
                  (ite divs
                    (ite c1neg
                      (and (not (and (= c1 #xff) (= c2 #x80)))
                        (bvsle (bvsdiv c2 c1) x))
                      (bvsle x (bvsdiv c2 c1)))
                    (ite c1neg
                      (ite c2neg
                        (bvslt (bvsdiv c2 c1) x)
                        (bvsle (bvsdiv c2 c1) x))
                      (ite c2neg
                        (bvslt x (bvsdiv c2 c1))
                        (bvsle x (bvsdiv c2 c1)))))))))) *)
        let c1 = bv_to_z signed bits c1 in
        let c2 = bv_to_z signed bits c2 in
        if Z.divisible c2 c1 then
          if Z.lt c1 Z.zero then
            if
              signed
              && Z.equal c1 (Z.of_int (-1))
              && Z.equal c2 (min_for signed bits)
            then Bool.v_false
            else leq ~signed (div ~signed v2 v1) x
          else leq ~signed x (div ~signed v2 v1)
        else if Z.lt c1 Z.zero then
          if Z.lt c2 Z.zero then lt ~signed (div ~signed v2 v1) x
          else leq ~signed (div ~signed v2 v1) x
        else if Z.lt c2 Z.zero then lt ~signed x (div ~signed v2 v1)
        else leq ~signed x (div ~signed v2 v1)
    | Binop (Mul true, l1, r1), Binop (Mul true, l2, r2)
      when equal l1 l2 || equal l1 r2 || equal r1 l2 || equal r1 r2 ->
        if equal l1 l2 then leq ~signed r1 r2
        else if equal l1 r2 then leq ~signed r1 l2
        else if equal r1 l2 then leq ~signed l1 r2
        else leq ~signed l1 l2
    | Binop (Div false, _, { node = { kind = BitVec d; _ }; _ }), BitVec n
      when Stdlib.not signed && Z.(gt (mul n d) (max_for false bits)) ->
        Bool.v_true
    | _ -> Binop (Leq signed, v1, v2) <| TBool

  let gt ~signed v1 v2 = lt ~signed v2 v1
  let geq ~signed v1 v2 = leq ~signed v2 v1

  let add_overflows ~signed v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | BitVec l, BitVec r -> ovf_check ~signed (size_of v1.node.ty) l r Z.( + )
    | BitVec z, _ when Z.equal z Z.zero -> Bool.v_false
    | _, BitVec z when Z.equal z Z.zero -> Bool.v_false
    | BitVec z, _ when Stdlib.not signed ->
        let n = size_of v1.node.ty in
        let z = bv_to_z signed n z in
        let m = max_for signed n in
        gt ~signed v2 (mk_masked n Z.(m - z))
    | _, BitVec z when Stdlib.not signed ->
        let n = size_of v1.node.ty in
        let z = bv_to_z signed n z in
        let m = max_for signed n in
        gt ~signed v1 (mk_masked n Z.(m - z))
    | Unop (BvOfBool _, _), Unop (BvOfBool _, _) -> Bool.v_false
    | Unop (BvOfBool _, b), other | other, Unop (BvOfBool _, b) ->
        (* ite(b, 1, 0) + x only overflows if b && x == max *)
        let n = size_of v1.node.ty in
        let max = max_for signed n in
        let other = other <| t_bv n in
        Bool.and_ b (Bool.sem_eq other (mk n max))
    | _ ->
        if signed then
          let sign_of v = lt ~signed v (zero (size_of v.node.ty)) in
          let sign_l = sign_of v1 in
          let sign_r = sign_of v2 in
          let msb_res = sign_of (add v1 v2) in
          Bool.and_
            (Bool.sem_eq sign_l sign_r)
            (Bool.not (Bool.sem_eq sign_l msb_res))
        else
          let res = add v1 v2 in
          Bool.or_ (lt ~signed res v1) (lt ~signed res v2)

  let mul_overflows ~signed v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | BitVec l, BitVec r -> ovf_check ~signed (size_of v1.node.ty) l r Z.( * )
    | BitVec z, _ when Z.equal z Z.zero || Z.equal z Z.one -> Bool.v_false
    | _, BitVec z when Z.equal z Z.zero || Z.equal z Z.one -> Bool.v_false
    | _
      when if signed then msb_of v1 + msb_of v2 < size_of v1.node.ty - 1
           else msb_of v1 + msb_of v2 < size_of v1.node.ty ->
        Bool.v_false
    | _ -> Binop (MulOvf signed, v1, v2) <| TBool

  let neg_overflows v =
    let n = size_of v.node.ty in
    Bool.sem_eq (mk_masked n (min_for true n)) v

  let sub_overflows ~signed v1 v2 =
    if Stdlib.not signed then lt ~signed v1 v2
    else
      let neg_ovf = neg_overflows v2 in
      let neg_v2 = neg v2 in
      let add_ovf = add_overflows ~signed v1 neg_v2 in
      Bool.or_ neg_ovf add_ovf

  let of_float ~rounding ~signed ~size v =
    let p = precision_of_f v.node.ty in
    match (p, v.node.kind, size) with
    | F32, Float f, 32 ->
        mk 32 @@ Z.of_int32 (Int32.bits_of_float (Stdlib.Float.of_string f))
    | F64, Float f, 64 ->
        mk 64 @@ Z.of_int64 (Int64.bits_of_float (Stdlib.Float.of_string f))
    | _, _, _ -> Unop (BvOfFloat (rounding, signed, size), v) <| t_bv size

  let to_float ~rounding ~signed ~fp v =
    Unop (FloatOfBv (rounding, signed, fp), v) <| t_f fp
end

(** {2 Floating point} *)
and Float : Float = struct
  let f2str = Stdlib.Float.to_string
  let str2f = Stdlib.Float.of_string
  let mk fp f = Float f <| t_f fp
  let mk_f fp f = Float (f2str f) <| t_f fp
  let like v f = Float (f2str f) <| v.node.ty

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
    | Float f1, Float f2 -> Bool.bool (str2f f1 < str2f f2)
    | _ -> Binop (FLt, v1, v2) <| TBool

  let leq v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | Float f1, Float f2 -> Bool.bool (str2f f1 <= str2f f2)
    | _ -> Binop (FLeq, v1, v2) <| TBool

  let gt v1 v2 = lt v2 v1
  let geq v1 v2 = leq v2 v1
  let add v1 v2 = mk_commut_binop FAdd v1 v2 <| v1.node.ty
  let sub v1 v2 = Binop (FSub, v1, v2) <| v1.node.ty
  let div v1 v2 = Binop (FDiv, v1, v2) <| v1.node.ty
  let mul v1 v2 = mk_commut_binop FMul v1 v2 <| v1.node.ty
  let rem v1 v2 = Binop (FRem, v1, v2) <| v1.node.ty

  let abs v =
    match v.node.kind with
    | Unop (FAbs, _) -> v
    | _ -> Unop (FAbs, v) <| v.node.ty

  let neg v =
    let fp = fp_of v in
    Binop (FSub, mk fp "0.0", v) <| v.node.ty

  let[@inline] is_floatclass fc =
   fun sv ->
    match sv.node.kind with
    | Float f -> Bool.bool (FloatClass.as_fpclass fc = classify_float (str2f f))
    | _ -> Unop (FIs fc, sv) <| TBool

  let is_normal = is_floatclass Normal
  let is_subnormal = is_floatclass Subnormal
  let is_infinite = is_floatclass Infinite
  let is_nan = is_floatclass NaN
  let is_zero = is_floatclass Zero
  let round rm sv = Unop (FRound rm, sv) <| sv.node.ty
end

(** {2 Pointers} *)

module Ptr = struct
  let mk l o = Ptr (l, o) <| TPointer (size_of o.node.ty)

  let loc p =
    match p.node.kind with
    | Ptr (l, _) -> l
    | _ -> Unop (GetPtrLoc, p) <| TLoc (size_of p.node.ty)

  let null_loc n = BitVec Z.zero <| TLoc n
  let is_null_loc l = Bool.sem_eq l (null_loc (size_of l.node.ty))
  let loc_of_z n z = BitVec z <| TLoc n
  let loc_of_int n i = loc_of_z n (Z.of_int i)

  let ofs p =
    match p.node.kind with
    | Ptr (_, o) -> o
    | _ ->
        let n = size_of p.node.ty in
        Unop (GetPtrOfs, p) <| TBitVector n

  let decompose p =
    match p.node.kind with Ptr (l, o) -> (l, o) | _ -> (loc p, ofs p)

  let add_ofs p o =
    let loc, ofs = decompose p in
    mk loc (BitVec.add ofs o)

  let null n = mk (null_loc n) (BitVec.zero n)
  let is_null p = Bool.sem_eq p (null (size_of p.node.ty))
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
  let bv_z = BitVec.mk
  let ptr = Ptr.mk
  let seq = SSeq.mk
  let ( ==@ ) = Bool.sem_eq
  let ( ==?@ ) = Bool.sem_eq_untyped
  let ( &&@ ) = Bool.and_
  let ( ||@ ) = Bool.or_
  let ( >@ ) = BitVec.gt ~signed:false
  let ( >=@ ) = BitVec.geq ~signed:false
  let ( <@ ) = BitVec.lt ~signed:false
  let ( <=@ ) = BitVec.leq ~signed:false
  let ( >$@ ) = BitVec.gt ~signed:true
  let ( >=$@ ) = BitVec.geq ~signed:true
  let ( <$@ ) = BitVec.lt ~signed:true
  let ( <=$@ ) = BitVec.leq ~signed:true
  let ( +@ ) = BitVec.add ~checked:false
  let ( -@ ) = BitVec.sub ~checked:false
  let ( ~- ) = BitVec.neg
  let ( *@ ) = BitVec.mul ~checked:false
  let ( /@ ) = BitVec.div ~signed:false
  let ( /$@ ) = BitVec.div ~signed:true
  let ( %@ ) = BitVec.rem ~signed:false
  let ( %$@ ) = BitVec.rem ~signed:true
  let ( <<@ ) = BitVec.shl
  let ( >>@ ) = BitVec.lshr
  let ( >>>@ ) = BitVec.ashr
  let ( ^@ ) = BitVec.xor
  let ( &@ ) = BitVec.and_
  let ( |@ ) = BitVec.or_
  let ( ==.@ ) = Float.eq
  let ( >.@ ) = Float.gt
  let ( >=.@ ) = Float.geq
  let ( <.@ ) = Float.lt
  let ( <=.@ ) = Float.leq
  let ( +.@ ) = Float.add
  let ( -.@ ) = Float.sub
  let ( *.@ ) = Float.mul
  let ( /.@ ) = Float.div
end
