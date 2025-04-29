open Hashcons
module Var = Soteria_symex.Var

module FloatPrecision = struct
  type t = Charon.Values.float_type = F16 | F32 | F64 | F128
  [@@deriving eq, show { with_path = false }, ord]

  let size = function F16 -> 16 | F32 -> 32 | F64 -> 64 | F128 -> 128

  let of_size = function
    | 16 -> F16
    | 32 -> F32
    | 64 -> F64
    | 128 -> F128
    | _ -> failwith "Invalid float size"
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
let is_float ty = match ty with TFloat _ -> true | _ -> false

module Nop = struct
  type t = Distinct [@@deriving eq, show { with_path = false }, ord]
end

module Unop = struct
  type t =
    | Not
    | GetPtrLoc
    | GetPtrOfs
    | IntOfBool
    | BvOfFloat
    | BvOfInt
    | FloatOfBv
    | IntOfBv of bool (* signed *)
  [@@deriving eq, ord]

  let pp ft = function
    | Not -> Fmt.string ft "!"
    | GetPtrLoc -> Fmt.string ft "loc"
    | GetPtrOfs -> Fmt.string ft "ofs"
    | IntOfBool -> Fmt.string ft "b2i"
    | BvOfFloat -> Fmt.string ft "f2bv"
    | BvOfInt -> Fmt.string ft "i2bv"
    | FloatOfBv -> Fmt.string ft "bv2f"
    | IntOfBv _ -> Fmt.string ft "bv2i"
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
    | Mod
    (* Binary operators (size * signed) *)
    | BitAnd
    | BitOr
    | BitXor
    | BitShl
    | BitShr
  [@@deriving eq, ord]

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
    | BitAnd -> Fmt.string ft "&"
    | BitOr -> Fmt.string ft "|"
    | BitXor -> Fmt.string ft "^"
    | BitShl -> Fmt.string ft "<<"
    | BitShr -> Fmt.string ft ">>"
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

let hash t = t.hkey
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
  | Int z -> pf ft "%a" Z.pp_print z
  | Float f -> pf ft "%s" f
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

let rec sure_neq a b =
  (not (equal_ty a.node.ty b.node.ty))
  ||
  match (a.node.kind, b.node.kind) with
  | Int a, Int b -> not (Z.equal a b)
  | Bool a, Bool b -> a <> b
  | Ptr (la, oa), Ptr (lb, ob) -> sure_neq la lb || sure_neq oa ob
  | _ -> false

module Hcons = Hashcons.Make (struct
  type t = t_node

  let equal = equal_t_node

  (* We could do a lot more efficient in terms of hashing probably,
     if this ever becomes a bottleneck. *)
  let hash { kind; ty } =
    let hty = Hashtbl.hash ty in
    match kind with
    | Var _ | Bool _ | Int _ | Float _ | BitVec _ -> Hashtbl.hash (kind, hty)
    | Ptr (l, r) -> Hashtbl.hash (l.hkey, r.hkey, hty)
    | Seq l -> Hashtbl.hash (List.map (fun sv -> sv.hkey) l, hty)
    | Unop (op, v) -> Hashtbl.hash (op, v.hkey, hty)
    | Binop (op, l, r) -> Hashtbl.hash (op, l.hkey, r.hkey, hty)
    | Nop (op, l) -> Hashtbl.hash (op, List.map (fun sv -> sv.hkey) l, hty)
    | Ite (c, t, e) -> Hashtbl.hash (c.hkey, t.hkey, e.hkey, hty)
end)

let table = Hcons.create 1023
let hashcons = Hcons.hashcons table
let ( <| ) kind ty : t = hashcons { kind; ty }
let mk_var v ty = Var v <| ty

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
  | Bool b1, Bool b2 -> bool (b1 && b2)
  | Bool false, _ | _, Bool false -> v_false
  | Bool true, _ -> v2
  | _, Bool true -> v1
  | _ -> Binop (And, v1, v2) <| TBool

let conj l = List.fold_left and_ v_true l
let int_z z = Int z <| TInt
let int i = int_z (Z.of_int i)
let zero = int_z Z.zero
let one = int_z Z.one
let float fp f = Float f <| t_f fp
let float_f fp f = Float (Float.to_string f) <| t_f fp
let float_like v f = Float (Float.to_string f) <| v.node.ty

let fp_of v =
  match v.node.ty with
  | TFloat fp -> fp
  | _ -> Fmt.failwith "Unsupported float type"

let f16 f = float_f F16 f
let f32 f = float_f F32 f
let f64 f = float_f F64 f
let f128 f = float_f F128 f

let rec not sv =
  if equal sv v_true then v_false
  else if equal sv v_false then v_true
  else
    match sv.node.kind with
    | Unop (Not, sv) -> sv
    | Binop (Lt, v1, v2) -> Binop (Leq, v2, v1) <| TBool
    | Binop (Leq, v1, v2) -> Binop (Lt, v2, v1) <| TBool
    | Binop (Or, v1, v2) -> Binop (And, not v1, not v2) <| TBool
    | _ -> Unop (Not, sv) <| TBool

let rec sem_eq v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Int z1, Int z2 -> bool (Z.equal z1 z2)
  | Bool b1, Bool b2 -> bool (b1 = b2)
  | Ptr (l1, o1), Ptr (l2, o2) -> and_ (sem_eq l1 l2) (sem_eq o1 o2)
  | Binop (Plus, v1, { node = { kind = Int x; _ }; _ }), Int y
  | Binop (Plus, { node = { kind = Int x; _ }; _ }, v1), Int y
  | Int y, Binop (Plus, v1, { node = { kind = Int x; _ }; _ })
  | Int y, Binop (Plus, { node = { kind = Int x; _ }; _ }, v1) ->
      sem_eq v1 (int_z @@ Z.sub y x)
  | Binop (Minus, v1, { node = { kind = Int x; _ }; _ }), Int y
  | Binop (Minus, { node = { kind = Int x; _ }; _ }, v1), Int y
  | Int y, Binop (Minus, v1, { node = { kind = Int x; _ }; _ })
  | Int y, Binop (Minus, { node = { kind = Int x; _ }; _ }, v1) ->
      sem_eq v1 (int_z @@ Z.add y x)
  | Unop (IntOfBool, v1), Int z -> if Z.equal Z.zero z then not v1 else v1
  | _ when is_float v1.node.ty -> Binop (Eq, v1, v2) <| TBool
  | _ ->
      if equal v1 v2 then v_true (* Start with a syntactic check *)
      else Binop (Eq, v1, v2) <| TBool

let sem_eq_untyped v1 v2 =
  if equal_ty v1.node.ty v2.node.ty then sem_eq v1 v2 else v_false

let or_ v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Bool b1, Bool b2 -> bool (b1 || b2)
  | Bool true, _ | _, Bool true -> v_true
  | Bool false, _ -> v2
  | _, Bool false -> v1
  | _ -> Binop (Or, v1, v2) <| TBool

let rec split_ands (sv : t) (f : t -> unit) : unit =
  match sv.node.kind with
  | Binop (And, s1, s2) ->
      split_ands s1 f;
      split_ands s2 f
  | _ -> f sv

let distinct l =
  (* [Distinct l] when l is empty or of size 1 is always true *)
  match l with
  | [] | _ :: [] -> v_true
  | l -> Nop (Distinct, l) <| TBool

let ite guard if_ else_ =
  match guard.node.kind with
  | Bool true -> if_
  | Bool false -> else_
  | _ -> Ite (guard, if_, else_) <| if_.node.ty

(** {2 Integers and Floats} *)

let int_of_bool b =
  match b.node.kind with
  | Bool true -> one
  | Bool false -> zero
  | _ -> Unop (IntOfBool, b) <| TInt

let bool_of_int sv =
  match sv.node.kind with
  | Int z -> bool (Stdlib.not (Z.equal z Z.zero))
  | Unop (IntOfBool, sv') -> sv'
  | _ -> not (sem_eq sv zero)

(* FIXME: once we get BitVectors as concrete values, add them here *)
let bv_of_float v =
  match (v.node.ty, v.node.kind) with
  | TFloat _, Unop (FloatOfBv, v) -> v
  | TFloat F32, Float f ->
      let z = Z.of_int32 (Int32.bits_of_float (Float.of_string f)) in
      BitVec z <| t_bv 32
  | TFloat F64, Float f ->
      let z = Z.of_int64 (Int64.bits_of_float (Float.of_string f)) in
      BitVec z <| t_bv 64
  | TFloat fp, _ -> Unop (BvOfFloat, v) <| t_bv (FloatPrecision.size fp)
  | _ -> failwith "Expected a float value in bv_of_float"

let bv_of_int n v =
  match v.node.kind with
  | Unop (IntOfBv _, v) -> v
  | Int z ->
      let z = if Z.geq z Z.zero then z else Z.neg z in
      BitVec z <| t_bv n
  | _ -> Unop (BvOfInt, v) <| t_bv n

let int_of_bv signed v =
  match v.node.kind with
  | Unop (BvOfInt, v) -> v
  | _ -> Unop (IntOfBv signed, v) <| t_int

let float_of_bv v =
  match (v.node.ty, v.node.kind) with
  | _, Unop (BvOfFloat, v) -> v
  | TBitVector n, _ -> Unop (FloatOfBv, v) <| t_f (FloatPrecision.of_size n)
  | _ -> failwith "Expected a float value in float_of_bv"

let float_of_int fp v =
  match v.node.kind with
  | Int i -> float fp (Z.to_string i)
  | _ -> float_of_bv (bv_of_int (FloatPrecision.size fp) v)

let int_of_float v =
  match (v.node.ty, v.node.kind) with
  | TFloat F32, Float f ->
      int_z (Z.of_int32 (Int32.bits_of_float (Float.of_string f)))
  | TFloat F64, Float f ->
      int_z (Z.of_int64 (Int64.bits_of_float (Float.of_string f)))
  | _ -> int_of_bv true (bv_of_float v)

let rec lt v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Int i1, Int i2 -> bool (Z.lt i1 i2)
  | Float f1, Float f2 -> bool (f1 < f2)
  | _, _ when equal v1 v2 -> v_false
  | _, Binop (Plus, v2, v3) when equal v1 v2 -> lt zero v3
  | Binop (Plus, v1, v2), Binop (Plus, v3, v4) when equal v1 v3 -> lt v2 v4
  | Binop (Plus, v1, { node = { kind = Int x; _ }; _ }), Int y
  | Binop (Plus, { node = { kind = Int x; _ }; _ }, v1), Int y ->
      lt v1 (int_z @@ Z.sub y x)
  | Binop (Minus, v1, { node = { kind = Int x; _ }; _ }), Int y
  | Binop (Minus, { node = { kind = Int x; _ }; _ }, v1), Int y ->
      lt v1 (int_z @@ Z.add y x)
  | Int y, Binop (Plus, v1, { node = { kind = Int x; _ }; _ })
  | Int y, Binop (Plus, { node = { kind = Int x; _ }; _ }, v1) ->
      lt (int_z @@ Z.sub y x) v1
  | Int y, Binop (Minus, v1, { node = { kind = Int x; _ }; _ })
  | Int y, Binop (Minus, { node = { kind = Int x; _ }; _ }, v1) ->
      lt (int_z @@ Z.add y x) v1
  | _ -> Binop (Lt, v1, v2) <| TBool

let gt v1 v2 = lt v2 v1

let rec leq v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Int i1, Int i2 -> bool (Z.leq i1 i2)
  | Float f1, Float f2 -> bool (f1 <= f2)
  | _, _ when equal v1 v2 -> v_true
  | _, Binop (Plus, v2, v3) when equal v1 v2 -> leq zero v3
  | Binop (Plus, v1, v2), Binop (Plus, v3, v4) when equal v1 v3 -> leq v2 v4
  | Binop (Plus, v1, { node = { kind = Int x; _ }; _ }), Int y
  | Binop (Plus, { node = { kind = Int x; _ }; _ }, v1), Int y ->
      leq v1 (int_z @@ Z.sub y x)
  | Binop (Minus, v1, { node = { kind = Int x; _ }; _ }), Int y
  | Binop (Minus, { node = { kind = Int x; _ }; _ }, v1), Int y ->
      leq v1 (int_z @@ Z.add y x)
  | Int y, Binop (Plus, v1, { node = { kind = Int x; _ }; _ })
  | Int y, Binop (Plus, { node = { kind = Int x; _ }; _ }, v1) ->
      leq (int_z @@ Z.sub y x) v1
  | Int y, Binop (Minus, v1, { node = { kind = Int x; _ }; _ })
  | Int y, Binop (Minus, { node = { kind = Int x; _ }; _ }, v1) ->
      leq (int_z @@ Z.add y x) v1
  | _ -> Binop (Leq, v1, v2) <| TBool

let geq v1 v2 = leq v2 v1

let rec plus v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, _ when equal v1 zero -> v2
  | _, _ when equal v2 zero -> v1
  | Int i1, Int i2 -> int_z (Z.add i1 i2)
  | Binop (Plus, v1, { node = { kind = Int i2; _ }; _ }), Int i3 ->
      plus v1 (int_z (Z.add i2 i3))
  | _ -> Binop (Plus, v1, v2) <| v1.node.ty

let minus v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, _ when equal v2 zero -> v1
  | Int i1, Int i2 -> int_z (Z.sub i1 i2)
  | _ -> Binop (Minus, v1, v2) <| v1.node.ty

let times v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, _ when equal v1 zero || equal v2 zero -> zero
  | _, _ when equal v1 one -> v2
  | _, _ when equal v2 one -> v1
  | Int i1, Int i2 -> int_z (Z.mul i1 i2)
  | _ -> Binop (Times, v1, v2) <| v1.node.ty

let div v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, _ when equal v2 one -> v1
  | Int i1, Int i2 -> int_z (Z.div i1 i2)
  | _ -> Binop (Div, v1, v2) <| v1.node.ty

let rec is_mod v n =
  match v.node.kind with
  | Int i1 -> Z.equal (Z.( mod ) i1 n) Z.zero
  | Binop (Plus, v2, v3) -> is_mod v2 n && is_mod v3 n
  | Binop (Minus, v2, v3) -> is_mod v2 n && is_mod v3 n
  | Binop (Times, v2, v3) -> is_mod v2 n || is_mod v3 n
  | _ -> false

let rec rem v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, Int i2 when is_mod v1 i2 -> int_z Z.zero
  | Int i1, Int i2 -> int_z (Z.rem i1 i2)
  | Binop (Times, v1, n), Binop (Times, v2, m) when equal n m ->
      times n (rem v1 v2)
  | Binop (Times, n, v1), Binop (Times, m, v2) when equal n m ->
      times n (rem v1 v2)
  | _ -> Binop (Rem, v1, v2) <| v1.node.ty

let ( mod ) v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, Int i2 when is_mod v1 i2 -> int_z Z.zero
  | Int i1, Int i2 -> int_z (Z.( mod ) i1 i2)
  | _ -> Binop (Mod, v1, v2) <| v1.node.ty

let bit_and size signed v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Int i1, Int i2 -> int_z (Z.( land ) i1 i2)
  | Bool b1, Bool b2 -> bool (b1 && b2)
  | _ ->
      let v1_bv = bv_of_int size v1 in
      let v2_bv = bv_of_int size v2 in
      let v = Binop (BitAnd, v1_bv, v2_bv) <| t_bv size in
      Unop (IntOfBv signed, v) <| t_int

let bit_or size signed v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Int i1, Int i2 -> int_z (Z.( lor ) i1 i2)
  | Bool b1, Bool b2 -> bool (b1 || b2)
  | _ ->
      let v1_bv = bv_of_int size v1 in
      let v2_bv = bv_of_int size v2 in
      let v = Binop (BitOr, v1_bv, v2_bv) <| t_bv size in
      Unop (IntOfBv signed, v) <| t_int

let bit_xor size signed v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Int i1, Int i2 -> int_z (Z.( lxor ) i1 i2)
  | Bool b1, Bool b2 -> bool (b1 <> b2)
  | _ ->
      let v1_bv = bv_of_int size v1 in
      let v2_bv = bv_of_int size v2 in
      let v = Binop (BitXor, v1_bv, v2_bv) <| t_bv size in
      Unop (IntOfBv signed, v) <| t_int

let bit_shl size signed v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Int i1, Int i2 -> int_z (Z.( lsl ) i1 (Z.to_int i2))
  | _ ->
      let v1_bv = bv_of_int size v1 in
      let v2_bv = bv_of_int size v2 in
      let v = Binop (BitShl, v1_bv, v2_bv) <| t_bv size in
      Unop (IntOfBv signed, v) <| t_int

let bit_shr size signed v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Int i1, Int i2 -> int_z (Z.( asr ) i1 (Z.to_int i2))
  | _ ->
      let v1_bv = bv_of_int size v1 in
      let v2_bv = bv_of_int size v2 in
      let v = Binop (BitShr, v1_bv, v2_bv) <| t_bv size in
      Unop (IntOfBv signed, v) <| t_int

let abs v =
  match (v.node.ty, v.node.kind) with
  | TInt, Int i -> int_z (Z.abs i)
  | TFloat fp, _ ->
      (* we just set the bit of the sign to 0 *)
      let bits = FloatPrecision.size fp in
      let mask = int_z @@ Z.pred @@ Z.shift_left Z.one (bits - 1) in
      let mask_bv = bv_of_int bits mask in
      let float_bv = bv_of_float v in
      (* make the BitAnd directly since bit_and converts to an int *)
      let bit_and = Binop (BitAnd, mask_bv, float_bv) <| mask_bv.node.ty in
      float_of_bv bit_and
  | _ -> ite (lt v zero) (minus zero v) v

let neg v =
  match (v.node.ty, v.node.kind) with
  | TInt, Int i -> int_z (Z.neg i)
  | TFloat fp, _ ->
      (* we just flip the bit of the sign *)
      let bits = FloatPrecision.size fp in
      let mask = int_z @@ Z.shift_left Z.one (bits - 1) in
      let mask_bv = bv_of_int bits mask in
      let float_bv = bv_of_float v in
      (* make the BitOr directly since bit_or converts to an int *)
      let bit_or = Binop (BitXor, mask_bv, float_bv) <| mask_bv.node.ty in
      float_of_bv bit_or
  | _ -> minus zero v

(* Negates a boolean that is in integer form (i.e. 0 for false, anything else is true) *)
let not_int_bool sv =
  match sv.node.kind with
  | Int z -> if Z.equal z Z.zero then one else zero
  | Unop (IntOfBool, sv') -> int_of_bool (not sv')
  | _ -> int_of_bool (sem_eq sv zero)

(** {2 Pointers} *)

module Ptr = struct
  let mk l o = Ptr (l, o) <| TPointer

  let loc p =
    match p.node.kind with Ptr (l, _) -> l | _ -> Unop (GetPtrLoc, p) <| TLoc

  let loc_of_int i = Int (Z.of_int i) <| TLoc

  let ofs p =
    match p.node.kind with Ptr (_, o) -> o | _ -> Unop (GetPtrOfs, p) <| TInt

  let decompose p =
    match p.node.kind with
    | Ptr (l, o) -> (l, o)
    | _ -> (Unop (GetPtrOfs, p) <| TInt, Unop (GetPtrOfs, p) <| TInt)

  let add_ofs p o =
    match p.node.kind with
    | Ptr (l, o') -> mk l (plus o' o)
    | _ -> mk (loc p) (plus (ofs p) o)

  let null_loc = Int Z.zero <| TLoc
  let null = mk null_loc zero
  let is_null p = sem_eq p null
  let is_at_null_loc p = sem_eq (loc p) null_loc
end

(** {2 Sequences} *)

module SSeq = struct
  let mk ~inner_ty l = Seq l <| TSeq inner_ty
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
  let ( %@ ) = ( mod )
  let ( +.@ ) = plus
  let ( -.@ ) = minus
  let ( *.@ ) = times
  let ( /.@ ) = div
  let ( %.@ ) = ( mod )
end

module Syntax = struct
  module Sym_int_syntax = struct
    let mk_int = int
    let zero = zero
    let one = one
  end
end
