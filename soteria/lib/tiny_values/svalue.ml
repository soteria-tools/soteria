open Soteria_std
open Hc
module Var = Symex.Var

type ty = TBool | TInt [@@deriving eq, show { with_path = false }, ord]

let t_bool = TBool
let t_int = TInt

module Nop = struct
  type t = Distinct [@@deriving eq, show { with_path = false }, ord]
end

module Unop = struct
  let equal_fpclass = ( = )
  let compare_fpclass = compare

  type t = Not [@@deriving eq, ord]

  let pp_signed ft b = Fmt.string ft (if b then "s" else "u")
  let pp ft = function Not -> Fmt.string ft "!"
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
  [@@deriving eq, show { with_path = false }, ord]

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
end

let pp_hash_consed pp_node ft t = pp_node ft t.node
let equal_hash_consed _ t1 t2 = Int.equal t1.tag t2.tag
let compare_hash_consed _ t1 t2 = Int.compare t1.tag t2.tag

type t_kind =
  | Var of Var.t
  | Bool of bool
  | Int of Z.t [@printer Fmt.of_to_string Z.to_string]
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
  | Bool _ | Int _ -> ()
  | Binop (_, l, r) ->
      iter_vars l f;
      iter_vars r f
  | Unop (_, sv) -> iter_vars sv f
  | Nop (_, l) -> List.iter (fun sv -> iter_vars sv f) l
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

let sure_neq a b =
  (not (equal_ty a.node.ty b.node.ty))
  ||
  match (a.node.kind, b.node.kind) with
  | Int a, Int b -> not (Z.equal a b)
  | Bool a, Bool b -> a <> b
  | _ -> false

module Hcons = Hc.Make (struct
  type t = t_node

  let equal = equal_t_node

  (* We could do a lot more efficient in terms of hashing probably,
     if this ever becomes a bottleneck. *)
  let hash { kind; ty } =
    let hty = Hashtbl.hash ty in
    match kind with
    | Var _ | Bool _ | Int _ -> Hashtbl.hash (kind, hty)
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
  | Bool _ | Int _ -> sv
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
  match (guard.node.kind, if_.node.kind, else_.node.kind) with
  | Bool true, _, _ -> if_
  | Bool false, _, _ -> else_
  | _, Bool true, Bool false -> guard
  | _, Bool false, Bool true -> not guard
  | _, Bool false, _ -> and_ (not guard) else_
  | _, Bool true, _ -> or_ guard else_
  | _, _, Bool false -> and_ guard if_
  | _, _, Bool true -> or_ (not guard) if_
  | _ when equal if_ else_ -> if_
  | _ -> Ite (guard, if_, else_) <| if_.node.ty

(** {2 Integers} *)

let int_z z = Int z <| TInt
let int i = int_z (Z.of_int i)

let nonzero_z z =
  if Z.equal Z.zero z then raise (Invalid_argument "nonzero_z") else int_z z

let nonzero x = if x = 0 then raise (Invalid_argument "nonzero") else int x
let zero = int_z Z.zero
let one = int_z Z.one

let rec add v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, _ when equal v1 zero -> v2
  | _, _ when equal v2 zero -> v1
  | Int i1, Int i2 -> int_z (Z.add i1 i2)
  | Binop (Plus, v1, { node = { kind = Int i2; _ }; _ }), Int i3 ->
      add v1 (int_z (Z.add i2 i3))
  | Binop (Plus, { node = { kind = Int i1; _ }; _ }, v2), Int i3 ->
      add (int_z (Z.add i1 i3)) v2
  | Int i1, Binop (Plus, v1, { node = { kind = Int i2; _ }; _ }) ->
      add (int_z (Z.add i1 i2)) v1
  | Int i1, Binop (Plus, { node = { kind = Int i2; _ }; _ }, v2) ->
      add (int_z (Z.add i1 i2)) v2
  | _ -> mk_commut_binop Plus v1 v2 <| TInt

let rec sub v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, _ when equal v2 zero -> v1
  | Int i1, Int i2 -> int_z (Z.sub i1 i2)
  | Var v1, Var v2 when Var.equal v1 v2 -> zero
  | Binop (Minus, { node = { kind = Int i2; _ }; _ }, v1), Int i1 ->
      sub (int_z (Z.sub i2 i1)) v1
  | Binop (Minus, v1, { node = { kind = Int i2; _ }; _ }), Int i1 ->
      sub v1 (int_z (Z.add i2 i1))
  | Int i1, Binop (Minus, { node = { kind = Int i2; _ }; _ }, v1) ->
      add (int_z (Z.sub i1 i2)) v1
  | Int i1, Binop (Minus, v1, { node = { kind = Int i2; _ }; _ }) ->
      sub (int_z (Z.add i1 i2)) v1
  | Binop (Plus, x, y), _ when equal x v2 -> y
  | Binop (Plus, x, y), _ when equal y v2 -> x
  | _, Binop (Plus, x, y) when equal x v1 -> sub zero y
  | _, Binop (Plus, x, y) when equal y v1 -> sub zero x
  | _ -> Binop (Minus, v1, v2) <| TInt

let mul v1 v2 =
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
  | _, _ when equal v2 one -> zero
  | _, Int i2 when is_mod v1 i2 -> zero
  | Int i1, Int i2 -> int_z (Z.rem i1 i2)
  | Binop (Times, v1, n), Binop (Times, v2, m) when equal n m ->
      mul n (rem v1 v2)
  | Binop (Times, n, v1), Binop (Times, m, v2) when equal n m ->
      mul n (rem v1 v2)
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
  (* (x + (y % n)) % m when n | m <=> (x + y) % m *)
  | ( Binop
        ( Plus,
          x,
          {
            node =
              { kind = Binop (Mod, l, { node = { kind = Int m1; _ }; _ }); _ };
            _;
          } ),
      Int m2 )
    when Z.geq m1 m2 && Z.divisible m1 m2 ->
      mod_ (add x l) v2
  | _ -> Binop (Mod, v1, v2) <| TInt

let neg v = match v.node.kind with Int i -> int_z (Z.neg i) | _ -> sub zero v

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
  | Int _, Ite (b, t, e) -> ite b (lt v1 t) (lt v1 e)
  | Ite (b, t, e), Int _ -> ite b (lt t v2) (lt e v2)
  | _ -> Binop (Lt, v1, v2) <| TBool

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
  | Int y, Binop (Rem, _, { node = { kind = Int x; _ }; _ })
    when Z.leq y (Z.neg (Z.abs x)) ->
      v_true
  | Int y, Binop (Mod, _, _) when Z.leq y Z.zero -> v_true
  | Int _, Ite (b, t, e) -> ite b (leq v1 t) (leq v1 e)
  | Ite (b, t, e), Int _ -> ite b (leq t v2) (leq e v2)
  | _ -> Binop (Leq, v1, v2) <| TBool

let geq v1 v2 = leq v2 v1
let gt v1 v2 = lt v2 v1

let rec sem_eq v1 v2 =
  if equal v1 v2 then v_true
  else
    match (v1.node.kind, v2.node.kind) with
    | Int z1, Int z2 -> bool (Z.equal z1 z2)
    | Bool b1, Bool b2 -> bool (b1 = b2)
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
    | Binop (Minus, z, { node = { kind = Int x; _ }; _ }), Int y
    | Int y, Binop (Minus, z, { node = { kind = Int x; _ }; _ }) ->
        sem_eq z (int_z @@ Z.add y x)
    | Int y, Binop (Times, { node = { kind = Int x; _ }; _ }, v1)
    | Int y, Binop (Times, v1, { node = { kind = Int x; _ }; _ })
    | Binop (Times, v1, { node = { kind = Int x; _ }; _ }), Int y
    | Binop (Times, { node = { kind = Int x; _ }; _ }, v1), Int y ->
        if Z.equal Z.zero x then bool (Z.equal Z.zero y)
        else if Z.(equal zero (rem y x)) then sem_eq v1 (int_z Z.(y / x))
        else v_false
    | _ -> mk_commut_binop Eq v1 v2 <| TBool

let sem_eq_untyped v1 v2 =
  if equal_ty v1.node.ty v2.node.ty then sem_eq v1 v2 else v_false

(** {2 General constructors} *)

let mk_unop (op : Unop.t) v = match op with Not -> not v

let mk_binop (op : Binop.t) v1 v2 =
  match op with
  | And -> and_ v1 v2
  | Or -> or_ v1 v2
  | Eq -> sem_eq v1 v2
  | Leq -> leq v1 v2
  | Lt -> lt v1 v2
  | Plus -> add v1 v2
  | Minus -> sub v1 v2
  | Times -> mul v1 v2
  | Div -> div v1 v2
  | Rem -> rem v1 v2
  | Mod -> mod_ v1 v2

let mk_nop (op : Nop.t) vs = match op with Nop.Distinct -> distinct vs

(** {2 Infix operators} *)

module Infix = struct
  let int_z = int_z
  let int = int
  let ( ==@ ) = sem_eq
  let ( ==?@ ) = sem_eq_untyped
  let ( >@ ) = gt
  let ( >=@ ) = geq
  let ( <@ ) = lt
  let ( <=@ ) = leq
  let ( &&@ ) = and_
  let ( ||@ ) = or_
  let ( +@ ) = add
  let ( -@ ) = sub
  let ( ~- ) = neg
  let ( *@ ) = mul
  let ( /@ ) = div
  let ( %@ ) = mod_
end

module Syntax = struct
  module Sym_int_syntax = struct
    let mk_nonzero = nonzero
    let[@inline] zero () = zero
    let[@inline] one () = one
  end
end
