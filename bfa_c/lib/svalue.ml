open Hashcons

module Var_name = struct
  type t = int

  let next = ref 0
  let to_string i = "|" ^ string_of_int i ^ "|"
  let of_string s = int_of_string (String.sub s 1 (String.length s - 2))
  let pp = Fmt.of_to_string to_string

  let fresh () =
    let r = !next in
    incr next;
    r

  let equal = Int.equal
  let compare = Int.compare
end

type ty = TBool | TInt | TPointer | TSeq of ty | TOption of ty
[@@deriving eq, show, ord]

let t_bool = TBool
let t_int = TInt
let t_ptr = TPointer
let t_seq ty = TSeq ty
let t_opt ty = TOption ty

module Nop = struct
  type t = Distinct [@@deriving eq, show, ord]
end

module Unop = struct
  type t = Not | IsSome | IsNone | UnwrapOpt | GetPtrLoc | GetPtrOfs
  [@@deriving eq, show, ord]
end

module Binop = struct
  type t =
    | (* Bool *) And
    | (* Comparison *) Eq
    | Geq
    | Gt
    | Leq
    | Lt
    | Plus
    | Minus
    | Times
    | Div
  [@@deriving eq, show { with_path = false }, ord]
end

let pp_hash_consed pp_node ft t = pp_node ft t.node
let equal_hash_consed _ t1 t2 = Int.equal t1.tag t2.tag
let compare_hash_consed _ t1 t2 = Int.compare t1.tag t2.tag

type t_node =
  | Var of (Var_name.t * ty)
  | Bool of bool
  | Int of Z.t [@printer Fmt.of_to_string Z.to_string]
  | Ptr of (t * t)
  (* | BitVec of (Z.t * int) *)
  | Seq of t list
  | Unop of (Unop.t * t)
  | Binop of (Binop.t * t * t)
  | Nop of Nop.t * t list
  | Opt of t option

and t = t_node hash_consed [@@deriving show { with_path = false }, eq, ord]

let pp ft t = pp_t_node ft t.node
let equal a b = Int.equal a.tag b.tag

module Hcons = Hashcons.Make (struct
  type t = t_node

  let equal = equal_t_node
  let hash = Hashtbl.hash
end)

let table = Hcons.create 1023
let hashcons = Hcons.hashcons table

let fresh ty =
  let v = Var_name.fresh () in
  hashcons (Var (v, ty))

(** {2 Booleans}  *)

let v_true = hashcons (Bool true)
let v_false = hashcons (Bool false)

let bool b =
  (* avoid hashconsing re-alloc *)
  if b then v_true else v_false

let sem_eq v1 v2 =
  match (v1.node, v2.node) with
  | Int z1, Int z2 -> bool (Z.equal z1 z2)
  | Bool b1, Bool b2 -> bool (b1 = b2)
  | _ ->
      if equal v1 v2 then v_true (* Start with a syntactic check *)
      else hashcons (Binop (Eq, v1, v2))

let and_ v1 v2 =
  match (v1.node, v2.node) with
  | Bool b1, Bool b2 -> bool (b1 && b2)
  | Bool false, _ | _, Bool false -> v_false
  | Bool true, _ -> v2
  | _, Bool true -> v1
  | _ -> hashcons (Binop (And, v1, v2))

let not sv =
  if equal sv v_true then v_false
  else if equal sv v_false then v_true
  else hashcons (Unop (Not, sv))

let distinct l = hashcons (Nop (Distinct, l))

(** {2 Integers}  *)

let int_z z = hashcons (Int z)
let int i = int_z (Z.of_int i)
let zero = int_z Z.zero
let one = int_z Z.one

(** [out_cons] is the outcome constructor, [f] is the function to apply to the int values, [b] is the binop *)
let lift_int_binop ~out_cons ~f ~binop v1 v2 =
  match (v1.node, v2.node) with
  | Int i1, Int i2 -> out_cons (f i1 i2)
  | _ -> hashcons (Binop (binop, v1, v2))

let geq = lift_int_binop ~out_cons:bool ~f:Z.geq ~binop:Geq
let leq = lift_int_binop ~out_cons:bool ~f:Z.leq ~binop:Leq
let lt = lift_int_binop ~out_cons:bool ~f:Z.lt ~binop:Lt
let gt = lift_int_binop ~out_cons:bool ~f:Z.gt ~binop:Gt
let plus = lift_int_binop ~out_cons:int_z ~f:Z.add ~binop:Plus
let minus = lift_int_binop ~out_cons:int_z ~f:Z.sub ~binop:Minus
let times = lift_int_binop ~out_cons:int_z ~f:Z.mul ~binop:Times
let div = lift_int_binop ~out_cons:int_z ~f:Z.div ~binop:Div

(** {2 Pointers} *)

module Ptr = struct
  let mk l o = hashcons (Ptr (l, o))

  let loc p =
    match p.node with Ptr (l, _) -> l | _ -> hashcons (Unop (GetPtrLoc, p))

  let ofs p =
    match p.node with Ptr (_, o) -> o | _ -> hashcons (Unop (GetPtrOfs, p))

  let null = mk zero zero
  let is_null p = sem_eq p null
end

module SOption = struct
  let is_some v =
    match v.node with
    | Opt (Some _) -> v_true
    | Opt None -> v_false
    | _ -> hashcons (Unop (IsSome, v))

  let is_none v =
    match v.node with
    | Opt None -> v_true
    | Opt (Some _) -> v_false
    | _ -> hashcons (Unop (IsNone, v))

  let none = hashcons (Opt None)
  let some v = hashcons (Opt (Some v))

  let unwrap v =
    match v.node with
    | Opt (Some v) -> v
    | Opt None -> failwith "opt_unwrap: got None"
    | _ -> hashcons (Unop (UnwrapOpt, v))
end

(** {2 Sequences} *)

module SSeq = struct
  let mk l = hashcons (Seq l)
end

(** {2 Infix operators}  *)

module Infix = struct
  let int_z = int_z
  let int = int
  let ptr = Ptr.mk
  let seq = SSeq.mk
  let ( #== ) = sem_eq
  let ( #> ) = gt
  let ( #>= ) = geq
  let ( #< ) = lt
  let ( #<= ) = leq
  let ( #&& ) = and_
  let ( #+ ) = plus
  let ( #- ) = minus
  let ( #* ) = times
  let ( #/ ) = div
end
