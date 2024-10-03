open Hashcons

module Var = struct
  type t = int

  let equal = Int.equal
  let next = ref 0
  let to_string i = "V" ^ string_of_int i
  let pp = Fmt.of_to_string to_string

  let fresh () =
    let r = !next in
    incr next;
    r

  let eq = Int.equal
end

type ty =
  | Bool
  | Int
  | Pointer
  (* | BitVec of int *)
  | Seq of ty
[@@deriving eq, show]

let pp_hash_consed pp_node ft t = pp_node ft t.node
let equal_hash_consed _ t1 t2 = Int.equal t1.tag t2.tag

type t_node =
  | Var of (Var.t * ty)
  | Bool of bool
  | Int of Z.t [@printer Fmt.of_to_string Z.to_string]
  | Ptr of (t * t)
  (* | BitVec of (Z.t * int) *)
  | Seq of t list

and t = t_node hash_consed [@@deriving show { with_path = false }, eq]

let pp ft t = pp_t_node ft t.node

module Hcons = Hashcons.Make (struct
  type t = t_node

  let equal = equal_t_node
  let hash = Hashtbl.hash
end)

let table = create 1024
let hashcons = hashcons table

let fresh ty =
  let v = Var.fresh () in
  hashcons (Var (v, ty))

let bool b = hashcons (Bool b)
let int_z z = hashcons (Int z)
let int i = int_z (Z.of_int i)
let ptr l o = hashcons (Ptr (l, o))
let seq s = hashcons (Seq s)
let zero = int_z Z.zero
let one = int_z Z.one
