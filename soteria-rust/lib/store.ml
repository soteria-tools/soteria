open Charon
open Svalue

module Map = Stdlib.Map.Make (struct
  type t = Expressions.LocalId.id

  let compare = Expressions.LocalId.compare_id
end)

(** We have four kinds of bindings:

    - Stackptr: the symbol is bound to a stack pointer, that lives in the heap.
    - Value: the symbol is bound to an immediate value; it does not have an
      address.
    - Uninit: the symbol is bound to an immediate, uninitialized value.
    - Dead: the symbol is dead; it doesn't exist (e.g. after a [StorageDead]).
*)
type binding_kind =
  | Stackptr of Typed.T.sptr_f Typed.t
  | Value of Typed.T.any Typed.t
  | Uninit
  | Dead
[@@deriving show { with_path = false }]

type binding = { kind : binding_kind; ty : Types.ty }
[@@deriving show { with_path = false }]

type t = binding Map.t

let reserve sym ty =
  let binding = { kind = Dead; ty } in
  Map.add sym binding

let[@inline] declare sym kind =
  Map.update sym (function
    | None -> failwith "Store: Assigning unknown symbol?"
    | Some { kind = _; ty } -> Some { kind; ty })

let declare_value sym value t = declare sym (Value value) t
let declare_ptr sym ptr t = declare sym (Stackptr ptr) t
let declare_uninit sym t = declare sym Uninit t
let dealloc sym t = declare sym Dead t

let get_ty sym t =
  match Map.find_opt sym t with
  | None -> failwith "Store: Getting type of unknown symbol?"
  | Some { ty; _ } -> ty

let find local (store : t) = Map.find local store
let empty = Map.empty
let bindings (store : t) = Map.bindings store
