open Charon

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
type 'a binding_kind =
  | Stackptr of 'a Rust_val.full_ptr
  | Value of 'a Rust_val.t
  | Uninit
  | Dead
[@@deriving show { with_path = false }]

type 'a binding = { kind : 'a binding_kind; ty : Types.ty }
[@@deriving show { with_path = false }]

type 'a t = 'a binding Map.t

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

let find local (store : 'a t) = Map.find local store
let empty = Map.empty
let bindings (store : 'a t) = Map.bindings store
