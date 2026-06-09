open Charon

module Map = Stdlib.Map.Make (struct
  type t = Expressions.LocalId.id

  let compare = Expressions.LocalId.compare_id
end)

module Place = struct
  type kind =
    | Local of Expressions.local_id
    | Field of t * Expressions.field_proj_kind * Types.field_id
    | Index of t * int (* Store places only support concrete offsets *)

  (** A simple place that lives on the store and doesn't need to be decayed to
      the heap for reading or writing. Almost identical to {!Expressions.place},
      but with only field and concrete index access. *)
  and t = { kind : kind; ty : Types.ty }

  let rec pp ft { kind; _ } =
    match kind with
    | Local v -> Expressions.pp_local_id ft v
    | Field (base, _, field) ->
        Fmt.pf ft "%a.%a" pp base Types.pp_field_id field
    | Index (base, idx) -> Fmt.pf ft "%a[%d]" pp base idx

  (** Local root of the place *)
  let rec root p =
    match p.kind with
    | Local v -> v
    | Field (base, _, _) | Index (base, _) -> root base

  (** The root value [v] with the value at [sp] replaced by [f] applied to it;
      [None] if not navigable. *)
  let rec update_val { kind; _ } ~f v =
    match kind with
    | Local _ -> f v
    | Field (base, _, field) ->
        update_val base
          ~f:(Rust_val.update_field (Types.FieldId.to_int field) ~f)
          v
    | Index (base, idx) -> update_val base ~f:(Rust_val.update_field idx ~f) v

  let is_local p = match p.kind with Local _ -> true | _ -> false
end

open Place

module Binding = struct
  (** We have four kinds of bindings:

      - Stackptr: the symbol is bound to a stack pointer, that lives in the
        heap.
      - Value: the symbol is bound to an immediate value; it does not have an
        address.
      - Uninit: the symbol is bound to an immediate, uninitialized value.
      - Dead: the symbol is dead; it doesn't exist (e.g. after a [StorageDead]).
  *)
  type 'a kind =
    | Stackptr of 'a Rust_val.full_ptr
    | Value of 'a Rust_val.t
    | Uninit
    | Dead
  [@@deriving show { with_path = false }]

  type 'a t = { kind : 'a kind; ty : Types.ty }
  [@@deriving show { with_path = false }]

  let as_value = function { kind = Value v; _ } -> Some v | _ -> None

  let bind_value (f : 'a Rust_val.t -> 'a kind option) (x : 'a kind option) :
      'a kind option =
    match x with
    | Some (Value v) -> f v
    | Some Uninit -> Some Uninit
    | Some Dead -> Some Dead
    | Some (Stackptr _) | None -> None
end

open Binding

type 'a t = 'a Binding.t Map.t

let pp ft store =
  let pp_binding ft { kind; ty } =
    match kind with
    | Stackptr ptr ->
        Fmt.pf ft "Stackptr(%a) : %a"
          (Rust_val.pp_full_ptr Fmt.nop)
          ptr Types.pp_ty ty
    | Value v ->
        Fmt.pf ft "Value(%a) : %a" (Rust_val.pp Fmt.nop) v Types.pp_ty ty
    | Uninit -> Fmt.pf ft "Uninit : %a" Types.pp_ty ty
    | Dead -> Fmt.pf ft "Dead : %a" Types.pp_ty ty
  in
  Map.iter
    (fun sym binding ->
      Fmt.pf ft "%a -> %a@." Expressions.pp_local_id sym pp_binding binding)
    store

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

(** Returns [Some v] if a place can be loaded directly from the store, and
    [None] if it has to be spilled into the heap. *)
let rec try_load (place : Place.t) (store : 'a t) : 'a Binding.kind option =
  match place.kind with
  | Local v -> Some (find v store).kind
  | Field (base, _, field) -> (
      let field_idx = Types.FieldId.to_int field in
      try_load base store
      |> bind_value @@ function
         | Tuple vs | Enum (_, vs) ->
             Option.map (fun v -> Value v) (List.nth_opt vs field_idx)
         | _ -> None)
  | Index (base, idx) -> (
      try_load base store
      |> bind_value @@ function
         | Tuple vs -> Option.map (fun v -> Value v) (List.nth_opt vs idx)
         | _ -> None)

let try_store (place : Place.t) store value =
  let open Syntaxes.Option in
  let root = Place.root place in
  let+ new_val =
    match (find root store).kind with
    | Value c -> Place.update_val place ~f:(fun _ -> Some value) c
    | Uninit when Place.is_local place -> Some value
    | _ -> None
  in
  declare_value root new_val store

let try_uninit (place : Place.t) store =
  (* Can only uninitialise a whole variable *)
  match place.kind with
  | Local v -> Some (declare_uninit v store)
  | _ -> None

type discr_ret = DVariant of Typed.(T.sint t) | DUninit | DDead

let try_load_discriminant (place : Place.t) store =
  let open Syntaxes.Option in
  let* binding = try_load place store in
  match binding with
  | Value (Enum (discriminant, _)) -> Some (DVariant discriminant)
  | Uninit -> Some DUninit
  | Dead -> Some DDead
  | _ -> None
