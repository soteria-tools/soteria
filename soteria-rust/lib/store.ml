open Charon
open Common.Charon_util

module Map = Stdlib.Map.Make (struct
  type t = Expressions.LocalId.id

  let compare = Expressions.LocalId.compare_id
end)

module Place = struct
  type kind =
    | Local of Expressions.local_id
    | Field of t * Expressions.field_proj_kind * Types.field_id
    | Index of t * int (* Store places only support concrete offsets *)
    | Metadata of t (* The metadata "field" of a (possibly fat) pointer *)

  (** A simple place that lives on the store and doesn't need to be decayed to
      the heap for reading or writing. Almost identical to {!Expressions.place},
      but with only field and concrete index access. *)
  and t = { kind : kind; origin : Expressions.place }

  let rec pp ft { kind; _ } =
    match kind with
    | Local v -> Expressions.pp_local_id ft v
    | Field (base, _, field) ->
        Fmt.pf ft "%a.%a" pp base Types.pp_field_id field
    | Index (base, idx) -> Fmt.pf ft "%a[%d]" pp base idx
    | Metadata base -> Fmt.pf ft "%a.metadata" pp base

  (** Local root of the place *)
  let rec root p =
    match p.kind with
    | Local v -> v
    | Field (base, _, _) | Index (base, _) | Metadata base -> root base

  (** The root value [v] with the value at [sp] replaced by [f] applied to it;
      [None] if not navigable. *)
  let rec update_val { kind; _ } ~f v =
    match kind with
    | Local _ -> Some (f v)
    | Field (base, _, field) ->
        update_val base
          ~f:(Rust_val.update_field (Types.FieldId.to_int field) ~f)
          v
    | Index (base, idx) -> update_val base ~f:(Rust_val.update_field idx ~f) v
    (* metadata isn't navigable for in-place writes; spill to the heap *)
    | Metadata _ -> None

  let is_local = function { kind = Local _; _ } -> true | _ -> false

  let local local_id ty =
    { kind = Local local_id; origin = { kind = PlaceLocal local_id; ty } }
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

  let pp ft { kind; ty } =
    match kind with
    | Stackptr ptr ->
        Fmt.pf ft "Stackptr(%a) : %a"
          (Rust_val.pp_full_ptr Fmt.nop)
          ptr pp_ty ty
    | Value v -> Fmt.pf ft "Value(%a) : %a" (Rust_val.pp Fmt.nop) v pp_ty ty
    | Uninit -> Fmt.pf ft "Uninit : %a" pp_ty ty
    | Dead -> Fmt.pf ft "Dead : %a" pp_ty ty

  let as_value = function { kind = Value v; _ } -> Some v | _ -> None

  let bind_value (f : 'a Rust_val.t -> 'a kind) :
      'a kind option -> 'a kind option = function
    | Some (Value v) -> Some (f v)
    | (Some Uninit | Some Dead) as v -> v
    | Some (Stackptr _) | None -> None
end

open Binding

type 'a t = 'a Binding.t Map.t

let pp ft s =
  Fmt.(
    iter_bindings ~sep:(any "@.") Map.iter
      (pair ~sep:(any " -> ") Expressions.pp_local_id Binding.pp))
    ft s

let reserve sym ty =
  let binding = { kind = Dead; ty } in
  Map.add sym binding

let[@inline] declare sym kind =
  Map.update sym (function
    | None -> L.failwith "Store: Assigning unknown symbol?"
    | Some { kind = _; ty } -> Some { kind; ty })

let declare_value sym value t = declare sym (Value value) t
let declare_ptr sym ptr t = declare sym (Stackptr ptr) t
let declare_uninit sym t = declare sym Uninit t
let dealloc sym t = declare sym Dead t
let get_ty sym t = (Map.find sym t).ty
let find local (store : 'a t) = Map.find local store
let empty = Map.empty
let bindings (store : 'a t) = Map.bindings store

(** [try_load p s] tries loading [p] from the [s], returning [Some v] if it
    succeded, and [None] if it has to be spilled into the heap. *)
let rec try_load (place : Place.t) (store : 'a t) : 'a Binding.kind option =
  match place.kind with
  | Local v -> Some (find v store).kind
  | Field (base, _, field) -> (
      let field_idx = Types.FieldId.to_int field in
      try_load base store
      |> bind_value @@ function
         | Tuple vs | Enum (_, vs) -> Value (List.nth vs field_idx)
         | _ -> L.failwith "tried loading field of non-aggregate")
  | Index (base, idx) -> (
      try_load base store
      |> bind_value @@ function
         | Tuple vs -> Value (List.nth vs idx)
         | _ -> L.failwith "tried loading index of non-tuple")
  | Metadata base -> (
      try_load base store
      |> bind_value @@ fun v ->
         (* the metadata projection output: [()] for thin pointer, [usize] for
            slices, and [ptr::DynMetadata] for VTables. However our frontend
            likes to sometimes treat the metadata as a raw pointer, so we much
            check for that. Additionally, we must flatten the value, because a
            valid metadata target can be e.g. [Box<T>]. *)
         let ptr, meta =
           match base.origin.ty with
           | TRawPtr _ | TRef _ -> Rust_val.as_ptr v
           | TAdt adt when adt_is_box adt -> Value_codec.ptr_of_box v
           | _ ->
               L.failwith "tried loading metadata of non-pointer: %a" pp_ty
                 base.origin.ty
         in
         match meta with
         | Thin -> Value (Tuple [])
         | Len len -> Value (Int len)
         | VTable vt ->
             let vt = Rust_val.Ptr (vt, Thin) in
             if
               Option.is_some_and
                 (Crate.adt_has_lang_item "dyn_metadata")
                 (ty_as_adt_opt place.origin.ty)
             then Value (Tuple [ Tuple [ vt ]; Tuple [] ])
             else Value vt)

let try_store (place : Place.t) store value =
  let open Syntaxes.Option in
  let root = Place.root place in
  let+ new_val =
    match (find root store).kind with
    | Value c -> Place.update_val place ~f:(fun _ -> value) c
    | Uninit when Place.is_local place -> Some value
    | _ -> None
  in
  declare_value root new_val store
