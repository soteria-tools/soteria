open Charon
module T = Typed.T

type meta = T.cval Typed.t option
and 'ptr full_ptr = 'ptr * meta [@@deriving show { with_path = false }]

type 'ptr t =
  | Base of T.cval Typed.t
  | Ptr of 'ptr full_ptr
      (** pointer, parametric to enable Ruxt, with optional meta *)
  | Enum of T.cval Typed.t * 'ptr t list  (** discriminant * values *)
  | Struct of 'ptr t list  (** contains ordered fields *)
  | Tuple of 'ptr t list
  | Array of 'ptr t list
  | Union of Types.field_id * 'ptr t  (** field and value of union *)
  | ConstFn of Types.fn_ptr

type 'ptr rust_val = 'ptr t

(** [is_empty v] is true if the value is "empty"; ie. it doesn't contain any
    [Base] or [Ptr] value. We also consider [Enum] to be non-empty, because of
    the discriminant, though this *may* be wrong if it's a ZST.

    Used in unsizing, to find the field with the pointer to modify. *)
let rec is_empty = function
  | Base _ -> false
  | Ptr _ -> false
  | Enum (_, _) -> false
  | ConstFn _ -> false
  | Struct fields -> List.for_all is_empty fields
  | Tuple vals -> List.for_all is_empty vals
  | Array vals -> List.for_all is_empty vals
  | Union (_, v) -> is_empty v

let rec pp_rust_val pp_ptr fmt =
  let pp_rust_val = pp_rust_val pp_ptr in
  function
  | Base v -> Fmt.pf fmt "%a" Typed.ppa v
  | Ptr (p, None) -> Fmt.pf fmt "Ptr(%a)" pp_ptr p
  | Ptr (p, Some meta) -> Fmt.pf fmt "Ptr(%a, %a)" pp_ptr p Typed.ppa meta
  | Enum (disc, vals) ->
      Fmt.pf fmt "Enum(%a: %a)" Typed.ppa disc
        (Fmt.list ~sep:(Fmt.any ", ") pp_rust_val)
        vals
  | Struct fields ->
      Fmt.pf fmt "{%a}" (Fmt.list ~sep:(Fmt.any ", ") pp_rust_val) fields
  | Tuple vals ->
      Fmt.pf fmt "(%a)" (Fmt.list ~sep:(Fmt.any ", ") pp_rust_val) vals
  | Array vals ->
      Fmt.pf fmt "[%a]" (Fmt.list ~sep:(Fmt.any ", ") pp_rust_val) vals
  | Union (field_id, v) ->
      Fmt.pf fmt "Union(%a: %a)" Types.pp_field_id field_id pp_rust_val v
  | ConstFn fn_ptr -> Fmt.pf fmt "FnPtr(%a)" Types.pp_fn_ptr fn_ptr

let pp = pp_rust_val
let ppa_rust_val ft rv = pp_rust_val (Fmt.any "?") ft rv
let unit_ = Tuple []

let as_ptr = function
  | Ptr ptr -> ptr
  | v ->
      Fmt.failwith "Unexpected rust_val kind, expected a pointer, got: %a"
        ppa_rust_val v

let as_base = function
  | Enum (v, []) | Base v -> v
  | v ->
      Fmt.failwith "Unexpected rust_val kind, expected a base value got: %a"
        ppa_rust_val v

let as_base_of ~ty = function
  | Enum (v, []) | Base v -> (
      match Typed.cast_checked v ty with
      | Some v -> v
      | None ->
          Fmt.failwith "Unexpected rust_val type, expected %a, got %a (%a)"
            Typed.ppa_ty ty Typed.ppa v Svalue.pp_ty (Typed.get_ty v))
  | v ->
      Fmt.failwith "Unexpected rust_val kind, expected a base value got: %a"
        ppa_rust_val v
