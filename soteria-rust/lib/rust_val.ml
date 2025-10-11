open Charon
module T = Typed.T

type 'ptr meta = Thin | Len of T.sint Typed.t | VTable of 'ptr
and 'ptr full_ptr = 'ptr * 'ptr meta

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

let pp_meta pp_ptr fmt = function
  | Thin -> Fmt.pf fmt "-"
  | Len v -> Fmt.pf fmt "%a" Typed.ppa v
  | VTable p -> Fmt.pf fmt "%a" pp_ptr p

let pp_full_ptr pp_ptr fmt = function
  | p, Thin -> Fmt.pf fmt "(%a)" pp_ptr p
  | p, meta -> Fmt.pf fmt "(%a, %a)" pp_ptr p (pp_meta pp_ptr) meta

let rec pp_rust_val pp_ptr fmt =
  let pp_rust_val = pp_rust_val pp_ptr in
  function
  | Base v -> Typed.ppa fmt v
  | Ptr ptr -> Fmt.pf fmt "Ptr%a" (pp_full_ptr pp_ptr) ptr
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
  | ConstFn fn_ptr -> Fmt.pf fmt "ConstFn(%a)" Types.pp_fn_ptr fn_ptr

let pp = pp_rust_val
let ppa_rust_val ft rv = pp_rust_val (Fmt.any "?") ft rv
let unit_ = Tuple []

let as_ptr = function
  | Ptr ptr -> ptr
  | v ->
      Fmt.failwith "Unexpected rust_val kind, expected a pointer, got: %a"
        ppa_rust_val v

let as_ptr_or ~make = function
  | Ptr ptr -> ptr
  | Base v -> (make @@ Typed.cast_i Usize v, Thin)
  | v ->
      Fmt.failwith
        "Unexpected rust_val kind, expected a pointer or base, got: %a"
        ppa_rust_val v

let as_base_f ty = function
  | Base v -> Typed.cast_f ty v
  | v ->
      Fmt.failwith "Unexpected rust_val kind, expected a base value got: %a"
        ppa_rust_val v

let as_base ty = function
  | Base v -> Typed.cast_lit ty v
  | v ->
      Fmt.failwith "Unexpected rust_val kind, expected a base value got: %a"
        ppa_rust_val v

let as_base_i ty = as_base (TUInt ty)

let flatten v =
  let rec aux acc = function
    | Tuple vs | Struct vs | Enum (_, vs) | Array vs ->
        List.fold_left aux acc vs
    | Union (_, v) -> aux acc v
    | (Base _ | Ptr _ | ConstFn _) as v -> v :: acc
  in
  List.rev (aux [] v)

let rec iter_vars ptr_iter_vars rv f =
  let iter_vars = iter_vars ptr_iter_vars in
  match rv with
  | Base v -> Typed.iter_vars v f
  | Union (_, v) -> iter_vars v f
  | Enum (disc, vals) ->
      Typed.iter_vars disc f;
      List.iter (fun v -> iter_vars v f) vals
  | Struct vals | Tuple vals | Array vals ->
      List.iter (fun v -> iter_vars v f) vals
  | Ptr (p, meta) ->
      (match meta with
      | Thin -> ()
      | Len v -> Typed.iter_vars v f
      | VTable v -> ptr_iter_vars v f);
      ptr_iter_vars p f
  | ConstFn _ -> ()

let rec subst ptr_subst subst_var rv =
  let subst = subst ptr_subst subst_var in
  let map_subst vals = List.map subst vals in
  match rv with
  | Base v -> Base (Typed.subst subst_var v)
  | Union (field_id, v) -> Union (field_id, subst v)
  | Enum (disc, vals) -> Enum (Typed.subst subst_var disc, map_subst vals)
  | Struct vals -> Struct (map_subst vals)
  | Tuple vals -> Tuple (map_subst vals)
  | Array vals -> Array (map_subst vals)
  | Ptr (p, meta) ->
      let meta =
        match meta with
        | Thin -> Thin
        | Len len -> Len (Typed.subst subst_var len)
        | VTable ptr -> VTable (ptr_subst subst_var ptr)
      in
      Ptr (ptr_subst subst_var p, meta)
  | ConstFn _ -> rv
