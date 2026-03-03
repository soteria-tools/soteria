open Typed

type ('v, 'ptr) meta_raw = Thin | Len of 'v | VTable of 'ptr
type 'ptr meta = (T.sint Typed.t, 'ptr) meta_raw
type ('v, 'ptr) full_ptr_raw = 'ptr * ('v, 'ptr) meta_raw
type 'ptr full_ptr = 'ptr * 'ptr meta

type ('sint, 'sfloat, 'ptr) raw =
  | Int of 'sint
  | Float of 'sfloat
  | Ptr of ('sint, 'ptr) full_ptr_raw
      (** pointer, parametric to enable Ruxt, with optional meta *)
  | Enum of 'sint * ('sint, 'sfloat, 'ptr) raw list
      (** discriminant * values *)
  | Tuple of ('sint, 'sfloat, 'ptr) raw list  (** contains ordered values *)
  | Union of (('sint, 'sfloat, 'ptr) raw * 'sint) list
      (** list of blocks in the union, with their offset *)
  | PolyVal of Charon.Types.type_var_id
      (** The opaque value of a type variable, identified by (type variable
          index, unique identifier). *)

type 'ptr t = (T.sint Typed.t, T.sfloat Typed.t, 'ptr) raw
type 'ptr rust_val = 'ptr t
type 'ptr syn = (Expr.t, Expr.t, 'ptr) raw

(** [is_empty v] is true if the value is "empty"; ie. it doesn't contain any
    [Base] or [Ptr] value. We also consider [Enum] to be non-empty, because of
    the discriminant, though this *may* be wrong if it's a ZST.

    Used in unsizing, to find the field with the pointer to modify. *)
let rec is_empty = function
  | Int _ | Float _ | Ptr _ | Enum (_, _) | PolyVal _ -> false
  | Tuple vals -> List.for_all is_empty vals
  (* I'm not sure about this one *)
  | Union _ -> false

let pp_meta_raw pp_v pp_ptr fmt = function
  | Thin -> Fmt.pf fmt "-"
  | Len v -> Fmt.pf fmt "%a" pp_v v
  | VTable p -> Fmt.pf fmt "%a" pp_ptr p

let pp_meta pp_ptr = pp_meta_raw Typed.ppa pp_ptr
let pp_meta_syn pp_ptr_syn = pp_meta_raw Expr.pp pp_ptr_syn

let pp_full_ptr_raw pp_v pp_ptr fmt = function
  | p, Thin -> Fmt.pf fmt "(%a)" pp_ptr p
  | p, meta -> Fmt.pf fmt "(%a, %a)" pp_ptr p (pp_meta_raw pp_v pp_ptr) meta

let pp_full_ptr pp_ptr = pp_full_ptr_raw Typed.ppa pp_ptr
let pp_full_ptr_syn pp_ptr_syn = pp_full_ptr_raw Expr.pp pp_ptr_syn

let rec pp pp_ptr fmt t =
  let pp = pp pp_ptr in
  match t with
  | Int v -> Typed.ppa fmt v
  | Float v -> Typed.ppa fmt v
  | Ptr ptr -> Fmt.pf fmt "Ptr%a" (pp_full_ptr pp_ptr) ptr
  | Enum (disc, vals) ->
      Fmt.pf fmt "Enum(%a: %a)" Typed.ppa disc
        (Fmt.list ~sep:(Fmt.any ", ") pp)
        vals
  | Tuple vals -> Fmt.pf fmt "(%a)" (Fmt.list ~sep:(Fmt.any ", ") pp) vals
  | Union vs ->
      let pp_block ft (v, ofs) = Fmt.pf ft "(%a: %a)" Typed.ppa ofs pp v in
      Fmt.pf fmt "Union(%a)" (Fmt.list ~sep:(Fmt.any ", ") pp_block) vs
  | PolyVal tid -> Fmt.pf fmt "PolyVal(%a)" Charon.Types.pp_type_var_id tid

let rec pp_syn pp_ptr fmt t =
  let pp_syn = pp_syn pp_ptr in
  match t with
  | Int v -> Expr.pp fmt v
  | Float v -> Expr.pp fmt v
  | Ptr ptr -> Fmt.pf fmt "Ptr%a" (pp_full_ptr_raw Expr.pp pp_ptr) ptr
  | Enum (disc, vals) ->
      Fmt.pf fmt "Enum(%a: %a)" Expr.pp disc
        (Fmt.list ~sep:(Fmt.any ", ") @@ pp_syn)
        vals
  | Tuple vals ->
      Fmt.pf fmt "(%a)" (Fmt.list ~sep:(Fmt.any ", ") @@ pp_syn) vals
  | Union vs ->
      let pp_block ft (v, ofs) = Fmt.pf ft "(%a: %a)" Expr.pp ofs pp_syn v in
      Fmt.pf fmt "Union(%a)" (Fmt.list ~sep:(Fmt.any ", ") pp_block) vs
  | PolyVal tid -> Fmt.pf fmt "PolyVal(%a)" Charon.Types.pp_type_var_id tid

let ppa ft rv = pp (Fmt.any "?") ft rv
let unit_ = Tuple []

let as_ptr = function
  | Ptr ptr -> ptr
  | v ->
      Fmt.failwith "Unexpected rust_val kind, expected a pointer, got: %a" ppa v

let as_ptr_or ~make = function
  | Ptr ptr -> ptr
  | Int v -> (make @@ Typed.cast_i Usize v, Thin)
  | v ->
      Fmt.failwith
        "Unexpected rust_val kind, expected a pointer or base, got: %a" ppa v

let as_base_f ty = function
  | Float v -> Typed.cast_f ty v
  | v ->
      Fmt.failwith "Unexpected rust_val kind, expected a base value got: %a" ppa
        v

let as_base ty = function
  | Int v -> Typed.cast_lit ty v
  | v ->
      Fmt.failwith "Unexpected rust_val kind, expected a base value got: %a" ppa
        v

let as_base_i ty = as_base (TUInt ty)

let as_tuple = function
  | Tuple vals -> vals
  | v ->
      Fmt.failwith "Unexpected rust_val kind, expected a tuple, got: %a" ppa v

let flatten v =
  let rec aux acc = function
    | Tuple vs | Enum (_, vs) -> List.fold_left aux acc vs
    | (Int _ | Float _ | Ptr _ | Union _ | PolyVal _) as v -> v :: acc
  in
  List.rev (aux [] v)

let rec iter_vars ptr_iter_vars rv f =
  let iter_vars = iter_vars ptr_iter_vars in
  match rv with
  | Int v -> Typed.iter_vars v f
  | Float v -> Typed.iter_vars v f
  | PolyVal _ -> ()
  | Union vs ->
      List.iter
        (fun (v, ofs) ->
          iter_vars v f;
          Typed.iter_vars ofs f)
        vs
  | Enum (disc, vals) ->
      Typed.iter_vars disc f;
      List.iter (fun v -> iter_vars v f) vals
  | Tuple vals -> List.iter (fun v -> iter_vars v f) vals
  | Ptr (p, meta) ->
      (match meta with
      | Thin -> ()
      | Len v -> Typed.iter_vars v f
      | VTable v -> ptr_iter_vars v f);
      ptr_iter_vars p f

let rec subst ptr_subst subst_var rv =
  let subst = subst ptr_subst subst_var in
  let map_subst vals = List.map subst vals in
  match rv with
  | Int v -> Int (Typed.subst subst_var v)
  | Float v -> Float (Typed.subst subst_var v)
  | PolyVal _ -> rv
  | Union vs ->
      Union (List.map (fun (v, ofs) -> (subst v, Typed.subst subst_var ofs)) vs)
  | Enum (disc, vals) -> Enum (Typed.subst subst_var disc, map_subst vals)
  | Tuple vals -> Tuple (map_subst vals)
  | Ptr (p, meta) ->
      let meta =
        match meta with
        | Thin -> Thin
        | Len len -> Len (Typed.subst subst_var len)
        | VTable ptr -> VTable (ptr_subst subst_var ptr)
      in
      Ptr (ptr_subst subst_var p, meta)
