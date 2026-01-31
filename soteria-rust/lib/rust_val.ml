module T = Typed.T
module BV = Typed.BitVec

type 'ptr meta = Thin | Len of T.sint Typed.t | VTable of 'ptr
type 'ptr full_ptr = 'ptr * 'ptr meta

type 'ptr t =
  | Int of T.sint Typed.t
  | Float of T.sfloat Typed.t
  | Ptr of 'ptr full_ptr
      (** pointer, parametric to enable Ruxt, with optional meta *)
  | Enum of T.sint Typed.t * 'ptr t list  (** discriminant * values *)
  | Tuple of 'ptr t list  (** contains ordered values *)
  | Union of ('ptr t * T.sint Typed.t) list
      (** list of blocks in the union, with their offset *)
  | PolyVal of Charon.Types.type_var_id
      (** The opaque value of a type variable, identified by (type variable
          index, unique identifier). *)

type 'ptr rust_val = 'ptr t

(** [is_empty v] is true if the value is "empty"; ie. it doesn't contain any
    [Base] or [Ptr] value. We also consider [Enum] to be non-empty, because of
    the discriminant, though this *may* be wrong if it's a ZST.

    Used in unsizing, to find the field with the pointer to modify. *)
let rec is_empty = function
  | Int _ | Float _ | Ptr _ | Enum (_, _) | PolyVal _ -> false
  | Tuple vals -> List.for_all is_empty vals
  (* I'm not sure about this one *)
  | Union _ -> false

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
  | Int v -> Typed.ppa fmt v
  | Float v -> Typed.ppa fmt v
  | Ptr ptr -> Fmt.pf fmt "Ptr%a" (pp_full_ptr pp_ptr) ptr
  | Enum (disc, vals) ->
      Fmt.pf fmt "Enum(%a: %a)" Typed.ppa disc
        (Fmt.list ~sep:(Fmt.any ", ") pp_rust_val)
        vals
  | Tuple vals ->
      Fmt.pf fmt "(%a)" (Fmt.list ~sep:(Fmt.any ", ") pp_rust_val) vals
  | Union vs ->
      let pp_block ft (v, ofs) =
        Fmt.pf ft "(%a: %a)" Typed.ppa ofs pp_rust_val v
      in
      Fmt.pf fmt "Union(%a)" (Fmt.list ~sep:(Fmt.any ", ") pp_block) vs
  | PolyVal tid -> Fmt.pf fmt "PolyVal(%a)" Charon.Types.pp_type_var_id tid

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
  | Int v -> (make @@ Typed.cast_i Usize v, Thin)
  | v ->
      Fmt.failwith
        "Unexpected rust_val kind, expected a pointer or base, got: %a"
        ppa_rust_val v

let as_base_f ty = function
  | Float v -> Typed.cast_f ty v
  | v ->
      Fmt.failwith "Unexpected rust_val kind, expected a base value got: %a"
        ppa_rust_val v

let as_base ty = function
  | Int v -> Typed.cast_lit ty v
  | v ->
      Fmt.failwith "Unexpected rust_val kind, expected a base value got: %a"
        ppa_rust_val v

let as_base_i ty = as_base (TUInt ty)

let as_tuple = function
  | Tuple vals -> vals
  | v ->
      Fmt.failwith "Unexpected rust_val kind, expected a tuple, got: %a"
        ppa_rust_val v

let size_of =
  let open Rustsymex.Result in
  function
  | Int v -> ok (BV.usizei (Typed.size_of_int v / 8))
  | Float f ->
      ok (BV.usizei (Svalue.FloatPrecision.size (Typed.Float.fp_of f) / 8))
  | Ptr (_, Thin) -> ok (BV.usizei (Crate.pointer_size ()))
  | Ptr (_, (Len _ | VTable _)) -> ok (BV.usizei (Crate.pointer_size () * 2))
  | PolyVal tid -> Layout.size_of (TVar (Free tid))
  (* We can't know the size of a union/tuple/enum, because of e.g. niches, or
     padding *)
  | Union _ | Enum _ | Tuple _ ->
      failwith "Impossible to get size of Enum/Tuple rust_val"

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
