open Typed

type ('v, 'ptr) meta_raw = Thin | Len of 'v | VTable of 'ptr
type 'ptr meta = (T.sint Typed.t, 'ptr) meta_raw
type 'ptr meta_syn = (Expr.t, 'ptr) meta_raw
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

let pp_rust_val = pp

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

let ppa_syn ft t = pp_syn (Fmt.any "?") ft t
let ppa ft rv = pp (Fmt.any "?") ft rv
let unit_ = Tuple []

let meta_exprs_syn ptr_exprs_syn : 'ptr meta_syn -> Expr.t list = function
  | Thin -> []
  | Len v -> [ v ]
  | VTable p -> ptr_exprs_syn p

let rec exprs_syn ptr_exprs_syn x =
  let exprs_syn = exprs_syn ptr_exprs_syn in
  match x with
  | Int v | Float v -> [ v ]
  | Ptr (p, meta) -> ptr_exprs_syn p @ meta_exprs_syn ptr_exprs_syn meta
  | Enum (disc, vals) -> disc :: List.concat_map exprs_syn vals
  | Tuple vals -> List.concat_map exprs_syn vals
  | Union vs -> List.concat_map (fun (v, ofs) -> exprs_syn v @ [ ofs ]) vs
  | PolyVal _ -> []

let rec to_syn ptr_to_syn x =
  let meta_to_syn (m : 'ptr meta) =
    match m with
    | Thin -> Thin
    | Len v -> Len (Expr.of_value v)
    | VTable p -> VTable (ptr_to_syn p)
  in
  let full_ptr_to_syn (ptr, meta) = (ptr_to_syn ptr, meta_to_syn meta) in
  let to_syn = to_syn ptr_to_syn in
  match x with
  | Int v -> Int (Expr.of_value v)
  | Float v -> Float (Expr.of_value v)
  | Enum (disc, vals) ->
      let disc = Expr.of_value disc in
      let vals = List.map to_syn vals in
      Enum (disc, vals)
  | Tuple vals -> Tuple (List.map to_syn vals)
  | Union vs ->
      let vs = List.map (fun (v, ofs) -> (to_syn v, Expr.of_value ofs)) vs in
      Union vs
  | Ptr p -> Ptr (full_ptr_to_syn p)
  | PolyVal v -> PolyVal v

module Learn_eq (Symex : Soteria.Symex.Base with module Value = Rustsymex.Value) =
struct
  let learn_eq_meta learn_eq_ptr s t =
    let open Symex.Consumer in
    match (s, t) with
    | Thin, Thin -> ok ()
    | Len s, Len v -> learn_eq s v
    | VTable ps, VTable p -> learn_eq_ptr ps p
    | _ -> lfail Typed.v_false

  let rec learn_eq learn_eq_ptr syn t =
    let rec_call = learn_eq learn_eq_ptr in
    let open Symex.Consumer in
    let open Syntax in
    let learn_list ~learn ls l =
      let* combined =
        List.combine_or ~ok ~err:(fun () -> lfail Typed.v_false) ls l
      in
      iter_list ~f:(fun (a, b) -> learn a b) combined
    in
    match (syn, t) with
    | Int s, Int v -> learn_eq s v
    | Float s, Float v -> learn_eq s v
    | Ptr (ps, meta_s), Ptr (p, meta) ->
        let* () = learn_eq_ptr ps p in
        learn_eq_meta learn_eq_ptr meta_s meta
    | Enum (disc_s, vals_s), Enum (disc, vals) ->
        let* () = learn_eq disc_s disc in
        learn_list ~learn:rec_call vals_s vals
    | Tuple vals_s, Tuple vals -> learn_list ~learn:rec_call vals_s vals
    | Union vs1, Union vs2 ->
        learn_list
          ~learn:(fun (v1, ofs1) (v2, ofs2) ->
            let* () = rec_call v1 v2 in
            learn_eq ofs1 ofs2)
          vs1 vs2
    | PolyVal _, PolyVal _ ->
        lift @@ Symex.give_up "Learning equality of polymorphic values"
    | _ -> Fmt.failwith "Mismatching rust_val kinds: %a vs %a" ppa_syn syn ppa t
end

(** [is_empty v] is true if the value is "empty"; ie. it doesn't contain any
    [Base] or [Ptr] value. We also consider [Enum] to be non-empty, because of
    the discriminant, though this *may* be wrong if it's a ZST.

    Used in unsizing, to find the field with the pointer to modify. *)
let rec is_empty = function
  | Int _ | Float _ | Ptr _ | Enum (_, _) | PolyVal _ -> false
  | Tuple vals -> List.for_all is_empty vals
  (* I'm not sure about this one *)
  | Union _ -> false

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

let rec subst subst_ptr subst_val rv =
  let subst = subst subst_ptr subst_val in
  let subst_expr = Expr.subst subst_val in
  match rv with
  | Int v -> Int (subst_expr v)
  | Float v -> Float (subst_expr v)
  | PolyVal i -> PolyVal i
  | Union vs -> Union (List.map (fun (v, ofs) -> (subst v, subst_expr ofs)) vs)
  | Enum (disc, vals) -> Enum (subst_expr disc, List.map subst vals)
  | Tuple vals -> Tuple (List.map subst vals)
  | Ptr (p, meta) ->
      let meta =
        match meta with
        | Thin -> Thin
        | Len len -> Len (subst_expr len)
        | VTable ptr -> VTable (subst_ptr subst_val ptr)
      in
      Ptr (subst_ptr subst_val p, meta)

let mk_enum ~ty variant fields =
  let open Charon in
  let open Common.Charon_util in
  let variant =
    ty_as_adt ty
    |> Crate.as_enum
    |> List.find (fun (v : Types.variant) -> v.variant_name = variant)
  in
  assert (List.compare_lengths variant.fields fields = 0);
  let discr = BV.of_literal variant.discriminant in
  Enum (discr, fields)
