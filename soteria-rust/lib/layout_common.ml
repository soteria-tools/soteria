open Charon
open Svalue
open Typed
open Typed.Infix

(** We use a custom type for the member offsets for layouts; this allows us to
    use a more efficient representation for arrays [T; N], that doesn't require
    N offsets. *)
module Fields_shape = struct
  (** Mirror's MiniRust and Charon's discriminator *)
  type discriminator =
    | Invalid
    | Known of Types.variant_id
    | Branch of {
        offset : T.sint Typed.t;
        tag_ty : Types.literal_type;
            [@printer Fmt.of_to_string Print.literal_type_to_string]
        children : (T.sint Typed.t * T.sint Typed.t * discriminator) list;
        fallback : discriminator;
      }
  [@@deriving show { with_path = false }]

  (** The [(offset * tag)] associated to a variant. If [None], the variant is
      either uninhabited or the untagged variant.

      Note tags are distinct from discriminants: a discriminant is user
      specified and is what [Rvalue.Discriminant] returns, whereas a tag is
      specific to variant layouts, and may be of smaller size than the
      discriminant, or not be encoded at all if it is the untagged variant of a
      niche-optimised enum. *)
  type tagger = (T.sint Typed.t * T.sint Typed.t) option

  let pp_tagger =
    Fmt.(
      option ~none:(any "none") (fun ft (from_, to_) ->
          Fmt.pf ft "[%a, %a]" Typed.ppa from_ Typed.ppa to_))

  type t =
    | Primitive  (** No fields present *)
    | Arbitrary of T.sint Typed.t Array.t
        (** Arbitrary field placement (structs, tuple, unions). *)
    | Enum of discriminator * (tagger * t) Array.t
        (** Enum fields: for each variant, a possible tag with [tagger], along
            with an array of field shapes for each variant (indexed by variant
            ID). The [discriminator] allows calculating the current variant.
            Using [offset_of] on this isn't valid; one must first retrieve the
            fields shape of the corresponding variant. *)
    | Array of { stride : T.sint Typed.t; is_ptr : bool }
        (** All fields are equally spaced (arrays, slices). We have a [is_ptr]
            flag, as we also use this layout for fat pointers. *)

  let rec pp ft = function
    | Primitive -> Fmt.string ft "()"
    | Arbitrary arr ->
        Fmt.pf ft "{%a}" Fmt.(braces @@ array ~sep:comma Typed.ppa) arr
    | Enum (discriminator, shapes) ->
        Fmt.pf ft "Enum (%a, %a)" pp_discriminator discriminator
          Fmt.(brackets @@ array ~sep:comma (pair ~sep:comma pp_tagger pp))
          shapes
    | Array { stride; _ } -> Fmt.pf ft "Array(%a)" Typed.ppa stride

  let offset_of f = function
    | Primitive -> L.failwith "This layout has no fields"
    | Enum _ -> L.failwith "Can't get fields of enum; use `shape_for_variant`"
    | Arbitrary arr -> arr.(f)
    | Array { stride; _ } -> BV.usizei f *!!@ stride

  let shape_for_variant variant = function
    | Enum (_, shapes) -> snd shapes.(Types.VariantId.to_int variant)
    | Arbitrary _ as fs when Types.VariantId.(equal_id zero variant) -> fs
    | s ->
        L.failwith "Shape %a has no variant %a" pp s Types.VariantId.pp_id
          variant

  let rec iter_discriminator (d : discriminator) f : unit =
    match d with
    | Invalid | Known _ -> ()
    | Branch { offset; tag_ty = _; children; fallback } ->
        f offset;
        List.iter
          (fun (from_, to_, child) ->
            f from_;
            f to_;
            iter_discriminator child f)
          children;
        iter_discriminator fallback f

  let iter_tagger (t : tagger) f : unit =
    match t with
    | None -> ()
    | Some (from_, to_) ->
        f from_;
        f to_

  let rec iter fields (f : 'a Typed.t -> unit) : unit =
    match fields with
    | Primitive -> ()
    | Arbitrary fields -> Array.iter (fun v -> f v) fields
    | Enum (discr, layouts) ->
        iter_discriminator discr f;
        Array.iter
          (fun (t, v) ->
            iter_tagger t f;
            iter v f)
          layouts
    | Array { stride; _ } -> f stride
end

(* TODO: size should be an [option], for unsized types *)
type t = {
  size : Typed.T.sint Typed.t;
  align : Typed.T.nonzero Typed.t;
  uninhabited : bool;
  fields : Fields_shape.t;
}
[@@deriving show]

(** [iter_values l f] applies [f] to every symbolic value in the layout [l]. *)
let iter_values ({ size; align; fields; uninhabited = _ } : t)
    (f : 'a. 'a Typed.t -> unit) : unit =
  f size;
  f align;
  Fields_shape.iter fields f

(** Whether the layout is made only of literal values, i.e. it does not depend
    on the path (through variables or type variables). *)
let only_has_literals (l : t) : bool =
  let exception Non_literal in
  try
    (iter_values l) (fun t ->
        if Typed.is_literal t then () else raise_notrace Non_literal);
    true
  with Non_literal -> false

(** Whether this layout is an aggregate layout, i.e. it has several fields,
    rather than representing just one atomic value. *)
let is_aggregate l = match l.fields with Primitive -> false | _ -> true
