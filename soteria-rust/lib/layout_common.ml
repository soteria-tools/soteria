open Charon
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
    | Arbitrary of Types.variant_id * T.sint Typed.t Array.t
        (** Arbitrary field placement (structs, unions...), with the variant
            (e.g. enums with a single inhabited variant) *)
    | Enum of discriminator * (tagger * t) Array.t
        (** Enum fields: for each variant, a possible tag with [tagger], along
            with an array of field shapes for each variant (indexed by variant
            ID). The [discriminator] allows calculating the current variant.
            Using [offset_of] on this isn't valid; one must first retrieve the
            fields shape of the corresponding variant. *)
    | Array of T.sint Typed.t
        (** All fields are equally spaced (arrays, slices) *)

  let rec pp ft = function
    | Primitive -> Fmt.string ft "()"
    | Arbitrary (var, arr) ->
        Fmt.pf ft "{%a: %a}" Types.VariantId.pp_id var
          Fmt.(braces @@ array ~sep:comma Typed.ppa)
          arr
    | Enum (discriminator, shapes) ->
        Fmt.pf ft "Enum (%a, %a)" pp_discriminator discriminator
          Fmt.(brackets @@ array ~sep:comma (pair ~sep:comma pp_tagger pp))
          shapes
    | Array stride -> Fmt.pf ft "Array(%a)" Typed.ppa stride

  let offset_of f = function
    | Primitive -> failwith "This layout has no fields"
    | Enum _ -> failwith "Can't get fields of enum; use `shape_for_variant`"
    | Arbitrary (_, arr) -> arr.(f)
    | Array stride -> BV.usizei f *!!@ stride

  let shape_for_variant variant = function
    | Enum (_, shapes) -> snd shapes.(Types.VariantId.to_int variant)
    | Arbitrary (v, _) as fs when Types.VariantId.equal_id v variant -> fs
    | s ->
        Fmt.failwith "Shape %a has no variant %a" pp s Types.VariantId.pp_id
          variant

  let rec iter_vars_discriminator (d : discriminator) f : unit =
    match d with
    | Invalid | Known _ -> ()
    | Branch { offset; tag_ty = _; children; fallback } ->
        iter_vars offset f;
        List.iter
          (fun (from_, to_, child) ->
            iter_vars from_ f;
            iter_vars to_ f;
            iter_vars_discriminator child f)
          children;
        iter_vars_discriminator fallback f

  let iter_vars_tagger (t : tagger) f : unit =
    match t with
    | None -> ()
    | Some (from_, to_) ->
        iter_vars from_ f;
        iter_vars to_ f

  let rec iter_vars fields f : unit =
    match fields with
    | Primitive -> ()
    | Arbitrary (_, fields) -> Array.iter (fun v -> Typed.iter_vars v f) fields
    | Enum (discr, layouts) ->
        iter_vars_discriminator discr f;
        Array.iter
          (fun (t, v) ->
            iter_vars_tagger t f;
            iter_vars v f)
          layouts
    | Array v -> Typed.iter_vars v f
end

(* TODO: size should be an [option], for unsized types *)
type t = {
  size : Typed.T.sint Typed.t;
  align : Typed.T.nonzero Typed.t;
  uninhabited : bool;
  fields : Fields_shape.t;
}
[@@deriving show]

let iter_vars ({ size; align; fields; uninhabited = _ } : t)
    (f : Svalue.Var.t * 'a Typed.ty -> unit) : unit =
  iter_vars size f;
  iter_vars align f;
  Fields_shape.iter_vars fields f
