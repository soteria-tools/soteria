open Charon
open Typed
open Typed.Infix

(** Layout of enum tags in memory. Note tags are distinct from discriminants: a
    discriminant is user specified and is what [Rvalue.Discriminant] returns,
    whereas a tag is specific to variant layouts, and may be of smaller size
    than the discriminant, or not be encoded at all if it is the untagged
    variant of a niche-optimised enum. *)
module Tag_layout = struct
  type encoding = Direct | Niche of Types.variant_id

  and t = {
    offset : T.sint Typed.t;
    ty : Types.literal_type; [@printer Charon_util.pp_literal_ty]
    encoding : encoding;
    tags : T.sint Typed.t option Array.t;
        [@printer
          Fmt.(
            brackets @@ array ~sep:comma (option ~none:(any "none") Typed.ppa))]
        (** The tag associated to each variant, indexed by variant ID. If
            [None], the variant is either uninhabited or the untagged variant.
        *)
  }
  [@@deriving show { with_path = false }]

  let iter_vars { offset; tags; encoding = _; ty = _ } f =
    Typed.iter_vars offset f;
    Array.iter (Option.iter (fun t -> Typed.iter_vars t f)) tags
end

(** We use a custom type for the member offsets for layouts; this allows us to
    use a more efficient representation for arrays [T; N], that doesn't require
    N offsets. *)
module Fields_shape = struct
  type t =
    | Primitive  (** No fields present *)
    | Arbitrary of Types.variant_id * T.sint Typed.t Array.t
        (** Arbitrary field placement (structs, unions...), with the variant
            (e.g. enums with a single inhabited variant) *)
    | Enum of Tag_layout.t * t Array.t
        (** Enum fields: encodes a tag, and an array of field shapes for each
            variant (indexed by variant ID). Using [offset_of] on this isn't
            valid; one must first retrieve the fields shape of the corresponding
            variant. *)
    | Array of T.sint Typed.t
        (** All fields are equally spaced (arrays, slices) *)

  let rec pp ft = function
    | Primitive -> Fmt.string ft "()"
    | Arbitrary (var, arr) ->
        Fmt.pf ft "{%a: %a}" Types.VariantId.pp_id var
          Fmt.(braces @@ array ~sep:comma Typed.ppa)
          arr
    | Enum (tag_layout, shapes) ->
        Fmt.pf ft "Enum (%a, %a)" Tag_layout.pp tag_layout
          Fmt.(brackets @@ array ~sep:comma pp)
          shapes
    | Array stride -> Fmt.pf ft "Array(%a)" Typed.ppa stride

  let offset_of f = function
    | Primitive -> failwith "This layout has no fields"
    | Enum _ -> failwith "Can't get fields of enum; use `shape_for_variant`"
    | Arbitrary (_, arr) -> arr.(f)
    | Array stride -> BV.usizei f *!!@ stride

  let shape_for_variant variant = function
    | Enum (_, shapes) -> shapes.(Types.VariantId.to_int variant)
    | Arbitrary (v, _) as fs when Types.VariantId.equal_id v variant -> fs
    | s ->
        Fmt.failwith "Shape %a has no variant %a" pp s Types.VariantId.pp_id
          variant

  let rec iter_vars fields f : unit =
    match fields with
    | Primitive -> ()
    | Arbitrary (_, fields) -> Array.iter (fun v -> Typed.iter_vars v f) fields
    | Enum (tl, layouts) ->
        Array.iter (fun v -> iter_vars v f) layouts;
        Tag_layout.iter_vars tl f
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
