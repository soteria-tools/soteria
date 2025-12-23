open Charon
open Typed.Infix
open Typed.Syntax
module BV = Typed.BitVec
open Rustsymex
open Rustsymex.Result
open Rustsymex.Syntax
open Rust_val
open Charon_util

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
    | Array of T.sint Typed.t
        (** All fields are equally spaced (arrays, slices) *)
    | Unsized
        (** An unsize object, that cannot be read or written; this is a
            placeholder value. *)

  let pp ft = function
    | Primitive -> Fmt.string ft "()"
    | Arbitrary (var, arr) ->
        Fmt.pf ft "{%a: %a}" Types.VariantId.pp_id var
          Fmt.(braces @@ array ~sep:comma Typed.ppa)
          arr
    | Array stride -> Fmt.pf ft "Array(%a)" Typed.ppa stride
    | Unsized -> Fmt.pf ft "Unsized"

  let offset_of f = function
    | Arbitrary (_, arr) -> arr.(f)
    | Array stride -> BV.usizei f *!!@ stride
    | Primitive | Unsized -> failwith "This layout has no fields"
end

module Variant_layout = struct
  type t = { uninhabited : bool; fields : Fields_shape.t }
  [@@deriving show { with_path = false }]
end

type t = {
  size : T.sint Typed.t option;
  align : T.nonzero Typed.t;
  uninhabited : bool;
  tag_layout : Tag_layout.t option;
  variants : Variant_layout.t array;
}
[@@deriving show { with_path = false }]

module Session = struct
  type ty_key = Types.ty
  type variant_key = Types.type_decl_id * Types.variant

  (* TODO: allow different caches for different crates *)
  (* FIXME: inter-test mutability *)

  (** Cache of (type or variant) -> layout *)
  let layout_cache : (Types.ty, t) Hashtbl.t = Hashtbl.create 128

  let get_or_compute_cached_layout ty f =
    match Hashtbl.find_opt layout_cache ty with
    | Some layout -> ok layout
    | None ->
        let++ layout = f () in
        Hashtbl.add layout_cache ty layout;
        layout
end

include Layout_common

(* TODO: this is not really accurate, but good enough for now.
   See https://doc.rust-lang.org/reference/type-layout.html#r-layout.primitive.align *)
let align_of_literal_ty ty = size_of_literal_ty ty
let size_of_literal_ty ty = size_of_literal_ty ty
let empty_generics = TypesUtils.empty_generic_args

type meta_kind = LenKind | VTableKind | NoneKind

let rec dst_kind : Types.ty -> meta_kind = function
  | TAdt { id = TBuiltin TStr; _ } | TAdt { id = TBuiltin TSlice; _ } -> LenKind
  | TDynTrait _ -> VTableKind
  | TAdt { id = TAdtId id; _ } when Crate.is_struct id -> (
      match List.last_opt (Crate.as_struct id) with
      | None -> NoneKind
      | Some last -> dst_kind Types.(last.field_ty))
  | _ -> NoneKind

(** If this is a DST type with a slice tail, return the type of the slice's
    element. *)
let rec dst_slice_ty : Types.ty -> Types.ty option = function
  | TAdt { id = TBuiltin TStr; _ } -> Some (TLiteral (TUInt U8))
  | TAdt { id = TBuiltin TSlice; generics = { types = [ sub_ty ]; _ } } ->
      Some sub_ty
  | TAdt { id = TAdtId id; _ } when Crate.is_struct id -> (
      match List.last_opt (Crate.as_struct id) with
      | None -> None
      | Some last -> dst_slice_ty Types.(last.field_ty))
  | _ -> None

(** If this is a dynamically sized type (requiring a fat pointer) *)
let is_dst ty = dst_kind ty <> NoneKind

let[@inline] size_to_fit ~size ~align =
  Typed.ite
    (size %@ align ==@ Usize.(0s))
    size
    (size +!@ align -!@ (size %@ align))

let mk ?size ~align ?(uninhabited = true) ?tag_layout
    ?(variants =
      [| Variant_layout.{ uninhabited = true; fields = Primitive } |]) () =
  { size; align; uninhabited; tag_layout; variants }

let mk_concrete ~size ~align =
  mk ~size:(BV.usizei size) ~align:(BV.usizeinz align)

let not_impl_layout msg ty =
  Fmt.kstr not_impl "Can't compute layout: %s %a" msg pp_ty ty

let layout_warning msg ty =
  L.debug (fun m -> m "⚠️ Layout: %s (%a)" msg pp_ty ty)

let rec layout_of (ty : Types.ty) : (t, 'e, 'f) Rustsymex.Result.t =
  Session.get_or_compute_cached_layout ty @@ fun () ->
  match ty with
  (* Literals *)
  | TLiteral ty ->
      let size = size_of_literal_ty ty in
      let align = align_of_literal_ty ty in
      ok (mk_concrete ~size ~align ())
  (* Fat pointers *)
  | TAdt { id = TBuiltin TBox; generics = { types = [ sub_ty ]; _ } }
  | TRef (_, sub_ty, _)
  | TRawPtr (sub_ty, _)
    when is_dst sub_ty ->
      let ptr_size = Crate.pointer_size () in
      ok
        (mk_concrete ~size:(ptr_size * 2) ~align:ptr_size
           ~variants:
             [| { uninhabited = true; fields = Array (BV.usizei ptr_size) } |]
           ())
  (* Refs, pointers, boxes, function pointers *)
  | TAdt { id = TBuiltin TBox; _ } | TRef (_, _, _) | TRawPtr (_, _) | TFnPtr _
    ->
      let ptr_size = Crate.pointer_size () in
      ok (mk_concrete ~size:ptr_size ~align:ptr_size ())
  (* Dynamically sized types. *)
  | TAdt { id = TBuiltin (TStr as ty); generics }
  | TAdt { id = TBuiltin (TSlice as ty); generics } ->
      let sub_ty =
        if ty = TSlice then List.hd generics.types else TLiteral (TUInt U8)
      in
      let++ { size; align; uninhabited; _ } = layout_of sub_ty in
      let size = Option.get ~msg:"Can't have a slice of a DST" size in
      mk ?size:None ~align ~uninhabited
        ~variants:[| { uninhabited; fields = Array size } |]
        ()
  (* Same as above, but here we have even less information ! *)
  | TDynTrait _ ->
      ok
        (mk ?size:None
           ~align:Usize.(1s)
           ~variants:[| { uninhabited = true; fields = Unsized } |]
           ())
  (* Tuples *)
  | TAdt { id = TTuple; generics = { types; _ } } ->
      compute_arbitrary_layout ty types
  (* Custom ADTs (struct, enum, etc.) *)
  | TAdt { id = TAdtId id; _ } -> (
      let adt = Crate.get_adt id in
      match (adt.layout, adt.kind) with
      | Some layout, _ -> translate_layout ty layout
      | _, (Struct fields | Union fields) ->
          compute_arbitrary_layout ty (field_tys fields)
      | _, Enum variants -> compute_enum_layout ty variants
      | _, (Opaque | TDeclError _ | Alias _) -> not_impl_layout "unexpected" ty)
  (* Arrays *)
  | TAdt { id = TBuiltin TArray; generics } ->
      let len = List.hd generics.const_generics in
      let len = BV.of_const_generic len in
      let subty = List.hd generics.types in
      let** { size; align; uninhabited; _ } = layout_of subty in
      let size = Option.get ~msg:"Can't have an array of unsized types" size in
      let++ () =
        (* We calculate the max array size for a 32bit architecture, like Miri does. *)
        assert_or_error
          (Typed.or_lazy
             (size ==@ Usize.(0s))
             (fun () -> len <=@ BV.usize Z.(one lsl 31) /@ Typed.cast size))
          (`InvalidLayout ty)
      in
      mk ~size:(len *!!@ size) ~align ~uninhabited
        ~variants:[| { uninhabited; fields = Array size } |]
        ()
  (* Never -- zero sized type *)
  | TNever -> ok (mk_concrete ~size:0 ~align:1 ~uninhabited:false ())
  (* Function definitions -- zero sized type *)
  | TFnDef _ -> ok (mk_concrete ~size:0 ~align:1 ())
  (* Others (unhandled for now) *)
  | TPtrMetadata _ -> not_impl_layout "pointer metadata" ty
  | TVar _ -> not_impl_layout "type variable" ty
  | TError _ -> not_impl_layout "error" ty
  | TTraitType (tref, ty_name) ->
      let** resolved = resolve_trait_ty tref ty_name in
      layout_of resolved

and translate_layout ty (layout : Types.layout) =
  let size = Option.map BV.usizei layout.size in
  let align = compute_align ty layout.align in
  let uninhabited = layout.uninhabited in
  let tag_layout =
    Option.map
      (fun (discr_layout : Types.discriminant_layout) : Tag_layout.t ->
        let tags =
          Monad.ListM.map layout.variant_layouts (fun v ->
              Option.map BV.of_scalar v.tag)
        in
        let tags = Array.of_list tags in
        let ty = lit_of_int_ty discr_layout.tag_ty in
        let offset = BV.usizei discr_layout.offset in
        let encoding : Tag_layout.encoding =
          match discr_layout.encoding with
          | Niche v -> Niche v
          | Direct -> Direct
        in
        { offset; ty; tags; encoding })
      layout.discriminant_layout
  in
  let variants =
    List.mapi
      (fun i (v : Types.variant_layout) : Variant_layout.t ->
        let ofs = Array.of_list (List.map BV.usizei v.field_offsets) in
        {
          uninhabited = true;
          fields = Arbitrary (Types.VariantId.of_int i, ofs);
        })
      layout.variant_layouts
    |> Array.of_list
  in
  ok { size; align; uninhabited; tag_layout; variants }

and compute_align ty align =
  match align with
  | Some a -> BV.usizeinz a
  | None ->
      layout_warning "Inferred align=1" ty;
      BV.usizeinz 1

and compute_arbitrary_layout ?fst_size ?fst_align
    ?(variant = Types.VariantId.zero) ty members =
  layout_warning "Computed an arbitrary layout" ty;
  (* Calculates the layout for an arbitrary layout with some gives types.
     If any field is unsized, the whole layout becomes unsized; however the other fields
     are still processed to figure out uninhabitedness and alignment. *)
  let rec aux offsets size align uninhabited = function
    | [] ->
        let++ size =
          match size with
          | None -> ok None
          | Some (size, overflowed) ->
              let++ () =
                assert_or_error (Typed.not overflowed) (`InvalidLayout ty)
              in
              Some (size_to_fit ~size ~align)
        in
        let fields : Fields_shape.t =
          match offsets with
          | None -> Unsized
          | Some offsets -> Arbitrary (variant, Array.of_list offsets)
        in
        mk ?size ~align ~variants:[| { uninhabited; fields } |] ()
    | ty :: rest ->
        let** layout = layout_of ty in
        let offset, new_size =
          match (offsets, size, layout.size) with
          | None, _, _ | _, None, _ | _, _, None -> (None, None)
          | Some offsets, Some (size, overflows), Some f_size ->
              let offset = size_to_fit ~size ~align:layout.align in
              let size, ovf = size +$?@ f_size in
              let new_size = (size, overflows ||@ ovf) in
              (Some (offset :: offsets), Some new_size)
        in
        let new_align = BV.max ~signed:false align layout.align in
        aux offset new_size new_align (uninhabited || layout.uninhabited) rest
  in
  let fst_size =
    Some (Option.value fst_size ~default:(BV.usizei 0), Typed.v_false)
  in
  let fst_align = Option.value fst_align ~default:(BV.usizeinz 1) in
  aux (Some []) fst_size fst_align false members

and compute_enum_layout ty (variants : Types.variant list) =
  layout_warning "Computed an enum layout" ty;
  (* best effort: we assume direct encoding *)
  let tags =
    Monad.ListM.map variants (fun v -> Some (BV.of_literal v.discriminant))
    |> Array.of_list
  in
  let tag_layout : Tag_layout.t =
    { offset = Usize.(0s); ty = TInt I32; tags; encoding = Direct }
  in
  let** tag = layout_of (TLiteral tag_layout.ty) in
  let tag_size = Option.get ~msg:"Literals are always sized" tag.size in
  let++ size, align, variants, uninhabited =
    Result.fold_list variants
      ~init:(Usize.(0s), Usize.(1s), [], true)
      ~f:(fun (size, align, variants, uninhabited) v ->
        let++ v =
          compute_arbitrary_layout ty ~fst_size:tag_size ~fst_align:tag.align
            (field_tys v.fields)
        in
        ( max (Option.get ~msg:"Enums variants must be sized" v.size) size,
          max v.align align,
          v.variants.(0) :: variants,
          v.uninhabited && uninhabited ))
  in
  let variants = List.rev variants in
  mk ~size ~align ~uninhabited ~variants:(Array.of_list variants) ()

and resolve_trait_ty (tref : Types.trait_ref) ty_name =
  match tref.kind with
  | TraitImpl { id; _ } -> (
      let impl = Crate.get_trait_impl id in
      match List.find_opt (fun (n, _) -> ty_name = n) impl.types with
      | Some (_, ty) -> ok ty.binder_value.value
      | None ->
          let msg =
            Fmt.str "missing type '%s' in impl %a" ty_name Crate.pp_name
              impl.item_meta.name
          in
          not_impl_layout msg (TTraitType (tref, ty_name)))
  | _ -> not_impl_layout "trait type" (TTraitType (tref, ty_name))

let size_of ty =
  let** { size; _ } = layout_of ty in
  let+ size =
    of_opt_not_impl "Tried getting the size of an unsized type" size
  in
  Soteria.Symex.Compo_res.Ok (Typed.cast size :> [> T.sint ] Typed.t)

let align_of ty =
  let++ { align; _ } = layout_of ty in
  (Typed.cast align :> [> T.nonzero ] Typed.t)

let min_value_z : Types.literal_type -> Z.t = function
  | TUInt _ -> Z.zero
  | TInt Isize -> Z.neg (Z.shift_left Z.one ((8 * Crate.pointer_size ()) - 1))
  | TInt I128 -> Z.neg (Z.shift_left Z.one 127)
  | TInt I64 -> Z.neg (Z.shift_left Z.one 63)
  | TInt I32 -> Z.neg (Z.shift_left Z.one 31)
  | TInt I16 -> Z.neg (Z.shift_left Z.one 15)
  | TInt I8 -> Z.neg (Z.shift_left Z.one 7)
  | _ -> failwith "Invalid integer type for min_value_z"

let max_value_z : Types.literal_type -> Z.t = function
  | TUInt U128 -> Z.pred (Z.shift_left Z.one 128)
  | TUInt U64 -> Z.pred (Z.shift_left Z.one 64)
  | TUInt U32 -> Z.pred (Z.shift_left Z.one 32)
  | TUInt U16 -> Z.pred (Z.shift_left Z.one 16)
  | TUInt U8 -> Z.pred (Z.shift_left Z.one 8)
  | TUInt Usize -> Z.pred (Z.shift_left Z.one (8 * Crate.pointer_size ()))
  | TInt I128 -> Z.pred (Z.shift_left Z.one 127)
  | TInt I64 -> Z.pred (Z.shift_left Z.one 63)
  | TInt I32 -> Z.pred (Z.shift_left Z.one 31)
  | TInt I16 -> Z.pred (Z.shift_left Z.one 15)
  | TInt I8 -> Z.pred (Z.shift_left Z.one 7)
  | TInt Isize -> Z.pred (Z.shift_left Z.one ((8 * Crate.pointer_size ()) - 1))
  | _ -> failwith "Invalid integer type for max_value_z"

let constraints :
    Types.literal_type -> [< T.cval ] Typed.t -> T.sbool Typed.t list = function
  | TInt _ | TUInt _ | TFloat (F16 | F32 | F64 | F128) -> fun _ -> []
  | TBool ->
      fun x ->
        let x = Typed.cast_lit TBool x in
        [ U8.(0s) <=@ x; (x <=@ U8.(1s)) ]
  | TChar ->
      (* A char is a ‘Unicode scalar value’, which is any ‘Unicode code point’ other than
       a surrogate code point. This has a fixed numerical definition: code points are in
       the range 0 to 0x10FFFF, inclusive. Surrogate code points, used by UTF-16, are in
       the range 0xD800 to 0xDFFF.
       https://doc.rust-lang.org/std/primitive.char.html *)
      let codepoint_min = U32.(0s) in
      let codepoint_max = U32.(0x10FFFFs) in
      let surrogate_min = U32.(0xD800s) in
      let surrogate_max = U32.(0xDFFFs) in
      fun x ->
        let x = Typed.cast_lit TChar x in
        [
          codepoint_min <=@ x;
          x <=@ codepoint_max;
          Typed.not (surrogate_min <=@ x &&@ (x <=@ surrogate_max));
        ]

let nondet_literal_ty (ty : Types.literal_type) : T.cval Typed.t Rustsymex.t =
  let open Rustsymex.Syntax in
  let rty =
    match ty with
    | TInt _ | TUInt _ | TBool | TChar -> Typed.t_int (size_of_literal_ty ty * 8)
    | TFloat F16 -> Typed.t_f16
    | TFloat F32 -> Typed.t_f32
    | TFloat F64 -> Typed.t_f64
    | TFloat F128 -> Typed.t_f128
  in
  let constrs = constraints ty in
  let* v = Rustsymex.nondet rty in
  let+ () = Rustsymex.assume (constrs v) in
  v

(** [nondet ~extern ~init ty] returns a nondeterministic value for [ty], along
    with some "state". It receives a function [extern] to get an optional
    external function that computes the arbitrary value; it tries using it, and
    otherwise guesses the valid values. [init] is the initial "state", that is
    modified and returned by [extern]. *)
let rec nondet : Types.ty -> ('a rust_val, 'e, 'f) Result.t =
  let open Soteria.Symex.Compo_res in
  function
  | TLiteral (TFloat _ as lit) ->
      let+ f = nondet_literal_ty lit in
      Ok (Float (Typed.cast f))
  | TLiteral lit ->
      let+ i = nondet_literal_ty lit in
      Ok (Int (Typed.cast i))
  | TAdt { id = TTuple; generics = { types; _ } } ->
      let++ fields = nondets types in
      Tuple fields
  | TAdt
      {
        id = TBuiltin TArray;
        generics = { const_generics = [ len ]; types = [ ty ]; _ };
      } ->
      let size = Charon_util.int_of_const_generic len in
      let++ fields = nondets @@ List.init size (fun _ -> ty) in
      Tuple fields
  | TAdt { id = TAdtId t_id; _ } as ty -> (
      let type_decl = Crate.get_adt t_id in
      match type_decl.kind with
      | Enum variants -> (
          let** layout = layout_of ty in
          match layout.tag_layout with
          | None -> not_impl "TODO: nondet enum with no tag layout"
          | Some tag_layout -> (
              let* d = nondet_literal_ty tag_layout.ty in
              let* res =
                match_on variants ~constr:(fun v ->
                    BV.of_literal v.discriminant ==@ d)
              in
              match (res, tag_layout.encoding) with
              | Some variant, _ ->
                  let discr = BV.of_literal variant.discriminant in
                  let++ fields =
                    nondets @@ Charon_util.field_tys variant.fields
                  in
                  Enum (discr, fields)
              | None, Direct -> vanish ()
              | None, Niche untagged ->
                  let variant = Types.VariantId.nth variants untagged in
                  let discr = BV.of_literal variant.discriminant in
                  let++ fields =
                    nondets @@ Charon_util.field_tys variant.fields
                  in
                  Enum (discr, fields)))
      | Struct fields ->
          let++ fields = nondets @@ Charon_util.field_tys fields in
          Tuple fields
      | ty ->
          Fmt.kstr Rustsymex.not_impl "nondet: unsupported type %a"
            Types.pp_type_decl_kind ty)
  | ty -> Fmt.kstr Rustsymex.not_impl "nondet: unsupported type %a" pp_ty ty

and nondets tys =
  let++ vs =
    Result.fold_list tys ~init:[] ~f:(fun fields ty ->
        let++ f = nondet ty in
        f :: fields)
  in
  List.rev vs

let rec is_inhabited : Types.ty -> bool = function
  | TNever -> false
  | TAdt { id = TAdtId id; _ } -> (
      let adt = Crate.get_adt id in
      match adt.kind with
      | Struct fs -> List.for_all is_inhabited @@ Charon_util.field_tys fs
      | Union fs -> List.exists is_inhabited @@ Charon_util.field_tys fs
      | Enum [] -> false
      | Enum vars ->
          List.exists
            (fun (v : Types.variant) ->
              List.for_all is_inhabited @@ Charon_util.field_tys v.fields)
            vars
      | _ -> true)
  | TAdt { id = TTuple; generics = { types; _ } } ->
      List.for_all is_inhabited types
  | _ -> true

(** Returns the given type as it's unique representant if it's a ZST; otherwise
    [None].

    FIXME: giltho: this plays awfully with polymorphism. *)
let rec as_zst : Types.ty -> 'a rust_val option =
  let as_zsts tys = Monad.OptionM.all as_zst tys in
  function
  | TNever -> Some (Tuple [])
  | TAdt { id = TBuiltin TArray; generics = { const_generics = [ len ]; _ } }
    when int_of_const_generic len = 0 ->
      Some (Tuple [])
  | TAdt { id = TAdtId id; _ } -> (
      let adt = Crate.get_adt id in
      match adt.kind with
      | Struct fs ->
          as_zsts @@ Charon_util.field_tys fs |> Option.map (fun fs -> Tuple fs)
      | Union _ -> None
      | Enum [] -> None (* an empty enum is uninhabited *)
      | Enum [ { fields; discriminant; _ } ] ->
          as_zsts @@ Charon_util.field_tys fields
          |> Option.map (fun fs -> Enum (BV.of_literal discriminant, fs))
      | Enum _ -> None
      | _ -> None)
  | TAdt { id = TTuple; generics = { types; _ } } ->
      as_zsts types |> Option.map (fun fs -> Tuple fs)
  | TFnDef binder -> Some (ConstFn binder.binder_value)
  | _ -> None

(** Apply the compiler-attribute to the given value *)
let apply_attribute v attr =
  let open Rustsymex.Syntax in
  match (v, attr) with
  | ( Int v,
      Meta.AttrUnknown
        { path = "rustc_layout_scalar_valid_range_start"; args = Some min } ) ->
      let min = Z.of_string min in
      let bits = Typed.size_of_int v in
      if%sat v >=@ BV.mk bits min then Result.ok ()
      else Result.error (`StdErr "rustc_layout_scalar_valid_range_start")
  | ( Int v,
      AttrUnknown
        { path = "rustc_layout_scalar_valid_range_end"; args = Some max_s } ) ->
      let max = Z.of_string max_s in
      let bits = Typed.size_of_int v in
      if%sat v <=@ BV.mk bits max then Result.ok ()
      else Result.error (`StdErr "rustc_layout_scalar_valid_range_end")
  | _ -> Result.ok ()

let apply_attributes v attributes =
  Result.fold_list attributes ~f:(fun () -> apply_attribute v) ~init:()

let rec is_unsafe_cell : Types.ty -> bool = function
  | TAdt { id = TAdtId adt_id; _ } -> (
      let adt = Crate.get_adt adt_id in
      if adt.item_meta.lang_item = Some "unsafe_cell" then true
      else
        match adt.kind with
        | Struct fs | Union fs -> List.exists is_unsafe_cell (field_tys fs)
        | Enum vs ->
            Iter.exists is_unsafe_cell
            @@ Iter.flat_map_l (fun (v : Types.variant) -> field_tys v.fields)
            @@ Iter.of_list vs
        | _ -> false)
  | TAdt { id = TBuiltin (TArray | TSlice); generics = { types = [ ty ]; _ } }
    ->
      is_unsafe_cell ty
  | TAdt { id = TTuple; generics = { types; _ } } ->
      List.exists is_unsafe_cell types
  | _ -> false

(** Traverses the given type and rust value, and returns all findable references
    with their type (ignores pointers, except if [include_ptrs] is true). This
    is needed e.g. when needing to get the pointers along with the size of their
    pointee, in particular in nested cases. *)
let rec ref_tys_in ?(include_ptrs = false) (v : 'a rust_val) (ty : Types.ty) :
    ('a full_ptr * Types.ty) list =
  let f = ref_tys_in ~include_ptrs in
  match (v, ty) with
  | Ptr ptr, (TAdt { id = TBuiltin TBox; _ } | TRef _) ->
      [ (ptr, get_pointee ty) ]
  | Ptr ptr, TRawPtr _ when include_ptrs -> [ (ptr, get_pointee ty) ]
  | (Int _ | Float _), _ -> []
  | Tuple vs, TAdt { id = TAdtId adt_id; _ } ->
      let fields = Crate.as_struct adt_id in
      List.concat_map2 f vs (field_tys fields)
  | ( Tuple vs,
      TAdt { id = TBuiltin (TArray | TSlice); generics = { types = [ ty ]; _ } }
    ) ->
      List.concat_map (fun v -> f v ty) vs
  | Tuple vs, TAdt { id = TTuple; generics = { types; _ } } ->
      List.concat_map2 f vs types
  | Enum (d, vs), TAdt { id = TAdtId adt_id; _ } -> (
      match BV.to_z d with
      | Some d -> (
          let variants = Crate.as_enum adt_id in
          let v =
            List.find_opt
              (fun (v : Types.variant) ->
                Z.equal d (z_of_literal v.discriminant))
              variants
          in
          match v with
          | Some v -> List.concat_map2 f vs (field_tys Types.(v.fields))
          | None -> [])
      | None -> [])
  | Union _, TAdt { id = TAdtId _; _ } ->
      (* FIXME: figure out if references inside unions get reborrowed. They could, but I
         suspect they don't because there's no guarantee the reference isn't some other field,
         e.g. in [union { a: &u8, b: &u16 }]  *)
      []
  | _ -> []

let rec update_ref_tys_in
    (fn :
      'acc ->
      'a full_ptr ->
      Types.ty ->
      Types.ref_kind ->
      ('a full_ptr * 'acc, 'e, 'f) Result.t) (init : 'acc) (v : 'a rust_val)
    (ty : Types.ty) : ('a rust_val * 'acc, 'e, 'f) Result.t =
  let open Rustsymex.Syntax in
  let f = update_ref_tys_in fn in
  let fs acc vs ty =
    let++ vs, acc =
      Result.fold_list vs ~init:([], acc) ~f:(fun (vs, acc) v ->
          let++ v, acc = f acc v ty in
          (v :: vs, acc))
    in
    (List.rev vs, acc)
  in
  let fs2 acc vs tys =
    let vs = List.combine vs tys in
    let++ vs, acc =
      Result.fold_list vs ~init:([], acc) ~f:(fun (vs, acc) (v, ty) ->
          let++ v, acc = f acc v ty in
          (v :: vs, acc))
    in
    (List.rev vs, acc)
  in
  match (v, ty) with
  | Ptr ptr, TRef (_, _, rk) ->
      let++ ptr, acc = fn init ptr (get_pointee ty) rk in
      (Ptr ptr, acc)
  | Tuple vs, TAdt { id = TAdtId adt_id; _ } ->
      let fields = Crate.as_struct adt_id in
      let++ vs, acc = fs2 init vs (field_tys fields) in
      (Tuple vs, acc)
  | ( Tuple vs,
      TAdt { id = TBuiltin (TArray | TSlice); generics = { types = [ ty ]; _ } }
    ) ->
      let++ vs, acc = fs init vs ty in
      (Tuple vs, acc)
  | Tuple vs, TAdt { id = TTuple; generics = { types; _ } } ->
      let++ vs, acc = fs2 init vs types in
      (Tuple vs, acc)
  | Enum (d, vs), TAdt { id = TAdtId adt_id; _ } -> (
      let variants = Crate.as_enum adt_id in
      let* var =
        match_on variants ~constr:(fun v -> BV.of_literal v.discriminant ==@ d)
      in
      match var with
      | Some var ->
          let++ vs, acc = fs2 init vs (field_tys Types.(var.fields)) in
          (Enum (d, vs), acc)
      | None -> Result.ok (v, init))
  | (Union _ as v), TAdt { id = TAdtId _; _ } ->
      (* FIXME: figure out if references inside unions get reborrowed. They could, but I
         suspect they don't because there's no guarantee the reference isn't some other field,
         e.g. in [union { a: &u8, b: &u16 }]  *)
      Result.ok (v, init)
  | v, _ -> Result.ok (v, init)

(** [is_abi_compatible ty1 ty2] is true if a function expecting an argument of
    type [ty1] can be called with an argument of type [ty2].

    The full specification is available at:
    https://doc.rust-lang.org/nightly/std/primitive.fn.html#abi-compatibility *)
let is_abi_compatible (ty1 : Types.ty) (ty2 : Types.ty) =
  let is_ptr_like : Types.ty -> bool = function
    | TRef _ | TRawPtr _ -> true
    | TAdt { id = TBuiltin TBox; _ } -> true
    | TAdt { id = TAdtId id; _ } ->
        let adt = Crate.get_adt id in
        adt.item_meta.lang_item = Some "owned_box"
        || Charon_util.meta_get_attr adt.item_meta "rustc_diagnostic_item"
           = Some "NonNull"
    | _ -> false
  in
  match (ty1, ty2) with
  (* Hack: &dyn is always compatible  *)
  | TDynTrait _, _
  | ( (TRef (_, TDynTrait _, _) | TRawPtr (TDynTrait _, _)),
      (TRef (_, _, _) | TRawPtr (_, _)) ) ->
      ok Typed.v_true
  (* Refs and raw pointers are ABI-compatible if they have the same metadata type *)
  | (TRef (_, ty1, _) | TRawPtr (ty1, _)), (TRef (_, ty2, _) | TRawPtr (ty2, _))
    ->
      ok (Typed.bool (dst_kind ty1 = dst_kind ty2))
  | TLiteral (TUInt uint1), TLiteral (TUInt uint2) ->
      ok (Typed.bool (size_of_uint_ty uint1 = size_of_uint_ty uint2))
  | TLiteral (TInt int1), TLiteral (TInt int2) ->
      ok (Typed.bool (size_of_int_ty int1 = size_of_int_ty int2))
  | TLiteral (TUInt U32), TLiteral TChar | TLiteral TChar, TLiteral (TUInt U32)
    ->
      ok Typed.v_true
  (* We keep this later down to avoid the check for everything *)
  | ty1, ty2 when is_ptr_like ty1 && is_ptr_like ty2 -> ok Typed.v_true
  (* FIXME: Function pointers are compatible if they have the same ABI-string (unsupported) *)
  | TFnPtr _, TFnPtr _ -> ok Typed.v_true
  | ty1, ty2 when Types.equal_ty ty1 ty2 -> ok Typed.v_true
  | _ ->
      let[@inline] is_1zst ty =
        let** size = size_of ty in
        let++ align = align_of ty in
        size ==@ Usize.(0s) &&@ (align ==@ Usize.(1s))
      in
      let** ty1_1zst = is_1zst ty1 in
      let++ ty2_1zst = is_1zst ty2 in
      (* 1ZSTs are exclusively compatible with themselves; otherwise type equality! *)
      ty1_1zst &&@ ty2_1zst
