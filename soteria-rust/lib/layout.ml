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
end

(* TODO: size should be an [option], for unsized types *)
type t = {
  size : T.sint Typed.t;
  align : T.nonzero Typed.t;
  uninhabited : bool;
  fields : Fields_shape.t;
}
[@@deriving show]

module Session = struct
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
  | TAdt { id = TBuiltin TStr; _ } | TSlice _ -> LenKind
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
  | TSlice sub_ty -> Some sub_ty
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
    (size +!!@ align -!!@ (size %@ align))

let mk ~size ~align ?(uninhabited = false)
    ?(fields : Fields_shape.t = Primitive) () =
  { size; align; uninhabited; fields }

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
           ~fields:(Array (BV.usizei ptr_size))
           ())
  (* Refs, pointers, boxes, function pointers *)
  | TAdt { id = TBuiltin TBox; _ } | TRef (_, _, _) | TRawPtr (_, _) | TFnPtr _
    ->
      let ptr_size = Crate.pointer_size () in
      ok (mk_concrete ~size:ptr_size ~align:ptr_size ())
  (* Dynamically sized types -- we assume they have a size of 0. In truth, these types should
     simply never be allocated directly, and instead can only be obtained hidden behind
     references; however we must be able to compute their layout, to get e.g. the offset of
     the tail in a DST struct.
     FIXME: Maybe we should mark the layout as a DST, and ensure a DST layout's size is never
     used for an allocation. *)
  | TAdt { id = TBuiltin TStr; _ } | TSlice _ ->
      let sub_ty = match ty with TSlice ty -> ty | _ -> TLiteral (TUInt U8) in
      let++ sub_layout = layout_of sub_ty in
      mk ~size:(BV.usizei 0) ~align:sub_layout.align
        ~fields:(Array sub_layout.size) ()
  (* Same as above, but here we have even less information ! *)
  | TDynTrait _ -> ok (mk_concrete ~size:0 ~align:1 ())
  (* Tuples *)
  | TAdt { id = TTuple; generics = { types; _ } } ->
      compute_arbitrary_layout ty types
  (* Custom ADTs (struct, enum, etc.) *)
  | TAdt { id = TAdtId id; _ } -> (
      let adt = Crate.get_adt id in
      match (adt.layout, adt.kind) with
      | Some layout, _ -> translate_layout ty layout
      | _, Struct fields -> compute_arbitrary_layout ty (field_tys fields)
      | _, Union fields -> compute_union_layout ty (field_tys fields)
      | _, Enum variants -> compute_enum_layout ty variants
      | _, (Opaque | TDeclError _ | Alias _) -> not_impl_layout "unexpected" ty)
  (* Arrays *)
  | TArray (subty, size) ->
      let max_array_len sub_size =
        (* We calculate the max array size for a 32bit architecture, like Miri does. *)
        let isize_bits = 32 - 1 in
        BV.usize Z.(one lsl isize_bits) /@ Typed.cast sub_size
      in
      let len = BV.of_const_generic size in
      let** sub_layout = layout_of subty in
      let++ () =
        assert_or_error
          (Typed.or_lazy
             (sub_layout.size ==@ Usize.(0s))
             (fun () -> len <=@ max_array_len sub_layout.size))
          (`InvalidLayout ty)
      in
      let size = len *!!@ sub_layout.size in
      mk ~size ~align:sub_layout.align ~fields:(Array sub_layout.size) ()
  (* Never -- zero sized type *)
  | TNever ->
      ok (mk_concrete ~size:0 ~align:1 ~uninhabited:true ~fields:Primitive ())
  (* Function definitions -- zero sized type *)
  | TFnDef _ -> ok (mk_concrete ~size:0 ~align:1 ~fields:Primitive ())
  (* Type variables : non-deterministically generate a layout *)
  | TVar _ ->
      (* FIXME: we need to scope these type variables, as the T in foo<T> and
         in bar<T> are "different" Ts. *)
      let* size = nondet (Typed.t_usize ()) in
      let* () = assume Usize.[ 0s <=$@ size; size <$@ 1024s ] in
      (* this is real non-determinism of the alignment; we don't do it because
         it creates quite expensive formulae that we want to avoid.
         We make the assumption the biggest possible alignment is 16, which
         is that of u128.
      let* align_shift = nondet (Typed.t_usize ()) in
      let* () = assume Usize.[ 0s <=$@ align_shift; align_shift <=$@ 4s ] in
      let align = Typed.cast (Usize.(1s) <<@ align_shift) in *)
      let align = Usize.(1s) in
      ok (mk ~size ~align ())
  (* Others (unhandled for now) *)
  | TPtrMetadata _ -> not_impl_layout "pointer metadata" ty
  | TError _ -> not_impl_layout "error" ty
  | TTraitType (tref, ty_name) ->
      let** resolved = resolve_trait_ty tref ty_name in
      layout_of resolved

and translate_layout ty (layout : Types.layout) =
  let size = compute_size ty layout.size in
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
  let variant_layouts =
    List.mapi
      (fun i (v : Types.variant_layout) : Fields_shape.t ->
        if v.uninhabited then Primitive
        else
          let ofs = Array.of_list (List.map BV.usizei v.field_offsets) in
          Arbitrary (Types.VariantId.of_int i, ofs))
      layout.variant_layouts
  in
  let fields : Fields_shape.t =
    match (tag_layout, variant_layouts) with
    (* tag layouts only exist on enum layouts *)
    | Some tag_layout, _ -> Enum (tag_layout, Array.of_list variant_layouts)
    (* no variants, so this is uninhabited; we can use primitive *)
    | None, [] -> Primitive
    (* there should be only one inhabited (non-Primitive) variant *)
    | None, _ -> (
        let is_inhabited = function
          | Fields_shape.Arbitrary _ -> true
          | _ -> false
        in
        let inhabited = List.filter is_inhabited variant_layouts in
        match inhabited with
        | [] -> Primitive
        | [ single ] -> single
        | _ :: _ :: _ -> failwith ">1 inhabited variants with no tag encoding?")
  in
  ok @@ mk ~size ~align ~uninhabited ~fields ()

and compute_size ty size =
  match size with
  | Some s -> BV.usizei s
  | None ->
      layout_warning "Inferred size=0" ty;
      BV.usizei 0

and compute_align ty align =
  match align with
  | Some a -> BV.usizeinz a
  | None ->
      layout_warning "Inferred align=1" ty;
      BV.usizeinz 1

and compute_arbitrary_layout ?fst_size ?fst_align
    ?(variant = Types.VariantId.zero) ty members =
  layout_warning "Computed an arbitrary layout" ty;
  (* Note: here we manually calculate a layout, à la [repr(C)]. We should avoid doing this,
     and make it clearer when we do. *)
  (* Calculates the offsets, size and alignment for a tuple-like type with fields of
     the given types. Also returns a symbolic boolean to assert this calculation did
     not overflow. *)
  let rec aux offsets curr_size curr_align overflowed = function
    | [] -> ok (List.rev offsets, curr_size, curr_align, overflowed)
    | ty :: rest ->
        let** { size; align; _ } = layout_of ty in
        let offset = size_to_fit ~size:curr_size ~align in
        let new_size, ovf = offset +$?@ size in
        let new_align = BV.max ~signed:false align curr_align in
        aux (offset :: offsets) new_size new_align (ovf ||@ overflowed) rest
  in
  let fst_size = Option.value fst_size ~default:(BV.usizei 0) in
  let fst_align = Option.value fst_align ~default:(BV.usizeinz 1) in
  let** offsets, size, align, overflowed =
    aux [] fst_size fst_align Typed.v_false members
  in
  let++ () = assert_or_error (Typed.not overflowed) (`InvalidLayout ty) in
  let size = size_to_fit ~size ~align in
  mk ~size ~align ~fields:(Arbitrary (variant, Array.of_list offsets)) ()

and compute_enum_layout ty (variants : Types.variant list) =
  layout_warning "Computed an enum layout" ty;
  match variants with
  (* no variants: uninhabited ZST *)
  | [] ->
      ok (mk_concrete ~size:0 ~align:1 ~uninhabited:true ~fields:Primitive ())
  (* N variants: we assume a tagged variant *)
  | _ :: _ ->
      let tags =
        Monad.ListM.map variants (fun v -> Some (BV.of_literal v.discriminant))
      in
      let tags = Array.of_list tags in
      let tag_layout : Tag_layout.t =
        (* best effort: we assume direct encoding *)
        let ty : Types.literal_type =
          match variants with
          | [] -> TInt I32 (* Shouldn't matter *)
          | v :: _ -> lit_ty_of_lit v.discriminant
        in
        { offset = Usize.(0s); ty; tags; encoding = Direct }
      in
      let** tag = layout_of (TLiteral tag_layout.ty) in
      let++ size, align, variants, uninhabited =
        Result.fold_list variants
          ~init:(Usize.(0s), Usize.(1s), [], true)
          ~f:(fun (size, align, variants, uninhabited) v ->
            let++ l =
              compute_arbitrary_layout ty ~fst_size:tag.size
                ~fst_align:tag.align (field_tys v.fields)
            in
            ( BV.max ~signed:false size l.size,
              BV.max ~signed:false align l.align,
              l.fields :: variants,
              uninhabited && l.uninhabited ))
      in
      let variants = List.rev variants in
      mk ~size ~align ~uninhabited
        ~fields:(Enum (tag_layout, Array.of_list variants))
        ()

and compute_union_layout ty members =
  layout_warning "Computed a union layout" ty;
  let++ size, align =
    Result.fold_list members
      ~init:(Usize.(0s), Usize.(1s))
      ~f:(fun (size, align) member_ty ->
        let++ member = layout_of member_ty in
        ( BV.max ~signed:false size member.size,
          BV.max ~signed:false align member.align ))
  in
  let fields = Array.make (List.length members) (BV.usizei 0) in
  mk ~size ~align ~fields:(Arbitrary (Types.VariantId.zero, fields)) ()

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
  let++ { size; _ } = layout_of ty in
  (Typed.cast size :> [> T.sint ] Typed.t)

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

let size_to_uint : int -> Types.ty = function
  | 1 -> TLiteral (TUInt U8)
  | 2 -> TLiteral (TUInt U16)
  | 4 -> TLiteral (TUInt U32)
  | 8 -> TLiteral (TUInt U64)
  | 16 -> TLiteral (TUInt U128)
  | _ -> failwith "Invalid integer size"

let lit_to_unsigned lit = size_to_uint @@ size_of_literal_ty lit

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
  | TArray (ty, len) ->
      let size = Charon_util.int_of_const_generic len in
      let++ fields = nondets @@ List.init size (fun _ -> ty) in
      Tuple fields
  | TAdt { id = TAdtId t_id; _ } as ty -> (
      let type_decl = Crate.get_adt t_id in
      match type_decl.kind with
      | Enum variants -> (
          let** layout = layout_of ty in
          let tag_layout =
            match layout.fields with
            | Fields_shape.Enum (tag_layout, _) -> tag_layout
            | _ -> failwith "Expected enum layout"
          in
          let* d = nondet_literal_ty tag_layout.ty in
          let* res =
            match_on variants ~constr:(fun v ->
                BV.of_literal v.discriminant ==@ d)
          in
          match (res, tag_layout.encoding) with
          | Some variant, _ ->
              let discr = BV.of_literal variant.discriminant in
              let++ fields = nondets @@ Charon_util.field_tys variant.fields in
              Enum (discr, fields)
          | None, Direct -> vanish ()
          | None, Niche untagged ->
              let variant = Types.VariantId.nth variants untagged in
              let discr = BV.of_literal variant.discriminant in
              let++ fields = nondets @@ Charon_util.field_tys variant.fields in
              Enum (discr, fields))
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

(** Apply the compiler-attribute to the given value *)
let apply_attribute v attr =
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
  | TAdt { id = TTuple; generics = { types; _ } } ->
      List.exists is_unsafe_cell types
  | TAdt { id = TBuiltin _; _ } -> false
  | TAdt { id = TAdtId id; _ } -> (
      let adt = Crate.get_adt id in
      if adt.item_meta.lang_item = Some "unsafe_cell" then true
      else
        match adt.kind with
        | Struct fs | Union fs -> List.exists is_unsafe_cell (field_tys fs)
        | Enum vs ->
            Iter.exists is_unsafe_cell
            @@ Iter.flat_map_l (fun (v : Types.variant) -> field_tys v.fields)
            @@ Iter.of_list vs
        | _ -> false)
  | TArray (ty, _) | TSlice ty -> is_unsafe_cell ty
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
  | Tuple vs, (TArray (ty, _) | TSlice ty) ->
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
  | Tuple vs, (TArray (ty, _) | TSlice ty) ->
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
        let++ layout = layout_of ty in
        layout.size ==@ Usize.(0s) &&@ (layout.align ==@ Usize.(1s))
      in
      let** ty1_1zst = is_1zst ty1 in
      let++ ty2_1zst = is_1zst ty2 in
      (* 1ZSTs are exclusively compatible with themselves; otherwise type equality! *)
      ty1_1zst &&@ ty2_1zst
