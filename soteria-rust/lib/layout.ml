open Charon
open Svalue
open Typed.Infix
open Typed.Syntax
open Compo_res
module BV = Typed.BV
open Rustsymex
open Rustsymex.Result
open Rustsymex.Syntax
open Common.Charon_util

(* Import types *)
include Layout_common

module Session = struct
  (** Cache of (type or variant) -> layout *)
  type cache = (Types.ty, t) Hashtbl.t

  type _ Effect.t += Get_cache : cache Effect.t

  let get_cache () = Effect.perform Get_cache

  let with_layout_cache f =
    let open Effect.Deep in
    let cache : (Types.ty, t) Hashtbl.t = Hashtbl.create 128 in
    try f () with effect Get_cache, k -> continue k cache

  let get_or_compute_cached_layout ty f =
    let cache = get_cache () in
    match Hashtbl.find_opt cache ty with
    | Some layout -> ok layout
    | None -> (
        let* gen_layout = Poly.get_layout ty in
        match gen_layout with
        | Some layout -> ok layout
        | None ->
            let** layout = f () in
            let is_concrete = Iter.is_empty (iter_vars layout) in
            if is_concrete then (
              Hashtbl.add cache ty layout;
              Result.ok layout)
            else
              let+ () = Poly.push_layout ty layout in
              Ok layout)
end

let size_of_int_ty = size_of_int_ty
let size_of_uint_ty = size_of_uint_ty
let size_of_literal_ty = size_of_literal_ty
let is_signed = is_signed

(* TODO: this is not really if we want to properly emulate different platforms,
   but this is good enough for now. See
   https://doc.rust-lang.org/reference/type-layout.html#r-layout.primitive.align *)
let align_of_literal_ty = size_of_literal_ty

type meta_kind = LenKind | VTableKind | NoneKind

let rec dst_kind : Types.ty -> meta_kind = function
  | TAdt { id = TBuiltin TStr; _ } | TSlice _ -> LenKind
  | TDynTrait _ -> VTableKind
  | TAdt adt when Crate.is_struct adt -> (
      match List.last_opt (Crate.as_struct adt) with
      | None -> NoneKind
      | Some last -> dst_kind Types.(last.field_ty))
  | _ -> NoneKind

(** If this is a DST type with a slice tail, return the type of the slice's
    element. *)
let rec dst_slice_ty : Types.ty -> Types.ty option = function
  | TAdt { id = TBuiltin TStr; _ } -> Some (TLiteral (TUInt U8))
  | TSlice sub_ty -> Some sub_ty
  | TAdt adt when Crate.is_struct adt -> (
      match List.last_opt (Crate.as_struct adt) with
      | None -> None
      | Some last -> dst_slice_ty Types.(last.field_ty))
  | _ -> None

(** If this is a dynamically sized type (requiring a fat pointer) *)
let is_dst ty = dst_kind ty <> NoneKind

(** The [Pointee::Metadata] associated type of a pointer to [pointee]: [()] for
    sized types, [usize] for slices and [str], and [DynMetadata<Dyn>] for trait
    objects. *)
let pointee_metadata (pointee : Types.ty) : Types.ty =
  match dst_kind pointee with
  | NoneKind -> TypesUtils.mk_unit_ty
  | LenKind -> TLiteral (TUInt Usize)
  | VTableKind ->
      let adt = Crate.get_adt_lang_item "dyn_metadata" in
      TAdt
        {
          id = TAdtId adt.def_id;
          generics = TypesUtils.mk_generic_args_from_types [ pointee ];
        }

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

let not_impl_layout msg ty = not_impl "Can't compute layout: %s %a" msg pp_ty ty
let layout_warning msg ty = [%l.warn "Layout: %s@.Type: %a" msg pp_ty ty]

let rec layout_of (ty : Types.ty) : (t, 'e, 'f) Rustsymex.Result.t =
  let* ty = Poly.subst_ty ty in
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
           ~fields:(Array { stride = BV.usizei ptr_size; is_ptr = true })
           ())
  (* Refs, pointers, boxes, function pointers *)
  | TAdt { id = TBuiltin TBox; _ } | TRef (_, _, _) | TRawPtr (_, _) | TFnPtr _
    ->
      let ptr_size = Crate.pointer_size () in
      ok (mk_concrete ~size:ptr_size ~align:ptr_size ())
  (* Dynamically sized types -- we assume they have a size of 0. In truth, these
     types should simply never be allocated directly, and instead can only be
     obtained hidden behind references; however we must be able to compute their
     layout, to get e.g. the offset of the tail in a DST struct. FIXME: Maybe we
     should mark the layout as a DST, and ensure a DST layout's size is never
     used for an allocation. *)
  | TAdt { id = TBuiltin TStr; _ } | TSlice _ ->
      let sub_ty = match ty with TSlice ty -> ty | _ -> TLiteral (TUInt U8) in
      let++ sub_layout = layout_of sub_ty in
      mk ~size:(BV.usizei 0) ~align:sub_layout.align
        ~fields:(Array { stride = sub_layout.size; is_ptr = false })
        ()
  (* Same as above, but here we have even less information ! *)
  | TDynTrait _ -> ok (mk_concrete ~size:0 ~align:1 ())
  (* Tuples *)
  | TAdt { id = TTuple; generics = { types; _ } } ->
      compute_arbitrary_layout ty types
  (* Custom ADTs (struct, enum, etc.) *)
  | TAdt adt -> (
      let adt = Crate.get_adt adt in
      match (adt.layout, adt.kind) with
      (* FIXME: Charon has surprising behaviour when translating layouts in
         polymorphic mode, meaning some types may have a layout while their
         fields don't. To avoid this, we *never* consider layouts of generic
         types, even if one is provided. This avoids inconsistent layouts. *)
      | [ (_triple, layout) ], _
        when (not (Config.get ()).polymorphic) || ty_is_monomorphic ty ->
          translate_layout ty layout
      | _ :: _ :: _, _ -> L.failwith "multiple layouts for the same ADT"
      | _, Struct fields -> compute_arbitrary_layout ty (field_tys fields)
      | _, Union fields -> compute_union_layout ty (field_tys fields)
      | _, Enum variants -> compute_enum_layout ty variants
      | _, (Opaque | TDeclError _ | Alias _) -> not_impl_layout "unexpected" ty)
  (* Arrays *)
  | TArray (subty, size) ->
      let max_array_len sub_size =
        (* We calculate the max array size for a 32bit architecture, like Miri
           does. *)
        let isize_bits = 32 - 1 in
        BV.usize Z.(one lsl isize_bits) /@ Typed.cast_nonzero sub_size
      in
      let len = BV.of_constant_expr size in
      let** sub_layout = layout_of subty in
      let++ () =
        assert_or_error
          (Typed.or_lazy
             (sub_layout.size ==@ Usize.(0s))
             (fun () -> len <=@ max_array_len sub_layout.size))
          (`InvalidLayout ty)
      in
      let size = len *!!@ sub_layout.size in
      mk ~size ~align:sub_layout.align
        ~fields:(Array { stride = sub_layout.size; is_ptr = false })
        ()
  (* Never -- zero sized type *)
  | TNever ->
      ok (mk_concrete ~size:0 ~align:1 ~uninhabited:true ~fields:Primitive ())
  (* Function definitions -- zero sized type *)
  | TFnDef _ -> ok (mk_concrete ~size:0 ~align:1 ~fields:Primitive ())
  (* Pattern types -- just their content, we assume they're primitives *)
  | TPattern (ty, _) -> layout_of ty
  (* Type variables : non-deterministically generate a layout *)
  | TVar (Free _) ->
      (* FIXME: we need to scope these type variables, as the T in foo<T> and in
         bar<T> are "different" Ts. *)
      let* size = nondet (Typed.t_usize ()) in
      let* () = assume Usize.[ 0s <=$@ size; size <$@ 1024s ] in
      (* this is real non-determinism of the alignment; we don't do it because
         it creates quite expensive formulae that we want to avoid. We make the
         assumption the biggest possible alignment is 16, which is that of
         u128. *)
      (* let* align_shift = nondet (Typed.t_usize ()) in
       * let* () = assume Usize.[ 0s <=$@ align_shift; align_shift <=$@ 4s ] in
       * let align = Typed.cast (Usize.(1s) <<@ align_shift) in *)
      let align = Usize.(1s) in
      ok (mk ~size ~align ())
  | TVar (Bound _) ->
      L.failwith "escaping bound type variable found in layout_of"
  (* Others (unhandled for now) *)
  | TPtrMetadata _ -> not_impl_layout "pointer metadata" ty
  | TError _ -> not_impl_layout "error" ty
  | TTraitType (tref, assoc_ty_id, args) ->
      let** resolved = resolve_trait_ty tref assoc_ty_id args in
      layout_of resolved

and translate_discriminator : Types.discriminator -> Fields_shape.discriminator
    = function
  | Known v -> Known v
  | Invalid -> Invalid
  | Branch (offset, int_ty, children, fallback) ->
      let offset = BV.usizei offset in
      let children =
        List.map
          (fun ((from_, to_), discr) ->
            (BV.of_scalar from_, BV.of_scalar to_, translate_discriminator discr))
          children
      in
      let tag_ty = lit_of_int_ty int_ty in
      let fallback = translate_discriminator fallback in
      Branch { offset; tag_ty; children; fallback }

and translate_layout ty (layout : Types.layout) =
  let size = compute_size ty layout.size in
  let align = compute_align ty layout.align in
  let uninhabited = layout.uninhabited in
  let discriminator = Option.map translate_discriminator layout.discriminator in
  let variant_layouts =
    List.mapi
      (fun i v_opt : (Fields_shape.tagger * Fields_shape.t) ->
        match (v_opt : Types.variant_layout option) with
        | None | Some { uninhabited = true; _ } -> (None, Primitive)
        | Some v ->
            let ofs = Array.of_list (List.map BV.usizei v.field_offsets) in
            let tagger =
              match v.tagger with
              | [] -> None
              | [ (ofs, value) ] -> Some (BV.usizei ofs, BV.of_scalar value)
              | _ :: _ :: _ -> L.failwith "unsupported: >1 tagger values"
            in
            (tagger, Arbitrary (Types.VariantId.of_int i, ofs)))
      layout.variant_layouts
  in
  let fields : Fields_shape.t =
    match (discriminator, variant_layouts) with
    (* tag layouts only exist on enum layouts *)
    | Some (Branch _ as d), _ -> Enum (d, Array.of_list variant_layouts)
    (* no variants/invalid, so this is uninhabited; we can use primitive *)
    | _, [] | Some Invalid, _ -> Primitive
    (* there is only one inhabited (non-Primitive) variant *)
    | Some (Known v), _ -> snd (Types.VariantId.nth variant_layouts v)
    (* we didn't translate a discriminator; we hope it's a struct-like! *)
    | None, [ (_, v) ] -> v
    | None, _ -> L.failwith "no discriminator and >1 variant layouts?"
  in
  let layout = mk ~size ~align ~uninhabited ~fields () in
  [%l.trace "Translated layout for %a:@.%a" pp_ty ty pp layout];
  ok layout

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
  (* Note: here we manually calculate a layout, à la [repr(C)]. We should avoid doing this,
     and make it clearer when we do. *)
  (* Calculates the offsets, size and alignment for a tuple-like type with fields of
     the given types. Also returns a symbolic boolean to assert this calculation did
     not overflow. *)
  let rec aux offsets curr_size curr_align overflowed uninhabited = function
    | [] -> ok (List.rev offsets, curr_size, curr_align, overflowed, uninhabited)
    | ty :: rest ->
        let** layout = layout_of ty in
        let offset = size_to_fit ~size:curr_size ~align:layout.align in
        let new_size, ovf = offset +$?@ layout.size in
        let new_align = BV.max ~signed:false layout.align curr_align in
        aux (offset :: offsets) new_size new_align (ovf ||@ overflowed)
          (uninhabited || layout.uninhabited)
          rest
  in
  let fst_size = Option.value fst_size ~default:(BV.usizei 0) in
  let fst_align = Option.value fst_align ~default:(BV.usizeinz 1) in
  let** offsets, size, align, overflowed, uninhabited =
    aux [] fst_size fst_align Typed.v_false false members
  in
  let++ () = assert_or_error (Typed.not overflowed) (`InvalidLayout ty) in
  let size = size_to_fit ~size ~align in
  let layout =
    mk ~size ~align ~uninhabited
      ~fields:(Arbitrary (variant, Array.of_list offsets))
      ()
  in
  Fmt.kstr layout_warning "Computed an arbitrary layout:@.%a" pp layout ty;
  layout

and compute_enum_layout ty (variants : Types.variant list) =
  match variants with
  (* no variants: uninhabited ZST *)
  | [] ->
      layout_warning "Computed an enum layout (ZST)" ty;
      ok (mk_concrete ~size:0 ~align:1 ~uninhabited:true ~fields:Primitive ())
  (* N variants: we assume a tagged variant *)
  | _ :: _ ->
      let tags =
        List.map
          (fun (v : Types.variant) -> BV.of_literal v.discriminant)
          variants
      in
      let tag_ty =
        match variants with
        | [] -> Values.TInt I32 (* Shouldn't matter *)
        | v :: _ -> lit_ty_of_lit v.discriminant
      in
      let discriminator : Fields_shape.discriminator =
        (* best effort: we assume direct encoding *)
        let children =
          List.map
            (fun (v : Types.variant) ->
              let tag = Types.VariantId.nth tags v.id in
              (tag, tag, Fields_shape.Known v.id))
            variants
        in
        let offset = Usize.(0s) in
        Branch { offset; tag_ty; children; fallback = Invalid }
      in
      let** tag = layout_of (TLiteral tag_ty) in
      let++ size, align, variants, uninhabited =
        Result.fold_list variants
          ~init:(Usize.(0s), Usize.(1s), [], true)
          ~f:(fun (size, align, variants, uninhabited) v ->
            let++ l =
              compute_arbitrary_layout ty ~fst_size:tag.size
                ~fst_align:tag.align ~variant:v.id (field_tys v.fields)
            in
            let tagger =
              if l.uninhabited then None
              else Some (BV.usizei 0, Types.VariantId.nth tags v.id)
            in
            ( BV.max ~signed:false size l.size,
              BV.max ~signed:false align l.align,
              (tagger, l.fields) :: variants,
              uninhabited && l.uninhabited ))
      in
      let variants = List.rev variants in
      let layout =
        mk ~size ~align ~uninhabited
          ~fields:(Enum (discriminator, Array.of_list variants))
          ()
      in
      Fmt.kstr layout_warning "Computed an enum layout:@.%a" pp layout ty;
      layout

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

and resolve_trait_ty (tref : Types.trait_ref) assoc_ty_id args =
  match tref.kind with
  | TraitImpl timplref ->
      let impl = Crate.get_trait_impl timplref in
      let trait_assoc_ty = Types.AssocTypeId.Map.find assoc_ty_id impl.types in
      (* HACK: we skip the binder here! *)
      ok trait_assoc_ty.binder_value.value
  | BuiltinOrAuto (BuiltinPointee, _, _) ->
      let pointee = List.hd tref.trait_decl_ref.binder_value.generics.types in
      ok (pointee_metadata pointee)
  | _ -> not_impl_layout "trait type" (TTraitType (tref, assoc_ty_id, args))

(** Normalise a type, by substituting any generics with the current generic
    environment, and resolving the trait type if needed. *)
let normalise (ty : Types.ty) =
  let** ty =
    match ty with
    | TTraitType (tref, name, args) -> resolve_trait_ty tref name args
    | _ -> ok ty
  in
  let+ ty = Poly.subst_ty ty in
  Ok ty

let size_of ty =
  let++ { size; _ } = layout_of ty in
  (size : Typed.T.sint Typed.t :> Typed.([> T.sint ] t))

let align_of ty =
  let++ { align; _ } = layout_of ty in
  (align : Typed.T.nonzero Typed.t :> Typed.([> T.nonzero ] t))

let min_value_z : Types.literal_type -> Z.t = function
  | TUInt _ -> Z.zero
  | TInt Isize -> Z.neg (Z.shift_left Z.one ((8 * Crate.pointer_size ()) - 1))
  | TInt I128 -> Z.neg (Z.shift_left Z.one 127)
  | TInt I64 -> Z.neg (Z.shift_left Z.one 63)
  | TInt I32 -> Z.neg (Z.shift_left Z.one 31)
  | TInt I16 -> Z.neg (Z.shift_left Z.one 15)
  | TInt I8 -> Z.neg (Z.shift_left Z.one 7)
  | _ -> L.failwith "Invalid integer type for min_value_z"

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
  | _ -> L.failwith "Invalid integer type for max_value_z"

let rec is_unsafe_cell : Types.ty -> bool = function
  | TAdt { id = TTuple; generics = { types; _ } } ->
      List.exists is_unsafe_cell types
  | TAdt { id = TBuiltin _; _ } -> false
  | TAdt adt -> (
      let adt = Crate.get_adt adt in
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

(** Whether this type is uninhabited; this is under-approximate, and should be
    avoided; use {!layout_of} and check the [uninhabited] flag instead, if
    possible. *)
let rec is_uninhabited : Types.ty -> bool = function
  | TLiteral _
  | TRef (_, _, _)
  | TRawPtr (_, _)
  | TFnPtr _ | TFnDef _
  | TAdt { id = TBuiltin _; _ } ->
      false
  | TNever -> true
  | TAdt { id = TTuple; generics = { types; _ } } ->
      List.exists is_uninhabited types
  | TAdt ({ id = TAdtId _; _ } as adt) as ty -> (
      match (Crate.get_adt adt).kind with
      | Struct tys -> List.exists is_uninhabited (field_tys tys)
      | Union tys -> false
      | Enum vs ->
          List.for_all
            (fun (v : Types.variant) ->
              List.exists is_uninhabited (field_tys v.fields))
            vs
      | Opaque | Alias _ | TDeclError _ ->
          Fmt.failwith "can't determine the uninhabitedness of %a" pp_ty ty)
  | TArray (ty, _) | TPattern (ty, _) | TSlice ty -> is_uninhabited ty
  | (TVar _ | TTraitType (_, _, _) | TDynTrait _ | TPtrMetadata _ | TError _) as
    ty ->
      Fmt.failwith "can't determine the uninhabitedness of %a" pp_ty ty

(** Whether this type is a ZST; this is under-approximate, and should be
    avoided; use {!layout_of} and symbolically compare [size] with [0] instead
*)
let rec is_zst : Types.ty -> bool = function
  | TLiteral _
  | TRef (_, _, _)
  | TRawPtr (_, _)
  | TFnPtr _
  | TAdt { id = TBuiltin _; _ } ->
      false
  | TNever | TFnDef _ -> true
  | TArray (ty, _) | TPattern (ty, _) | TSlice ty -> is_zst ty
  | TAdt { id = TTuple; generics = { types; _ } } -> List.for_all is_zst types
  | TAdt ({ id = TAdtId _; _ } as adt) as ty -> (
      match (Crate.get_adt adt).kind with
      | Struct tys | Union tys -> List.for_all is_zst (field_tys tys)
      | Enum [] -> true
      | Enum vs ->
          let rec aux saw_inhabited = function
            | [] -> true
            | (v : Types.variant) :: rest ->
                let fs = field_tys v.fields in
                if List.exists is_uninhabited fs then aux saw_inhabited rest
                else if List.for_all is_zst fs then
                  (not saw_inhabited) && aux true rest
                else false
          in
          aux false vs
      | Opaque | Alias _ | TDeclError _ ->
          Fmt.failwith "can't determine the ZST-ness of %a" pp_ty ty)
  | (TVar _ | TTraitType (_, _, _) | TDynTrait _ | TPtrMetadata _ | TError _) as
    ty ->
      Fmt.failwith "can't determine the ZST-ness of %a" pp_ty ty

(** [is_abi_compatible ty1 ty2] is true if a function expecting an argument of
    type [ty1] can be called with an argument of type [ty2].

    The full specification is available at:
    https://doc.rust-lang.org/nightly/std/primitive.fn.html#abi-compatibility *)
let rec is_abi_compatible (ty1 : Types.ty) (ty2 : Types.ty) =
  let is_ptr_like : Types.ty -> bool = function
    | TRef _ | TRawPtr _ -> true
    | TAdt { id = TBuiltin TBox; _ } -> true
    | TAdt adt -> adt_is_box adt
    | _ -> false
  in
  let[@inline] is_1zst ty =
    let++ layout = layout_of ty in
    layout.size ==@ Usize.(0s) &&@ (layout.align ==@ Usize.(1s))
  in
  let is_repr_transparent (adt : Types.type_decl_ref) =
    match adt.id with
    | TAdtId id -> (
        match (Crate.get_adt_raw id).layout with
        | [ (_triple, { repr = { transparent = true; _ }; _ }) ] -> true
        | _ -> false)
    | _ -> false
  in
  let rec find_non_zst_field = function
    | [] -> Result.ok None
    | ty :: rest ->
        let** is_zst = is_1zst ty in
        if%sat is_zst then find_non_zst_field rest else ok (Some ty)
  in
  let as_transparent (adt : Types.type_decl_ref) =
    match (Crate.get_adt adt).kind with
    | Struct fields -> find_non_zst_field (field_tys fields)
    | Enum [ v ] -> find_non_zst_field (field_tys v.fields)
    | _ -> ok None
  in
  match (ty1, ty2) with
  (* Hack: &dyn is always compatible *)
  | TDynTrait _, _
  | ( (TRef (_, TDynTrait _, _) | TRawPtr (TDynTrait _, _)),
      (TRef (_, _, _) | TRawPtr (_, _)) ) ->
      ok Typed.v_true
  (* Refs and raw pointers are ABI-compatible if they have the same metadata
     type *)
  | (TRef (_, ty1, _) | TRawPtr (ty1, _)), (TRef (_, ty2, _) | TRawPtr (ty2, _))
    ->
      ok (Typed.of_bool (dst_kind ty1 = dst_kind ty2))
  | TLiteral (TUInt uint1), TLiteral (TUInt uint2) ->
      ok (Typed.of_bool (size_of_uint_ty uint1 = size_of_uint_ty uint2))
  | TLiteral (TInt int1), TLiteral (TInt int2) ->
      ok (Typed.of_bool (size_of_int_ty int1 = size_of_int_ty int2))
  | TLiteral (TUInt U32), TLiteral TChar | TLiteral TChar, TLiteral (TUInt U32)
    ->
      ok Typed.v_true
  (* patterns: recurse *)
  | TPattern (inner1, _), ty2 -> is_abi_compatible inner1 ty2
  | ty1, TPattern (inner2, _) -> is_abi_compatible ty1 inner2
  (* Function pointers are compatible if they have the same ABI-string *)
  | TFnPtr sig1, TFnPtr sig2 ->
      ok
        (Typed.of_bool
           (Types.equal_abi sig1.binder_value.abi sig2.binder_value.abi))
  (* We keep this later down to avoid the check for everything *)
  | ty1, ty2 when is_ptr_like ty1 && is_ptr_like ty2 -> ok Typed.v_true
  (* transparent ADTs: recurse *)
  | TAdt adt1, _ when is_repr_transparent adt1 -> (
      let** ty1 = as_transparent adt1 in
      match ty1 with
      | None -> is_1zst ty2
      | Some ty1 -> is_abi_compatible ty1 ty2)
  | _, TAdt adt2 when is_repr_transparent adt2 -> (
      let** ty2 = as_transparent adt2 in
      match ty2 with
      | None -> is_1zst ty1
      | Some ty2 -> is_abi_compatible ty1 ty2)
  | ty1, ty2 when Types.equal_ty ty1 ty2 -> ok Typed.v_true
  | _ ->
      let** ty1_1zst = is_1zst ty1 in
      let++ ty2_1zst = is_1zst ty2 in
      (* 1ZSTs are exclusively compatible with themselves; otherwise type
         equality! *)
      ty1_1zst &&@ ty2_1zst

(** Returns the path through an ADT to the pointer that is the target of an
    unsizing operation, returning [None] if no path was found. If the path is
    [Some], it is guaranteed that following the path leads to a pointer type.

    The path is found by recursively exploring the last non-ZST field of the
    structure, until a pointer is found. *)
let rec unsize_path ty : int list option =
  let rec aux acc : Types.ty -> int list option = function
    | TRawPtr _ | TRef _ -> Some acc
    | TPattern (ty, _) -> aux acc ty
    | TAdt adt when Crate.is_struct_or_tuple adt ->
        let tys = Crate.as_struct_or_tuple adt in
        let len = List.length tys in
        List.find_mapi
          (fun idx ty -> if is_zst ty then None else Some (len - idx - 1, ty))
          (List.rev tys)
        |> Option.bind (fun (idx, ty) -> aux (idx :: acc) ty)
    | ty -> None
  in
  Option.map List.rev @@ aux [] ty
