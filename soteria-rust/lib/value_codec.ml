open Charon
open Svalue
open Typed.Syntax
open Typed.Infix
open Common.Charon_util
open Layout
module DecayMap = Sptr.DecayMap
open DecayMap.SM
open Syntax

(** Returns the variant id and variant matching the given discriminant. *)
let variant_for_discr discr adt =
  let open Rustsymex in
  let open Syntax in
  let variants = Crate.as_enum adt in
  let* variant =
    match_on variants ~constr:(fun v -> BV.of_literal v.discriminant ==@ discr)
  in
  of_opt_not_impl "no matching variant for enum discriminant" variant

(** Iterator over the fields and offsets of a type; for primitive types, returns
    a singleton iterator for that value. *)
let iter_fields ?variant ?meta layout (ty : Types.ty) =
  let rec to_fields ?variant fields : Types.ty -> Types.ty Iter.t = function
    | TAdt { id = TTuple; generics = { types; _ } } -> Iter.of_list types
    | TArray (ty, len) -> Iter.repeatz (z_of_constant_expr len) ty
    | TSlice _ | TAdt { id = TBuiltin TStr; _ } -> (
        let sub_ty =
          match ty with TSlice ty -> ty | _ -> TLiteral (TUInt U8)
        in
        let meta : [< Typed.T.ptr_meta ] Typed.t =
          Option.get ~msg:"meta required for unsized iter_fields" meta
        in
        let len = Typed.cast_i Usize meta in
        match BV.to_z len with
        (* TODO: strings and slices of symbolic length *)
        | Some len -> Iter.repeatz len sub_ty
        | None -> failwith "iter_fields: symbolic length for slice/str")
    | TAdt adt -> (
        let type_decl = Crate.get_adt adt in
        match (type_decl.kind, variant) with
        | Struct fields, _ ->
            let field_tys = field_tys fields in
            Iter.of_list field_tys
        | Enum variants, Some variant ->
            let variant = Types.VariantId.nth variants variant in
            let field_tys = field_tys variant.fields in
            Iter.of_list field_tys
        | _ -> failwith "invalid iter_fields type_decl")
    | TRef (_, pointee, _) | TRawPtr (pointee, _) -> (
        match Layout.dst_kind pointee with
        | NoneKind -> failwith "invalid iter_fields: no metadata"
        | LenKind -> Iter.of_list [ unit_ptr; TLiteral (TInt Isize) ]
        | VTableKind -> Iter.of_list [ unit_ptr; unit_ptr ])
    | TPattern (inner, _) -> to_fields ?variant fields inner
    | TLiteral _ | TNever | TVar _ | TTraitType _ | TDynTrait _ | TFnPtr _
    | TFnDef _ | TPtrMetadata _ | TError _ ->
        Fmt.failwith "invalid iter_fields: %a" pp_ty ty
  in
  let aux ?variant fields ty =
    to_fields ?variant fields ty
    |> Iter.mapi (fun i ty -> (ty, Fields_shape.offset_of i fields))
  in
  match layout.fields with
  | Primitive -> Iter.singleton (ty, Usize.(0s))
  | Array _ -> aux ?variant layout.fields ty
  | Arbitrary (variant, _) -> aux ~variant layout.fields ty
  | Enum (_, variant_layouts) ->
      let variant = Option.get ~msg:"variant required for enum" variant in
      let _, fields = variant_layouts.(Types.VariantId.to_int variant) in
      aux ~variant fields ty

module Decoder (State_tys : sig
  module SM :
    Soteria.Sym_states.State_monad.S
      with type 'a Symex.t = 'a DecayMap.SM.t
       and type 'a Symex.Value.t = 'a Typed.t
       and type 'a Symex.Value.ty = 'a Typed.ty
       and type Symex.Value.sbool = Typed.sbool

  type fix
end) =
struct
  module ParserMonad = struct
    open State_tys

    type query = Types.ty * Typed.(T.sint t)
    type 'a res = ('a, Error.t, fix) SM.Result.t

    (* size * offset *)
    type get_all_query = Typed.(T.nonzero t) * Typed.(T.sint t)

    (* The following is just query -> (rust_val, 'err, 'fix) StateResult.t where
       StateResult = StateT (Result), but I need StateT1of3 urgh. *)
    type handler = query -> Typed.T.any Typed.t res
    type get_all_handler = get_all_query -> Typed.(T.any t * T.sint t) list res

    (* A parser monad is an object such that, given a query handler with state
       ['state], returns a state monad-ish for that state which may fail or
       branch *)
    type 'a t = handler -> get_all_handler -> 'a res

    let parse ~(handler : handler) ~(get_all : get_all_handler) scheduler :
        'a res =
      scheduler handler get_all

    let ok (x : 'a) : 'a t = fun _handler _get_all -> SM.Result.ok x
    let error (e : Error.t) : 'a t = fun _handler _get_all -> SM.Result.error e

    let bind2 (f : 'a -> 'b t) (fe : 'err -> 'b t) (m : 'a t) : 'b t =
     fun handler get_all ->
      let open SM.Syntax in
      let* res = m handler get_all in
      match res with
      | Compo_res.Ok x -> f x handler get_all
      | Compo_res.Error e -> fe e handler get_all
      | Compo_res.Missing f -> SM.Result.miss f

    let bind (f : 'a -> 'b t) (m : 'a t) : 'b t =
     fun handler get_all ->
      let open SM.Syntax in
      let** x = m handler get_all in
      f x handler get_all

    let map (f : 'a -> 'b) (m : 'a t) : 'b t =
     fun handler get_all ->
      let open SM.Syntax in
      let++ x = m handler get_all in
      f x

    let query (q : query) : 'a t = fun (handler : handler) _ -> handler q

    let get_all (q : get_all_query) : 'a t =
     fun _ get_all state -> get_all q state

    let[@inline] lift (m : 'a DecayMap.SM.t) : 'a t =
      let open SM.Syntax in
      fun _handler _get_all ->
        let*^ m in
        SM.Result.ok m

    let lift_rsymex (m : ('a, 'err, 'fix) Rustsymex.Result.t) : 'a t =
     fun _handler _get_all -> SM.lift (DecayMap.SM.lift m)

    let not_impl msg = lift @@ not_impl msg
    let of_opt_not_impl msg x = lift @@ of_opt_not_impl msg x
    let layout_of ty = lift_rsymex @@ Layout.layout_of ty
    let normalise ty = lift_rsymex @@ Layout.normalise ty

    let assert_or_error cond err =
     fun _handler _get_all state ->
      DecayMap.SM.Result.map (fun () -> ((), state)) (assert_or_error cond err)

    let fold_iter x ~init ~f =
      Monad.foldM ~bind ~return:ok ~fold:Foldable.Iter.fold x ~init ~f

    module Syntax = struct
      let ( let* ) x f = bind f x
      let ( let+ ) x f = map f x
      let ( let*^ ) x f = bind f (lift x)

      module Symex_syntax = struct
        let branch_on ?left_branch_name ?right_branch_name guard ~then_ ~else_ =
         fun handler get_all state ->
          DecayMap.SM.branch_on ?left_branch_name ?right_branch_name guard
            ~then_:(fun () -> then_ () handler get_all state)
            ~else_:(fun () -> else_ () handler get_all state)
      end
    end
  end

  (** Parses the current variant of the enum at the given offset. This handles
      cases such as niches, where the discriminant isn't directly encoded as a
      tag. *)
  let variant_of_enum ~offset ty : Types.variant_id ParserMonad.t =
    let open ParserMonad in
    let open ParserMonad.Syntax in
    let rec exec ?(pp_info = Fmt.nop) :
        Fields_shape.discriminator -> Types.variant_id ParserMonad.t = function
      | Known v -> ok v
      | Branch { offset = tag_ofs; tag_ty; children; fallback } ->
          let* tag = query (TLiteral tag_ty, offset +!!@ tag_ofs) in
          let tag = Typed.cast_lit tag_ty tag in
          let pp_info ft () =
            Fmt.pf ft ", read %a: %s at offset %a" Typed.ppa tag
              (Print.literal_type_to_string tag_ty)
              Typed.ppa offset
          in
          let rec aux = function
            | [] -> exec fallback
            | (from_, to_, discr) :: rest when Typed.equal from_ to_ ->
                if%sat tag ==@ from_ then exec ~pp_info discr else aux rest
            | (from_, to_, discr) :: rest ->
                if%sat from_ <=@ tag &&@ (tag <=@ to_) then exec ~pp_info discr
                else aux rest
          in
          aux children
      | Invalid ->
          let adt = ty_as_adt ty in
          let adt = Crate.get_adt adt in
          error
            (`UBTransmute
               (Fmt.str "No valid variant for enum %a%a" Crate.pp_name
                  adt.item_meta.name pp_info ()))
    in
    let* layout = layout_of ty in
    match layout.fields with
    | Arbitrary (vid, _) -> ok vid
    | Enum (discriminator, _) -> exec discriminator
    | Array _ | Primitive -> failwith "Unexpected layout for enum"

  (** [decode ~meta ~offset ty] Parses a rust value of type [ty] at the given
      offset, using the provided metadata for DSTs, and returns the associated
      [Rust_val]. This does not perform any validity checking, aside from
      erroring if the type is uninhabited. *)
  let rec decode ~meta ~offset ty : Typed.T.any Typed.t ParserMonad.t =
    let open ParserMonad in
    let open ParserMonad.Syntax in
    let iter fields offset =
      fold_iter fields ~init:[] ~f:(fun vs (ty, o) ->
          let+ v = decode ~meta ~offset:(offset +!!@ o) ty in
          v :: vs)
      |> map List.rev
    in
    let* ty = normalise ty in
    let* layout = layout_of ty in
    match (layout.fields, ty) with
    | _ when layout.uninhabited -> error (`RefToUninhabited ty)
    | _, TDynTrait _ -> not_impl "Tried reading a trait object?"
    | _, TAdt adt when Crate.is_union adt ->
        if%sat layout.size ==@ Usize.(0s) then ok (Typed.Adt.mk_union [])
        else
          (* FIXME: this isn't exactly correct; union actually doesn't copy the
             padding bytes (i.e. the intersection of the padding bytes of all
             fields). It is quite painful to actually calculate these padding
             bytes so we just copy the whole thing for now. See
             https://github.com/rust-lang/unsafe-code-guidelines/issues/518 And
             a proper implementation is here:
             https://github.com/minirust/minirust/blob/master/tooling/minimize/src/chunks.rs *)
          let+ blocks = get_all (Typed.cast_nonzero layout.size, offset) in
          Typed.Adt.mk_union blocks
    | Primitive, TFnDef _ -> ok (Typed.Adt.mk_tuple [])
    | Primitive, TVar (Free id) ->
        if%sat layout.size ==@ Usize.(0s) then ok (Typed.Adt.mk_poly id)
        else query (ty, offset)
    | Primitive, _ -> query (ty, offset)
    | Array { is_ptr = true; _ }, _ ->
        let+ vs = iter (iter_fields ?meta layout ty) offset in
        let ptr, meta =
          match vs with
          | [ ptr; meta ] -> (Typed.cast_ptr_t ptr, meta)
          | _ -> failwith "decode: unexpected values"
        in
        let meta =
          Option.get ~msg:"read invalid meta?" (Typed.Ptr.cast_meta meta)
        in
        Typed.Ptr.mk_ptr_f ptr (Some meta)
    | Array { is_ptr = false; _ }, _ ->
        let+ vs = iter (iter_fields ?meta layout ty) offset in
        Typed.Adt.mk_tuple vs
    | Arbitrary (variant, _), _ -> (
        let+ fields = iter (iter_fields ?meta layout ty) offset in
        match ty with
        (* HACK: we don't want enums to be handled in arbitrary. *)
        | TAdt adt when Crate.is_enum adt ->
            let variants = Crate.as_enum adt in
            let variant = Types.VariantId.nth variants variant in
            let discr = BV.of_literal variant.discriminant in
            Typed.Adt.mk_enum discr fields
        | _ -> Typed.Adt.mk_tuple fields)
    | Enum _, TAdt adt ->
        let variants = Crate.as_enum adt in
        let* variant = variant_of_enum ~offset ty in
        let+ fields = iter (iter_fields ~variant ?meta layout ty) offset in
        let variant = Types.VariantId.nth variants variant in
        let discr = BV.of_literal variant.discriminant in
        Typed.Adt.mk_enum discr fields
    | Enum _, _ -> failwith "decode: expected enum type for enum layout"
end

(** Given a value of type [Box], returns the container pointer *)
let ptr_of_box v =
  let box = Typed.cast_any_adt v in
  (* Unique<T> *)
  let unique = Typed.Adt.field_of box 0 in
  let unique = Typed.cast_any_adt unique in
  (* NonNull<T> *)
  let nonnull = Typed.Adt.field_of unique 0 in
  let nonnull = Typed.cast_any_adt nonnull in
  (* *mut T *)
  let ptr = Typed.Adt.field_of nonnull 0 in
  Typed.cast_ptr_f ptr

(** [encode ?offset v ty] Converts a [Rust_val.t] of type [ty] into an iterator
    over its sub values, along with their offset. Offsets all blocks by [offset]
    if specified *)
let rec encode ~offset (value : Typed.(T.any t)) (ty : Types.ty) :
    (Typed.(T.any t * T.sint t) Iter.t, 'e, 'f) Rustsymex.Result.t =
  let open Rustsymex in
  let open Syntax in
  let open Result in
  let chain fields iter =
    fields
    |> Iter.combine_list iter
    |> Result.fold_iter ~init:Iter.empty ~f:(fun acc ((ty, ofs), v) ->
        let offset = offset +!!@ ofs in
        let++ ys = encode ~offset v ty in
        Iter.append acc ys)
  in
  let** ty = Layout.normalise ty in
  let** layout = Layout.layout_of ty in
  if%sat layout.size ==@ Usize.(0s) then ok Iter.empty
  else
    match layout.fields with
    | Primitive -> ok (Iter.singleton (value, offset))
    | Arbitrary (_, _) ->
        let adt = ty_as_adt ty in
        let value = Typed.cast_adt adt value in
        if Crate.is_union adt then
          let blocks = Typed.Adt.as_union value in
          ok (Iter.of_list blocks |> Iter.map (fun (v, o) -> (v, offset +!!@ o)))
        else
          let fields = Typed.Adt.as_tuple value in
          chain fields (iter_fields layout ty)
    | Array { is_ptr = false } ->
        let value = Typed.cast_any_adt value in
        let fields = Typed.Adt.as_tuple value in
        chain fields (iter_fields layout ty)
    | Array { is_ptr = true } ->
        let value = Typed.cast_ptr_f value in
        let ptr, meta = Typed.Ptr.split value in
        let meta = Option.get meta in
        chain [ Typed.as_any ptr; Typed.as_any meta ] (iter_fields layout ty)
    | Enum (_, layouts) -> (
        let adt = ty_as_adt ty in
        let value = Typed.cast_adt adt value in
        let discr, fields = Typed.Adt.as_enum value in
        let* variant = variant_for_discr discr adt in
        let variant = variant.id in
        let++ fields = chain fields (iter_fields ~variant layout ty) in
        match fst layouts.(Types.VariantId.to_int variant) with
        | None -> fields
        | Some (ofs, tag) ->
            let offset = ofs +!!@ offset in
            Iter.cons ((tag :> Typed.T.any Typed.t), offset) fields)

(** Iterates over the validity constraints of this particular value for a given
    type, traversing it recursively. For every requirement, this associates to
    it the error to be raised if the requirement is not met.

    An optional [check_refs] function can be provided, to further check the
    validity of references and boxes; this allows checking that references are
    not dangling, well-aligned, and that their pointees are valid. Note that
    this check will raise errors in the outside monad, rather than in the
    returned list, since it is not possible to represent constraints in the
    state in first order logic. We provide this possibility here, to avoid
    re-implementing value traversal elsewhere.

    Note that this function doesn't (and can't) check basic validity
    requirements of references and boxes, even alignment, as e.g. for a
    [&dyn Trait] the alignment cannot be known without some auxiliary state.

    This doesn't check:
    - the fact the bytes of the value cannot be undefined, as that is checked
      when the memory is read; once we have a [Rust_val.t], we are guaranteed
      the data is initialised.
    - the validity of the discriminant of an enum, as that is checked when the
      memory is read (otherwise it would be impossible to even decode the value
      in the first place).

    Reference:
    https://doc.rust-lang.org/reference/behavior-considered-undefined.html#r-undefined.validity
*)
let rec validity ?(check_ref = fun _ _ -> Rustsymex.Result.ok ()) ty v f =
  (* We annotate each relevant helper function or match branch with the matching
     rule in the above-referenced document. *)
  let open Rustsymex in
  let open Syntax in
  let open Result in
  (* undefined.validity.wide *)
  let metadata_validity ~is_raw_ptr pointee
      (meta : Typed.T.ptr_meta Typed.t option) =
    let meta_k = Option.bind Typed.Ptr.meta_kind_of meta in
    match (meta, meta_k, Layout.dst_kind pointee) with
    | None, None, NoneKind -> ok ()
    | Some meta, Some `Len, LenKind ->
        let len = Typed.cast_i Usize meta in
        if is_raw_ptr then ok ()
        else f Usize.(0s <=$@ len) (`UBTransmute "Negative slice length")
    | Some vt, Some `VTable, VTableKind ->
        (* TODO: the vtable must always match the trait type of the pointee.
           Will require a new input to this function (?) *)
        let vt = Typed.cast_ptr vt in
        f
          (Typed.not (Typed.Ptr.is_null vt))
          (`UBTransmute "Null vtable pointer")
    | _, Some `Len, VTableKind ->
        f Typed.v_false (`UBTransmute "Length metadata when VTable expected")
    | _, Some `VTable, LenKind ->
        f Typed.v_false (`UBTransmute "VTable metadata when length expected")
    | _ ->
        f Typed.v_false (`UBTransmute "Mismatch between metadata and DST kind")
  in
  (* undefined.validity.reference-box *)
  let ref_box_validity (fptr : Typed.([< T.sptr_f ] t)) pointee =
    let meta = Typed.Ptr.meta_of fptr in
    let** () = metadata_validity ~is_raw_ptr:false pointee meta in
    let** layout = Layout.layout_of pointee in
    if layout.uninhabited then f Typed.v_false (`RefToUninhabited pointee)
    else check_ref fptr pointee
  in
  (* validity of pattern types (not stabilized) *)
  let rec pattern_valid_cond (inner_ty : Types.ty) (v : Typed.([< T.any ] t))
      (pat : Types.type_pattern) =
    match pat with
    | Range (start_expr, stop_expr) ->
        let ty = TypesUtils.ty_as_literal inner_ty in
        let v = Typed.cast_lit ty v in
        let signed = is_signed ty in
        let lo = BV.of_constant_expr start_expr in
        let hi = BV.of_constant_expr stop_expr in
        if signed then lo <=$@ v &&@ (v <=$@ hi) else lo <=@ v &&@ (v <=@ hi)
    | NotNull ->
        let ptr = Typed.cast_ptr_f v in
        let ptr = Typed.Ptr.ptr_of ptr in
        Typed.not (Typed.Ptr.is_null' ptr)
    | OrPattern pats ->
        List.fold_left
          (fun acc p -> acc ||@ pattern_valid_cond inner_ty v p)
          Typed.v_false pats
  in
  let** ty = Layout.normalise ty in
  match (ty : Types.ty) with
  (* undefined.validity.bool *)
  | TLiteral TBool ->
      let v = Typed.cast_i U8 v in
      f U8.(0s <=@ v &&@ (v <=@ 1s)) (`UBTransmute "Invalid bool value")
  (* undefined.validity.fn-pointer *)
  | TFnPtr _ ->
      let ptr, _ = Typed.Ptr.split @@ Typed.cast_ptr_f v in
      f (Typed.not (Typed.Ptr.is_null' ptr)) `UBDanglingPointer
  (* undefined.validity.char *)
  | TLiteral TChar ->
      let v = Typed.cast_lit TChar v in
      let is_surrogate = U32.(0xD800s <=@ v &&@ (v <=@ 0xDFFFs)) in
      let is_valid = U32.(v <=@ 0x10FFFFs) &&@ Typed.not is_surrogate in
      f is_valid (`UBTransmute "Invalid char value")
  (* undefined.validity.never *)
  | TNever -> f Typed.v_false (`RefToUninhabited ty)
  (* undefined.validity.scalar *)
  | TLiteral (TInt _ | TUInt _) -> ok ()
  | TLiteral (TFloat _) -> ok ()
  (* undefined.validity.str *)
  | TAdt { id = TBuiltin TStr; _ } -> ok ()
  (* undefined.validity.reference-box *)
  | TRef (_, pointee, _) ->
      let v = Typed.cast_ptr_f v in
      ref_box_validity v pointee
      (* NOTE: this check must go before the struct check, since boxes are
         structs *)
  | TAdt adt when adt_is_box adt ->
      let pointee = get_pointee ty in
      let ptr = ptr_of_box v in
      ref_box_validity ptr pointee
  (* undefined.validity.wide *)
  | TRawPtr (pointee, _) ->
      let ptr, meta = Typed.Ptr.split @@ Typed.cast_ptr_f v in
      metadata_validity ~is_raw_ptr:true pointee meta
  (* undefined.validity.enum *)
  | TAdt adt when Crate.is_enum adt ->
      let v = Typed.cast_adt adt v in
      let discr = Typed.Adt.discriminant_of v in
      let* variant = variant_for_discr discr adt in
      Iter.of_list (field_tys variant.fields)
      |> Iter.mapi (fun i ty -> (ty, Typed.Adt.field_of v i))
      |> iter_iter ~f:(fun (ty, v) -> validity ~check_ref ty v f)
  (* undefined.validity.struct *)
  | TAdt adt when Crate.is_struct adt ->
      let v = Typed.cast_adt adt v in
      Iter.of_list (Crate.as_struct_or_tuple adt)
      |> Iter.mapi (fun i ty -> (ty, Typed.Adt.field_of v i))
      |> iter_iter ~f:(fun (ty, v) -> validity ~check_ref ty v f)
  | TArray (ty, _) | TSlice ty ->
      let v = Typed.cast_any_adt v in
      let vs = Typed.Adt.as_tuple v in
      iter_list vs ~f:(fun v -> validity ~check_ref ty v f)
  (* undefined.validity.union *)
  | TAdt adt when Crate.is_union adt -> ok ()
  (* fndefs are ZSTs *)
  | TFnDef _ -> ok ()
  (* we assume polymorphic data has no validity requirement *)
  | TVar (Free _) -> ok ()
  (* undefined.validity.pattern-type *)
  | TPattern (inner_ty, pat) ->
      let** () = validity ~check_ref inner_ty v f in
      f
        (pattern_valid_cond inner_ty v pat)
        (`UBTransmute "Value violates pattern type constraint")
  (* we fail loudly to avoid missing cases *)
  | _ -> Fmt.failwith "validity: unhandled %a" pp_ty ty

(** Applies a validity check for a value, given a [check_ref] state monad
    operation. This assumes [check_ref] is effect-free: the returned state is
    the input state. See {!validity}. *)
let check_validity ~check_ref ty value st =
  let open Rustsymex in
  let open Syntax in
  (* we need to "unlift" [check_ref] *)
  let check_ref ptr ty : (unit, 'e, 'b) Rustsymex.Result.t =
    let open Rustsymex.Syntax in
    let+ res, _st = check_ref ptr ty st in
    res
  in
  (* and then lift the result *)
  let+ res = validity ~check_ref ty value assert_or_error in
  (res, st)

(** Cast between literals; perform validation of the type's constraints. This is
    different from a transmute! It doesn't simply reinterpret the bits, but
    rather converts between types, e.g. rounding, truncating, extending, etc.

    See also:
    https://doc.rust-lang.org/stable/reference/expressions/operator-expr.html#numeric-cast
*)
let cast_literal ~(from_ty : Types.literal_type) ~(to_ty : Types.literal_type)
    (v : Typed.([< T.cval ] t)) =
  match (from_ty, to_ty) with
  | _, _ when Types.equal_literal_type from_ty to_ty ->
      Typed.((v : [< T.cval ] t :> [> T.cval ] t))
  | TFloat fty, ((TInt _ | TUInt _) as lit_ty) ->
      let sv = Typed.cast_f fty v in
      let signed = Layout.is_signed lit_ty in
      let size = 8 * size_of_literal_ty lit_ty in
      BV.of_float ~rounding:Truncate ~signed ~size sv
  | (TInt _ | TUInt _), TFloat fp ->
      let sv = Typed.cast_lit from_ty v in
      let fp = Typed.float_precision fp in
      let signed = Layout.is_signed from_ty in
      BV.to_float ~rounding:NearestTiesToEven ~signed ~fp sv
  | TFloat _, _ | _, TFloat _ ->
      Fmt.failwith "Unhandled float transmute: %a -> %a" pp_literal_ty from_ty
        pp_literal_ty to_ty
  (* here we know we're only handling scalars: bool, char, or int/uint, so we
     can just resize the value as needed! *)
  | (TInt _ | TUInt _ | TBool | TChar), (TInt _ | TUInt _ | TBool | TChar) ->
      let from_bits = 8 * Layout.size_of_literal_ty from_ty in
      let from_signed = Layout.is_signed from_ty in
      let to_bits = 8 * Layout.size_of_literal_ty to_ty in
      let v = Typed.cast_lit from_ty v in
      if from_bits = to_bits then v
      else if from_bits < to_bits then
        BV.extend ~signed:from_signed (to_bits - from_bits) v
      else BV.extract 0 (to_bits - 1) v

(** Converts a floating value to a bitvector, preserving it's bit
    representation. This is a symbolic process, because SMT-Lib has no operation
    for "float->bv" that preserves the bits, due to NaN.

    See https://smt-lib.org/theories-FloatingPoint.shtml, "Conversions to other
    sorts" *)
let float_to_bv_bits (f : Typed.([< T.sfloat ] t)) :
    Typed.([> T.sint ] t) DecayMap.SM.t =
  let fp = Typed.Float.fp_of f in
  let size = Typed.FloatPrecision.size fp in
  let* bv = nondet (Typed.t_int size) in
  let bv_f = BV.to_float_raw bv in
  (* here we use structural equality rather than float equality; this is
     intended. *)
  let+ () = assume [ bv_f ==@ f ] in
  Typed.((bv : T.sint t :> [> T.sint ] t))

(** Transmutes a singular typed value, without splitting. This is under the
    assumption that [size_of to_ty = size_of v], and both are primitives
    (literal or pointer). *)
let rec transmute_one ~(to_ty : Types.ty) (v : [< Typed.T.any ] Typed.t) :
    [> Typed.T.any ] Typed.t DecayMap.SM.t =
  match%ty (v, to_ty) with
  (* Same-kind transmutes are the identity. *)
  | TBitVector _, TLiteral (TInt _ | TUInt _ | TBool | TChar) ->
      return (Typed.as_any v)
  | TFloat _, TLiteral (TFloat _) -> return (Typed.as_any v)
  | TExtension TFullPtr, (TRawPtr _ | TRef _ | TFnPtr _) ->
      return (Typed.as_any v)
  | TBitVector _, TLiteral (TFloat _) -> return (BV.to_float_raw v)
  | TExtension TFullPtr, TLiteral (TInt _ | TUInt _ | TBool | TChar) ->
      let ptr, _ = Typed.Ptr.split v in
      Sptr.decay ptr
  | TFloat _, TLiteral (TInt _ | TUInt _ | TBool | TChar) -> float_to_bv_bits v
  | TBitVector _, (TRawPtr _ | TRef _ | TFnPtr _) ->
      return (Typed.Ptr.mk_ptr_f (Sptr.of_address v) None)
  | _, TPattern (inner_ty, _) -> transmute_one ~to_ty:inner_ty v
  (* TODO: ????? *)
  (* | (PolyVal tid as v), TVar (Free type_var_id) ->
      if Types.TypeVarId.equal_id type_var_id tid then return v
      else
        Fmt.kstr not_impl "transmute_one: mismatched type variables %a -> %a"
          Types.pp_type_var_id type_var_id Types.pp_type_var_id tid *)
  | _, TVar (Bound _) ->
      failwith "transmute_one: bound type variable encountered?"
  | _, TVar _ ->
      Fmt.kstr not_impl
        "losing concrete value in %a -> %a; somewhere we lost track of generics"
        Typed.ppa v pp_ty to_ty
  | _, _ ->
      Fmt.kstr not_impl "transmute_one: unsupported %a -> %a" Typed.ppa v pp_ty
        to_ty

(** [nondet_raw ty] returns a nondeterministic value for [ty], by traversing
    [ty]: the returned value will have the right structure, and any required
    nondet variable will have been created. Importantly,
    {b the returned value may not uphold the validity invariant of [ty]}*. To
    ensure the value is also valid, use {!validity}, [assume]-ing the
    constraints it returns.

    * Much like in {!validity}, this function actually assumes two validity
    invariants: the data is initialised, and the discriminant of enums
    corresponds to that a variant. *)
let rec nondet_raw :
    Types.ty -> ([> Typed.T.any ] Typed.t, 'e, 'f) Rustsymex.Result.t =
  let open Rustsymex in
  let open Syntax in
  let open Compo_res in
  let nondets_raw tys = Rustsymex.Result.map_list tys ~f:nondet_raw in
  function
  | TLiteral (TFloat fp) ->
      let+ f = nondet (Typed.t_float fp) in
      Ok (f : Typed.(T.sfloat t) :> Typed.([> T.any ] t))
  | TLiteral lit ->
      let+ i = nondet (Typed.t_lit lit) in
      Ok (i : Typed.(T.sint t) :> Typed.([> T.any ] t))
  | (TRef (_, pointee, _) | TRawPtr (pointee, _))
    when not (Layout.is_dst pointee) ->
      let** { size; align; _ } = Layout.layout_of pointee in
      let+ ptr = nondet (Typed.t_ptr ()) in
      let ptr = Typed.Ptr.mk_ptr_t ~ptr ~size ~align ~tag:None in
      Ok (Typed.Ptr.mk_ptr_f ptr None)
  | TAdt { id = TTuple; generics = { types; _ } } ->
      let++ fields = nondets_raw types in
      Typed.Adt.mk_tuple fields
  | TArray (ty, len) ->
      let size = int_of_constant_expr len in
      let++ fields = nondets_raw @@ List.init size (fun _ -> ty) in
      Typed.Adt.mk_tuple fields
  | TAdt adt as ty -> (
      let type_decl = Crate.get_adt adt in
      match type_decl.kind with
      | Enum [] -> vanish ()
      | Enum (v :: _ as variants) ->
          let* discr = nondet (Typed.t_lit (lit_ty_of_lit v.discriminant)) in
          let* variant =
            match_on variants ~constr:(fun v ->
                BV.of_literal v.discriminant ==@ discr)
          in
          let* variant =
            match variant with Some v -> return v | None -> vanish ()
          in
          let++ fields = nondets_raw @@ field_tys variant.fields in
          Typed.Adt.mk_enum (BV.of_literal variant.discriminant) fields
      | Struct fields ->
          let++ fields = nondets_raw @@ field_tys fields in
          Typed.Adt.mk_tuple fields
      | Union _ ->
          let** size = Layout.size_of ty in
          let* sizei =
            match BV.to_z size with
            | Some s -> return (Z.to_int s)
            | None -> vanish ()
          in
          let+ bytes = nondet (Typed.t_int (sizei * 8)) in
          Ok (Typed.Adt.mk_union [ (bytes, Usize.(0s)) ])
      | ty ->
          Fmt.kstr Rustsymex.not_impl "nondet: unsupported type %a"
            Types.pp_type_decl_kind ty)
  | TPattern (inner, _) -> nondet_raw inner
  | TVar (Free id) -> Result.ok (Typed.Adt.mk_poly id)
  | ty -> Fmt.kstr Rustsymex.not_impl "nondet: unsupported type %a" pp_ty ty

(** Much like {!nondet_raw}, but also assumes validity invariants for the value,
    with {!validity}. Note this uses "stateless" validity: references are not
    checked to be e.g. non-dangling. *)
let nondet_valid ty =
  let open Rustsymex in
  let open Syntax in
  let** v = nondet_raw ty in
  let++ () =
    validity ty v (fun c _err ->
        let+ () = assume [ c ] in
        Compo_res.Ok ())
  in
  v

(** Traverses the given type and rust value, and returns all findable references
    with their type (ignores pointers, except if [include_ptrs] is true). This
    is needed e.g. when needing to get the pointers along with the size of their
    pointee, in particular in nested cases. *)
let rec ref_tys_in ?(include_ptrs = false) (_ : Typed.([< T.any ] t))
    (_ : Types.ty) : (Typed.([< T.sptr_f ] t) * Types.ty) list =
  (* FIXME: this is removed in a future PR *)
  []

(** Folds over all the references and boxes in the given value and type,
    applying [fn] to them. This is used to update nested references when
    reborrowing. Calls [fn] with the pointer value and the pointer type (not the
    pointee). *)
let rec ref_tys_in
    (fn :
      'acc ->
      Types.ty ->
      Typed.([< T.sptr_f ] t) ->
      (Typed.([> T.sptr_f ] t) * 'acc, 'e, 'f) Rustsymex.Result.t) (init : 'acc)
    (ty : Types.ty) (v : Typed.([< T.any ] t)) :
    (Typed.([> T.any ] t) * 'acc, 'e, 'f) Rustsymex.Result.t =
  let open Rustsymex in
  let open Syntax in
  let f = ref_tys_in fn in
  let fs acc ty vs =
    let++ vs, acc =
      Result.fold_list vs ~init:([], acc) ~f:(fun (vs, acc) v ->
          let++ v, acc = f acc ty v in
          (v :: vs, acc))
    in
    (List.rev vs, acc)
  in
  let fs' acc tys vs =
    let vs = List.combine tys vs in
    let++ vs, acc =
      Result.fold_list vs ~init:([], acc) ~f:(fun (vs, acc) (v, ty) ->
          let++ v, acc = f acc v ty in
          (v :: vs, acc))
    in
    (List.rev vs, acc)
  in
  match ty with
  | TRef (_, _, _) | TAdt { id = TBuiltin TBox; _ } ->
      let v = Typed.cast_ptr_f v in
      let++ res, acc = fn init ty v in
      (Typed.cast res, acc)
  | TAdt adt when adt_is_box adt ->
      (* a box has only one non ZST, the pointer *)
      let ptr, allocator, marker =
        match Typed.Adt.as_tuple @@ Typed.cast_any_adt v with
        | [ unique; allocator ] -> (
            match Typed.Adt.as_tuple @@ Typed.cast_any_adt unique with
            | [ nonnull; marker ] -> (
                match Typed.Adt.as_tuple @@ Typed.cast_any_adt nonnull with
                | [ ptr ] -> (Typed.cast_ptr_f ptr, allocator, marker)
                | _ -> failwith "wrong nonull shape")
            | _ -> failwith "wrong unique shape")
        | _ -> failwith "wrong box shape"
      in
      let++ ptr, acc = fn init ty ptr in
      let res =
        Typed.Adt.(
          mk_tuple
            [ mk_tuple [ mk_tuple [ Typed.cast ptr ]; marker ]; allocator ])
      in
      (res, acc)
  | TAdt adt when Crate.is_struct_or_tuple adt ->
      let v = Typed.cast_adt adt v in
      let++ vs, acc =
        fs' init (Crate.as_struct_or_tuple adt) (Typed.Adt.as_tuple v)
      in
      (Typed.Adt.mk_tuple vs, acc)
  | TArray (ty, _) | TSlice ty ->
      let v = Typed.cast_any_adt v in
      let++ vs, acc = fs init ty (Typed.Adt.as_tuple v) in
      (Typed.Adt.mk_tuple vs, acc)
  | TAdt adt when Crate.is_enum adt ->
      let v = Typed.cast_adt adt v in
      let d, vs = Typed.Adt.as_enum v in
      let* var = variant_for_discr d adt in
      let++ vs, acc = fs' init (field_tys Types.(var.fields)) vs in
      (Typed.Adt.mk_enum d vs, acc)
  | TPattern (inner, _) -> f init inner v
  | _ -> Result.ok (v, init)

(** Calculates the size and alignment of a type [t], according to the pointer
    metadata [meta]. Receives an arbitrary state and [load] function, to
    possibly access a heap to get VTable information. *)
let rec size_and_align_of_val ~load_vtable ~t ~meta =
  let open Rustsymex in
  let open Rustsymex.Syntax in
  (* Takes inspiration from rustc, to calculate the size and alignment of DSTs.
     https://github.com/rust-lang/rust/blob/a8664a1534913ccff491937ec2dc7ec5d973c2bd/compiler/rustc_codegen_ssa/src/size_of_val.rs *)
  if not (Layout.is_dst t) then
    let++ layout = Layout.layout_of t in
    (layout.size, layout.align)
  else
    match t with
    | TSlice _ | TAdt { id = TBuiltin TStr; _ } ->
        let sub_ty = Layout.dst_slice_ty t in
        let* sub_ty =
          of_opt_not_impl "size_of_val: missing a DST slice type" sub_ty
        in
        let** layout = Layout.layout_of sub_ty in
        let meta = Option.get ~msg:"size_of_val: missing slice meta" meta in
        let len = Typed.cast_i Usize meta in
        let size, ovf_mul = layout.size *?@ len in
        let++ () = assert_or_error (Typed.not ovf_mul) `Overflow in
        (size, layout.align)
    | TDynTrait _ ->
        let meta = Option.get ~msg:"size_of_val: missing trait meta" meta in
        let meta = Typed.cast_ptr_t meta in
        let** size = load_vtable `Size meta in
        let++ align = load_vtable `Align meta in
        let size = Typed.cast_i Usize size in
        let align = Typed.cast_i Usize align in
        (size, Typed.cast_nonzero align)
    | TAdt { id = TTuple | TAdtId _; _ } ->
        let field_tys =
          match t with
          | TAdt adt -> Crate.as_struct_or_tuple adt
          | _ -> failwith "impossible"
        in
        let last_field_ty = List.last field_tys in
        let** layout = Layout.layout_of t in
        let last_field_ofs =
          match layout.fields with
          | Arbitrary (_, offsets) -> offsets.(Array.length offsets - 1)
          | _ -> failwith "size_and_align_of_val: Unexpected layout for ADT"
        in
        let++ unsized_size, unsized_align =
          size_and_align_of_val ~load_vtable ~t:last_field_ty ~meta
        in
        (* TODO: we need to check if [layout] is packed, in which case
           unsized_align is 1! See 113-125 of above function. *)
        let align = BV.max ~signed:false unsized_align layout.align in
        let size = last_field_ofs +!!@ unsized_size in
        let size = Layout.size_to_fit ~size ~align in
        (size, align)
    | _ -> not_impl "size_and_align_of_val: Unexpected type"
