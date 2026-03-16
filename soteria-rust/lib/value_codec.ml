module Compo_res = Soteria.Symex.Compo_res
open Charon
open Typed.Syntax
open Typed.Infix
open Common.Charon_util
open Rust_val
open Layout
module DecayMapMonad = Sptr.DecayMapMonad
open DecayMapMonad
open DecayMapMonad.Syntax

(** Returns the variant id and variant matching the given discriminant. *)
let variant_for_discr discr adt =
  let open Rustsymex in
  let open Syntax in
  let variants = Crate.as_enum adt in
  let variants = List.mapi (fun i v -> (i, v)) variants in
  let* variant =
    match_on variants ~constr:(fun (_, v) ->
        BV.of_literal v.discriminant ==@ discr)
  in
  let+ i, variant =
    of_opt_not_impl "no matching variant for enum discriminant" variant
  in
  let vid = Types.VariantId.of_int i in
  (vid, variant)

(** Iterator over the fields and offsets of a type; for primitive types, returns
    a singleton iterator for that value. *)
let iter_fields ?variant ?(meta = Thin) layout (ty : Types.ty) =
  let aux ?variant fields =
    Iter.mapi (fun i ty -> (ty, Fields_shape.offset_of i fields))
    @@
    match ty with
    | TAdt { id = TTuple; generics = { types; _ } } -> Iter.of_list types
    | TArray (ty, len) -> Iter.repeatz (z_of_constant_expr len) ty
    | TSlice _ | TAdt { id = TBuiltin TStr; _ } -> (
        let sub_ty =
          match ty with TSlice ty -> ty | _ -> TLiteral (TUInt U8)
        in
        match meta with
        | Len len when Option.is_some (BV.to_z len) ->
            (* TODO: strings and slices of symbolic length *)
            Iter.repeatz (Option.get (BV.to_z len)) sub_ty
        | Thin | Len _ | VTable _ ->
            failwith "iter_fields: invalid length for slice/str")
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
    | TLiteral _ | TNever | TVar _ | TTraitType _ | TDynTrait _ | TFnPtr _
    | TFnDef _ | TPtrMetadata _ | TError _ ->
        Fmt.failwith "invalid iter_fields: %a" pp_ty ty
  in
  match layout.fields with
  | Primitive -> Iter.singleton (ty, Usize.(0s))
  | Array _ -> aux ?variant layout.fields
  | Arbitrary (variant, _) -> aux ~variant layout.fields
  | Enum (_, variant_layouts) ->
      let variant = Option.get ~msg:"variant required for enum" variant in
      let fields = variant_layouts.(Types.VariantId.to_int variant) in
      aux ~variant fields

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

module Decoder
    (Sptr : Sptr.S)
    (State_tys : sig
      module SM :
        Soteria.Sym_states.State_monad.S
          with type 'a Symex.t = 'a DecayMapMonad.t
           and type 'a Symex.Value.t = 'a Typed.t
           and type 'a Symex.Value.ty = 'a Typed.ty
           and type Symex.Value.sbool = Typed.sbool

      type fix
    end) =
struct
  type nonrec rust_val = Sptr.t Rust_val.t

  module ParserMonad = struct
    open State_tys

    type query = Types.ty * Typed.(T.sint t)
    type 'a res = ('a, Error.t, fix) SM.Result.t

    (* size * offset *)
    type get_all_query = Typed.(T.nonzero t) * Typed.(T.sint t)

    (* The following is just query -> (rust_val, 'err, 'fix) StateResult.t where
       StateResult = StateT (Result), but I need StateT1of3 urgh. *)
    type handler = query -> rust_val res

    type get_all_handler =
      get_all_query -> (rust_val * Typed.(T.sint t)) list res

    (* A parser monad is an object such that, given a query handler with state
       ['state], returns a state monad-ish for that state which may fail or
       branch *)
    type 'a t = handler -> get_all_handler -> 'a res

    let parse ~(handler : handler) ~(get_all : get_all_handler) scheduler :
        'a res =
      scheduler handler get_all

    let ok (x : 'a) : 'a t = fun _handler _get_all -> SM.Result.ok x
    let error (e : Error.t) : 'a t = fun _handler _get_all -> SM.Result.error e

    let bind2 (m : 'a t) (f : 'a -> 'b t) (fe : 'err -> 'b t) : 'b t =
     fun handler get_all ->
      let open SM.Syntax in
      let* res = m handler get_all in
      match res with
      | Compo_res.Ok x -> f x handler get_all
      | Compo_res.Error e -> fe e handler get_all
      | Compo_res.Missing f -> SM.Result.miss f

    let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
     fun handler get_all ->
      let open SM.Syntax in
      let** x = m handler get_all in
      f x handler get_all

    let map (m : 'a t) (f : 'a -> 'b) : 'b t =
     fun handler get_all ->
      let open SM.Syntax in
      let++ x = m handler get_all in
      f x

    let query (q : query) : 'a t = fun (handler : handler) _ -> handler q

    let get_all (q : get_all_query) : 'a t =
     fun _ get_all state -> get_all q state

    let[@inline] lift (m : 'a DecayMapMonad.t) : 'a t =
      let open SM.Syntax in
      fun _handler _get_all ->
        let*^ m in
        SM.Result.ok m

    let lift_rsymex (m : ('a, 'err, 'fix) Rustsymex.Result.t) : 'a t =
     fun _handler _get_all -> SM.lift (DecayMapMonad.lift m)

    let not_impl msg = lift @@ not_impl msg
    let of_opt_not_impl msg x = lift @@ of_opt_not_impl msg x
    let layout_of ty = lift_rsymex @@ Layout.layout_of ty
    let normalise ty = lift_rsymex @@ Layout.normalise ty

    let assert_or_error cond err =
     fun _handler _get_all state ->
      DecayMapMonad.Result.map (assert_or_error cond err) (fun () ->
          ((), state))

    let fold_iter x ~init ~f =
      Monad.foldM ~bind ~return:ok ~fold:Foldable.Iter.fold x ~init ~f

    module Syntax = struct
      let ( let* ) x f = bind x f
      let ( let+ ) x f = map x f
      let ( let*^ ) x f = bind (lift x) f

      module Symex_syntax = struct
        let branch_on ?left_branch_name ?right_branch_name guard ~then_ ~else_ =
         fun handler get_all state ->
          DecayMapMonad.branch_on ?left_branch_name ?right_branch_name guard
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
    let* layout = layout_of ty in
    match layout.fields with
    | Arbitrary (vid, _) -> ok vid
    | Enum (tag_layout, _) -> (
        let offset = offset +!!@ tag_layout.offset in
        let* tag = query (TLiteral tag_layout.ty, offset) in
        let tag = as_base tag_layout.ty tag in
        let tags = Array.to_seqi tag_layout.tags |> List.of_seq in
        let*^ res =
          match_on tags ~constr:(function
            | _, None -> Typed.v_false
            | _, Some t -> tag ==@ t)
        in
        match (tag_layout.encoding, res) with
        | _, Some (vid, _) -> ok (Types.VariantId.of_int vid)
        | Niche untagged, None -> ok untagged
        | Direct, None ->
            let adt = ty_as_adt ty in
            let adt = Crate.get_adt adt in
            let msg =
              Fmt.str "Unmatched discriminant for enum %a: %a" Crate.pp_name
                adt.item_meta.name Typed.ppa tag
            in
            error (`UBTransmute msg))
    | Array _ | Primitive -> failwith "Unexpected layout for enum"

  (** [decode ~meta ~offset ty] Parses a rust value of type [ty] at the given
      offset, using the provided metadata for DSTs, and returns the associated
      [Rust_val]. This does not perform any validity checking, aside from
      erroring if the type is uninhabited. *)
  let rec decode ~meta ~offset ty : rust_val ParserMonad.t =
    let open ParserMonad in
    let open ParserMonad.Syntax in
    let iter fields offset =
      fold_iter fields ~init:[] ~f:(fun vs (ty, o) ->
          let+ v = decode ~meta ~offset:(offset +!!@ o) ty in
          v :: vs)
      |> (Fun.flip map) (fun vs -> Tuple (List.rev vs))
    in
    let* ty = normalise ty in
    let* layout = layout_of ty in
    match (layout.fields, ty) with
    | _ when layout.uninhabited -> error (`RefToUninhabited ty)
    | _, TDynTrait _ -> not_impl "Tried reading a trait object?"
    | _, TAdt adt when Crate.is_union adt ->
        if%sat layout.size ==@ Usize.(0s) then ok (Union [])
        else
          (* FIXME: this isn't exactly correct; union actually doesn't copy the
             padding bytes (i.e. the intersection of the padding bytes of all
             fields). It is quite painful to actually calculate these padding
             bytes so we just copy the whole thing for now. See
             https://github.com/rust-lang/unsafe-code-guidelines/issues/518 And
             a proper implementation is here:
             https://github.com/minirust/minirust/blob/master/tooling/minimize/src/chunks.rs *)
          let+ blocks = get_all (Typed.cast layout.size, offset) in
          Union blocks
    | Primitive, TFnDef _ -> ok (Tuple [])
    | Primitive, TVar (Free id) ->
        if%sat layout.size ==@ Usize.(0s) then ok (PolyVal id)
        else query (ty, offset)
    | Primitive, _ -> query (ty, offset)
    | Array _, (TRawPtr (pointee, _) | TRef (_, pointee, _)) -> (
        let+ vs = iter (iter_fields ~meta layout ty) offset in
        let vs = as_tuple vs in
        match (dst_kind pointee, vs) with
        | LenKind, [ Ptr (base, Thin); Int len ] -> Ptr (base, Len len)
        | VTableKind, [ Ptr (base, Thin); Ptr (vtable, Thin) ] ->
            Ptr (base, VTable vtable)
        | _ -> failwith "decode: invalid metadata for pointer type")
    | Array _, _ -> iter (iter_fields ~meta layout ty) offset
    | Arbitrary (variant, _), _ -> (
        let+ vs = iter (iter_fields ~meta layout ty) offset in
        match ty with
        | TAdt adt when Crate.is_enum adt ->
            let variants = Crate.as_enum adt in
            let variant = Types.VariantId.nth variants variant in
            let fields = as_tuple vs in
            let discr = BV.of_literal variant.discriminant in
            Enum (discr, fields)
        | _ -> vs)
    | Enum _, TAdt adt ->
        let variants = Crate.as_enum adt in
        let* variant = variant_of_enum ~offset ty in
        let+ fields = iter (iter_fields ~variant ~meta layout ty) offset in
        let fields = as_tuple fields in
        let variant = Types.VariantId.nth variants variant in
        let discr = BV.of_literal variant.discriminant in
        Enum (discr, fields)
    | Enum _, _ -> failwith "decode: expected enum type for enum layout"
end

module Encoder (Sptr : Sptr.S) = struct
  type nonrec rust_val = Sptr.t Rust_val.t

  let pp_rust_val = Rust_val.pp Sptr.pp

  (** [encode ?offset v ty] Converts a [Rust_val.t] of type [ty] into an
      iterator over its sub values, along with their offset. Offsets all blocks
      by [offset] if specified *)
  let rec encode ~offset (value : rust_val) (ty : Types.ty) :
      ((rust_val * Typed.(T.sint t)) Iter.t, 'e, 'f) Rustsymex.Result.t =
    let open Rustsymex in
    let open Syntax in
    let open Result in
    let chain iter =
      (match value with
        | Tuple vals | Enum (_, vals) -> vals
        | Ptr (base, VTable vt) -> [ Ptr (base, Thin); Ptr (vt, Thin) ]
        | Ptr (base, Len len) -> [ Ptr (base, Thin); Int len ]
        | Ptr (_, Thin) | Int _ | Float _ | PolyVal _ ->
            failwith "Cannot split primitive"
        | Union _ -> failwith "Cannot encode union directly")
      |> Iter.combine_list iter
      |> Result.fold_iter ~init:(0, Iter.empty)
           ~f:(fun (i, acc) ((ty, ofs), v) ->
             let offset = offset +!!@ ofs in
             let++ ys = encode ~offset v ty in
             (i + 1, Iter.append acc ys))
      |> (Fun.flip Result.map) snd
    in
    let** ty = Layout.normalise ty in
    let** layout = Layout.layout_of ty in
    if%sat layout.size ==@ Usize.(0s) then ok Iter.empty
    else
      match (layout.fields, value) with
      | _, Union blocks ->
          ok (Iter.of_list blocks |> Iter.map (fun (v, o) -> (v, offset +!!@ o)))
      | Primitive, _ -> ok (Iter.singleton (value, offset))
      | Array _, _ | Arbitrary (_, _), _ -> chain (iter_fields layout ty)
      | Enum (tag_layout, _), Enum (disc, _) -> (
          let adt = ty_as_adt ty in
          let* variant, _ = variant_for_discr disc adt in
          let i = Types.VariantId.to_int variant in
          let++ fields = chain (iter_fields ~variant layout ty) in
          match tag_layout.tags.(i) with
          | None -> fields
          | Some tag ->
              let offset = tag_layout.offset +!!@ offset in
              Iter.cons (Int tag, offset) fields)
      | Enum _, _ ->
          Fmt.kstr not_impl "encode: expected enum value for enum type %a" pp_ty
            ty

  (** Iterates over the validity constraints of this particular value for a
      given type, traversing it recursively. For every requirement, this
      associates to it the error to be raised if the requirement is not met.

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
        memory is read (otherwise it would be impossible to even decode the
        value in the first place).

      Reference:
      https://doc.rust-lang.org/reference/behavior-considered-undefined.html#r-undefined.validity
  *)
  let rec validity ?(check_ref = fun _ _ -> Rustsymex.Result.ok ()) ty v f =
    (* We annotate each relevant helper function or match branch with the
       matching rule in the above-referenced document. *)
    let open Rustsymex in
    let open Syntax in
    let open Result in
    (* undefined.validity.wide *)
    let metadata_validity ~is_raw_ptr pointee meta =
      match (meta, Layout.dst_kind pointee) with
      | Thin, NoneKind -> ok ()
      | Len len, LenKind ->
          if is_raw_ptr then ok ()
          else f Usize.(0s <=$@ len) (`UBTransmute "Negative slice length")
      | VTable _, VTableKind ->
          (* TODO: the vtable must always match the trait type of the pointee.
             Will require a new input to this function (?) *)
          ok ()
      | Thin, (LenKind | VTableKind) ->
          f Typed.v_false (`UBTransmute "Thin pointer to DST")
      | (Len _ | VTable _), NoneKind ->
          f Typed.v_false (`UBTransmute "Metadata for non-DST pointee")
      | Len _, VTableKind ->
          f Typed.v_false (`UBTransmute "Length metadata when VTable expected")
      | VTable _, LenKind ->
          f Typed.v_false (`UBTransmute "VTable metadata when length expected")
    in
    (* undefined.validity.reference-box *)
    let ref_box_validity ((_, meta) as fptr) pointee =
      let** () = metadata_validity ~is_raw_ptr:false pointee meta in
      let** layout = Layout.layout_of pointee in
      if layout.uninhabited then f Typed.v_false (`RefToUninhabited pointee)
      else check_ref fptr pointee
    in
    let** ty = Layout.normalise ty in
    match (v, (ty : Types.ty)) with
    (* undefined.validity.bool *)
    | Int v, TLiteral TBool ->
        f U8.(0s <=@ v &&@ (v <=@ 1s)) (`UBTransmute "Invalid bool value")
    (* undefined.validity.fn-pointer *)
    | Ptr (p, _), TFnPtr _ -> f (Typed.not (Sptr.is_null p)) `UBDanglingPointer
    (* undefined.validity.char *)
    | Int v, TLiteral TChar ->
        let is_surrogate = U32.(0xD800s <=@ v &&@ (v <=@ 0xDFFFs)) in
        let is_valid = U32.(v <=@ 0x10FFFFs) &&@ Typed.not is_surrogate in
        f is_valid (`UBTransmute "Invalid char value")
    (* undefined.validity.never *)
    | _, TNever -> f Typed.v_false (`RefToUninhabited ty)
    (* undefined.validity.scalar *)
    | Int _, TLiteral (TInt _ | TUInt _) -> ok ()
    | Float _, TLiteral (TFloat _) -> ok ()
    (* undefined.validity.str *)
    | Tuple _, TAdt { id = TBuiltin TStr; _ } -> ok ()
    (* undefined.validity.enum *)
    | Enum (discr, vs), TAdt adt ->
        let* _, variant = variant_for_discr discr adt in
        List.combine (field_tys variant.fields) vs
        |> iter_list ~f:(fun (ty, v) -> validity ~check_ref ty v f)
    (* undefined.validity.struct *)
    | Tuple vs, TAdt adt ->
        List.combine (Crate.as_struct_or_tuple adt) vs
        |> iter_list ~f:(fun (ty, v) -> validity ~check_ref ty v f)
    | Tuple vs, (TArray (ty, _) | TSlice ty) ->
        iter_list vs ~f:(fun v -> validity ~check_ref ty v f)
    (* undefined.validity.union *)
    | Union _, TAdt _ -> ok ()
    (* undefined.validity.reference-box *)
    | Ptr ptr, TRef (_, pointee, _) -> ref_box_validity ptr pointee
    | _, TAdt adt when adt_is_box adt ->
        let pointee = get_pointee ty in
        let ptr, meta = as_ptr @@ List.hd @@ flatten v in
        ref_box_validity (ptr, meta) pointee
    (* undefined.validity.wide *)
    | Ptr (_, meta), TRawPtr (pointee, _) ->
        metadata_validity ~is_raw_ptr:true pointee meta
    (* fndefs are ZSTs *)
    | Tuple [], TFnDef _ -> ok ()
    (* we assume polymorphic data has no validity requirement *)
    | PolyVal _, TVar (Free _) -> ok ()
    (* we fail loudly to avoid missing cases *)
    | _ -> Fmt.failwith "validity: unhandled %a/%a" pp_rust_val v pp_ty ty

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

  (** Cast between literals; perform validation of the type's constraints. This
      is different from a transmute! It doesn't simply reinterpret the bits, but
      rather converts between types, e.g. rounding, truncating, extending, etc.

      See also:
      https://doc.rust-lang.org/stable/reference/expressions/operator-expr.html#numeric-cast
  *)
  let cast_literal ~(from_ty : Types.literal_type) ~(to_ty : Types.literal_type)
      (v : Typed.([< T.cval ] t)) =
    match (from_ty, to_ty) with
    | _, TFloat _ when from_ty = to_ty -> Float (Typed.cast v)
    | _, (TInt _ | TUInt _ | TBool | TChar) when from_ty = to_ty ->
        Int (Typed.cast v)
    | TFloat fty, ((TInt _ | TUInt _) as lit_ty) ->
        let sv = Typed.cast_f fty v in
        let signed = Layout.is_signed lit_ty in
        let size = 8 * size_of_literal_ty lit_ty in
        let sv' = BV.of_float ~rounding:Truncate ~signed ~size sv in
        Int sv'
    | (TInt _ | TUInt _), TFloat fp ->
        let sv = Typed.cast_lit from_ty v in
        let fp = float_precision fp in
        let signed = Layout.is_signed from_ty in
        let sv' = BV.to_float ~rounding:NearestTiesToEven ~signed ~fp sv in
        Float sv'
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

        if from_bits = to_bits then Int v
        else if from_bits < to_bits then
          Int (BV.extend ~signed:from_signed (to_bits - from_bits) v)
        else Int (BV.extract 0 (to_bits - 1) v)

  (** Converts a floating value to a bitvector, preserving it's bit
      representation. This is a symbolic process, because SMT-Lib has no
      operation for "float->bv" that preserves the bits, due to NaN.

      See https://smt-lib.org/theories-FloatingPoint.shtml, "Conversions to
      other sorts" *)
  let float_to_bv_bits (f : Typed.([< T.sfloat ] t)) :
      Typed.([> T.sint ] t) DecayMapMonad.t =
    let fp = Typed.Float.fp_of f in
    let size = Svalue.FloatPrecision.size fp in
    let* bv = nondet (Typed.t_int size) in
    let bv_f = BV.to_float_raw bv in
    (* here we use structural equality rather than float equality; this is
       intended. *)
    let+ () = assume [ bv_f ==@ f ] in
    bv

  (** Transmutes a singular rust value, without splitting. This is under the
      assumption that [size_of to_ty = size_of v], and both are primitives
      (literal or pointer). *)
  let transmute_one ~(to_ty : Types.ty) (v : rust_val) =
    match (to_ty, v) with
    | TLiteral (TFloat _), Float _ -> return v
    | TLiteral (TFloat _), Int v -> return (Float (BV.to_float_raw v))
    | TLiteral (TInt _ | TUInt _ | TBool | TChar), Int _ -> return v
    | TLiteral (TInt _ | TUInt _ | TBool | TChar), Ptr (p, Thin) ->
        let+ p = Sptr.decay p in
        Int p
    | TLiteral (TInt _ | TUInt _ | TBool | TChar), Float f ->
        let+ v = float_to_bv_bits f in
        Int v
    | (TRawPtr _ | TRef _ | TFnPtr _), Ptr (_, Thin) -> return v
    | (TRawPtr _ | TRef _ | TFnPtr _), Int v ->
        return (Ptr (Sptr.of_address v, Thin))
    | TVar (Free type_var_id), (PolyVal tid as v) ->
        if Types.TypeVarId.equal_id type_var_id tid then return v
        else
          Fmt.kstr not_impl "transmute_one: mismatched type variables %a -> %a"
            Types.pp_type_var_id type_var_id Types.pp_type_var_id tid
    | TVar (Bound _), _ ->
        failwith "transmute_one: bound type variable encountered?"
    | TVar _, _ ->
        Fmt.kstr not_impl
          "losing concrete value in %a -> %a; somewhere we lost track of \
           generics"
          pp_rust_val v pp_ty to_ty
    | _ ->
        Fmt.kstr not_impl "transmute_one: unsupported %a -> %a" pp_rust_val v
          pp_ty to_ty

  (** [nondet_raw ty] returns a nondeterministic value for [ty], by traversing
      [ty]: the returned value will have the right structure, and any required
      nondet variable will have been created. Importantly,
      {b the returned value may not uphold the validity invariant of [ty]}*. To
      ensure the value is also valid, use {!validity}, [assume]-ing the
      constraints it returns.

      * Much like in {!validity}, this function actually assumes two validity
      invariants: the data is initialised, and the discriminant of enums
      corresponds to that a variant. *)
  let rec nondet_raw : Types.ty -> (rust_val, 'e, 'f) Rustsymex.Result.t =
    let open Rustsymex in
    let open Syntax in
    let open Soteria.Symex.Compo_res in
    let nondets_raw tys = Rustsymex.Result.map_list tys ~f:nondet_raw in
    function
    | TLiteral (TFloat fp) ->
        let+ f = nondet (Typed.t_float fp) in
        Ok (Float f)
    | TLiteral lit ->
        let+ i = nondet (Typed.t_lit lit) in
        Ok (Int (Typed.cast i))
    | (TRef (_, pointee, _) | TRawPtr (pointee, _))
      when not (Layout.is_dst pointee) ->
        let++ p = Sptr.nondet pointee in
        Ptr (p, Thin)
    | TAdt { id = TTuple; generics = { types; _ } } ->
        let++ fields = nondets_raw types in
        Tuple fields
    | TArray (ty, len) ->
        let size = int_of_constant_expr len in
        let++ fields = nondets_raw @@ List.init size (fun _ -> ty) in
        Tuple fields
    | TAdt adt as ty -> (
        let type_decl = Crate.get_adt adt in
        match type_decl.kind with
        | Enum [] -> vanish ()
        | Enum (v :: _ as variants) ->
            let* discr = nondet (Typed.t_lit (lit_ty_of_lit v.discriminant)) in
            let discr : Typed.(T.sint t) = Typed.cast discr in
            let* variant =
              match_on variants ~constr:(fun v ->
                  BV.of_literal v.discriminant ==@ discr)
            in
            let* variant =
              match variant with Some v -> return v | None -> vanish ()
            in
            let++ fields = nondets_raw @@ field_tys variant.fields in
            Enum (BV.of_literal variant.discriminant, fields)
        | Struct fields ->
            let++ fields = nondets_raw @@ field_tys fields in
            Tuple fields
        | Union _ ->
            let** size = Layout.size_of ty in
            let* sizei =
              match BV.to_z size with
              | Some s -> return (Z.to_int s)
              | None -> vanish ()
            in
            let+ bytes = nondet (Typed.t_int (sizei * 8)) in
            Ok (Union [ (Int bytes, size) ])
        | ty ->
            Fmt.kstr Rustsymex.not_impl "nondet: unsupported type %a"
              Types.pp_type_decl_kind ty)
    | TVar (Free id) -> Result.ok (PolyVal id)
    | ty -> Fmt.kstr Rustsymex.not_impl "nondet: unsupported type %a" pp_ty ty

  (** Much like {!nondet_raw}, but also assumes validity invariants for the
      value, with {!validity}. Note this uses "stateless" validity: references
      are not checked to be e.g. non-dangling. *)
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

  (** Apply the compiler-attribute to the given value *)
  let apply_attribute v attr =
    let open Rustsymex in
    let open Syntax in
    match (v, attr) with
    | ( Int v,
        Charon.Meta.AttrUnknown
          { path = "rustc_layout_scalar_valid_range_start"; args = Some min } )
      ->
        let min = Z.of_string min in
        let bits = Typed.size_of_int v in
        if%sat v >=@ BV.mk bits min then Result.ok ()
        else Result.error (`StdErr "rustc_layout_scalar_valid_range_start")
    | ( Int v,
        AttrUnknown
          { path = "rustc_layout_scalar_valid_range_end"; args = Some max_s } )
      ->
        let max = Z.of_string max_s in
        let bits = Typed.size_of_int v in
        if%sat v <=@ BV.mk bits max then Result.ok ()
        else Result.error (`StdErr "rustc_layout_scalar_valid_range_end")
    | _ -> Result.ok ()

  let apply_attributes v attributes =
    Rustsymex.Result.iter_list attributes ~f:(apply_attribute v)

  (** Traverses the given type and rust value, and returns all findable
      references with their type (ignores pointers, except if [include_ptrs] is
      true). This is needed e.g. when needing to get the pointers along with the
      size of their pointee, in particular in nested cases. *)
  let rec ref_tys_in ?(include_ptrs = false) (v : rust_val) (ty : Types.ty) :
      ('a full_ptr * Types.ty) list =
    let f = ref_tys_in ~include_ptrs in
    match (v, ty) with
    | Ptr ptr, (TAdt { id = TBuiltin TBox; _ } | TRef _) ->
        [ (ptr, get_pointee ty) ]
    | Ptr ptr, TRawPtr _ when include_ptrs -> [ (ptr, get_pointee ty) ]
    | (Int _ | Float _), _ -> []
    | Tuple vs, TAdt adt -> List.concat_map2 f vs (Crate.as_struct_or_tuple adt)
    | Tuple vs, (TArray (ty, _) | TSlice ty) ->
        List.concat_map (fun v -> f v ty) vs
    | Enum (d, vs), TAdt adt -> (
        match BV.to_z d with
        | Some d -> (
            let variants = Crate.as_enum adt in
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
        (* FIXME: figure out if references inside unions get reborrowed. They
           could, but I suspect they don't because there's no guarantee the
           reference isn't some other field, e.g. in [union { a: &u8, b: &u16
           }] *)
        []
    | _ -> []

  (** Folds over all the references and boxes in the given value and type,
      applying [fn] to them. This is used to update nested references when
      reborrowing. Calls [fn] with the pointer value and the pointer type (not
      the pointee). *)
  let rec update_ref_tys_in
      (fn :
        'acc ->
        'a full_ptr ->
        Types.ty ->
        ('a full_ptr * 'acc, 'e, 'f) Rustsymex.Result.t) (init : 'acc)
      (v : rust_val) (ty : Types.ty) :
      (rust_val * 'acc, 'e, 'f) Rustsymex.Result.t =
    let open Rustsymex in
    let open Syntax in
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
    | Ptr ptr, TRef (_, _, _) ->
        let++ ptr, acc = fn init ptr ty in
        (Ptr ptr, acc)
    | Tuple _, TAdt adt when adt_is_box adt ->
        (* a box has only one non ZST, the pointer *)
        let ptr = as_ptr @@ List.hd @@ flatten v in
        let++ ptr, acc = fn init ptr ty in
        (* recursively look for where the pointer is and replace it *)
        let rec subst_ptr = function
          | Tuple vs -> Tuple (List.map subst_ptr vs)
          | Ptr _ -> Ptr ptr
          | v -> v
        in
        (subst_ptr v, acc)
    | Tuple vs, TAdt adt ->
        let++ vs, acc = fs2 init vs (Crate.as_struct_or_tuple adt) in
        (Tuple vs, acc)
    | Tuple vs, (TArray (ty, _) | TSlice ty) ->
        let++ vs, acc = fs init vs ty in
        (Tuple vs, acc)
    | Enum (d, vs), TAdt adt -> (
        let variants = Crate.as_enum adt in
        let* var =
          match_on variants ~constr:(fun v ->
              BV.of_literal v.discriminant ==@ d)
        in
        match var with
        | Some var ->
            let++ vs, acc = fs2 init vs (field_tys Types.(var.fields)) in
            (Enum (d, vs), acc)
        | None -> Result.ok (v, init))
    | (Union _ as v), TAdt { id = TAdtId _; _ } ->
        (* FIXME: figure out if references inside unions get reborrowed. They
           could, but I suspect they don't because there's no guarantee the
           reference isn't some other field, e.g. in [union { a: &u8, b: &u16
           }] *)
        Result.ok (v, init)
    | v, _ -> Result.ok (v, init)

  (** Calculates the size and alignment of a type [t], according to the pointer
      metadata [meta]. Receives an arbitrary state and [load] function, to
      possibly access a heap to get VTable information. *)
  let rec size_and_align_of_val ~load ~t ~meta st =
    let open Rustsymex in
    let open Rustsymex.Syntax in
    (* Takes inspiration from rustc, to calculate the size and alignment of
       DSTs.
       https://github.com/rust-lang/rust/blob/a8664a1534913ccff491937ec2dc7ec5d973c2bd/compiler/rustc_codegen_ssa/src/size_of_val.rs *)
    if not (Layout.is_dst t) then
      let++ layout = Layout.layout_of t in
      (layout.size, layout.align)
    else
      match (t, meta) with
      | (TSlice _ | TAdt { id = TBuiltin TStr; _ }), (Thin | VTable _) ->
          failwith "size_and_align_of_val: Invalid metadata for slice type"
      | (TSlice _ | TAdt { id = TBuiltin TStr; _ }), Len meta ->
          let sub_ty = Layout.dst_slice_ty t in
          let* sub_ty =
            of_opt_not_impl "size_of_val: missing a DST slice type" sub_ty
          in
          let** layout = Layout.layout_of sub_ty in
          let len = Typed.cast_i Usize meta in
          let size, ovf_mul = layout.size *?@ len in
          let++ () = assert_or_error (Typed.not ovf_mul) `Overflow in
          (size, layout.align)
      | TDynTrait _, (Thin | Len _) ->
          failwith "size_and_align_of_val: Invalid metadata for dyn type"
      | TDynTrait _, VTable vtable ->
          let usize = Types.TLiteral (TUInt Usize) in
          let** size_ptr =
            Sptr.offset ~signed:true ~ty:usize vtable Usize.(1s)
          in
          let** align_ptr =
            Sptr.offset ~signed:true ~ty:usize vtable Usize.(2s)
          in
          let* size, _ = load (size_ptr, Thin) usize st in
          let** size = return size in
          let* align, _ = load (align_ptr, Thin) usize st in
          let++ align = return align in
          let size = as_base_i Usize size in
          let align = as_base_i Usize align in
          (size, Typed.cast align)
      | TAdt { id = TTuple | TAdtId _; _ }, _ ->
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
            size_and_align_of_val ~load ~t:last_field_ty ~meta st
          in
          (* TODO: we need to check if [layout] is packed, in which case
             unsized_align is 1! See 113-125 of above function. *)
          let align = BV.max ~signed:false unsized_align layout.align in
          let size = last_field_ofs +!!@ unsized_size in
          let size = Layout.size_to_fit ~size ~align in
          (size, align)
      | _ -> not_impl "size_and_align_of_val: Unexpected type"
end
