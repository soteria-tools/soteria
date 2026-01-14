module Compo_res = Soteria.Symex.Compo_res
open Charon
module BV = Typed.BitVec
open Typed.Syntax
open Typed.Infix
open Charon_util
open Rust_val
open Layout
module DecayMapMonad = Sptr.DecayMapMonad
open DecayMapMonad
open DecayMapMonad.Syntax

module Make (Sptr : Sptr.S) = struct
  type nonrec rust_val = Sptr.t Rust_val.t

  let pp_rust_val = Rust_val.pp Sptr.pp

  module ParserMonad = struct
    type query = Types.ty * T.sint Typed.t

    (* size * offset  *)
    type get_all_query = T.nonzero Typed.t * T.sint Typed.t

    (* The following is just query -> (rust_val, 'err, 'fix) StateResult.t
         where StateResult = StateT (Result), but I need StateT1of3 urgh. *)
    type ('state, 'err, 'fix) handler =
      query -> 'state -> (rust_val * 'state, 'err, 'fix) Result.t

    type ('state, 'err, 'fix) get_all_handler =
      get_all_query ->
      'state ->
      ((rust_val * T.sint Typed.t) list * 'state, 'err, 'fix) Result.t

    (* A parser monad is an object such that, given a query handler with state ['state],
      returns a state monad-ish for that state which may fail or branch *)
    type ('res, 'state, 'err, 'fix) t =
      ('state, 'err, 'fix) handler ->
      ('state, 'err, 'fix) get_all_handler ->
      'state ->
      ('res * 'state, 'err, 'fix) Result.t

    let parse ~(init : 'state) ~(handler : ('state, 'err, 'fix) handler)
        ~(get_all : ('state, 'err, 'fix) get_all_handler) scheduler :
        ('res * 'state, 'err, 'fix) Result.t =
      scheduler handler get_all init

    let ok (x : 'a) : ('a, 'state, 'err, 'fix) t =
     fun _handler _get_all state -> Result.ok (x, state)

    let error (e : 'err) : ('a, 'state, 'err, 'fix) t =
     fun _handler _get_all _state -> Result.error e

    let bind2 (m : ('a, 'state, 'err, 'fix) t)
        (f : 'a -> ('b, 'state, 'err, 'fix) t)
        (fe : 'err -> ('b, 'state, 'err, 'fix) t) : ('b, 'state, 'err, 'fix) t =
     fun handler get_all state ->
      let* res = m handler get_all state in
      match res with
      | Compo_res.Ok (x, new_state) -> f x handler get_all new_state
      | Compo_res.Error e -> fe e handler get_all state
      | Compo_res.Missing f -> Result.miss f

    let bind (m : ('a, 'state, 'err, 'fix) t)
        (f : 'a -> ('b, 'state, 'err, 'fix) t) : ('b, 'state, 'err, 'fix) t =
     fun handler get_all state ->
      let** x, new_state = m handler get_all state in
      f x handler get_all new_state

    let map (m : ('a, 'state, 'err, 'fix) t) (f : 'a -> 'b) :
        ('b, 'state, 'err, 'fix) t =
     fun handler get_all state ->
      let++ x, new_state = m handler get_all state in
      (f x, new_state)

    let query (q : query) : ('a, 'state, 'err, 'fix) t =
     fun handler _ state -> handler q state

    let get_all (q : get_all_query) : ('a, 'state, 'err, 'fix) t =
     fun _ get_all state -> get_all q state

    let[@inline] lift (m : 'a DecayMapMonad.t) : ('a, 'state, 'err, 'fix) t =
     fun _handler _get_all state ->
      let+ m in
      Compo_res.Ok (m, state)

    let lift_rsymex (m : ('a, 'err, 'fix) Rustsymex.Result.t) :
        ('a, 'state, 'err, 'fix) t =
     fun _handler _get_all state ->
      let++ m = DecayMapMonad.lift m in
      (m, state)

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

      module Symex_syntax = struct
        let branch_on ?left_branch_name ?right_branch_name guard ~then_ ~else_ =
         fun handler get_all state ->
          DecayMapMonad.branch_on ?left_branch_name ?right_branch_name guard
            ~then_:(fun () -> then_ () handler get_all state)
            ~else_:(fun () -> else_ () handler get_all state)
      end
    end
  end

  (** Iterator over the fields and offsets of a type; for primitive types,
      returns a singleton iterator for that value. *)
  let iter_fields ?variant ?(meta = Thin) layout (ty : Types.ty) =
    let aux ?variant fields =
      Iter.mapi (fun i ty -> (ty, Fields_shape.offset_of i fields))
      @@
      match ty with
      | TAdt { id = TTuple; generics = { types; _ } } -> Iter.of_list types
      | TArray (ty, len) -> Iter.repeatz (z_of_const_generic len) ty
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
      | _ -> Fmt.failwith "invalid iter_fields: %a" pp_ty ty
    in
    match layout.fields with
    | Primitive -> Iter.singleton (ty, Usize.(0s))
    | Array _ -> aux ?variant layout.fields
    | Arbitrary (variant, _) -> aux ~variant layout.fields
    | Enum (_, variant_layouts) ->
        let variant = Option.get ~msg:"variant required for enum" variant in
        let fields = variant_layouts.(Types.VariantId.to_int variant) in
        aux ~variant fields

  (** [encode ?offset v ty] Converts a [Rust_val.t] of type [ty] into an
      iterator over its sub values, along with their offset. Offsets all blocks
      by [offset] if specified *)
  let rec encode ~offset (value : rust_val) (ty : Types.ty) :
      ((rust_val * T.sint Typed.t) Iter.t, 'e, 'f) Rustsymex.Result.t =
    let open Rustsymex in
    let open Syntax in
    let open Result in
    let chain iter =
      (match value with
      | Tuple vals | Enum (_, vals) -> vals
      | Ptr (base, VTable vt) -> [ Ptr (base, Thin); Ptr (vt, Thin) ]
      | Ptr (base, Len len) -> [ Ptr (base, Thin); Int len ]
      | Ptr (_, Thin) | Int _ | Float _ | TypeVar _ ->
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
          let adt = Charon_util.ty_as_adt ty in
          let variants = Crate.as_enum adt in
          let variants = List.mapi (fun i v -> (i, v)) variants in
          let* variant =
            match_on variants ~constr:(fun (_, v) ->
                BV.of_literal v.discriminant ==@ disc)
          in
          let* i, _ =
            of_opt_not_impl "no matching variant for enum discriminant" variant
          in
          let variant = Types.VariantId.of_int i in
          let++ fields = chain (iter_fields ~variant layout ty) in
          match tag_layout.tags.(i) with
          | None -> fields
          | Some tag ->
              let offset = tag_layout.offset +!!@ offset in
              Iter.cons (Int tag, offset) fields)
      | Enum _, _ ->
          Fmt.kstr not_impl "encode: expected enum value for enum type %a" pp_ty
            ty

  (** Parses the current variant of the enum at the given offset. This handles
      cases such as niches, where the discriminant isn't directly encoded as a
      tag. *)
  let variant_of_enum ~offset ty :
      (Types.variant_id, 'state, 'e, 'fix) ParserMonad.t =
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
        let* res =
          lift
          @@ match_on tags ~constr:(function
               | _, None -> Typed.v_false
               | _, Some t -> tag ==@ t)
        in
        match (tag_layout.encoding, res) with
        | _, Some (vid, _) -> ok (Types.VariantId.of_int vid)
        | Direct, None ->
            let adt = Charon_util.ty_as_adt ty in
            let adt = Crate.get_adt adt in
            let msg =
              Fmt.str "Unmatched discriminant for enum %a: %a" Crate.pp_name
                adt.item_meta.name Typed.ppa tag
            in
            error (`UBTransmute msg)
        | Niche untagged, None -> ok untagged)
    | Array _ | Primitive -> failwith "Unexpected layout for enum"

  (** [decode ~meta ~offset ty] Parses a rust value of type [ty] at the given
      offset, using the provided metadata for DSTs, and returns the associated
      [Rust_val]. This does not perform any validity checking, aside from
      erroring if the type is uninhabited. *)
  let rec decode ~meta ~offset ty : (rust_val, 'state, 'e, 'fix) ParserMonad.t =
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
    | _ when layout.uninhabited -> error `RefToUninhabited
    | _, TDynTrait _ -> not_impl "Tried reading a trait object?"
    | _, TAdt adt when Crate.is_union adt ->
        if%sat layout.size ==@ Usize.(0s) then ok (Union [])
        else
          (* FIXME: this isn't exactly correct; union actually doesn't copy the
             padding bytes (i.e. the intersection of the padding bytes of all
             fields). It is quite painful to actually calculate these padding
             bytes so we just copy the whole thing for now.
             See https://github.com/rust-lang/unsafe-code-guidelines/issues/518
             And a proper implementation is here:
             https://github.com/minirust/minirust/blob/master/tooling/minimize/src/chunks.rs *)
          let+ blocks = get_all (Typed.cast layout.size, offset) in
          Union blocks
    | Primitive, TFnDef _ -> ok (Tuple [])
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

  (** Ensures this value is valid for the given type. This includes checking
      pointer metadata, e.g. slice lengths and vtables. The [fake_read] function
      is used to simulate reading from memory to check the validity of a pointee
      type. *)
  let check_valid ~fake_read v ty st =
    let open Rustsymex in
    let open Syntax in
    let open Result in
    match (v, (ty : Types.ty)) with
    | Ptr ((_, meta) as p), TRef (_, pointee, _) -> (
        let** () =
          match meta with
          | Thin -> ok ()
          | Len len ->
              assert_or_error
                (Usize.(0s) <=$@ len)
                (`UBTransmute "Negative slice length")
          | VTable _ ->
              (* TODO: check the vtable pointer is of the right trait kind *)
              ok ()
        in
        let** layout = Layout.layout_of pointee in
        if layout.uninhabited then error `RefToUninhabited
        else
          let* opt_err, st = fake_read p pointee st in
          match opt_err with Some err -> error err | None -> ok st)
    | Ptr (p, _), TFnPtr _ ->
        let++ () =
          assert_or_error
            (Typed.not (Sptr.sem_eq (Sptr.null_ptr ()) p))
            `UBDanglingPointer
        in
        st
    | _ -> ok st

  let with_constraints ~ty v =
    let constraints = Typed.conj @@ Layout.constraints ty v in
    let msg = Fmt.str "Constraints of %a unsatisfied" pp_literal_ty ty in
    let++ () = assert_or_error constraints (`UBTransmute msg) in
    Int v

  (** Cast between literals; perform validation of the type's constraints. This
      is different from a transmute! It doesn't simply reinterpret the bits, but
      rather converts between types, e.g. rounding, truncating, extending, etc.

      See also:
      https://doc.rust-lang.org/stable/reference/expressions/operator-expr.html#numeric-cast
  *)
  let cast_literal ~(from_ty : Types.literal_type) ~(to_ty : Types.literal_type)
      (v : [< T.cval ] Typed.t) =
    let open DecayMapMonad.Result in
    match (from_ty, to_ty) with
    | _, TFloat _ when from_ty = to_ty -> ok (Float (Typed.cast v))
    | _, (TInt _ | TUInt _ | TBool | TChar) when from_ty = to_ty ->
        ok (Int (Typed.cast v))
    | TFloat fty, ((TInt _ | TUInt _) as lit_ty) ->
        let sv = Typed.cast_f fty v in
        let signed = Layout.is_signed lit_ty in
        let size = 8 * size_of_literal_ty lit_ty in
        let sv' = BV.of_float ~rounding:Truncate ~signed ~size sv in
        Result.ok (Int sv')
    | (TInt _ | TUInt _), TFloat fp ->
        let sv = Typed.cast_lit from_ty v in
        let fp = Charon_util.float_precision fp in
        let signed = Layout.is_signed from_ty in
        let sv' = BV.to_float ~rounding:NearestTiesToEven ~signed ~fp sv in
        Result.ok (Float sv')
    | TFloat _, _ | _, TFloat _ ->
        Fmt.kstr not_impl "Unhandled float transmute: %a -> %a" pp_literal_ty
          from_ty pp_literal_ty to_ty
    (* here we know we're only handling scalars: bool, char, or int/uint, so we can just
       resize the value as needed! *)
    | _ ->
        let from_bits = 8 * Layout.size_of_literal_ty from_ty in
        let from_signed = Layout.is_signed from_ty in
        let to_bits = 8 * Layout.size_of_literal_ty to_ty in
        let v = Typed.cast_lit from_ty v in
        let v =
          if from_bits = to_bits then v
          else if from_bits < to_bits then
            BV.extend ~signed:from_signed (to_bits - from_bits) v
          else BV.extract 0 (to_bits - 1) v
        in
        with_constraints ~ty:to_ty v

  (** Converts a floating value to a bitvector, preserving it's bit
      representation. This is a symbolic process, because SMT-Lib has no
      operation for "float->bv" that preserves the bits, due to NaN.

      See https://smt-lib.org/theories-FloatingPoint.shtml, "Conversions to
      other sorts" *)
  let float_to_bv_bits (f : [< T.sfloat ] Typed.t) :
      [> T.sint ] Typed.t DecayMapMonad.t =
    let fp = Typed.Float.fp_of f in
    let size = Svalue.FloatPrecision.size fp in
    let* bv = nondet (Typed.t_int size) in
    let bv_f = BV.to_float_raw bv in
    (* here we use structural equality rather than float equality; this is intended. *)
    let+ () = assume [ bv_f ==@ f ] in
    bv

  (** Transmutes a singular rust value, without splitting. This is under the
      assumption that [size_of to_ty = size_of v], and both are primitives
      (literal or pointer). *)
  let transmute_one ~(to_ty : Types.ty) (v : rust_val) =
    let open DecayMapMonad.Result in
    match (to_ty, v) with
    | TLiteral (TFloat _), Float _ -> ok v
    | TLiteral (TFloat _), Int v -> ok (Float (BV.to_float_raw v))
    | TLiteral ((TInt _ | TUInt _ | TBool | TChar) as ty), Int v ->
        with_constraints ~ty v
    | TLiteral ((TInt _ | TUInt _ | TBool | TChar) as ty), Ptr (p, Thin) ->
        let* p = Sptr.decay p in
        with_constraints ~ty p
    | TLiteral ((TInt _ | TUInt _ | TBool | TChar) as ty), Float f ->
        let* v = float_to_bv_bits f in
        with_constraints ~ty v
    | (TRawPtr _ | TRef _ | TFnPtr _), Ptr (_, Thin) -> ok v
    | TRef _, Int _ -> error `UBDanglingPointer
    | TFnPtr _, Int v ->
        if%sat v ==@ Usize.(0s) then error `UBDanglingPointer
        else ok (Ptr (Sptr.null_ptr_of v, Thin))
    | TRawPtr _, Int v -> ok (Ptr (Sptr.null_ptr_of v, Thin))
    | TVar (Free type_var_id), (TypeVar (tid, _) as v) ->
        let type_var_id = Types.TypeVarId.to_int type_var_id in
        if type_var_id = tid then ok v
        else
          Fmt.kstr not_impl "transmute_one: mismatched type variables %d -> %d"
            type_var_id tid
    | TVar (Bound _), _ ->
        not_impl "transmute_one: cannot handle bound type variables"
    | TVar _, _ ->
        Fmt.kstr not_impl
          "losing concrete value in %a -> %a; somewhere we lost track of \
           generics"
          pp_rust_val v pp_ty to_ty
    | _ ->
        Fmt.kstr not_impl "transmute_one: unsupported %a -> %a" pp_rust_val v
          pp_ty to_ty

  (** [nondet ~extern ~init ty] returns a nondeterministic value for [ty], along
      with some "state". It receives a function [extern] to get an optional
      external function that computes the arbitrary value; it tries using it,
      and otherwise guesses the valid values. [init] is the initial "state",
      that is modified and returned by [extern]. *)
  let rec nondet : Types.ty -> (rust_val, 'e, 'f) Rustsymex.Result.t =
    let open Rustsymex in
    let open Syntax in
    let open Soteria.Symex.Compo_res in
    function
    | TLiteral (TFloat _ as lit) ->
        let+ f = Layout.nondet_literal_ty lit in
        Ok (Float (Typed.cast f))
    | TLiteral lit ->
        let+ i = Layout.nondet_literal_ty lit in
        Ok (Int (Typed.cast i))
    | TAdt { id = TTuple; generics = { types; _ } } ->
        let++ fields = nondets types in
        Tuple fields
    | TArray (ty, len) ->
        let size = Charon_util.int_of_const_generic len in
        let++ fields = nondets @@ List.init size (fun _ -> ty) in
        Tuple fields
    | TAdt adt as ty -> (
        let type_decl = Crate.get_adt adt in
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
                Enum (discr, fields))
        | Struct fields ->
            let++ fields = nondets @@ Charon_util.field_tys fields in
            Tuple fields
        | ty ->
            Fmt.kstr Rustsymex.not_impl "nondet: unsupported type %a"
              Types.pp_type_decl_kind ty)
    | TVar (Free id) ->
        let+ v = Rustsymex.nondet (Typed.t_usize ()) in
        let id = Types.TypeVarId.to_int id in
        Ok (TypeVar (id, v))
    | ty -> Fmt.kstr Rustsymex.not_impl "nondet: unsupported type %a" pp_ty ty

  and nondets tys =
    let open Rustsymex.Syntax in
    let++ l =
      Rustsymex.Result.fold_list tys ~init:[] ~f:(fun fields ty ->
          let++ f = nondet ty in
          f :: fields)
    in
    List.rev l

  (** Apply the compiler-attribute to the given value *)
  let apply_attribute v attr =
    let open Rustsymex in
    let open Syntax in
    match (v, attr) with
    | ( Int v,
        Meta.AttrUnknown
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
    Rustsymex.Result.fold_list attributes
      ~f:(fun () -> apply_attribute v)
      ~init:()

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
    | Ptr ptr, TRef (_, _, rk) ->
        let++ ptr, acc = fn init ptr (get_pointee ty) rk in
        (Ptr ptr, acc)
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
        (* FIXME: figure out if references inside unions get reborrowed. They could, but I
           suspect they don't because there's no guarantee the reference isn't some other field,
           e.g. in [union { a: &u8, b: &u16 }]  *)
        Result.ok (v, init)
    | v, _ -> Result.ok (v, init)
end
