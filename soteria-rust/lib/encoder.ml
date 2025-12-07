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

    let lift (m : 'a DecayMapMonad.t) : ('a, 'state, 'err, 'fix) t =
     fun _handler _get_all state ->
      let+ m in
      Compo_res.Ok (m, state)

    let not_impl msg = lift @@ not_impl msg
    let of_opt_not_impl msg x = lift @@ of_opt_not_impl msg x

    let assert_or_error cond err =
     fun _handler _get_all state ->
      DecayMapMonad.Result.map (assert_or_error cond err) (fun () ->
          ((), state))

    module Syntax = struct
      let ( let*** ) x f = bind x f
      let ( let++* ) x f = bind x (fun v -> lift @@ f v)
      let ( let+++ ) x f = map x f

      module Symex_syntax = struct
        let branch_on ?left_branch_name ?right_branch_name guard ~then_ ~else_ =
         fun handler state ->
          DecayMapMonad.branch_on ?left_branch_name ?right_branch_name guard
            ~then_:(fun () -> then_ () handler state)
            ~else_:(fun () -> else_ () handler state)
      end
    end
  end

  type cval_info = rust_val * T.sint Typed.t
  [@@deriving show { with_path = false }]

  (** Converts a Rust value of the given type into a list of sub values, along
      with their size and offset, and whether they are interiorly mutable. *)
  let rec rust_to_cvals ?offset (value : rust_val) (ty : Types.ty) :
      cval_info list =
    let illegal_pair () =
      L.error (fun m ->
          m "Wrong pair of rust_value and Charon.ty:@.- Val: %a@.- Ty: %a"
            pp_rust_val value Types.pp_ty ty);
      failwith "Wrong pair of rust_value and Charon.ty"
    in
    let offset = Option.value ~default:Usize.(0s) offset in
    let chain_cvals layout vals types =
      List.map2i
        (fun i value ty ->
          let field_offset =
            BV.usizei (Layout.Fields_shape.offset_of i layout.fields)
          in
          let offset = field_offset +!!@ offset in
          rust_to_cvals ~offset value ty)
        vals types
      |> List.flatten
    in
    match (value, ty) with
    (* Trait types: we resolve them early *)
    | _, TTraitType (tref, name) ->
        let ty = Layout.resolve_trait_ty tref name in
        rust_to_cvals ~offset value ty
    (* Literals *)
    | Int _, TLiteral _ -> [ (value, offset) ]
    | Float _, TLiteral _ -> [ (value, offset) ]
    | Ptr _, TLiteral (TInt Isize | TUInt Usize) -> [ (value, offset) ]
    | _, TLiteral _ -> illegal_pair ()
    (* References / Pointers *)
    | ( Ptr (ptr, meta),
        TAdt { id = TBuiltin TBox; generics = { types = [ sub_ty ]; _ } } )
    | Ptr (ptr, meta), TRef (_, sub_ty, _)
    | Ptr (ptr, meta), TRawPtr (sub_ty, _) -> (
        let size = BV.usizei (Layout.size_of_int_ty Isize) in
        match (meta, is_dst sub_ty) with
        | _, false -> [ (Ptr (ptr, Thin), offset) ]
        | Thin, true -> failwith "Expected a fat pointer"
        | Len len, true ->
            [ (Ptr (ptr, Thin), offset); (Int len, offset +!!@ size) ]
        | VTable vt, true ->
            [ (Ptr (ptr, Thin), offset); (Ptr (vt, Thin), offset +!!@ size) ])
    (* Function pointer *)
    | Ptr (_, Thin), TFnPtr _ -> [ (value, offset) ]
    (* References / Pointers obtained from casting *)
    | Int _, TAdt { id = TBuiltin TBox; _ }
    | Int _, TRef _
    | Int _, TRawPtr _
    | Int _, TFnPtr _ ->
        [ (value, offset) ]
    | _, TAdt { id = TBuiltin TBox; _ } | _, TRawPtr _ | _, TRef _ ->
        illegal_pair ()
    (* Tuples *)
    | Tuple vs, TAdt { id = TTuple; generics = { types; _ } } ->
        chain_cvals (layout_of ty) vs types
    | _, TAdt { id = TTuple; _ } -> illegal_pair ()
    (* Structs *)
    | Tuple vals, TAdt { id = TAdtId t_id; _ } ->
        let fields = field_tys @@ Crate.as_struct t_id in
        chain_cvals (layout_of ty) vals fields
    (* Enums *)
    | Enum (disc, vals), (TAdt { id = TAdtId t_id; _ } as ty) -> (
        let variants = Crate.as_enum t_id in
        let layout = Layout.layout_of ty in
        match layout with
        (* zero-sized *)
        | { size = 0; _ } -> []
        | { fields = Arbitrary (variant, _); _ } ->
            let variant = Types.VariantId.nth variants variant in
            let var_fields = field_tys variant.fields in
            chain_cvals layout vals var_fields
        | { fields = Enum (tag_layout, var_fields); _ } ->
            let disc =
              Option.get ~msg:"Discriminant not concrete" (BV.to_z disc)
            in
            let variant_id, variant =
              Option.get ~msg:"No matching variant?"
              @@ List.find_mapi
                   (fun i v ->
                     let v_disc = z_of_literal Types.(v.discriminant) in
                     if Z.equal disc v_disc then Some (i, v) else None)
                   variants
            in
            let var_fields = var_fields.(variant_id) in
            let discriminant =
              match tag_layout.tags.(variant_id) with
              | None -> []
              | Some tag ->
                  let v = BV.mk_lit tag_layout.ty tag in
                  let offset = BV.usizei tag_layout.offset +!!@ offset in
                  [ (Int v, offset) ]
            in
            let var_layout = { layout with fields = var_fields } in
            discriminant
            @ chain_cvals var_layout vals (field_tys variant.fields)
        | _ -> Fmt.failwith "Unexpected layout for enum")
    | Int value, TAdt { id = TAdtId t_id; _ } when Crate.is_enum t_id ->
        let layout = Layout.layout_of ty in
        let tag_ty =
          match layout.fields with
          | Enum (tag, _) -> tag.ty
          | _ -> failwith "Expected enum layout"
        in
        let value = Typed.cast_lit tag_ty value in
        [ (Int value, offset) ]
    | Enum _, _ -> illegal_pair ()
    (* Arrays *)
    | ( Tuple vals,
        TAdt
          {
            id = TBuiltin TArray;
            generics = { types = [ sub_ty ]; const_generics = [ len ]; _ };
          } ) ->
        let layout = layout_of ty in
        let len = int_of_const_generic len in
        if List.length vals <> len then failwith "Array length mismatch"
        else chain_cvals layout vals (List.init len (fun _ -> sub_ty))
    | _, TAdt { id = TBuiltin TArray; _ } -> illegal_pair ()
    (* Unions *)
    | Union blocks, TAdt { id = TAdtId _; _ } ->
        List.map (fun (v, o) -> (v, offset +!!@ o)) blocks
    | Union _, _ -> illegal_pair ()
    (* Static Functions (ZSTs) *)
    | ConstFn _, TFnDef _ -> []
    | ConstFn _, _ | _, TFnDef _ -> illegal_pair ()
    (* Should have been handled for arrays, tuples and structs *)
    | Tuple _, _ -> illegal_pair ()
    (* Rest *)
    | _ ->
        Fmt.failwith "Unhandled rust_value and Charon.ty: %a / %a" pp_rust_val
          value pp_ty ty

  (** Parses the current variant of the enum at the given offset. This handles
      cases such as niches, where the discriminant isn't directly encoded as a
      tag. *)
  let variant_of_enum ~offset ty :
      (Types.variant_id, 'state, 'e, 'fix) ParserMonad.t =
    let open ParserMonad in
    let open ParserMonad.Syntax in
    let layout = Layout.layout_of ty in
    (* if it's a ZST, we assume it's the first variant; I don't think this is
       always true, e.g. enum { A(!), B }, but it's ok for now. *)
    match layout with
    | { fields = Arbitrary (vid, _); _ } -> ok vid
    | { size = 0; _ } -> ok (Types.VariantId.of_int 0)
    | { fields = Enum (tag_layout, _); _ } -> (
        let offset = offset +!!@ BV.usizei tag_layout.offset in
        let*** tag = query (TLiteral tag_layout.ty, offset) in
        (* here we need to check and decay if it's a pointer, for niche encoding! *)
        let*** tag =
          match tag with
          | Int tag -> ok (Typed.cast_lit tag_layout.ty tag)
          | Ptr (p, Thin) -> lift @@ Sptr.decay p
          | _ -> Fmt.failwith "Unexpected tag: %a" pp_rust_val tag
        in
        let tags = Array.to_seqi tag_layout.tags |> List.of_seq in
        let*** res =
          lift
          @@ match_on tags ~constr:(function
               | _, None -> Typed.v_false
               | _, Some t -> tag ==@ BV.mk_lit tag_layout.ty t)
        in
        match (tag_layout.encoding, res) with
        | _, Some (vid, _) -> ok (Types.VariantId.of_int vid)
        | Direct, None ->
            let adt_id, _ = TypesUtils.ty_as_custom_adt ty in
            let adt = Crate.get_adt adt_id in
            let msg =
              Fmt.str "Unmatched discriminant for enum %a: %a" Crate.pp_name
                adt.item_meta.name Typed.ppa tag
            in
            error (`UBTransmute msg)
        | Niche untagged, None -> ok untagged)
    | _ -> failwith "Unexpected layout for enum"

  type ('e, 'fix, 'state) parser = (rust_val, 'state, 'e, 'fix) ParserMonad.t

  (** Converts a Rust type into a list of types to read, along with their
      offset; once these are read, symbolically decides whether we must keep
      reading. [offset] is the initial offset to read from, [meta] is the
      optional metadata, that originates from a fat pointer. *)
  let rust_of_cvals ?(meta = Thin) ~offset :
      Types.ty -> (rust_val, 'state, 'e, 'fix) ParserMonad.t =
    let open ParserMonad in
    let open ParserMonad.Syntax in
    let module T = Typed.T in
    (* Base case, parses all types. *)
    let rec aux offset : Types.ty -> ('e, 'fix, 'state) parser = function
      | TLiteral _ as ty -> (
          let*** q_res = query (ty, offset) in
          match q_res with
          | (Int _ | Float _) as v -> ok v
          | Ptr (ptr, Thin) ->
              let+++ ptr_v = lift @@ Sptr.decay ptr in
              Int ptr_v
          | _ ->
              Fmt.kstr not_impl "Expected a base or a thin pointer, got %a"
                pp_rust_val q_res)
      | ( TAdt { id = TBuiltin TBox; generics = { types = [ sub_ty ]; _ } }
        | TRef (_, sub_ty, _)
        | TRawPtr (sub_ty, _) ) as ty ->
          let must_be_valid =
            match ty with
            | TRef _ | TAdt { id = TBuiltin TBox; _ } -> true
            | TRawPtr _ -> false
            | _ -> failwith "Impossible"
          in
          let ptr_size = BV.usizei @@ Layout.size_of_int_ty Isize in
          let meta_kind = dst_kind sub_ty in
          (* Small hack; we don't want to read the metadata ! *)
          let ty =
            match (meta_kind, ty) with
            | NoneKind, _ -> ty
            | _, TRawPtr _ -> unit_ptr
            | _, _ -> unit_ref
          in
          let*** ptr = query (ty, offset) in
          let*** meta : Sptr.t Rust_val.meta =
            match meta_kind with
            | NoneKind -> ok Thin
            | LenKind -> (
                let isize : Types.ty = TLiteral (TInt Isize) in
                let*** meta = query (isize, offset +!!@ ptr_size) in
                match meta with
                | Int meta -> ok (Len meta)
                | _ -> not_impl "Unexpected metadata value")
            | VTableKind -> (
                let*** meta = query (unit_ptr, offset +!!@ ptr_size) in
                match meta with
                | Ptr (meta_v, Thin) -> ok (VTable meta_v)
                | _ -> not_impl "Unexpected metadata value")
          in
          let*** () =
            if must_be_valid then
              if match ptr with Ptr _ -> false | _ -> true then
                error `UBDanglingPointer
              else if not @@ Layout.is_inhabited sub_ty then
                error `RefToUninhabited
              else
                match meta with
                | Len len when must_be_valid ->
                    assert_or_error
                      (Usize.(0s) <=$@ len)
                      (`UBTransmute "Negative slice length")
                | _ -> ok ()
            else ok ()
          in
          let+++ ptr =
            match ptr with
            | Ptr (ptr_v, Thin) -> ok ptr_v
            | Int ptr_v ->
                let ptr_v = Typed.cast_i Usize ptr_v in
                ok (Sptr.null_ptr_of ptr_v)
            | v ->
                Fmt.kstr not_impl "Unexpected pointer value: %a" pp_rust_val v
          in
          Ptr (ptr, meta)
      | TFnPtr _ as ty -> (
          let*** boxed = query (ty, offset) in
          match boxed with
          | Ptr (p, _) as ptr ->
              let+++ () =
                assert_or_error
                  (Typed.not (Sptr.sem_eq (Sptr.null_ptr ()) p))
                  `UBDanglingPointer
              in
              ptr
          | Int _ -> error `UBDanglingPointer
          | _ -> not_impl "Expected a pointer or base")
      | TAdt { id = TTuple; generics = { types; _ } } as ty ->
          let layout = layout_of ty in
          let types = List.to_seq types in
          aux_fields ~f:(fun fs -> Tuple fs) ~layout offset types
      | TAdt { id = TAdtId t_id; _ } as ty -> (
          let type_decl = Crate.get_adt t_id in
          match type_decl.kind with
          | Struct fields ->
              let layout = layout_of ty in
              fields
              |> field_tys
              |> List.to_seq
              |> aux_fields ~f:(fun fs -> Tuple fs) ~layout offset
          | Enum [] -> error `RefToUninhabited
          | Enum variants -> aux_enum offset ty variants
          | Union _ ->
              let layout = layout_of ty in
              if layout.size = 0 then ok (Union [])
              else
                (* FIXME: this isn't exactly correct; union actually doesn't copy the padding
                   bytes (i.e. the intersection of the padding bytes of all fields). It is
                   quite painful to actually calculate these padding bytes so we just copy
                   the whole thing for now.
                   See https://github.com/rust-lang/unsafe-code-guidelines/issues/518
                   And a proper implementation is here:
                   https://github.com/minirust/minirust/blob/master/tooling/minimize/src/chunks.rs *)
                let+++ blocks = get_all (BV.usizeinz layout.size, offset) in
                Union blocks
          | _ ->
              Fmt.kstr failwith "Unhandled ADT kind in rust_of_cvals: %a"
                Types.pp_type_decl_kind type_decl.kind)
      | TAdt { id = TBuiltin TArray; generics = { types; const_generics; _ } }
        as ty ->
          let sub_ty = List.hd types in
          let len = z_of_const_generic @@ List.hd const_generics in
          let layout = layout_of ty in
          let fields = Seq.init_z len (fun _ -> sub_ty) in
          aux_fields ~f:(fun fs -> Tuple fs) ~layout offset fields
      | TAdt { id = TBuiltin (TStr as ty); generics }
      | TAdt { id = TBuiltin (TSlice as ty); generics } ->
          (* We can only read a slice if we have the metadata of its length, in which case
           we interpret it as an array of that length. *)
          let*** len =
            match meta with
            | Thin -> failwith "Tried reading slice without metadata"
            | Len l -> ok l
            | VTable ptr -> lift @@ Sptr.decay ptr
          in
          let*** len =
            of_opt_not_impl
              (Fmt.str "Slice length not concrete: %a" Typed.ppa len)
              (BV.to_z len)
          in
          let sub_ty =
            if ty = TSlice then List.hd generics.types else TLiteral (TUInt U8)
          in
          (* FIXME: This is a bit hacky, and not performant -- instead we should try to
                 group the reads together, at least for primitive types. *)
          let arr_ty = mk_array_ty sub_ty len in
          let layout = layout_of arr_ty in
          let fields = Seq.init_z len (fun _ -> sub_ty) in
          aux_fields ~f:(fun fs -> Tuple fs) ~layout offset fields
      | TNever -> error `RefToUninhabited
      | TTraitType (tref, name) ->
          let ty = Layout.resolve_trait_ty tref name in
          aux offset ty
      | TFnDef fnptr -> ok (ConstFn fnptr.binder_value)
      | TDynTrait _ -> not_impl "Tried reading a trait object?"
      | TAdt { id = TBuiltin TBox; _ } -> failwith "Invalid box"
      | (TVar _ | TError _ | TPtrMetadata _) as ty ->
          Fmt.kstr not_impl "Unhandled Charon.ty: %a" Types.pp_ty ty
    (* Parses a sequence of fields (for structs, tuples, arrays) *)
    and aux_fields ~f ~layout offset (fields : Types.ty Seq.t) :
        ('e, 'fix, 'state) parser =
      let base_offset = offset +!!@ (offset %@ BV.usizeinz layout.align) in
      let rec mk_callback idx to_parse parsed : ('e, 'fix, 'state) parser =
        match to_parse () with
        | Seq.Nil -> ok (f (List.rev parsed))
        | Seq.Cons (ty, rest) ->
            let field_off = Layout.Fields_shape.offset_of idx layout.fields in
            let offset = base_offset +!!@ BV.usizei field_off in
            bind (aux offset ty) (fun v ->
                mk_callback (succ idx) rest (v :: parsed))
      in
      mk_callback 0 fields []
    (* Parses what enum variant we're handling *)
    and aux_enum offset ty variants : ('e, 'fix, 'state) parser =
      let*** v_id = variant_of_enum ~offset ty in
      let layout = Layout.layout_of ty in
      let fields = Layout.Fields_shape.shape_for_variant v_id layout.fields in
      let variant = Types.VariantId.nth variants v_id in
      let layout = { layout with fields } in
      let discr = BV.of_literal variant.discriminant in
      variant.fields
      |> field_tys
      |> List.to_seq
      |> aux_fields ~f:(fun fs -> Enum (discr, fs)) ~layout offset
    in
    aux offset

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

  (** Transmutes a singular rust value, without splitting. This is under the
      assumption that [size_of to_ty = size_of v], and both are primitives
      (literal or pointer). *)
  let transmute_one ~(to_ty : Types.ty) (v : rust_val) =
    let open DecayMapMonad.Result in
    match (to_ty, v) with
    | TLiteral (TFloat _), Float _ -> ok v
    | TLiteral (TFloat fp), Int v ->
        (* FIXME: here we should have *no* rounding, just interpret bytes *)
        let fp = Charon_util.float_precision fp in
        let f = BV.to_float ~rounding:NearestTiesToEven ~signed:false ~fp v in
        ok (Float f)
    | TLiteral ((TInt _ | TUInt _ | TBool | TChar) as ty), Int v ->
        with_constraints ~ty v
    | TLiteral ((TInt _ | TUInt _ | TBool | TChar) as ty), Ptr (p, Thin) ->
        let* p = Sptr.decay p in
        with_constraints ~ty p
    | TLiteral ((TInt _ | TUInt _ | TBool | TChar) as ty), Float f ->
        (* FIXME: here we should have *no* rounding, just interpret bytes *)
        let size = Svalue.FloatPrecision.size (Typed.Float.fp_of f) in
        let v = BV.of_float ~rounding:Truncate ~signed:false ~size f in
        with_constraints ~ty v
    | (TRawPtr _ | TRef _ | TFnPtr _), Ptr (_, Thin) -> ok v
    | TRef _, Int _ -> error `UBDanglingPointer
    | (TRawPtr _ | TFnPtr _), Int v -> ok (Ptr (Sptr.null_ptr_of v, Thin))
    | _ ->
        Fmt.kstr not_impl "transmute_one: unsupported %a -> %a" pp_rust_val v
          pp_ty to_ty

  (** Transmute a series of blocks into another type.

      Accepts an optional [verify_ptr] function, that symbolically checks if a
      pointer can be used to read a value of the given type. This verification
      is a *ghost read*, and does not have side-effects. *)
  let transmute ?verify_ptr ~(to_ty : Types.ty) (vs : cval_info list) =
    (* FIXME: we heavily manipulate integers here, which is ok in most cases but really we should
       be doing all of this at least with Z.t, and at best at the symbolic level directly. *)
    let open DecayMapMonad.Result in
    let pp_triple fmt (v, o, s) = Fmt.pf fmt "(%a, %d, %d)" pp_rust_val v o s in
    let size_of_ty ty = (Layout.layout_of ty).size in
    let int_of_val v =
      Z.to_int (Option.get ~msg:"Non-concrete size" (BV.to_z v))
    in
    let to_bitvec v =
      match v with
      | Int v -> return v
      | Float f ->
          let size = Svalue.FloatPrecision.size (Typed.Float.fp_of f) in
          (* FIXME: here we should have *no* rounding, just interpret bytes *)
          return @@ BV.of_float ~rounding:Truncate ~signed:false ~size f
      | Ptr (ptr, Thin) -> Sptr.decay ptr
      | Ptr (ptr, Len len) ->
          let+ ptr = Sptr.decay ptr in
          BV.concat len ptr
      | Ptr (ptr, VTable vt) ->
          let* ptr = Sptr.decay ptr in
          let+ vt = Sptr.decay vt in
          BV.concat vt ptr
      | _ -> failwith "Expected Base or Ptr"
    in
    let fuse_bvs bvs =
      let bvs = List.sort (fun (_, o1, _) (_, o2, _) -> o1 - o2) bvs in
      let rec aux = function
        | [] -> failwith "impossible: empty list"
        | [ (last, _, _) ] -> to_bitvec last
        | (v, _, _) :: rest ->
            let* l = aux rest in
            let+ r = to_bitvec v in
            BV.concat l r
      in
      aux bvs
    in

    (* to make our life easier, we check for concrete offsets in the layout *)
    let** vs =
      try
        vs
        |> List.map (fun (value, offset) ->
               (value, int_of_val offset, size_of value))
        |> Result.ok
      with _ -> not_impl "Symbolic offset in layout"
    in
    L.debug (fun m ->
        m "Transmute: %a <- [%a]" pp_ty to_ty Fmt.(list ~sep:comma pp_triple) vs);

    let get_relevant size off =
      let vs = List.map (fun (v, o, s) -> (v, o - off, s)) vs in
      DecayMapMonad.fold_list vs ~init:[] ~f:(fun acc (v, o, s) ->
          if o + s <= 0 || o >= size then return acc
          else if 0 <= o && o + s <= size then return ((v, o, s) :: acc)
          else
            let+ v = to_bitvec v in
            let crop_l = max 0 (0 - o) in
            let crop_r = max 0 (o + s - size) in
            let v = BV.extract (crop_l * 8) (((s - crop_r) * 8) - 1) v in
            let o = max 0 o in
            (Int v, max 0 o, s - crop_l - crop_r) :: acc)
    in

    let get_all (size, off) () =
      let size = int_of_val size in
      let off = int_of_val off in
      let+ relevant = get_relevant size off in
      let relevant =
        List.map (fun (v, o, _) -> (v, BV.usizei (o + off))) relevant
      in
      Compo_res.Ok (relevant, ())
    in

    let extract_block (ty, off) =
      let off = int_of_val off in
      let tgt_size = size_of_ty ty in
      (* get all blocks within the desired range, cropping as needed *)
      let* relevant = get_relevant tgt_size off in
      let** () =
        if
          Iter.for_all
            (fun i ->
              List.exists (fun (_, o, s) -> o <= i && i < o + s) relevant)
            Iter.(0 -- (tgt_size - 1))
        then ok ()
        else error `UninitializedMemoryAccess
      in
      match ty with
      | TLiteral (TInt _ | TUInt _ | TChar | TBool) ->
          let* res = fuse_bvs relevant in
          transmute_one ~to_ty:ty (Int res)
      | TLiteral (TFloat _) ->
          let* v =
            match relevant with
            | [ ((Float _ as f), _, _) ] -> return f
            | _ ->
                let+ v = fuse_bvs relevant in
                Int v
          in
          transmute_one ~to_ty:ty v
      | TRawPtr _ | TRef _ | TFnPtr _ -> (
          let* ptr =
            match relevant with
            | [ ((Ptr (_, Thin) as ptr), _, _) ] -> return ptr
            | _ ->
                let+ res = fuse_bvs relevant in
                Int res
          in
          let** ptr = transmute_one ~to_ty:ty ptr in
          match (ty, verify_ptr, ptr) with
          | TRef (_, inner_ty, _), Some fn, Ptr inner ->
              let* is_valid = lift @@ fn inner inner_ty in
              if is_valid then ok ptr else error `UBDanglingPointer
          | _ -> ok ptr)
      | _ -> Fmt.kstr not_impl "Couldn't transmute to %a" pp_ty ty
    in
    let handler query () =
      let++ r = extract_block query in
      (r, ())
    in
    let++ res, () =
      ParserMonad.parse ~init:() ~handler ~get_all
      @@ rust_of_cvals ~offset:Usize.(0s) to_ty
    in
    res

  type 'a split_tree =
    [ `Node of T.sint Typed.t * 'a split_tree * 'a split_tree | `Leaf of 'a ]

  let rec split v at :
      (rust_val split_tree * rust_val split_tree) DecayMapMonad.t =
    match v with
    | Ptr (ptr, meta) ->
        let* v = Sptr.decay ptr in
        let* v =
          match meta with
          | Thin -> return v
          | Len len -> return (BV.concat v len)
          | VTable ptr ->
              let+ v2 = Sptr.decay ptr in
              BV.concat v v2
        in
        split (Int v) at
    | Int v ->
        (* get our starting size and unsigned integer *)
        let size = Typed.size_of_int v / 8 in
        let+ at =
          match BV.to_z at with
          | Some at -> return (Z.to_int at)
          | _ -> (
              (* as per the contract of [split], we assume [at] is in [[1, size)] *)
              let options = List.init (size - 1) (( + ) 1) in
              let* res =
                match_on options ~constr:(fun x ->
                    Typed.sem_eq at (BV.usizei x))
              in
              match res with Some i -> return i | None -> vanish ())
        in
        let mask_l = BV.extract 0 ((at * 8) - 1) v in
        let mask_r = BV.extract (at * 8) ((size * 8) - 1) v in
        (`Leaf (Int mask_l), `Leaf (Int mask_r))
    | _ ->
        Fmt.kstr not_impl "Split unsupported: %a at %a" pp_rust_val v Typed.ppa
          at
end
