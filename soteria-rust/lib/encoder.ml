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

    (* The following is just query -> (rust_val, 'err, 'fix) StateResult.t
         where StateResult = StateT (Result), but I need StateT1of3 urgh. *)
    type ('state, 'err, 'fix) handler =
      query -> 'state -> (rust_val * 'state, 'err, 'fix) Result.t

    (* A parser monad is an object such that, given a query handler with state ['state],
      returns a state monad-ish for that state which may fail or branch *)
    type ('res, 'state, 'err, 'fix) t =
      ('state, 'err, 'fix) handler ->
      'state ->
      ('res * 'state, 'err, 'fix) Result.t

    let parse ~(init : 'state) ~(handler : ('state, 'err, 'fix) handler)
        scheduler : ('res * 'state, 'err, 'fix) Result.t =
      scheduler handler init

    let ok (x : 'a) : ('a, 'state, 'err, 'fix) t =
     fun _handler state -> Result.ok (x, state)

    let error (e : 'err) : ('a, 'state, 'err, 'fix) t =
     fun _handler _state -> Result.error e

    let bind2 (m : ('a, 'state, 'err, 'fix) t)
        (f : 'a -> ('b, 'state, 'err, 'fix) t)
        (fe : 'err -> ('b, 'state, 'err, 'fix) t) : ('b, 'state, 'err, 'fix) t =
     fun handler state ->
      let* res = m handler state in
      match res with
      | Compo_res.Ok (x, new_state) -> f x handler new_state
      | Compo_res.Error e -> fe e handler state
      | Compo_res.Missing f -> Result.miss f

    let bind (m : ('a, 'state, 'err, 'fix) t)
        (f : 'a -> ('b, 'state, 'err, 'fix) t) : ('b, 'state, 'err, 'fix) t =
     fun handler state ->
      let** x, new_state = m handler state in
      f x handler new_state

    let map (m : ('a, 'state, 'err, 'fix) t) (f : 'a -> 'b) :
        ('b, 'state, 'err, 'fix) t =
     fun handler state ->
      let++ x, new_state = m handler state in
      (f x, new_state)

    let query (q : query) : ('a, 'state, 'err, 'fix) t =
     fun handler state -> handler q state

    let lift (m : 'a DecayMapMonad.t) : ('a, 'state, 'err, 'fix) t =
     fun _handler state ->
      let+ m in
      Compo_res.Ok (m, state)

    let not_impl msg = lift @@ not_impl msg
    let of_opt_not_impl msg x = lift @@ of_opt_not_impl msg x

    let assert_or_error cond err =
     fun _handler state ->
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

    (** Returns the first element that parsed, if one parses succesfully, and
        else returns the first error that occurred. *)
    let first fn xs =
      let rec aux es = function
        | [] -> error (List.last es)
        | x :: xs -> bind2 (fn x) ok (fun e -> aux (e :: es) xs)
      in
      aux [] xs
  end

  type cval_info = {
    value : rust_val;
    ty : Types.ty; [@printer Charon_util.pp_ty]
    offset : T.sint Typed.t;
  }
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
          let offset = field_offset +!@ offset in
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
    | Base _, TLiteral _ -> [ { value; ty; offset } ]
    | Ptr _, TLiteral (TInt Isize | TUInt Usize) -> [ { value; ty; offset } ]
    | _, TLiteral _ -> illegal_pair ()
    (* References / Pointers *)
    | ( Ptr (ptr, meta),
        TAdt { id = TBuiltin TBox; generics = { types = [ sub_ty ]; _ } } )
    | Ptr (ptr, meta), TRef (_, sub_ty, _)
    | Ptr (ptr, meta), TRawPtr (sub_ty, _) -> (
        let ty : Types.ty = TLiteral (TInt Isize) in
        let size = BV.usizei (Layout.size_of_int_ty Isize) in
        match (meta, is_dst sub_ty) with
        | _, false -> [ { value = Ptr (ptr, Thin); ty; offset } ]
        | Thin, true -> failwith "Expected a fat pointer"
        | Len len, true ->
            let len = (len :> T.cval Typed.t) in
            [
              { value = Ptr (ptr, Thin); ty; offset };
              { value = Base len; ty; offset = offset +!@ size };
            ]
        | VTable vt, true ->
            [
              { value = Ptr (ptr, Thin); ty; offset };
              { value = Ptr (vt, Thin); ty; offset = offset +!@ size };
            ])
    (* Function pointer *)
    | Ptr (_, Thin), TFnPtr _ ->
        [ { value; ty = TLiteral (TInt Isize); offset } ]
    (* References / Pointers obtained from casting *)
    | Base _, TAdt { id = TBuiltin TBox; _ }
    | Base _, TRef _
    | Base _, TRawPtr _
    | Base _, TFnPtr _ ->
        [ { value; ty = TLiteral (TInt Isize); offset } ]
    | _, TAdt { id = TBuiltin TBox; _ } | _, TRawPtr _ | _, TRef _ ->
        illegal_pair ()
    (* Tuples *)
    | Tuple vs, TAdt { id = TTuple; generics = { types; _ } } ->
        chain_cvals (layout_of ty) vs types
    | Tuple _, _ | _, TAdt { id = TTuple; _ } -> illegal_pair ()
    (* Structs *)
    | Struct vals, TAdt { id = TAdtId t_id; _ } ->
        let fields = field_tys @@ Crate.as_struct t_id in
        chain_cvals (layout_of ty) vals fields
    | Struct _, _ -> illegal_pair ()
    (* Enums *)
    | Enum (disc, vals), (TAdt { id = TAdtId t_id; _ } as ty) -> (
        let variants = Crate.as_enum t_id in
        let layout = Layout.layout_of ty in
        match layout with
        (* fieldless enums with one option are zero-sized *)
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
                     if Z.equal disc (z_of_literal Types.(v.discriminant)) then
                       Some (i, v)
                     else None)
                   variants
            in
            let var_fields = var_fields.(variant_id) in
            let discriminant =
              match tag_layout.tags.(variant_id) with
              | None -> []
              | Some tag ->
                  let v = BV.mk_lit tag_layout.ty tag in
                  let offset = BV.usizei tag_layout.offset +!@ offset in
                  [ { value = Base v; ty = TLiteral tag_layout.ty; offset } ]
            in
            let var_layout = { layout with fields = var_fields } in
            discriminant
            @ chain_cvals var_layout vals (field_tys variant.fields)
        | _ -> Fmt.failwith "Unexpected layout for enum")
    | Base value, TAdt { id = TAdtId t_id; _ } when Crate.is_enum t_id ->
        let layout = Layout.layout_of ty in
        let tag_ty =
          match layout.fields with
          | Enum (tag, _) -> tag.ty
          | _ -> failwith "Expected enum layout"
        in
        let value = Typed.cast_lit tag_ty value in
        [ { value = Base value; ty = TLiteral tag_ty; offset } ]
    | Enum _, _ -> illegal_pair ()
    (* Arrays *)
    | ( Array vals,
        TAdt
          {
            id = TBuiltin TArray;
            generics = { types = [ sub_ty ]; const_generics = [ len ]; _ };
          } ) ->
        let layout = layout_of ty in
        let len = int_of_const_generic len in
        if List.length vals <> len then failwith "Array length mismatch"
        else chain_cvals layout vals (List.init len (fun _ -> sub_ty))
    | Array _, _ | _, TAdt { id = TBuiltin TArray; _ } -> illegal_pair ()
    (* Unions *)
    | Union (f, v), TAdt { id = TAdtId id; _ } ->
        let fields = Crate.as_union id in
        let field = Types.FieldId.nth fields f in
        rust_to_cvals ~offset v field.field_ty
    | Union _, _ -> illegal_pair ()
    (* Static Functions (ZSTs) *)
    | ConstFn _, TFnDef _ -> []
    | ConstFn _, _ | _, TFnDef _ -> illegal_pair ()
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
        let offset = offset +!@ BV.usizei tag_layout.offset in
        let*** tag = query (TLiteral tag_layout.ty, offset) in
        (* here we need to check and decay if it's a pointer, for niche encoding! *)
        let*** tag =
          match tag with
          | Base tag -> ok (Typed.cast_lit tag_layout.ty tag)
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
          | Base _ as v -> ok v
          | Ptr (ptr, Thin) ->
              let+++ ptr_v = lift @@ Sptr.decay ptr in
              Base ptr_v
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
          let isize : Types.ty = TLiteral (TInt Isize) in
          let*** ptr = query (isize, offset) in
          let*** ptr =
            match ptr with
            | Ptr (ptr_v, Thin) -> ok ptr_v
            | Base ptr_v when not must_be_valid ->
                let ptr_v = Typed.cast_i Usize ptr_v in
                ok (Sptr.null_ptr_of ptr_v)
            | Base _ -> error `UBDanglingPointer
            | _ -> not_impl "Unexpected pointer value"
          in
          let meta_kind = dst_kind sub_ty in
          let*** meta : Sptr.t Rust_val.meta =
            match meta_kind with
            | NoneKind -> ok Thin
            | LenKind | VTableKind -> (
                let*** meta = query (isize, offset +!@ ptr_size) in
                match (meta_kind, meta) with
                | LenKind, Base meta -> ok (Len (Typed.cast_i Usize meta))
                | LenKind, Ptr (meta_v, Thin) ->
                    let+++ meta = lift @@ Sptr.decay meta_v in
                    Len meta
                | VTableKind, Ptr (meta_v, Thin) -> ok (VTable meta_v)
                | VTableKind, Base meta ->
                    let meta = Typed.cast_i Usize meta in
                    ok (VTable (Sptr.null_ptr_of meta))
                | _ -> not_impl "Unexpected metadata value")
          in
          let+++ () =
            match meta with
            | Len len when must_be_valid ->
                assert_or_error
                  (Usize.(0s) <$@ len)
                  (`UBTransmute "Negative slice length")
            | _ -> ok ()
          in
          Ptr (ptr, meta)
      | TFnPtr _ -> (
          let*** boxed = query (TLiteral (TInt Isize), offset) in
          match boxed with
          | Ptr _ as ptr -> ok ptr
          | Base _ -> error `UBDanglingPointer
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
              |> aux_fields ~f:(fun fs -> Struct fs) ~layout offset
          | Enum [] -> error `RefToUninhabited
          | Enum variants -> aux_enum offset ty variants
          | Union fs -> aux_union offset fs
          | _ ->
              Fmt.kstr failwith "Unhandled ADT kind in rust_of_cvals: %a"
                Types.pp_type_decl_kind type_decl.kind)
      | TAdt { id = TBuiltin TArray; generics = { types; const_generics; _ } }
        as ty ->
          let sub_ty = List.hd types in
          let len = z_of_const_generic @@ List.hd const_generics in
          let layout = layout_of ty in
          let fields = Seq.init_z len (fun _ -> sub_ty) in
          aux_fields ~f:(fun fs -> Array fs) ~layout offset fields
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
          aux_fields ~f:(fun fs -> Array fs) ~layout offset fields
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
      let base_offset = offset +!@ (offset %@ BV.usizeinz layout.align) in
      let rec mk_callback idx to_parse parsed : ('e, 'fix, 'state) parser =
        match to_parse () with
        | Seq.Nil -> ok (f (List.rev parsed))
        | Seq.Cons (ty, rest) ->
            let field_off = Layout.Fields_shape.offset_of idx layout.fields in
            let offset = base_offset +!@ BV.usizei field_off in
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
    and aux_union offset fs : ('e, 'fix, 'state) parser =
      let parse_field (i, ty) = map (aux offset ty) (fun v -> Union (i, v)) in
      (* We try parsing all of fields of the enum, sorted by decreasing layout size.
         The first that succeeds gets returned, and otherwise we return the first error.
         We parse in decreasing type size, because if a union has () as a field (e.g.
         MaybeUninit), that field would always be returned, even in the presence of data. *)
      fs
      |> List.mapi (fun i (f : Types.field) ->
             let fid = Types.FieldId.of_int i in
             let l = layout_of f.field_ty in
             (fid, f.field_ty, l.size))
      |> List.sort (fun (_, _, s1) (_, _, s2) -> s2 - s1)
      |> List.map (fun (fid, ty, _) -> (fid, ty))
      |> first parse_field
    in
    aux offset

  (** Transmute between literals; perform validation of the type's constraints.
      See also:
      https://doc.rust-lang.org/stable/reference/expressions/operator-expr.html#numeric-cast
  *)
  let transmute_literal ~(from_ty : Types.literal_type)
      ~(to_ty : Types.literal_type) (v : [< T.cval ] Typed.t) =
    let open DecayMapMonad.Result in
    match (from_ty, to_ty) with
    | _, _ when from_ty = to_ty -> ok (Base v)
    | TFloat fty, ((TInt _ | TUInt _) as lit_ty) ->
        let sv = Typed.cast_f fty v in
        let signed = Layout.is_signed lit_ty in
        let size = 8 * size_of_literal_ty lit_ty in
        let sv' = BV.of_float ~rounding:Truncate ~signed ~size sv in
        ok (Base sv')
    | (TInt _ | TUInt _), TFloat fp ->
        let sv = Typed.cast_lit from_ty v in
        let fp = Charon_util.float_precision fp in
        let signed = Layout.is_signed from_ty in
        let sv' = BV.to_float ~rounding:NearestTiesToEven ~signed ~fp sv in
        ok (Base sv')
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
        let constraints = Typed.conj @@ Layout.constraints to_ty v in
        let msg = Fmt.str "Constraints of %a unsatisfied" pp_literal_ty to_ty in
        let++ () = assert_or_error constraints (`UBTransmute msg) in
        Base v

  (** Transmute a value of the given type into the other type.

      Accepts an optional [verify_ptr] function, that symbolically checks if a
      pointer can be used to read a value of the given type. This verification
      is a *ghost read*, and should not have side-effects. *)
  let rec transmute ?verify_ptr ?(try_splitting = true) ~(from_ty : Types.ty)
      ~(to_ty : Types.ty) v =
    let open DecayMapMonad.Result in
    L.debug (fun m ->
        m "Transmuting %a: %a -> %a" pp_rust_val v pp_ty from_ty pp_ty to_ty);
    if from_ty = to_ty then ok v
    else
      match (from_ty, to_ty, v) with
      | TLiteral from_ty, TLiteral to_ty, Base v ->
          transmute_literal ~from_ty ~to_ty v
      (* A ref cannot be an invalid pointer *)
      | _, (TRef _ | TAdt { id = TBuiltin TBox; _ }), Base _ ->
          error `UBDanglingPointer
      (* A ref must point to a readable location *)
      | ( _,
          ( TRef (_, inner_ty, _)
          | TAdt { id = TBuiltin TBox; generics = { types = [ inner_ty ]; _ } }
            ),
          Ptr ptr ) -> (
          match verify_ptr with
          | None -> Result.ok v
          | Some fn ->
              let* is_valid = lift @@ fn ptr inner_ty in
              if is_valid then ok v else error `UBDanglingPointer)
      (* A raw pointer can be whatever *)
      | _, TRawPtr _, Base off ->
          let off = Typed.cast_i Usize off in
          let ptr = Sptr.null_ptr_of off in
          ok (Ptr (ptr, Thin))
      | _, TRawPtr _, Ptr _ -> ok v
      | _, TLiteral ((TInt _ | TUInt _) as litty), Ptr (ptr, Thin)
        when size_of_literal_ty litty = size_of_literal_ty (TInt Isize) ->
          let* ptr_v = Sptr.decay ptr in
          ok (Base ptr_v)
      | _ when try_splitting ->
          let blocks = rust_to_cvals v from_ty in
          transmute_many ~to_ty blocks
      | _ ->
          Fmt.kstr not_impl "Unhandled transmute of %a: %a -> %a" pp_rust_val v
            pp_ty from_ty pp_ty to_ty

  and transmute_many ~(to_ty : Types.ty) vs =
    let pp_triple fmt (v, ty, o) =
      Fmt.pf fmt "(%a:%a, %d)" pp_rust_val v pp_ty ty o
    in
    let transmute = transmute ~try_splitting:false in
    let size_of ty = (Layout.layout_of ty).size in
    let int_of_val v =
      Z.to_int (Option.get ~msg:"Non-concrete size" (BV.to_z v))
    in
    (* to make our life easier, we check for concrete offsets in the layout; this should
           always be true anyways. *)
    let** vs =
      try
        Result.ok
        @@ List.map
             (fun { value; ty; offset; _ } -> (value, ty, int_of_val offset))
             vs
      with _ -> not_impl "Symbolic offset in layout"
    in
    L.debug (fun m ->
        m "Transmute many: %a <- [%a]" pp_ty to_ty
          Fmt.(list ~sep:comma pp_triple)
          vs);
    let extract_block (ty, off) =
      let open Syntaxes.Option in
      let off = int_of_val off in
      let vs = List.map (fun (v, ty, o) -> (v, ty, o - off)) vs in
      (* 0. make sure the entire range exists; otherwise it would mean there's an uninit access *)
      let- () =
        let size = (layout_of ty).size in
        let bytes = Array.make size false in
        List.iter
          (fun (_, ty, o) ->
            let s = (layout_of ty).size in
            Iter.(o -- (o + s - 1)) (fun i ->
                if 0 <= i && i < size then bytes.(i) <- true))
          vs;
        if Array.for_all (fun b -> b) bytes then None
        else Some (Result.error `UninitializedMemoryAccess)
      in
      (* 1. ideal case, we find a block with the same size and offset *)
      let- () =
        List.find_map
          (fun (v, ty', o) ->
            if o = 0 && size_of ty = size_of ty' then
              Some (transmute ~from_ty:ty' ~to_ty:ty v)
            else None)
          vs
      in
      (* 2. Several integers that can be merged together without splitting. *)
      let- () =
        match ty with
        | TLiteral lit_ty ->
            let size = size_of_literal_ty lit_ty in
            let bytes = Array.make size false in
            let relevant =
              List.filter_map
                (function
                  | Base v, Types.TLiteral lit_ty, o
                    when 0 <= o && o + size_of_literal_ty lit_ty <= size ->
                      Iter.(0 -- (size_of_literal_ty lit_ty - 1)) (fun i ->
                          bytes.(o + i) <- true);
                      Some (Typed.cast_lit lit_ty v, o)
                  | _ -> None)
                vs
            in
            if Array.for_all (fun b -> b) bytes then
              let relevant =
                List.sort (fun (_, o1) (_, o2) -> o1 - o2) relevant
              in
              let rec aux = function
                | [] -> failwith "impossible: empty list"
                | [ (last, _) ] -> last
                | (v, _) :: rest -> BV.concat (aux rest) v
              in
              let res = aux relevant in
              Some (Result.ok (Base (res :> T.cval Typed.t)))
            else None
        | _ -> None
      in
      (* 3. If there's an integer block that contains what we're looking for, we split it *)
      let- () =
        match ty with
        | TLiteral lit_ty ->
            let size = size_of_literal_ty lit_ty in
            vs
            |> List.find_map (function
                 | v, Types.TLiteral lit_ty, o
                   when o <= 0 && size <= o + size_of_literal_ty lit_ty ->
                     Some (v, lit_ty, o)
                 | _ -> None)
            |> Option.map @@ fun (v, lit_ty, o) ->
               let open DecayMapMonad.Syntax in
               let* v =
                 match v with
                 | Base v -> return v
                 | Ptr (ptr, _) -> Sptr.decay ptr
                 | _ -> not_impl "Transmute: don't know hot to split this"
               in
               let v = Typed.cast_lit lit_ty v in
               let v = BV.extract (o * -8) (((size - o) * 8) - 1) v in
               Result.ok (Base v)
        | _ -> None
      in
      (* X. give up *)
      Fmt.kstr not_impl "Transmute: Couldn't extract %a at %d from [%a]" pp_ty
        ty off
        Fmt.(list ~sep:comma pp_triple)
        vs
    in
    let parse_fn query () =
      let++ r = extract_block query in
      (r, ())
    in
    let++ res, () =
      ParserMonad.parse ~init:() ~handler:parse_fn
      @@ rust_of_cvals ~offset:Usize.(0s) to_ty
    in
    res

  type 'a split_tree =
    [ `Node of T.sint Typed.t * 'a split_tree * 'a split_tree | `Leaf of 'a ]

  let rec split v (ty : Types.ty) at :
      ((rust_val * Types.ty) split_tree * (rust_val * Types.ty) split_tree)
      DecayMapMonad.t =
    match (v, ty) with
    | Ptr (ptr, meta), _ ->
        let* v = Sptr.decay ptr in
        let* v =
          match meta with
          | Thin -> return v
          | Len len -> return (BV.concat v len)
          | VTable ptr ->
              let+ v2 = Sptr.decay ptr in
              BV.concat v v2
        in
        split (Base (v :> T.cval Typed.t)) ty at
    | Base _, TLiteral ((TInt _ | TUInt _ | TChar) as lit_ty) ->
        let+ at =
          of_opt_not_impl
            (Fmt.str "Don't know how to read this size: %a" Typed.ppa at)
            (Option.map Z.to_int (BV.to_z at))
        in
        (* Given an integer value and its size in bytes, returns a binary tree with leaves that are
           of size 2^n *)
        let rec aux v sz =
          (* we're a power of two, so we're done *)
          if Z.(popcount (of_int sz)) = 1 then
            let ty = size_to_uint sz in
            `Leaf (Base (v :> T.cval Typed.t), ty)
          else
            (* Split at the most significant bit; e.g. for size 7 (0b111), will split at 0b100,
               resulting in a leaf of size 3 (0b11) and a right leaf of size 4 (0b100) *)
            let at = 1 lsl Z.(log2 (of_int sz)) in
            let leaf_l, leaf_r = split v sz at in
            `Node (BV.usizei at, leaf_l, leaf_r)
        and split v sz at =
          let size_l = at in
          let size_r = sz - at in
          let mask_l = BV.extract 0 ((at * 8) - 1) v in
          let mask_r = BV.extract (at * 8) ((sz * 8) - 1) v in
          let leaf_l = aux mask_l size_l in
          let leaf_r = aux mask_r size_r in
          (leaf_l, leaf_r)
        in
        (* get our starting size and unsigned integer *)
        let size = size_of_literal_ty lit_ty in
        if at < 1 || at >= size then
          Fmt.failwith "Invalid split: %a at %d" pp_ty ty at;
        let v = as_base lit_ty v in
        split v size at
    | _ ->
        Fmt.kstr not_impl "Split unsupported: %a: %a at %a" pp_rust_val v pp_ty
          ty Typed.ppa at
end
