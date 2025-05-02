open Charon
open Typed
open Typed.Syntax
open Typed.Infix
open T
open Charon_util
open Rustsymex
open Rustsymex.Syntax
open Layout

module Make (Sptr : Sptr.S) = struct
  type nonrec rust_val = Sptr.t rust_val

  let pp_rust_val = pp_rust_val Sptr.pp

  type cval_info = {
    value : rust_val;
    ty : Types.ty; [@printer Charon_util.pp_ty]
    offset : sint Typed.t;
  }
  [@@deriving show { with_path = false }]

  (** Converts a Rust value of the given type into a list of sub values, along
      with their size and offset *)
  let rec rust_to_cvals ?(offset = 0s) (value : rust_val) (ty : Types.ty) :
      cval_info list =
    let illegal_pair () =
      L.error (fun m ->
          m "Wrong pair of rust_value and Charon.ty: %a / %a" ppa_rust_val value
            Types.pp_ty ty);
      failwith "Wrong pair of rust_value and Charon.ty"
    in
    let chain_cvals layout vals types =
      List.map2i
        (fun i value ty ->
          let offset =
            (Array.get layout.members_ofs i |> Typed.int) +@ offset
          in
          rust_to_cvals ~offset value ty)
        vals types
      |> List.flatten
    in

    match (value, ty) with
    (* Literals *)
    | Base _, TLiteral _ -> [ { value; ty; offset } ]
    | Ptr _, TLiteral (TInteger (Isize | Usize)) -> [ { value; ty; offset } ]
    | _, TLiteral _ -> illegal_pair ()
    (* References / Pointers *)
    | Ptr (_, None), TAdt (TBuiltin TBox, { types = [ sub_ty ]; _ })
    | Ptr (_, None), TRef (_, sub_ty, _)
    | Ptr (_, None), TRawPtr (sub_ty, _) ->
        let ty : Types.ty = TLiteral (TInteger Isize) in
        if is_dst sub_ty then failwith "Expected a fat pointer"
        else [ { value; ty; offset } ]
    | Ptr (ptr, Some meta), TAdt (TBuiltin TBox, { types = [ sub_ty ]; _ })
    | Ptr (ptr, Some meta), TRef (_, sub_ty, _)
    | Ptr (ptr, Some meta), TRawPtr (sub_ty, _) ->
        let ty : Types.ty = TLiteral (TInteger Isize) in
        let value = Ptr (ptr, None) in
        if is_dst sub_ty then
          let size = Typed.int Archi.word_size in
          [
            { value; ty; offset };
            { value = Base meta; ty; offset = offset +@ size };
          ]
        else [ { value; ty; offset } ]
    (* References / Pointers obtained from casting *)
    | Base _, TAdt (TBuiltin TBox, _) | Base _, TRef _ | Base _, TRawPtr _ ->
        [ { value; ty = TLiteral (TInteger Isize); offset } ]
    | _, TAdt (TBuiltin TBox, _) | _, TRawPtr _ | _, TRef _ -> illegal_pair ()
    (* Tuples *)
    | Tuple vs, TAdt (TTuple, { types; _ }) ->
        chain_cvals (layout_of ty) vs types
    | Tuple _, _ | _, TAdt (TTuple, _) -> illegal_pair ()
    (* Structs *)
    | Struct vals, TAdt (TAdtId t_id, _) ->
        let type_decl = Session.get_adt t_id in
        let fields =
          match type_decl.kind with
          | Struct fields -> field_tys fields
          | _ ->
              Fmt.failwith "Unexpected type declaration in struct value: %a"
                Types.pp_type_decl type_decl
        in
        chain_cvals (layout_of ty) vals fields
    | Struct _, _ -> illegal_pair ()
    (* Enums *)
    | Enum (disc, vals), TAdt (TAdtId t_id, _) -> (
        let type_decl = Session.get_adt t_id in
        match (type_decl.kind, Typed.kind disc) with
        (* fieldless enums with one option are zero-sized *)
        | Enum [ { fields = []; _ } ], _ -> []
        | Enum variants, Int disc_z ->
            let variant =
              List.find
                (fun v -> Z.equal disc_z Types.(v.discriminant.value))
                variants
            in
            let disc_ty =
              Types.TLiteral (TInteger variant.discriminant.int_ty)
            in
            chain_cvals (of_variant variant) (Base disc :: vals)
              (disc_ty :: field_tys variant.fields)
        | _ ->
            Fmt.failwith "Unexpected ADT type or discr for enum: %a"
              Types.pp_type_decl type_decl)
    | Base value, TAdt (TAdtId t_id, _) when Session.is_enum t_id ->
        let type_decl = Session.get_adt t_id in
        let disc_ty =
          match type_decl.kind with
          | Enum [] -> failwith "Can't convert discriminant for empty enum"
          | Enum (v :: _) -> v.discriminant.int_ty
          | _ -> assert false
        in
        [
          { value = Enum (value, []); ty = TLiteral (TInteger disc_ty); offset };
        ]
    | Enum _, _ -> illegal_pair ()
    (* Arrays *)
    | Array vals, TAdt (TBuiltin TArray, { types = [ sub_ty ]; _ }) ->
        let layout = layout_of ty in
        let size = Array.length layout.members_ofs in
        if List.length vals <> size then failwith "Array length mismatch"
        else chain_cvals layout vals (List.init size (fun _ -> sub_ty))
    | Array _, _ | _, TAdt (TBuiltin TArray, _) -> illegal_pair ()
    (* Unions *)
    | Union (f, v), TAdt (TAdtId id, _) ->
        let type_decl = Session.get_adt id in
        let field =
          match type_decl.kind with
          | Union fs -> Types.FieldId.nth fs f
          | _ -> failwith "Unexpected ADT type for union"
        in
        rust_to_cvals ~offset v field.field_ty
    | Union _, _ -> illegal_pair ()
    (* Rest *)
    | _ ->
        L.error (fun m ->
            m "Unhandled rust_value and Charon.ty: %a / %a" ppa_rust_val value
              Types.pp_ty ty);
        failwith "Unhandled rust_value and Charon.ty"

  type ('e, 'f) parse_callback =
    rust_val list -> (('e, 'f) parser_return, 'e, 'f) Result.t

  and ('e, 'f) parser_return =
    [ `Done of rust_val
    | `More of (Types.ty * sint Typed.t) list * ('e, 'f) parse_callback ]

  (** Converts a Rust type into a list of C blocks, along with their offset;
      once these are read, symbolically decides whether we must keep reading.
      [offset] is the initial offset to read from, [meta] is the optional
      metadata, that originates from a fat pointer. *)
  let rust_of_cvals ?offset ?meta ty : ('e, 'f) parser_return =
    (* Base case, parses all types. *)
    let rec aux offset : Types.ty -> ('e, 'f) parser_return = function
      | TLiteral _ as ty ->
          `More
            ( [ (ty, offset) ],
              function
              | [ (Base _ as v) ] -> Result.ok (`Done v)
              | [ Ptr (ptr, None) ] ->
                  let* ptr_v = Sptr.decay ptr in
                  Result.ok (`Done (Base (ptr_v :> T.cval Typed.t)))
              | _ -> not_impl "Expected a base or a thin pointer" )
      | ( TAdt (TBuiltin TBox, { types = [ sub_ty ]; _ })
        | TRef (_, sub_ty, _)
        | TRawPtr (sub_ty, _) ) as ty
        when is_dst sub_ty ->
          let callback = function
            | [ ((Base _ | Ptr (_, None)) as ptr); Base meta ] -> (
                let* ptr =
                  match ptr with
                  | Ptr (ptr_v, None) -> return ptr_v
                  | Base ptr_v ->
                      let+ ptr_v = cast_checked ~ty:Typed.t_int ptr_v in
                      Sptr.offset Sptr.null_ptr ptr_v
                  | _ -> assert false
                in
                (* The check for the validity of the metadata is only required for references,
                   not raw pointers. *)
                match ty with
                | TRawPtr _ ->
                    Result.ok
                    @@ `Done (Ptr (ptr, Some (meta :> T.cval Typed.t)))
                | _ ->
                    let* meta = cast_checked ~ty:Typed.t_int meta in
                    (* FIXME: this only applies to slices, I'm not sure for other fat pointers... *)
                    if%sat meta <@ 0s then Result.error `UBTransmute
                    else
                      Result.ok
                      @@ `Done (Ptr (ptr, Some (meta :> T.cval Typed.t))))
            | vs ->
                Fmt.kstr not_impl "Expected a pointer and base, got %a"
                  Fmt.(list ~sep:comma pp_rust_val)
                  vs
          in
          let ptr_size = Typed.int Archi.word_size in
          let isize : Types.ty = TLiteral (TInteger Isize) in
          `More ([ (isize, offset); (isize, offset +@ ptr_size) ], callback)
      (* Raw pointers can be both a valid pointer or a number, whereas a reference must
         always be a valid pointer. *)
      | TRawPtr _ ->
          `More
            ( [ (TLiteral (TInteger Isize), offset) ],
              function
              | [ ((Ptr _ | Base _) as ptr) ] -> Result.ok (`Done ptr)
              | _ -> not_impl "Expected a pointer or base" )
      | TAdt (TBuiltin TBox, _) | TRef _ ->
          `More
            ( [ (TLiteral (TInteger Isize), offset) ],
              function
              | [ (Ptr _ as ptr) ] -> Result.ok (`Done ptr)
              | [ Base _ ] -> Result.error `UBTransmute
              | _ -> not_impl "Expected a pointer or base" )
      | TAdt (TTuple, { types; _ }) as ty ->
          let layout = layout_of ty in
          aux_fields ~f:(fun fs -> Tuple fs) ~layout offset types
      | TAdt (TAdtId t_id, _) as ty -> (
          let type_decl = Session.get_adt t_id in
          match type_decl.kind with
          | Struct fields ->
              let layout = layout_of ty in
              fields
              |> field_tys
              |> aux_fields ~f:(fun fs -> Struct fs) ~layout offset
          | Enum [] -> `More ([], fun _ -> Result.error `RefToUninhabited)
          | Enum [ { fields = []; discriminant; _ } ] ->
              `Done (Enum (value_of_scalar discriminant, []))
          | Enum variants -> aux_enum offset variants
          | Union fs -> aux_union offset fs
          | _ ->
              Fmt.failwith "Unhandled type kind in rust_of_cvals: %a"
                Types.pp_type_decl_kind type_decl.kind)
      | TAdt (TBuiltin TArray, { types = [ sub_ty ]; _ }) as ty ->
          let layout = layout_of ty in
          let len = Array.length layout.members_ofs in
          let fields = List.init len (fun _ -> sub_ty) in
          aux_fields ~f:(fun fs -> Array fs) ~layout offset fields
      | TAdt (TBuiltin (TStr as ty), generics)
      | TAdt (TBuiltin (TSlice as ty), generics) -> (
          (* We can only read a slice if we have the metadata of its length, in which case
           we interpret it as an array of that length. *)
          match meta with
          | None -> Fmt.failwith "Tried reading slice without metadata"
          | Some meta ->
              let len =
                match Typed.kind meta with
                | Int len -> Z.to_int len
                | _ -> failwith "Can't read a slice of non-concrete size"
              in
              let sub_ty =
                if ty = TSlice then List.hd generics.types
                else TLiteral (TInteger U8)
              in
              (* FIXME: This is a bit hacky, and not performant -- instead we should try to
                 group the reads together, at least for primitive types. *)
              let arr_ty = mk_array_ty sub_ty len in
              let layout = layout_of arr_ty in
              let fields = List.init len (fun _ -> sub_ty) in
              aux_fields ~f:(fun fs -> Array fs) ~layout offset fields)
      | TNever -> `More ([], fun _ -> Result.error `RefToUninhabited)
      | ty -> Fmt.failwith "Unhandled Charon.ty: %a" Types.pp_ty ty
    (* basically, a parser is just a sort of monad, so we can have the usual operations on it *)
    and aux_bind ~f parser_ret : ('e, 'f) parser_return =
      match parser_ret with
      | `More (blocks, callback) ->
          let callback v = Result.map (callback v) (aux_bind ~f) in
          `More (blocks, callback)
      | `Done value -> f value
    (* util to map a parser result, at the end *)
    and aux_map ~f parser_ret : ('e, 'f) parser_return =
      aux_bind ~f:(fun v -> `Done (f v)) parser_ret
    (* Parses a list of fields (for structs and tuples) *)
    and aux_fields ~f ~layout offset fields : ('e, 'f) parser_return =
      let base_offset = offset +@ (offset %@ Typed.nonzero layout.align) in
      let rec mk_callback to_parse parsed : ('e, 'f) parser_return =
        match to_parse with
        | [] -> `Done (f (List.rev parsed))
        | ty :: rest ->
            let idx = List.length parsed in
            let offset = Array.get layout.members_ofs idx |> Typed.int in
            let offset = base_offset +@ offset in
            aux_bind ~f:(fun v -> mk_callback rest (v :: parsed))
            @@ aux offset ty
      in
      mk_callback fields []
    (* Parses what enum variant we're handling *)
    and aux_enum offset (variants : Types.variant list) : ('e, 'f) parser_return
        =
      let disc = (List.hd variants).discriminant in
      let disc_ty = Values.TInteger disc.int_ty in
      let disc_align = Typed.nonzero (align_of_literal_ty disc_ty) in
      let offset = offset +@ (offset %@ disc_align) in
      let callback cval =
        let cval = Charon_util.as_base_of ~ty:Typed.t_int @@ List.hd cval in
        let* res =
          match_on variants ~constr:(fun (v : Types.variant) ->
              cval ==@ value_of_scalar v.discriminant)
        in
        match res with
        | Some var ->
            (* skip discriminant *)
            let discr = value_of_scalar var.discriminant in
            let ({ members_ofs = mems; _ } as layout) = of_variant var in
            let members_ofs = Array.sub mems 1 (Array.length mems - 1) in
            let layout = { layout with members_ofs } in
            let parser =
              var.fields
              |> field_tys
              |> aux_fields ~f:(fun fs -> Enum (discr, fs)) ~layout offset
            in
            Result.ok parser
        | None ->
            L.error (fun m ->
                m "Unmatched discriminant in rust_of_cvals: %a" Typed.ppa cval);
            Result.error `UBTransmute
      in
      `More ([ (TLiteral disc_ty, offset) ], callback)
    and aux_union offset fs : ('e, 'f) parser_return =
      (* read largest field *)
      let layouts =
        fs
        |> List.mapi @@ fun i (f : Types.field) ->
           (i, f.field_ty, layout_of f.field_ty)
      in
      let f, ty, _ =
        List.fold_left
          (fun ((_, _, accl) as acc) ((_, _, l) as cur) ->
            if l.size > accl.size then cur else acc)
          (List.hd layouts) (List.tl layouts)
      in
      aux_map ~f:(fun fs -> Union (Types.FieldId.of_int f, fs)) @@ aux offset ty
    in
    let off = Option.value ~default:0s offset in
    aux off ty

  (** Transmute a value of the given type into the other type.

      Accepts an optional [verify_ptr] function, that symbolically checks if a
      pointer can be used to read a value of the given type. This verification
      is a *ghost read*, and should not have side-effects. *)
  let rec transmute ?verify_ptr ?(try_splitting = true) ~(from_ty : Types.ty)
      ~(to_ty : Types.ty) v =
    let open Soteria_symex.Compo_res in
    let open Result in
    L.debug (fun m ->
        m "Transmuting %a: %a -> %a" pp_rust_val v pp_ty from_ty pp_ty to_ty);
    if from_ty = to_ty then ok v
    else
      match (from_ty, to_ty, v) with
      | TLiteral (TFloat _), TLiteral (TInteger _), Base sv ->
          let+ sv =
            of_opt_not_impl ~msg:"Unsupported: non-float in float-to-int"
            @@ Typed.cast_float sv
          in
          let sv' = Typed.int_of_float sv in
          Ok (Base sv')
      | TLiteral (TInteger _), TLiteral (TFloat fp), Base sv ->
          let+ sv = cast_checked sv ~ty:Typed.t_int in
          let sv' = Typed.float_of_int fp sv in
          Ok (Base sv')
      | TLiteral (TInteger U8), TLiteral TChar, v
      | TLiteral TBool, TLiteral (TInteger (U8 | U16 | U32 | U64 | U128)), v
      | TLiteral TChar, TLiteral (TInteger (U32 | U64 | U128)), v ->
          Result.ok v
      | TLiteral (TInteger from_ty), TLiteral (TInteger to_ty), Base sv ->
          let* v = cast_checked ~ty:Typed.t_int sv in
          let bits = 8 * Layout.size_of_int_ty to_ty in
          let max = Typed.nonzero_z (Z.shift_left Z.one bits) in
          let maxsigned = Typed.nonzero_z (Z.shift_left Z.one (bits - 1)) in
          let* v =
            if Layout.is_signed from_ty then
              if%sat v <@ 0s then return (((v %@ max) +@ max) %@ max)
              else return (v %@ max)
            else return (v %@ max)
          in
          let* v =
            if Layout.is_signed to_ty then
              if%sat v >=@ maxsigned then return (v -@ max) else return v
            else return v
          in
          ok (Base (v :> Typed.T.cval Typed.t))
      | TLiteral _, TLiteral to_ty, Base sv ->
          let constrs = Layout.constraints to_ty in
          if%sat Typed.conj (constrs sv) then ok v else error `UBTransmute
      (* A ref cannot be an invalid pointer *)
      | _, (TRef _ | TAdt (TBuiltin TBox, _)), Base _ -> error `UBTransmute
      (* A ref must point to a readable location *)
      | ( (TRef _ | TRawPtr _ | TAdt (TBuiltin TBox, _)),
          ( TRef (_, inner_ty, _)
          | TAdt (TBuiltin TBox, { types = [ inner_ty ]; _ }) ),
          Ptr ptr ) -> (
          match verify_ptr with
          | None -> Result.ok v
          | Some fn ->
              let* is_valid = fn ptr inner_ty in
              if is_valid then ok v else error `UBTransmute)
      | ( ( TRef _ | TRawPtr _
          | TAdt (TBuiltin TBox, _)
          | TLiteral (TInteger (Isize | Usize | I64 | U64)) ),
          ( TRef _ | TRawPtr _
          | TAdt (TBuiltin TBox, _)
          | TLiteral (TInteger (Isize | Usize | I64 | U64)) ),
          (Ptr _ | Base _) ) ->
          ok v
      | _ when try_splitting ->
          let blocks = rust_to_cvals v from_ty in
          transmute_many ~to_ty blocks
      | _ ->
          Fmt.kstr not_impl "Unhandled transmute of %a: %a -> %a" pp_rust_val v
            pp_ty from_ty pp_ty to_ty

  and transmute_many ~(to_ty : Types.ty) vs =
    let open Syntaxes.Option in
    let ( |>** ) = Result.bind in
    let ( |>++ ) = Result.map in
    let pp_triple fmt (v, ty, o) =
      Fmt.pf fmt "(%a:%a, %d)" ppa_rust_val v pp_ty ty o
    in
    let transmute = transmute ~try_splitting:false in
    let size_of ty = (Layout.layout_of ty).size in
    let int_of_val v =
      match Typed.kind v with
      | Int v -> Z.to_int v
      | _ -> failwith "Expected a concrete integer"
    in
    (* to make our life easier, we check for concrete offsets in the layout; this should
           always be true anyways. *)
    let** vs =
      try
        Result.ok
        @@ List.map
             (fun { value; ty; offset } -> (value, ty, int_of_val offset))
             vs
      with _ -> not_impl "Symbolic offset in layout"
    in
    L.debug (fun m ->
        m "Transmute many: %a <- [%a]" pp_ty to_ty
          Fmt.(list ~sep:comma pp_triple)
          vs);
    let rec extract_block (ty, off) =
      let off = int_of_val off in
      let vs = List.map (fun (v, ty, o) -> (v, ty, o - off)) vs in
      (* 1. ideal case, we find a block with the same size and offset *)
      let- () =
        List.find_map
          (fun (v, ty', o) ->
            if o = 0 && size_of ty = size_of ty' then
              Some (transmute ~from_ty:ty' ~to_ty:ty v)
            else None)
          vs
      in
      (* 2. only one block, so we convert that if we expect an integer *)
      let- () =
        match (vs, ty) with
        | ( [ (v, (TLiteral (TInteger _) as from_ty), 0) ],
            (TLiteral (TInteger _) as to_ty) ) ->
            Some (transmute ~from_ty ~to_ty v)
        | _ -> None
      in
      (* 3. Several integers that can be merged together without splitting *)
      let- () =
        match ty with
        | TLiteral (TInteger ity) ->
            let size = size_of_int_ty ity in
            let bytes = Array.init size (fun _ -> false) in
            let relevant =
              List.filter
                (function
                  | _, Types.TLiteral (TInteger ity), o
                    when 0 <= o && o + size_of_int_ty ity <= size ->
                      Iter.(0 -- (size_of_int_ty ity - 1)) (fun i ->
                          bytes.(o + i) <- true);
                      true
                  | _ -> false)
                vs
            in
            if Array.for_all (fun b -> b) bytes then
              Some
                (let++ v =
                   Result.fold_list relevant ~init:0s ~f:(fun acc (v, ty, o) ->
                       let++ v =
                         transmute ~from_ty:ty ~to_ty:(int_to_unsigned ty) v
                       in
                       let v = Typed.cast @@ as_base v in
                       let pow = Z.shift_left Z.one (o * 8) in
                       acc +@ (v *@ Typed.int_z pow))
                 in
                 Base (v :> T.cval Typed.t))
            else None
        | _ -> None
      in
      (* 4. Floats can be parsed from ints *)
      let- () =
        match ty with
        | TLiteral (TFloat fp) ->
            Some
              (let ity : Types.integer_type =
                 match fp with
                 | F16 -> U16
                 | F32 -> U32
                 | F64 -> U64
                 | F128 -> U128
               in
               let++ v =
                 extract_block (TLiteral (TInteger ity), Typed.int off)
               in
               let v = Typed.cast @@ as_base v in
               let v = Typed.float_of_int fp v in
               Base v)
        | _ -> None
      in
      (* 5. If there's an integer block that contains what we're looking for, we split it *)
      let- () =
        match ty with
        | TLiteral (TInteger ity) as target_ty ->
            let size = size_of_int_ty ity in
            vs
            |> List.find_opt (function
                 | _, Types.TLiteral (TInteger ity), o ->
                     o <= 0 && size <= o + size_of_int_ty ity
                 | _ -> false)
            |> Option.map @@ fun (v, ty, o) ->
               let parent_size = size_of_int_ty (TypesUtils.ty_as_integer ty) in
               let** v = transmute ~from_ty:ty ~to_ty:(int_to_unsigned ty) v in
               let v = Typed.cast @@ as_base v in
               let shift = o + parent_size - size in
               let shift = Z.shift_left Z.one (shift * 8) in
               let v = v /@ Typed.nonzero_z shift in
               transmute
                 ~from_ty:(int_to_unsigned target_ty)
                 ~to_ty:target_ty (Base v)
        | _ -> None
      in
      (* X. give up *)
      Fmt.kstr not_impl "Transmute: Couldn't extract %a at %d from %a" pp_ty ty
        off
        Fmt.(list ~sep:comma pp_triple)
        vs
    in
    let rec aux = function
      | `Done v -> Result.ok v
      | `More (blocks, callback) ->
          Result.fold_list blocks ~init:[] ~f:(fun acc block ->
              let++ block = extract_block block in
              block :: acc)
          |>++ List.rev
          |>** callback
          |>** aux
    in
    aux @@ rust_of_cvals to_ty

  type 'a split_tree =
    [ `Node of T.sint Typed.t * 'a split_tree * 'a split_tree | `Leaf of 'a ]

  let split v (ty : Types.ty) at :
      ((rust_val * Types.ty) split_tree * (rust_val * Types.ty) split_tree)
      Rustsymex.t =
    let transmute ~from_ty ~to_ty v =
      let* res = transmute ~from_ty ~to_ty v in
      match res with
      | Ok v -> return v
      | _ -> not_impl "Transmute failed in split - vanishing"
    in
    let* at =
      match Typed.kind at with
      | Int size -> return (Z.to_int size)
      | _ -> not_impl "Don't know how to read this size"
    in
    match (v, ty) with
    | Base _, TLiteral (TInteger ity) ->
        (* Given an integer value and its size in bits, returns a binary tree with leaves that are
           of size 2^n *)
        let rec aux v sz =
          (* we're a power of two, so we're done *)
          if Z.popcount sz = 1 then
            let ty = size_to_uint (Z.to_int sz) in
            `Leaf (Base (v :> T.cval Typed.t), ty)
          else
            (* Split at the most significant bit; e.g. for size 7 (0b111), will split at 0b100,
               resulting in a leaf of size 3 (0b11) and a right leaf of size 4 (0b100) *)
            let at = Z.(one lsl log2 sz) in
            let leaf_l, leaf_r = split v sz at in
            `Node (Typed.int_z at, leaf_l, leaf_r)
        and split v sz at =
          let size_l = at in
          let size_r = Z.(sz - at) in
          let pow = Z.shift_left Z.one (Z.to_int size_l * 8) in
          let left = v %@ Typed.nonzero_z pow in
          let right = v /@ Typed.nonzero_z pow in
          let leaf_l = aux left size_l in
          let leaf_r = aux right size_r in
          (leaf_l, leaf_r)
        in
        (* get our starting size and unsigned integer *)
        let size = size_of_int_ty ity in
        if at < 1 || at >= size then
          Fmt.failwith "Invalid split: %a at %d" pp_ty ty at;
        let+ as_uint = transmute ~from_ty:ty ~to_ty:(int_to_unsigned ty) v in
        let v = Typed.cast @@ as_base as_uint in
        split v (Z.of_int size) (Z.of_int at)
    | _ ->
        Fmt.kstr not_impl "Split unspported: %a: %a at %d" pp_rust_val v pp_ty
          ty at
end
