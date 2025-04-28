open Charon
open Typed
open Typed.Syntax
open Typed.Infix
open T
open Charon_util
open Rustsymex
open Rustsymex.Syntax
open Layout

type 'ptr cval_info = {
  value : 'ptr rust_val;
  ty : Types.ty;
  offset : sint Typed.t;
}
[@@deriving show]

(** Converts a Rust value of the given type into a list of sub values, along
    with their size and offset *)
let rec rust_to_cvals ?(offset = 0s) (value : 'ptr rust_val) (ty : Types.ty) :
    'ptr cval_info list =
  let illegal_pair () =
    L.err (fun m ->
        m "Wrong pair of rust_value and Charon.ty: %a / %a" ppa_rust_val value
          Types.pp_ty ty);
    failwith "Wrong pair of rust_value and Charon.ty"
  in
  let chain_cvals layout vals types =
    List.map2i
      (fun i value ty ->
        let offset = (Array.get layout.members_ofs i |> Typed.int) +@ offset in
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
      if is_fat_ptr sub_ty then failwith "Expected a fat pointer"
      else [ { value; ty; offset } ]
  | Ptr (ptr, Some meta), TAdt (TBuiltin TBox, { types = [ sub_ty ]; _ })
  | Ptr (ptr, Some meta), TRef (_, sub_ty, _)
  | Ptr (ptr, Some meta), TRawPtr (sub_ty, _) ->
      let ty : Types.ty = TLiteral (TInteger Isize) in
      let value = Ptr (ptr, None) in
      if is_fat_ptr sub_ty then
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
  | Tuple vs, TAdt (TTuple, { types; _ }) -> chain_cvals (layout_of ty) vs types
  | Tuple [], TNever -> []
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
          let disc_ty = Types.TLiteral (TInteger variant.discriminant.int_ty) in
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
      [ { value = Enum (value, []); ty = TLiteral (TInteger disc_ty); offset } ]
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
      L.err (fun m ->
          m "Unhandled rust_value and Charon.ty: %a / %a" ppa_rust_val value
            Types.pp_ty ty);
      failwith "Unhandled rust_value and Charon.ty"

type ('ptr, 'e, 'f) parse_callback =
  'ptr rust_val list -> (('ptr, 'e, 'f) parser_return, 'e, 'f) Result.t

and ('ptr, 'e, 'f) parser_return =
  [ `Done of 'ptr rust_val
  | `More of (Types.ty * sint Typed.t) list * ('ptr, 'e, 'f) parse_callback ]

(** Converts a Rust type into a list of C blocks, along with their offset; once
    these are read, symbolically decides whether we must keep reading. [offset]
    is the initial offset to read from, [meta] is the optional metadata, that
    originates from a fat pointer. *)
let rust_of_cvals ?offset ?meta ty : ('ptr, 'e, 'f) parser_return =
  (* Base case, parses all types. *)
  let rec aux offset : Types.ty -> ('ptr, 'e, 'f) parser_return = function
    | TLiteral _ as ty ->
        `More
          ( [ (ty, offset) ],
            function
            | [ ((Base _ | Ptr _) as v) ] -> Result.ok (`Done v)
            | _ -> not_impl "Expected a base" )
    | TAdt (TBuiltin TBox, { types = [ sub_ty ]; _ })
    | TRef (_, sub_ty, _)
    | TRawPtr (sub_ty, _)
      when is_fat_ptr sub_ty ->
        let callback = function
          | [ Ptr (ptr, None); Base meta ] ->
              Result.ok @@ `Done (Ptr (ptr, Some meta))
          | _ -> not_impl "Expected a pointer and base"
        in
        let ptr_size = Typed.int Archi.word_size in
        let isize : Types.ty = TLiteral (TInteger Isize) in
        `More ([ (isize, offset); (isize, offset +@ ptr_size) ], callback)
    | TAdt (TBuiltin TBox, _) | TRef _ | TRawPtr _ ->
        `More
          ( [ (TLiteral (TInteger Isize), offset) ],
            function
            | [ ((Ptr _ | Base _) as ptr) ] -> Result.ok (`Done ptr)
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
    | TAdt (TBuiltin TSlice, { types = [ sub_ty ]; _ }) -> (
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
            let arr_ty = mk_array_ty sub_ty len in
            let layout = layout_of arr_ty in
            let fields = List.init len (fun _ -> sub_ty) in
            aux_fields ~f:(fun fs -> Array fs) ~layout offset fields)
    | TAdt (TBuiltin TStr, _) -> (
        (* We can only read a slice if we have the metadata of its length, in which case
       we interpret it as an array of that length. *)
        match meta with
        | None -> Fmt.failwith "Tried reading string without metadata"
        | Some meta ->
            let len =
              match Typed.kind meta with
              | Int len -> Z.to_int len
              | _ -> failwith "Can't read a slice of non-concrete size"
            in
            let arr_ty = mk_array_ty (TLiteral (TInteger U8)) len in
            let layout = layout_of arr_ty in
            let fields =
              List.init len (fun _ -> Types.TLiteral (TInteger U8))
            in
            aux_fields ~f:(fun fs -> Array fs) ~layout offset fields)
    | ty -> Fmt.failwith "Unhandled Charon.ty: %a" Types.pp_ty ty
  (* basically, a parser is just a sort of monad, so we can have the usual operations on it *)
  and aux_bind ~f parser_ret : ('ptr, 'e, 'f) parser_return =
    match parser_ret with
    | `More (blocks, callback) ->
        let callback v = Result.map (callback v) (aux_bind ~f) in
        `More (blocks, callback)
    | `Done value -> f value
  (* util to map a parser result, at the end *)
  and aux_map ~f parser_ret : ('ptr, 'e, 'f) parser_return =
    aux_bind ~f:(fun v -> `Done (f v)) parser_ret
  (* Parses a list of fields (for structs and tuples) *)
  and aux_fields ~f ~layout offset fields : ('ptr, 'e, 'f) parser_return =
    let base_offset = offset +@ (offset %@ Typed.nonzero layout.align) in
    let rec mk_callback to_parse parsed : ('ptr, 'e, 'f) parser_return =
      match to_parse with
      | [] -> `Done (f (List.rev parsed))
      | ty :: rest ->
          let idx = List.length parsed in
          let offset = Array.get layout.members_ofs idx |> Typed.int in
          let offset = base_offset +@ offset in
          aux_bind ~f:(fun v -> mk_callback rest (v :: parsed)) @@ aux offset ty
    in
    mk_callback fields []
  (* Parses what enum variant we're handling *)
  and aux_enum offset (variants : Types.variant list) :
      ('ptr, 'e, 'f) parser_return =
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
          L.err (fun m ->
              m "Unmatched discriminant in rust_of_cvals: %a" Typed.ppa cval);
          Result.error `UBTransmute
    in
    `More ([ (TLiteral disc_ty, offset) ], callback)
  and aux_union offset fs : ('ptr, 'e, 'f) parser_return =
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

let rec transmute ~(from_ty : Types.ty) ~(to_ty : Types.ty) v =
  let open Soteria_symex.Compo_res in
  let open Result in
  let _unhandled () =
    let ctx = PrintUllbcAst.Crate.crate_to_fmt_env @@ Session.get_crate () in
    Fmt.kstr not_impl "Unhandled transmute of %a: %s -> %s" ppa_rust_val v
      (PrintTypes.ty_to_string ctx from_ty)
      (PrintTypes.ty_to_string ctx to_ty)
  in
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
    | ( ( TRef _ | TRawPtr _
        | TAdt (TBuiltin TBox, _)
        | TLiteral (TInteger (Isize | Usize)) ),
        ( TRef _ | TRawPtr _
        | TAdt (TBuiltin TBox, _)
        | TLiteral (TInteger (Isize | Usize)) ),
        (Ptr _ | Base _) ) ->
        ok v
    | _ ->
        let open Syntaxes.Option in
        let ( |>** ) = Result.bind in
        let size_of ty = (Layout.layout_of ty).size in
        let int_of_val v =
          match Typed.kind v with
          | Int v -> Z.to_int v
          | _ -> failwith "Expected a concrete integer"
        in
        (* to make our life easier, we check for concrete offsets in the layout; this should
           always be true anyways. *)
        let blocks = rust_to_cvals v from_ty in
        let** blocks =
          try
            Result.ok
            @@ List.map
                 (fun { value; ty; offset } -> (value, ty, int_of_val offset))
                 blocks
          with _ -> not_impl "Symbolic offset in layout"
        in
        let extract_block (ty, off) =
          (* 1. ideal case, we find a block with the same size and offset *)
          let- () =
            List.find_map
              (fun (v, ty', o) ->
                let off = int_of_val off in
                if o = off && size_of ty = size_of ty' then
                  Some (transmute ~from_ty:ty' ~to_ty:ty v)
                else None)
              blocks
          in
          (* X. give up *)
          let pp_triple fmt (v, ty, o) =
            Fmt.pf fmt "(%a:%a, %d)" ppa_rust_val v pp_ty ty o
          in
          Fmt.kstr not_impl "Transmute: Couldn't extract %a at %a from %a" pp_ty
            ty Typed.ppa off
            Fmt.(list ~sep:comma pp_triple)
            blocks
        in
        let rec aux = function
          | `Done v -> Result.ok v
          | `More (blocks, callback) ->
              Result.fold_list blocks ~init:[] ~f:(fun acc block ->
                  let++ block = extract_block block in
                  block :: acc)
              |>** callback
              |>** aux
        in
        aux @@ rust_of_cvals to_ty
