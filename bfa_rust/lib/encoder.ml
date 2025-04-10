open Utils
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
    List_ex.map2i
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

type 'ptr aux_ret = (Types.ty * sint Typed.t) list * 'ptr parse_callback
and 'ptr parse_callback = 'ptr rust_val list -> 'ptr callback_return
and 'ptr parser_return = [ `Done of 'ptr rust_val | `More of 'ptr aux_ret ]
and 'ptr callback_return = 'ptr parser_return Rustsymex.t

(** Converts a Rust type into a list of C blocks, along with their offset; once
    these are read, symbolically decides whether we must keep reading. [offset]
    is the initial offset to read from, [meta] is the optional metadata, that
    originates from a fat pointer. *)
let rust_of_cvals ?offset ?meta ty : 'ptr parser_return =
  (* Base case, parses all types. *)
  let rec aux offset : Types.ty -> 'ptr parser_return = function
    | TLiteral _ as ty ->
        `More
          ( [ (ty, offset) ],
            function
            | [ ((Base _ | Ptr _) as v) ] -> return (`Done v)
            | _ -> failwith "Expected a base" )
    | TAdt (TBuiltin TBox, { types = [ sub_ty ]; _ })
    | TRef (_, sub_ty, _)
    | TRawPtr (sub_ty, _)
      when is_fat_ptr sub_ty ->
        let callback = function
          | [ Ptr (ptr, None); Base meta ] ->
              return @@ `Done (Ptr (ptr, Some meta))
          | _ -> failwith "Expected a pointer and base"
        in
        let ptr_size = Typed.int Archi.word_size in
        let isize : Types.ty = TLiteral (TInteger Isize) in
        `More ([ (isize, offset); (isize, offset +@ ptr_size) ], callback)
    | TAdt (TBuiltin TBox, _) | TRef _ | TRawPtr _ ->
        `More
          ( [ (TLiteral (TInteger Isize), offset) ],
            function
            | [ ((Ptr _ | Base _) as ptr) ] -> return (`Done ptr)
            | _ -> failwith "Expected a pointer or base" )
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
  and aux_bind ~f parser_ret : 'ptr parser_return =
    match parser_ret with
    | `More (blocks, callback) ->
        let callback v = Rustsymex.map (callback v) (aux_bind ~f) in
        `More (blocks, callback)
    | `Done value -> f value
  (* util to map a parser result, at the end *)
  and aux_map ~f parser_ret : 'ptr parser_return =
    aux_bind ~f:(fun v -> `Done (f v)) parser_ret
  (* Parses a list of fields (for structs and tuples) *)
  and aux_fields ~f ~layout offset fields : 'ptr parser_return =
    let base_offset = offset +@ (offset %@ Typed.nonzero layout.align) in
    let rec mk_callback to_parse parsed : 'ptr parser_return =
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
  and aux_enum offset (variants : Types.variant list) : 'ptr parser_return =
    let disc = (List.hd variants).discriminant in
    let disc_ty = Values.TInteger disc.int_ty in
    let disc_align = Typed.nonzero (align_of_literal_ty disc_ty) in
    let offset = offset +@ (offset %@ disc_align) in
    let callback cval : 'ptr callback_return =
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
          return parser
      | None ->
          Fmt.kstr not_impl
            "Vanishing rust_of_cvals, as no variant matches variant id %a"
            Typed.ppa cval
    in
    `More ([ (TLiteral disc_ty, offset) ], callback)
  and aux_union offset fs : 'ptr parser_return =
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

let transmute ~(from_ty : Types.ty) ~(to_ty : Types.ty) v =
  let open Bfa_symex.Compo_res in
  let open Result in
  let unhandled () =
    let ctx = PrintUllbcAst.Crate.crate_to_fmt_env @@ Session.get_crate () in
    Fmt.kstr not_impl "Unhandled transmute of %a: %s -> %s" ppa_rust_val v
      (PrintTypes.ty_to_string ctx from_ty)
      (PrintTypes.ty_to_string ctx to_ty)
  in
  match (from_ty, to_ty, v) with
  | TLiteral (TFloat _), TLiteral (TInteger _), Base sv ->
      let+ sv =
        of_opt_not_impl ~msg:"Unsupported: non-float in float-to-int"
        @@ Typed.cast_float sv
      in
      let sv' = Typed.int_of_float sv in
      Ok (Base sv')
  | TLiteral (TInteger _), TLiteral (TFloat f), Base sv ->
      let+ sv = cast_checked sv ~ty:Typed.t_int in
      let fp = Charon_util.float_precision f in
      let sv' = Typed.float_of_int fp sv in
      Ok (Base sv')
  | TLiteral _, TLiteral to_ty, Base sv ->
      let constrs = Layout.constraints to_ty in
      if%sat Typed.conj (constrs sv) then ok v else error `UBTransmute
  | ( (TRef _ | TRawPtr _ | TLiteral (TInteger Usize)),
      (TRef _ | TRawPtr _ | TLiteral (TInteger Usize)),
      (Ptr _ | Base _) ) ->
      ok v
  | _ -> unhandled ()
