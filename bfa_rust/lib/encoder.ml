open Utils
open Charon
open Typed
open Typed.Syntax
open Typed.Infix
open T
open Charon_util
open Rustsymex
open Rustsymex.Syntax
module Sptr = Sptr.ArithPtr
open Layout

type cval_info = {
  value : Sptr.t rust_val;
  ty : Types.literal_type;
  offset : sint Typed.t;
}

(** Converts a Rust value of the given type into a list of C values, along with
    their size and offset *)
let rec rust_to_cvals ?(offset = 0s) (v : Sptr.t rust_val) (ty : Types.ty) :
    cval_info list =
  let illegal_pair () =
    L.err (fun m ->
        m "Wrong pair of rust_value and Charon.ty: %a / %a" ppa_rust_val v
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

  match (v, ty) with
  (* Literals *)
  | Base _, TLiteral ty -> [ { value = v; ty; offset } ]
  | Ptr (_, None, _), TLiteral (TInteger (Isize | Usize) as ty) ->
      [ { value = v; ty; offset } ]
  | _, TLiteral _ -> illegal_pair ()
  (* References / Pointers *)
  | Ptr ((_, meta, _) as ptr), TAdt (TBuiltin TBox, { types = [ sub_ty ]; _ })
  | Ptr ((_, meta, _) as ptr), TRef (_, sub_ty, _)
  | Ptr ((_, meta, _) as ptr), TRawPtr (sub_ty, _) -> (
      let value = Ptr (Sptr.with_meta ?meta:None ptr) in
      match (meta, is_fat_ptr sub_ty) with
      | _, false -> [ { value; ty = TInteger Isize; offset } ]
      | Some meta, true ->
          let size = Typed.int Archi.word_size in
          let isize = Values.TInteger Isize in
          [
            { value; ty = isize; offset };
            { value = Base meta; ty = isize; offset = offset +@ size };
          ]
      | None, true -> failwith "Expected a fat pointer, got a thin pointer.")
  (* References / Pointers obtained from casting *)
  | Base _, TAdt (TBuiltin TBox, _) | Base _, TRef _ | Base _, TRawPtr _ ->
      [ { value = v; ty = TInteger Isize; offset } ]
  | _, TAdt (TBuiltin TBox, _) | _, TRawPtr _ | _, TRef _ -> illegal_pair ()
  (* Tuples *)
  | Tuple vs, TAdt (TTuple, { types; _ }) ->
      if List.compare_lengths vs types <> 0 then
        Fmt.failwith "Mistmached rust_val / TTuple lengths: %d/%d"
          (List.length vs) (List.length types)
      else chain_cvals (layout_of ty) vs types
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
  | Enum (disc, vals), TAdt (TAdtId t_id, _) ->
      let type_decl = Session.get_adt t_id in
      let variant =
        match (type_decl.kind, Typed.kind disc) with
        | Enum variants, Int disc_z ->
            List.find
              (fun v -> Z.equal disc_z Types.(v.discriminant.value))
              variants
        | _ ->
            Fmt.failwith "Unexpected ADT type or discr for enum: %a"
              Types.pp_type_decl type_decl
      in
      let disc_ty = Types.TLiteral (TInteger variant.discriminant.int_ty) in
      chain_cvals (of_variant variant) (Base disc :: vals)
        (disc_ty :: field_tys variant.fields)
  | Enum _, _ -> illegal_pair ()
  (* Arrays *)
  | Array vals, TAdt (TBuiltin TArray, { types = [ sub_ty ]; _ }) ->
      let layout = layout_of ty in
      let size = Array.length layout.members_ofs in
      if List.length vals <> size then failwith "Array length mismatch"
      else chain_cvals layout vals (List.init size (fun _ -> sub_ty))
  | Array _, _ | _, TAdt (TBuiltin TArray, _) -> illegal_pair ()
  (* Rest *)
  | _ ->
      L.err (fun m ->
          m "Unhandled rust_value and Charon.ty: %a / %a" (pp_rust_val Sptr.pp)
            v Types.pp_ty ty);
      failwith "Unhandled rust_value and Charon.ty"

type aux_ret = (Types.literal_type * sint Typed.t) list * parse_callback
and parse_callback = Sptr.t rust_val list -> callback_return
and parser_return = [ `Done of Sptr.t rust_val | `More of aux_ret ]
and callback_return = parser_return Rustsymex.t

(** Converts a Rust type into a list of C blocks, along with their
    offset; once these are read, symbolically decides whether we must keep
    reading. @offset is the initial offset to read from, @meta is the optional
    metadata, that originates from a fat pointer. *)
let rust_of_cvals ?offset ?meta ty : parser_return =
  (* Base case, parses all types. *)
  let rec aux offset : Types.ty -> parser_return = function
    | TLiteral ty ->
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
          | [ Ptr ptr; Base meta ] ->
              return @@ `Done (Ptr (Sptr.with_meta ~meta ptr))
          | _ -> failwith "Expected a pointer and base"
        in
        let ptr_size = Typed.int Archi.word_size in
        let isize = Values.TInteger Isize in
        `More ([ (isize, offset); (isize, offset +@ ptr_size) ], callback)
    | TAdt (TBuiltin TBox, _) | TRef _ | TRawPtr _ ->
        `More
          ( [ (TInteger Isize, offset) ],
            function
            | [ ((Ptr _ | Base _) as ptr) ] -> return (`Done ptr)
            | _ -> failwith "Expected a pointer or base" )
    | TAdt (TTuple, { types; _ }) as ty ->
        let layout = layout_of ty in
        aux_fields ~f:(fun fs -> Tuple fs) ~layout offset types
    | TAdt (TAdtId t_id, _) as ty -> (
        let type_decl = Session.get_adt t_id in
        match (type_decl : Types.type_decl) with
        | { kind = Enum variants; _ } -> aux_enum offset variants
        | { kind = Struct fields; _ } ->
            let layout = layout_of ty in
            fields
            |> field_tys
            |> aux_fields ~f:(fun fs -> Struct fs) ~layout offset
        | _ ->
            Fmt.failwith "Unhandled type declaration in rust_of_cvals: %a"
              Types.pp_type_decl type_decl)
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
    | ty -> Fmt.failwith "Unhandled Charon.ty: %a" Types.pp_ty ty
  (* Parses a list of fields (for structs and tuples) *)
  and aux_fields ~f ~layout offset fields : parser_return =
    let base_offset = offset +@ (offset %@ Typed.nonzero layout.align) in
    let rec mk_callback to_parse parsed : parser_return =
      match to_parse with
      | [] -> `Done (f (List.rev parsed))
      | ty :: rest -> (
          let idx = List.length parsed in
          let offset = Array.get layout.members_ofs idx |> Typed.int in
          let offset = base_offset +@ offset in
          match aux offset ty with
          | `More (blocks, callback) ->
              wrap_callback rest parsed blocks callback
          | `Done value -> mk_callback rest (value :: parsed))
    and wrap_callback to_parse parsed blocks callback =
      `More
        ( blocks,
          fun values ->
            let+ res = callback values in
            match res with
            | `More (blocks, callback) ->
                wrap_callback to_parse parsed blocks callback
            | `Done value -> mk_callback to_parse (value :: parsed) )
    in
    mk_callback fields []
  (* Parses what enum variant we're handling *)
  and aux_enum offset (variants : Types.variant list) : parser_return =
    let disc = (List.hd variants).discriminant in
    let disc_ty = Values.TInteger disc.int_ty in
    let disc_align = Typed.nonzero (align_of_literal_ty disc_ty) in
    let offset = offset +@ (offset %@ disc_align) in
    let callback cval : callback_return =
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
    `More ([ (TInteger disc.int_ty, offset) ], callback)
  in
  let off = Option.value ~default:0s offset in
  aux off ty
