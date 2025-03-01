open Charon
open Rustsymex.Syntax
open Typed.Infix
open Typed.Syntax
open Rustsymex
open Charon_util

exception CantComputeLayout of Types.ty

module Archi = struct
  let word_size = 8
end

module Session = struct
  let current_crate : UllbcAst.crate ref =
    ref
      UllbcAst.
        {
          name = "";
          options =
            {
              ullbc = true;
              lib = false;
              bin = None;
              mir_promoted = false;
              mir_optimized = false;
              crate_name = None;
              input_file = None;
              read_llbc = None;
              dest_dir = None;
              dest_file = None;
              use_polonius = false;
              no_code_duplication = false;
              extract_opaque_bodies = false;
              translate_all_methods = false;
              included = [];
              opaque = [];
              exclude = [];
              remove_associated_types = [];
              hide_marker_traits = false;
              no_cargo = false;
              rustc_args = [];
              cargo_args = [];
              monomorphize = true;
              skip_borrowck = true;
              abort_on_error = false;
              error_on_warnings = false;
              no_serialize = false;
              print_original_ullbc = false;
              print_ullbc = false;
              print_built_llbc = false;
              print_llbc = false;
              no_merge_goto_chains = false;
              only_cargo = false;
            };
          declarations = [];
          type_decls = Types.TypeDeclId.Map.empty;
          fun_decls = FunDeclId.Map.empty;
          global_decls = GlobalDeclId.Map.empty;
          trait_decls = TraitDeclId.Map.empty;
          trait_impls = TraitImplId.Map.empty;
        }

  let set_crate = ( := ) current_crate
  let get_crate () = !current_crate
end

type layout = {
  size : int;
  align : int;
  members_ofs : (int * int) Array.t;
      (** Array of (member-index; offset in layout) *)
}

type sint = Typed.T.sint Typed.t
type cval = Typed.T.cval Typed.t
type sbool = Typed.T.sbool Typed.t

let is_int : Types.ty -> bool = function
  | TLiteral (TInteger _) -> true
  | _ -> false

let to_zeros : Types.literal_type -> cval = function
  | TInteger _ -> 0s
  | t ->
      Fmt.failwith "to_zeros: unsupported literal type %a" Types.pp_literal_type
        t

let size_of_int_ty : Types.integer_type -> int = function
  | I128 | U128 -> 16
  | I64 | U64 -> 8
  | I32 | U32 -> 4
  | I16 | U16 -> 2
  | I8 | U8 -> 1
  | Isize | Usize -> Archi.word_size

let size_of_literal_ty : Types.literal_type -> int = function
  | TInteger int_ty -> size_of_int_ty int_ty
  | TBool -> 1
  | ty -> Fmt.failwith "Unspported literal type %a" Types.pp_literal_type ty

(* TODO: this is not really accurate, but good enough for now.
   See https://doc.rust-lang.org/reference/type-layout.html#r-layout.primitive.align *)
let align_of_int_ty : Types.integer_type -> int = size_of_int_ty
let empty_generics = TypesUtils.empty_generic_args

let rec layout_of : Types.ty -> layout = function
  | TLiteral (TInteger inty) ->
      let size = size_of_int_ty inty in
      let align = align_of_int_ty inty in
      { size; align; members_ofs = [||] }
  | TLiteral TBool -> { size = 1; align = 1; members_ofs = [||] }
  | TAdt (TTuple, g) when g = empty_generics ->
      (* unit () *)
      (* TODO: this actually has size 0, which makes things... awkward *)
      { size = 1; align = 1; members_ofs = [||] }
  | TAdt (TAdtId id, g) when g = empty_generics -> (
      let crate = Session.get_crate () in
      let adt = Types.TypeDeclId.Map.find id crate.type_decls in
      match adt with
      | { kind = Struct fields; _ } ->
          let fields = List.map (fun (f : Types.field) -> f.field_ty) fields in
          layout_of_members fields
      | { kind = Enum variants; _ } ->
          (* TODO: empty enums? *)
          (* assume all discriminants are of equal size (should be ok?) *)
          let layouts =
            List.map
              (fun ({ fields; discriminant; _ } : Types.variant) ->
                let layout =
                  List.map (fun (f : Types.field) -> f.field_ty) fields
                  |> layout_of_members
                in
                let disc_size = size_of_int_ty discriminant.int_ty in
                { layout with size = layout.size + disc_size })
              variants
          in
          List.fold_left
            (fun acc l -> if l.size > acc.size then l else acc)
            (List.hd layouts) (List.tl layouts)
      | { kind; _ } ->
          Fmt.pr "Unspported ADT kind %a" Types.pp_type_decl_kind kind;
          failwith "Unsupported ADT kind")
  | TRef (_, _, _) ->
      { size = Archi.word_size; align = Archi.word_size; members_ofs = [||] }
  | ty ->
      L.debug (fun m -> m "Cannot compute layout of %a" Types.pp_ty ty);
      raise (CantComputeLayout ty)

and layout_of_members members =
  let rec aux i members_ofs (layout : layout) = function
    | [] -> (List.rev members_ofs, layout)
    | ty :: rest ->
        let { size = curr_size; align = curr_align; members_ofs = _ } =
          layout
        in
        let { size; align; _ } = layout_of ty in
        let mem_ofs = curr_size + (curr_size mod align) in
        let new_size = mem_ofs + size in
        let new_align = Int.max align curr_align in
        aux (i + 1)
          ((i, mem_ofs) :: members_ofs)
          { size = new_size; align = new_align; members_ofs = [||] }
          rest
  in
  let members_ofs, { size; align; members_ofs = _ } =
    aux 0 [] { size = 0; align = 1; members_ofs = [||] } members
  in
  {
    size = size + (size mod align);
    align;
    members_ofs = Array.of_list members_ofs;
  }

let size_of_s ty =
  try
    let { size; _ } = layout_of ty in
    return (Typed.int size)
  with CantComputeLayout ty ->
    Fmt.kstr Rustsymex.not_impl "Cannot yet compute size of type %a" Types.pp_ty
      ty

let min_value : Types.integer_type -> [> Typed.T.sint ] Typed.t = function
  | U128 | U64 | U32 | U16 | U8 | Usize -> 0s
  | Isize ->
      Typed.int_z (Z.neg (Z.shift_left Z.one ((8 * Archi.word_size) - 1)))
  | I128 -> Typed.int_z (Z.neg (Z.shift_left Z.one 127))
  | I64 -> Typed.int_z (Z.neg (Z.shift_left Z.one 63))
  | I32 -> Typed.int_z (Z.neg (Z.shift_left Z.one 31))
  | I16 -> Typed.int_z (Z.neg (Z.shift_left Z.one 15))
  | I8 -> Typed.int_z (Z.neg (Z.shift_left Z.one 7))

let max_value : Types.integer_type -> [> Typed.T.sint ] Typed.t = function
  | U128 -> Typed.int_z (Z.pred (Z.shift_left Z.one 128))
  | U64 -> Typed.int_z (Z.pred (Z.shift_left Z.one 64))
  | U32 -> Typed.int_z (Z.pred (Z.shift_left Z.one 32))
  | U16 -> Typed.int_z (Z.pred (Z.shift_left Z.one 16))
  | U8 -> Typed.int_z (Z.pred (Z.shift_left Z.one 8))
  | Usize -> Typed.int_z (Z.pred (Z.shift_left Z.one (8 * Archi.word_size)))
  | I128 -> Typed.int_z (Z.pred (Z.shift_left Z.one 127))
  | I64 -> Typed.int_z (Z.pred (Z.shift_left Z.one 63))
  | I32 -> Typed.int_z (Z.pred (Z.shift_left Z.one 31))
  | I16 -> Typed.int_z (Z.pred (Z.shift_left Z.one 15))
  | I8 -> Typed.int_z (Z.pred (Z.shift_left Z.one 7))
  | Isize ->
      Typed.int_z (Z.pred (Z.shift_left Z.one ((8 * Archi.word_size) - 1)))

let int_constraints ty =
  let min = min_value ty in
  let max = max_value ty in
  fun x -> [ min <=@ x; x <=@ max ]

let constraints : Types.literal_type -> (cval -> sbool list) option = function
  | TInteger ity ->
      let constrs = int_constraints ity in
      Some
        (fun x ->
          match Typed.cast_checked x Typed.t_int with
          | None -> [ Typed.v_false ]
          | Some x -> constrs x)
  | ty ->
      L.info (fun m ->
          m "No constraints implemented for type %a" Types.pp_literal_type ty);
      None

let nondet_literal_ty : Types.literal_type -> cval Rustsymex.t =
  let open Rustsymex.Syntax in
  function
  | TInteger ity ->
      let constrs = int_constraints ity in
      let+ res = Rustsymex.nondet ~constrs Typed.t_int in
      (res :> cval)
  | ty ->
      Rustsymex.not_impl
        (Fmt.str "nondet_literal_ty: unsupported type %a" Types.pp_literal_type
           ty)

let rec nondet : Types.ty -> rust_val Rustsymex.t =
  let open Rustsymex.Syntax in
  function
  | TLiteral lit ->
      let+ cval = nondet_literal_ty lit in
      Base cval
  | TAdt (TAdtId t_id, _) -> (
      let crate = Session.get_crate () in
      let type_decl =
        Types.TypeDeclId.Map.find t_id UllbcAst.(crate.type_decls)
      in
      match type_decl.kind with
      | Enum variants -> (
          let disc_ty = (List.hd variants).discriminant.int_ty in
          let* disc_val = nondet_literal_ty (Values.TInteger disc_ty) in
          let* res =
            Rustsymex.fold_list variants ~init:None
              ~f:(fun value (variant : Types.variant) ->
                match value with
                | Some _ -> return value
                | None ->
                    let d = value_of_scalar variant.discriminant in
                    if%sat disc_val ==@ d then
                      let* fields =
                        Rustsymex.fold_list variant.fields ~init:[]
                          ~f:(fun fields ty ->
                            let+ f = nondet ty.field_ty in
                            f :: fields)
                      in
                      let fields = List.rev fields in
                      return (Some (Enum (d, fields)))
                    else return None)
          in
          match res with None -> vanish () | Some value -> return value)
      | ty ->
          Rustsymex.not_impl
            (Fmt.str "nondet: unsupported type %a" Types.pp_type_decl_kind ty))
  | ty ->
      Rustsymex.not_impl (Fmt.str "nondet: unsupported type %a" Types.pp_ty ty)

(** Converts a Rust value of the given type into a list of C values, along with
    their size and offset *)
let rec rust_to_cvals (v : rust_val) (ty : Types.ty) :
    (cval * Types.literal_type * sint * sint) list =
  let crate = Session.get_crate () in
  match (v, ty) with
  | Base v, TLiteral ty ->
      [ (v, ty, Typed.int (size_of_literal_ty ty), Typed.zero) ]
  | Tuple [], TAdt (TTuple, g) when g = empty_generics -> []
  | Enum (disc, vals), TAdt (TAdtId t_id, _) ->
      let type_decl =
        Types.TypeDeclId.Map.find t_id UllbcAst.(crate.type_decls)
      in
      let disc_z =
        match Typed.kind disc with
        | Int d -> d
        | k ->
            Fmt.failwith "Unexpected enum discriminant kind: %a"
              Svalue.pp_t_kind k
      in
      let variant =
        match (type_decl : Types.type_decl) with
        | { kind = Enum variants; _ } ->
            List.find
              (fun v -> Types.(Z.equal disc_z v.discriminant.value))
              variants
        | _ ->
            Fmt.failwith "Unexpected type declaration in enum aggregate: %a"
              Types.pp_type_decl type_decl
      in
      let disc_ty = Values.TInteger variant.discriminant.int_ty in
      let disc_size = Typed.int (size_of_literal_ty disc_ty) in
      let disc_val = (disc, disc_ty, disc_size, 0s) in
      let variant_tys = List.map (fun f -> Types.(f.field_ty)) variant.fields in
      let rest = List.map2 rust_to_cvals vals variant_tys in
      let open Typed.Infix in
      let cvals =
        List.rev rest
        |> List.fold_left_map
             (fun off vals ->
               let _, _, l_size, l_off = List.hd (List.rev vals) in
               let off' = off +@ l_off +@ l_size in
               let vals =
                 List.map (fun (v, t, s, o) -> (v, t, s, o +@ off)) vals
               in
               (off', vals))
             disc_size
        |> snd
        |> List.flatten
      in
      disc_val :: cvals
  | _ ->
      Fmt.failwith "Unhandled / Mismatched rust_value and Charon.ty: %a / %a"
        pp_rust_val v Types.pp_ty ty

type aux_ret = (Types.literal_type * sint * sint) list * parse_callback
and parse_callback = cval list -> callback_return
and callback_return = [ `Done of rust_val | `More of aux_ret ] Rustsymex.t

(** Converts a Rust type into a list of C blocks, along with their size and
    offset; once these are read, symbolically decides whether we must keep
    reading. *)
let rust_of_cvals ?offset ty =
  let crate = Session.get_crate () in

  (* Base case, parses all types. *)
  let rec aux offset : Types.ty -> aux_ret = function
    | TLiteral ty ->
        ( [ (ty, Typed.int (size_of_literal_ty ty), offset) ],
          (* TODO: conversions? *)
          fun v ->
            if Stdlib.not (List.length v = 1) then failwith "Expected one cval"
            else return (`Done (Base (List.hd v))) )
    | TAdt (TTuple, g) when g = empty_generics ->
        ([], fun _ -> return (`Done (Tuple [])))
    | TAdt (TAdtId t_id, _) ->
        let type_decl =
          Types.TypeDeclId.Map.find t_id UllbcAst.(crate.type_decls)
        in
        let variants =
          match (type_decl : Types.type_decl) with
          | { kind = Enum variants; _ } -> variants
          | _ ->
              Fmt.failwith "Unexpected type declaration in enum aggregate: %a"
                Types.pp_type_decl type_decl
        in
        let disc = (List.hd variants).discriminant in
        let disc_size = Typed.int (size_of_int_ty disc.int_ty) in
        let next_offset = offset +@ disc_size in
        let callback cval : callback_return =
          let cval =
            match cval with
            | [ cval ] -> cval
            | _ -> failwith "Expected one cval"
          in
          let* res =
            Rustsymex.fold_list variants ~init:None
              ~f:(fun acc (var : Types.variant) ->
                match acc with
                | Some _ -> return acc
                | None ->
                    let var_disc = value_of_scalar var.discriminant in
                    if%sat var_disc ==@ cval then aux_enum next_offset var
                    else return None)
          in
          match res with None -> vanish () | Some res -> return res
        in
        ([ (TInteger disc.int_ty, disc_size, offset) ], callback)
    | ty -> Fmt.failwith "Unhandled Charon.ty: %a" Types.pp_ty ty
  (* Parses an enum variant *)
  and aux_enum offset (var : Types.variant) =
    let calc_offset = function
      | [] -> 0s
      | blocks ->
          let _, size, offset = List.hd @@ List.rev blocks in
          offset +@ size
    in
    (* Recursively keep doing callbacks until we're 100% finished.
      Requires parsing each field as many times as needed, and putting
      all the fields together in the end. *)
    let rec mk_callback next_offset to_parse parsed (callback : parse_callback)
        cvals : callback_return =
      let+ res = callback cvals in
      match res with
      | `Done f -> (
          let parsed = if f = Never then parsed else f :: parsed in
          match to_parse with
          | [] ->
              let disc_cval = value_of_scalar var.discriminant in
              `Done (Enum (disc_cval, List.rev parsed))
          | ty :: rest ->
              let blocks, callback = aux next_offset ty in
              let next_off = calc_offset blocks in
              `More (blocks, mk_callback next_off rest parsed callback))
      | `More (blocks, callback) ->
          let next_off = calc_offset blocks in
          `More (blocks, mk_callback next_off to_parse parsed callback)
    in
    (* Little trick to avoid having to special case empty enums *)
    let empty_callback _ = return (`Done Never) in
    let field_tys = List.rev_map (fun f -> Types.(f.field_ty)) var.fields in
    return (Some (`More ([], mk_callback offset field_tys [] empty_callback)))
  in
  let off = Option.value ~default:0s offset in
  aux off ty
