open Charon

exception MissingDecl of string

type t = UllbcAst.crate
type _ Effect.t += Get_crate : t Effect.t

let get_crate () = Effect.perform Get_crate

let with_crate (crate : t) f =
  let open Effect.Deep in
  try f () with effect Get_crate, k -> continue k crate

let pointer_size () =
  let crate = get_crate () in
  crate.target_information.target_pointer_size

let as_namematcher_ctx () = NameMatcher.ctx_from_crate (get_crate ())

let as_fmt_env () =
  let crate = get_crate () in
  PrintUllbcAst.Crate.crate_to_fmt_env crate

let[@inline] mk_pp to_string =
 fun ft v -> Fmt.of_to_string (to_string (as_fmt_env ())) ft v

let[@inline] mk_pp_indent to_string =
 fun ft v ->
  let to_str = to_string (as_fmt_env ()) "" in
  Fmt.pf ft "%s" (to_str v)

let pp_constant_expr = mk_pp PrintTypes.constant_expr_to_string
let pp_fn_operand = mk_pp PrintUllbcAst.Ast.fn_operand_to_string
let pp_fun_decl_ref = mk_pp PrintTypes.fun_decl_ref_to_string
let pp_generic_args = mk_pp PrintTypes.generic_args_to_string

let pp_generic_params =
  mk_pp (fun env params ->
      let generics, clauses = PrintTypes.generic_params_to_strings env params in
      String.concat " + " (generics @ clauses))

let pp_global_decl_ref = mk_pp PrintTypes.global_decl_ref_to_string
let pp_name = mk_pp PrintTypes.name_to_string
let pp_place = mk_pp PrintExpressions.place_to_string
let pp_trait_ref = mk_pp PrintTypes.trait_ref_to_string
let pp_statement = mk_pp_indent PrintUllbcAst.Ast.statement_to_string
let pp_terminator = mk_pp_indent PrintUllbcAst.Ast.terminator_to_string

let get_adt_raw id =
  let crate = get_crate () in
  match Types.TypeDeclId.Map.find_opt id crate.type_decls with
  | Some adt -> adt
  | None -> raise (MissingDecl "Type")

(** Gets an ADT in the crate, and applies the given generic arguments to it. *)
let get_adt (adt_ref : Types.type_decl_ref) =
  match adt_ref.id with
  | TAdtId id ->
      let open Substitute in
      let adt = get_adt_raw id in
      let subst =
        make_sb_subst_from_generics adt.generics adt_ref.generics Self
      in
      let subst = subst_at_binder_zero subst in
      st_substitute_visitor#visit_type_decl subst adt
  | TBuiltin _ | TTuple ->
      Fmt.failwith "get_adt: unexpected non-ADT type decl id: %a"
        Types.pp_type_id adt_ref.id

let get_fun id =
  let crate = get_crate () in
  match UllbcAst.FunDeclId.Map.find_opt id crate.fun_decls with
  | Some fn -> fn
  | None -> raise (MissingDecl "Fun")

let get_global id =
  let crate = get_crate () in
  match UllbcAst.GlobalDeclId.Map.find_opt id crate.global_decls with
  | Some global -> global
  | None -> raise (MissingDecl "Global")

let get_trait_impl_raw id =
  let crate = get_crate () in
  match UllbcAst.TraitImplId.Map.find_opt id crate.trait_impls with
  | Some impl -> impl
  | None -> raise (MissingDecl "TraitImpl")

let get_trait_impl (timplref : Types.trait_impl_ref) =
  let open Substitute in
  let impl = get_trait_impl_raw timplref.id in
  let subst =
    make_sb_subst_from_generics impl.generics timplref.generics Self
  in
  let subst = subst_at_binder_zero subst in
  st_substitute_visitor#visit_trait_impl subst impl

let get_trait_decl_raw id =
  let crate = get_crate () in
  match Types.TraitDeclId.Map.find_opt id crate.trait_decls with
  | Some trait -> trait
  | None -> raise (MissingDecl "TraitDecl")

let get_trait_decl (trait_ref : Types.trait_decl_ref) =
  let open Substitute in
  let trait = get_trait_decl_raw trait_ref.id in
  let subst =
    make_sb_subst_from_generics trait.generics trait_ref.generics Self
  in
  let subst = subst_at_binder_zero subst in
  st_substitute_visitor#visit_trait_decl subst trait

let is_enum (adt_ref : Types.type_decl_ref) =
  match adt_ref.id with
  | TAdtId id -> (
      match (get_adt_raw id).kind with Enum _ -> true | _ -> false)
  | _ -> false

let is_struct (adt_ref : Types.type_decl_ref) =
  match adt_ref.id with
  | TAdtId id -> (
      match (get_adt_raw id).kind with Struct _ -> true | _ -> false)
  | _ -> false

let is_union (adt_ref : Types.type_decl_ref) =
  match adt_ref.id with
  | TAdtId id -> (
      match (get_adt_raw id).kind with Union _ -> true | _ -> false)
  | _ -> false

let as_enum adt_ref =
  match (get_adt adt_ref).kind with
  | Enum variants -> variants
  | _ -> failwith "as_enum expected an enum"

let as_struct adt_ref =
  match (get_adt adt_ref).kind with
  | Struct fields -> fields
  | _ -> failwith "as_struct expected a struct"

let as_struct_or_tuple (adt_ref : Types.type_decl_ref) =
  match adt_ref.id with
  | TTuple -> adt_ref.generics.types
  | TAdtId _ ->
      as_struct adt_ref |> List.map (fun (f : Types.field) -> f.field_ty)
  | _ -> failwith "as_struct_or_tuple expected a struct or tuple"

let as_union adt_ref =
  match (get_adt adt_ref).kind with
  | Union fields -> fields
  | _ -> failwith "as_union expected a union"
