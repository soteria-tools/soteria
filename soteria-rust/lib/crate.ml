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

let pp_name ft name =
  let to_str = PrintTypes.name_to_string (as_fmt_env ()) in
  Fmt.pf ft "%s" (to_str name)

let pp_statement ft stt =
  let to_str = PrintUllbcAst.Ast.statement_to_string (as_fmt_env ()) "" in
  Fmt.pf ft "%s" (to_str stt)

let pp_terminator ft term =
  let to_str = PrintUllbcAst.Ast.terminator_to_string (as_fmt_env ()) "" in
  Fmt.pf ft "%s" (to_str term)

let pp_fn_operand ft op =
  let to_str = PrintUllbcAst.Ast.fn_operand_to_string (as_fmt_env ()) in
  Fmt.pf ft "%s" (to_str op)

let get_adt id =
  let crate = get_crate () in
  match Types.TypeDeclId.Map.find_opt id crate.type_decls with
  | Some adt -> adt
  | None -> raise (MissingDecl "Type")

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

let get_trait_impl id =
  let crate = get_crate () in
  match UllbcAst.TraitImplId.Map.find_opt id crate.trait_impls with
  | Some impl -> impl
  | None -> raise (MissingDecl "TraitImpl")

let is_enum adt_id =
  match (get_adt adt_id).kind with Enum _ -> true | _ -> false

let is_struct adt_id =
  match (get_adt adt_id).kind with Struct _ -> true | _ -> false

let as_enum adt_id =
  match (get_adt adt_id).kind with
  | Enum variants -> variants
  | _ -> failwith "as_struct expected an enum"

let as_struct adt_id =
  match (get_adt adt_id).kind with
  | Struct fields -> fields
  | _ -> failwith "as_struct expected a struct"

let as_union adt_id =
  match (get_adt adt_id).kind with
  | Union fields -> fields
  | _ -> failwith "as_union expected a union"
