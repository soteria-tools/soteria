open Charon

exception MissingDecl of string

type t = UllbcAst.crate

(* These computations are pure and safe to cache accross execution, as long as
   it is within the same crate. *)
type decl_cache = {
  adt : (Types.type_decl_ref, Types.type_decl) Hashtbl.t;
  trait_impl : (Types.trait_impl_ref, GAst.trait_impl) Hashtbl.t;
  trait_decl : (Types.trait_decl_ref, GAst.trait_decl) Hashtbl.t;
}

type _ Effect.t += Get_crate : t Effect.t | Get_decl_cache : decl_cache Effect.t

(* DLS-backed cache for the active crate / decl cache. These are constants for
   the duration of [with_crate], but [Effect.perform] is much heavier than a DLS
   read and the get_crate path is on the symex hot path. *)
let crate_dls : t option Domain.DLS.key = Domain.DLS.new_key (fun () -> None)

let decl_cache_dls : decl_cache option Domain.DLS.key =
  Domain.DLS.new_key (fun () -> None)

let[@inline] get_crate () =
  match Domain.DLS.get crate_dls with
  | Some c -> c
  | None -> Effect.perform Get_crate

let[@inline] get_decl_cache () =
  match Domain.DLS.get decl_cache_dls with
  | Some dc -> dc
  | None -> Effect.perform Get_decl_cache

let memoize tbl key compute =
  match Hashtbl.find_opt tbl key with
  | Some v -> v
  | None ->
      let v = compute () in
      Hashtbl.add tbl key v;
      v

(* Cache for [pointer_size]; populated by [with_crate]. The value is constant
   over a single symex run, but each [pointer_size ()] call would otherwise
   perform a [Get_crate] effect — millions of times on Rust hot paths. The DLS
   makes the cache safe across parallel [Symex.run] calls in different
   domains. *)
let pointer_size_dls =
  Domain.DLS.new_key ~split_from_parent:Fun.id (fun () -> 8)

let with_crate (crate : t) f =
  let open Effect.Deep in
  let dc =
    {
      adt = Hashtbl.create 256;
      trait_impl = Hashtbl.create 64;
      trait_decl = Hashtbl.create 64;
    }
  in
  assert (List.length crate.target_information = 1);
  let _, info = List.hd crate.target_information in
  (* We save the values just in case, for some unknown reason, [with_crate] is
     called within itself. *)
  let saved_ps = Domain.DLS.get pointer_size_dls in
  let saved_crate = Domain.DLS.get crate_dls in
  let saved_dc = Domain.DLS.get decl_cache_dls in
  Domain.DLS.set pointer_size_dls info.target_pointer_size;
  Domain.DLS.set crate_dls (Some crate);
  Domain.DLS.set decl_cache_dls (Some dc);
  let finally () =
    Domain.DLS.set pointer_size_dls saved_ps;
    Domain.DLS.set crate_dls saved_crate;
    Domain.DLS.set decl_cache_dls saved_dc
  in
  Fun.protect ~finally @@ fun () ->
  try f () with
  | effect Get_crate, k -> continue k crate
  | effect Get_decl_cache, k -> continue k dc

let[@inline] pointer_size () = Domain.DLS.get pointer_size_dls
let as_namematcher_ctx () = NameMatcher.ctx_from_crate (get_crate ())

let as_fmt_env () =
  let crate = get_crate () in
  Print.crate_to_fmt_env crate

let[@inline] mk_pp pp = fun ft v -> (pp (as_fmt_env ())) ft v
let[@inline] mk_pp_indent pp = fun ft v -> pp (as_fmt_env ()) "" ft v
let pp_constant_expr = mk_pp PrintFmt.pp_constant_expr
let pp_fn_operand = mk_pp PrintFmt.pp_fn_operand
let pp_fun_decl_ref = mk_pp PrintFmt.pp_fun_decl_ref
let pp_generic_args = mk_pp PrintFmt.pp_generic_args
let pp_operand = mk_pp PrintFmt.pp_operand
let pp_generic_params = mk_pp PrintFmt.pp_generic_params_single_line
let pp_global_decl_ref = mk_pp PrintFmt.pp_global_decl_ref
let pp_name = mk_pp PrintFmt.pp_name
let pp_place = mk_pp PrintFmt.pp_place
let pp_trait_impl_ref = mk_pp PrintFmt.pp_trait_impl_ref
let pp_trait_ref = mk_pp PrintFmt.pp_trait_ref
let pp_statement = mk_pp_indent PrintFmt.Ullbc.pp_statement
let pp_terminator = mk_pp_indent PrintFmt.Ullbc.pp_terminator

let get_adt_raw id =
  let crate = get_crate () in
  match Types.TypeDeclId.Map.find_opt id crate.type_decls with
  | Some adt -> adt
  | None -> raise (MissingDecl "Type")

(** Gets an ADT in the crate, and applies the given generic arguments to it. *)
let get_adt (adt_ref : Types.type_decl_ref) =
  match adt_ref.id with
  | TAdtId id ->
      memoize (get_decl_cache ()).adt adt_ref @@ fun () ->
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
  memoize (get_decl_cache ()).trait_impl timplref @@ fun () ->
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
  memoize (get_decl_cache ()).trait_decl trait_ref @@ fun () ->
  let open Substitute in
  let trait = get_trait_decl_raw trait_ref.id in
  let subst =
    make_sb_subst_from_generics trait.generics trait_ref.generics Self
  in
  let subst = subst_at_binder_zero subst in
  st_substitute_visitor#visit_trait_decl subst trait

let get_assoc_type_name (trait_ref : Types.trait_ref) type_id =
  Charon.GAstUtils.get_assoc_type_name (get_crate ())
    trait_ref.trait_decl_ref.binder_value.id type_id

let get_method_name (trait_ref : Types.trait_ref) method_id =
  Charon.GAstUtils.get_method_name (get_crate ())
    trait_ref.trait_decl_ref.binder_value.id method_id

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

let is_struct_or_tuple (adt_ref : Types.type_decl_ref) =
  match adt_ref.id with
  | TTuple -> true
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
