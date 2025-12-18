module Frontend = Soteria_rust_lib.Frontend
module Crate = Soteria_rust_lib.Crate
open Charon.Types

type t = {
  constructors : Frontend.fun_decl list;
  fun_decls : Frontend.fun_decl list;
  drops : Frontend.fun_decl TypeDeclId.Map.t;
}

let get () =
  let crate = Crate.get_crate () in
  let can_infer (fun_decl : Frontend.fun_decl) =
    fun_decl.item_meta.is_local
    && (not fun_decl.signature.is_unsafe)
    && (fun_decl.item_meta.attr_info.public || not !Config.current.only_public)
  in
  let is_constructor (fun_decl : Frontend.fun_decl) =
    let is_base ty = match ty with TLiteral _ -> true | _ -> false in
    List.fold_left (fun b ty -> b && is_base ty) true fun_decl.signature.inputs
  in
  let update_drops (fun_decl : Frontend.fun_decl) library =
    match List.hd fun_decl.signature.inputs with
    | TRef (_, TAdt { id = TAdtId id; _ }, _) ->
        { library with drops = TypeDeclId.Map.add id fun_decl library.drops }
    | _ -> failwith "Library with invalid drop signature"
  in
  let update_fun_decls (fun_decl : Frontend.fun_decl) library =
    if is_constructor fun_decl then
      { library with constructors = fun_decl :: library.constructors }
    else { library with fun_decls = fun_decl :: library.fun_decls }
  in
  FunDeclId.Map.fold
    (fun _ (fun_decl : Frontend.fun_decl) library ->
      match fun_decl.src with
      | TraitImplItem (_, _, "drop", _) -> update_drops fun_decl library
      | _ ->
          if not (can_infer fun_decl) then library
          else update_fun_decls fun_decl library)
    crate.fun_decls
    { constructors = []; fun_decls = []; drops = TypeDeclId.Map.empty }

let infer_summaries ?summ_ctx f library =
  let f summ_ctx acc (fun_decl : Frontend.fun_decl) =
    let wrappers = Wrapper.make library.drops fun_decl summ_ctx in
    ListLabels.fold_left wrappers ~init:acc ~f:(fun acc wrapper ->
        Result.bind acc @@ fun summ_ctx -> f wrapper summ_ctx)
  in
  match summ_ctx with
  | Some summ_ctx ->
      List.fold_left (f summ_ctx) (Result.ok summ_ctx) library.fun_decls
  | None ->
      let summ_ctx = Summary.Context.empty in
      List.fold_left (f summ_ctx) (Result.ok summ_ctx) library.constructors
