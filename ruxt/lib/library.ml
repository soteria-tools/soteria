module Config_ = Config
open Soteria_rust_lib
module Config = Config_
open Result.Syntax
open Charon.Types

type t = {
  constructors : (Wrapper.t * ty list) list;
  fun_decls : (Wrapper.t * ty list) list;
}

let get () =
  let crate = Crate.get_crate () in
  let is_drop ({ src; _ } : Frontend.fun_decl) =
    match src with TraitImplItem (_, _, "drop", _) -> true | _ -> false
  in
  let update_drops (fun_decl : Frontend.fun_decl)
      (constructors, fun_decls, drops) =
    match List.hd fun_decl.signature.inputs with
    | TRef (_, TAdt { id = TAdtId id; _ }, _) ->
        (constructors, fun_decls, TypeDeclId.Map.add id fun_decl drops)
    | _ -> failwith "Library with invalid drop signature"
  in
  let can_infer ({ src; item_meta; signature; _ } : Frontend.fun_decl) =
    src = TopLevelItem
    && item_meta.is_local
    && (not signature.is_unsafe)
    && (item_meta.attr_info.public || not (Config.get ()).only_public)
  in
  let is_constructor ({ signature = { inputs; _ }; _ } : Frontend.fun_decl) =
    List.for_all Summary.Context.is_base_ty inputs
  in
  let update_fun_decls (fun_decl : Frontend.fun_decl)
      (constructors, fun_decls, drops) =
    if is_constructor fun_decl then (fun_decl :: constructors, fun_decls, drops)
    else (constructors, fun_decl :: fun_decls, drops)
  in
  let constructors, fun_decls, drops =
    FunDeclId.Map.fold
      (fun _ (fun_decl : Frontend.fun_decl) library ->
        if is_drop fun_decl then update_drops fun_decl library
        else if can_infer fun_decl then update_fun_decls fun_decl library
        else library)
      crate.fun_decls
      ([], [], TypeDeclId.Map.empty)
  in
  let wrap = List.map (Wrapper.make drops) in
  { constructors = wrap constructors; fun_decls = wrap fun_decls }

let infer_summaries ~fuel summ_ctx wrappers =
  (* Infer summaries and prune summary context  *)
  let+ summ_ctx =
    Result.fold_list wrappers ~init:summ_ctx ~f:(fun summ_ctx (wrapper, tys) ->
        (* Iterate over snapshot of current summary context *)
        let snapshot = Summary.Context.iter_summs tys summ_ctx in
        Result.fold_iter snapshot ~init:summ_ctx ~f:(fun summ_ctx inputs ->
            (* Stage update to summary context with inferred summaries *)
            let+ outputs = Wrapper.exec ~fuel wrapper inputs in
            ListLabels.fold_left outputs ~init:summ_ctx
              ~f:(fun ctx (ty, summ) -> Summary.Context.stage ty summ ctx)))
  in
  (* Commit update with new summaries *)
  Summary.Context.commit summ_ctx

let init_summaries ~fuel library =
  infer_summaries ~fuel Summary.Context.empty library.constructors

let infer_summaries ~fuel summ_ctx library =
  infer_summaries ~fuel summ_ctx library.fun_decls
