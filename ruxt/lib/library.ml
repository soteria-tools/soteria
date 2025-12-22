module Config_ = Config
open Soteria_rust_lib
module Config = Config_
open Result.Syntax
open Charon.Types

type t = {
  constructors : Frontend.fun_decl list;
  fun_decls : Frontend.fun_decl list;
  drops : Frontend.fun_decl TypeDeclId.Map.t;
}

let get () =
  let crate = Crate.get_crate () in
  let is_drop ({ src; _ } : Frontend.fun_decl) =
    match src with TraitImplItem (_, _, "drop", _) -> true | _ -> false
  in
  let update_drops (fun_decl : Frontend.fun_decl) library =
    match List.hd fun_decl.signature.inputs with
    | TRef (_, TAdt { id = TAdtId id; _ }, _) ->
        { library with drops = TypeDeclId.Map.add id fun_decl library.drops }
    | _ -> failwith "Library with invalid drop signature"
  in
  let can_infer ({ item_meta; signature; _ } : Frontend.fun_decl) =
    item_meta.is_local
    && (not signature.is_unsafe)
    && (item_meta.attr_info.public || not !Config.current.only_public)
  in
  let is_constructor ({ signature = { inputs; _ }; _ } : Frontend.fun_decl) =
    List.for_all Summary.Context.is_base_ty inputs
  in
  let update_fun_decls (fun_decl : Frontend.fun_decl) library =
    if is_constructor fun_decl then
      { library with constructors = fun_decl :: library.constructors }
    else { library with fun_decls = fun_decl :: library.fun_decls }
  in
  FunDeclId.Map.fold
    (fun _ (fun_decl : Frontend.fun_decl) library ->
      if is_drop fun_decl then update_drops fun_decl library
      else if can_infer fun_decl then update_fun_decls fun_decl library
      else library)
    crate.fun_decls
    { constructors = []; fun_decls = []; drops = TypeDeclId.Map.empty }

let infer_summaries ?summ_ctx ~fuel library : (Summary.Context.t, 'a) result =
  (* No context provided: initialize first context with constructors *)
  let summ_ctx, fun_decls =
    match summ_ctx with
    | Some summ_ctx -> (summ_ctx, library.fun_decls)
    | None -> (Summary.Context.empty, library.constructors)
  in
  (* Set drop context for creating wrappers *)
  let wrap = Wrapper.make library.drops in
  (* Set fuel for executing wrappers *)
  let exec = Wrapper.exec ~fuel in
  (* Infer summaries and prune summary context  *)
  let+ summ_ctx =
    Result.fold_list fun_decls ~init:summ_ctx ~f:(fun summ_ctx fun_decl ->
        let wrapper, tys = wrap fun_decl in
        (* Iterate over snapshot of current summary context *)
        let snapshot = Summary.Context.iter_summs tys summ_ctx in
        Result.fold_iter snapshot ~init:summ_ctx ~f:(fun summ_ctx inputs ->
            (* Stage update to summary context with inferred summaries *)
            let+ outputs = exec wrapper inputs in
            ListLabels.fold_left outputs ~init:summ_ctx
              ~f:(fun ctx (ty, summ) -> Summary.Context.stage ty summ ctx)))
  in
  (* Commit update with new summaries *)
  Summary.Context.commit summ_ctx
