module Frontend = Soteria_rust_lib.Frontend
module Crate = Soteria_rust_lib.Crate
open Charon.Types

let ( let* ) = Result.bind
let ( let+ ) x f = Result.map f x

type t = {
  constructors : Frontend.fun_decl list;
  fun_decls : Frontend.fun_decl list;
  drops : Frontend.fun_decl TypeDeclId.Map.t;
}

let get () =
  let crate = Crate.get_crate () in
  let is_drop (fun_decl : Frontend.fun_decl) =
    match fun_decl.src with
    | TraitImplItem (_, _, "drop", _) -> true
    | _ -> false
  in
  let update_drops (fun_decl : Frontend.fun_decl) library =
    match List.hd fun_decl.signature.inputs with
    | TRef (_, TAdt { id = TAdtId id; _ }, _) ->
        { library with drops = TypeDeclId.Map.add id fun_decl library.drops }
    | _ -> failwith "Library with invalid drop signature"
  in
  let can_infer (fun_decl : Frontend.fun_decl) =
    fun_decl.item_meta.is_local
    && (not fun_decl.signature.is_unsafe)
    && (fun_decl.item_meta.attr_info.public || not !Config.current.only_public)
  in
  let is_constructor (fun_decl : Frontend.fun_decl) =
    let is_base ty = match ty with TLiteral _ -> true | _ -> false in
    List.fold_left (fun b ty -> b && is_base ty) true fun_decl.signature.inputs
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

let infer_summaries ?summ_ctx f library =
  let wrap = Wrapper.make library.drops in
  let f summ_ctx acc (fun_decl : Frontend.fun_decl) =
    let wrapper, tys = wrap fun_decl in
    let summs_iter = Summary.Context.iter_summs tys summ_ctx in
    IterLabels.fold summs_iter ~init:acc ~f:(fun acc inputs ->
        let* summ_ctx = acc in
        let+ outputs = f wrapper inputs in
        let update acc (ty, summ) = Summary.Context.add ty summ acc in
        List.fold_left update summ_ctx outputs)
  in
  match summ_ctx with
  | Some summ_ctx ->
      List.fold_left (f summ_ctx) (Result.ok summ_ctx) library.fun_decls
  | None ->
      let summ_ctx = Summary.Context.empty in
      List.fold_left (f summ_ctx) (Result.ok summ_ctx) library.constructors
