module Rustsymex = Soteria_rust_lib.Rustsymex
module Rust_val = Soteria_rust_lib.Rust_val
module Wpst_interp = Soteria_rust_lib.Interp.Make (Heap)
module Compo_res = Soteria_symex.Compo_res
open Rustsymex.Syntax
open Charon.Types

exception ExecutionError of string

let execution_err msg = raise (ExecutionError msg)

module Symok = struct
  let unwrap res =
    Rustsymex.map res (function
      | Compo_res.Ok v -> v
      | _ -> execution_err "Expected Ok")

  let load ptr ty state = Heap.load ptr ty state |> unwrap

  let store ptr ty rv state =
    let+ (), state = Heap.store ptr ty rv state |> unwrap in
    state

  let alloc_ty ty state = Heap.alloc_ty ty state |> unwrap

  let alloc ty rv state =
    let* ptr, state = alloc_ty ty state in
    let+ state = store ptr ty rv state in
    (ptr, state)

  let free ptr state =
    let+ (), state = Heap.free ptr state |> unwrap in
    state

  let exec_fun ~args ~state fundef =
    Wpst_interp.exec_fun ~args ~state fundef |> unwrap
end

module Drop = struct
  type t = Charon.UllbcAst.fun_decl TypeDeclId.Map.t

  let is_impl (fundef : Charon.UllbcAst.fun_decl) =
    match fundef.kind with
    | TraitImplItem (_, _, "drop", _) -> true
    | _ -> false

  let create_ctx () : t =
    let crate = Soteria_rust_lib.Crate.get_crate () in
    let add_drop _ fundef ctx =
      if is_impl fundef then
        match List.hd fundef.signature.inputs with
        | TRef (_, TAdt { id = TAdtId id; _ }, _) ->
            TypeDeclId.Map.add id fundef ctx
        | _ -> ctx
      else ctx
    in
    FunDeclId.Map.fold add_drop crate.fun_decls TypeDeclId.Map.empty

  let find_and_exec ty (ctx : t) ~none ~some =
    match ty with
    | TAdt { id = TAdtId id; _ } -> (
        match TypeDeclId.Map.find_opt id ctx with
        | Some fundef ->
            let* ptr, state = some in
            let* _, state = Symok.exec_fun fundef ~args:[ Ptr ptr ] ~state in
            Symok.free ptr state
        | None -> none)
    | _ -> none
end

let wrap_call (fundef : Charon.UllbcAst.fun_decl) summs =
  (* Produce the initial state with reference-free inputs *)
  let* args, state = Summary.Symex.flatten summs in
  (* Check reference arguments and allocate values on heap *)
  let* args, state, arg_ptrs =
    ListLabels.fold_left2 args fundef.signature.inputs
      ~init:(Rustsymex.return ([], state, []))
      ~f:(fun acc arg ty ->
        let* args, state, arg_ptrs = acc in
        match ty with
        | TRef (_, ty, _) ->
            let+ ptr, state = Symok.alloc ty arg state in
            (Rust_val.Ptr ptr :: args, state, (ty, ptr) :: arg_ptrs)
        | _ -> Rustsymex.return (arg :: args, state, arg_ptrs))
  in
  let args = List.rev args in
  (* Symbolically execute the function call *)
  let++ ret, state = Wpst_interp.exec_fun fundef ~args ~state in
  (fundef.signature.output, ret, state, arg_ptrs)

let wrap_return drop_ctx process =
  (* Obtain the result from the executing the function call *)
  let** ty, ret, state, arg_ptrs = process in
  (* Drop the return value *)
  let drop_ret state =
    Drop.find_and_exec ty drop_ctx ~none:(Rustsymex.return state)
      ~some:(Symok.alloc ty ret state)
  in
  (* Drop a reference argument *)
  let drop_ptr ty ptr state =
    Drop.find_and_exec ty drop_ctx ~none:(Symok.free ptr state)
      ~some:(Rustsymex.return (ptr, state))
  in
  let rec get_rets ?(acc = []) state = function
    | [] -> Rustsymex.Result.ok ((ty, ret, state) :: acc)
    | (ty, ptr) :: arg_ptrs ->
        (* Case 1: we infer nothing from this pointer, so it must be dropped *)
        let* dropped_state = drop_ptr ty ptr state in
        (* Case 2: we will infer something from this pointer and drop the rest *)
        let* ret, state = Symok.load ptr ty state in
        let* state = Symok.free ptr state in
        let* state =
          ListLabels.fold_left arg_ptrs ~init:(drop_ret state)
            ~f:(fun st (ty, ptr) -> Rustsymex.bind st (drop_ptr ty ptr))
        in
        (* We store case 2 in the result and proceed with the state from case 1 *)
        let acc = (ty, ret, state) :: acc in
        get_rets ~acc dropped_state arg_ptrs
  in
  get_rets state arg_ptrs

let leak_check = function
  | [] -> Result.ok ()
  | _ ->
      Fmt.pr "GOT LEAK\n";
      Result.error `MemoryLeak

let try_refute fuel process summ_ctx =
  let ( let** ) = Result.bind in
  (* Symbolically execute the function call *)
  Rustsymex.run ~fuel process
  (* For each successful outcome, the summary context will be updated. *)
  |> ListLabels.fold_left ~init:(Result.ok summ_ctx) ~f:(fun acc -> function
       (* Successful termination: update the summary context *)
       | Compo_res.Ok rets, pcs ->
           ListLabels.fold_left rets ~init:acc ~f:(fun acc (ty, ret, state) ->
               let** ctx = acc in
               let summ, leaks = Summary.make ret pcs state in
               let update_ctx () = Summary.Context.update ty summ ctx in
               leak_check leaks |> Result.map update_ctx)
       (* Unsuccessful termination: found a type unsoundness *)
       | _ -> Result.error `TypeUnsound)

let get_funs () =
  let crate = Soteria_rust_lib.Crate.get_crate () in
  let can_infer _ (fundef : Charon.UllbcAst.fun_decl) =
    fundef.item_meta.is_local
    && (not fundef.signature.is_unsafe)
    && (not @@ Drop.is_impl fundef)
    && (fundef.item_meta.attr_info.public || not !Config.current.only_public)
  in
  FunDeclId.Map.filter can_infer crate.fun_decls

let find_unsoundness () =
  let fun_decls = get_funs () in
  let drop_ctx = Drop.create_ctx () in
  let fuel =
    Soteria_symex.Fuel_gauge.
      {
        steps = Finite !Config.current.step_fuel;
        branching = Finite !Config.current.branch_fuel;
      }
  in
  (* The try_refute procedure is called for each function *)
  let try_refute _ (fundef : Charon.UllbcAst.fun_decl) acc =
    let ( let** ) = Result.bind in
    let** summ_ctx = acc in
    let tys =
      List.map
        (function TRef (_, ty, _) -> ty | ty -> ty)
        fundef.signature.inputs
    in
    let iter_summs = Summary.Context.iter_summs tys summ_ctx in
    let try_refute ctx summs =
      let process = wrap_call fundef summs |> wrap_return drop_ctx in
      Result.bind ctx (try_refute fuel process)
    in
    Iter.fold try_refute (Result.ok summ_ctx) iter_summs
  in
  let handle_exn exn err =
    Fmt.kstr err "Exn: %a@\nTrace: %s" Fmt.exn exn (Printexc.get_backtrace ())
  in
  (* The meta-loop iterates over all safe functions *)
  let rec meta_loop summ_ctx fuel =
    if fuel <= 0 then false (* Fuel exhausted *)
    else
      let res =
        (* Call try_refute on all functions and accumulate the results *)
        try FunDeclId.Map.fold try_refute fun_decls (Result.ok summ_ctx) with
        | ExecutionError _ as exn -> handle_exn exn execution_err
        | exn -> handle_exn exn Soteria_rust_lib.Driver.execution_err
      in
      match res with
      | Ok summ_ctx -> meta_loop summ_ctx (fuel - 1)
      | Error _ -> true (* Type unsoundness found *)
  in
  (* Run the algorithm starting from an empty summary context *)
  meta_loop Summary.Context.empty !Config.current.pass_fuel

let config_set (config : Config.global) =
  Config.set config.ruxt;
  let (config : Soteria_rust_lib.Config.global) =
    {
      logs = config.logs;
      terminal = config.terminal;
      solver = config.solver;
      rusteria =
        {
          no_compile = config.ruxt.no_compile;
          no_timing = config.ruxt.no_timing;
          cleanup = config.ruxt.cleanup;
          ignore_leaks = true;
          ignore_aliasing = true;
          monomorphize_experimental = true;
          with_kani = config.ruxt.with_kani;
          with_miri = config.ruxt.with_miri;
          log_compilation = config.ruxt.log_compilation;
          step_fuel = config.ruxt.step_fuel;
          branch_fuel = config.ruxt.branch_fuel;
        };
    }
  in
  Soteria_rust_lib.Driver.config_set config

let exec_ruxt config file_name =
  let open Soteria_rust_lib in
  config_set config;
  let plugin = Plugin.create_using_current_config () in
  try
    let crate = Driver.parse_ullbc_of_file ~plugin file_name in
    let open Syntaxes.FunctionWrap in
    let@ () = Crate.with_crate crate in
    let ret, msg =
      if find_unsoundness () then (1, "Found type unsoundness!\n")
      else (0, "No type unsoundness found!\n")
    in
    Fmt.pr "%s" msg;
    exit ret
  with
  | ExecutionError e -> Driver.fatal e
  | Plugin.PluginError e -> Driver.fatal ~name:"Plugin" ~code:3 e
  | Driver.ExecutionError e -> Driver.fatal ~name:"Rusteria" ~code:3 e
  | Driver.FrontendError e -> Driver.fatal ~name:"Charon" ~code:4 e
