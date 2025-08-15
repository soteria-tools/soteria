module Rustsymex = Soteria_rust_lib.Rustsymex
module Wpst_interp = Soteria_rust_lib.Interp.Make (Heap)
module Compo_res = Soteria_symex.Compo_res
open Rustsymex.Syntax
open Charon

exception ExecutionError of string

let execution_err msg = raise (ExecutionError msg)

let target_decls (crate : UllbcAst.crate) =
  let can_infer _ (fundef : UllbcAst.fun_decl) =
    fundef.item_meta.attr_info.public
    && fundef.item_meta.is_local
    && not fundef.signature.is_unsafe
  in
  Types.FunDeclId.Map.filter can_infer crate.fun_decls

let leak_check leaks : (unit, [> `MemoryLeak ]) Result.t =
  match leaks with [] -> Result.ok () | _ -> Result.error `MemoryLeak

let try_refute fuel (fundef : UllbcAst.fun_decl) summs summ_ctx =
  (* Construct precondition state and symbolically execute function *)
  let process =
    let* args, state = Summary.Symex.flatten summs in
    Wpst_interp.exec_fun ~args ~state fundef
  in
  (* For each successful outcome, the summary context will be updated. *)
  let extend_ctx res_ctx (outcome, pcs) =
    let ( let** ) = Result.bind in
    let** ctx = res_ctx in
    match outcome with
    (* Successful termination: update the summary context *)
    | Compo_res.Ok (ret, state) ->
        let ty = fundef.signature.output in
        let summ, leaks = Summary.make ret pcs state in
        let update_ctx () = Summary.Context.update ty summ ctx in
        Result.map update_ctx (leak_check leaks)
    (* Unsuccessful termination: found a type unsoundness *)
    | _ -> Result.error `TypeUnsound
  in
  Rustsymex.run ~fuel process |> List.fold_left extend_ctx (Result.ok summ_ctx)

let find_unsoundness (crate : UllbcAst.crate) =
  let decls = target_decls crate in
  let fuel =
    Soteria_symex.Fuel_gauge.
      {
        steps = Finite !Soteria_rust_lib.Config.current.step_fuel;
        branching = Finite !Soteria_rust_lib.Config.current.branch_fuel;
      }
  in
  (* The try_refute procedure is called for each function *)
  let try_refute _ (fundef : UllbcAst.fun_decl) acc =
    let ( let** ) = Result.bind in
    let** summ_ctx = acc in
    let tys = fundef.signature.inputs in
    let f ctx summs = Result.bind ctx (try_refute fuel fundef summs) in
    let iter_summs = Summary.Context.iter_summs tys summ_ctx in
    Iter.fold f (Result.ok summ_ctx) iter_summs
  in
  (* The meta-loop iterates over all safe functions *)
  let rec meta_loop summ_ctx fuel =
    if fuel <= 0 then false (* Fuel exhausted *)
    else
      let res =
        (* Call try_refute on all functions and accumulate the results *)
        try Types.FunDeclId.Map.fold try_refute decls (Result.ok summ_ctx)
        with exn ->
          let msg =
            Fmt.str "Exn: %a@\nTrace: %s" Fmt.exn exn
              (Printexc.get_backtrace ())
          in
          raise (Soteria_rust_lib.Driver.ExecutionError msg)
      in
      match res with
      | Ok summ_ctx -> meta_loop summ_ctx (fuel - 1)
      | Error _ -> true (* Type unsoundness found *)
  in
  (* Run the algorithm starting from an empty summary context *)
  meta_loop Summary.Context.empty !Config.current.pass_fuel

let config_set (config : Config.global) =
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
      if find_unsoundness crate then (1, "Found type unsoundness!\n")
      else (0, "No type unsoundness found!\n")
    in
    Fmt.pr "%s" msg;
    exit ret
  with
  | ExecutionError e -> Driver.fatal e
  | Plugin.PluginError e -> Driver.fatal ~name:"Plugin" ~code:3 e
  | Driver.ExecutionError e -> Driver.fatal ~name:"Rusteria" ~code:3 e
  | Driver.FrontendError e -> Driver.fatal ~name:"Charon" ~code:4 e
