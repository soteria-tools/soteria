module Config_ = Config
open Soteria_rust_lib
module Config = Config_
open Soteria.Symex
open Syntaxes.FunctionWrap

let ( let* ) = Result.bind

let exec_wrapper fuel (wrapper : Wrapper.t) summs =
  (* Symbolically execute the wrapped function call *)
  Rustsymex.run_needs_stats ~mode:UX ~fuel (Wrapper.process wrapper summs)
  |> ListLabels.fold_left ~init:(Result.ok []) ~f:(fun acc -> function
       (* Successful termination: a new summary can been inferred *)
       | Compo_res.Ok (ret, state, ty), pcs ->
           let* summ = Summary.make ret pcs state in
           Result.map (fun summs -> (ty, summ) :: summs) acc
       (* Unsuccessful termination: found a type unsoundness *)
       | _ -> Result.error `TypeUnsound)

let exec_crate (crate : Crate.t) =
  let@ () = Crate.with_crate crate in
  let config = !Config.current in
  (* Set fuel for wrapper execution *)
  let exec_wrapper =
    let soteria_fuel =
      let open Fuel_gauge in
      let fuel = function None -> Fuel_value.Infinite | Some i -> Finite i in
      { steps = fuel config.step_fuel; branching = fuel config.branch_fuel }
    in
    exec_wrapper soteria_fuel
  in
  (* Fetch the library and perform a bounded number of inference passes *)
  let library = Library.get () in
  let rec find_unsoundness fuel summ_ctx =
    if fuel <= 0 then Result.ok () (* Fuel exhausted, no unsoundness found *)
    else (* Pass over the library and update the summary context *)
      let* summ_ctx = Library.infer_summaries exec_wrapper library ~summ_ctx in
      find_unsoundness (fuel - 1) summ_ctx
  in
  (* Collect statistics from all the runs *)
  let Soteria.Stats.{ res; stats } =
    Rustsymex.Stats.As_ctx.with_stats () (fun () ->
        (* Run the algorithm starting from the base summary context *)
        let* summ_ctx = Library.infer_summaries exec_wrapper library in
        find_unsoundness config.pass_fuel summ_ctx)
  in
  if config.print_stats then Driver.print_stats stats;
  res

let exec_ruxt config file_name =
  Config.set config;
  let compile () = fst @@ Frontend.parse_ullbc_of_file file_name in
  match Driver.wrap_step "Compiling" compile |> exec_crate with
  | soundness_res ->
      let code, msg =
        match soundness_res with
        | Ok () -> (0, "No type unsoundness found!\n")
        | Error `TypeUnsound -> (1, "Found type unsoundness!\n")
        | Error `MemoryLeak -> (1, "Found memory leak!\n")
      in
      Fmt.pr "%s" msg;
      exit code
  | exception Frontend.PluginError e -> Driver.fatal ~name:"Plugin" ~code:3 e
  | exception Frontend.FrontendError e -> Driver.fatal ~name:"Charon" ~code:4 e
  | exception Frontend.CompilationError e ->
      Soteria.Terminal.Diagnostic.print_diagnostic_simple ~severity:Error
        ("Compilation error:\n" ^ e);
      Driver.Outcome.exit Error
  | exception Driver.ExecutionError e -> Driver.fatal ~name:"Rusteria" ~code:3 e
