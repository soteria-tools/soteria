module Config_ = Config
open Soteria_rust_lib
module Config = Config_
open Result.Syntax
open Soteria.Terminal.Diagnostic
open Syntaxes.FunctionWrap
open Analyses.Util

let exec_crate (crate : Crate.t) =
  let@ () = Crate.with_crate crate in
  let@ () = Call_graph.with_dumped_callgraph () in
  let config = Config.get () in
  (* Set fuel for summary inference *)
  let fuel =
    let open Soteria.Symex.Fuel_gauge in
    let fuel = function None -> Fuel_value.Infinite | Some i -> Finite i in
    { steps = fuel config.step_fuel; branching = fuel config.branch_fuel }
  in
  (* Fetch the library and perform a bounded number of inference passes *)
  let library = Library.get () in
  let rec find_unsoundness passes summ_ctx =
    if passes <= 0 then Result.ok () (* Fuel exhausted, no unsoundness found *)
    else (* Pass over the library and update the summary context *)
      let* summ_ctx = Library.infer_summaries ~fuel summ_ctx library in
      find_unsoundness (passes - 1) summ_ctx
  in
  (* Collect statistics from all the runs *)
  let Soteria.Stats.{ res; stats } =
    let@ () = Layout.Session.with_layout_cache in
    Soteria.Stats.As_ctx.with_stats () (fun () ->
        (* Run the algorithm starting from the base summary context *)
        let* summ_ctx = Library.init_summaries ~fuel library in
        find_unsoundness config.pass_fuel summ_ctx)
  in
  Soteria.Stats.output stats;
  res

let with_exn_and_config config f =
  try
    Config.set_and_lock_global config;
    let outcome = f () in
    Analyses.Outcome.exit outcome
  with
  | Frontend.PluginError e -> fatal ~name:"Plugin" e
  | Frontend.FrontendError e -> fatal ~name:"Frontend" ~code:3 e
  | Frontend.CompilationError e ->
      print_diagnostic_simple ~severity:Error ("Compilation error:\n" ^ e);
      Analyses.Outcome.exit Error
  | Exn.Config_error err ->
      fatal ~name:"Config" ~code:Cmdliner.Cmd.Exit.cli_error err

let exec (crate : Crate.t) =
  let outcome, msg =
    match exec_crate crate with
    | Ok () -> (Analyses.Outcome.Ok, "No type unsoundness found!\n")
    | Error `TypeUnsound -> (Analyses.Outcome.Error, "Found type unsoundness!\n")
    | Error `MemoryLeak -> (Analyses.Outcome.Error, "Found memory leak!\n")
  in
  Fmt.pr "%s" msg;
  Analyses.Outcome.exit outcome

let exec_ruxt config target =
  let@ () = with_exn_and_config config in
  let compile () = Frontend.parse_ullbc target in
  Driver.wrap_step "Compiling" compile |> exec
