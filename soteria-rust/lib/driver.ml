module Stats = Soteria.Stats
open Soteria.Terminal.Diagnostic
open Soteria.Logs.Printers
open Syntaxes.FunctionWrap

let wrap_step name f =
  Fmt.pr "%a...@?" (pp_style `Bold) name;
  try
    let time = Unix.gettimeofday () in
    let res = f () in
    let time = Unix.gettimeofday () -. time in
    Fmt.pr " done in %a@." pp_time time;
    res
  with e ->
    let bt = Printexc.get_raw_backtrace () in
    Fmt.pr " errored@.";
    Printexc.raise_with_backtrace e bt

let fatal ?name ?(code = 2) err =
  let msg = Option.fold ~none:"Fatal: " ~some:(Fmt.str "Fatal (%s): ") name in
  print_diagnostic_simple ~severity:Error (msg ^ err);
  exit code

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

let exec_rustc config file_name =
  let@ () = with_exn_and_config config in
  let compile () = Frontend.parse_ullbc_of_file file_name in
  wrap_step "Compiling" compile |> Analyses.Wpst.exec

let exec_cargo config crate_dir =
  let@ () = with_exn_and_config config in
  let compile () = Frontend.parse_ullbc_of_crate crate_dir in
  wrap_step "Compiling" compile |> Analyses.Wpst.exec

let build_plugins config =
  let@ () = with_exn_and_config config in
  wrap_step "Compiling plugins" Frontend.compile_all_plugins;
  Ok
