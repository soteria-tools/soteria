open Soteria.Terminal.Diagnostic
open Soteria.Logs.Printers
open Syntaxes.FunctionWrap
open Analyses.Util

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

let with_exn_and_config mode config f =
  try
    Config.set_and_lock_global mode config;
    let outcome = f () in
    Analyses.Outcome.exit outcome
  with
  | Frontend.FrontendError e -> fatal ~name:"Frontend" ~code:3 e
  | Frontend.CompilationError (info, msg) ->
      print_diagnostic_simple ~severity:Error "Compilation error";
      Fmt.pr "@.%s@.%a@.@." msg Unimplemented.pp
        (Unimplemented.make
           ~tip:
             ( "You can try cleaning plugins and rebuilding them",
               Some "soteria-rust build-plugins [compilation flags]" )
           ~issue:388
           ("Compilation failed while " ^ info));
      Analyses.Outcome.exit Error
  | Exn.Config_error err ->
      fatal ~name:"Config" ~code:Cmdliner.Cmd.Exit.cli_error err

let list_tests (crate, entry_points) =
  let fmt_env = Charon.Print.crate_to_fmt_env crate in
  let names =
    List.map
      (fun (ep : Frontend.entry_point) ->
        `String (Charon.Print.name_to_string fmt_env ep.fun_decl.item_meta.name))
      entry_points
  in
  print_endline (Yojson.Safe.to_string (`List names));
  Analyses.Outcome.Ok

let exec_wpst config target =
  let@ () = with_exn_and_config Whole_program config in
  let compile () = Frontend.parse_ullbc_with_entry_points target in
  let targets = wrap_step "Compiling" compile in
  if (Config.get ()).list_tests then list_tests targets
  else Analyses.Wpst.exec targets

let build_plugins config =
  let@ () = with_exn_and_config Whole_program config in
  wrap_step "Compiling plugins" Frontend.compile_all_plugins;
  Ok
