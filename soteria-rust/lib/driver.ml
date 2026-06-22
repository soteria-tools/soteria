open Soteria.Terminal.Diagnostic
open Soteria.Logs.Printers
open Syntaxes.FunctionWrap
open Analyses.Util

let wrap_step name f =
  Fmt.epr "%a...@?" (pp_style `Bold) name;
  try
    let time = Unix.gettimeofday () in
    let res = f () in
    let time = Unix.gettimeofday () -. time in
    Fmt.epr " done in %a@." pp_time time;
    res
  with e ->
    let bt = Printexc.get_raw_backtrace () in
    Fmt.epr " errored@.";
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
  | Analyses.Outcome.EarlyExit code -> Analyses.Outcome.exit code

let pp_path ft = function
  | `Dir path -> Fmt.pf ft "%s" path
  | `File path -> Fmt.pf ft "%s" path

(* The [--list-tests] entry points of a single target, as JSON objects tagging
   each entry point with its target, e.g. [{ "test": "tests::test_ok", "target":
   { "kind": "test", "name": "tests" } }]. These are aggregated across all
   targets into a single list. *)
let list_tests_entries target (crate, entry_points) : Yojson.Safe.t list =
  let fmt_env = Charon.Print.crate_to_fmt_env crate in
  let name_to_string = Charon.Print.name_to_string fmt_env in
  List.map
    (fun (ep : Frontend.entry_point) ->
      `Assoc
        [
          ("test", `String (name_to_string ep.fun_decl.item_meta.name));
          ("target", Frontend.target_to_yojson target);
        ])
    entry_points

(* Prints the aggregated [--list-tests] entry points as a single one-line JSON
   list to stdout. This is the only thing printed to stdout by [compile], so its
   output can be piped directly into e.g. [jq]. *)
let print_list_tests entries =
  print_endline (Yojson.Safe.to_string (`List entries))

let compile_target path target =
  let compile () = Frontend.compile_target ~target path in
  Fmt.kstr wrap_step "Compiling %a" Frontend.pp_target target compile

let compile config path =
  let@ () = with_exn_and_config Whole_program config in
  let list_tests = (Config.get ()).list_tests in
  let entries =
    Frontend.targets_to_run path
    |> List.concat_map (fun target ->
        let compiled = compile_target path target in
        if list_tests then list_tests_entries target compiled else [])
  in
  if list_tests then print_list_tests entries;
  Ok

let exec_wpst config path =
  let@ () = with_exn_and_config Whole_program config in
  let targets = Frontend.targets_to_run path in
  if List.is_empty targets then (
    Fmt.epr "No targets found for %a@." pp_path path;
    Analyses.Outcome.raise_outcome Fatal);

  (* only print out targets if a target was specified *)
  let specified_targets = targets <> [ Default ] in

  (* accumulate --list-tests entry points across targets, printed as a single
     aggregated JSON list once every target has been compiled *)
  let list_tests = (Config.get ()).list_tests in
  let entries = ref [] in

  (* iterate over targets, compiling then running them *)
  let found_non_empty = ref false in
  let res =
    Iter.of_list targets
    |> Iter.map (fun target ->
        let ((crate, entry_points) as compiled) = compile_target path target in
        if list_tests then
          entries := !entries @ list_tests_entries target compiled;
        if not (List.is_empty entry_points) then found_non_empty := true
        else
          Fmt.epr "No entry points found in %a, skipping@."
            Fmt.(list ~sep:(any ", ") Frontend.pp_target)
            [ target ];
        (crate, entry_points, if specified_targets then Some target else None))
    |> Analyses.Wpst.exec
  in
  if list_tests then print_list_tests !entries;

  (* ensure we ran something *)
  if !found_non_empty then res
  else (
    Fmt.epr "No entry points found for %a in %a@." pp_path path
      Fmt.(list ~sep:(any ", ") Frontend.pp_target)
      targets;
    Fatal)

let build_plugins config =
  let@ () = with_exn_and_config Whole_program config in
  wrap_step "Compiling plugins" Frontend.compile_all_plugins;
  Ok
