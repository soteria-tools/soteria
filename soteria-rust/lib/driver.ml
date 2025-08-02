module Config_ = Config
open Soteria_terminal
module Config = Config_
open Color
module Wpst_interp = Interp.Make (State)
module Compo_res = Soteria_symex.Compo_res
open Syntaxes.FunctionWrap
open Charon

exception ExecutionError of string
exception FrontendError of string

let execution_err msg = raise (ExecutionError msg)
let frontend_err msg = raise (FrontendError msg)

module Cleaner = struct
  let files = ref []
  let touched file = files := file :: !files
  let cleanup () = List.iter Sys.remove !files
  let () = at_exit (fun () -> if !Config.current.cleanup then cleanup ())
end

let config_set (config : Config.global) =
  Solver_config.set config.solver;
  Soteria_logs.Config.check_set_and_lock config.logs;
  Soteria_terminal.Config.set_and_lock config.terminal;
  Config.set config.rusteria

let chrono_reset, chrono =
  let time : float ref = ref 0.0 in
  let chrono_reset () = time := Unix.gettimeofday () in
  let chrono () =
    let elapsed = Unix.gettimeofday () -. !time in
    time := Unix.gettimeofday ();
    elapsed
  in
  (chrono_reset, chrono)

(** Given a Rust file, parse it into LLBC, using Charon. *)
let parse_ullbc ~mode ~(plugin : Plugin.root_plugin) ~input ~output ~pwd =
  if not !Config.current.no_compile then (
    let cmd = plugin.mk_cmd ~input ~output () in
    let res = Plugin.Cmd.exec_in ~mode pwd cmd in
    if res <> 0 then
      Fmt.kstr frontend_err "Failed compilation to ULLBC: code %d" res;
    Cleaner.touched output);
  let crate =
    try
      output |> Yojson.Basic.from_file |> Charon.UllbcOfJson.crate_of_json
    with
    | Sys_error _ -> frontend_err "File doesn't exist"
    | _ -> frontend_err "Failed to parse ULLBC"
  in
  let crate = Result.get_or_raise frontend_err crate in
  if not !Config.current.no_compile then (
    (* save pretty-printed crate to local file *)
    let crate_file = Printf.sprintf "%s.crate" output in
    let str = Charon.PrintUllbcAst.Crate.crate_to_string crate in
    let oc = open_out_bin crate_file in
    output_string oc str;
    close_out oc;
    Cleaner.touched crate_file);
  crate

(** Given a Rust file, parse it into LLBC, using Charon. *)
let parse_ullbc_of_file ?(with_obol = false) ~(plugin : Plugin.root_plugin)
    file_name =
  let file_name =
    if Filename.is_relative file_name then
      Filename.concat (Sys.getcwd ()) file_name
    else file_name
  in
  let parent_folder = Filename.dirname file_name in
  let output = Printf.sprintf "%s.llbc.json" file_name in
  let mode : Plugin.Cmd.mode = if with_obol then Obol else Rustc in
  parse_ullbc ~mode ~plugin ~input:file_name ~output ~pwd:parent_folder

(** Given a Rust file, parse it into LLBC, using Charon. *)
let parse_ullbc_of_crate ~(plugin : Plugin.root_plugin) crate =
  let crate_dir =
    if Filename.is_relative crate then Filename.concat (Sys.getcwd ()) crate
    else crate
  in
  let output = Printf.sprintf "%s/crate.llbc.json" crate_dir in
  parse_ullbc ~mode:Cargo ~plugin ~input:"" ~output ~pwd:crate_dir

let pp_branches ft n = Fmt.pf ft "%i branch%s" n (if n = 1 then "" else "es")

let pp_time ft t =
  if !Config.current.no_timing then Fmt.pf ft "<time>"
  else if t < 0.05 then Fmt.pf ft "%ams" (Fmt.float_dfrac 2) (t *. 1000.)
  else Fmt.pf ft "%as" (Fmt.float_dfrac 2) t

let print_outcomes entry_name f =
  let open Fmt in
  match f () with
  | Ok (pcs, ntotal) ->
      let pcs = List.mapi (fun i pc -> (pc, i + 1)) pcs in
      let pp_info ft (pc, i) =
        let name = "PC " ^ string_of_int i ^ ":" in
        if List.is_empty pc then pf ft "%a empty" (pp_style `Bold) name
        else
          pf ft "%a @[<-1>%a@]" (pp_style `Bold) name
            (list ~sep:(any " /\\@, ") Typed.ppa)
            pc
      in
      Fmt.kstr
        (Diagnostic.print_diagnostic_simple ~severity:Note)
        "%s: done in %a, ran %a" entry_name pp_time (chrono ()) pp_branches
        ntotal;
      Fmt.pr "@\n%a" (list ~sep:(any "@\n") pp_info) pcs;
      Fmt.pr "@\n@\n@?";
      true
  | Error (errs, ntotal) ->
      Fmt.kstr
        (Diagnostic.print_diagnostic_simple ~severity:Error)
        "%s: found issues in %a, errors in %a (out of %d)" entry_name pp_time
        (chrono ()) pp_branches (List.length errs) ntotal;
      Fmt.pr "@\n@?";
      let ( let@@ ) f x = List.iter x f in
      let () =
        let@@ error, call_trace = List.sort_uniq Stdlib.compare errs in
        Error.Diagnostic.print_diagnostic ~fname:entry_name ~call_trace ~error
      in
      Fmt.pr "@\n@\n@?";
      false
  | exception ExecutionError e ->
      Fmt.kstr
        (Diagnostic.print_diagnostic_simple ~severity:Error)
        "%s: runtime error in %a: %s" entry_name pp_time (chrono ()) e;
      Fmt.pr "@\n@\n@?";
      false

let exec_crate ~(plugin : Plugin.root_plugin) (crate : Charon.UllbcAst.crate) =
  (* get entry points to the crte *)
  let entry_points =
    Types.FunDeclId.Map.values crate.fun_decls
    |> List.filter_map plugin.get_entry_point
  in
  if List.is_empty entry_points then execution_err "No entry points found";

  (* prepare executing the entry points *)
  let fold_and f l = List.fold_left (fun acc x -> f x && acc) true l in
  let exec_fun = Wpst_interp.exec_fun ~args:[] ~state:State.empty in
  let@ () = Crate.with_crate crate in
  entry_points
  |> fold_and @@ fun (entry : Plugin.entry_point) ->
     (* execute! *)
     let entry_name =
       Fmt.to_to_string Crate.pp_name entry.fun_decl.item_meta.name
     in
     let@ () = print_outcomes entry_name in
     let branches =
       let@ () = L.entry_point_section entry.fun_decl.item_meta.name in
       Option.iter Rustsymex.set_default_fuel entry.fuel;
       try Rustsymex.run @@ exec_fun entry.fun_decl with
       | Layout.InvalidLayout ty ->
           [ (Error (`InvalidLayout ty, Call_trace.empty), []) ]
       | exn ->
           Fmt.kstr execution_err "Exn: %a@\nTrace: %s" Fmt.exn exn
             (Printexc.get_backtrace ())
     in

     (* inverse ok and errors if we expect a failure *)
     let nbranches = List.length branches in
     let branches =
       if not entry.expect_error then branches
       else
         let open Compo_res in
         let trace =
           Call_trace.singleton ~loc:entry.fun_decl.item_meta.span ()
         in
         let oks, errors =
           branches
           |> List.partition_map @@ function
              | Ok _, pcs -> Left (Error (`MetaExpectedError, trace), pcs)
              | Error _, pcs -> Right (Ok (Rust_val.unit_, State.empty), pcs)
              | v -> Left v
         in
         if List.is_empty errors then oks else errors
     in

     (* check for uncaught failure conditions *)
     let outcomes = List.map fst branches in
     (if Option.is_some !Rustsymex.not_impl_happened then
        let msg = Option.get !Rustsymex.not_impl_happened in
        let () = Rustsymex.not_impl_happened := None in
        execution_err msg);
     if List.is_empty branches then execution_err "Execution vanished";
     if List.exists Compo_res.is_missing outcomes then
       execution_err "Miss encountered in WPST";

     let errors = Compo_res.only_errors outcomes in
     if List.is_empty errors then
       let pcs = List.map snd branches in
       Ok (pcs, nbranches)
     else Error (errors, nbranches)

let wrap_step name f =
  Fmt.pr "%a...@?" (pp_style `Bold) name;
  try
    chrono_reset ();
    let res = f () in
    Fmt.pr " done in %a@\n@?" pp_time (chrono ());
    res
  with e ->
    Fmt.pr " errored@\n@?";
    raise e

let fatal ?name ?(code = 2) err =
  let msg = Option.fold ~none:"Fatal: " ~some:(Fmt.str "Fatal (%s): ") name in
  Diagnostic.print_diagnostic_simple ~severity:Error (msg ^ err);
  exit code

let exec_and_output_crate ~plugin compile_fn =
  match wrap_step "Compiling" compile_fn |> exec_crate ~plugin with
  | ok -> exit (if ok then 0 else 1)
  | exception Plugin.PluginError e -> fatal ~name:"Plugin" e
  | exception ExecutionError e -> fatal e
  | exception FrontendError e -> fatal ~name:"Frontend" ~code:3 e

let exec_rustc config file_name =
  config_set config;
  let plugin = Plugin.create_using_current_config () in
  let compile () = parse_ullbc_of_file ~plugin file_name in
  exec_and_output_crate ~plugin compile

let exec_cargo config crate_dir =
  config_set config;
  let plugin = Plugin.create_using_current_config () in
  let compile () = parse_ullbc_of_crate ~plugin crate_dir in
  exec_and_output_crate ~plugin compile

let exec_obol config file_name =
  config_set config;
  let plugin = Plugin.create_using_current_config () in
  let compile () = parse_ullbc_of_file ~with_obol:true ~plugin file_name in
  exec_and_output_crate ~plugin compile
