module Stats = Soteria.Stats
module Config_ = Config
open Soteria.Terminal
module Config = Config_
open Color
module Wpst_interp = Interp.Make (State)
module Compo_res = Soteria.Symex.Compo_res
open Syntaxes.FunctionWrap
open Charon

exception ExecutionError of string
exception FrontendError of string

let execution_err msg = raise (ExecutionError msg)
let frontend_err msg = raise (FrontendError msg)

module Outcome = struct
  type t = Ok | Error | Fatal

  let merge o1 o2 =
    match (o1, o2) with
    | Fatal, _ | _, Fatal -> Fatal
    | Error, _ | _, Error -> Error
    | Ok, Ok -> Ok

  let merge_list = List.fold_left (fun o1 (_, o2) -> merge o1 o2) Ok
  let as_status_code = function Ok -> 0 | Error -> 1 | Fatal -> 2
  let exit o = exit (as_status_code o)

  let pp ft = function
    | Ok -> Color.pp_clr `Green ft "ok"
    | Error -> Color.pp_clr `Red ft "error"
    | Fatal -> Color.pp_clr `Yellow ft "unknown"
end

module Cleaner = struct
  let files = ref []
  let touched file = files := file :: !files
  let cleanup () = List.iter Sys.remove !files
  let () = at_exit (fun () -> if !Config.current.cleanup then cleanup ())
end

(** Given a Rust file, parse it into LLBC, using Charon. *)
let parse_ullbc ~mode ~(plugin : Plugin.root_plugin) ~input ~output ~pwd =
  if not !Config.current.no_compile then (
    let cmd = plugin.mk_cmd ~input ~output () in
    let _, err, res = Plugin.Cmd.exec_in ~mode pwd cmd in
    if not (Plugin.Exe.is_ok res) then
      Fmt.kstr frontend_err "Failed compilation to ULLBC:@,%a"
        Fmt.(list string)
        err;
    Cleaner.touched output);
  let crate =
    try
      output |> Yojson.Basic.from_file |> Charon.UllbcOfJson.crate_of_json
    with
    | Sys_error _ -> frontend_err "File doesn't exist"
    | _ -> frontend_err "Failed to parse ULLBC"
  in
  let crate = Result.get_or_raise frontend_err crate in
  if !Config.current.output_crate then (
    (* save pretty-printed crate to local file *)
    let crate_file = Printf.sprintf "%s.crate" output in
    let str = Charon.PrintUllbcAst.Crate.crate_to_string crate in
    let oc = open_out_bin crate_file in
    output_string oc str;
    close_out oc;
    Cleaner.touched crate_file);
  crate

let normalize_path path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

(** Given a Rust file, parse it into LLBC, using Charon. *)
let parse_ullbc_of_file ~(plugin : Plugin.root_plugin) file_name =
  let file_name = normalize_path file_name in
  let parent_folder = Filename.dirname file_name in
  let output = Printf.sprintf "%s.llbc.json" file_name in
  parse_ullbc ~mode:Rustc ~plugin ~input:file_name ~output ~pwd:parent_folder

(** Given a Rust file, parse it into LLBC, using Charon. *)
let parse_ullbc_of_crate ~(plugin : Plugin.root_plugin) crate_dir =
  let crate_dir = normalize_path crate_dir in
  let output = Printf.sprintf "%s/crate.llbc.json" crate_dir in
  parse_ullbc ~mode:Cargo ~plugin ~input:"" ~output ~pwd:crate_dir

let pp_branches ft n = Fmt.pf ft "%i branch%s" n (if n = 1 then "" else "es")

let pp_time ft t =
  if !Config.current.no_timing then Fmt.pf ft "<time>"
  else if t < 0.05 then Fmt.pf ft "%ams" (Fmt.float_dfrac 2) (t *. 1000.)
  else Fmt.pf ft "%as" (Fmt.float_dfrac 2) t

let print_outcomes entry_name f =
  let open Fmt in
  let time = Unix.gettimeofday () in
  match f () with
  | Ok (pcs, ntotal) ->
      let time = Unix.gettimeofday () -. time in
      let pcs = List.mapi (fun i pc -> (pc, i + 1)) pcs in
      let pp_info ft (pc, i) =
        let name = "PC " ^ string_of_int i ^ ":" in
        if List.is_empty pc then pf ft "%a empty" (pp_style `Bold) name
        else
          let pp_pc = list ~sep:(any " /\\@, ") Typed.ppa in
          pf ft "%a @[<-1>%a@]" (pp_style `Bold) name pp_pc pc
      in
      Fmt.kstr
        (Diagnostic.print_diagnostic_simple ~severity:Note)
        "%s: done in %a, ran %a" entry_name pp_time time pp_branches ntotal;
      if not @@ Soteria.Terminal.Config.compact () then
        Fmt.pr "@\n%a" (list ~sep:(any "@\n") pp_info) pcs;
      Fmt.pr "@\n@.";
      (entry_name, Outcome.Ok)
  | Error (errs, ntotal) ->
      let time = Unix.gettimeofday () -. time in
      Fmt.kstr
        (Diagnostic.print_diagnostic_simple ~severity:Error)
        "%s: found issues in %a, errors in %a (out of %d)" entry_name pp_time
        time pp_branches (List.length errs) ntotal;
      Fmt.pr "@.";
      let ( let@@ ) f x = List.iter x f in
      let () =
        let@@ error, call_trace = List.sort_uniq Stdlib.compare errs in
        Error.Diagnostic.print_diagnostic ~fname:entry_name ~call_trace ~error;
        Fmt.pr "@.@."
      in
      (entry_name, Outcome.Error)
  | exception e ->
      let time = Unix.gettimeofday () -. time in
      let error, msg =
        match e with
        | ExecutionError msg -> ("runtime error", msg)
        | Soteria.Symex.Gave_up reason -> ("unsupported feature", reason)
        | e ->
            ( "exception",
              Fmt.str "%a@\nTrace: %s" Fmt.exn e (Printexc.get_backtrace ()) )
      in
      Fmt.kstr
        (Diagnostic.print_diagnostic_simple ~severity:Warning)
        "%s (%a): %s, %s@.@." entry_name pp_time time error msg;
      (entry_name, Outcome.Fatal)

let print_outcomes_summary outcomes =
  let open Fmt in
  let pp_outcome ft (name, res) = Fmt.pf ft "• %s: %a" name Outcome.pp res in
  pr "%a:@\n%a@\n" (pp_style `Bold) "Summary"
    (list ~sep:(any "@\n") pp_outcome)
    outcomes

let print_stats (stats : Meta.span Stats.stats) =
  let open Fmt in
  let entries =
    [
      ("Steps", fun ft () -> int ft stats.steps_number);
      ("Branches", fun ft () -> int ft stats.branch_number);
      ("Exec time", fun ft () -> pp_time ft stats.exec_time);
      ( "Solver time",
        fun ft () ->
          Fmt.pf ft "%a (%a%%)" pp_time stats.sat_time (float_dfrac 2)
            (100. *. stats.sat_time /. stats.exec_time) );
    ]
  in
  let pp_entry ft (name, pp_value) = Fmt.pf ft " • %s: %a" name pp_value () in
  pr "%a:@\n%a@\n" (pp_style `Bold) "Statistics"
    (list ~sep:(any "@\n") pp_entry)
    entries

let exec_crate ~(plugin : Plugin.root_plugin) (crate : Charon.UllbcAst.crate) =
  let@ () = Crate.with_crate crate in

  (* get entry points to the crate *)
  let entry_points =
    Types.FunDeclId.Map.values crate.fun_decls
    |> List.filter_map plugin.get_entry_point
  in
  if List.is_empty entry_points then execution_err "No entry points found";

  (* prepare executing the entry points *)
  let exec_fun = Wpst_interp.exec_fun ~args:[] ~state:State.empty in

  let@ entry : 'fuel Plugin.entry_point = (Fun.flip List.map) entry_points in
  (* execute! *)
  let entry_name =
    Fmt.to_to_string Crate.pp_name entry.fun_decl.item_meta.name
  in
  let@ () = print_outcomes entry_name in
  let { res = branches; stats } : ('res, 'range) Soteria.Stats.with_stats =
    let@ () = L.entry_point_section entry.fun_decl.item_meta.name in
    try
      Rustsymex.run_with_stats ~mode:OX ~fuel:entry.fuel
      @@ exec_fun entry.fun_decl
    with Layout.InvalidLayout ty ->
      {
        res = [ (Error (`InvalidLayout ty, Call_trace.empty), []) ];
        stats = Rustsymex.Stats.create ();
      }
  in

  if !Config.current.print_stats then print_stats stats;

  (* inverse ok and errors if we expect a failure *)
  let nbranches = List.length branches in
  let branches =
    if not entry.expect_error then branches
    else
      let open Compo_res in
      let trace = Call_trace.singleton ~loc:entry.fun_decl.item_meta.span () in
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
  if stats.unexplored_branch_number > 0 then
    Fmt.kstr execution_err "Missed %d branches" stats.unexplored_branch_number
  else if List.exists Compo_res.is_missing outcomes then
    execution_err "Miss encountered in WPST";

  let errors = Compo_res.only_errors outcomes in
  if List.is_empty errors then
    let pcs = List.map snd branches in
    Ok (pcs, nbranches)
  else Error (errors, nbranches)

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
  Diagnostic.print_diagnostic_simple ~severity:Error (msg ^ err);
  exit code

let exec_and_output_crate ~plugin compile_fn =
  match wrap_step "Compiling" compile_fn |> exec_crate ~plugin with
  | outcomes ->
      if !Config.current.print_summary then print_outcomes_summary outcomes;
      let outcome = Outcome.merge_list outcomes in
      Outcome.exit outcome
  | exception Plugin.PluginError e -> fatal ~name:"Plugin" e
  | exception ExecutionError e -> fatal e
  | exception FrontendError e -> fatal ~name:"Frontend" ~code:3 e

let exec_rustc config file_name =
  Config.set config;
  let plugin = Plugin.create_using_current_config () in
  let compile () = parse_ullbc_of_file ~plugin file_name in
  exec_and_output_crate ~plugin compile

let exec_cargo config crate_dir =
  Config.set config;
  let plugin = Plugin.create_using_current_config () in
  let compile () = parse_ullbc_of_crate ~plugin crate_dir in
  exec_and_output_crate ~plugin compile

let build_plugins config =
  Config.set config;
  wrap_step "Compiling plugins" Plugin.compile_all_plugins
