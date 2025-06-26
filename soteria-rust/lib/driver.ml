open Soteria_terminal.Color
module Wpst_interp = Interp.Make (State)
module Compo_res = Soteria_symex.Compo_res
open Syntaxes.FunctionWrap
open Cmd
open Charon

exception ExecutionError of string
exception CharonError of string

module Cleaner = struct
  let files = ref []
  let touched file = files := file :: !files
  let cleanup () = List.iter Sys.remove !files
  let init ~clean () = if clean then at_exit cleanup
end

let config_set log_config term_config solver_config config =
  Solver_config.set solver_config;
  Soteria_logs.Config.check_set_and_lock log_config;
  Soteria_terminal.Config.set_and_lock term_config;
  Config.set config;
  Cleaner.init ~clean:config.cleanup ()

(** Given a Rust file, parse it into LLBC, using Charon. *)
let parse_ullbc_of_file ~(plugin : Plugin.root_plugin) file_name =
  let file_name =
    if Filename.is_relative file_name then
      Filename.concat (Sys.getcwd ()) file_name
    else file_name
  in
  let parent_folder = Filename.dirname file_name in
  let output = Printf.sprintf "%s.llbc.json" file_name in
  (if not !Config.current.no_compile then
     (* TODO: make these flags! *)
     let cmd = plugin.mk_cmd ~input:file_name ~output () in
     let res = exec_cmd @@ "cd " ^ parent_folder ^ " && " ^ build_cmd cmd in
     if res = 0 then Cleaner.touched output
     else
       let msg = Fmt.str "Failed compilation to ULLBC: code %d" res in
       raise (CharonError msg));
  let crate =
    try
      output |> Yojson.Basic.from_file |> Charon.UllbcOfJson.crate_of_json
    with
    | Sys_error _ -> raise (CharonError "File doesn't exist")
    | _ -> raise (CharonError "Failed to parse ULLBC")
  in
  match crate with
  | Ok crate ->
      if not !Config.current.no_compile then (
        (* save crate to local file *)
        let crate_file = Printf.sprintf "%s.crate" file_name in
        let str = Charon.PrintUllbcAst.Crate.crate_to_string crate in
        let oc = open_out_bin crate_file in
        output_string oc str;
        close_out oc;
        Cleaner.touched crate_file);
      crate
  | Error err -> raise (CharonError err)

let exec_main ~(plugin : Plugin.root_plugin) (crate : Charon.UllbcAst.crate) =
  let entry_points =
    Types.FunDeclId.Map.values crate.fun_decls
    |> List.filter_map plugin.get_entry_point
  in
  if List.is_empty entry_points then
    raise (ExecutionError "No entry points found");
  let exec_fun = Wpst_interp.exec_fun ~args:[] ~state:State.empty in
  let@ () = Crate.with_crate crate in
  let outcomes =
    entry_points
    |> List.map @@ fun (entry : Plugin.entry_point) ->
       let branches =
         let@ () = L.entry_point_section entry.fun_decl.item_meta.name in
         Option.iter Rustsymex.set_default_fuel entry.fuel;
         try Rustsymex.run @@ exec_fun entry.fun_decl with
         | Layout.InvalidLayout ->
             [ (Error (`InvalidLayout, Soteria_terminal.Call_trace.empty), []) ]
         | exn ->
             let msg =
               Fmt.str "Exn: %a@\nTrace: %s" Fmt.exn exn
                 (Printexc.get_backtrace ())
             in
             raise (ExecutionError msg)
       in
       let nbranches = List.length branches in
       let branches =
         if not entry.expect_error then branches
         else
           let open Compo_res in
           let trace =
             Soteria_terminal.Call_trace.singleton
               ~loc:entry.fun_decl.item_meta.span ()
           in
           let oks, errors =
             branches
             |> List.partition_map @@ function
                | Ok _, pcs -> Left (Error (`MetaExpectedError, trace), pcs)
                | Error _, pcs ->
                    Right (Ok (Charon_util.unit_, State.empty), pcs)
                | v -> Left v
           in
           if List.is_empty errors then oks else errors
       in
       let outcomes = List.map fst branches in
       if Option.is_some !Rustsymex.not_impl_happened then
         let msg = Option.get !Rustsymex.not_impl_happened in
         let () = Rustsymex.not_impl_happened := None in
         raise (ExecutionError msg)
       else if List.is_empty branches then
         raise (ExecutionError "Execution vanished")
       else if List.exists Compo_res.is_missing outcomes then
         raise (ExecutionError "Miss encountered in WPST")
       else
         let entry_name =
           Fmt.to_to_string Crate.pp_name entry.fun_decl.item_meta.name
         in
         let errors = Compo_res.only_errors outcomes in
         if List.is_empty errors then
           branches
           |> List.filter_map (function
                | Compo_res.Ok _, pcs -> Some pcs
                | _ -> None)
           |> fun brs -> Ok (brs, entry_name, nbranches)
         else Result.error (errors, entry_name, nbranches)
  in
  List.join_results outcomes

let pp_branches ft n = Fmt.pf ft "%i branch%s" n (if n = 1 then "" else "es")

let exec_main_and_print log_config term_config solver_config config file_name =
  config_set log_config term_config solver_config config;
  match
    let plugin = Plugin.create () in
    let crate = parse_ullbc_of_file ~plugin file_name in
    exec_main ~plugin crate
  with
  | Ok res ->
      Soteria_terminal.Diagnostic.print_diagnostic_simple ~severity:Note
        "Done, no errors found";
      (res
      |> List.iter @@ fun (pcs, entry_name, ntotal) ->
         let open Fmt in
         let pcs = List.mapi (fun i pc -> (pc, i + 1)) pcs in
         let pp_info ft (pc, i) =
           let name = "PC " ^ string_of_int i ^ ":" in
           if List.is_empty pc then pf ft "%a empty" (pp_style `Bold) name
           else
             pf ft "%a @[<-1>%a@]" (pp_style `Bold) name
               (list ~sep:(any " /\\@, ") Typed.ppa)
               pc
         in
         Fmt.pr "@\n%a: ran %a@\n%a@\n" (pp_style `Bold) entry_name pp_branches
           ntotal
           (list ~sep:(any "@\n") pp_info)
           pcs);
      exit 0
  | Error res ->
      Soteria_terminal.Diagnostic.print_diagnostic_simple ~severity:Error
        "Found issues";
      (res
      |> List.iter @@ fun (errs, entry_name, ntotal) ->
         let n = List.length res in
         Fmt.pr "@\n%a: error in %a (out of %d):@\n@?" (pp_style `Bold)
           entry_name pp_branches n ntotal;
         List.iter
           (fun (error, call_trace) ->
             Error.Diagnostic.print_diagnostic ~fname:entry_name ~call_trace
               ~error)
           (List.sort_uniq Stdlib.compare errs));
      exit 1
  | exception Plugin.PluginError e ->
      Fmt.kstr
        (Soteria_terminal.Diagnostic.print_diagnostic_simple ~severity:Error)
        "Fatal (Plugin): %s" e;
      exit 2
  | exception ExecutionError e ->
      Fmt.kstr
        (Soteria_terminal.Diagnostic.print_diagnostic_simple ~severity:Error)
        "Fatal: %s" e;
      exit 2
  | exception CharonError e ->
      Fmt.kstr
        (Soteria_terminal.Diagnostic.print_diagnostic_simple ~severity:Error)
        "Fatal (Charon): %s" e;
      exit 3
