module Wpst_interp = Interp.Make (Heap)
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

(** Given a Rust file, parse it into LLBC, using Charon. *)
let parse_ullbc_of_file ~no_compile ~(plugin : Plugin.root_plugin) file_name =
  let file_name =
    if Filename.is_relative file_name then
      Filename.concat (Sys.getcwd ()) file_name
    else file_name
  in
  let parent_folder = Filename.dirname file_name in
  let output = Printf.sprintf "%s.llbc.json" file_name in
  (if not no_compile then
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
      if not no_compile then (
        (* save crate to local file *)
        let crate_file = Printf.sprintf "%s.crate" file_name in
        let str = Charon.PrintUllbcAst.Crate.crate_to_string crate in
        let oc = open_out_bin crate_file in
        output_string oc str;
        close_out oc;
        Cleaner.touched crate_file);
      crate
  | Error err -> raise (CharonError err)

let exec_main ?(ignore_leaks = false) ~(plugin : Plugin.root_plugin)
    (crate : Charon.UllbcAst.crate) =
  let entry_points =
    Types.FunDeclId.Map.values crate.fun_decls
    |> List.filter_map plugin.get_entry_point
  in
  if List.is_empty entry_points then
    raise (ExecutionError "No entry points found");
  let exec_fun =
    Wpst_interp.exec_fun ~ignore_leaks ~args:[] ~state:Heap.empty
  in
  let outcomes =
    entry_points
    |> List.map @@ fun (entry : Plugin.entry_point) ->
       let branches =
         let@ () = Crate.with_crate crate in
         let@ () = L.entry_point_section entry.fun_decl.item_meta.name in
         try Rustsymex.run @@ exec_fun entry.fun_decl with
         | Layout.InvalidLayout ->
             [ (Error (`InvalidLayout, Call_trace.empty), []) ]
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
             Call_trace.singleton ~loc:entry.fun_decl.item_meta.span ()
           in
           let oks, errors =
             branches
             |> List.partition_map @@ function
                | Ok _, pcs -> Left (Error (`MetaExpectedError, trace), pcs)
                | Error _, pcs -> Right (Ok (Charon_util.unit_, Heap.empty), pcs)
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
         let errors = Compo_res.only_errors outcomes in
         if List.is_empty errors then
           branches
           |> List.filter_map (function
                | Compo_res.Ok _, pcs -> Some pcs
                | _ -> None)
           |> fun brs -> (Ok brs, nbranches)
         else (Result.error errors, nbranches)
  in
  let outcomes, nbranches = List.split outcomes in
  let outcomes =
    List.join_results outcomes
    |> Result.map List.flatten
    |> Result.map_error List.flatten
  in
  let nbranches = List.fold_left ( + ) 0 nbranches in
  (outcomes, nbranches)

let exec_main_and_print log_level solver_config no_compile clean ignore_leaks
    kani miri file_name =
  Solver_config.set solver_config;
  Soteria_logs.Config.check_set_and_lock log_level;
  Cleaner.init ~clean ();
  try
    let plugin =
      Plugin.merge_ifs
        [ (true, Plugin.default); (kani, Plugin.kani); (miri, Plugin.miri) ]
    in
    let crate = parse_ullbc_of_file ~no_compile ~plugin file_name in
    let res, ntotal = exec_main ~ignore_leaks ~plugin crate in
    match res with
    | Ok res ->
        let open Fmt in
        let n = List.length res in
        let pp_pc ft pc = pf ft "%a" (list ~sep:(any " /\\@, ") Typed.ppa) pc in
        let pp_info ft pc =
          if List.is_empty pc then pf ft "PC: empty"
          else pf ft "PC: @.  @[<-1>%a@]" pp_pc pc
        in
        Fmt.pr "Done. - Ran %i branches\n%a\n" n
          (list ~sep:(any "@\n@\n") pp_info)
          res;
        exit 0
    | Error res ->
        let open Fmt in
        let pp_err ft e = pf ft "- %a" Error.pp_err e in
        let n = List.length res in
        Fmt.pr "Error in %i branch%s (out of %d):@\n%a\n" n
          (if n = 1 then "" else "es")
          ntotal
          (list ~sep:(any "@\n@\n") pp_err)
          res;
        exit 1
  with
  | Plugin.PluginError e ->
      Fmt.pr "Fatal (Plugin): %s" e;
      exit 2
  | ExecutionError e ->
      Fmt.pr "Fatal: %s" e;
      exit 2
  | CharonError e ->
      Fmt.pr "Fatal (Charon): %s" e;
      exit 3
