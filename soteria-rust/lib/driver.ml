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

let pp_err ft (err, call_trace) =
  Format.open_vbox 0;
  let () =
    match err with
    | `NullDereference -> Fmt.string ft "NullDereference"
    | `OutOfBounds -> Fmt.string ft "OutOfBounds"
    | `UninitializedMemoryAccess -> Fmt.string ft "UninitializedMemoryAccess"
    | `UseAfterFree -> Fmt.string ft "UseAfterFree"
    | `DivisionByZero -> Fmt.string ft "DivisionByZero"
    | `ParsingError s -> Fmt.pf ft "ParsingError: %s" s
    | `UBPointerComparison -> Fmt.string ft "UBPointerComparison"
    | `UBPointerArithmetic -> Fmt.string ft "UBPointerArithmetic"
    | `UBAbort -> Fmt.string ft "UBAbort"
    | `UBArithShift -> Fmt.string ft "UBArithShift"
    | `UBTransmute -> Fmt.string ft "UBTransmute"
    | `UBTreeBorrow -> Fmt.string ft "UBTreeBorrow"
    | `DeadVariable -> Fmt.string ft "DeadVariable"
    | `DoubleFree -> Fmt.string ft "DoubleFree"
    | `InvalidFree -> Fmt.string ft "InvalidFree"
    | `MisalignedPointer -> Fmt.string ft "MisalignedPointer"
    | `RefToUninhabited -> Fmt.string ft "RefToUninhabited"
    | `InvalidLayout -> Fmt.string ft "InvalidLayout"
    | `MemoryLeak -> Fmt.string ft "Memory leak"
    | `FailedAssert (Some msg) -> Fmt.pf ft "Failed assertion: %s" msg
    | `FailedAssert None -> Fmt.string ft "Failed assertion"
    | `Overflow -> Fmt.string ft "Overflow"
    | `StdErr msg -> Fmt.pf ft "Std error: %s" msg
    | `Panic (Some msg) -> Fmt.pf ft "Panic: %s" msg
    | `Panic None -> Fmt.pf ft "Panic"
    | `MetaExpectedError -> Fmt.string ft "MetaExpectedError"
  in
  Fmt.pf ft "@,Trace:@,%a" Call_trace.pp call_trace;
  Format.close_box ()

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
           |> Result.ok
         else Result.error errors
  in
  List.join_results outcomes
  |> Result.map List.flatten
  |> Result.map_error List.flatten

let exec_main_and_print log_level smt_file no_compile clean ignore_leaks kani
    miri file_name =
  Z3solver.set_smt_file smt_file;
  Soteria_logs.Config.check_set_and_lock log_level;
  Cleaner.init ~clean ();
  try
    let plugin =
      Plugin.merge_ifs
        [ (true, Plugin.default); (kani, Plugin.kani); (miri, Plugin.miri) ]
    in
    let crate = parse_ullbc_of_file ~no_compile ~plugin file_name in
    let res = exec_main ~ignore_leaks ~plugin crate in
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
        let pp_err ft e = pf ft "- %a" pp_err e in
        let n = List.length res in
        Fmt.pr "Error in %i branch%s:@\n%a\n" n
          (if n = 1 then "" else "es")
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
