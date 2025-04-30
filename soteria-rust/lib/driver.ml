module Wpst_interp = Interp.Make (Heap)
module Compo_res = Soteria_symex.Compo_res
open Syntaxes.FunctionWrap
open Cmd

exception ExecutionError of string
exception CharonError of string

module Cleaner = struct
  let files = ref []
  let touched file = files := file :: !files
  let cleanup () = List.iter Sys.remove !files
  let init ~clean () = if clean then at_exit cleanup
end

let pp_err ft (err, call_trace) =
  Format.open_hbox ();
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
    | `DoubleFree -> Fmt.string ft "DoubleFree"
    | `InvalidFree -> Fmt.string ft "InvalidFree"
    | `MemoryLeak -> Fmt.string ft "Memory leak"
    | `FailedAssert (Some msg) -> Fmt.pf ft "Failed assertion: %s" msg
    | `FailedAssert None -> Fmt.string ft "Failed assertion"
    | `Overflow -> Fmt.string ft "Overflow"
    | `StdErr msg -> Fmt.pf ft "Std error: %s" msg
    | `Panic msg -> Fmt.pf ft "Panic: %s" msg
    | `MetaExpectedError -> Fmt.string ft "MetaExpectedError"
  in
  Fmt.pf ft " with trace %a" Call_trace.pp call_trace;
  Format.close_box ()

let default_cmd ~file_name ~output () =
  mk_cmd
    ~charon:
      [
        "--ullbc";
        Fmt.str "--input %s" file_name;
        Fmt.str "--dest-file %s" output;
        (* We can't enable this because it removes statements we care about... *)
        (* "--mir_optimized"; *)
        "--translate-all-methods";
        "--extract-opaque-bodies";
        "--monomorphize";
      ]
    ~rustc:
      [
        (* i.e. not always a binary! *)
        "--crate-type=lib";
        "-Zunstable-options";
        (* Not sure this is needed *)
        "--extern=std";
        "--extern=core";
        (* No warning *)
        "-Awarnings";
      ]
    ()

let mk_kani_cmd () =
  let path = List.hd Runtime_sites.Sites.kani_lib in
  let cargo =
    "RUSTC=$(charon toolchain-path)/bin/rustc $(charon \
     toolchain-path)/bin/cargo"
  in
  let target =
    (* look for line "host: <host>", to get the target architecture *)
    let info = Fmt.kstr exec_and_read "%s -vV" cargo in
    match List.find_opt (String.starts_with ~prefix:"host") info with
    | Some s -> String.sub s 6 (String.length s - 6)
    | None -> raise (ExecutionError "Couldn't find target host")
  in
  (* build Kani lib *)
  let res =
    Fmt.kstr exec_cmd
      "cd %s/std && %s build --lib --target %s > /dev/null 2>/dev/null" path
      cargo target
  in
  if res <> 0 && res <> 255 then
    let msg = "Couldn't compile Kani lib: error " ^ Int.to_string res in
    raise (ExecutionError msg)
  else ();
  let ( / ) = Filename.concat in
  let rlib = path / "std" / "target" / target / "debug" / "libstd.rlib" in
  mk_cmd
    ~rustc:
      [
        "-Zcrate-attr=feature\\(register_tool\\)";
        "-Zcrate-attr=register_tool\\(kanitool\\)";
        (* Code often hides kani proofs behind a cfg *)
        "--cfg=kani";
        "--extern=kani";
        (* The below is cursed and should be fixed !!! *)
        Fmt.str "-L%s/std/target/%s/debug/deps" path target;
        Fmt.str "-L%s/std/target/debug/deps" path;
        Fmt.str "--extern noprelude:std=%s" rlib;
      ]
    ()

let mk_miri_cmd () =
  mk_cmd ~charon:[ "--opaque=miri_extern" ] ~rustc:[ "--cfg=miri" ] ()

(** Given a Rust file, parse it into LLBC, using Charon. *)
let parse_ullbc_of_file ~no_compile ~kani ~miri file_name =
  let file_name =
    if Filename.is_relative file_name then
      Filename.concat (Sys.getcwd ()) file_name
    else file_name
  in
  let parent_folder = Filename.dirname file_name in
  let output = Printf.sprintf "%s.llbc.json" file_name in
  (if not no_compile then
     (* TODO: make these flags! *)
     let args =
       default_cmd ~file_name ~output ()
       |> concat_cmd_if kani mk_kani_cmd
       |> concat_cmd_if miri mk_miri_cmd
     in
     let res = exec_cmd @@ "cd " ^ parent_folder ^ " && " ^ build_cmd args in
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

let exec_main ?(ignore_leaks = false) (crate : Charon.UllbcAst.crate) =
  let open Charon in
  Layout.Session.set_crate crate;
  let ctx = PrintUllbcAst.Crate.crate_to_fmt_env crate in
  let entry_points =
    Types.FunDeclId.Map.values crate.fun_decls
    |> List.filter_map (fun (decl : UllbcAst.blocks UllbcAst.gfun_decl) ->
           let is_main =
             match List.rev decl.item_meta.name with
             | PeIdent ("main", _) :: _ -> true
             | _ -> false
           in
           if is_main then Some (decl, false)
           else
             let is_proof = Charon_util.decl_has_attr decl "kanitool::proof" in
             if is_proof then
               let should_err =
                 Charon_util.decl_has_attr decl "kanitool::should_panic"
               in
               Some (decl, should_err)
             else None)
  in
  if List.is_empty entry_points then
    raise (ExecutionError "No entry points found");
  let exec_fun =
    Wpst_interp.exec_fun ~ignore_leaks ~crate ~args:[] ~state:Heap.empty
  in
  let outcomes =
    entry_points
    |> List.map @@ fun ((entry_point : UllbcAst.fun_decl), should_err) ->
       let@ () =
         L.entry_point_section
         @@ PrintTypes.name_to_string ctx entry_point.item_meta.name
       in
       let branches =
         try Rustsymex.run @@ exec_fun entry_point
         with exn ->
           let msg =
             Fmt.str "Exn: %a@\nTrace: %s" Fmt.exn exn
               (Printexc.get_backtrace ())
           in
           raise (ExecutionError msg)
       in
       let branches =
         if not should_err then branches
         else
           let open Compo_res in
           let trace =
             Call_trace.singleton ~loc:entry_point.item_meta.span ()
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
    let crate = parse_ullbc_of_file ~no_compile ~kani ~miri file_name in
    let res = exec_main ~ignore_leaks crate in
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
        Fmt.pr "Error in %i branch%s:\n%a\n" n
          (if n = 1 then "" else "s")
          (list ~sep:(any "@\n@\n") pp_err)
          res;
        exit 1
  with
  | ExecutionError e ->
      Fmt.pr "Fatal: %s" e;
      exit 2
  | CharonError e ->
      Fmt.pr "Fatal (Charon): %s" e;
      exit 3
