open Utils.Syntax
module Wpst_interp = Interp.Make (Heap)
module Compo_res = Bfa_symex.Compo_res

let setup_console_log level =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

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
    | `DoubleFree -> Fmt.string ft "DoubleFree"
    | `InvalidFree -> Fmt.string ft "InvalidFree"
    | `Memory_leak -> Fmt.string ft "Memory leak"
    | `FailedAssert -> Fmt.string ft "Failed assertion"
  in
  Fmt.pf ft " with trace %a" Call_trace.pp call_trace;
  Format.close_box ()

let res_of_code = function
  | Ok 0 -> Ok ()
  | Ok code -> Fmt.error "Error code: %d" code
  | Error e -> Error e

let ( let*> ) x f = Result.bind (res_of_code x) f

(** Given a Rust file, parse it into LLBC, using Charon. *)
let parse_ullbc_of_file ~no_compile file_name =
  let open Syntaxes.Result in
  let parent_folder = Filename.dirname file_name in
  let output = Printf.sprintf "%s.llbc.json" file_name in
  let*> () =
    match no_compile with
    | true -> Ok 0
    | false ->
        (* load from env *)
        let+ kani_lib =
          try
            match Sys.getenv "KANI_LIB_PATH" with
            | path when String.ends_with ~suffix:"/" path -> Ok path
            | path -> Ok (path ^ "/")
          with Not_found -> Error "KANI_LIB_PATH not set"
        in
        let command =
          Fmt.str
            "cd %s && RUSTFLAGS=\"-Zcrate-attr=feature(register_tool) \
             -Zcrate-attr=register_tool(kanitool)\" && charon --ullbc \
             --opaque=kani --translate-all-methods --no-cargo \
             --rustc-arg=--crate-type=lib --rustc-arg=-Zunstable-options \
             --rustc-arg=-Zcrate-attr=feature\\(register_tool\\) \
             --rustc-arg=--cfg=kani --rustc-arg=--extern=kani --rustc-arg=-v \
             --rustc-arg=-L%skani/target/aarch64-apple-darwin/debug/deps \
             --rustc-arg=-L%skani/target/debug/deps --input %s --dest-file %s"
            parent_folder kani_lib kani_lib file_name output
        in
        Fmt.pf Fmt.stderr "Running command: %s\n" command;
        Sys.command command
  in
  let json = Yojson.Basic.from_file output in
  let crate = Charon.UllbcOfJson.crate_of_json json in
  (* save crate to local file *)
  let () =
    match (crate, no_compile) with
    | Ok crate, false ->
        let oc = open_out_bin (Printf.sprintf "%s.crate" file_name) in
        let str = Charon.PrintUllbcAst.Crate.crate_to_string crate in
        output_string oc str;
        close_out oc
    | _ -> ()
  in
  crate

let exec_main (crate : Charon.UllbcAst.crate) =
  let open Syntaxes.Result in
  let open Charon in
  Layout.Session.set_crate crate;
  let+ entry_points =
    Types.FunDeclId.Map.bindings crate.fun_decls
    |> List.filter (fun (_, (decl : UllbcAst.blocks UllbcAst.gfun_decl)) ->
           let name_rev = List.rev decl.item_meta.name in
           (* TODO: is the entry point the function with index 0? *)
           match (decl.item_meta.name, name_rev) with
           | PeIdent (root, _) :: _, PeIdent (fn, _) :: _
             when root = crate.name && fn = "main" ->
               true
           | _ -> false)
    |> List.map snd
    |> function
    | [] -> Error "No main function found"
    | rest -> Ok rest
  in
  let exec_fun = Wpst_interp.exec_fun ~prog:crate ~args:[] ~state:Heap.empty in
  entry_points |> List.concat_map (Rustsymex.run << exec_fun)

let exec_main_and_print log_level smt_file no_compile file_name =
  let open Syntaxes.Result in
  Z3solver.set_smt_file smt_file;
  setup_console_log log_level;
  let res =
    let* crate = parse_ullbc_of_file ~no_compile file_name in
    let* res = exec_main crate in
    let errors = Compo_res.only_errors @@ List.map fst res in
    if List.is_empty errors then
      res
      |> List.filter_map (function
           | Compo_res.Ok ok, pcs -> Some (ok, pcs)
           | _ -> None)
      |> Result.ok
    else
      let errors_parsed = Fmt.str "Errors: %a" Fmt.(Dump.list pp_err) errors in
      Error errors_parsed
  in
  match res with
  | Ok res ->
      let open Fmt in
      let n = List.length res in
      let pp_pc ft pc =
        pf ft "@[%a@]" (list ~sep:(any " /\\@, ") Typed.ppa) pc
      in
      let pp_info ft ((ret, heap), pc) =
        pf ft "Returned: %a@\nHeap:@\n%a@\nPC: @[%a@]" Charon_util.pp_rust_val
          ret Heap.pp heap pp_pc pc
      in
      Fmt.pr "Done. - Ran %i branches\n%a\n" n
        (list ~sep:(any "@\n@\n") pp_info)
        res;
      exit 0
  | Error e ->
      L.err (fun f -> f "Error: %s" e);
      exit 1
