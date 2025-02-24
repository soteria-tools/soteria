open Charon
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
    | `DoubleFree -> Fmt.string ft "DoubleFree"
    | `InvalidFree -> Fmt.string ft "InvalidFree"
    | `Memory_leak -> Fmt.string ft "Memory leak"
    | `FailedAssert -> Fmt.string ft "Failed assertion"
  in
  Fmt.pf ft " with trace %a" Call_trace.pp call_trace;
  Format.close_box ()

let res_of_code = function
  | 0 -> Ok ()
  | code -> Fmt.error "Error code: %d" code

let ( let*> ) x f = Result.bind (res_of_code x) f

(** Given a Rust file, parse it into LLBC, using Charon. *)
let parse_ullbc_of_file ~no_compile file_name =
  let output = Printf.sprintf "%s.llbc.json" file_name in
  let*> () =
    match no_compile with
    | true -> 0
    | false ->
        Fmt.kstr Sys.command
          "charon --ullbc --rustc-arg=--extern=std --monomorphize \
           --extract-opaque-bodies --no-cargo --include '*' --input %s \
           --dest-file %s"
          file_name output
  in
  let json = Yojson.Basic.from_file output in
  let crate = UllbcOfJson.crate_of_json json in
  (* save crate to local file *)
  let () =
    match (crate, no_compile) with
    | Ok crate, false ->
        let oc = open_out_bin (Printf.sprintf "%s.crate" file_name) in
        let str = PrintUllbcAst.Crate.crate_to_string crate in
        output_string oc str;
        close_out oc
    | _ -> ()
  in
  crate

let exec_main (crate : UllbcAst.crate) =
  let open Syntaxes.Result in
  let open Charon in
  Layout.Session.set_crate crate;
  let ctx = PrintUllbcAst.Crate.crate_to_fmt_env crate in
  UllbcAst.FunDeclId.Map.iter
    (fun _ (f : UllbcAst.fun_decl) ->
      let s = PrintUllbcAst.Ast.any_decl_id_to_string (Types.IdFun f.def_id) in
      let name = PrintTypes.name_to_string ctx f.item_meta.name in
      Fmt.pr " %s, %s\n" name s)
    crate.fun_decls;
  let+ _, entry_point =
    Types.FunDeclId.Map.bindings crate.fun_decls
    |> List.find_opt (fun (_, (decl : UllbcAst.blocks UllbcAst.gfun_decl)) ->
           let name_rev = List.rev decl.item_meta.name in
           (* TODO: is the entry point the function with index 0? *)
           match (decl.item_meta.name, name_rev) with
           | PeIdent (root, _) :: _, PeIdent (fn, _) :: _
             when root = crate.name && fn = "main" ->
               true
           | _ -> false)
    |> Option.fold ~none:(Error "No main function found") ~some:Result.ok
  in
  let symex =
    Wpst_interp.exec_fun ~prog:crate ~args:[] ~state:Heap.empty entry_point
  in
  Rustsymex.run symex

let exec_main_and_print log_level smt_file no_compile file_name =
  let open Syntaxes.Result in
  Z3solver.set_smt_file smt_file;
  setup_console_log log_level;
  let res =
    let* crate = parse_ullbc_of_file ~no_compile file_name in
    let* res = exec_main crate in
    let errors = Compo_res.only_errors @@ List.map fst res in
    if List.is_empty errors then Ok (List.length res)
    else
      let errors_parsed = Fmt.str "Errors: %a" Fmt.(Dump.list pp_err) errors in
      Error errors_parsed
  in
  match res with
  | Ok n -> Fmt.pr "Done. - Ran %i branches" n
  | Error e -> Fmt.pr "Error: %s" e
