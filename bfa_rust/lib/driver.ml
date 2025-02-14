open Charon
module Wpst_interp = Interp.Make (Heap)

let setup_console_log level =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let res_of_code = function
  | 0 -> Ok ()
  | code -> Fmt.error "Error code: %d" code

let ( let*> ) x f = Result.bind (res_of_code x) f

(** Given a Rust file, parse it into LLBC, using Charon. *)
let parse_ullbc_of_file file_name =
  let output = Printf.sprintf "%s.llbc.json" file_name in
  let*> () =
    Printf.sprintf
      "charon --ullbc --rustc-arg=--extern=std --extract-opaque-bodies \
       --no-cargo --include '*' --input %s --dest-file %s"
      file_name output
    |> Sys.command
  in
  let json = Yojson.Basic.from_file output in
  let crate = UllbcOfJson.crate_of_json json in
  crate

let exec_main (crate : UllbcAst.crate) =
  let open Syntaxes.Result in
  let open Charon in
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
  let result = Ok (Rustsymex.run symex) in
  match result with Ok v -> v | Error e -> [ (Error e, []) ]

let exec_main_and_print log_level _smt_file file_name =
  let open Syntaxes.Result in
  setup_console_log log_level;
  let res =
    let* crate = parse_ullbc_of_file file_name in
    let* res = exec_main crate in
    Ok (List.length res)
  in
  match res with
  | Ok n -> Fmt.pf Fmt.stdout "Done. - Ran %i branches" n
  | Error e -> Printf.printf "Error: %s\n" e
