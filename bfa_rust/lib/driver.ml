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
let parse_ullbc_of_file ~no_compile file_path =
  let file_name = file_path |> Filename.basename |> Filename.remove_extension in
  let output = Printf.sprintf "../stable-mir-json/%s.smir.json" file_name in
  let*> () =
    match no_compile with
    | true -> 0
    | false ->
        Printf.ksprintf Sys.command
          "cd ../stable-mir-json && cargo run -- ../bfa-ocaml/%s" file_path
  in
  let json = Yojson.Safe.from_file output in
  Omir.parse json

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

let exec_main_and_print log_level _smt_file no_compile file_name =
  let open Syntaxes.Result in
  setup_console_log log_level;
  let res =
    let* crate = parse_ullbc_of_file ~no_compile file_name in
    (* let* res = exec_main crate in *)
    Ok crate
  in
  match res with
  | Ok crate -> Fmt.pf Fmt.stdout "Done. - %a" Omir.pp crate
  | Error e -> Printf.printf "Error: %s\n" e
