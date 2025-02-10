open Charon

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
let parse_llbc_of_file file_name =
  let output = Printf.sprintf "%s.llbc.json" file_name in
  let*> () =
    Printf.sprintf
      "charon --mir_optimized --rustc-arg=--extern=std --extract-opaque-bodies \
       --no-cargo --input %s --dest-file %s"
      file_name output
    |> Sys.command
  in
  let json = Yojson.Basic.from_file output in
  let crate = LlbcOfJson.crate_of_json json in
  crate

let exec_main_and_print log_level _smt_file file_name =
  setup_console_log log_level;
  let res = parse_llbc_of_file file_name in
  match res with
  | Ok crate ->
      Fmt.pf Fmt.stdout "Parsed crate: %s"
        (PrintLlbcAst.Crate.crate_to_string crate)
  | Error e -> Printf.printf "Error: %s\n" e
