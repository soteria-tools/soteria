open Cmdliner

let file_arg =
  let doc = "FILE" in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

let dump_smt_arg =
  let doc = "Dump the SMT queries to the given file" in
  Arg.(
    value
    & opt (some string) None
    & info [ "dump-smt-to"; "dump-smt" ] ~docv:"SMT_FILE" ~doc)

module Exec_main = struct
  let term =
    Term.(
      const Bfa_rust_lib.Driver.exec_main_and_print
      $ Logs_cli.level ()
      $ dump_smt_arg
      $ file_arg)

  let cmd = Cmd.v (Cmd.info "exec-main") term
end

let cmd = Cmd.group (Cmd.info "bfa-rust") [ Exec_main.cmd ]
let () = exit @@ Cmd.eval cmd
