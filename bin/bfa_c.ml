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

let exec_main_t =
  Term.(
    const Bfa_c_lib.Driver.exec_main
    $ Logs_cli.level () $ dump_smt_arg $ file_arg)

let cmd = Cmd.v (Cmd.info "bfa-c") exec_main_t
let () = exit @@ Cmd.eval cmd
