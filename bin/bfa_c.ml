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
    const Bfa_c_lib.Driver.exec_main_and_print
    $ Logs_cli.level () $ dump_smt_arg $ file_arg)

let exec_main_cmd = Cmd.v (Cmd.info "exec-main") exec_main_t
let lsp_t = Term.(const Bfa_c_lib.Driver.lsp $ const ())
let lsp_cmd = Cmd.v (Cmd.info "lsp") lsp_t
let cmd = Cmd.group (Cmd.info "bfa-c") [ exec_main_cmd; lsp_cmd ]
let () = exit @@ Cmd.eval cmd
