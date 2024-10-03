open Cmdliner

let file_arg =
  let doc = "FILE" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)

let exec_main_t =
  Term.(const Bfa_c_lib.Driver.exec_main $ Logs_cli.level () $ file_arg)

let cmd = Cmd.v (Cmd.info "bfa-c") exec_main_t
let () = exit @@ Cmd.eval cmd
