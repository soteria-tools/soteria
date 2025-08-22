open Cmdliner

module Global_config = struct
  open Ruxt_lib.Config

  let make_from_args logs terminal solver ruxt =
    make_global ~logs ~terminal ~solver ~ruxt

  let term =
    Cmdliner.Term.(
      const make_from_args
      $ Soteria_logs.Cli.term
      $ Soteria_terminal.Config.cmdliner_term ()
      $ Soteria_c_values.Solver_config.Cli.term
      $ Ruxt_lib.Config.cmdliner_term ())
end

let exits =
  [
    Cmd.Exit.info ~doc:"on success" 0;
    Cmd.Exit.info ~doc:"on failure (bug or error found)" 1;
    Cmd.Exit.info ~doc:"on crash caused by RUXt" 2;
    Cmd.Exit.info ~doc:"on crash caused by Rusteria" 3;
    Cmd.Exit.info ~doc:"on crash caused by Charon" 4;
  ]

let file_arg =
  let doc = "FILE" in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

let term =
  Term.(const Ruxt_lib.Refute.exec_ruxt $ Global_config.term $ file_arg)

let cmd = Cmd.v (Cmd.info ~exits "ruxt") term
let () = exit @@ Cmd.eval cmd
