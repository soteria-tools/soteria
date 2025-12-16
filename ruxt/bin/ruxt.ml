open Cmdliner

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
  Term.(
    const Ruxt_lib.Refute.exec_ruxt $ Ruxt_lib.Config.global_term $ file_arg)

let cmd = Cmd.v (Cmd.info ~exits "ruxt") term
let () = exit @@ Cmd.eval cmd
