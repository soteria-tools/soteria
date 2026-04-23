open Soteria.Soteria_std.Cmdliner_helpers
open Cmdliner

let exits =
  [
    Cmd.Exit.info ~doc:"on success" 0;
    Cmd.Exit.info ~doc:"on failure (bug or error found)" 1;
    Cmd.Exit.info ~doc:"on crash caused by Soteria Rust" 2;
    Cmd.Exit.info ~doc:"on crash caused by Charon" 3;
    Cmd.Exit.info ~doc:"on crash caused by RUXt" 4;
  ]

let dir_arg =
  Arg.(
    required
    & pos 0 (some file_or_dir_as_absolute) None
    & info [] ~doc:"The .rs file or the directory of the crate to analyse")

let term =
  Term.(const Ruxt_lib.Refute.exec_ruxt $ Ruxt_lib.Config.global_term $ dir_arg)

let cmd = Cmd.v (Cmd.info ~exits "ruxt") term
let () = exit @@ Cmd.eval cmd
