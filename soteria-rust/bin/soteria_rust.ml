open Cmdliner

module Global_config = struct
  open Soteria_rust_lib.Config

  let make_from_args logs terminal solver rusteria =
    make_global ~logs ~terminal ~solver ~rusteria

  let term =
    Cmdliner.Term.(
      const make_from_args
      $ Soteria_logs.Cli.term
      $ Soteria_terminal.Cli.term
      $ Soteria_c_values.Solver_config.Cli.term
      $ Soteria_rust_lib.Config.cmdliner_term ())
end

let exits =
  [
    Cmd.Exit.info ~doc:"on success" 0;
    Cmd.Exit.info ~doc:"on failure (bug or error found)" 1;
    Cmd.Exit.info ~doc:"on crash caused by Rusteria" 2;
    Cmd.Exit.info ~doc:"on crash caused by Charon" 3;
  ]

let file_arg =
  let doc = "FILE" in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

let dir_arg =
  let doc = "DIR" in
  Arg.(required & pos 0 (some dir) None & info [] ~docv:"DIR" ~doc)

module Exec_rustc = struct
  let term =
    Term.(
      const Soteria_rust_lib.Driver.exec_rustc $ Global_config.term $ file_arg)

  let cmd =
    Cmd.v
      (Cmd.info ~exits
         ~doc:
           "Run Rusteria on the specified file; this will use Rustc to compile \
            that file only (not the crate), and look for all entrypoints."
         "rustc")
      term
end

module Exec_cargo = struct
  let term =
    Term.(
      const Soteria_rust_lib.Driver.exec_cargo $ Global_config.term $ dir_arg)

  let cmd =
    Cmd.v
      (Cmd.info ~exits
         ~doc:
           "Run Rusteria on the crate at the specified directory; this will \
            use Cargo to compile that crate, and look for all entrypoints."
         "cargo")
      term
end

module Exec_obol = struct
  let term =
    Term.(
      const Soteria_rust_lib.Driver.exec_obol $ Global_config.term $ file_arg)

  let cmd =
    Cmd.v
      (Cmd.info ~exits
         ~doc:
           "Run Rusteria on the specified file; this will use Obol to compile \
            that file only (not the crate), and look for all entrypoints."
         "obol")
      term
end

let cmd =
  Cmd.group
    (Cmd.info ~exits "soteria-rust")
    [ Exec_rustc.cmd; Exec_cargo.cmd; Exec_obol.cmd ]

let () = exit @@ Cmd.eval cmd
