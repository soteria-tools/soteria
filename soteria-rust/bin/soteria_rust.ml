open Cmdliner

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
      const Soteria_rust_lib.Driver.exec_rustc
      $ Soteria_rust_lib.Config.global_term
      $ file_arg)

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
      const Soteria_rust_lib.Driver.exec_cargo
      $ Soteria_rust_lib.Config.global_term
      $ dir_arg)

  let cmd =
    Cmd.v
      (Cmd.info ~exits
         ~doc:
           "Run Rusteria on the crate at the specified directory; this will \
            use Cargo to compile that crate, and look for all entrypoints."
         "cargo")
      term
end

let cmd =
  Cmd.group (Cmd.info ~exits "soteria-rust") [ Exec_rustc.cmd; Exec_cargo.cmd ]

let () = exit @@ Cmd.eval cmd
