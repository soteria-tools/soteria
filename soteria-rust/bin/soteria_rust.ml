open Soteria.Soteria_std.Cmdliner_helpers
open Cmdliner

let exits =
  [
    Cmd.Exit.info ~doc:"on success" 0;
    Cmd.Exit.info ~doc:"on failure (bug or error found)" 1;
    Cmd.Exit.info ~doc:"on crash caused by Soteria Rust" 2;
    Cmd.Exit.info ~doc:"on crash caused by Charon" 3;
  ]

let dir_arg =
  Arg.(
    required
    & pos 0 (some file_or_dir_as_absolute) None
    & info [] ~doc:"The .rs file or the directory of the crate to analyse")

module Exec = struct
  let term =
    Term.(
      const Soteria_rust_lib.Driver.exec_wpst
      $ Soteria_rust_lib.Config.global_term
      $ dir_arg)

  let cmd =
    Cmd.make
      (Cmd.info ~exits ~doc:"Run symbolic execution"
         ~man:
           [
             `P
               "Run Soteria Rust on the specified file or crate; this will \
                either use Rustc to compile that file only, or use Cargo to \
                compile the whole crate if it's a directory. It will then look \
                for all entrypoints and execute them symbolically.";
           ]
         "exec")
      term
end

module Build_plugins = struct
  let term =
    Term.(
      const Soteria_rust_lib.Driver.build_plugins
      $ Soteria_rust_lib.Config.global_term)

  let cmd =
    Cmd.make
      (Cmd.info ~exits ~doc:"Build plugins"
         ~man:
           [
             `P
               "Build the plugins for Soteria Rust; this is done automatically \
                when running Soteria Rust except when --no-compile-plugins is \
                used, so you should only need to run this command if you want \
                to build the plugins separately.";
           ]
         "build-plugins")
      term
end

module Biab = struct
  let term =
    Term.(
      const Soteria_rust_lib.Driver.exec_biab
      $ Soteria_rust_lib.Config.global_term
      $ dir_arg)

  let cmd =
    Cmd.make
      (Cmd.info ~exits ~doc:"Run bi-abduction"
         ~man:
           [
             `P
               "Perform bi-abduction on the specified file or crate; will \
                analyse all functions compositionally, and look for reachable \
                bugs.";
           ]
         "auto")
      term
end

let cmd =
  Cmd.group
    (Cmd.info ~exits "soteria-rust")
    [ Exec.cmd; Biab.cmd; Build_plugins.cmd ]

let () =
  Printexc.record_backtrace true;
  exit @@ Cmd.eval cmd
