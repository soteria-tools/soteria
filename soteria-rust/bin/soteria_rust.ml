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
             `S Cmdliner.Manpage.s_description;
             `P
               "Run Soteria Rust on the specified file or crate; this will \
                either use Rustc to compile that file only, or use Cargo to \
                compile the whole crate if it's a directory. It will then look \
                for all entrypoints and execute them symbolically.";
           ]
         "exec")
      term
end

module Compile = struct
  let term =
    Term.(
      const Soteria_rust_lib.Driver.compile
      $ Soteria_rust_lib.Config.global_term
      $ dir_arg)

  let cmd =
    Cmd.make
      (Cmd.info ~exits ~doc:"Compile without running any analysis"
         ~man:
           [
             `S Cmdliner.Manpage.s_description;
             `P
               "Compile the specified file or crate to ULLBC, as [exec] would, \
                but do not run any symbolic execution. This is useful to warm \
                up the compilation cache, or, combined with --list-tests, to \
                discover the available entry points. With --list-tests, the \
                JSON list of entry points is the only thing printed to stdout \
                (the compilation progress goes to stderr), so it can be piped \
                directly into e.g. [jq].";
           ]
         "compile")
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
             `S Cmdliner.Manpage.s_description;
             `P
               "Build the plugins for Soteria Rust; this is done automatically \
                when running Soteria Rust except when --no-compile-plugins is \
                used, so you should only need to run this command if you want \
                to build the plugins separately.";
           ]
         "build-plugins")
      term
end

module Version = struct
  let print_version () =
    Fmt.pr "%s@.@.charon: %s@.charon hash: %s@.obol hash: %s@."
      Soteria.Version.version Charon.CharonVersion.supported_charon_version
      Soteria_rust_lib.Version.charon_commit
      Soteria_rust_lib.Version.obol_commit

  let term = Term.(const print_version $ const ())

  let cmd =
    Cmd.make
      (Cmd.info ~exits ~doc:"Print version information"
         ~man:
           [
             `S Cmdliner.Manpage.s_description;
             `P
               "Print the version of Soteria Rust, Charon, and the git commit \
                hashes of the Soteria Rust and Obol repositories.";
           ]
         "version")
      term
end

let cmd =
  Cmd.group
    (Cmd.info ~exits ~version:Soteria.Version.version "soteria-rust")
    [ Exec.cmd; Compile.cmd; Build_plugins.cmd; Version.cmd ]

let () =
  (* Trade extra memory for better performance (~2% across benchmarks) *)
  Gc.set { (Gc.get ()) with space_overhead = 240 };
  exit @@ Cmd.eval cmd
