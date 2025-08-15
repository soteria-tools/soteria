open Cmdliner

module Config = struct
  open Soteria_rust_lib.Config

  let no_compile_flag =
    let doc = "Do not compile the Rust code, as it is already compiled" in
    Arg.(value & flag & info [ "no-compile" ] ~doc)

  let cleanup_flag =
    let doc = "Clean up compiles files after execution" in
    Arg.(value & flag & info [ "clean" ] ~doc)

  let with_kani_flag =
    let doc = "Use the Kani library" in
    Arg.(value & flag & info [ "kani" ] ~doc)

  let with_miri_flag =
    let doc = "Use the Miri library" in
    Arg.(value & flag & info [ "miri" ] ~doc)

  let make_from_args no_compile cleanup with_kani with_miri =
    make ~no_compile ~cleanup ~with_kani ~with_miri ~ignore_leaks:true
      ~monomorphize:true ~ignore_aliasing:true ()

  let term =
    Cmdliner.Term.(
      const make_from_args
      $ no_compile_flag
      $ cleanup_flag
      $ with_kani_flag
      $ with_miri_flag)
end

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
      $ Config.term)
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

let term =
  Term.(const Ruxt_lib.Refute.exec_ruxt $ Global_config.term $ file_arg)

let cmd = Cmd.v (Cmd.info ~exits "ruxt") term
let () = exit @@ Cmd.eval cmd
