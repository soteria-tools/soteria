open Cmdliner

module Config = struct
  open Soteria_rust_lib.Config

  let no_compile_flag =
    let doc = "Do not compile the Rust code, as it is already compiled" in
    Arg.(value & flag & info [ "no-compile" ] ~doc)

  let no_timing =
    let doc = "Do not display execution times" in
    let env = Cmdliner.Cmd.Env.info ~doc "NO_TIMING" in
    Arg.(value & flag & info [ "no-timing" ] ~doc ~env)

  let cleanup_flag =
    let doc = "Clean up compiles files after execution" in
    Arg.(value & flag & info [ "clean" ] ~doc)

  let ignore_leaks_flag =
    let doc = "Ignore memory leaks" in
    Arg.(value & flag & info [ "ignore-leaks" ] ~doc)

  let ignore_aliasing_flag =
    let doc = "Ignore pointer aliasing rules (tree borrows)" in
    Arg.(value & flag & info [ "ignore-aliasing" ] ~doc)

  let with_kani_flag =
    let doc = "Use the Kani library" in
    Arg.(value & flag & info [ "kani" ] ~doc)

  let with_miri_flag =
    let doc = "Use the Miri library" in
    Arg.(value & flag & info [ "miri" ] ~doc)

  let log_compilation_flag =
    let doc = "Log the compilation process" in
    Arg.(value & flag & info [ "log-compilation" ] ~doc)

  let step_fuel_flag =
    let doc =
      "The default step fuel for each entrypoint -- every control flow jump \
       counts as one fuel"
    in
    let env = Cmdliner.Cmd.Env.info ~doc "STEP_FUEL" in
    Arg.(value & opt int 1000 & info [ "step-fuel" ] ~doc ~env)

  let branch_fuel_flag =
    let doc =
      "The default branch fuel for each entrypoint -- every symbolic execution \
       branching point counts as one fuel"
    in
    let env = Cmdliner.Cmd.Env.info ~doc "BRANCH_FUEL" in
    Arg.(value & opt int 4 & info [ "branch-fuel" ] ~doc ~env)

  let make_from_args no_compile no_timing cleanup ignore_leaks ignore_aliasing
      with_kani with_miri log_compilation step_fuel branch_fuel =
    make ~no_compile ~no_timing ~cleanup ~ignore_leaks ~ignore_aliasing
      ~with_kani ~with_miri ~log_compilation ~step_fuel ~branch_fuel ()

  let term =
    Cmdliner.Term.(
      const make_from_args
      $ no_compile_flag
      $ no_timing
      $ cleanup_flag
      $ ignore_leaks_flag
      $ ignore_aliasing_flag
      $ with_kani_flag
      $ with_miri_flag
      $ log_compilation_flag
      $ step_fuel_flag
      $ branch_fuel_flag)
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
