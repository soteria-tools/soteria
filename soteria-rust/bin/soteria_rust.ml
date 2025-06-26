open Cmdliner

module Config = struct
  open Soteria_rust_lib.Config

  let no_compile_flag =
    let doc = "Do not compile the Rust code, as it is already compiled" in
    Arg.(value & flag & info [ "no-compile" ] ~doc)

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

  let make_from_args no_compile cleanup ignore_leaks ignore_aliasing with_kani
      with_miri =
    make ~no_compile ~cleanup ~ignore_leaks ~ignore_aliasing ~with_kani
      ~with_miri ()

  let term =
    Cmdliner.Term.(
      const make_from_args
      $ no_compile_flag
      $ cleanup_flag
      $ ignore_leaks_flag
      $ ignore_aliasing_flag
      $ with_kani_flag
      $ with_miri_flag)
end

let file_arg =
  let doc = "FILE" in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

module Exec_main = struct
  let term =
    Term.(
      const Soteria_rust_lib.Driver.exec_main_and_print
      $ Soteria_logs.Cli.term
      $ Soteria_terminal.Cli.term
      $ Soteria_c_values.Solver_config.Cli.term
      $ Config.term
      $ file_arg)

  let cmd = Cmd.v (Cmd.info "exec-main") term
end

let cmd = Cmd.group (Cmd.info "soteria-rust") [ Exec_main.cmd ]
let () = exit @@ Cmd.eval cmd
