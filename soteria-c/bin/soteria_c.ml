open Cmdliner

let functions_arg =
  let doc = "List of functions to analyse" in
  let docv = "FUNCTION_NAME" in
  Arg.(value & opt_all string [] & info [ "f" ] ~doc ~docv)

let files_arg =
  let doc = "FILES" in
  Arg.(non_empty & pos_all file [] & info [] ~docv:"FILES" ~doc)

let version_arg =
  let doc = "Print version information" in
  Arg.(value & flag & info [ "version" ] ~doc)

let includes_arg =
  let doc = "Add a directory to the include path" in
  Arg.(value & opt_all dir [] & info [ "I" ] ~doc ~docv:"DIR")

module Exec_main = struct
  let entry_point_arg =
    let doc = "Entry point of the program to execute" in
    let docv = "ENTRYPOINT" in
    Arg.(
      value
      & opt string "main"
      & info [ "entry"; "entry-point"; "harness" ] ~doc ~docv)

  let term =
    Term.(
      const Soteria_c_lib.Driver.exec_and_print
      $ Soteria.Logs.Cli.term
      $ Soteria.Terminal.Config.cmdliner_term ()
      $ Soteria.Solvers.Config.cmdliner_term ()
      $ Soteria_c_lib.Config.cmdliner_term ()
      $ includes_arg
      $ files_arg
      $ entry_point_arg)

  let cmd = Cmd.v (Cmd.info "exec") term
end

module Lsp = struct
  let lsp config show_version =
    if show_version then print_endline "dev"
    else Soteria_c_lib.Driver.lsp config ()

  let term =
    Term.(const lsp $ Soteria_c_lib.Config.cmdliner_term () $ version_arg)

  let cmd = Cmd.v (Cmd.info "lsp") term
end

module Show_ail = struct
  let term =
    Term.(
      const Soteria_c_lib.Driver.show_ail
      $ Soteria.Logs.Cli.term
      $ Soteria.Terminal.Config.cmdliner_term ()
      $ includes_arg
      $ files_arg)

  let cmd = Cmd.v (Cmd.info "show-ail") term
end

module Generate_summaries = struct
  let term =
    Term.(
      const Soteria_c_lib.Driver.generate_all_summaries
      $ Soteria.Logs.Cli.term
      $ Soteria.Terminal.Config.cmdliner_term ()
      $ Soteria.Solvers.Config.cmdliner_term ()
      $ Soteria_c_lib.Config.cmdliner_term ()
      $ includes_arg
      $ functions_arg
      $ files_arg)

  let cmd = Cmd.v (Cmd.info "gen-summaries") term
end

module Capture_db = struct
  let compilation_db_arg =
    let doc = "JSON file following the Clang compilation database format" in
    let docv = "COMPILE_COMMANDS.JSON" in
    Arg.(required & pos 0 (some file) None & info [] ~doc ~docv)

  let term =
    Term.(
      const Soteria_c_lib.Driver.capture_db
      $ Soteria.Logs.Cli.term
      $ Soteria.Terminal.Config.cmdliner_term ()
      $ Soteria.Solvers.Config.cmdliner_term ()
      $ Soteria_c_lib.Config.cmdliner_term ()
      $ compilation_db_arg
      $ functions_arg)

  let cmd = Cmd.v (Cmd.info "capture-db") term
end

let cmd =
  Cmd.group (Cmd.info "soteria-c")
    [
      Exec_main.cmd;
      Lsp.cmd;
      Show_ail.cmd;
      Generate_summaries.cmd;
      Capture_db.cmd;
    ]

let () = exit @@ Cmd.eval cmd
