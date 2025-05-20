open Cmdliner

module Config = struct
  open Soteria_c_lib.Config

  let auto_include_path_arg =
    let doc = "Path to the directory that contains the soteria-c.h" in
    let env = Cmdliner.Cmd.Env.info ~doc "SOTERIA_AUTO_INCLUDE_PATH" in
    Arg.(
      value
      & opt dir default.auto_include_path
      & info [ "auto-include-path" ] ~env ~doc)

  let dump_smt_arg =
    let doc = "Dump the SMT queries to the given file" in
    Arg.(
      value
      & opt (some string) default.dump_smt_file
      & info [ "dump-smt-to"; "dump-smt" ] ~docv:"SMT_FILE" ~doc)

  let dump_unsupported_arg =
    let doc =
      "Dump a json file with unsupported features and their number of \
       occurences"
    in
    Arg.(
      value
      & opt (some string) default.dump_unsupported_file
      & info [ "dump-unsupported" ] ~docv:"FILE" ~doc)

  let solver_timeout_arg =
    let doc = "Set the solver timeout in miliseconds" in
    Arg.(
      value
      & opt (some int) default.solver_timeout
      & info [ "solver-timeout" ] ~doc ~docv:"TIMEOUT")

  let z3_path_arg =
    let doc = "Path to the Z3 executable" in
    let env = Cmdliner.Cmd.Env.info ~doc "SOTERIA_Z3_PATH" in
    Arg.(value & opt string default.z3_path & info [ "z3-path" ] ~env ~doc)

  let no_ignore_parse_failures_arg =
    let doc =
      "Files that cannot be parsed correctly are ignored by default, this flag \
       deactivates that behaviour."
    in
    let env = Cmdliner.Cmd.Env.info ~doc "SOTERIA_IGNORE_PARSE_FAILURES" in
    Arg.(value & flag & info [ "no-ignore-parse-failures" ] ~env ~doc)

  let no_ignore_duplicate_symbols_arg =
    let doc =
      "Programs that contain duplicate symbols are ignored by default, this \
       flag deactivates that behaviour."
    in
    let env = Cmdliner.Cmd.Env.info ~doc "SOTERIA_IGNORE_DUPLICATE_SYMBOLS" in
    Arg.(value & flag & info [ "no-ignore-duplicate-symbols" ] ~env ~doc)

  let make_from_args auto_include_path dump_smt_file dump_unsupported_file
      solver_timeout z3_path no_ignore_parse_failures
      no_ignore_duplicate_symbols =
    make ~auto_include_path ~dump_smt_file ~dump_unsupported_file
      ~solver_timeout ~z3_path ~no_ignore_parse_failures
      ~no_ignore_duplicate_symbols ()

  let term =
    Cmdliner.Term.(
      const make_from_args
      $ auto_include_path_arg
      $ dump_smt_arg
      $ dump_unsupported_arg
      $ solver_timeout_arg
      $ z3_path_arg
      $ no_ignore_parse_failures_arg
      $ no_ignore_duplicate_symbols_arg)
end

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
  let term =
    Term.(
      const Soteria_c_lib.Driver.exec_main_and_print
      $ Soteria_logs.Cli.term
      $ Config.term
      $ includes_arg
      $ files_arg)

  let cmd = Cmd.v (Cmd.info "exec-main") term
end

module Lsp = struct
  let lsp show_version =
    if show_version then print_endline "dev" else Soteria_c_lib.Driver.lsp ()

  let term = Term.(const lsp $ version_arg)
  let cmd = Cmd.v (Cmd.info "lsp") term
end

module Show_ail = struct
  let term =
    Term.(const Soteria_c_lib.Driver.show_ail $ includes_arg $ files_arg)

  let cmd = Cmd.v (Cmd.info "show-ail") term
end

module Generate_summary = struct
  let fun_name_arg =
    let doc = "FUNCTION" in
    Arg.(
      required
      & opt (some string) None
      & info [ "f"; "fn"; "function" ] ~docv:"FUNCTION" ~doc)

  let term =
    Term.(
      const Soteria_c_lib.Driver.generate_summary_for
      $ Soteria_logs.Cli.term
      $ Config.term
      $ includes_arg
      $ files_arg
      $ fun_name_arg)

  let cmd = Cmd.v (Cmd.info "gen-summary") term
end

module Generate_summaries = struct
  let term =
    Term.(
      const Soteria_c_lib.Driver.generate_all_summaries
      $ Soteria_logs.Cli.term
      $ Config.term
      $ includes_arg
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
      $ Soteria_logs.Cli.term
      $ Config.term
      $ compilation_db_arg)

  let cmd = Cmd.v (Cmd.info "capture-db") term
end

let cmd =
  Cmd.group (Cmd.info "soteria-c")
    [
      Exec_main.cmd;
      Lsp.cmd;
      Show_ail.cmd;
      Generate_summary.cmd;
      Generate_summaries.cmd;
      Capture_db.cmd;
    ]

let () = exit @@ Cmd.eval cmd
