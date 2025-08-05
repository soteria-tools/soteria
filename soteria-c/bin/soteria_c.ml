open Cmdliner

let functions_arg =
  let doc = "List of functions to analyse" in
  let docv = "FUNCTION_NAME" in
  Arg.(value & opt_all string [] & info [ "f" ] ~doc ~docv)

module Config = struct
  open Soteria_c_lib.Config
  open Soteria_std.Cmdliner_helpers

  let auto_include_path_arg =
    let doc = "Path to the directory that contains the soteria-c.h" in
    let env = Cmdliner.Cmd.Env.info ~doc "SOTERIA_AUTO_INCLUDE_PATH" in
    Arg.(
      value
      & opt dir_as_absolute default.auto_include_path
      & info [ "auto-include-path" ] ~env ~doc)

  let dump_unsupported_arg =
    let doc =
      "Dump a json file with unsupported features and their number of \
       occurences"
    in
    Arg.(
      value
      & opt (some string) default.dump_unsupported_file
      & info [ "dump-unsupported" ] ~docv:"FILE" ~doc)

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

  let parse_only_arg =
    let doc = "Only parse and link the C program, do not perform analysis" in
    let env = Cmdliner.Cmd.Env.info ~doc "SOTERIA_PARSE_ONLY" in
    Arg.(value & flag & info [ "parse-only" ] ~env ~doc)

  let dump_summaries_file_arg =
    let doc = "Dump the generated summaries to a file" in
    let docv = "FILE" in
    let env = Cmdliner.Cmd.Env.info ~doc "SOTERIA_DUMP_SUMMARIES_FILE" in
    Arg.(
      value
      & opt (some string) None
      & info [ "dump-summaries"; "dump-summaries-to" ] ~env ~doc ~docv)

  let show_manifest_summaries_arg =
    let doc =
      "Print a corresponding manifest summary after the bug report if a bug is \
       found"
    in
    let env =
      Cmdliner.Cmd.Env.info ~doc "SOTERIA_SHOW_MANIFEST_BUG_SUMMARIES"
    in
    Arg.(value & flag & info [ "show-manifest-summaries" ] ~env ~doc)

  let alloc_cannot_fail_arg =
    let doc = "Assume that all allocations cannot fail" in
    let env = Cmdliner.Cmd.Env.info ~doc "SOTERIA_ALLOC_CANNOT_FAIL" in
    Arg.(value & flag & info [ "alloc-cannot-fail" ] ~env ~doc)

  let use_cerb_headers_arg =
    let doc =
      "Use the Cerberus-provided standard headers instead of the system \
       headers."
    in
    let env = Cmdliner.Cmd.Env.info ~doc "SOTERIA_USE_CERB_HEADERS" in
    Arg.(value & flag & info [ "use-cerb-headers" ] ~env ~doc)

  let infinite_fuel_arg =
    let doc =
      "Infinite fuel for the analysis. If there is an unbounded loop, the \
       analysis will not stop. (Used only for whole-program analysis)"
    in
    let env = Cmdliner.Cmd.Env.info ~doc "SOTERIA_INFINITE_FUEL" in
    Arg.(value & flag & info [ "infinite-fuel" ] ~env ~doc)

  let make_from_args auto_include_path dump_unsupported_file
      no_ignore_parse_failures no_ignore_duplicate_symbols parse_only
      dump_summaries_file show_manifest_summaries alloc_cannot_fail
      use_cerb_headers infinite_fuel =
    make ~auto_include_path ~dump_unsupported_file ~no_ignore_parse_failures
      ~no_ignore_duplicate_symbols ~parse_only ~dump_summaries_file
      ~show_manifest_summaries ~alloc_cannot_fail ~use_cerb_headers
      ~infinite_fuel ()

  let term =
    Cmdliner.Term.(
      const make_from_args
      $ auto_include_path_arg
      $ dump_unsupported_arg
      $ no_ignore_parse_failures_arg
      $ no_ignore_duplicate_symbols_arg
      $ parse_only_arg
      $ dump_summaries_file_arg
      $ show_manifest_summaries_arg
      $ alloc_cannot_fail_arg
      $ use_cerb_headers_arg
      $ infinite_fuel_arg)
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
      $ Soteria_logs.Cli.term
      $ Soteria_terminal.Cli.term
      $ Soteria_c_values.Solver_config.Cli.term
      $ Config.term
      $ includes_arg
      $ files_arg
      $ entry_point_arg)

  let cmd = Cmd.v (Cmd.info "exec") term
end

module Lsp = struct
  let lsp config show_version =
    if show_version then print_endline "dev"
    else Soteria_c_lib.Driver.lsp config ()

  let term = Term.(const lsp $ Config.term $ version_arg)
  let cmd = Cmd.v (Cmd.info "lsp") term
end

module Show_ail = struct
  let term =
    Term.(
      const Soteria_c_lib.Driver.show_ail
      $ Soteria_logs.Cli.term
      $ Soteria_terminal.Cli.term
      $ includes_arg
      $ files_arg)

  let cmd = Cmd.v (Cmd.info "show-ail") term
end

module Generate_summaries = struct
  let term =
    Term.(
      const Soteria_c_lib.Driver.generate_all_summaries
      $ Soteria_logs.Cli.term
      $ Soteria_terminal.Cli.term
      $ Soteria_c_values.Solver_config.Cli.term
      $ Config.term
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
      $ Soteria_logs.Cli.term
      $ Soteria_terminal.Cli.term
      $ Soteria_c_values.Solver_config.Cli.term
      $ Config.term
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
