open Cmdliner

let file_arg =
  let doc = "FILE" in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

let files_arg =
  let doc = "FILES" in
  Arg.(non_empty & pos_all file [] & info [] ~docv:"FILES" ~doc)

let dump_smt_arg =
  let doc = "Dump the SMT queries to the given file" in
  Arg.(
    value
    & opt (some string) None
    & info [ "dump-smt-to"; "dump-smt" ] ~docv:"SMT_FILE" ~doc)

let dump_unsupported_arg =
  let doc =
    "Dump a json file with unsupported features and their number of occurences"
  in
  Arg.(
    value
    & opt (some string) None
    & info [ "dump-unsupported" ] ~docv:"FILE" ~doc)

let version_arg =
  let doc = "Print version information" in
  Arg.(value & flag & info [ "version" ] ~doc)

let includes_arg =
  let doc = "Add a directory to the include path" in
  Arg.(value & opt_all dir [] & info [ "I" ] ~doc ~docv:"DIR")

let solver_timeout_arg =
  let doc = "Set the solver timeout in miliseconds" in
  Arg.(
    value & opt (some int) None & info [ "solver-timeout" ] ~doc ~docv:"TIMEOUT")

module Exec_main = struct
  let term =
    Term.(
      const Soteria_c_lib.Driver.exec_main_and_print
      $ Soteria_logs.Cli.term
      $ dump_smt_arg
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

module Bi_main = struct
  let term = Term.(const Soteria_c_lib.Driver.generate_main_summary $ file_arg)
  let cmd = Cmd.v (Cmd.info "bi-main") term
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
      $ solver_timeout_arg
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
      $ solver_timeout_arg
      $ dump_unsupported_arg
      $ dump_smt_arg
      $ includes_arg
      $ files_arg)

  let cmd = Cmd.v (Cmd.info "gen-summaries") term
end

let cmd =
  Cmd.group (Cmd.info "soteria-c")
    [
      Exec_main.cmd;
      Lsp.cmd;
      Show_ail.cmd;
      Bi_main.cmd;
      Generate_summary.cmd;
      Generate_summaries.cmd;
    ]

let () = exit @@ Cmd.eval cmd
