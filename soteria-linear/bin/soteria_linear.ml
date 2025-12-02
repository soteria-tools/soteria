open Cmdliner

let file_arg =
  let doc = "FILE to parse" in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

let exec_term =
  let exec logs_config solver_config file =
    Soteria_linear_lib.with_config logs_config solver_config (fun () ->
        Soteria_linear_lib.exec file)
  in
  Term.(
    const exec
    $ Soteria.Logs.Cli.term
    $ Soteria.Solvers.Config.cmdliner_term ()
    $ file_arg)

let exec_cmd = Cmd.v (Cmd.info "exec") exec_term

let gen_summaries_ter =
  let gen_summaries logs_config solver_config file =
    Soteria_linear_lib.with_config logs_config solver_config (fun () ->
        Soteria_linear_lib.generate_summaries file)
  in
  Term.(
    const gen_summaries
    $ Soteria.Logs.Cli.term
    $ Soteria.Solvers.Config.cmdliner_term ()
    $ file_arg)

let gen_summaries_cmd = Cmd.v (Cmd.info "gen-summaries") gen_summaries_ter
let cmd = Cmd.group (Cmd.info "soteria-linear") [ exec_cmd; gen_summaries_cmd ]
let () = Cmd.eval cmd |> exit
