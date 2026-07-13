open Cmdliner

let exits =
  [
    Cmd.Exit.info ~doc:"on success" 0;
    Cmd.Exit.info ~doc:"when execution finds a failure" 1;
    Cmd.Exit.info ~doc:"on a frontend, IR, or entry-point error" 2;
    Cmd.Exit.info ~doc:"when execution is incomplete" 3;
  ]

let file_arg =
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc:"PHP file")

let parse_command =
  let term = Term.(const Soteria_php.Driver.parse $ file_arg) in
  Cmd.make
    (Cmd.info ~exits ~doc:"Parse PHP source and print validated Soteria PHP IR"
       "parse")
    term

let discover_command =
  let term = Term.(const Soteria_php.Driver.discover $ file_arg) in
  Cmd.make
    (Cmd.info ~exits
       ~doc:"List zero-argument entry points marked with #[Soteria\\\\Test]"
       "discover")
    term

let exec_command =
  let fuel =
    Soteria.Symex.Fuel_gauge.Cli.term ~default:Soteria_php.Driver.default_fuel
      ()
  in
  let function_name =
    Arg.(
      value
      & opt (some string) None
      & info [ "function" ] ~docv:"NAME"
          ~doc:
            "Execute the named zero-argument function or Class::method as the \
             entry point")
  in
  let runtime_event_policy =
    let parse = function
      | "conservative" -> Ok Soteria_php.Config.Conservative
      | "report" -> Ok Report
      | "ignore" -> Ok Ignore
      | value -> Error (`Msg ("unknown runtime-event policy " ^ value))
    in
    let print formatter = function
      | Soteria_php.Config.Conservative ->
          Format.pp_print_string formatter "conservative"
      | Report -> Format.pp_print_string formatter "report"
      | Ignore -> Format.pp_print_string formatter "ignore"
    in
    Arg.(
      value
      & opt (conv (parse, print)) Soteria_php.Config.Conservative
      & info [ "runtime-events" ] ~docv:"POLICY"
          ~doc:
            "Handle PHP runtime events using $(docv): conservative treats \
             warnings and errors as bugs; report retains diagnostics; ignore \
             suppresses them")
  in
  let term =
    Term.(
      const Soteria_php.Driver.execute
      $ runtime_event_policy
      $ fuel
      $ function_name
      $ file_arg)
  in
  Cmd.make
    (Cmd.info ~exits ~doc:"Symbolically execute a supported PHP script" "exec")
    term

let command =
  Cmd.group
    (Cmd.info ~exits ~version:Soteria.Version.version
       ~doc:"Symbolic execution and bug finding for PHP" "soteria-php")
    [ parse_command; discover_command; exec_command ]

let () = exit (Cmd.eval' command)
