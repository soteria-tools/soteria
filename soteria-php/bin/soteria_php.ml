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
          ~doc:"Execute the named zero-argument function as the entry point")
  in
  let term =
    Term.(const Soteria_php.Driver.execute $ fuel $ function_name $ file_arg)
  in
  Cmd.make
    (Cmd.info ~exits ~doc:"Symbolically execute a supported PHP script" "exec")
    term

let command =
  Cmd.group
    (Cmd.info ~exits ~version:Soteria.Version.version
       ~doc:"Symbolic execution and bug finding for PHP" "soteria-php")
    [ parse_command; exec_command ]

let () = exit (Cmd.eval' command)
