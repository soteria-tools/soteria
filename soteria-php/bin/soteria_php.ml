open Cmdliner

let exits =
  [
    Cmd.Exit.info ~doc:"on success" 0;
    Cmd.Exit.info ~doc:"on a frontend or IR error" 2;
  ]

let file_arg =
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc:"PHP file")

let parse_command =
  let term = Term.(const Soteria_php.Driver.parse $ file_arg) in
  Cmd.make
    (Cmd.info ~exits ~doc:"Parse PHP source and print validated Soteria PHP IR"
       "parse")
    term

let command =
  Cmd.group
    (Cmd.info ~exits ~version:Soteria.Version.version
       ~doc:"Symbolic execution and bug finding for PHP" "soteria-php")
    [ parse_command ]

let () = exit (Cmd.eval' command)
