open Soteria_c_lib
open Cmdliner
open Soteria.Soteria_std.Cmdliner_helpers
module Exit_code = Error.Exit_code

let exits =
  List.map
    (fun code ->
      Cmd.Exit.info ~doc:(Exit_code.explain code) (Exit_code.to_int code))
    [ Success; Found_bug; Tool_error ]

let functions_arg =
  let doc = "List of functions to analyse" in
  let docv = "FUNCTION_NAME" in
  Arg.(
    value & opt_all string [] & info [ "f" ] ~docs:Sections.frontend ~doc ~docv)

let files_arg =
  let doc = "FILES" in
  Arg.(non_empty & pos_all file [] & info [] ~docv:"FILES" ~doc)

let includes_arg =
  let doc = "Add a directory to the include path" in
  Arg.(
    value
    & opt_all dir []
    & info [ "I" ] ~docs:Sections.frontend ~doc ~docv:"DIR")

module Exec_main = struct
  let entry_point_arg =
    let doc = "Entry point of the program to execute" in
    let docv = "ENTRYPOINT" in
    Arg.(
      value
      & opt string "main"
      & info
          [ "entry"; "entry-point"; "harness" ]
          ~docs:Sections.frontend ~doc ~docv)

  let term =
    Term.(
      const Soteria_c_lib.Driver.exec_and_print
      $ Soteria.Config.cmdliner_term ()
      $ Soteria_c_lib.Config.cmdliner_term ()
      $ Soteria.Symex.Fuel_gauge.Cli.term ~default:Driver.default_wpst_fuel ()
      $ includes_arg
      $ files_arg
      $ entry_point_arg)

  let cmd =
    Cmd.v
      (Cmd.info ~exits
         ~doc:"Symbolically execute a program starting from the main function."
         "exec")
      (Term.map Exit_code.to_int term)
end

module Show_ail = struct
  let term =
    Term.(
      const Soteria_c_lib.Driver.show_ail
      $ Soteria.Config.cmdliner_term ()
      $ Soteria_c_lib.Config.cmdliner_term ()
      $ includes_arg
      $ files_arg)

  let cmd =
    Cmd.v
      (Cmd.info ~exits
         ~doc:
           "Parse and link the ail program and print its AST (for debugging \
            purposes)"
         "show-ail")
      (Term.map Exit_code.to_int term)
end

module Generate_summaries = struct
  let term =
    Term.(
      const Soteria_c_lib.Driver.generate_all_summaries
      $ Soteria.Config.cmdliner_term ()
      $ Soteria_c_lib.Config.cmdliner_term ()
      $ includes_arg
      $ functions_arg
      $ files_arg)

  let cmd =
    Cmd.v
      (Cmd.info ~exits
         ~doc:
           "Run soteria-c in bug-finding mode. Soteria will perform \
            bi-abduction and generate summaries for all functions in the code \
            base. It will then analyse these summaries and report any bugs it \
            finds."
         "gen-summaries")
      (Term.map Exit_code.to_int term)
end

module Capture_db = struct
  let compilation_db_arg =
    let doc = "JSON file following the Clang compilation database format" in
    let docv = "COMPILE_COMMANDS.JSON" in
    Arg.(required & pos 0 (some file) None & info [] ~doc ~docv)

  let term =
    Term.(
      const Soteria_c_lib.Driver.capture_db
      $ Soteria.Config.cmdliner_term ()
      $ Soteria_c_lib.Config.cmdliner_term ()
      $ compilation_db_arg
      $ functions_arg)

  let cmd =
    Cmd.v
      (Cmd.info ~exits
         ~doc:"Same as gen-summaries but runs on a compilation database."
         "capture-db")
      (Term.map Exit_code.to_int term)
end

module Version = struct
  let print_version () =
    Fmt.pr "%s@." Soteria.Version.version;
    0

  let term = Term.(const print_version $ const ())

  let cmd =
    Cmd.make
      (Cmd.info ~exits ~doc:"Print version information"
         ~man:
           [
             `S Cmdliner.Manpage.s_description;
             `P "Print the version of Soteria C.";
           ]
         "version")
      term
end

let cmd =
  Cmd.group
    (Cmd.info ~version:Soteria.Version.version "soteria-c")
    [
      Exec_main.cmd;
      Show_ail.cmd;
      Generate_summaries.cmd;
      Capture_db.cmd;
      Version.cmd;
    ]

let () = exit @@ Cmd.eval' cmd
