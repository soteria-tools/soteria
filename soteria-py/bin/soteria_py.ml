open Cmdliner
open Soteria_py_lib

let logs_term = Soteria_logs.Cli.term

let filename =
  let doc = "Input python filename" in
  let docv = "FILE" in
  Arg.(required & pos 0 (some string) None & info [] ~docv ~doc)

module Print_module = struct
  let cmd =
    let doc = "Prints module as parsed by Pyre in sexp form" in
    let info = Cmd.info "print-ast" ~doc in
    Cmd.v info Term.(const Driver.print_module $ filename)
end

module Exec = struct
  let cmd =
    let doc = "Symbolically execute a python file" in
    let info = Cmd.info "exec" ~doc in
    Cmd.v info Term.(const Driver.exec_module_and_print $ logs_term $ filename)
end

let cmd = Cmd.group (Cmd.info "soteria-py") [ Exec.cmd; Print_module.cmd ]
let () = exit @@ Cmd.eval cmd
