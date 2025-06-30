open Cmdliner
open Soteria_py_lib

let filename =
  let doc = "Input python filename" in
  let docv = "FILE" in
  Arg.(required & pos 0 (some string) None & info [] ~docv ~doc)

module Exec = struct
  let cmd =
    let doc = "Symbolically execute a python file" in
    let info = Cmd.info "exec" ~doc in
    Cmd.v info Term.(const Driver.exec_module_and_print $ filename)
end

let cmd = Cmd.group (Cmd.info "soteria-py") [ Exec.cmd ]
let () = exit @@ Cmd.eval cmd
