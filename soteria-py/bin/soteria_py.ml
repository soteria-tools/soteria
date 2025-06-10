open Cmdliner
open Soteria_py_lib

let filename =
  let doc = "Input python filename" in
  let docv = "FILE" in
  Arg.(required & pos 0 (some string) None & info [] ~docv ~doc)

let cmd =
  let doc = "Process a file" in
  let info = Cmd.info "soteria-py" ~doc in
  Cmd.v info Term.(const Driver.parse_py $ filename)

let () = exit @@ Cmd.eval cmd
