open Js_of_ocaml

let _start_language_server _t = Promise.return ()
let activate (_extension : Vscode.ExtensionContext.t) = Promise.return ()

let () =
  let open Js_of_ocaml.Js in
  let console = Js.Unsafe.global##.console in
  let _ = ignore @@ console##log (Js.string "Hello from OCaml!") in
  export "activate" (wrap_callback activate)
