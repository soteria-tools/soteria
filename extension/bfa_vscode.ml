open Vscode
open Promise.Syntax

let activate (extension : Vscode.ExtensionContext.t) =
  let instance = Server.empty_instance () in
  let+ () = Server.start instance in
  ExtensionContext.subscribe ~disposable:(Server.disposable instance) extension

let () =
  let open Js_of_ocaml.Js in
  export "activate" (wrap_callback activate)
