include Vscode
include Vscode_languageclient
include Promise.Syntax

let show_message kind fmt =
  let k message =
    match kind with
    | `Warn -> Window.showWarningMessage ~message ()
    | `Info -> Window.showInformationMessage ~message ()
    | `Error -> Window.showInformationMessage ~message ()
  in
  Printf.ksprintf
    (fun x ->
      let (_ : unit option Promise.t) = k x in
      ())
    fmt
