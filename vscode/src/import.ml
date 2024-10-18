include Vscode
include Vscode_languageclient
include Promise.Syntax

exception StopExtension of string

let ( let@ ) = ( @@ )

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

let storage_path extension =
  let uri = ExtensionContext.globalStorageUri extension in
  let path = Uri.fsPath uri in
  Logging.debug "Global storage path is: %s" path;
  path

let install_path extension =
  let ( / ) = Filename.concat in
  storage_path extension / "installed"
