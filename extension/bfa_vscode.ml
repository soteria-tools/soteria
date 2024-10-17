open Import

let storage_path instance extension =
  let uri = ExtensionContext.globalStorageUri extension in
  let path = Uri.fsPath uri in
  Server.log_message ~instance `Info "Got storage path: %s" path;
  path

let activate (extension : Vscode.ExtensionContext.t) =
  let instance = Server.empty_instance () in
  Server.log_message ~instance `Info "About to get storage path";
  let* _ = Install.check_version (storage_path instance extension) in
  let* () = Server.start instance in
  ExtensionContext.subscribe ~disposable:(Server.disposable instance) extension;
  Bfa_commands.register_all_commands extension instance;
  Promise.return ()

let () =
  let open Js_of_ocaml.Js in
  export "activate" (wrap_callback activate)
