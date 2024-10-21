open Import

let should_install ~ask extension =
  match Settings.Server_kind.get () with
  | Auto ->
      (* In auto mode we check for the current installation *)
      let* installed = Install.check_version (install_path extension) in
      if not installed then
        if ask then
          let msg =
            "BFA is not installed. Do you want to install it now? This will \
             download a binary from the internet."
          in
          let action = ("Install", `NeedInstall) in
          let* choice =
            Window.showInformationMessage ~message:msg ~choices:[ action ] ()
          in
          Promise.return (Option.value ~default:`RefusedInstall choice)
        else Promise.return `NeedInstall
      else Promise.return `NoInstall
  | _ -> Promise.return `NoInstall

let with_stop f =
  try f ()
  with StopExtension msg ->
    show_message `Error "Stopping BFA: %s" msg;
    Promise.return ()

let activate (extension : Vscode.ExtensionContext.t) =
  with_stop @@ fun () ->
  let instance = Instance.empty extension in
  let* should_install = should_install ~ask:true extension in
  let* () =
    match should_install with
    | `NeedInstall -> (
        let+ res = Install.install (storage_path extension) in
        match res with
        | Ok () -> ()
        | Error () -> raise (StopExtension "Installation failed"))
    | `RefusedInstall -> raise (StopExtension "Installation refused")
    | `NoInstall -> Promise.return ()
  in
  let* () = Instance.start_server instance in
  ExtensionContext.subscribe
    ~disposable:(Instance.disposable instance)
    extension;
  Bfa_commands.register_all_commands extension instance;
  Promise.return ()

let () =
  let open Js_of_ocaml.Js in
  export "activate" (wrap_callback activate)
