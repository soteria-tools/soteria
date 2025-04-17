open Import

type t = { server : LanguageClient.t option ref; installed_path : string }

let empty extension =
  let installed_path = Filename.concat (storage_path extension) "installed" in
  { server = ref None; installed_path }

let server_exe (exe : Install.executable) =
  let env = Interop.Dict.of_list exe.env in
  let opt = ExecutableOptions.create ~shell:false ~env () in
  Executable.create ~command:exe.command ~args:exe.args ~options:opt ()

let server_options instance =
  let open Settings.Server_kind in
  let server_kind = Settings.Server_kind.get () in
  match server_kind with
  | Shell exe ->
      Logging.info "Using shell server!";
      server_exe exe
  | Auto ->
      Logging.info "Using auto-installed server!";
      let exe = Install.executable instance.installed_path in
      server_exe exe

let client_options _instance =
  let documentSelector = DocumentSelector.[| language "c" |] in
  let (lazy oc) = Logging.output_channel in
  let revealOutputChannelOn = RevealOutputChannelOn.Never in
  ClientOptions.create ~documentSelector ~outputChannel:oc
    ~revealOutputChannelOn ()

let stop_server (instance : t) =
  match !(instance.server) with
  | None -> Promise.return ()
  | Some client ->
      instance.server := None;
      if LanguageClient.isRunning client then LanguageClient.stop client
      else Promise.return ()

(* Restart anguage server *)
let start_server instance =
  let* () = stop_server instance in
  let clientOptions = client_options instance in
  let serverOptions = server_options instance in
  let client =
    LanguageClient.make ~id:"soteria-c" ~name:"Soteria for C" ~serverOptions
      ~clientOptions ()
  in
  instance.server := Some client;
  LanguageClient.start client

let toggle_debug_mode instance =
  match !(instance.server) with
  | None -> show_message `Warn "Server is not running"
  | Some server ->
      (* TODO: Put constant in a common library shared between frontend and backend *)
      LanguageClient.sendNotification server "soteria/toggleDebugMode" Ojs.null

let disposable instance =
  Disposable.make ~dispose:(fun () ->
      let (_ : unit Promise.t) = stop_server instance in
      ())
