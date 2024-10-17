open Import

type instance = {
  server : LanguageClient.t option ref;
  output_channel : OutputChannel.t;
}

let log_message ~instance kind fmt =
  let kind_s =
    match kind with
    | `Warn -> "warn"
    | `Info -> "info"
    | `Error -> "error"
    | `Debug -> "debug"
  in
  Printf.ksprintf
    (fun s ->
      let value = Printf.sprintf "[EXT: %s]: %s" kind_s s in
      OutputChannel.appendLine instance.output_channel ~value)
    fmt

let empty_instance () =
  let output_channel = Window.createOutputChannel ~name:"Bfa-C" in
  { server = ref None; output_channel }

let server_options () =
  let open Settings.Server_kind in
  let server_kind = Settings.(get ~section:"bfa" s) in
  match server_kind with
  | Some (Shell exe) -> exe
  | None | Some Auto ->
      show_message `Error "Cannot handle auto server kind yet";
      raise Exit

let client_options instance =
  let documentSelector = DocumentSelector.[| language "c" |] in
  let revealOutputChannelOn = RevealOutputChannelOn.Never in
  ClientOptions.create ~documentSelector ~outputChannel:instance.output_channel
    ~revealOutputChannelOn ()

let stop (instance : instance) =
  match !(instance.server) with
  | None -> Promise.return ()
  | Some client ->
      instance.server := None;
      if LanguageClient.isRunning client then LanguageClient.stop client
      else Promise.return ()

(* Restart anguage server *)
let start instance =
  let* () = stop instance in
  let clientOptions = client_options instance in
  let serverOptions = server_options () in
  let client =
    LanguageClient.make ~id:"bfa-c" ~name:"Bfa for C" ~serverOptions
      ~clientOptions ()
  in
  LanguageClient.start client

let disposable instance =
  Disposable.make ~dispose:(fun () ->
      let (_ : unit Promise.t) = stop instance in
      ())
