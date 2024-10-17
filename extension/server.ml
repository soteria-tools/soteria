open Vscode
open Vscode_languageclient
open Promise.Syntax

type instance = LanguageClient.t option ref

let empty_instance () = ref None

let server_options () =
  let options = ExecutableOptions.create ~shell:false () in
  let command = "opam" in
  let args = [ "exec"; "--"; "dune"; "exec"; "--"; "bfa-c"; "lsp" ] in
  Executable.create ~command ~args ~options ()

let client_options () =
  let documentSelector = DocumentSelector.[| language "c" |] in
  let outputChannel = Window.createOutputChannel ~name:"Bfa-C LSP" in
  let revealOutputChannelOn = RevealOutputChannelOn.Never in
  ClientOptions.create ~documentSelector ~outputChannel ~revealOutputChannelOn
    ()

let stop (instance : instance) =
  match !instance with
  | None -> Promise.return ()
  | Some client ->
      instance := None;
      if LanguageClient.isRunning client then LanguageClient.stop client
      else Promise.return ()

(* Restart anguage server *)
let start instance =
  let* () = stop instance in
  let clientOptions = client_options () in
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
