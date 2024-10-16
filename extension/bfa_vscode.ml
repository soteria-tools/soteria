(* open Js_of_ocaml *)
open Vscode
open Vscode_languageclient
open Promise.Syntax

(* type 'a setting = {
     key : string;
     to_json : 'a -> Jsonoo.t;
     of_json : Jsonoo.t -> 'a;
     scope : ConfigurationTarget.t;
   }
   [@@deriving make]

   let get_config ?section setting =
     let section = Workspace.getConfiguration ?section () in
     match WorkspaceConfiguration.get section ~section:setting.key with
     | None -> None
     | Some _v -> None (* match setting.of_json (Jsonoo.t_of_js v) with
        *) *)

(* TODO: This should be extented to look up in the config for either a binary path or switch or install it wherever. *)
let serverOptions =
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

let stop_language_server client =
  if LanguageClient.isRunning client then LanguageClient.stop client
  else Promise.return ()

let start_language_server () =
  let clientOptions = client_options () in
  let client =
    LanguageClient.make ~id:"bfa-c" ~name:"Bfa for C" ~serverOptions
      ~clientOptions ()
  in
  let+ () = LanguageClient.start client in
  Disposable.make ~dispose:(fun () ->
      let (_ : unit Promise.t) = stop_language_server client in
      ())

let activate (extension : Vscode.ExtensionContext.t) =
  let+ server_disposable = start_language_server () in
  ExtensionContext.subscribe ~disposable:server_disposable extension

let () =
  let open Js_of_ocaml.Js in
  export "activate" (wrap_callback activate)
