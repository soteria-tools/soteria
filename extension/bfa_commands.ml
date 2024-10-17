open Import

type t = { id : string; handler : Server.instance -> args:Ojs.t list -> unit }

let commands = Dynarray.create ()

let _restart_server =
  let handler (instance : Server.instance) ~args:_ =
    let (_ : unit Promise.t) = Server.start instance in
    ()
  in
  let cmd = { id = Constants.Commands.restart_server; handler } in
  Dynarray.add_last commands cmd;
  cmd

let register_command extension instance { id; handler } =
  let callback = handler instance in
  let disposable = Vscode.Commands.registerCommand ~command:id ~callback in
  Server.log_message ~instance `Info "Registered command %s" id;
  ExtensionContext.subscribe extension ~disposable

let register_all_commands extension instance =
  Dynarray.iter (register_command extension instance) commands
