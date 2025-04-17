open Import

type t = { id : string; handler : Instance.t -> args:Ojs.t list -> unit }

let commands = Dynarray.create ()

let _restart_server =
  let handler (instance : Instance.t) ~args:_ =
    let (_ : unit Promise.t) = Instance.start_server instance in
    ()
  in
  let cmd = { id = Constants.Commands.restart_server; handler } in
  Dynarray.add_last commands cmd;
  cmd

let _toggle_debug_mode =
  let handler (instance : Instance.t) ~args:_ =
    Instance.toggle_debug_mode instance
  in
  let cmd = { id = Constants.Commands.toggle_debug_mode; handler } in
  Dynarray.add_last commands cmd;
  cmd

let register_command extension instance { id; handler } =
  let callback = handler instance in
  let disposable = Vscode.Commands.registerCommand ~command:id ~callback in
  Logging.info "Registered command %s" id;
  ExtensionContext.subscribe extension ~disposable

let register_all_commands extension instance =
  Dynarray.iter (register_command extension instance) commands
