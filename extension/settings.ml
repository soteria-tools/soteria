open Import

type 'a t = {
  key : string;
  to_json : 'a -> Jsonoo.t;
  of_json : Jsonoo.t -> 'a;
  scope : ConfigurationTarget.t;
}
[@@deriving make]

let get ?section setting =
  let section = Workspace.getConfiguration ?section () in
  match WorkspaceConfiguration.get section ~section:setting.key with
  | None -> None
  | Some v -> (
      match setting.of_json (Jsonoo.t_of_js v) with
      | s -> Some s
      | exception Jsonoo.Decode_error msg ->
          show_message `Error "Setting %s is invalid: %s" setting.key msg;
          None)

let set ?section setting v =
  let section = Workspace.getConfiguration ?section () in
  match Workspace.name () with
  | None -> Promise.return ()
  | Some _ ->
      let value = Jsonoo.t_to_js (setting.to_json v) in
      WorkspaceConfiguration.update section ~section:setting.key ~value
        ~configurationTarget:(`ConfigurationTarget setting.scope) ()
