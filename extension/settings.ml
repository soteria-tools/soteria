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

module Server_kind = struct
  type t = Auto | Shell of Executable.t

  let key = "bfa.server.kind"

  let of_json json =
    let open Jsonoo.Decode in
    match field "kind" string json with
    | "auto" -> Auto
    | "shell" ->
        let cmd = field "cmd" string json in
        let args = field "args" (list string) json in
        let opt = ExecutableOptions.create ~shell:false () in
        let exe = Executable.create ~command:cmd ~args ~options:opt () in
        Shell exe
    | _ -> raise (Jsonoo.Decode_error "Invalid server kind")

  let to_json = function
    | Auto -> Jsonoo.Encode.(object_ [ ("kind", string "auto") ])
    | Shell exe ->
        Jsonoo.Encode.(
          object_
            [
              ("kind", string "shell");
              ("cmd", string (Executable.command exe));
              ( "args",
                (list string) (Executable.args exe |> Option.value ~default:[])
              );
            ])

  let s = make ~key ~of_json ~to_json ~scope:ConfigurationTarget.Workspace
end
