module String_map = Map.Make (String)

type scope = Value.t String_map.t
type t = { scopes : scope list; output_rev : string list }

let empty = { scopes = [ String_map.empty ]; output_rev = [] }

let current_scope = function
  | { scopes = scope :: _; _ } -> scope
  | { scopes = []; _ } -> failwith "PHP state has no active scope"

let find_variable name state = String_map.find_opt name (current_scope state)

let set_variable name value state =
  match state.scopes with
  | scope :: scopes ->
      { state with scopes = String_map.add name value scope :: scopes }
  | [] -> failwith "PHP state has no active scope"

let enter_scope bindings state =
  let scope =
    List.fold_left
      (fun scope (name, value) -> String_map.add name value scope)
      String_map.empty bindings
  in
  { state with scopes = scope :: state.scopes }

let leave_scope state =
  match state.scopes with
  | _ :: (_ :: _ as scopes) -> { state with scopes }
  | [ _ ] -> failwith "cannot leave the global PHP scope"
  | [] -> failwith "PHP state has no active scope"

let emit output state = { state with output_rev = output :: state.output_rev }
let output state = state.output_rev |> List.rev |> String.concat ""
