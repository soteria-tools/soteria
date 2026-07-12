module String_map = Map.Make (String)
module String_set = Set.Make (String)
module Cell_map = Map.Make (Int)
module Object_map = Map.Make (Int)

type cell_id = int
type scope = cell_id String_map.t
type object_id = int

type php_object = {
  class_name : string;
  declared_properties : String_set.t;
  properties : cell_id String_map.t;
  message : string;
}

type t = {
  scopes : scope list;
  cells : Value.t Cell_map.t;
  next_cell : cell_id;
  objects : php_object Object_map.t;
  next_object : object_id;
  output_rev : string list;
  runtime_events_rev : Error.Runtime_event.t list;
}

let empty =
  {
    scopes = [ String_map.empty ];
    cells = Cell_map.empty;
    next_cell = 0;
    objects = Object_map.empty;
    next_object = 0;
    output_rev = [];
    runtime_events_rev = [];
  }

let current_scope = function
  | { scopes = scope :: _; _ } -> scope
  | { scopes = []; _ } -> failwith "PHP state has no active scope"

let find_cell cell state = Cell_map.find_opt cell state.cells

let set_cell cell value state =
  if Cell_map.mem cell state.cells then
    { state with cells = Cell_map.add cell value state.cells }
  else failwith "write to an unknown PHP cell"

let allocate_cell value state =
  let cell = state.next_cell in
  ( cell,
    {
      state with
      cells = Cell_map.add cell value state.cells;
      next_cell = cell + 1;
    } )

let allocate_object ?(properties = []) class_name message state =
  let id = state.next_object in
  let declared_properties, object_properties, state =
    List.fold_left
      (fun (declared, object_properties, state) (name, value) ->
        let cell, state = allocate_cell value state in
        ( String_set.add name declared,
          String_map.add name cell object_properties,
          state ))
      (String_set.empty, String_map.empty, state)
      properties
  in
  let object_ =
    { class_name; declared_properties; properties = object_properties; message }
  in
  ( id,
    {
      state with
      objects = Object_map.add id object_ state.objects;
      next_object = id + 1;
    } )

let find_object id state = Object_map.find_opt id state.objects

let object_declares_property id name state =
  match find_object id state with
  | Some object_ -> String_set.mem name object_.declared_properties
  | None -> failwith "property access on an unknown PHP object"

let find_object_property_cell id name state =
  match find_object id state with
  | Some object_ -> String_map.find_opt name object_.properties
  | None -> failwith "property access on an unknown PHP object"

let find_object_property id name state =
  Option.bind (find_object_property_cell id name state) (fun cell ->
      find_cell cell state)

let set_object_property id name value state =
  match find_object id state with
  | None -> failwith "property write on an unknown PHP object"
  | Some object_ -> (
      match String_map.find_opt name object_.properties with
      | Some cell -> set_cell cell value state
      | None ->
          let cell, state = allocate_cell value state in
          let object_ =
            {
              object_ with
              properties = String_map.add name cell object_.properties;
            }
          in
          { state with objects = Object_map.add id object_ state.objects })

let bind_object_property id name cell state =
  if not (Cell_map.mem cell state.cells) then
    failwith "bind object property to an unknown PHP cell";
  match find_object id state with
  | None -> failwith "property binding on an unknown PHP object"
  | Some object_ ->
      let object_ =
        {
          object_ with
          properties = String_map.add name cell object_.properties;
        }
      in
      { state with objects = Object_map.add id object_ state.objects }

let unset_object_property id name state =
  match find_object id state with
  | None -> failwith "property unset on an unknown PHP object"
  | Some object_ ->
      let object_ =
        { object_ with properties = String_map.remove name object_.properties }
      in
      { state with objects = Object_map.add id object_ state.objects }

let find_variable_cell name state =
  String_map.find_opt name (current_scope state)

let find_variable name state =
  Option.bind (find_variable_cell name state) (fun cell -> find_cell cell state)

let ensure_variable name state =
  match find_variable_cell name state with
  | Some cell -> (cell, state)
  | None -> (
      let cell, state = allocate_cell Value.undef state in
      match state.scopes with
      | scope :: scopes ->
          ( cell,
            { state with scopes = String_map.add name cell scope :: scopes } )
      | [] -> failwith "PHP state has no active scope")

let bind_variable name cell state =
  if not (Cell_map.mem cell state.cells) then
    failwith "bind variable to an unknown PHP cell";
  match state.scopes with
  | scope :: scopes ->
      { state with scopes = String_map.add name cell scope :: scopes }
  | [] -> failwith "PHP state has no active scope"

let unset_variable name state =
  match state.scopes with
  | scope :: scopes ->
      { state with scopes = String_map.remove name scope :: scopes }
  | [] -> failwith "PHP state has no active scope"

let set_variable name value state =
  let cell, state = ensure_variable name state in
  set_cell cell value state

let value_of_array_entry entry state =
  match entry with
  | Value.Inline value -> Some value
  | Value.Reference cell -> find_cell cell state

let find_array_value key array state =
  Option.bind (Value.array_find key array) (fun entry ->
      value_of_array_entry entry state)

let enter_scope bindings state =
  let scope, state =
    List.fold_left
      (fun (scope, state) (name, value) ->
        let cell, state = allocate_cell value state in
        (String_map.add name cell scope, state))
      (String_map.empty, state) bindings
  in
  { state with scopes = scope :: state.scopes }

let leave_scope state =
  match state.scopes with
  | _ :: (_ :: _ as scopes) -> { state with scopes }
  | [ _ ] -> failwith "cannot leave the global PHP scope"
  | [] -> failwith "PHP state has no active scope"

let emit output state = { state with output_rev = output :: state.output_rev }
let output state = state.output_rev |> List.rev |> String.concat ""

let emit_runtime_event event state =
  { state with runtime_events_rev = event :: state.runtime_events_rev }

let runtime_events state = List.rev state.runtime_events_rev
