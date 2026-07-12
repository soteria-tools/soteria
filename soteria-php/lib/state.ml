module String_map = Map.Make (String)
module Cell_map = Map.Make (Int)
module Object_map = Map.Make (Int)

type object_property =
  | Declared_property of { declaring_class : string; source_name : string }
  | Dynamic_property of string

module Property_map = Map.Make (struct
  type t = object_property

  let compare = Stdlib.compare
end)

type cell_id = int
type scope = cell_id String_map.t
type object_id = int

type php_object = {
  class_name : string;
  properties : cell_id Property_map.t;
  message : string;
}

type t = {
  scopes : scope list;
  class_contexts : string option list;
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
    class_contexts = [ None ];
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

let current_class_context = function
  | { class_contexts = context :: _; _ } -> context
  | { class_contexts = []; _ } -> failwith "PHP state has no class context"

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
  let object_properties, state =
    List.fold_left
      (fun (object_properties, state) (property, value) ->
        let cell, state = allocate_cell value state in
        (Property_map.add property cell object_properties, state))
      (Property_map.empty, state)
      properties
  in
  let object_ = { class_name; properties = object_properties; message } in
  ( id,
    {
      state with
      objects = Object_map.add id object_ state.objects;
      next_object = id + 1;
    } )

let find_object id state = Object_map.find_opt id state.objects

let declared_property ~declaring_class source_name =
  Declared_property { declaring_class; source_name }

let dynamic_property source_name = Dynamic_property source_name

let find_object_property_cell id property state =
  match find_object id state with
  | Some object_ -> Property_map.find_opt property object_.properties
  | None -> failwith "property access on an unknown PHP object"

let find_object_property id property state =
  Option.bind (find_object_property_cell id property state) (fun cell ->
      find_cell cell state)

let set_object_property id property value state =
  match find_object id state with
  | None -> failwith "property write on an unknown PHP object"
  | Some object_ -> (
      match Property_map.find_opt property object_.properties with
      | Some cell -> set_cell cell value state
      | None ->
          let cell, state = allocate_cell value state in
          let object_ =
            {
              object_ with
              properties = Property_map.add property cell object_.properties;
            }
          in
          { state with objects = Object_map.add id object_ state.objects })

let bind_object_property id property cell state =
  if not (Cell_map.mem cell state.cells) then
    failwith "bind object property to an unknown PHP cell";
  match find_object id state with
  | None -> failwith "property binding on an unknown PHP object"
  | Some object_ ->
      let object_ =
        {
          object_ with
          properties = Property_map.add property cell object_.properties;
        }
      in
      { state with objects = Object_map.add id object_ state.objects }

let unset_object_property id property state =
  match find_object id state with
  | None -> failwith "property unset on an unknown PHP object"
  | Some object_ ->
      let object_ =
        {
          object_ with
          properties = Property_map.remove property object_.properties;
        }
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

let enter_scope ?(class_context = None) bindings state =
  let scope, state =
    List.fold_left
      (fun (scope, state) (name, value) ->
        let cell, state = allocate_cell value state in
        (String_map.add name cell scope, state))
      (String_map.empty, state) bindings
  in
  {
    state with
    scopes = scope :: state.scopes;
    class_contexts = class_context :: state.class_contexts;
  }

let leave_scope state =
  match (state.scopes, state.class_contexts) with
  | _ :: (_ :: _ as scopes), _ :: (_ :: _ as class_contexts) ->
      { state with scopes; class_contexts }
  | [ _ ], [ _ ] -> failwith "cannot leave the global PHP scope"
  | _ -> failwith "PHP scope and class-context stacks are inconsistent"

let emit output state = { state with output_rev = output :: state.output_rev }
let output state = state.output_rev |> List.rev |> String.concat ""

let emit_runtime_event event state =
  { state with runtime_events_rev = event :: state.runtime_events_rev }

let runtime_events state = List.rev state.runtime_events_rev
