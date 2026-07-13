module String_map = Map.Make (String)
module Cell_map = Map.Make (Int)
module Object_map = Map.Make (Int)
module Closure_map = Map.Make (Int)

module Static_property_map = Map.Make (struct
  type t = string * string

  let compare = Stdlib.compare
end)

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

type closure_capture = By_value of Value.t | By_reference of cell_id

type php_closure = {
  declaration : Php_ir.closure_decl;
  captures : closure_capture String_map.t;
  class_context : string option;
  called_class : string option;
}

type t = {
  scopes : scope list;
  class_contexts : string option list;
  called_classes : string option list;
  cells : Value.t Cell_map.t;
  next_cell : cell_id;
  objects : php_object Object_map.t;
  next_object : object_id;
  static_properties : cell_id Static_property_map.t;
  closures : php_closure Closure_map.t;
  next_closure : int;
  output_rev : string list;
  runtime_events_rev : Error.Runtime_event.t list;
}

let empty =
  {
    scopes = [ String_map.empty ];
    class_contexts = [ None ];
    called_classes = [ None ];
    cells = Cell_map.empty;
    next_cell = 0;
    objects = Object_map.empty;
    next_object = 0;
    static_properties = Static_property_map.empty;
    closures = Closure_map.empty;
    next_closure = 0;
    output_rev = [];
    runtime_events_rev = [];
  }

let current_scope = function
  | { scopes = scope :: _; _ } -> scope
  | { scopes = []; _ } -> failwith "PHP state has no active scope"

let current_class_context = function
  | { class_contexts = context :: _; _ } -> context
  | { class_contexts = []; _ } -> failwith "PHP state has no class context"

let current_called_class = function
  | { called_classes = called_class :: _; _ } -> called_class
  | { called_classes = []; _ } -> failwith "PHP state has no called class"

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

let static_property_key ~declaring_class name =
  (String.lowercase_ascii declaring_class, name)

let allocate_static_property ~declaring_class name value state =
  let key = static_property_key ~declaring_class name in
  if Static_property_map.mem key state.static_properties then
    failwith "duplicate PHP static property cell";
  let cell, state = allocate_cell value state in
  {
    state with
    static_properties = Static_property_map.add key cell state.static_properties;
  }

let find_static_property_cell ~declaring_class name state =
  Static_property_map.find_opt
    (static_property_key ~declaring_class name)
    state.static_properties

let find_static_property ~declaring_class name state =
  Option.bind (find_static_property_cell ~declaring_class name state)
    (fun cell -> find_cell cell state)

let set_static_property ~declaring_class name value state =
  match find_static_property_cell ~declaring_class name state with
  | Some cell -> set_cell cell value state
  | None -> failwith "write to an unknown PHP static property"

let bind_static_property ~declaring_class name cell state =
  if not (Cell_map.mem cell state.cells) then
    failwith "bind static property to an unknown PHP cell";
  let key = static_property_key ~declaring_class name in
  if not (Static_property_map.mem key state.static_properties) then
    failwith "bind unknown PHP static property";
  {
    state with
    static_properties = Static_property_map.add key cell state.static_properties;
  }

let allocate_closure declaration captures class_context called_class state =
  let id = state.next_closure in
  let closure = { declaration; captures; class_context; called_class } in
  ( id,
    {
      state with
      closures = Closure_map.add id closure state.closures;
      next_closure = id + 1;
    } )

let fresh_callable_id state =
  let id = state.next_closure in
  (id, { state with next_closure = id + 1 })

let find_closure id state = Closure_map.find_opt id state.closures
let find_object id state = Object_map.find_opt id state.objects

let set_object_message id message state =
  match find_object id state with
  | None -> failwith "message write on an unknown PHP object"
  | Some object_ ->
      let object_ = { object_ with message } in
      { state with objects = Object_map.add id object_ state.objects }

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

let enter_scope ?(class_context = None) ?(called_class = class_context) bindings
    state =
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
    called_classes = called_class :: state.called_classes;
  }

let enter_closure_scope ?(class_context = None) ?(called_class = class_context)
    captures bindings state =
  let captured, state =
    String_map.fold
      (fun name capture (scope, state) ->
        match capture with
        | By_reference cell -> (String_map.add name cell scope, state)
        | By_value value ->
            let cell, state = allocate_cell value state in
            (String_map.add name cell scope, state))
      captures (String_map.empty, state)
  in
  let state =
    {
      state with
      scopes = captured :: state.scopes;
      class_contexts = class_context :: state.class_contexts;
      called_classes = called_class :: state.called_classes;
    }
  in
  List.fold_left
    (fun state (name, value) -> set_variable name value state)
    state bindings

let leave_scope state =
  match (state.scopes, state.class_contexts, state.called_classes) with
  | ( _ :: (_ :: _ as scopes),
      _ :: (_ :: _ as class_contexts),
      _ :: (_ :: _ as called_classes) ) ->
      { state with scopes; class_contexts; called_classes }
  | [ _ ], [ _ ], [ _ ] -> failwith "cannot leave the global PHP scope"
  | _ -> failwith "PHP scope and class-context stacks are inconsistent"

let emit output state = { state with output_rev = output :: state.output_rev }
let output state = state.output_rev |> List.rev |> String.concat ""

let emit_runtime_event event state =
  { state with runtime_events_rev = event :: state.runtime_events_rev }

let runtime_events state = List.rev state.runtime_events_rev
