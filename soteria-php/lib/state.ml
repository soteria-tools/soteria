module String_map = Map.Make (String)
module Cell_map = Map.Make (Int)

type cell_id = int
type scope = cell_id String_map.t

type t = {
  scopes : scope list;
  cells : Value.t Cell_map.t;
  next_cell : cell_id;
  output_rev : string list;
}

let empty =
  {
    scopes = [ String_map.empty ];
    cells = Cell_map.empty;
    next_cell = 0;
    output_rev = [];
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

let set_variable name value state =
  let cell, state = ensure_variable name state in
  set_cell cell value state

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
