module String_map = Map.Make (String)

type t = { variables : Value.t String_map.t; output_rev : string list }

let empty = { variables = String_map.empty; output_rev = [] }
let find_variable name state = String_map.find_opt name state.variables

let set_variable name value state =
  { state with variables = String_map.add name value state.variables }

let emit output state = { state with output_rev = output :: state.output_rev }
let output state = state.output_rev |> List.rev |> String.concat ""
