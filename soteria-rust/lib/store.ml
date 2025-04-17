open Charon

include Stdlib.Map.Make (struct
  type t = Expressions.LocalId.id

  let compare = Expressions.LocalId.compare_id
end)

let find_type sym store = Option.map snd (find_opt sym store)
let find_value sym store = Option.bind (find_opt sym store) fst
