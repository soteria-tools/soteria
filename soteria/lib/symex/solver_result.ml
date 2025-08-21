type t = Sat | Unsat | Unknown

let is_sat (result : t) =
  match result with Sat -> true | Unsat | Unknown -> false

let is_unsat (result : t) =
  match result with Unsat -> true | Sat | Unknown -> false
