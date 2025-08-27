type t = Sat | Unsat | Unknown

let is_sat = function Sat -> true | Unsat | Unknown -> false
let is_unsat = function Unsat -> true | Sat | Unknown -> false
