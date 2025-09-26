type t = Sat | Unsat | Unknown

let is_sat = function Sat -> true | Unsat | Unknown -> false
let is_unsat = function Unsat -> true | Sat | Unknown -> false

let admissible ~(mode : Approx.t) res =
  match (res, mode) with
  (* A Sat branch is always taken. If it's unknown and in OX mode,
     we must take it to avoid dropping potentially valid branches *)
  | Sat, _ | Unknown, OX -> true
  (* An Unsat branch is always dropped. If it's unknown and in UX mode,
     we must drop it to avoid taking potentially invalid branches *)
  | Unsat, _ | Unknown, UX -> false
