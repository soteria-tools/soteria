type t = UX | OX

let is_ux = function UX -> true | OX -> false
let is_ox = function OX -> true | UX -> false

module As_ctx = struct
  type _ Effect.t += Apply : (t -> 'a) -> 'a Effect.t

  let with_mode ~mode f =
    try f ()
    with effect Apply g, k ->
      let result = g mode in
      Effect.Deep.continue k result

  let apply f = Effect.perform (Apply f)
  let is_ux () = apply is_ux
  let is_ox () = apply is_ox
end
