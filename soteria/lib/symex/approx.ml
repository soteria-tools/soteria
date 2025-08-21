(** Approximation mode: {!OX} for "Over-Approximation" and {!UX} for
    "Under-Approximation". Symbolic execution can be done in either mode, with
    {!OX} guaranteeing the absence of false negatives and {!UX} guaranteeing the
    absence of false positives.

    The mode (passed to the {!Symex.S.run} function) determines the behavior of
    certain operations. For instance, in {!UX} mode, sat checks that return
    [unknown] are the corresponding branch is discarded: since analysis cannot
    conclude, we avoid reporting false positives in that case. Conversly, in
    {!OX} mode, a sat check that returns [unknown] is more or less equivalent to
    returning [SAT]; that is, the branch is explored as to not ignored a
    potentially feasible branch. *)
type t = UX | OX

let is_ux = function UX -> true | OX -> false
let is_ox = function OX -> true | UX -> false

module As_ctx = struct
  type _ Effect.t += Apply : (t -> 'a) -> 'a Effect.t

  let with_mode mode f =
    try f ()
    with effect Apply g, k ->
      let result = g mode in
      Effect.Deep.continue k result

  let apply f = Effect.perform (Apply f)
  let is_ux () = apply is_ux
  let is_ox () = apply is_ox
end
