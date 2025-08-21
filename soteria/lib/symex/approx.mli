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

module As_ctx : sig
  val with_mode : t -> (unit -> 'a) -> 'a
  val is_ux : unit -> bool
  val is_ox : unit -> bool
end
