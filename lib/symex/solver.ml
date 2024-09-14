module type S = sig
  (** This module represents a solver state, it is fully imperative! *)

  module Value : Value.S

  val add_constraints : Value.t list -> unit
  val sat : unit -> bool

  val delayed_sat : unit -> bool
  (** Like [sat] but may return true for now even though the constraint isn't actually sat.
      Therefore batching the sat checks *)

  val save : unit -> unit
  val backtrack : unit -> unit
  val simplified : Value.t -> bool option

  val check_entailment : Value.t list -> bool
  (** Returns [true] if current state entails the given constraint, false otherwise. *)
end
