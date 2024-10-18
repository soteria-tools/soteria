module type S = sig
  (** This module represents a solver state, it is fully imperative! *)

  module Value : Value.S

  val add_constraints : Value.t list -> unit
  val sat : unit -> bool

  (** Like [sat] but may return true for now even though the constraint isn't actually sat.
      Therefore batching the sat checks *)
  val delayed_sat : unit -> bool

  val save : unit -> unit
  val backtrack : unit -> unit
  val simplify : Value.t -> Value.t
  val as_bool : Value.t -> bool option
  val fresh : Value.ty -> Value.t

  (** Returns [true] if current state entails the given constraint, false otherwise. *)
  val check_entailment : Value.t list -> bool
end
