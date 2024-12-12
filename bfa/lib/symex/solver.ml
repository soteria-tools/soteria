module type Mutable_incremental = sig
  (** This module represents a solver state, it is fully imperative! *)

  include Incremental.Mutable
  module Value : Value.S

  (** simplified indicates if constraits were already simplified *)
  val add_constraints : t -> ?simplified:bool -> Value.t list -> unit

  val sat : t -> bool

  (** Like [sat] but may return true for now even though the constraint isn't actually sat.
      Therefore batching the sat checks *)

  val simplify : t -> Value.t -> Value.t
  val fresh_var : t -> Value.ty -> Var.t
  val as_values : t -> Value.t list
end

module type In_place_incremental = sig
  include Incremental.In_place
  module Value : Value.S

  (** simplified indicates if constraits were already simplified *)
  val add_constraints : ?simplified:bool -> Value.t list -> unit

  val sat : unit -> bool

  (** Like [sat] but may return true for now even though the constraint isn't actually sat.
      Therefore batching the sat checks *)

  val simplify : Value.t -> Value.t
  val fresh_var : Value.ty -> Var.t
  val as_values : unit -> Value.t list
end

module Mutable_to_in_place (M : Mutable_incremental) = struct
  include Incremental.Mutable_to_in_place (M)
  module Value = M.Value

  let add_constraints ?simplified vs =
    M.add_constraints (Lazy.force state) ?simplified vs

  let sat () = M.sat (Lazy.force state)
  let simplify x = M.simplify (Lazy.force state) x
  let fresh_var x = M.fresh_var (Lazy.force state) x
  let as_values () = M.as_values (Lazy.force state)
end
