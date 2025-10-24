open Soteria_std

module type Mutable_incremental = sig
  (** This module represents a solver state, it is fully imperative! *)

  include Reversible.Mutable
  module Value : Value.S

  type sbool_v := Value.sbool Value.t

  (** Adds constraints to the solver state. The [simplified] flag indicates if
      {!simplify} was already applied to the constraints, and is [false] by
      default. When it is [false], the solver may simplify constraints before
      adding them to the state, depending on its implementation. *)
  val add_constraints : t -> ?simplified:bool -> sbool_v list -> unit

  val check_sat : t -> Solver_result.t
  val simplify : t -> 'a Value.t -> 'a Value.t
  val fresh_var : t -> 'a Value.ty -> Var_id.t
  val as_values : t -> sbool_v list
end

module type In_place_incremental = sig
  include Reversible.In_place
  module Value : Value.S

  (** simplified indicates if constraits were already simplified *)
  val add_constraints : ?simplified:bool -> Value.sbool Value.t list -> unit

  val check_sat : unit -> bool

  (** Like [sat] but may return true for now even though the constraint isn't
      actually sat. Therefore batching the sat checks *)

  val simplify : 'a Value.t -> 'a Value.t
  val fresh_var : 'a Value.ty -> Var_id.t
  val as_values : unit -> 'a Value.t list
end

module Mutable_to_in_place (M : Mutable_incremental) = struct
  include Reversible.Mutable_to_in_place (M)
  module Value = M.Value

  let add_constraints ?simplified vs =
    M.add_constraints (Lazy.force state) ?simplified vs

  let check_sat = wrap M.check_sat
  let simplify x = (wrap M.simplify ()) x
  let fresh_var x = (wrap M.fresh_var ()) x
  let as_values = wrap M.as_values
end
