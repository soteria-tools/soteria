open Soteria_std

module type Mutable_incremental = sig
  (** This module represents a solver state, it is fully imperative! *)

  include Reversible.Mutable
  module Value : Value.S

  type sbool_v := Value.(sbool t)

  (** Adds constraints to the solver state. The [simplified] flag indicates if
      {!simplify} was already applied to the constraints, and is [false] by
      default. When it is [false], the solver may simplify constraints before
      adding them to the state, depending on its implementation. *)
  val add_constraints : t -> ?simplified:bool -> sbool_v list -> unit

  (** Returns the satisfiability of the current state. *)
  val sat : t -> Solver_result.t

  (** Attempts to simplify the given value according to the current solver
      state. *)
  val simplify : t -> 'a Value.t -> 'a Value.t

  (** Creates a fresh variable of the given type. *)
  val fresh_var : t -> 'a Value.ty -> Var.t

  (** Converts the current solver state into the list of constraints it
      contains. *)
  val as_values : t -> sbool_v list
end

(** Converts a mutable incremental solver into an effectful one. See the
    documentation of {!Soteria_std.Reversible.Mutable_to_effectful} for more
    details. *)
module Mutable_to_effectful (M : Mutable_incremental) = struct
  include Reversible.Mutable_to_effectful (M)
  module Value = M.Value

  let add_constraints ?simplified vs =
    wrap (fun st -> M.add_constraints st ?simplified vs) ()

  let sat () = wrap M.sat ()
  let simplify x = wrap (fun st -> M.simplify st x) ()
  let fresh_var x = wrap (fun st -> M.fresh_var st x) ()
  let as_values () = wrap M.as_values ()
end

(** Converts a mutable incremental solver into a pooled one. See the
    documentation of {!Soteria_std.Reversible.Mutable_to_pooled} for more
    details. *)
module Mutable_to_pooled (M : Mutable_incremental) = struct
  include Reversible.Mutable_to_pooled (M)
  module Value = M.Value

  let add_constraints ?simplified vs =
    wrap (fun st -> M.add_constraints st ?simplified vs) ()

  let sat () = wrap M.sat ()
  let simplify x = wrap (fun st -> M.simplify st x) ()
  let fresh_var x = wrap (fun st -> M.fresh_var st x) ()
  let as_values () = wrap M.as_values ()
end
