open Soteria_std

module type Mutable_incremental = sig
  (** This module represents a solver state, it is fully imperative! *)

  include Reversible.Mutable
  module Value : Value.S

  type sbool_v := Value.(sbool t)

  (** Adds constraints to the solver state. The [simplified] flag indicates if
      {!simplify} was already applied to the constraints, and is [false] by
      default. When it is [false], the solver may simplify constraints before
      adding them to the state, depending on its implementation.

      The [hidden] flag (default [false]) marks the added constraints as not
      meant to be shown to the end user. They still participate in the semantics
      of execution (and are returned by [as_values] by default), but callers of
      [as_values] may pass [~include_hidden:false] to filter them out. See
      {{:https://github.com/soteria-tools/soteria/issues/289}#289}. *)
  val add_constraints :
    t -> ?simplified:bool -> ?hidden:bool -> sbool_v list -> unit

  (** Returns the satisfiability of the current state. *)
  val sat : t -> Solver_result.t

  (** Attempts to simplify the given value according to the current solver
      state. *)
  val simplify : t -> 'a Value.t -> 'a Value.t

  (** Creates a fresh variable of the given type. *)
  val fresh_var : t -> 'a Value.ty -> Var.t

  (** Converts the current solver state into the list of constraints it
      contains. If [~include_hidden:false] is passed, constraints that were
      added with [~hidden:true] are filtered out. Defaults to [true] (i.e.
      returns everything, for backwards compatibility). *)
  val as_values :
    t -> ?include_hidden:bool -> ?simplified:bool -> unit -> sbool_v list
end

(** Converts a mutable incremental solver into an effectful one. See the
    documentation of {!Soteria_std.Reversible.Mutable_to_effectful} for more
    details. *)
module Mutable_to_effectful (M : Mutable_incremental) = struct
  include Reversible.Mutable_to_effectful (M)
  module Value = M.Value

  let add_constraints ?simplified ?hidden vs =
    wrap (fun st -> M.add_constraints st ?simplified ?hidden vs) ()

  let sat () = wrap M.sat ()
  let simplify x = wrap (fun st -> M.simplify st x) ()
  let fresh_var x = wrap (fun st -> M.fresh_var st x) ()

  let as_values ?include_hidden ?simplified () =
    wrap (fun st -> M.as_values st ?include_hidden ?simplified ()) ()
end

(** Converts a mutable incremental solver into a pooled one. See the
    documentation of {!Soteria_std.Reversible.Mutable_to_pooled} for more
    details. *)
module Mutable_to_pooled (M : Mutable_incremental) = struct
  include Reversible.Mutable_to_pooled (M)
  module Value = M.Value

  let add_constraints ?simplified ?hidden vs =
    wrap (fun st -> M.add_constraints st ?simplified ?hidden vs) ()

  let sat () = wrap M.sat ()
  let simplify x = wrap (fun st -> M.simplify st x) ()
  let fresh_var x = wrap (fun st -> M.fresh_var st x) ()

  let as_values ?include_hidden ?simplified () =
    wrap (fun st -> M.as_values st ?include_hidden ?simplified ()) ()
end
