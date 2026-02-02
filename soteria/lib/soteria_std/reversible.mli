(** {1 Reversible Computation}

    This module provides abstractions for reversible (backtrackable)
    computation, essential for symbolic execution where we explore
    multiple paths and need to backtrack.

    {2 Overview}

    Three main interfaces are provided:

    - {!Mutable}: Direct mutable state with explicit save/backtrack
    - {!Effectful}: Effect-based interface for cleaner code
    - {!Make_effectful}: Creates fresh effect types for isolation

    State is organized as a stack of checkpoints. [save] pushes a
    checkpoint, [backtrack_n] pops checkpoints to restore earlier state.

    {2 Usage Example}

    {[
      module RevState = Reversible.Make_mutable(struct
        type t = int
        let default = 0
      end)

      let state = RevState.init () in
      RevState.wrap (fun s -> (s, s + 1)) state;  (* s = 1 *)
      RevState.save state;                         (* checkpoint *)
      RevState.wrap (fun s -> (s, s + 10)) state; (* s = 11 *)
      RevState.backtrack_n state 1;                (* s = 1 *)
    ]}
*)

(** {2 Mutable Interface} *)

(** Interface for mutable reversible state with explicit operations. *)
module type Mutable = sig
  (** The state type (typically a dynarray). *)
  type t

  (** [init ()] creates a fresh state with the default value. *)
  val init : unit -> t

  (** [backtrack_n t n] removes the last [n] checkpoints.

      @raise Invalid_argument if [n] exceeds checkpoint count. *)
  val backtrack_n : t -> int -> unit

  (** [save t] creates a checkpoint of the current state.

      The state is copied, so modifications don't affect the checkpoint. *)
  val save : t -> unit

  (** [reset t] clears all checkpoints and resets to default. *)
  val reset : t -> unit
end

(** [Make_mutable(M)] creates a mutable reversible state from a type
    with a default value.

    The state is stored as a dynarray where each element is a checkpoint.
    The last element is the "current" state. *)
module Make_mutable (M : sig
  type t
  val default : t
end) : sig
  include Mutable with type t = M.t Dynarray.t

  (** [set_default v] changes the default value for future [init]/[reset]. *)
  val set_default : M.t -> unit

  (** [wrap f t] applies [f] to current state, updating it with the result.

      [f] receives the current value and returns [(result, new_value)]. *)
  val wrap : (M.t -> 'a * M.t) -> t -> 'a

  (** [wrap_read f t] applies [f] to current state without modifying it. *)
  val wrap_read : (M.t -> 'a) -> t -> 'a
end

(** {2 Effectful Interface} *)

(** Interface for effect-based reversible computation.

    Effects are used to access the state, avoiding explicit passing.
    Use [run] to execute computations with effect handling. *)
module type Effectful = sig
  (** The state type (may be mutable). *)
  type t

  (** [backtrack_n n] removes the last [n] checkpoints.

      Performs the [Backtrack_n] effect. *)
  val backtrack_n : int -> unit

  (** [save ()] creates a checkpoint of current state.

      Performs the [Save] effect. *)
  val save : unit -> unit

  (** [wrap f ()] applies [f] to the current state.

      [f] may modify the state (if mutable).
      Performs the [Wrap] effect. *)
  val wrap : (t -> 'a) -> unit -> 'a

  (** [run f] executes [f] with a fresh state and effect handling.

      The state is initialized, effects are handled, and state is
      reset on completion. *)
  val run : (unit -> 'a) -> 'a
end

(** [Mutable_to_pooled(M)] creates an effectful interface backed by
    a resource pool for efficient state reuse.

    When [run] is called, a state is acquired from the pool (or created
    if none available). On completion, the state is returned to the pool.

    This is useful when symbolic execution spawns many parallel explorations
    that each need independent solver states. *)
module Mutable_to_pooled (M : Mutable) : sig
  include Effectful with type t = M.t

  (** [total_resources ()] returns the total number of pooled states. *)
  val total_resources : unit -> int

  (** [available_resources ()] returns currently idle pooled states. *)
  val available_resources : unit -> int
end

(** [Mutable_to_effectful(M)] creates an effectful interface from
    a mutable one.

    Unlike [Mutable_to_pooled], a fresh state is created for each [run].
    Simpler but potentially less efficient. *)
module Mutable_to_effectful (M : Mutable) : Effectful with type t = M.t

(** [Make_effectful(M)] creates an effectful interface with fresh
    effect types.

    Each application of this functor creates independent effects,
    allowing multiple reversible states to coexist. *)
module Make_effectful (M : sig
  type t
end) : sig
  type t = M.t

  (** [backtrack_n n] performs the backtrack effect. *)
  val backtrack_n : int -> unit

  (** [save ()] performs the save effect. *)
  val save : unit -> unit

  (** [wrap f ()] performs update effect with function [f].

      [f] returns [(result, new_state)]. *)
  val wrap : (M.t -> 'a * M.t) -> unit -> 'a

  (** [wrap_read f ()] performs read-only access. *)
  val wrap_read : (M.t -> 'a) -> unit -> 'a

  (** [run ~init f] executes [f] with initial state [init]. *)
  val run : init:M.t -> (unit -> 'a) -> 'a
end
