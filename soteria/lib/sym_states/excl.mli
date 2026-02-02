(** {1 Exclusive Ownership State Model}

    This module implements a state model for exclusive (unique) ownership
    of a single value. It represents resources where exactly one owner
    exists at a time.

    {2 Overview}

    The exclusive model wraps an element type [E] and provides:
    - [load]: Read the value (creates fresh if missing via bi-abduction)
    - [store]: Write a new value (requires existing ownership)
    - [consume]/[produce]: Predicate-based state manipulation

    This is similar to the "agreement" algebra in separation logic,
    but with the constraint that a value must exist (no allocation).

    {2 Usage Example}

    {[
      module MyElem = struct
        type t = int
        let fresh () = Symex.fresh_var "x" TInt
        let sem_eq a b = Symex.Value.(a ==@ b)
        (* ... *)
      end

      module MyExcl = Excl.Make(Symex)(MyElem)

      (* Load creates value if missing, store requires ownership *)
      let* value = MyExcl.load () in
      let* () = MyExcl.store new_value in
    ]}
*)

open Symex

(** Input signature for elements that can be exclusively owned. *)
module Elem (Symex : Symex.Base) : sig
  module type S = sig
    (** The element type. *)
    type t [@@deriving show]

    (** [fresh ()] creates a fresh symbolic value. *)
    val fresh : unit -> t Symex.t

    (** [sem_eq a b] returns a symbolic boolean for equality. *)
    val sem_eq : t -> t -> Symex.Value.sbool Symex.Value.t

    (** [subst f t] substitutes variables using [f]. *)
    val subst : (Var.t -> Var.t) -> t -> t

    (** [iter_vars t f] iterates over variables in [t]. *)
    val iter_vars : t -> 'a Symex.Value.ty Var.iter_vars
  end
end

(** [Make(Symex)(E)] creates an exclusive ownership model for elements [E]. *)
module Make (Symex : Symex.Base) (E : Elem(Symex).S) : sig
  (** The state type (same as element type). *)
  type t = E.t [@@deriving show]

  (** Serialized form (same as state). *)
  type serialized = E.t [@@deriving show]

  (** State monad for exclusive state. *)
  module SM :
    State_monad.S with type Symex.t = Symex.t and type state = t option

  (** [assert_exclusively_owned st] verifies exclusive ownership.

      Returns [Ok ()] if owned, [Missing] with fix if not. *)
  val assert_exclusively_owned :
    t option -> (unit, 'err, serialized list) Symex.Result.t

  (** [load ()] reads the exclusively owned value.

      If no value exists, proposes a fresh value as a fix (bi-abduction). *)
  val load : unit -> (t, 'err, serialized list) SM.Result.t

  (** [store x] stores value [x], replacing any existing value.

      Requires existing ownership (fails if not owned). *)
  val store : t -> (unit, 'err, serialized list) SM.Result.t

  (** [serialize t] returns the value as a singleton predicate list. *)
  val serialize : t -> serialized list

  (** [iter_vars_serialized s f] iterates over variables. *)
  val iter_vars_serialized : serialized -> 'b Symex.Value.ty Var.iter_vars

  (** [subst_serialized f s] substitutes variables. *)
  val subst_serialized : (Var.t -> Var.t) -> serialized -> serialized

  (** [consume x] consumes predicate [x] from the state.

      Checks semantic equality with current value. *)
  val consume : t -> (unit, [> Symex.lfail ], serialized list) SM.Result.t

  (** [produce v] produces predicate [v] into the state.

      Vanishes if state already contains a value. *)
  val produce : serialized -> unit SM.t
end
