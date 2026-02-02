(** {1 Pure Function State Model}

    This module models pure (side-effect-free) functions as state components.
    A piece of state represents a function [f()] that always returns the same
    value from some domain.

    {2 Overview}

    Unlike the agreement algebra where allocation is explicit, pure functions:
    - Always have a value (possibly unknown initially)
    - Return the same value on every read
    - Can be constrained via consumption (equality checks)

    When the value is unknown, reading instantiates it to a fresh symbolic
    value. Subsequent reads return the same value.

    {2 Usage Example}

    {[
      module PureInt = Pure_fun.Make(Symex)(struct
        type t = int_value
        let fresh () = Symex.fresh_var "v" TInt
        let sem_eq a b = a ==@ b
        (* ... *)
      end)

      (* First load creates fresh value if unknown *)
      let* v1 = PureInt.load () in
      (* Second load returns same value *)
      let* v2 = PureInt.load () in
      (* v1 and v2 are semantically equal *)
    ]}
*)

open Symex

(** Input signature for the codomain (return type) of pure functions. *)
module Codom (Symex : Symex.Base) : sig
  module type S = sig
    (** The value type. *)
    type t

    val pp : Format.formatter -> t -> unit

    (** [fresh ()] creates a fresh symbolic value. *)
    val fresh : unit -> t Symex.t

    (** [sem_eq a b] returns symbolic equality. *)
    val sem_eq : t -> t -> Symex.Value.sbool Symex.Value.t

    (** [subst f t] substitutes variables using [f]. *)
    val subst : (Var.t -> Var.t) -> t -> t

    (** [iter_vars t f] iterates over variables in [t]. *)
    val iter_vars : t -> 'a Symex.Value.ty Var.iter_vars
  end
end

(** [Make(Symex)(C)] creates a pure function model with codomain [C]. *)
module Make (Symex : Symex.Base) (C : Codom(Symex).S) : sig
  (** The state type (same as codomain). *)
  type t = C.t [@@deriving show]

  (** Serialized form. *)
  type serialized = t [@@deriving show]

  (** State monad for pure function state. *)
  module SM :
    State_monad.S with type Symex.t = Symex.t and type state = C.t option

  (** [serialize s] returns the value as a singleton list. *)
  val serialize : t -> serialized list

  (** [subst_serialized f s] substitutes variables. *)
  val subst_serialized : (Var.t -> Var.t) -> serialized -> serialized

  (** [iter_vars_serialized s f] iterates over variables. *)
  val iter_vars_serialized : serialized -> 'a Symex.Value.ty Var.iter_vars

  val pp : Format.formatter -> t -> unit
  val pp_serialized : Format.formatter -> serialized -> unit

  (** [load ()] reads the pure function's value.

      If unknown, creates a fresh symbolic value and stores it.
      Subsequent loads return the same value. *)
  val load : unit -> (t, 'err, serialized list) SM.Result.t

  (** [produce s] produces a value into the state.

      If state already has a value, assumes equality. Otherwise, sets it. *)
  val produce : serialized -> unit SM.t

  (** [consume s] consumes by checking equality with current value. *)
  val consume : serialized -> (unit, 'err, serialized list) SM.Result.t
end
