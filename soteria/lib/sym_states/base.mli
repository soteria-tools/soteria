(** {1 Base State Signature}

    This module defines the base signature that all symbolic state models
    must implement to work with bi-abduction and the state monad.

    {2 Overview}

    A state model represents some piece of program state (e.g., memory,
    ownership, permissions) that can be:
    - Serialized to predicates for specification
    - Produced (added to the state)
    - Consumed (removed from the state, possibly via bi-abduction)

    {2 Usage}

    State models are typically created using functors that build on this
    base signature, adding domain-specific operations like load/store.
*)

open Symex

(** Functor to create the base state signature for a given symbolic
    execution monad. *)
module M (Symex : Symex.Base) : sig
  (** The signature that all state models must implement. *)
  module type S = sig
    (** The state type. *)
    type t [@@deriving show]

    (** The state monad for this state type. *)
    module SM :
      State_monad.S
        with type 'a Symex.t = 'a Symex.t
         and type st = t option
         and type 'a Symex.Value.t = 'a Symex.Value.t
         and type 'a Symex.Value.ty = 'a Symex.Value.ty
         and type Symex.Value.sbool = Symex.Value.sbool

    (** The serialized predicate form of this state. *)
    type serialized [@@deriving show]

    (** [serialize t] converts state [t] to a list of predicates.

        The predicates should fully describe the state such that
        producing them on an empty state reconstructs [t]. *)
    val serialize : t -> serialized list

    (** [subst_serialized f s] substitutes variables in predicate [s]
        using function [f]. *)
    val subst_serialized : (Var.t -> Var.t) -> serialized -> serialized

    (** [iter_vars_serialized s f] iterates over variables in [s]. *)
    val iter_vars_serialized :
      serialized -> (Var.t * 'a Symex.Value.ty -> unit) -> unit

    (** [produce s st] adds predicate [s] to state [st].

        If [s] overlaps with existing state, the computation may vanish
        (in separation logic terms, the states must be disjoint). *)
    val produce : serialized -> t option -> (unit * t option) Symex.t
  end
end
