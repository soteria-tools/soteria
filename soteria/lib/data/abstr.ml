(** Basic mixin signatures for symbolic abstractions *)

module M (Symex : Symex.Base) = struct
  open Symex

  module type S = sig
    (** Module type describing a symbolic abstraction. That is, some object that
        contains symbolic variables. *)

    type t [@@deriving show]

    val fresh : unit -> t Symex.t
  end

  module type S_with_syn = sig
    (** A symbolic abstraction that comes with a syntactic counterpart. *)

    (* TODO: explain the concept of syntax somewhere.*)

    include S

    (** Syntactic representation of the abstraction (that can be serialized).
        Basically, everywhere a symbolic value exists, it must be cast to an
        [Expr.t] *)
    type syn [@@deriving show]

    val to_syn : t -> syn

    (** Given a substitution, casts a syntactic object to a semantic object. *)
    val subst : (Value.Expr.t -> 'a Value.t) -> syn -> t

    (** [learn_eq s t] extends the substitution [θ] of the consumer monad such
        that all variables of [s] are bound and [θ(s) = t] (or fails to do so).
    *)
    val learn_eq : syn -> t -> (unit, 'a) Symex.Consumer.t

    (** Returns the list of expressions contained by the abstraction. *)
    val exprs_syn : syn -> Symex.Value.Expr.t list
  end

  module type Sem_eq = sig
    type t

    val sem_eq : t -> t -> Symex.Value.(sbool t)
  end

  module type Simplifiable = sig
    type t

    val simplify : t -> t Symex.t
  end
end
