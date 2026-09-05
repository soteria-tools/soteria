(** Basic mixin signatures for symbolic abstractions *)

module M (Symex : Symex.Base) = struct
  open Symex

  module type S = sig
    (** Module type describing a symbolic abstraction. That is, some object that
        contains symbolic variables. *)

    type t [@@deriving show]

    (* FIXME: fresh should probably not be required by default anymore. *)
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

  (** Given a value type, creates a module satisfying {!S_with_syn}. This is
      helpful to create a bridge between {!Symex.Value} and abstractions, which
      expect a slightly different interface.

      @see <https://github.com/soteria-tools/soteria/issues/344> *)
  module With_syn_of_value (V : sig
    type ty

    (** Create the runtime type value for this type. Used for
        {!S_with_syn.fresh}, which uses it to call {!Symex.nondet}. *)
    val ty : unit -> ty Symex.Value.ty
  end) :
    S_with_syn
      with type t = V.ty Symex.Value.t
       and type syn = Symex.Value.Expr.t = struct
    type t = V.ty Symex.Value.t
    type syn = Symex.Value.Expr.t [@@deriving show { with_path = false }]

    let fresh () = Symex.nondet (V.ty ())
    let pp x = Symex.Value.ppa x
    let show x = Fmt.to_to_string pp x
    let to_syn = Symex.Value.Expr.of_value
    let learn_eq = Symex.Consumer.learn_eq
    let exprs_syn s : Symex.Value.Expr.t list = [ s ]
    let subst = Symex.Value.Expr.subst
  end
end
