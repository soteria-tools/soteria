module M (Symex : Symex.Base) = struct
  open Symex

  module type Abstr = sig
    (** Module type describing a symbolic abstraction. That is, some object that
        contains symbolic variables. *)

    type t

    val pp : Format.formatter -> t -> unit
    val fresh : unit -> t Symex.t
  end

  module type Abstr_with_syn = sig
    (** A symbolic abstraction that comes with a syntactic counterpart. *)

    (* TODO: explain the concept of syntax somewhere.*)

    include Abstr

    type syn

    val pp_syn : Format.formatter -> syn -> unit
    val to_syn : t -> syn
    val subst : (Value.Expr.t -> 'a Value.t) -> syn -> t
    val learn_eq : syn -> t -> (unit, 'a) Symex.Consumer.t
    val exprs_syn : syn -> Symex.Value.Expr.t list
  end

  module type Sem_eq = sig
    type t
    type sbool

    val sem_eq : t -> t -> sbool
  end
end
