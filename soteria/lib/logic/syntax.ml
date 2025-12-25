(** This module describes the syntactic representations of objects, in a logic
    or in the symbolic engine.

    Unlike symbolic values, syntactic representations are AST that don't carry
    any additional type information. This is helpful to implement, for instance,
    substitutions. *)

module Raw_Subst = struct end

module M (Symex : Symex.Base) = struct
  (** A syntactic representation *)

  module Subst (E : Expr with type 'a value = 'a Symex.Value.t) = struct
    include Map.Make (E)

    type nonrec t = E.t t
  end

  module Syn (E : Expr with type 'a value = 'a Symex.Value.t) = struct
    module type S = sig
      type t
      type subst := Subst(E).t

      val learn_eq : subst -> t -> t -> subst Symex.t
    end
  end
end
