open Symex

module type S = sig
  (** Describes something symbolic, i.e. that contains symbolic variables. *)

  type t

  module Symex : Symex.S

  val subst : (Var.t -> Var.t) -> t -> t
  val iter_vars : t -> 'a Symex.Value.ty Var.iter_vars
end

module Of_concrete
    (Symex : Symex.S)
    (C : sig
      type t
    end) =
struct
  module Symex = Symex
  include C

  let subst _ x = x
  let iter_vars _ = fun _ -> ()
end
