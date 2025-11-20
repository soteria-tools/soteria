(** Basic operations of symbolic abstractions *)

open Symex

module type S = sig
  (** Describes something symbolic, i.e. that contains symbolic variables. *)

  type t

  include Soteria_std.Printable.S with type t := t
  module Symex : Symex.Base

  val subst : (Var.t -> Var.t) -> t -> t
  val iter_vars : t -> 'a Symex.Value.ty Var.iter_vars
end

module Of_concrete
    (Symex : Symex.Base)
    (C : sig
      type t
    end) =
struct
  module Symex = Symex
  include C

  let subst _ x = x
  let iter_vars _ = fun _ -> ()
end
