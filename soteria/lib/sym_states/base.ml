open Symex

module M (Symex : Symex.Base) = struct
  module type S = sig
    type t [@@deriving show]

    module SM :
      State_monad.S
        with module Symex = Symex
         and module Value = Symex.Value
         and type st = t option

    type serialized [@@deriving show]

    val serialize : t -> serialized list
    val subst_serialized : (Var.t -> Var.t) -> serialized -> serialized

    val iter_vars_serialized :
      serialized -> (Var.t * 'a Symex.Value.ty -> unit) -> unit

    val produce : serialized -> unit SM.t
  end
end
