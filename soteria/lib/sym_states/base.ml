open Symex

module M (Symex : Symex.Base) = struct
  module type S = sig
    type t [@@deriving show]

    module SM :
      State_monad.S
        with type 'a Symex.t = 'a Symex.t
         and type st = t option
         and type 'a Symex.Value.t = 'a Symex.Value.t
         and type 'a Symex.Value.ty = 'a Symex.Value.ty
         and type Symex.Value.sbool = Symex.Value.sbool

    type serialized [@@deriving show]

    val serialize : t -> serialized list
    val subst_serialized : (Var.t -> Var.t) -> serialized -> serialized

    val iter_vars_serialized :
      serialized -> (Var.t * 'a Symex.Value.ty -> unit) -> unit

    val produce : serialized -> t option -> (unit * t option) Symex.t
  end
end
