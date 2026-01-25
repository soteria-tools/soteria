open Symex

module M (Symex : Symex.Base) = struct
  module type S = sig
    type t [@@deriving show]
    type serialized [@@deriving show]

    val serialize : t -> serialized list
    val subst_serialized : (Var.t -> Var.t) -> serialized -> serialized

    val iter_vars_serialized :
      serialized -> (Var.t * 'a Symex.Value.ty -> unit) -> unit

    val produce : serialized -> t option -> (unit * t option) Symex.t
  end
end
