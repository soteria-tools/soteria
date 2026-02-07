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

    type syn [@@deriving show]

    val to_syn : t -> syn list
    val ins_outs : syn -> Symex.Value.Expr.(t list * t list)
    val produce : syn -> t option -> t option Symex.Producer.t
    (* val consume : syn -> t option -> (t option, syn list) Symex.Consumer.t *)
  end
end
