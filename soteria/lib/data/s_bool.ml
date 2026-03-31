(** Symbolic abstraction over booleans. *)

module S (Symex : Symex.Base) = struct
  module type S = sig
    type +'a v := 'a Symex.Value.t
    type t = Symex.Value.sbool

    val not : t v -> t v
    val and_ : t v -> t v -> t v
    val or_ : t v -> t v -> t v
    val to_bool : t v -> bool option
    val of_bool : bool -> t v
  end
end

module Make_syntax (Symex : Symex.Base) (S_bool : S(Symex).S) = struct
  let ( &&@ ) = S_bool.and_
  let ( ||@ ) = S_bool.or_
  let not = S_bool.not
end
