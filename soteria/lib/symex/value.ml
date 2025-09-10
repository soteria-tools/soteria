module S_bool = struct
  module type S = sig
    type +'a v
    type t

    val not : t v -> t v
    val and_ : t v -> t v -> t v
    val or_ : t v -> t v -> t v
    val to_bool : t v -> bool option
    val of_bool : bool -> t v
  end

  module Make_syntax (S_bool : S) = struct
    let[@inline] ( &&@ ) = S_bool.and_
    let[@inline] ( ||@ ) = S_bool.or_
    let[@inline] not = S_bool.not
  end
end

module type S = sig
  type +'a t
  type +'a ty

  module S_bool : S_bool.S with type +'a v := 'a t

  val sem_eq : 'a t -> 'a t -> S_bool.t t
  val ppa : Format.formatter -> 'a t -> unit
  val iter_vars : 'a t -> 'b ty Var.iter_vars
  val subst : (Var.t -> Var.t) -> 'a t -> 'a t
  val mk_var : Var.t -> 'a ty -> 'a t
end
