module type S = sig
  type +'a t
  type +'a ty

  module S_bool : sig
    type +'a v := 'a t
    type t

    val not : t v -> t v
    val and_ : t v -> t v -> t v
    val or_ : t v -> t v -> t v
    val to_bool : t v -> bool option
    val of_bool : bool -> t v
  end

  val sem_eq : 'a t -> 'a t -> S_bool.t t
  val ppa : Format.formatter -> 'a t -> unit
  val iter_vars : 'a t -> 'b ty Var.iter_vars
  val subst : (Var.t -> Var.t) -> 'a t -> 'a t
  val mk_var : Var.t -> 'a ty -> 'a t
end
