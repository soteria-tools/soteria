module type S = sig
  module Var : sig
    type t

    val pp : t Fmt.t
  end

  type t
  type ty

  val not : t -> t
  val sem_eq : t -> t -> t
  val pp : t Fmt.t
  val iter_vars : t -> (Var.t * ty -> unit) -> unit
  val subst : (Var.t -> Var.t) -> t -> t
end
