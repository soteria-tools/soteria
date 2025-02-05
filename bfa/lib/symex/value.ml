module type S = sig
  type +'a t
  type +'a ty
  type sbool

  val not : sbool t -> sbool t
  val sem_eq : 'a t -> 'b t -> sbool t
  val ppa : 'a t Fmt.t
  val iter_vars : 'a t -> 'b ty Var.iter_vars
  val subst : (Var.t -> Var.t) -> 'a t -> 'a t
  val mk_var : Var.t -> 'a ty -> 'a t
  val as_bool : 'a t -> bool option
end
