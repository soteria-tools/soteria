module type S = sig
  type +'a t
  type +'a ty
  type sbool

  val not : sbool t -> sbool t
  val ppa : Format.formatter -> 'a t -> unit
  val iter_vars : 'a t -> 'b ty Var_id.iter_vars
  val subst : (Var_id.t -> Var_id.t) -> 'a t -> 'a t
  val mk_var : Var_id.t -> 'a ty -> 'a t
  val as_bool : 'a t -> bool option
  val bool : bool -> sbool t
end
