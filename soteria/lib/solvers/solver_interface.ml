module type S = sig
  type t
  type value
  type ty

  val init : unit -> t
  val add_constraint : t -> value -> unit
  val check_sat : t -> Soteria_symex.Solver_result.t
  val declare_var : t -> Soteria_symex.Var.t -> ty -> unit
  val push : t -> int -> unit
  val pop : t -> int -> unit
  val reset : t -> unit
end
