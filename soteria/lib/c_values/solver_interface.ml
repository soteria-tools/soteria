module type S = sig
  type t

  val init : unit -> t
  val add_constraint : t -> Svalue.t -> unit
  val check_sat : t -> Soteria_symex.Solver_result.t
  val declare_var : t -> Soteria_symex.Var.t -> Svalue.ty -> unit
  val push : t -> int -> unit
  val pop : t -> int -> unit
end
