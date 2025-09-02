module type S = sig
  type t

  val init : unit -> t
  val add_constraint : t -> Svalue.t -> unit
  val check_sat : t -> Symex.Solver_result.t
  val declare_var : t -> Symex.Var.t -> Svalue.ty -> unit
  val push : t -> int -> unit
  val pop : t -> int -> unit
end
