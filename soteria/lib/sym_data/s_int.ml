(** Symbolic abstractions over integers. *)

module type S = sig
  include S_elt.S
  include S_eq.S with module Symex := Symex and type t := t

  (** Takes an integer and creates an abstraction over it *)
  val of_z : Z.t -> t

  (** Takes a symbolic integer and returns [Some z] if this abstraction
      describes exactly a single integer, and [None] otherwise *)
  val to_z : t -> Z.t option

  (** {3 Arithmetic operations} *)

  type sbool_v := Symex.Value.S_bool.t Symex.Value.t

  val ( +@ ) : t -> t -> t
  val ( -@ ) : t -> t -> t
  val ( <@ ) : t -> t -> sbool_v
  val ( <=@ ) : t -> t -> sbool_v
end
