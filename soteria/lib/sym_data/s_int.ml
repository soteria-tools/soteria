(** Symbolic abstractions over integers. *)

module type S = sig
  include S_elt.S
  include S_eq.S with module Symex := Symex and type t := t
  include Soteria_std.Printable.S with type t := t

  (** Takes an integer and creates an abstraction over it *)
  val of_z : Z.t -> t

  (** Takes a symbolic integer and returns [Some z] if this abstraction
      describes exactly a single integer, and [None] otherwise *)
  val to_z : t -> Z.t option

  val zero : unit -> t
  val one : unit -> t

  (** {3 Arithmetic operations} *)

  type sbool_v := Symex.Value.S_bool.t Symex.Value.t

  val add : t -> t -> t
  val minus : t -> t -> t
  val lt : t -> t -> sbool_v
  val leq : t -> t -> sbool_v
end

module Make_syntax (S_int : S) = struct
  include S_eq.Make_syntax (S_int)

  let ( +@ ) = S_int.add
  let ( -@ ) = S_int.minus
  let ( <@ ) = S_int.lt
  let ( <=@ ) = S_int.leq

  module Sym_int_syntax = struct
    let mk_nonzero x = S_int.of_z (Z.of_int x)
    let zero = S_int.zero
    let one = S_int.one
  end
end
