(** Symbolic abstraction over integers. *)

open Soteria_std

module S (Symex : Symex.Base) = struct
  module Abstr = Abstr.M (Symex)

  (** Symbolic integers, with basic operations: addition, subtraction, and
      comparison. *)
  module type S = sig
    type t [@@mixins Abstr.Sem_eq + Sigs.Printable]

    (** Takes an integer and creates an abstraction over it *)
    val of_z : Z.t -> t

    (** Takes a symbolic integer and returns [Some z] if this abstraction
        describes exactly a single integer, and [None] otherwise *)
    val to_z : t -> Z.t option

    val zero : unit -> t
    val one : unit -> t

    (** {3 Arithmetic operations} *)

    val add : t -> t -> t
    val sub : t -> t -> t
    val lt : t -> t -> Symex.Value.(sbool t)
    val leq : t -> t -> Symex.Value.(sbool t)
  end

  (** Symbolic integers that are {i bounded}: there is some invariant about them
      being in a range which must be upheld. This can for instance be used to
      represent machine integers (which have a min and max value) with numerals.
  *)
  module type Bounded_S = sig
    include S

    (** Checks whether the given symbolic integer is in the range described by
        the invariant. *)
    val is_in_bound : t -> Symex.Value.(sbool t)
  end
end

module Make_syntax (Symex : Symex.Base) (S_int : S(Symex).S) = struct
  let ( +@ ) = S_int.add
  let ( -@ ) = S_int.sub
  let ( <@ ) = S_int.lt
  let ( <=@ ) = S_int.leq
  let ( ==@ ) = S_int.sem_eq

  module Sym_int_syntax = struct
    let mk_nonzero x = S_int.of_z (Z.of_int x)
    let zero = S_int.zero
    let one = S_int.one
  end
end
