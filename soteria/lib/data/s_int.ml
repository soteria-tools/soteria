(** Symbolic abstraction over integers. *)

module S (Symex : Symex.Base) (S_bool : S_bool.S(Symex).S) = struct
  (** Symbolic integers, with basic operations: addition, subtraction, and
      comparison. *)
  module type S = sig
    type +'a v := 'a Symex.Value.t
    type t

    (** Symbolic semantic equality *)
    val sem_eq : t v -> t v -> S_bool.t Symex.Value.t

    (** Takes an integer and creates an abstraction over it *)
    val of_z : Z.t -> t v

    (** Takes a symbolic integer and returns [Some z] if this abstraction
        describes exactly a single integer, and [None] otherwise *)
    val to_z : t v -> Z.t option

    val zero : unit -> t v
    val one : unit -> t v

    (** {3 Arithmetic operations} *)

    val add : t v -> t v -> t v
    val sub : t v -> t v -> t v
    val lt : t v -> t v -> S_bool.t v
    val leq : t v -> t v -> S_bool.t v
  end

  (** Symbolic integers that are {i bounded}: there is some invariant about them
      being in a range which must be upheld. This can for instance be used to
      represent machine integers (which have a min and max value) with numerals.
  *)
  module type Bounded_S = sig
    include S

    (** Checks whether the given symbolic integer is in the range described by
        the invariant. *)
    val is_in_bound : t Symex.Value.t -> S_bool.t Symex.Value.t
  end
end

module Make_syntax
    (Symex : Symex.Base)
    (S_bool : S_bool.S(Symex).S)
    (S_int : S(Symex)(S_bool).S) =
struct
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
