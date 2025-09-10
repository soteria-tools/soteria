(** Symbolic abstractions that support equality *)

module type S = sig
  (** Defines a type that supports symbolic semantic equality. For performance
      reasons, such a type most also support {e syntactic} equality, such that
      syntactic equality ([equal]) implies semantic equality ([sem_eq]) *)

  (** The symex world in which the type lives. *)
  module Symex : Symex.S

  type t

  (** Syntactic equality, should be fast. *)
  val equal : t -> t -> bool

  (** Symbolic semantic equality *)
  val sem_eq : t -> t -> Symex.Value.S_bool.t Symex.Value.t

  (** Receives a list of {e syntactically different} values and returns a
      symbolic boolean corresponding to the fact that these values are also
      {e semantically} distinct. *)
  val distinct : t list -> Symex.Value.S_bool.t Symex.Value.t
end

module Of_concrete
    (Symex : Symex.S)
    (C : sig
      type t

      val equal : t -> t -> bool
    end) : S with module Symex = Symex and type t = C.t = struct
  module Symex = Symex
  include C

  let sem_eq x y = Symex.Value.S_bool.of_bool (C.equal x y)

  (* Always returns true as the input list should always be syntactically distinct *)
  let distinct _ = Symex.Value.S_bool.of_bool true
end
