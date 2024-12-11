module type Mutable = sig
  type t

  val init : unit -> t
  val backtrack_n : t -> int -> unit
  val save : t -> unit
  val reset : t -> unit
end

module type Immutable = sig
  type t

  val init : t
  val backtrack_n : t -> int -> t
  val save : t -> t
  val reset : t -> t
end
