type t

val of_string : string -> t
val to_string : t -> string
val pp : Format.formatter -> t -> unit
val equal : t -> t -> bool
val hash : t -> int
