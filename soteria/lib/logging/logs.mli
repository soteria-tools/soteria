type ('a, 'b) msgf = (('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b

val start_section : string -> unit
val end_section : unit -> unit

module L : sig
  val log : level:Level.t -> ('a, unit) msgf -> unit
  val trace : ('a, unit) msgf -> unit
  val debug : ('a, unit) msgf -> unit
  val info : ('a, unit) msgf -> unit
  val warn : ('a, unit) msgf -> unit
  val app : ('a, unit) msgf -> unit
  val error : ('a, unit) msgf -> unit
end
