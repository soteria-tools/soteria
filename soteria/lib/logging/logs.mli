(** Main entry to the {!Soteria_logs}, usually opened wherever logging is
    needed. *)

type ('a, 'b) msgf = (('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b

val start_section : ?is_branch:bool -> string -> unit
val end_section : unit -> unit
val with_section : ?is_branch:bool -> string -> (unit -> 'a) -> 'a

module L : sig
  val log : level:Level.t -> ('a, unit) msgf -> unit
  val smt : ('a, unit) msgf -> unit
  val trace : ('a, unit) msgf -> unit
  val debug : ('a, unit) msgf -> unit
  val info : ('a, unit) msgf -> unit
  val warn : ('a, unit) msgf -> unit
  val app : ('a, unit) msgf -> unit
  val error : ('a, unit) msgf -> unit
end
