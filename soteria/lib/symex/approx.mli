type t = UX | OX

module As_ctx : sig
  val with_mode : mode:t -> (unit -> 'a) -> 'a
  val is_ux : unit -> bool
  val is_ox : unit -> bool
end
