(** A collection of algebraic effect and their handler *)

module Read_only_state (S : sig
  type t
end) : sig
  val get : unit -> S.t
  val run : S.t -> (unit -> 'a) -> 'a
end = struct
  type _ Effect.t += Get : S.t Effect.t

  let get () = Effect.perform Get

  let run (state : S.t) (k : unit -> 'a) : 'a =
    try k () with effect Get, k -> Effect.Deep.continue k state
end
