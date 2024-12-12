module type Mutable = sig
  type t

  val init : unit -> t
  val backtrack_n : t -> int -> unit
  val save : t -> unit
  val reset : t -> unit
end

module type In_place = sig
  val backtrack_n : int -> unit
  val save : unit -> unit
  val reset : unit -> unit
end

module Mutable_to_in_place (M : Mutable) = struct
  let state = lazy (M.init ())
  let save () = M.save (Lazy.force state)
  let backtrack_n n = M.backtrack_n (Lazy.force state) n
  let reset () = M.reset (Lazy.force state)
end

module type Immutable = sig
  type t

  val init : t
  val backtrack_n : t -> int -> t
  val save : t -> t
  val reset : t -> t
end
