(** Symbolic abstractions over ranges. *)

module Make (S_int : S_int.S) = struct
  module Symex = S_int.Symex

  type sbool = Symex.Value.S_bool.t Symex.Value.t

  (** Type of value representing the bounds of the interval *)
  type v = S_int.t

  (** A value [(a, b)] of type [t] represents the interval starting at [a]
      {e included} and ending at [b] {e excluded}. Often denoted {m [a, b)} or
      {m [a, b[}*)
  type t = v * v

  open S_int

  let sem_eq (a1, b1) (a2, b2) =
    Symex.Value.S_bool.and_ (S_int.sem_eq a1 a2) (S_int.sem_eq b1 b2)

  (** [size (a, b)] is [b - a] *)
  let size (a, b) = sub b a

  (** [split_at (a, b) x] is [(a, x), (x, b)] *)
  let split_at (l, h) x = ((l, x), (x, h))

  (** [offset (a, b) x] is [(a + x, b + x)] *)
  let offset (l, h) x = (add l x, add h x)

  (** [subseteq r1 r2] is a symbolic boolean characterising whether [r1] is a
      subset of [r2] *)
  let subset_eq (a1, b1) (a2, b2) =
    Symex.Value.S_bool.and_ (leq a2 a1) (leq b1 b2)

  (** [strictly_inside (a, b) r] is [a < r < b] *)
  let strictly_inside x (a, b) = Symex.Value.S_bool.and_ (lt a x) (lt x b)

  (** [of_low_and_size a b] is [(a, a + b)]*)
  let of_low_and_size low size = (low, add low size)

  let pp fmt (a, b) = Format.fprintf fmt "[%a, %a[" S_int.pp a S_int.pp b
end
