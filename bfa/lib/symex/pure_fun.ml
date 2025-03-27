(** State model component of pure functions. A piece of state represents a
    function `f()` that returns a value in a certain set. It is quite similar to
    the agreement algebra, except that there is no allocation, there necessarily
    exists a value, and if we don't know it, we instantiate it on
    read/consumption. *)

module type Codom = sig
  module Symex : Symex.S

  type t

  val fresh : unit -> t Symex.t
  val sem_eq : t -> t -> Symex.Value.sbool Symex.Value.t
end

module Make (C : Codom) = struct
  open C
  open Symex.Syntax

  type t = C.t
  type serialized = t

  let pp pp_value = pp_value

  let load (st : t option) : ('a * t option, 'err, 'fix list) Symex.Result.t =
    match st with
    | Some x -> Symex.Result.ok (x, st)
    | None ->
        let* x = fresh () in
        Symex.Result.ok (x, Some x)

  let aux (serialized : serialized) (t : t option) : t Symex.t =
    match t with
    | Some x ->
        let+ () = Symex.assume [ sem_eq x serialized ] in
        x
    | None ->
        let+ x = fresh () in
        x

  let produce (serialized : serialized) (t : t option) =
    let+ new_x = aux serialized t in
    Some new_x

  let consume (serialized : serialized) (t : t option) =
    let+ new_x = aux serialized t in
    Symex.Result.ok (new_x, Some new_x)
end
