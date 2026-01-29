(** State model component of pure functions. A piece of state represents a
    function `f()` that returns a value in a certain set. It is quite similar to
    the agreement algebra, except that there is no allocation, there necessarily
    exists a value, and if we don't know it, we instantiate it on
    read/consumption. *)

open Symex
open Compo_res

module Codom (Symex : Symex.Base) = struct
  module type S = sig
    type t

    val pp : Format.formatter -> t -> unit
    val fresh : unit -> t Symex.t
    val sem_eq : t -> t -> Symex.Value.(sbool t)
    val subst : (Var.t -> Var.t) -> t -> t
    val iter_vars : t -> 'a Symex.Value.ty Var.iter_vars
  end
end

module Make (Symex : Symex.Base) (C : Codom(Symex).S) = struct
  open C

  type t = C.t [@@deriving show]
  type serialized = t [@@deriving show]

  module SM =
    State_monad.Make
      (Symex)
      (struct
        type t = C.t option
      end)

  open SM
  open SM.Syntax

  let serialize s = [ s ]
  let subst_serialized subst_var s = C.subst subst_var s
  let iter_vars_serialized s f = C.iter_vars s f
  let pp = C.pp
  let pp_serialized = pp

  let load () =
    let* st = SM.get_state () in
    match st with
    | Some x -> Result.ok x
    | None ->
        let* x = lift @@ fresh () in
        let+ () = SM.set_state (Some x) in
        Ok x

  let produce (serialized : serialized) : unit SM.t =
    let* t = SM.get_state () in
    match t with
    | Some x -> SM.assume [ sem_eq x serialized ]
    | None -> SM.set_state (Some serialized)

  let consume (serialized : serialized) =
    let** t = load () in
    lift @@ Symex.consume_pure (sem_eq t serialized)
end
