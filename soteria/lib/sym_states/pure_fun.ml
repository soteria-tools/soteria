(** State model component of pure functions. A piece of state represents a
    function `f()` that returns a value in a certain set. It is quite similar to
    the agreement algebra, except that there is no allocation, there necessarily
    exists a value, and if we don't know it, we instantiate it on
    read/consumption. *)

open Symex
open Compo_res

(** FIXME: This is almost verbatim the same thing as the input of excl *)
module Codom (Symex : Symex.Base) = struct
  open Symex

  module type S = sig
    type t [@@deriving show]
    type syn [@@deriving show]

    val fresh : unit -> t Symex.t
    val sem_eq : t -> t -> Symex.Value.(sbool t)
    val to_syn : t -> syn
    val subst : (Value.Expr.t -> 'a Value.t) -> syn -> t
    val learn_eq : syn -> t -> (unit, 'a) Symex.Consumer.t
    val exprs_syn : syn -> Symex.Value.Expr.t list
  end
end

module Make (Symex : Symex.Base) (C : Codom(Symex).S) = struct
  open C

  type t = C.t [@@deriving show]
  type syn = C.syn [@@deriving show]

  let produce (s : syn) (t : E.t option) : E.t option Producer.t =
    let open Producer.Syntax in
    match t with
    | None ->
        let+ x = Producer.apply_subst E.subst s in
        Some x
    | Some _ -> Producer.vanish ()

  module SM =
    State_monad.Make
      (Symex)
      (struct
        type t = C.t option
      end)

  open SM
  open SM.Syntax

  let to_syn s = [ s ]
  let pp = C.pp
  let pp_syn = pp

  let load () =
    let* st = SM.get_state () in
    match st with
    | Some x -> Result.ok x
    | None ->
        let* x = lift @@ fresh () in
        let+ () = SM.set_state (Some x) in
        Ok x

  (* let consume (syn : syn) = let** t = load () in lift @@ Symex.consume_pure
     (sem_eq t syn) *)
end
