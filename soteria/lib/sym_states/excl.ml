open Symex

module Elem (Symex : Symex.Base) = struct
  open Symex

  module type S = sig
    type t [@@deriving show]
    type syn [@@deriving show]

    val fresh : unit -> t Symex.t
    val to_syn : t -> syn
    val subst : (Value.Expr.t -> 'a Value.t) -> syn -> t
    val learn_eq : syn -> t -> (unit, 'a) Symex.Consumer.t
    val exprs_syn : syn -> Symex.Value.Expr.t list
  end
end

module Make (Symex : Symex.Base) (E : Elem(Symex).S) = struct
  type t = E.t [@@deriving show { with_path = false }]
  type syn = E.syn [@@deriving show { with_path = false }]

  module SM =
    State_monad.Make
      (Symex)
      (struct
        type nonrec t = t option
      end)

  let unwrap () st =
    let open Symex.Syntax in
    match st with
    | Some x -> SM.Result.ok x st
    | None ->
        let* v = E.fresh () in
        SM.Result.miss [ [ E.to_syn v ] ] st

  open SM
  open SM.Syntax

  let assert_exclusively_owned st : (unit, 'err, syn list) Symex.Result.t =
    let open Symex.Syntax in
    let+ res, _ = unwrap () st in
    Compo_res.map res (fun _ -> ())

  let load = unwrap

  let store x =
    let** _ = unwrap () in
    SM.Result.set_state (Some x)

  let to_syn s = [ s ]

  (* let consume x : (unit, [> Symex.lfail ], syn list) SM.Result.t = let** y =
     unwrap () in let** () = SM.consume_pure (E.sem_eq x y) in
     SM.Result.set_state None *)

  let produce (s : syn) (t : E.t option) : E.t option Producer.t =
    let open Producer.Syntax in
    match t with
    | None ->
        let+ x = Producer.apply_subst E.subst s in
        Some x
    | Some _ -> Producer.vanish ()
end
