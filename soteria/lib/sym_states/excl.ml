module Elem (Symex : Symex.Base) = struct
  open Symex

  module type S = sig
    type t [@@deriving show]
    type syn [@@deriving show]

    val fresh : unit -> t Symex.t
    val to_syn : t -> syn
    val subst : (Value.Syn.t -> 'a Value.t) -> syn -> t
    val learn_eq : syn -> t -> (unit, 'a) Symex.Consumer.t
    val exprs_syn : syn -> Symex.Value.Syn.t list
  end
end

module Make (Symex : Symex.Base) (E : Elem(Symex).S) = struct
  open Symex
  open Symex.Syntax

  type t = E.t [@@deriving show { with_path = false }]
  type syn = E.syn [@@deriving show { with_path = false }]

  let to_syn v = [ E.to_syn v ]

  let unwrap st =
    match st with
    | Some x -> Symex.Result.ok x
    | None ->
        let* v = E.fresh () in
        Result.miss [ [ E.to_syn v ] ]

  let assert_exclusively_owned (st : t option) =
    let** _ = unwrap st in
    Result.ok ()

  let load (st : t option) =
    let++ x = unwrap st in
    (x, st)

  let store x (st : t option) =
    let++ _ = unwrap st in
    ((), Some x)

  let ins_outs (syn : syn) = ([], E.exprs_syn syn)

  let produce (s : syn) (t : t option) : t option Producer.t =
    let open Producer.Syntax in
    match t with
    | None ->
        let+ x = Producer.apply_subst E.subst s in
        Some x
    | Some _ -> Producer.vanish ()

  let consume (s : syn) (t : t option) : (t option, syn list) Consumer.t =
    let open Consumer.Syntax in
    let* v = Consumer.lift_res (unwrap t) in
    let+ () = E.learn_eq s v in
    None
end
