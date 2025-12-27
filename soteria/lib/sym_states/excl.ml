module Elem (Symex : Symex.Base) = struct
  module type S = sig
    type t [@@deriving show]
    type syn [@@deriving show]

    val to_syn : t -> syn

    module Produce : sig
      val subst : syn -> t Symex.Producer.t
    end

    module Consume : sig
      val learn_eq : syn -> t -> (unit, 'a) Symex.Consumer.t
    end

    val fresh : unit -> t Symex.t
  end
end

module Make (Symex : Symex.Base) (E : Elem(Symex).S) = struct
  open Symex
  open Symex.Syntax

  type t = E.t [@@deriving show { with_path = false }]
  type syn = E.syn [@@deriving show { with_path = false }]

  let to_syn = E.to_syn

  let unwrap st =
    match st with
    | Some x -> Symex.Result.ok x
    | None ->
        let* v = E.fresh () in
        Result.miss [ E.to_syn v ]

  let assert_exclusively_owned (st : t option) =
    let** _ = unwrap st in
    Result.ok ()

  let load (st : t option) =
    let++ x = unwrap st in
    (x, st)

  let store x (st : t option) =
    let++ _ = unwrap st in
    ((), Some x)

  let serialize x = x
  let subst_serialized subst_inner subst_var x = subst_inner subst_var x

  let produce (s : syn) (t : t option) : t option Producer.t =
    let open Producer.Syntax in
    match t with
    | None ->
        let+ x = E.Produce.subst s in
        Some x
    | Some _ -> Producer.vanish ()

  let consume (s : syn) (t : t option) : (t option, syn) Consumer.t =
    let open Consumer.Syntax in
    let* v = Consumer.lift_res (unwrap t) in
    let+ () = E.Consume.learn_eq s v in
    None
end
