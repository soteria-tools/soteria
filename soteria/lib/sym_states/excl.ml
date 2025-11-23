open Symex

module Elem (Symex : Symex.Base) = struct
  module type S = sig
    type t [@@deriving show]

    val fresh : unit -> t Symex.t
    val sem_eq : t -> t -> Symex.Value.sbool Symex.Value.t
  end
end

module Make (Symex : Symex.Base) (E : Elem(Symex).S) = struct
  open Symex
  open Symex.Syntax

  module Logic = struct
    type pred = Owned [@@deriving show { with_path = false }]
    type ins = unit [@@deriving show { with_path = false }]
    type outs = E.t [@@deriving show { with_path = false }]
  end

  type t = E.t [@@deriving show { with_path = false }]

  type serialized = Logic.pred * Logic.ins * Logic.outs
  [@@deriving show { with_path = false }]

  let unwrap st =
    match st with
    | Some x -> Symex.Result.ok x
    | None ->
        let* v = E.fresh () in
        Result.miss [ (Logic.Owned, (), v) ]

  let assert_exclusively_owned (st : t option) =
    let** _ = unwrap st in
    Result.ok ()

  let load (st : t option) =
    let++ x = unwrap st in
    (x, st)

  let store x (st : t option) =
    let++ _ = unwrap st in
    ((), Some x)

  let serialize x = (Logic.Owned, (), x)

  let iter_vars_serialized (i : 'a -> 'b Symex.Value.ty Var.iter_vars)
      ((Logic.Owned, (), x) : serialized) : 'b Symex.Value.ty Var.iter_vars =
   fun f -> i x f

  let subst_serialized subst_inner subst_var x = subst_inner subst_var x

  let consume Logic.Owned (() : Logic.ins) (t : t option) :
      (Logic.outs * t option, [> Symex.lfail ], serialized) Symex.Result.t =
    let++ x = unwrap t in
    (x, None)

  let produce ((Owned, (), v) : serialized) (t : t option) =
    match t with None -> Symex.return (Some v) | Some _ -> Symex.vanish ()
end
