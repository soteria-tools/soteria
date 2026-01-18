open Symex

module Elem (Symex : Symex.Base) = struct
  module type S = sig
    type t [@@deriving show]

    val fresh : unit -> t Symex.t
    val sem_eq : t -> t -> Symex.Value.(sbool t)
  end
end

module Make (Symex : Symex.Base) (E : Elem(Symex).S) = struct
  open Symex
  open Symex.Syntax

  type t = E.t [@@deriving show { with_path = false }]
  type serialized = E.t [@@deriving show { with_path = false }]

  let unwrap st =
    match st with
    | Some x -> Symex.Result.ok x
    | None ->
        let* v = E.fresh () in
        Result.miss [ v ]

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

  let iter_vars_serialized (i : 'a -> 'b Symex.Value.ty Var.iter_vars)
      (x : serialized) : 'b Symex.Value.ty Var.iter_vars =
   fun f -> i x f

  let subst_serialized subst_inner subst_var x = subst_inner subst_var x

  let consume x (t : t option) :
      (t option, [> Symex.lfail ], serialized) Symex.Result.t =
    let** y = unwrap t in
    let++ () = Symex.consume_pure (E.sem_eq x y) in
    None

  let produce (v : serialized) (t : t option) =
    match t with None -> Symex.return (Some v) | Some _ -> Symex.vanish ()
end
