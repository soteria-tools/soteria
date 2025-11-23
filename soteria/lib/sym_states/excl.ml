open Symex

module type Elem = sig
  module Symex : Symex.Base

  type t [@@deriving show]

  val fresh : unit -> t Symex.t
end

module Make (Symex : Symex.Base) (E : Elem with module Symex = Symex) = struct
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
  let pp_serialized = pp

  let iter_vars_serialized (i : 'a -> 'b Symex.Value.ty Var.iter_vars)
      (x : serialized) : 'b Symex.Value.ty Var.iter_vars =
   fun f -> i x f

  let subst_serialized subst_inner subst_var x = subst_inner subst_var x

  let consume ~sem_eq (serialized : serialized) (t : t option) :
      (t option, [> Symex.lfail ], serialized) Symex.Result.t =
    let open Symex.Syntax in
    match t with
    | Some x ->
        let++ () = Symex.consume_pure (sem_eq x serialized) in
        None
    | None -> Symex.Result.miss [ serialized ]

  let produce (serialized : serialized) (t : t option) =
    match t with
    | None -> Symex.return (Some serialized)
    | Some _ -> Symex.vanish ()
end
