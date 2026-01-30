open Symex

module Elem (Symex : Symex.Base) = struct
  module type S = sig
    type t [@@deriving show]

    val fresh : unit -> t Symex.t
    val sem_eq : t -> t -> Symex.Value.(sbool t)
    val subst : (Var.t -> Var.t) -> t -> t
    val iter_vars : t -> 'a Symex.Value.ty Var.iter_vars
  end
end

module Make (Symex : Symex.Base) (E : Elem(Symex).S) = struct
  type t = E.t [@@deriving show { with_path = false }]
  type serialized = E.t [@@deriving show { with_path = false }]

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
        SM.Result.miss [ [ v ] ] st

  open SM
  open SM.Syntax

  let assert_exclusively_owned st : (unit, 'err, serialized list) Symex.Result.t
      =
    let open Symex.Syntax in
    let+ res, _ = unwrap () st in
    Compo_res.map res (fun _ -> ())

  let load = unwrap

  let store x =
    let** _ = unwrap () in
    SM.Result.set_state (Some x)

  let serialize s = [ s ]

  let iter_vars_serialized (x : serialized) : 'b Symex.Value.ty Var.iter_vars =
   fun f -> E.iter_vars x f

  let subst_serialized (subst_var : Var.t -> Var.t) (x : serialized) :
      serialized =
    E.subst subst_var x

  let consume x : (unit, [> Symex.lfail ], serialized list) SM.Result.t =
    let** y = unwrap () in
    let** () = SM.consume_pure (E.sem_eq x y) in
    SM.Result.set_state None

  let produce (v : serialized) : unit SM.t =
    let* t = SM.get_state () in
    match t with None -> SM.set_state (Some v) | Some _ -> SM.vanish ()
end
