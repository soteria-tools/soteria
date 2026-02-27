open Symex

module Make (Symex : Symex.Base) (E : Data.M(Symex).Abstr_with_syn) = struct
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

  let to_syn (s : E.t) : syn list = [ E.to_syn s ]
  let ins_outs (s : syn) = ([], E.exprs_syn s)

  (* let consume x : (unit, [> Symex.lfail ], syn list) SM.Result.t = let** y =
     unwrap () in let** () = SM.consume_pure (E.sem_eq x y) in
     SM.Result.set_state None *)

  open Symex

  let produce (s : syn) (t : E.t option) : E.t option Producer.t =
    let open Producer.Syntax in
    match t with
    | None ->
        let+ x = Producer.apply_subst E.subst s in
        Some x
    | Some _ -> Producer.vanish ()
end
