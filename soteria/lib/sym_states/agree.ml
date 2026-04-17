module Make
    (Symex : Symex.Base)
    (E : sig
      module Abstr := Data.Abstr.M(Symex)

      type t
      [@@mixins
        Abstr.S_with_syn;
        Abstr.Sem_eq]
    end) =
struct
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

  let assert_exclusively_owned () : (unit, 'err, syn list) Result.t =
    SM.Result.miss_no_fix ~reason:"Agreement can never be exclusively owned" ()

  let load = unwrap
  let to_syn (s : E.t) : syn list = [ E.to_syn s ]
  let ins_outs (s : syn) = ([], E.exprs_syn s)

  open Symex

  let produce (s : syn) (t : st) : st Producer.t =
    let open Producer in
    let open Syntax in
    let* x = apply_subst E.subst s in
    match t with
    | None -> return (Some x)
    | Some y ->
        let+^ () = Symex.assume [ E.sem_eq x y ] in
        Some x

  let consume (s : syn) (t : st) : (st, syn list) Consumer.t =
    let open Consumer.Syntax in
    match t with
    | None -> Consumer.miss [ [ s ] ]
    | Some x ->
        let+ () = E.learn_eq s x in
        Some x
end
