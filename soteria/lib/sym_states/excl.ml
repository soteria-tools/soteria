open Soteria_std
open Logs.Import

(** Create an exclusively owned state from a semantic value type. This is the
    most simple of state models: the value is either owned or not owned. *)
module Make (Symex : Symex.Base) (E : Data.Abstr.M(Symex).S_with_syn) = struct
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

  let assert_exclusively_owned () : (unit, 'err, syn list) Result.t =
    let++ _ = unwrap () in
    ()

  let load = unwrap

  let store x =
    let** _ = unwrap () in
    SM.Result.set_state (Some x)

  let to_syn (s : E.t) : syn list = [ E.to_syn s ]
  let ins_outs (s : syn) = ([], E.exprs_syn s)

  open Symex

  let produce (s : syn) (t : st) : st Producer.t =
    let open Producer.Syntax in
    match t with
    | None ->
        let+ x = Producer.apply_subst E.subst s in
        Some x
    | Some _ -> Producer.vanish ()

  let consume (s : syn) (t : st) : (st, syn list) Consumer.t =
    let open Consumer.Syntax in
    match t with
    | None -> Consumer.miss [ [ s ] ]
    | Some x ->
        let+ () = E.learn_eq s x in
        None
end

(** Create an exclusively owned state from a concrete value type. This is the
    most simple of state models: the value is either owned or not owned. The
    syntactic value type is the same as the semantic value type, and
    substitutions are the identity. We only require
    {{!Data.Abstr.M.S.fresh}[fresh]} to be implemented. *)
module Make_concrete
    (Symex : Symex.Base)
    (E : sig
      type t [@@mixins Sigs.Eq]

      include Data.Abstr.M(Symex).S with type t := t
    end) =
struct
  include
    Make
      (Symex)
      (struct
        include E

        type syn = t [@@deriving show { with_path = false }]

        let to_syn x = x
        let fresh = E.fresh
        let subst _ x = x
        let exprs_syn _ = []

        let learn_eq syn t =
          let open Symex in
          if E.equal syn t then Consumer.ok ()
          else Consumer.lfail (Value.of_bool false)
      end)
end
