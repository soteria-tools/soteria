(** The state of globals is a map from identifier to address. We model it as an
    immutable total map from identifier to address *)

open Csymex
module Expr = Typed.Expr

module Loc = struct
  open Csymex.Syntax

  type t = Typed.T.sloc Typed.t [@@deriving show { with_path = false }]
  type syn = Expr.t [@@deriving show { with_path = false }]

  let to_syn (loc : t) : syn = Expr.of_value loc
  let learn_eq s l = Consumer.learn_eq s l
  let exprs_syn (loc : syn) : Expr.t list = [ loc ]
  let subst = Expr.subst
  let pp = Typed.ppa

  let fresh () =
    let* loc = Csymex.fresh_alloc_id () in
    let+ () = Csymex.assume [ Typed.not (Typed.Ptr.is_null_loc loc) ] in
    loc

  let sem_eq = Typed.sem_eq
end

module Glob_fn = Pure_fun (Loc)
include Concrete_map (Symbol_std) (Glob_fn)

let pp ft t = pp ft t

(* The Concrete_map [pp] function has an option ?ignore parameter *)

let get sym =
  (* TODO: This is correct, but a tiny bit of a hack. We need to enforce the
     invariant that all symbols are different. It'd be nice to enforce this
     modularly, but right now we do it as a wrapper. Unless one has hundreds of
     globals, the cost is negligeable, but still. *)
  let open SM.Syntax in
  let* st = SM.get_state () in
  let existed = Option.fold ~none:false ~some:(syntactic_mem sym) st in
  let* res = wrap sym (Glob_fn.load ()) in
  let loc = Soteria.Symex.Compo_res.get_ok res in
  let+ () =
    if existed then (* We haven't created a new location *) SM.return ()
    else
      (* We learn that the new location is distinct from all other global
         locations *)
      syntactic_bindings (Option.value ~default:empty st)
      |> Seq.filter_map (fun (k, v) ->
          let open Typed.Infix in
          if Cerb_frontend.Symbol.equal_sym k sym then None
          else
            let neq = Typed.not (v ==@ loc) in
            Some neq)
      |> List.of_seq
      |> SM.assume
  in
  loc

let produce syn (t : t option) : t option Producer.t =
  let open Producer.Syntax in
  let* t = produce syn t in
  (* Bit heavy-handed but we just massively assume the well-formedness *)
  let distinct_locs =
    syntactic_bindings (Option.value ~default:empty t)
    |> Seq.map snd
    |> List.of_seq
    |> Typed.distinct
  in
  let* loc = Producer.apply_subst Loc.subst (snd syn) in
  let+^ () = assume [ distinct_locs; Typed.(not (Ptr.is_null_loc loc)) ] in
  t

let empty = None
