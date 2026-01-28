(** The state of globals is a map from identifier to address. We model it as an
    immutable total map from identifier to address *)

open Csymex

module Loc = struct
  open Csymex.Syntax

  type t = Typed.T.sloc Typed.t

  let pp = Typed.ppa

  let fresh () =
    let* loc = Csymex.nondet Typed.t_loc in
    let+ () = Csymex.assume [ Typed.not (Typed.Ptr.is_null_loc loc) ] in
    loc

  let sem_eq = Typed.sem_eq
  let subst = Typed.subst
  let iter_vars = Typed.iter_vars
end

module Glob_fn = Pure_fun (Loc)
include Concrete_map (Symbol_std) (Glob_fn)

let pp ft t = pp ft t

(* The Concrete_map [pp] function has an option ?ignore parameter *)

let get sym =
  (* TODO: This is correct, but a tiny bit of a hack.
           We need to enforce the invariant that all symbols are different.
           It'd be nice to enforce this modularly, but right now we do it as a wrapper.
           Unless one has hundreds of globals, the cost is negligeable, but still. *)
  let open SM.Syntax in
  let* st = SM.get_state () in
  let existed = Option.fold ~none:false ~some:(syntactic_mem sym) st in
  let* res = wrap sym (Glob_fn.load ()) in
  let loc = Soteria.Symex.Compo_res.get_ok res in
  let+ () =
    if existed then (* We haven't created a new location *) SM.return ()
    else
      (* We learn that the new location is distinct from all other global locations *)
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

let produce serialized : unit SM.t =
  let open SM.Syntax in
  let* () = produce serialized in
  let* state_after_prod = SM.get_state () in
  (* Bit heavy-handed but we just massively assume the well-formedness *)
  let to_assume =
    syntactic_bindings (Option.value ~default:empty state_after_prod)
    |> Seq.map snd
    |> List.of_seq
    |> Typed.distinct
  in
  SM.assume [ to_assume ]

let empty = None
