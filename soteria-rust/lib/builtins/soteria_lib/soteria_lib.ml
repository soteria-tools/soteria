(** This file was generated with [scripts/stubs.py] -- do not edit it manually,
    instead modify the script and re-run it. *)

[@@@warning "-unused-open"]

open Common
open Svalue

type fn =
  | KaniAssert
  | KaniAssume
  | KaniPanic
  | SoteriaAssert
  | SoteriaAssume
  | SoteriaNondetBytes
  | SoteriaPanic

let fn_pats : (string * fn) list =
  [
    ("kani::assert", KaniAssert);
    ("kani::assume", KaniAssume);
    ("kani::panic", KaniPanic);
    ("soteria::assert", SoteriaAssert);
    ("soteria::assume", SoteriaAssume);
    ("soteria::nondet_bytes", SoteriaNondetBytes);
    ("soteria::panic", SoteriaPanic);
  ]

module M (StateM : State.StateM.S) = struct
  open StateM
  open Syntax

  type 'a ret = ('a, unit) StateM.t
  type fun_exec = Fun_kind.t -> Typed.(T.any t) list -> Typed.(T.any t) ret

  include Impl.M (StateM)

  let[@inline] fn_to_stub stub _fun_sig _fun_exec
      (generics : Charon.Types.generic_args) args =
    match[@warning "-redundant-case"]
      (stub, generics.types, generics.const_generics, args)
    with
    | KaniAssert, _, _, _ -> kani_assert ~args
    | KaniAssume, _, _, _ -> kani_assume ~args
    | KaniPanic, _, _, _ -> kani_panic ~args
    | SoteriaAssert, _, _, _ -> soteria_assert ~args
    | SoteriaAssume, _, _, _ -> soteria_assume ~args
    | SoteriaNondetBytes, _, _, _ -> nondet_bytes ~types:generics.types ~args
    | SoteriaPanic, _, _, _ -> soteria_panic ~args
    | _, tys, cs, args ->
        not_impl
          "Custom stub found but called with the wrong arguments; got:@.Types: \
           %a@.Consts: %a@.Args: %a"
          Fmt.(list ~sep:comma Charon_util.pp_ty)
          tys
          Fmt.(list ~sep:comma Crate.pp_constant_expr)
          cs
          Fmt.(list ~sep:comma Typed.ppa)
          args
end
