(** This file was generated with [scripts/stubs.py] -- do not edit it manually,
    instead modify the script and re-run it. *)

[@@@warning "-unused-open"]

open Common
open Svalue

type fn = CorePtrDropInPlace | StdPanickingCatchUnwindCleanup

let fn_pats : (string * fn) list =
  [
    ("core::ptr::drop_in_place", CorePtrDropInPlace);
    ("std::panicking::catch_unwind::cleanup", StdPanickingCatchUnwindCleanup);
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
    | CorePtrDropInPlace, [ t ], [], [ to_drop ] ->
        let to_drop = Typed.cast_ptr_f to_drop in
        let+ () = drop_in_place ~t ~to_drop in
        Typed.Adt.mk_tuple []
    | StdPanickingCatchUnwindCleanup, [], [], [ payload ] ->
        let payload = Typed.cast_ptr_f payload in
        cleanup ~payload
    | _, tys, cs, args ->
        Fmt.kstr not_impl
          "Custom stub found but called with the wrong arguments; got:@.Types: \
           %a@.Consts: %a@.Args: %a"
          Fmt.(list ~sep:comma Charon_util.pp_ty)
          tys
          Fmt.(list ~sep:comma Crate.pp_constant_expr)
          cs
          Fmt.(list ~sep:comma Typed.ppa)
          args
end
