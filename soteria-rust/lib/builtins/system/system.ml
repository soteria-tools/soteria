(** This file was generated with [scripts/stubs.py] -- do not edit it manually,
    instead modify the script and re-run it. *)

[@@@warning "-unused-open"]

open Common
open Svalue

type fn =
  | StdSysEnvUnixGetenv
  | StdSysRandomHashmapRandomKeys
  | StdSysThreadLocalGuardAppleEnableTlvAtexit
  | StdSysTimeUnixInstantNow
  | StdThreadFunctionsAvailableParallelism

let fn_pats : (string * fn) list =
  [
    ("std::sys::env::_::getenv", StdSysEnvUnixGetenv);
    ("std::sys::random::hashmap_random_keys", StdSysRandomHashmapRandomKeys);
    ( "std::sys::thread_local::guard::apple::enable::_tlv_atexit",
      StdSysThreadLocalGuardAppleEnableTlvAtexit );
    ("std::sys::time::unix::Instant::now", StdSysTimeUnixInstantNow);
    ( "std::thread::functions::available_parallelism",
      StdThreadFunctionsAvailableParallelism );
    ( "std::thread::available_parallelism",
      StdThreadFunctionsAvailableParallelism );
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
    | StdSysEnvUnixGetenv, [], [], [ k ] ->
        let k = Typed.cast_ptr_f k in
        getenv ~fun_sig:_fun_sig ~k
    | StdSysRandomHashmapRandomKeys, [], [], [] ->
        hashmap_random_keys ~fun_sig:_fun_sig
    | StdSysThreadLocalGuardAppleEnableTlvAtexit, _, _, _ ->
        _tlv_atexit ~fun_exec:_fun_exec ~args
    | StdSysTimeUnixInstantNow, [], [], [] -> now ()
    | StdThreadFunctionsAvailableParallelism, [], [], [] ->
        available_parallelism ~fun_sig:_fun_sig
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
