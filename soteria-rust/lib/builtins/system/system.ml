(** This file was generated with [scripts/stubs.py] -- do not edit it manually,
    instead modify the script and re-run it. *)

[@@@warning "-unused-open"]

open Common
open Rust_val

type fn =
  | StdEnvVarInner
  | StdSysRandomHashmapRandomKeys
  | StdSysTimeUnixInstantNow
  | StdThreadFunctionsAvailableParallelism

let fn_pats : (string * fn) list =
  [
    ("std::env::var::inner", StdEnvVarInner);
    ("std::sys::random::hashmap_random_keys", StdSysRandomHashmapRandomKeys);
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
  type fun_exec = Fun_kind.t -> rust_val list -> (rust_val, unit) StateM.t

  let as_ptr (v : rust_val) = Rust_val.as_ptr v
  let as_base ty (v : rust_val) = Rust_val.as_base ty v
  let as_base_i ty (v : rust_val) = Rust_val.as_base_i ty v
  let as_base_f ty (v : rust_val) = Rust_val.as_base_f ty v

  include Impl.M (StateM)

  let[@inline] fn_to_stub stub _fun_sig _fun_exec
      (generics : Charon.Types.generic_args) args =
    match[@warning "-redundant-case"]
      (stub, generics.types, generics.const_generics, args)
    with
    | StdEnvVarInner, [], [], [ key ] ->
        let key = as_ptr key in
        inner ~fun_sig:_fun_sig ~key
    | StdSysRandomHashmapRandomKeys, [], [], [] ->
        hashmap_random_keys ~fun_sig:_fun_sig
    | StdSysTimeUnixInstantNow, [], [], [] -> now ()
    | StdThreadFunctionsAvailableParallelism, [], [], [] ->
        available_parallelism ~fun_sig:_fun_sig
    | _, tys, cs, args ->
        not_impl
          "Custom stub found but called with the wrong arguments; got:@.Types: \
           %a@.Consts: %a@.Args: %a"
          Fmt.(list ~sep:comma Charon_util.pp_ty)
          tys
          Fmt.(list ~sep:comma Crate.pp_constant_expr)
          cs
          Fmt.(list ~sep:comma Rust_val.pp)
          args
end
