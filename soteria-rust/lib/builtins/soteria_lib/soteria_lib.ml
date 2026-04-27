(** This file was generated with [scripts/stubs.py] -- do not edit it manually,
    instead modify the script and re-run it. *)

[@@@warning "-unused-open"]

open Common
open Rust_val

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

  type rust_val = Sptr.t Rust_val.t
  type 'a ret = ('a, unit) StateM.t
  type fun_exec = Fun_kind.t -> rust_val list -> (rust_val, unit) StateM.t
  type full_ptr = StateM.Sptr.t Rust_val.full_ptr

  let[@inline] as_ptr (v : rust_val) =
    match v with
    | Ptr ptr -> ptr
    | Int v ->
        let v = Typed.cast_i Usize v in
        let ptr = Sptr.of_address v in
        (ptr, Thin)
    | _ -> failwith "expected pointer"

  let as_base ty (v : rust_val) = Rust_val.as_base ty v
  let as_base_i ty (v : rust_val) = Rust_val.as_base_i ty v
  let as_base_f ty (v : rust_val) = Rust_val.as_base_f ty v

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
        Fmt.kstr not_impl
          "Custom stub found but called with the wrong arguments; got:@.Types: \
           %a@.Consts: %a@.Args: %a"
          Fmt.(list ~sep:comma Charon_util.pp_ty)
          tys
          Fmt.(list ~sep:comma Crate.pp_constant_expr)
          cs
          Fmt.(list ~sep:comma pp_rust_val)
          args
end
