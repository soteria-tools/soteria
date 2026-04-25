(** This file was generated with [scripts/intrinsics.py] -- do not edit it
    manually, instead modify the script and re-run it. *)

[@@@warning "-unused-open"]

open Charon
open Common
open Rust_val
module NameMatcherMap = Charon.NameMatcher.NameMatcherMap

let match_config =
  NameMatcher.{ map_vars_to_vars = false; match_with_trait_decl_refs = false }

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
        let ptr = Sptr.null_ptr_of v in
        (ptr, Thin)
    | _ -> failwith "expected pointer"

  let as_base ty (v : rust_val) = Rust_val.as_base ty v
  let as_base_i ty (v : rust_val) = Rust_val.as_base_i ty v
  let as_base_f ty (v : rust_val) = Rust_val.as_base_f ty v

  module type Intf = sig
    val hashmap_random_keys : unit -> rust_val ret

    val _tlv_atexit :
      fun_exec:fun_exec ->
      types:Types.ty list ->
      consts:Types.constant_expr list ->
      args:rust_val list ->
      rust_val ret
  end

  type fn =
    | StdSysRandomHashmapRandomKeys
    | StdSysThreadLocalGuardAppleEnableTlvAtexit

  let fn_map : fn NameMatcherMap.t =
    [
      ("std::sys::random::hashmap_random_keys", StdSysRandomHashmapRandomKeys);
      ( "std::sys::thread_local::guard::apple::enable::_tlv_atexit",
        StdSysThreadLocalGuardAppleEnableTlvAtexit );
    ]
    |> List.map (fun (p, v) -> (NameMatcher.parse_pattern p, v))
    |> NameMatcherMap.of_list

  (* BEGIN USER IMPLEMENTATION *)
  module Impl : Intf = struct
    let hashmap_random_keys () =
      Encoder.nondet_valid
        (Charon_util.mk_tuple_ty [ TLiteral (TUInt U64); TLiteral (TUInt U64) ])

    let _tlv_atexit ~fun_exec ~types:_ ~consts:_ ~args =
      let* dtor, arg =
        match args with
        | [ Ptr dtor_ptr; arg_ptr ] -> ok (dtor_ptr, arg_ptr)
        | _ ->
            Fmt.kstr not_impl "tlv_atexit: unexpected arguments: %a"
              Fmt.(list pp_rust_val)
              args
      in
      let+ () =
        State.register_thread_exit (fun () ->
            let* fn = State.lookup_fn dtor in
            let* _ = fun_exec fn [ arg ] in
            ok ())
      in
      Tuple []
  end
  (* END USER IMPLEMENTATION *)

  let fn_of_stub stub _fun_exec (generics : Charon.Types.generic_args) args =
    match (stub, generics.types, generics.const_generics, args) with
    | StdSysRandomHashmapRandomKeys, [], [], [] -> Impl.hashmap_random_keys ()
    | StdSysThreadLocalGuardAppleEnableTlvAtexit, _, _, _ ->
        Impl._tlv_atexit ~fun_exec:_fun_exec ~types:generics.types
          ~consts:generics.const_generics ~args
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

  let eval_fun (f : UllbcAst.fun_decl) (fun_exec : fun_exec)
      (generics : Charon.Types.generic_args) =
    let ctx = Crate.as_namematcher_ctx () in
    let stub =
      NameMatcherMap.find_opt ctx match_config f.item_meta.name fn_map
    in
    Option.map (fun stub -> fn_of_stub stub fun_exec generics) stub
end
