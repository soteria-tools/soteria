open Common
open Rust_val

module M (StateM : State.StateM.S) : Intf.M(StateM).S = struct
  open StateM
  open Syntax

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
