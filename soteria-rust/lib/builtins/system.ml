module BV = Typed.BitVec
open Rust_val

module M (Rust_state_m : Rust_state_m.S) = struct
  open Rust_state_m
  open Syntax

  (** Used on macOS to register thread local destructors; receives a function
      pointer and an argument. Should call the destructor with the argument at
      the end of the thread.

      [_tlv_atexit(dtor: unsafe extern "C" fn( *mut u8), arg: *mut u8)] *)
  let tlv_atexit exec_fun args =
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
          let* _ = exec_fun fn [ arg ] in
          ok ())
    in
    Tuple []

  let hashmap_random_keys args =
    let u64 = Charon.Types.TLiteral (TUInt U64) in
    let u64_u64 = Charon_util.mk_concrete_tuple_ty [ u64; u64 ] in
    match args with
    | [] -> Encoder.nondet u64_u64
    | _ -> Fmt.failwith "hashmap_random_keys: invalid arguments"
end
