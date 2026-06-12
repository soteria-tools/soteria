open Rust_val

type fn = TlvAtexit

let fn_pats = [ ("_tlv_atexit", TlvAtexit) ]

module M (StateM : State.StateM.S) = struct
  open StateM
  open Syntax

  (** Used on macOS to register thread local destructors; receives a function
      pointer and an argument. Should call the destructor with the argument at
      the end of the thread.

      [_tlv_atexit(dtor: unsafe extern "C" fn( *mut u8), arg: *mut u8)] *)
  let tlv_atexit ~fun_exec args =
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

  let[@inline] fn_to_stub fun_exec = function
    | TlvAtexit -> tlv_atexit ~fun_exec
end
