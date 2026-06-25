open Svalue

type fn = PanicCleanup | PanicImpl

let fn_pats =
  [ ("__rust_panic_cleanup", PanicCleanup); ("panic_impl", PanicImpl) ]

module M (StateM : State.StateM.S) = struct
  open StateM

  (** Stub for [__rust_panic_cleanup]: returns a dummy [&dyn Any] pointer so the
      caller does not crash before we handle the panic. *)
  let panic_cleanup _ =
    ok (Typed.Ptr.mk_ptr_f (Typed.Ptr.null ()) (Some (Typed.Ptr.null ())))

  (** This shoyld be a call that gets resolved to the `#[panic_handler]`
      function. For now, we just panic. *)
  let panic_impl _ = error (`Panic None)

  let[@inline] fn_to_stub = function
    | PanicCleanup -> panic_cleanup
    | PanicImpl -> panic_impl
end
