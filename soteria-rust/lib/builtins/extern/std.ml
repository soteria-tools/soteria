open Rust_val

type fn = PanicCleanup

let fn_pats = [ ("__rust_panic_cleanup", PanicCleanup) ]

module M (StateM : State.StateM.S) = struct
  open StateM

  (** Stub for [__rust_panic_cleanup]: returns a dummy [&dyn Any] pointer so the
      caller does not crash before we handle the panic. *)
  let panic_cleanup _ = ok (Ptr (Sptr.null (), VTable (Sptr.null ())))

  let[@inline] fn_to_stub = function PanicCleanup -> panic_cleanup
end
