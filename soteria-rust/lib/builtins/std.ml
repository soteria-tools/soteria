open Rust_val

module M (StateM : State.StateM.S) = struct
  open StateM

  (** Stub for [__rust_panic_cleanup]: returns a dummy [&dyn Any] pointer so the
      caller does not crash before we handle the panic. *)
  let panic_cleanup _ = ok (Ptr (Sptr.null_ptr (), VTable (Sptr.null_ptr ())))
end
