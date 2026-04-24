(** Builtins related to the Tokio async runtime.

    See https://docs.rs/tokio/latest/tokio/ *)

module M (StateM : State.StateM.S) = struct
  open StateM

  let rngseed_new ~ret_ty _args = Encoder.nondet_valid ret_ty
end
