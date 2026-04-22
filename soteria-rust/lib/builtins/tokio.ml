(** Builtins related to the Tokio async runtime.

    See https://docs.rs/tokio/latest/tokio/ *)

open Rust_val

module M (StateM : State.StateM.S) = struct
  open StateM
  open Syntax

  let rngseed_new args =
    let () =
      match args with
      | [] -> ()
      | _ -> failwith "tokio::RngSeed::new: expected no arguments"
    in
    let* s = Encoder.nondet_valid (TLiteral (TUInt U32)) in
    let* r = Encoder.nondet_valid (TLiteral (TUInt U32)) in
    ok (Tuple [ s; r ])
end
