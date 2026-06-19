open Prelude

(* An or-pattern mixing different runtime types is a hard error. *)
let test (x : [< Typed.T.any ] Typed.t) =
  match%ty x with TBitVector _ | TFloat _ -> x | _ -> x
