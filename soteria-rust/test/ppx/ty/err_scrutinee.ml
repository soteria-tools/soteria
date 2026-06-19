open Prelude

(* The scrutinee must be a simple identifier. *)
let test (x : [< Typed.T.any ] Typed.t) =
  match%ty Typed.cast x with TBool -> () | _ -> ()
