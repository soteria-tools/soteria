open Prelude

(* An or-pattern whose branches all refine to the same type is allowed. *)
let use_sint (_ : [< Typed.T.sint ] Typed.t) = ()

let test (x : [< Typed.T.any ] Typed.t) =
  match%ty x with TBitVector 8 | TBitVector 16 -> use_sint x | _ -> ()
