open Prelude

type cty = CInt | CFloat

(* Even across tuple branches, the first component must refine consistently. *)
let test (x : [< Typed.T.any ] Typed.t) (c : cty) =
  match%ty (x, c) with TBitVector _, CInt | TFloat _, CFloat -> x | _, _ -> x
