open Prelude

(* A second scrutinee (here a charon-like type) is matched as-is; only the first
   scrutinee gets the [get_ty]/cast treatment. *)
type cty = CInt | CFloat | CPtr | COther

let use_sint (_ : [< Typed.T.sint ] Typed.t) = ()
let use_sfloat (_ : [< Typed.T.sfloat ] Typed.t) = ()
let use_sptr_f (_ : [< Typed.T.sptr_f ] Typed.t) = ()

let test (x : [< Typed.T.any ] Typed.t) (c : cty) =
  match%ty (x, c) with
  | TBitVector _, CInt -> use_sint x
  | TFloat _, (CFloat | COther) -> use_sfloat x
  | TExtension FullPtr, CPtr -> use_sptr_f x
  | _, _ -> ()
