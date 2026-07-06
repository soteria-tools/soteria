open Prelude

let use_sint (_ : [< Typed.T.sint ] Typed.t) = ()
let use_sptr (_ : [< Typed.T.sptr ] Typed.t) = ()
let use_sptr_f (_ : [< Typed.T.sptr_f ] Typed.t) = ()

(* Nested [match%ty] on a different value rebinds its own scrutinee. *)
let test (x : [< Typed.T.any ] Typed.t) (y : [< Typed.T.any ] Typed.t) =
  match%ty x with
  | TExtension TFullPtr -> (
      use_sptr_f x;
      match%ty y with
      | TPointer _ -> use_sptr y
      | TBitVector _ -> use_sint y
      | _ -> ())
  | _ -> ()
