open Prelude

(* Each [use_*] pins the static type expected in its branch, so this only
   type-checks if [match%ty] casts to exactly the right type. *)
let use_sint (_ : [< Typed.T.sint ] Typed.t) = ()
let use_sfloat (_ : [< Typed.T.sfloat ] Typed.t) = ()
let use_sbool (_ : [< Typed.T.sbool ] Typed.t) = ()
let use_sptr (_ : [< Typed.T.sptr ] Typed.t) = ()
let use_sptr_f (_ : [< Typed.T.sptr_f ] Typed.t) = ()
let use_sptr_t (_ : [< Typed.T.sptr_t ] Typed.t) = ()
let use_adt (_ : [< Typed.T.enum ] Typed.t) = ()

let test (x : [< Typed.T.any ] Typed.t) =
  match%ty x with
  | TBool -> use_sbool x
  | TBitVector _ -> use_sint x
  | TFloat _ -> use_sfloat x
  | TPointer _ -> use_sptr x
  | TExtension TFullPtr -> use_sptr_f x
  | TExtension TThinPtr -> use_sptr_t x
  | TExtension (TEnum _) -> use_adt x
  | _ -> ()
