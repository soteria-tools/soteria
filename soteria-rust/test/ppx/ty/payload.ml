open Prelude

(* Sub-pattern bindings ([n], [size], [a]) and guards are preserved, and the
   cast is added on top. *)
let use_sint (_ : [< Typed.T.sint ] Typed.t) = 0

let test (x : [< Typed.T.any ] Typed.t) =
  match%ty x with
  | TBitVector n when n > 8 -> n + use_sint x
  | TBitVector size -> size + use_sint x
  | TExtension (TEnum a) -> a
  | _ -> -1
