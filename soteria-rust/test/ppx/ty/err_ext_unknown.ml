open Prelude

(* An unknown extension constructor inside [TExtension] is a hard error. *)
let test (x : [< Typed.T.any ] Typed.t) =
  match%ty x with TExtension TBogus -> x | _ -> x
