open Prelude

(* An unknown runtime-type constructor is a hard error, rather than silently
   leaving the scrutinee uncast. *)
let test (x : [< Typed.T.any ] Typed.t) =
  match%ty x with TBogus _ -> x | _ -> x
