module SYMEX = Bfa_symex.Symex.M (Z3solver)
include SYMEX

let not_impl ?source_loc ?loc what =
  let pp_source_loc ft sl =
    Fmt.pf ft "%s@\n" (Cerb_location.location_to_string sl)
  in
  L.info (fun m ->
      m "%aMISSING FEATURE, VANISHING %a@\n%s" (Fmt.option pp_source_loc)
        source_loc
        Fmt.(option (parens string))
        loc what);
  vanish ()

let of_opt = function Some x -> return x | None -> vanish ()

let of_opt_not_impl ?source_loc ?loc ~msg = function
  | Some x -> return x
  | None -> not_impl ?source_loc ?loc msg

module Freeable = Bfa_symex.Freeable.Make (SYMEX)
module Pmap = Bfa_symex.Pmap.Make (SYMEX)
