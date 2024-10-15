module SYMEX = Bfa_symex.Symex.M (Z3solver)
include SYMEX

let push_give_up, flush_give_up =
  let give_up_reasons = Dynarray.create () in
  let push_give_up r = Dynarray.add_last give_up_reasons r in
  let flush_give_up () =
    let reasons = Dynarray.to_list give_up_reasons in
    Dynarray.clear give_up_reasons;
    reasons
  in
  (push_give_up, flush_give_up)

let not_impl ?source_loc ?loc what =
  let pp_source_loc ft sl =
    Fmt.pf ft "%s@\n" (Cerb_location.location_to_string sl)
  in
  let msg =
    Fmt.str "%aMISSING FEATURE, VANISHING %a@\n%s" (Fmt.option pp_source_loc)
      source_loc
      Fmt.(option (parens string))
      loc what
  in
  L.info (fun m -> m "%s" msg);
  push_give_up (msg, source_loc);
  vanish ()

let of_opt = function Some x -> return x | None -> vanish ()

let of_opt_not_impl ?source_loc ?loc ~msg = function
  | Some x -> return x
  | None -> not_impl ?source_loc ?loc msg

module Freeable = Bfa_symex.Freeable.Make (SYMEX)
module Pmap = Bfa_symex.Pmap.Make (SYMEX)
