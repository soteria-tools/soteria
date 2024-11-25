module SYMEX = Bfa_symex.Symex.M (Z3solver)
include SYMEX

let ( let@ ) = ( @@ )

let push_give_up, flush_give_up =
  let give_up_reasons = Dynarray.create () in
  let push_give_up r = Dynarray.add_last give_up_reasons r in
  let flush_give_up () =
    let reasons = Dynarray.to_list give_up_reasons in
    Dynarray.clear give_up_reasons;
    reasons
  in
  (push_give_up, flush_give_up)

let current_loc = ref Cerb_location.unknown
let get_loc () = !current_loc

let with_loc ~(loc : Cerb_location.t) f =
  let open Syntax in
  let old_loc = !current_loc in
  current_loc := loc;
  let* res = f () in
  current_loc := old_loc;
  return res

let with_loc_immediate ~loc f =
  let old_loc = !current_loc in
  current_loc := loc;
  let res = f () in
  current_loc := old_loc;
  res

let not_impl msg =
  let msg = "MISSING FEATURE, VANISHING: " ^ msg in
  L.info (fun m -> m "%s" msg);
  push_give_up (msg, get_loc ());
  vanish ()

let with_loc_err () f =
  let loc = get_loc () in
  Result.map_error (fun e -> (e, loc)) (f ())
[@@inline]

let error ?learned e = Result.error ?learned (e, get_loc ())
let of_opt = function Some x -> return x | None -> vanish ()
let of_opt_not_impl ~msg = function Some x -> return x | None -> not_impl msg

module Freeable = Bfa_symex.Freeable.Make (SYMEX)
module Pmap = Bfa_symex.Pmap.Make (SYMEX)
