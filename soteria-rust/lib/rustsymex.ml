module SYMEX =
  Soteria_symex.Symex.Make
    (struct
      module Range = struct
        type t = Charon.Meta.span

        let to_yojson _ = `Null
        let of_yojson _ = Ok Charon_util.empty_span
      end
    end)
    (C_solver.Z3_solver)

include SYMEX
include Syntaxes.FunctionWrap

let match_on ~(constr : 'a -> Typed.sbool Typed.t) (elements : 'a list) :
    'a option t =
  let open Syntax in
  let rec aux = function
    | e :: rest -> if%sat constr e then return (Some e) else aux rest
    | [] -> return None
  in
  aux elements

let current_loc = ref Charon_util.empty_span
let get_loc () = !current_loc

let with_loc ~loc f =
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

let[@inline] with_loc_err () f =
  let loc = get_loc () in
  Result.map_error (f ()) (fun e -> (e, loc))

let error e = Result.error (e, get_loc ())
let not_impl msg = give_up ~loc:(get_loc ()) msg
let of_opt_not_impl msg = some_or_give_up ~loc:(get_loc ()) msg

let cast_checked ~ty x =
  match Typed.cast_checked x ty with
  | Some x -> return x
  | None ->
      Fmt.kstr not_impl "Failed to cast %a to %a" Typed.ppa x Typed.ppa_ty ty

let cast_checked2 x y =
  match Typed.cast_checked2 x y with
  | Some x -> return x
  | None ->
      Fmt.kstr not_impl "Values %a and %a have mismatched types" Typed.ppa x
        Typed.ppa y

module Freeable = Soteria_symex.Freeable.Make (SYMEX)
module Pmap_direct_access = Soteria_symex.Pmap.Direct_access (SYMEX)
module Pmap = Soteria_symex.Pmap.Make (SYMEX)
module Tree_block = Soteria_symex.Tree_block.Make (SYMEX)
module Bi = Soteria_symex.Bi_abd.Make (SYMEX)
