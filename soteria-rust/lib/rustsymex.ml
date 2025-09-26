module RMeta = struct
  module Range = struct
    type t = Charon.Meta.span

    let to_yojson _ = `Null
    let of_yojson _ = Ok Charon_util.empty_span
  end
end

module LocMap = Map.Make (struct
  type t = Typed.T.sloc Typed.t

  let compare = Typed.compare
end)

module RMut = struct
  type t = Typed.T.sint Typed.t LocMap.t

  let init = LocMap.empty
end

module SYMEX = Soteria.Symex.Make (RMeta) (RMut) (Bv_solver.Z3_solver)
include SYMEX
include Syntaxes.FunctionWrap

let match_on (elements : 'a list) ~(constr : 'a -> Typed.sbool Typed.t) :
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

let lookup_decay_map loc =
  let open Syntax in
  let+ map = read_mut () in
  LocMap.find_opt loc map

let update_decay_map loc decayed = wrap_mut (LocMap.add loc decayed)

module Freeable = Soteria.Sym_states.Freeable.Make (SYMEX)
module Pmap_direct_access = Soteria.Sym_states.Pmap.Direct_access (SYMEX)
module Pmap = Soteria.Sym_states.Pmap.Make (SYMEX)
module Tree_block = Soteria.Sym_states.Tree_block.Make (SYMEX)
module Bi = Soteria.Sym_states.Bi_abd.Make (SYMEX)
