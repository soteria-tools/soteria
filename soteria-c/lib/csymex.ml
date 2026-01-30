module Meta = struct
  module Range = struct
    type t = Cerb_location.t

    let to_yojson _ = `Null
    let of_yojson _ = Ok Cerb_location.unknown
  end
end

module SYMEX = Soteria.Symex.Make (Meta) (Bv_solver.Z3_solver)
include SYMEX
include Syntaxes.FunctionWrap

let check_nonzero (t : Typed.T.sint Typed.t) :
    ([> Typed.T.nonzero ] Typed.t, [> `NonZeroIsZero ], 'fix) Result.t =
  let open Syntax in
  let open Typed.Infix in
  if%sat t ==@ Typed.BitVec.zero (Typed.size_of_int t) then
    Result.error `NonZeroIsZero
  else Result.ok (Typed.cast t)

let current_loc = ref Cerb_location.unknown
let get_loc () = !current_loc

(* FIXME: this is actually wrong because of branching.
          the loc should probably be carried in the monad itself? *)
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
  let msg = "Unsupported: " ^ msg in
  give_up ~loc:(get_loc ()) msg

let of_opt = function Some x -> return x | None -> vanish ()
let of_opt_not_impl ~msg = function Some x -> return x | None -> not_impl msg

module With_origin =
  Soteria.Sym_states.With_info.Make
    (SYMEX)
    (struct
      type t = Cerb_location.t

      let pp = Fmt_ail.pp_loc
    end)

module Freeable = Soteria.Sym_states.Freeable.Make (SYMEX)

module Pmap_direct_access =
  Soteria.Sym_states.Pmap.Direct_access_patricia_tree (SYMEX)

module Pmap = Soteria.Sym_states.Pmap.Make_patricia_tree (SYMEX)
module Tree_block = Soteria.Sym_states.Tree_block.Make (SYMEX)
module Concrete_map = Soteria.Sym_states.Pmap.Concrete (SYMEX)
module Bi = Soteria.Sym_states.Bi_abd.Make (SYMEX)
module Pure_fun = Soteria.Sym_states.Pure_fun.Make (SYMEX)
