module SYMEX = Soteria.Symex.Make (Bv_solver.Z3_solver)
include SYMEX
include Syntaxes.FunctionWrap

module StatKeys = struct
  let give_up_reasons = "soteria-c.give-up-reasons"

  let () =
    let open Soteria.Stats in
    (* Soteria already keeps track of give up reasons so we don't need to
       display them twice. *)
    disable_printer give_up_reasons
end

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
  Soteria.Stats.As_ctx.push_binding StatKeys.give_up_reasons msg
    (Yojson (Ail_helpers.cerb_loc_to_yojson (get_loc ())));
  give_up ("Unsupported: " ^ msg)

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
