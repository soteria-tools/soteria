module SYMEX = Soteria.Symex.Make (Bv_solver.Z3_solver)

(* Adding the current location being executed to the general execution state *)
module CSYMEX =
  Soteria.Sym_states.State_monad.Make
    (SYMEX)
    (struct
      type t = Cerb_location.t
    end)

include CSYMEX
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

let get_loc () = get_state ()

(* FIXME: this is actually wrong because of branching. the loc should probably
   be carried in the monad itself? *)
let with_loc ~(loc : Cerb_location.t) f = with_state ~state:loc f

let not_impl msg =
  let open Syntax in
  let* loc = get_loc () in
  let json = Ail_helpers.cerb_loc_to_yojson loc in
  Soteria.Stats.As_ctx.push_binding StatKeys.give_up_reasons msg (Yojson json);
  give_up ("Unsupported: " ^ msg)

let of_opt = function Some x -> return x | None -> vanish ()
let of_opt_not_impl ~msg = function Some x -> return x | None -> not_impl msg

let run ?fuel ~mode process =
  run_with_state ~state:Cerb_location.unknown process
  |> (Fun.flip SYMEX.map) fst
  |> SYMEX.run ?fuel ~mode

let run_needs_stats ?fuel ~mode process =
  run_with_state ~state:Cerb_location.unknown process
  |> (Fun.flip SYMEX.map) fst
  |> SYMEX.run_needs_stats ?fuel ~mode

module Result = struct
  include CSYMEX.Result

  let run_needs_stats ?fuel ?fail_fast ~mode
      (process : ('a, 'b, 'c) CSYMEX.Result.t) =
    CSYMEX.run_with_state ~state:Cerb_location.unknown process
    |> (Fun.flip SYMEX.map) fst
    |> SYMEX.Result.run_needs_stats ?fuel ?fail_fast ~mode

  let error_with_loc ?msg err =
    let open Syntax in
    let* loc = get_loc () in
    let err = Error.with_trace ?msg err loc in
    Result.error err
end

module With_origin =
  Soteria.Sym_states.With_info.Make
    (CSYMEX)
    (struct
      type t = Cerb_location.t

      let pp = Fmt_ail.pp_loc
    end)

module Freeable = Soteria.Sym_states.Freeable.Make (CSYMEX)

module Pmap_direct_access =
  Soteria.Sym_states.Pmap.Direct_access_patricia_tree (CSYMEX)

module Pmap = Soteria.Sym_states.Pmap.Make_patricia_tree (CSYMEX)
module Tree_block = Soteria.Sym_states.Tree_block.Make (CSYMEX)
module Concrete_map = Soteria.Sym_states.Pmap.Concrete (CSYMEX)
module Bi = Soteria.Sym_states.Bi_abd.Make (CSYMEX)
module Pure_fun = Soteria.Sym_states.Pure_fun.Make (CSYMEX)
