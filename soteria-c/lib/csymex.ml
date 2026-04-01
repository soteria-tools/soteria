module SYMEX = Soteria.Symex.Make (Bv_solver.Z3_solver)

module Concrete_alloc_id = struct
  let next_id = ref 1

  let get_next () =
    let id = !next_id in
    incr next_id;
    Typed.Ptr.loc_of_int id
end

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

let fresh_alloc_id () =
  match Config.current_mode () with
  | Compositional -> nondet Typed.t_loc
  | Whole_program -> return (Concrete_alloc_id.get_next ())

let check_nonzero (t : Typed.T.sint Typed.t) :
    ([> Typed.T.nonzero ] Typed.t, [> `NonZeroIsZero ], 'fix) Result.t =
  let open Syntax in
  let open Typed.Infix in
  if%sat t ==@ Typed.BitVec.zero (Typed.size_of_int t) then
    Result.error `NonZeroIsZero
  else Result.ok (Typed.cast t)

let get_loc () = get_state ()

let with_loc ~(loc : Cerb_location.t) f =
  Option.iter
    (fun (file, line, _col) -> Soteria.Coverage.As_ctx.mark ~file (Line line))
    (Error.Diagnostic.extract_location loc);
  with_state ~state:loc f

let branch_span_of_loc (loc : Cerb_location.t) :
    Soteria.Coverage.branch_span option =
  Option.map
    (fun (file, line, _col) : Soteria.Coverage.branch_span ->
      { file; line; branch_id = Fmt.to_to_string Fmt_ail.pp_loc loc })
    (Error.Diagnostic.extract_location loc)

let not_impl msg =
  let open Syntax in
  let* loc = get_loc () in
  let json = Ail_helpers.cerb_loc_to_yojson loc in
  Soteria.Stats.As_ctx.push_binding StatKeys.give_up_reasons msg (Yojson json);
  give_up ("Unsupported: " ^ msg)

let of_opt = function Some x -> return x | None -> vanish ()
let of_opt_not_impl ~msg = function Some x -> return x | None -> not_impl msg

let run ?fuel ?stats ?coverage ~mode process =
  SYMEX.run ?fuel ?stats ?coverage ~mode @@ fun () ->
  run_with_state ~state:Cerb_location.unknown (process ())
  |> (Fun.flip SYMEX.map) fst

module Result = struct
  include CSYMEX.Result

  let run ?fuel ?stats ?coverage ?fail_fast ~mode
      (process : unit -> ('a, 'b, 'c) CSYMEX.Result.t) =
    SYMEX.Result.run ?fuel ?stats ?coverage ?fail_fast ~mode @@ fun () ->
    CSYMEX.run_with_state ~state:Cerb_location.unknown (process ())
    |> (Fun.flip SYMEX.map) fst

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
