module SYMEX = Soteria.Symex.Make (Bv_solver.Z3_solver)

module Concrete_alloc_id = struct
  let next_id = ref 1

  let get_next () =
    let id = !next_id in
    incr next_id;
    Typed.Ptr.loc_of_int id
end

module MonadState = struct
  type t = { loc : Cerb_location.t; fn : Ail_tys.fundef option }

  let empty = { loc = Cerb_location.unknown; fn = None }
end

(* Adding the current location being executed to the general execution state *)
module CSYMEX = Soteria.Sym_states.State_monad.Make (SYMEX) (MonadState)
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

let get_loc () =
  let open Syntax in
  let+ { loc; _ } = get_state () in
  loc

let with_loc ~(loc : Cerb_location.t) (f : 'a t) : 'a t =
 fun st ->
  let open SYMEX.Syntax in
  let old_loc = st.loc in
  let+ x, st = f { st with loc } in
  (x, { st with loc = old_loc })

let fn_cov_info (fn : Ail_tys.fundef) : Soteria.Coverage.function_info option =
  let sym, (loc, _, _, _, _) = fn in
  Error.Diagnostic.extract_location loc
  |> Option.map (fun (file, line, _col) ->
      let name = Fmt.to_to_string Ail_helpers.pp_sym_hum sym in
      Soteria.Coverage.make_function_info ~file ~name ~line ())

let current_cov_loc (st : MonadState.t) : Soteria.Coverage.location option =
  match Option.bind st.fn fn_cov_info with
  | Some fn -> Some (Function fn)
  | None -> (
      match Error.Diagnostic.extract_location st.loc with
      | Some (file, _line, _col) -> Some (File file)
      | None -> None)

let with_loc_covered ~loc (f : 'a t) : 'a t =
  let open Syntax in
  let@@ () = with_loc ~loc in
  let* st = get_state () in
  Option.iter2
    (fun loc (_, line, _) -> Soteria.Coverage.As_ctx.mark loc (Line line))
    (current_cov_loc st)
    (Error.Diagnostic.extract_location loc);
  f

let with_function ~(fn : Ail_tys.fundef) (f : 'a t) : 'a t =
 fun st ->
  let open SYMEX.Syntax in
  Option.iter Soteria.Coverage.As_ctx.mark_function (fn_cov_info fn);
  let _, (fn_loc, _, _, _, _) = fn in
  let+ x, { fn = _; loc = _ } = f { fn = Some fn; loc = fn_loc } in
  (x, st)

let branch_span_of_loc (loc : Cerb_location.t) :
    Soteria.Coverage.branch_span option t =
  let open Syntax in
  let+ st = get_state () in
  Option.map2
    (fun loc (file, line, col) : Soteria.Coverage.branch_span ->
      { loc; line; branch_id = Fmt.str "%s-%d-%d" file line col })
    (current_cov_loc st)
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
  run_with_state ~state:MonadState.empty (process ())
  |> (Fun.flip SYMEX.map) fst

module Result = struct
  include CSYMEX.Result

  let run ?fuel ?stats ?coverage ?fail_fast ~mode
      (process : unit -> ('a, 'b, 'c) CSYMEX.Result.t) =
    SYMEX.Result.run ?fuel ?stats ?coverage ?fail_fast ~mode @@ fun () ->
    CSYMEX.run_with_state ~state:MonadState.empty (process ())
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
