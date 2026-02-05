module StatKeys = struct
  let load_accesses = "soteria-rust.loads"
  let loads_from_store = "soteria-rust.loads_from_store"
  let function_calls = "soteria-rust.function_calls"

  let () =
    let open Soteria.Stats in
    let open Soteria.Logs.Printers in
    disable_printer loads_from_store;
    register_int_printer ~name:"Load accesses" load_accesses (fun stats ft n ->
        let store_loads = get_int stats loads_from_store in
        Fmt.pf ft "%d (%a through store)" n pp_percent
          (Float.of_int n, Float.of_int store_loads));
    register_int_printer ~name:"Function calls" function_calls (fun _ ->
        Fmt.int)
end

module MonoSymex = Soteria.Symex.Make (Bv_solver.Z3_solver)

module TypeMap = Map.Make (struct
  type t = Charon.Types.ty

  let compare = Charon.Types.compare_ty
end)

module MonadState = struct
  type t = {
    where : Where.t;
    subst : Charon.Substitute.subst;
    generic_layouts : Layout_common.t TypeMap.t;
  }

  let empty =
    {
      where = Where.nowhere;
      subst = Charon.Substitute.empty_subst;
      generic_layouts = TypeMap.empty;
    }
end

include Soteria.Sym_states.State_monad.Make (MonoSymex) (MonadState)
include Syntaxes.FunctionWrap

let run_with_stats ?fuel ?fail_fast ~mode symex =
  run_with_state ~state:MonadState.empty symex
  |> (Fun.flip MonoSymex.map) fst
  |> MonoSymex.Result.run_with_stats ?fuel ?fail_fast ~mode

module Poly = struct
  open Charon
  open Substitute
  open Syntax

  let push_generics ~params ~args (x : 'a t) : 'a t =
    (* We only push generics in polymorphic mode, as otherwise we may get some
       wrong generics in monomorphic code we want to ignore. *)
    if (Config.get ()).polymorphic then (
      let* ({ subst; _ } as st) = get_state () in
      let args' = generic_args_substitute subst args in
      L.debug (fun m -> m "Pushing generics %a" Crate.pp_generic_args args');
      let subst =
        subst_at_binder_zero (make_sb_subst_from_generics params args' Self)
      in
      with_state ~state:{ st with subst } x)
    else x

  let subst f x =
    let+ { subst; _ } = get_state () in
    f subst x

  let subst_ty = subst ty_substitute
  let subst_tys = subst (fun subst -> List.map (ty_substitute subst))
  let subst_tref = subst trait_ref_substitute
  let subst_constant_expr = subst st_substitute_visitor#visit_constant_expr

  let fill_params params =
    subst generic_args_substitute @@ bound_identity_args params

  let get_layout ty =
    let+ { generic_layouts; _ } = get_state () in
    TypeMap.find_opt ty generic_layouts

  let push_layout ty layout =
    map_state (fun ({ generic_layouts; _ } as st) ->
        { st with generic_layouts = TypeMap.add ty layout generic_layouts })
end

let match_on (elements : 'a list) ~(constr : 'a -> Typed.sbool Typed.t) :
    'a option t =
  let open Syntax in
  let rec aux = function
    | e :: rest -> if%sat constr e then return (Some e) else aux rest
    | [] -> return None
  in
  aux elements

let with_loc ~loc (f : 'a t) : 'a t =
  let open Syntax in
  let* st = get_state () in
  with_state ~state:{ st with where = Where.move_to loc st.where } f

let get_where () : Where.t t =
  let open Syntax in
  let+ { where; _ } = get_state () in
  where

let error ?trace e : ('a, Error.with_trace, 'f) Result.t =
  let open Syntax in
  let+ where = get_where () in
  let where = Option.fold trace ~some:Where.set_op ~none:Fun.id where in
  Error.log_at where e;
  let e = Error.decorate where e in
  Soteria.Symex.Compo_res.Error e

let with_extra_call_trace ~loc ~msg (f : 'a t) : 'a t =
  let open Syntax in
  let* st = get_state () in
  with_state ~state:{ st with where = Where.add_to_stack ~loc ~msg st.where } f

let rename_trace trace (f : unit -> ('a, Error.with_trace, 'f) Result.t) :
    ('a, Error.with_trace, 'f) Result.t =
  let open Syntax in
  let+- err : Error.with_trace = f () in
  match err with
  | e, { loc; msg = _ } :: stack ->
      let new_elem =
        Soteria.Terminal.Call_trace.mk_element ~loc ~msg:trace ()
      in
      (e, new_elem :: stack)
  | _, [] -> failwith "Impossible: rename_trace on an empty trace?"

let not_impl = give_up
let of_opt_not_impl = some_or_give_up
