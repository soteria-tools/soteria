module MonoSymex =
  Soteria.Symex.Make
    (struct
      module Range = struct
        type t = Charon.Meta.span_data

        let to_yojson _ = `Null
        let of_yojson _ = Ok Charon_util.empty_span_data
      end
    end)
    (Bv_solver.Z3_solver)

include
  Soteria.Sym_states.State_monad.Make
    (MonoSymex)
    (struct
      type t = Charon.Substitute.subst
    end)

include Syntaxes.FunctionWrap

let run_with_stats ?fuel ~mode symex =
  run_with_state ~state:Charon.Substitute.empty_subst symex
  |> (Fun.flip MonoSymex.map) fst
  |> MonoSymex.run_with_stats ?fuel ~mode

module Poly = struct
  open Charon
  open Substitute
  open Syntax

  let push_generics ~params ~args (x : 'a t) : 'a t =
    let* subst = get_state () in
    let args' = generic_args_substitute subst args in
    L.debug (fun m -> m "Pushing generics %a" Crate.pp_generic_args args');
    let subst =
      args' |> make_sb_subst_from_generics params |> subst_at_binder_zero
    in
    with_state ~state:subst x

  let subst f x =
    let+ subst = get_state () in
    f subst x

  let subst_ty = subst ty_substitute
  let subst_tys = subst (fun subst -> List.map (ty_substitute subst))
  let subst_tref = subst trait_ref_substitute

  let fill_params params =
    let args = bound_identity_args params in
    let+ subst = get_state () in
    generic_args_substitute subst args
end

let match_on (elements : 'a list) ~(constr : 'a -> Typed.sbool Typed.t) :
    'a option t =
  let open Syntax in
  let rec aux = function
    | e :: rest -> if%sat constr e then return (Some e) else aux rest
    | [] -> return None
  in
  aux elements

let current_loc = ref Charon_util.empty_span_data
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
