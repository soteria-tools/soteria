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

module MonoSymex =
  Soteria.Symex.Make
    (struct
      module Range = struct
        type t = Charon.Meta.span_data

        let file_name_to_yojson (fn : Charon.Meta.file_name) : Yojson.Safe.t =
          match fn with
          | Virtual s -> `List [ `String "Virtual"; `String s ]
          | Local s -> `List [ `String "Local"; `String s ]
          | NotReal s -> `List [ `String "NotReal"; `String s ]

        let to_yojson ({ file; beg_loc; end_loc } : t) : Yojson.Safe.t =
          `Assoc
            [
              ("file", file_name_to_yojson file.name);
              ("beg_loc", `List [ `Int beg_loc.line; `Int beg_loc.col ]);
              ("end_loc", `List [ `Int end_loc.line; `Int end_loc.col ]);
            ]
      end
    end)
    (Bv_solver.Z3_solver)

module TypeMap = Map.Make (struct
  type t = Charon.Types.ty

  let compare = Charon.Types.compare_ty
end)

module MonadState = struct
  type t = {
    subst : Charon.Substitute.subst;
    generic_layouts : Layout_common.t TypeMap.t;
  }

  let empty =
    { subst = Charon.Substitute.empty_subst; generic_layouts = TypeMap.empty }
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
