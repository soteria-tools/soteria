open Soteria.Symex.Compo_res
open Rustsymex
open Rustsymex.Syntax
module BV = Typed.BitVec
open Charon
open Sptr

module Make (State : State_intf.S) = struct
  (* utilities *)
  module Sptr = State.Sptr

  type full_ptr = Sptr.t Rust_val.full_ptr
  type rust_val = Sptr.t Rust_val.t

  let pp_rust_val = Rust_val.pp Sptr.pp
  let pp_full_ptr = Rust_val.pp_full_ptr Sptr.pp

  type ('a, 'env) t =
    'env ->
    State.t ->
    ( 'a * 'env * State.t,
      Error.t State.err * State.t,
      State.serialized )
    Result.t

  let ok x : ('a, 'env) t = fun env state -> Result.ok (x, env, state)
  let error err : ('a, 'env) t = fun _env state -> State.error err state
  let miss f : ('a, 'env) t = fun _env _state -> Result.miss f
  let error_raw err : ('a, 'env) t = fun _env state -> Result.error (err, state)
  let not_impl str : ('a, 'env) t = fun _env _state -> Rustsymex.not_impl str
  let get_env () = fun env state -> Result.ok (env, env, state)

  (* we don't type annotate this to allow for ['env] type changes through [f] *)
  let bind x f =
   fun env state ->
    let** y, env, state = x env state in
    (f y) env state

  let map (x : ('a, 'env) t) (f : 'a -> 'b) : ('b, 'env) t =
   fun env state ->
    let++ y, env, state = x env state in
    (f y, env, state)

  let fold_list x ~init ~f =
    Monad.foldM ~bind ~return:ok ~fold:Foldable.List.fold x ~init ~f

  let fold_iter x ~init ~f =
    Monad.foldM ~bind ~return:ok ~fold:Foldable.Iter.fold x ~init ~f

  let map_env f = fun env state -> Result.ok ((), f env, state)

  let with_env ~(env : 'env1) (f : ('a, 'env1) t) : ('a, 'env) t =
   fun old_env state ->
    let++ res, _, state = f env state in
    (res, old_env, state)

  let[@inline] lift_state_op f =
   fun env state ->
    let++ v, state = f state in
    (v, env, state)

  let[@inline] lift_symex (s : 'a Rustsymex.t) : ('a, 'env) t =
   fun env state ->
    let+ s = s in
    Ok (s, env, state)

  let of_opt_not_impl msg x = lift_symex (of_opt_not_impl msg x)
  let assume x = lift_symex (assume x)

  let with_loc ~loc f =
    let old_loc = !current_loc in
    current_loc := loc;
    map (f ()) @@ fun res ->
    current_loc := old_loc;
    res

  let with_extra_call_trace ~loc ~msg (x : ('a, 'env) t) : ('a, 'env) t =
   fun env state ->
    let+ res = x env state in
    match res with
    | Ok triple -> Ok triple
    | Error (e, st) ->
        let elem = Soteria.Terminal.Call_trace.mk_element ~loc ~msg () in
        Error (State.add_to_call_trace e elem, st)
    | Missing f -> Missing f

  (** Run the state monad, with the given initial state and environment; the
      environment is discarded at the end of the execution. *)
  let run ~env ~state (f : unit -> ('a, 'env) t) :
      ('a * State.t, Error.t State.err, 'f) Result.t =
    let+ res = f () env state in
    match res with
    | Ok (res, _, state) -> Ok (res, state)
    | Error (e, _) -> Error e
    | Missing f -> Missing f

  (** We painfully lift [Layout.update_ref_tys_in] to make it nicer to use
      without having to re-define. *)
  let update_ref_tys_in
      ~(f :
         'acc ->
         full_ptr ->
         Types.ty ->
         Types.ref_kind ->
         (full_ptr * 'acc, 'env) t) ~(init : 'acc) (v : rust_val)
      (ty : Types.ty) : (rust_val * 'acc, 'env) t =
   fun env state ->
    let f (acc, env, state) ptr ty rk =
      let++ (res, acc), env, state = f acc ptr ty rk env state in
      (res, (acc, env, state))
    in
    let++ res, (acc, env, state) =
      Layout.update_ref_tys_in f (init, env, state) v ty
    in
    ((res, acc), env, state)

  module State = struct
    include State

    let[@inline] load ?is_move ptr ty = lift_state_op (load ?is_move ptr ty)

    let[@inline] load_discriminant ptr ty =
      lift_state_op (load_discriminant ptr ty)

    let[@inline] store ptr ty v = lift_state_op (store ptr ty v)
    let[@inline] zeros ptr size = lift_state_op (zeros ptr size)

    let[@inline] alloc_ty ?kind ?span ty =
      lift_state_op (alloc_ty ?kind ?span ty)

    let[@inline] alloc_tys ?kind ?span tys =
      lift_state_op (alloc_tys ?kind ?span tys)

    let[@inline] alloc_untyped ?kind ?span ~zeroed ~size ~align () =
      lift_state_op (alloc_untyped ?kind ?span ~zeroed ~size ~align)

    let[@inline] copy_nonoverlapping ~src ~dst ~size =
      lift_state_op (copy_nonoverlapping ~src ~dst ~size)

    let[@inline] uninit ptr ty = lift_state_op (uninit ptr ty)
    let[@inline] free ptr = lift_state_op (free ptr)
    let[@inline] check_ptr_align ptr ty = lift_state_op (check_ptr_align ptr ty)
    let[@inline] borrow ptr ty mut = lift_state_op (borrow ptr ty mut)
    let[@inline] protect ptr ty mut = lift_state_op (protect ptr ty mut)
    let[@inline] unprotect ptr ty = lift_state_op (unprotect ptr ty)
    let[@inline] with_exposed addr = lift_state_op (with_exposed addr)
    let[@inline] tb_load ptr ty = lift_state_op (tb_load ptr ty)
    let[@inline] load_global g = lift_state_op (load_global g)
    let[@inline] store_global g ptr = lift_state_op (store_global g ptr)
    let[@inline] load_str_global str = lift_state_op (load_str_global str)

    let[@inline] store_str_global str ptr =
      lift_state_op (store_str_global str ptr)

    let[@inline] declare_fn fn = lift_state_op (declare_fn fn)
    let[@inline] lookup_fn fn = lift_state_op (lookup_fn fn)
    let[@inline] add_error e = lift_state_op (add_error e)
    let[@inline] pop_error () = lift_state_op pop_error
    let[@inline] leak_check () = lift_state_op leak_check

    let[@inline] unwind_with ~f ~fe x =
     fun env state ->
      unwind_with
        ~f:(fun (x, env, state) -> f x env state)
        ~fe:(fun (e, state) -> fe e env state)
        (x env state)

    let[@inline] is_valid_ptr_fn =
     fun env state -> Result.ok (is_valid_ptr state, env, state)

    let[@inline] is_valid_ptr ptr ty =
     fun env state ->
      let+ is_valid = is_valid_ptr state ptr ty in
      Ok (is_valid, env, state)

    let[@inline] lift_err sym =
     fun env state ->
      let++ res = lift_err state sym in
      (res, env, state)

    let[@inline] with_decay_map_res (f : ('a, 'e, 'f) DecayMapMonad.Result.t) =
     fun env state ->
      let* res, state = with_decay_map f state in
      lift_err (return res) env state

    let[@inline] with_decay_map f =
     fun env state ->
      let+ res, state = with_decay_map f state in
      Ok (res, env, state)

    let[@inline] assert_ guard err =
     fun env state ->
      let++ () = assert_ (guard :> Typed.T.sbool Typed.t) err state in
      ((), env, state)

    let[@inline] assert_not guard err = assert_ (Typed.not guard) err
  end

  module Syntax = struct
    let ( let* ) = bind
    let ( let+ ) = map
    let ( let^ ) x f = bind (lift_symex x) f
    let ( let^+ ) x f = map (lift_symex x) f
    let ( let^^ ) x f = bind (State.lift_err x) f
    let ( let^^+ ) x f = map (State.lift_err x) f
    let ( let$ ) x f = bind (State.with_decay_map x) f
    let ( let$+ ) x f = map (State.with_decay_map x) f
    let ( let$$ ) x f = bind (State.with_decay_map_res x) f
    let ( let$$+ ) x f = map (State.with_decay_map_res x) f

    module Symex_syntax = struct
      let branch_on ?left_branch_name ?right_branch_name guard ~then_ ~else_ =
       fun env state ->
        Rustsymex.branch_on ?left_branch_name ?right_branch_name guard
          ~then_:(fun () -> then_ () env state)
          ~else_:(fun () -> else_ () env state)

      let branch_on_take_one ?left_branch_name ?right_branch_name guard ~then_
          ~else_ =
       fun env state ->
        Rustsymex.branch_on_take_one ?left_branch_name ?right_branch_name guard
          ~then_:(fun () -> then_ () env state)
          ~else_:(fun () -> else_ () env state)

      let if_sure ?left_branch_name ?right_branch_name guard ~then_ ~else_ =
       fun env state ->
        Rustsymex.if_sure ?left_branch_name ?right_branch_name guard
          ~then_:(fun () -> then_ () env state)
          ~else_:(fun () -> else_ () env state)
    end
  end
end
