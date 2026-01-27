open Soteria.Symex.Compo_res
open Rustsymex
open Rustsymex.Syntax
open Charon

module type S = sig
  type serialized
  type st
  type ('a, 'env) t
  type ('a, 'env) monad := ('a, 'env) t
  type 'e err

  val ok : 'a -> ('a, 'env) t
  val error : Error.t -> ('a, 'env) t
  val error_raw : Error.t err -> ('a, 'env) t
  val miss : serialized list -> ('a, 'env) t
  val not_impl : string -> ('a, 'env) t
  val bind : ('a, 'env) t -> ('a -> ('b, 'env) t) -> ('b, 'env) t
  val map : ('a, 'env) t -> ('a -> 'b) -> ('b, 'env) t

  val fold_list :
    'a list -> init:'b -> f:('b -> 'a -> ('b, 'env) t) -> ('b, 'env) t

  val fold_iter :
    'a Foldable.Iter.t ->
    init:'b ->
    f:('b -> 'a -> ('b, 'env) t) ->
    ('b, 'env) t

  val get_state : unit -> (st, 'env) t
  val get_env : unit -> ('env, 'env) t
  val map_env : ('env -> 'env) -> (unit, 'env) t
  val with_env : env:'env1 -> ('a, 'env1) t -> ('a, 'env) t
  val of_opt_not_impl : string -> 'a option -> ('a, 'env) t
  val assume : Typed.T.sbool Typed.t list -> (unit, 'env) t
  val with_loc : loc:Meta.span_data -> (unit -> ('a, 'env) t) -> ('a, 'env) t

  val with_extra_call_trace :
    loc:Meta.span_data -> msg:string -> ('a, 'env) t -> ('a, 'env) t

  val run :
    env:'env ->
    state:st ->
    (unit -> ('a, 'env) t) ->
    ('a * st, Error.t err, serialized) Result.t

  val lift_symex : 'a Rustsymex.t -> ('a, 'env) t

  module Poly : sig
    val push_generics :
      params:Types.generic_params ->
      args:Types.generic_args ->
      ('a, 'env) t ->
      ('a, 'env) t

    val fill_params : Types.generic_params -> (Types.generic_args, 'a) monad
    val subst_ty : Types.ty -> (Types.ty, 'env) t
    val subst_tys : Types.ty list -> (Types.ty list, 'env) t
    val subst_tref : Types.trait_ref -> (Types.trait_ref, 'env) t

    val subst_constant_expr :
      Types.constant_expr -> (Types.constant_expr, 'env) t
  end

  module Sptr : sig
    include Sptr.S

    val offset :
      ?check:bool ->
      ?ty:Charon.Types.ty ->
      signed:bool ->
      t ->
      [< Typed.T.sint ] Typed.t ->
      (t, 'env) monad

    val project :
      Types.ty ->
      Expressions.field_proj_kind ->
      Types.field_id ->
      t ->
      (t, 'env) monad

    val distance : t -> t -> (Typed.T.sint Typed.t, 'env) monad
    val decay : t -> (Typed.T.sint Typed.t, 'env) monad
    val expose : t -> (Typed.T.sint Typed.t, 'env) monad
  end

  type full_ptr = Sptr.t Rust_val.full_ptr
  type rust_val = Sptr.t Rust_val.t

  val pp_rust_val : Format.formatter -> rust_val -> unit
  val pp_full_ptr : Format.formatter -> full_ptr -> unit

  module Layout : sig
    include module type of Layout

    val layout_of : Types.ty -> (Layout.t, 'env) monad
    val size_of : Types.ty -> ([> Typed.T.sint ] Typed.t, 'env) monad
    val align_of : Types.ty -> ([> Typed.T.nonzero ] Typed.t, 'env) monad

    val is_abi_compatible :
      Types.ty -> Types.ty -> ([> Typed.T.sbool ] Typed.t, 'env) monad
  end

  module Encoder : sig
    val encode :
      offset:Typed.T.sint Typed.t ->
      rust_val ->
      Types.ty ->
      ((rust_val * Typed.T.sint Typed.t) Iter.t, 'env) monad

    val cast_literal :
      from_ty:Values.literal_type ->
      to_ty:Values.literal_type ->
      [< Typed.T.cval ] Typed.t ->
      (rust_val, 'env) monad

    val nondet : Types.ty -> (rust_val, 'env) monad
    val apply_attributes : rust_val -> Meta.attribute list -> (unit, 'env) monad

    val ref_tys_in :
      ?include_ptrs:bool -> rust_val -> Types.ty -> (full_ptr * Types.ty) list

    val update_ref_tys_in :
      f:
        ('acc ->
        full_ptr ->
        Types.ty ->
        Types.ref_kind ->
        (full_ptr * 'acc, 'env) monad) ->
      init:'acc ->
      rust_val ->
      Types.ty ->
      (rust_val * 'acc, 'env) monad
  end

  module State : sig
    val empty : st
    val load : ?ignore_borrow:bool -> full_ptr -> Types.ty -> (rust_val, 'env) t
    val load_discriminant : full_ptr -> Types.ty -> (Types.variant_id, 'env) t
    val store : full_ptr -> Types.ty -> rust_val -> (unit, 'env) t
    val zeros : full_ptr -> Typed.T.sint Typed.t -> (unit, 'env) t

    val alloc_ty :
      ?kind:Alloc_kind.t ->
      ?span:Meta.span_data ->
      Types.ty ->
      (full_ptr, 'env) t

    val alloc_tys :
      ?kind:Alloc_kind.t ->
      ?span:Meta.span_data ->
      Types.ty list ->
      (full_ptr list, 'env) t

    val alloc_untyped :
      ?kind:Alloc_kind.t ->
      ?span:Meta.span_data ->
      zeroed:bool ->
      size:Typed.T.sint Typed.t ->
      align:Typed.T.nonzero Typed.t ->
      unit ->
      (full_ptr, 'env) t

    val copy_nonoverlapping :
      src:full_ptr ->
      dst:full_ptr ->
      size:Typed.T.sint Typed.t ->
      (unit, 'env) t

    val uninit : full_ptr -> Types.ty -> (unit, 'env) t
    val free : full_ptr -> (unit, 'env) t
    val check_ptr_align : full_ptr -> Types.ty -> (unit, 'env) t

    val borrow :
      full_ptr -> Types.ty -> Expressions.borrow_kind -> (full_ptr, 'env) t

    val protect : full_ptr -> Types.ty -> Types.ref_kind -> (full_ptr, 'env) t
    val unprotect : full_ptr -> Types.ty -> (unit, 'env) t
    val with_exposed : [< Typed.T.sint ] Typed.t -> (full_ptr, 'env) t
    val tb_load : full_ptr -> Types.ty -> (unit, 'env) t
    val load_global : Types.global_decl_id -> (full_ptr option, 'env) t
    val store_global : Types.global_decl_id -> full_ptr -> (unit, 'env) t
    val load_str_global : string -> (full_ptr option, 'env) t
    val store_str_global : string -> full_ptr -> (unit, 'env) t
    val declare_fn : Fun_kind.t -> (full_ptr, 'env) t
    val lookup_fn : full_ptr -> (Fun_kind.t, 'env) t

    val lookup_const_generic :
      Types.const_generic_var_id -> Types.ty -> (rust_val, 'env) t

    val register_thread_exit : (unit -> (unit, unit) t) -> (unit, 'env) t
    val run_thread_exits : unit -> (unit, 'env) t
    val add_error : Error.t err -> (unit, 'env) t
    val pop_error : unit -> ('a, 'env) t
    val leak_check : unit -> (unit, 'env) t

    val unwind_with :
      f:('a -> ('b, 'env) t) ->
      fe:(Error.t err -> ('b, 'env) t) ->
      ('a, 'env) t ->
      ('b, 'env) t

    val fake_read : full_ptr -> Types.ty -> (unit, 'env) t
    val assert_ : [< Typed.T.sbool ] Typed.t -> Error.t -> (unit, 'env) t
    val assert_not : [< Typed.T.sbool ] Typed.t -> Error.t -> (unit, 'env) t
    val lift_err : ('a, Error.t, serialized) Result.t -> ('a, 'env) t
  end

  module Syntax : sig
    val ( let* ) : ('a, 'env) t -> ('a -> ('b, 'env) t) -> ('b, 'env) t
    val ( let+ ) : ('a, 'env) t -> ('a -> 'b) -> ('b, 'env) t
    val ( let^ ) : 'a Rustsymex.t -> ('a -> ('b, 'env) t) -> ('b, 'env) t
    val ( let^+ ) : 'a Rustsymex.t -> ('a -> 'b) -> ('b, 'env) t

    module Symex_syntax : sig
      val branch_on :
        ?left_branch_name:string ->
        ?right_branch_name:string ->
        Typed.T.sbool Typed.t ->
        then_:(unit -> ('a, 'env) t) ->
        else_:(unit -> ('a, 'env) t) ->
        ('a, 'env) t

      val branch_on_take_one :
        ?left_branch_name:string ->
        ?right_branch_name:string ->
        Typed.T.sbool Typed.t ->
        then_:(unit -> ('a, 'env) t) ->
        else_:(unit -> ('a, 'env) t) ->
        ('a, 'env) t

      val if_sure :
        ?left_branch_name:string ->
        ?right_branch_name:string ->
        Typed.T.sbool Typed.t ->
        then_:(unit -> ('a, 'env) t) ->
        else_:(unit -> ('a, 'env) t) ->
        ('a, 'env) t
    end
  end
end

module Make (State : State_intf.S) :
  S
    with type st = State.t
     and type 'e err = 'e State.err
     and type serialized = State.serialized = struct
  (* utilities *)

  type st = State.t
  type 'e err = 'e State.err
  type serialized = State.serialized
  type full_ptr = State.Sptr.t Rust_val.full_ptr
  type rust_val = State.Sptr.t Rust_val.t

  let pp_rust_val = Rust_val.pp State.Sptr.pp
  let pp_full_ptr = Rust_val.pp_full_ptr State.Sptr.pp

  type ('a, 'env) t =
    'env ->
    State.t ->
    ( 'a * 'env * State.t,
      Error.t State.err * State.t,
      State.serialized )
    Result.t

  type ('a, 'env) monad = ('a, 'env) t

  let ok x : ('a, 'env) t = fun env state -> Result.ok (x, env, state)
  let error err : ('a, 'env) t = fun _env state -> State.error err state
  let error_raw err : ('a, 'env) t = fun _env state -> Result.error (err, state)
  let miss f : ('a, 'env) t = fun _env _state -> Result.miss f
  let not_impl str : ('a, 'env) t = fun _env _state -> Rustsymex.not_impl str
  let get_state () = fun env state -> Result.ok (state, env, state)
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

  let[@inline] lift_err sym =
   fun env state ->
    let++ res = State.lift_err state sym in
    (res, env, state)

  let[@inline] lift_state_op f =
   fun env state ->
    let++ v, state = f state in
    (v, env, state)

  let[@inline] with_decay_map_res f =
   fun env state ->
    let* res, state = State.with_decay_map f state in
    lift_err (return res) env state

  let[@inline] with_decay_map f =
   fun env state ->
    let+ res, state = State.with_decay_map f state in
    Ok (res, env, state)

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

  module Poly = struct
    let[@inline] push_generics ~params ~args (x : ('a, 'env) t) =
     fun env state -> Poly.push_generics ~params ~args (x env state)

    let[@inline] fill_params params = lift_symex (Poly.fill_params params)
    let[@inline] subst_ty ty = lift_symex (Poly.subst_ty ty)
    let[@inline] subst_tys tys = lift_symex (Poly.subst_tys tys)
    let[@inline] subst_tref tref = lift_symex (Poly.subst_tref tref)
    let[@inline] subst_constant_expr c = lift_symex (Poly.subst_constant_expr c)
  end

  module Sptr = struct
    include State.Sptr

    let[@inline] offset ?check ?ty ~signed ptr off =
      lift_err (offset ?check ?ty ~signed ptr off)

    let[@inline] project ty proj_kind field_id ptr =
      lift_err (project ty proj_kind field_id ptr)

    let[@inline] distance ptr1 ptr2 = with_decay_map (distance ptr1 ptr2)
    let[@inline] decay ptr = with_decay_map (decay ptr)
    let[@inline] expose ptr = with_decay_map (expose ptr)
  end

  module Layout = struct
    include Layout

    let[@inline] layout_of ty = lift_err (layout_of ty)
    let[@inline] size_of ty = lift_err (size_of ty)
    let[@inline] align_of ty = lift_err (align_of ty)

    let[@inline] is_abi_compatible ty1 ty2 =
      lift_err (is_abi_compatible ty1 ty2)
  end

  module Encoder = struct
    include Encoder.Make (State.Sptr)

    let[@inline] encode ~offset v ty = lift_err (encode ~offset v ty)

    let[@inline] cast_literal ~from_ty ~to_ty cval =
      with_decay_map_res (cast_literal ~from_ty ~to_ty cval)

    let[@inline] nondet ty = lift_err (nondet ty)
    let[@inline] apply_attributes v attrs = lift_err (apply_attributes v attrs)

    (* We painfully lift [Layout.update_ref_tys_in] to make it nicer to use
        without having to re-define. *)
    let update_ref_tys_in
        ~(f :
           'acc ->
           full_ptr ->
           Types.ty ->
           Types.ref_kind ->
           (full_ptr * 'acc, 'env) monad) ~(init : 'acc) (v : rust_val)
        (ty : Types.ty) : (rust_val * 'acc, 'env) monad =
     fun env state ->
      let f (acc, env, state) ptr ty rk =
        let++ (res, acc), env, state = f acc ptr ty rk env state in
        (res, (acc, env, state))
      in
      let++ res, (acc, env, state) =
        update_ref_tys_in f (init, env, state) v ty
      in
      ((res, acc), env, state)
  end

  module State = struct
    open State

    let empty = State.empty

    let[@inline] load ?ignore_borrow ptr ty =
      lift_state_op (load ?ignore_borrow ptr ty)

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

    let[@inline] lookup_const_generic id ty =
      lift_state_op (lookup_const_generic id ty)

    let[@inline] add_error e = lift_state_op (add_error e)
    let[@inline] pop_error () = lift_state_op pop_error
    let[@inline] leak_check () = lift_state_op leak_check

    let[@inline] register_thread_exit (f : unit -> (unit, unit) monad) =
      let unlifted () st =
        let++ (), (), st = f () () st in
        ((), st)
      in
      lift_state_op (register_thread_exit unlifted)

    let[@inline] run_thread_exits () = lift_state_op run_thread_exits

    let[@inline] unwind_with ~f ~fe x =
     fun env state ->
      unwind_with
        ~f:(fun (x, env, state) -> f x env state)
        ~fe:(fun (e, state) -> fe e env state)
        (x env state)

    let[@inline] fake_read ptr ty =
     fun env state ->
      let* is_valid, state = fake_read ptr ty state in
      match is_valid with
      | None -> ok () env state
      | Some err -> error err state

    let[@inline] lift_err sym =
     fun env state ->
      let++ res = lift_err state sym in
      (res, env, state)

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
