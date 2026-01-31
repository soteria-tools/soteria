module Compo_res = Soteria.Symex.Compo_res
open Compo_res
open Rustsymex
open Charon

module Compo_resT2 (I : sig
  type fix
  type error
end) (M : sig
  type ('a, 'b) t

  val return : 'a -> ('a, 'b) t
  val bind : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
  val map : ('a, 'b) t -> ('a -> 'c) -> ('c, 'b) t
end) =
struct
  open I

  type ('a, 'b) t = (('a, error, fix) Compo_res.t, 'b) M.t

  let ok x = M.return (Ok x)
  let error e = M.return (Error e)
  let miss f = M.return (Missing f)

  let bind x f =
    M.bind x @@ function
    | Ok v -> f v
    | Error e -> M.return (Error e)
    | Missing f' -> M.return (Missing f')

  let bind2 x f fe =
    M.bind x @@ function
    | Ok v -> f v
    | Error e -> fe e
    | Missing f' -> M.return (Missing f')

  let map (x : ('a, 'b) t) (f : 'a -> 'c) : ('c, 'b) t =
    M.map x @@ Fun.flip Compo_res.map f

  let lift (x : ('a, 'b) M.t) : ('a, 'b) t = M.map x @@ fun v -> Ok v
end

module type S = sig
  type serialized
  type st
  type ('a, 'env) t
  type ('a, 'env) monad := ('a, 'env) t

  val ok : 'a -> ('a, 'env) t
  val error : Error.t -> ('a, 'env) t
  val error_raw : Error.with_trace -> ('a, 'env) t
  val assert_ : [< Typed.T.sbool ] Typed.t -> Error.t -> (unit, 'env) t
  val assert_not : [< Typed.T.sbool ] Typed.t -> Error.t -> (unit, 'env) t
  val miss : serialized list list -> ('a, 'env) t
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

  val iter_list : 'a list -> f:('a -> (unit, 'env) t) -> (unit, 'env) t

  val iter_iter :
    'a Foldable.Iter.t -> f:('a -> (unit, 'env) t) -> (unit, 'env) t

  val map_list : 'a list -> f:('a -> ('b, 'env) t) -> ('b list, 'env) t
  val get_state : unit -> (st, 'env) t
  val get_env : unit -> ('env, 'env) t
  val map_env : ('env -> 'env) -> (unit, 'env) t
  val with_env : env:'env1 -> ('a, 'env1) t -> ('a, 'env) t
  val of_opt_not_impl : string -> 'a option -> ('a, 'env) t
  val assume : Typed.T.sbool Typed.t list -> (unit, 'env) t
  val with_loc : loc:Meta.span_data -> (unit -> ('a, 'env) t) -> ('a, 'env) t
  val get_loc : unit -> (Meta.span_data, 'env) t

  val with_extra_call_trace :
    loc:Meta.span_data -> msg:string -> ('a, 'env) t -> ('a, 'env) t

  val unwind_with :
    f:('a -> ('b, 'env) t) ->
    fe:(Error.with_trace -> ('b, 'env) t) ->
    ('a, 'env) t ->
    ('b, 'env) t

  val run :
    env:'env ->
    state:st ->
    (unit -> ('a, 'env) t) ->
    ('a * st, Error.with_trace, serialized list) Result.t

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
    val add_error : Error.with_trace -> (unit, 'env) t
    val pop_error : unit -> ('a, 'env) t
    val leak_check : unit -> (unit, 'env) t
    val fake_read : full_ptr -> Types.ty -> (unit, 'env) t
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
  S with type st = State.t option and type serialized = State.serialized =
struct
  (* utilities *)

  type st = State.t option
  type serialized = State.serialized
  type full_ptr = State.Sptr.t Rust_val.full_ptr
  type rust_val = State.Sptr.t Rust_val.t

  let pp_rust_val = Rust_val.pp State.Sptr.pp
  let pp_full_ptr = Rust_val.pp_full_ptr State.Sptr.pp

  module ESM = struct
    module MONAD = Monad.StateT_p (State.SM)
    include MONAD

    module Result = struct
      let l = lift

      include
        Compo_resT2
          (struct
            type fix = serialized list
            type error = Error.with_trace
          end)
          (MONAD)

      let lift_state x = lift @@ l x
    end
  end

  type ('a, 'env) t = ('a, 'env) ESM.Result.t
  type ('a, 'env) monad = ('a, 'env) t

  let ok x : ('a, 'env) t = ESM.Result.ok x
  let error_raw err : ('a, 'env) t = ESM.Result.error err

  let error err : ('a, 'env) t =
    ESM.lift
      (let open State.SM.Syntax in
       let+^ loc = get_loc () in
       Error (decorate_error err loc))

  let lift_err (sym : ('a, Error.t, 'f) Rustsymex.Result.t) : ('a, 'env) t =
    ESM.lift
      (let open State.SM.Syntax in
       let*- err = State.SM.lift sym in
       let+^ loc = get_loc () in
       Compo_res.Error (decorate_error err loc))

  let assert_ cond err =
    lift_err (assert_or_error (cond :> Typed.T.sbool Typed.t) err)

  let assert_not cond err = assert_ (Typed.not cond) err
  let miss f : ('a, 'env) t = ESM.Result.miss f

  let not_impl str : ('a, 'env) t =
    ESM.lift @@ State.SM.lift @@ Rustsymex.not_impl str

  let get_state () : (st, 'env) t =
    ESM.Result.lift_state @@ State.SM.get_state ()

  let get_env () : ('env, 'env) t = ESM.Result.lift @@ ESM.get_state ()

  let bind (x : ('a, 'env) t) (f : 'a -> ('b, 'env) t) : ('b, 'env) t =
    ESM.Result.bind x f

  let map (x : ('a, 'env) t) (f : 'a -> 'b) : ('b, 'env) t =
    ESM.map x (Fun.flip Compo_res.map f)

  let foldM ~fold x ~init ~f = Monad.foldM ~bind ~return:ok ~fold x ~init ~f
  let fold_list x ~init ~f = foldM ~fold:Foldable.List.fold x ~init ~f
  let fold_iter x ~init ~f = foldM ~fold:Foldable.Iter.fold x ~init ~f
  let iterM ~fold x ~f = foldM ~fold x ~init:() ~f:(fun () -> f)
  let iter_list x ~f = iterM ~fold:Foldable.List.fold x ~f
  let iter_iter x ~f = iterM ~fold:Foldable.Iter.fold x ~f

  let mapM ~fold ~rev ~cons ~init x ~f =
    foldM ~fold x ~init ~f:(fun acc a -> map (f a) (fun b -> cons b acc))
    |> Fun.flip map rev

  let map_list x ~f =
    mapM ~init:[] ~fold:Foldable.List.fold ~rev:List.rev ~cons:List.cons x ~f

  let map_env f =
    let open ESM.Syntax in
    let+ () = ESM.map_state f in
    Compo_res.Ok ()

  let with_env ~(env : 'env1) (f : ('a, 'env1) t) : ('a, 'env) t =
   fun old_env ->
    let open State.SM.Syntax in
    let+ res, _ = f env in
    (res, old_env)

  let[@inline] with_decay_map_res
      (f : ('a, Error.t, serialized list) Sptr.DecayMapMonad.Result.t) :
      ('a, 'env) t =
    ESM.lift
      (let open State.SM.Syntax in
       let*- err = State.with_decay_map f in
       let+^ loc = get_loc () in
       Compo_res.Error (decorate_error err loc))

  let[@inline] with_decay_map (f : 'a Sptr.DecayMapMonad.t) : ('a, 'env) t =
    ESM.lift (State.SM.map (State.with_decay_map f) Compo_res.ok)

  let[@inline] lift_symex (s : 'a Rustsymex.t) : ('a, 'env) t =
    ESM.Result.lift_state @@ State.SM.lift s

  let of_opt_not_impl msg x = lift_symex (of_opt_not_impl msg x)
  let assume x = lift_symex (assume x)

  let with_loc ~loc (f : unit -> ('a, 'env) t) : ('a, 'env) t =
   fun env state -> with_loc ~loc (f () env state)

  let get_loc () : (Meta.span_data, 'env) t = lift_symex @@ Rustsymex.get_loc ()

  let with_extra_call_trace ~loc ~msg (x : ('a, 'env) t) : ('a, 'env) t =
    let open ESM.Syntax in
    let+ res = x in
    match res with
    | Ok v -> Compo_res.Ok v
    | Error e ->
        let elem = Soteria.Terminal.Call_trace.mk_element ~loc ~msg () in
        Compo_res.Error (Error.add_to_call_trace e elem)
    | Missing f -> Missing f

  let[@inline] unwind_with ~f ~fe (x : ('a, 'env) monad) : ('b, 'env) monad =
    ESM.Result.bind2 x f (fun ((err_ty, _) as err) ->
        if Error.is_unwindable err_ty then fe err else error_raw err)

  (** Run the state monad, with the given initial state and environment; the
      environment is discarded at the end of the execution. *)
  let run ~env ~state (f : unit -> ('a, 'env) t) :
      ('a * st, Error.with_trace, serialized list) Result.t =
    let open Rustsymex.Syntax in
    let+ (res, _env), st = f () env state in
    match res with
    | Ok v -> Ok (v, st)
    | Error e -> Error e
    | Missing f -> Missing f

  module Poly = struct
    let[@inline] push_generics ~params ~args (x : ('a, 'env) t) : ('a, 'env) t =
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
    include Value_codec.Encoder (State.Sptr)

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
      let open Rustsymex.Syntax in
      (* The inner function operates in Rustsymex.Result.t, carrying (acc, env,
         state) as accumulator *)
      let f_inner (acc, env, state) ptr ty rk =
        let+ (res, new_env), new_state = f acc ptr ty rk env state in
        Compo_res.map res (fun (ptr, acc) -> (ptr, (acc, new_env, new_state)))
      in
      let+ res = update_ref_tys_in f_inner (init, env, state) v ty in
      match res with
      | Compo_res.Ok (v, (acc, env, state)) ->
          ((Compo_res.Ok (v, acc), env), state)
      | Compo_res.Error e -> ((Compo_res.Error e, env), state)
      | Compo_res.Missing fixes -> ((Compo_res.Missing fixes, env), state)
  end

  module State = struct
    open State

    let empty = State.empty

    let[@inline] load ?ignore_borrow ptr ty =
      ESM.lift (load ?ignore_borrow ptr ty)

    let[@inline] load_discriminant ptr ty = ESM.lift (load_discriminant ptr ty)
    let[@inline] store ptr ty v = ESM.lift (store ptr ty v)
    let[@inline] zeros ptr size = ESM.lift (zeros ptr size)
    let[@inline] alloc_ty ?kind ?span ty = ESM.lift (alloc_ty ?kind ?span ty)

    let[@inline] alloc_tys ?kind ?span tys =
      ESM.lift (alloc_tys ?kind ?span tys)

    let[@inline] alloc_untyped ?kind ?span ~zeroed ~size ~align () =
      ESM.lift (alloc_untyped ?kind ?span ~zeroed ~size ~align)

    let[@inline] copy_nonoverlapping ~src ~dst ~size =
      ESM.lift (copy_nonoverlapping ~src ~dst ~size)

    let[@inline] uninit ptr ty = ESM.lift (uninit ptr ty)
    let[@inline] free ptr = ESM.lift (free ptr)
    let[@inline] check_ptr_align ptr ty = ESM.lift (check_ptr_align ptr ty)
    let[@inline] borrow ptr ty mut = ESM.lift (borrow ptr ty mut)
    let[@inline] protect ptr ty mut = ESM.lift (protect ptr ty mut)
    let[@inline] unprotect ptr ty = ESM.lift (unprotect ptr ty)
    let[@inline] with_exposed addr = ESM.lift (with_exposed addr)
    let[@inline] tb_load ptr ty = ESM.lift (tb_load ptr ty)
    let[@inline] load_global g = ESM.lift (load_global g)
    let[@inline] store_global g ptr = ESM.lift (store_global g ptr)
    let[@inline] load_str_global str = ESM.lift (load_str_global str)
    let[@inline] store_str_global str ptr = ESM.lift (store_str_global str ptr)
    let[@inline] declare_fn fn = ESM.lift (declare_fn fn)
    let[@inline] lookup_fn fn = ESM.lift (lookup_fn fn)

    let[@inline] lookup_const_generic id ty =
      ESM.lift (lookup_const_generic id ty)

    let[@inline] add_error e = ESM.lift (add_error e)

    (** pop_error always errors, so the result type 'a is polymorphic. We need
        to coerce the unit result to 'a since it never succeeds. *)
    let[@inline] pop_error () : ('a, 'env) monad =
     fun env ->
      let open SM.Syntax in
      let+ res = pop_error () in
      (res, env)

    let[@inline] leak_check () = ESM.lift (leak_check ())

    let[@inline] register_thread_exit (f : unit -> (unit, unit) monad) =
      let unlifted () : (unit, Error.with_trace, serialized list) SM.Result.t =
        SM.map (f () ()) fst
      in
      ESM.lift (register_thread_exit unlifted)

    let[@inline] run_thread_exits () = ESM.lift (run_thread_exits ())

    let[@inline] fake_read ptr ty : (unit, 'env) monad =
     fun env ->
      let open SM.Syntax in
      let* is_valid = fake_read ptr ty in
      match is_valid with
      | None -> SM.return (Compo_res.Ok (), env)
      | Some err -> error err env
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
