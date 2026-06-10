open Compo_res
open Rustsymex
open Charon
open Common

module Compo_resT2 (I : sig
  type fix
  type error
end) (M : sig
  type ('a, 'b) t

  val return : 'a -> ('a, 'b) t
  val bind : ('a -> ('c, 'b) t) -> ('a, 'b) t -> ('c, 'b) t
  val map : ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
end) =
struct
  open I

  type ('a, 'b) t = (('a, error, fix) Compo_res.t, 'b) M.t

  let ok x = M.return (Ok x)
  let error e = M.return (Error e)
  let miss f = M.return (Missing f)

  let bind f =
    M.bind @@ function
    | Ok v -> f v
    | Error e -> M.return (Error e)
    | Missing f' -> M.return (Missing f')

  let bind2 f fe =
    M.bind @@ function
    | Ok v -> f v
    | Error e -> fe e
    | Missing f' -> M.return (Missing f')

  let map (f : 'a -> 'c) : ('a, 'b) t -> ('c, 'b) t = M.map @@ Compo_res.map f
  let lift (x : ('a, 'b) M.t) : ('a, 'b) t = M.map Compo_res.ok x
end

module type S = sig
  type syn
  type st
  type ('a, 'env) t
  type ('a, 'env) monad := ('a, 'env) t

  val ok : 'a -> ('a, 'env) t
  val error : Error.t -> ('a, 'env) t
  val error_raw : Error.with_trace -> ('a, 'env) t
  val assert_ : [< Typed.T.sbool ] Typed.t -> Error.t -> (unit, 'env) t
  val assert_not : [< Typed.T.sbool ] Typed.t -> Error.t -> (unit, 'env) t
  val miss : syn list list -> ('a, 'env) t
  val vanish : unit -> ('a, 'env) t
  val not_impl : string -> ('a, 'env) t
  val bind : ('a -> ('b, 'env) t) -> ('a, 'env) t -> ('b, 'env) t
  val map : ('a -> 'b) -> ('a, 'env) t -> ('b, 'env) t

  include Monad.Extension2 with type ('a, 'env) t := ('a, 'env) t

  val get_state : unit -> (st, 'env) t
  val get_env : unit -> ('env, 'env) t
  val set_env : 'env -> (unit, 'env) t
  val map_env : ('env -> 'env) -> (unit, 'env) t
  val with_env : env:'env1 -> ('a, 'env1) t -> ('a, 'env) t
  val of_opt_not_impl : string -> 'a option -> ('a, 'env) t
  val assume : Typed.T.sbool Typed.t list -> (unit, 'env) t
  val with_loc : loc:Meta.span_data -> (unit -> ('a, 'env) t) -> ('a, 'env) t

  val with_alloc_kind :
    kind:Alloc_kind.t -> (unit -> ('a, 'env) t) -> ('a, 'env) t

  val get_trace : unit -> (Trace.t, 'env) t

  val with_extra_call_trace :
    ?name:Types.name ->
    loc:Meta.span_data ->
    msg:string ->
    ('a, 'env) t ->
    ('a, 'env) t

  val with_frame : string -> (unit -> ('a, 'env) t) -> ('a, 'env) t

  val unwind_with :
    f:('a -> ('b, 'env) t) ->
    fe:(Error.with_trace -> ('b, 'env) t) ->
    ('a, 'env) t ->
    ('b, 'env) t

  val run :
    env:'env ->
    state:st ->
    (unit -> ('a, 'env) t) ->
    ('a * st, Error.with_trace * st, syn list) Compo_res.t Rustsymex.t

  val lift_symex : 'a Rustsymex.t -> ('a, 'env) t

  module Poly : sig
    val push_generics :
      params:Types.generic_params ->
      args:Types.generic_args ->
      (Types.generic_args -> ('a, 'env) t) ->
      ('a, 'env) t

    val fill_params : Types.generic_params -> (Types.generic_args, 'a) monad
    val subst_ty : Types.ty -> (Types.ty, 'env) t
    val subst_tys : Types.ty list -> (Types.ty list, 'env) t
    val subst_tref : Types.trait_ref -> (Types.trait_ref, 'env) t
    val subst_generic_args : Types.generic_args -> (Types.generic_args, 'env) t

    val subst_constant_expr :
      Types.constant_expr -> (Types.constant_expr, 'env) t
  end

  module Sptr : sig
    include Sptr.S

    val offset :
      ?check:bool ->
      ?ty:Charon.Types.ty ->
      signed:bool ->
      [< Typed.T.sint ] Typed.t ->
      t ->
      (t, 'env) monad

    val check_aligned : t Rust_val.full_ptr -> Types.ty -> (unit, 'env) monad

    val check_non_dangling :
      t Rust_val.full_ptr -> Types.ty -> (unit, 'env) monad

    val check_non_dangling_untyped :
      t Rust_val.full_ptr -> Typed.(T.sint t) -> (unit, 'env) monad

    val distance : t -> t -> (Typed.T.sint Typed.t, 'env) monad
    val decay : t -> (Typed.T.sint Typed.t, 'env) monad
    val expose : t -> (Typed.T.sint Typed.t, 'env) monad
    val dangling_if_zst : Types.ty -> (t Rust_val.full_ptr option, 'env) monad
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
      rust_val

    val nondet_valid : Types.ty -> (rust_val, 'env) monad

    val ref_tys_in :
      f:('acc -> Types.ty -> full_ptr -> (full_ptr * 'acc, 'env) monad) ->
      init:'acc ->
      Types.ty ->
      rust_val ->
      (rust_val * 'acc, 'env) monad
  end

  module State : sig
    val empty : st
    val load : ?ignore_borrow:bool -> full_ptr -> Types.ty -> (rust_val, 'env) t
    val load_discriminant : full_ptr -> Types.ty -> (Types.variant_id, 'env) t
    val store : full_ptr -> Types.ty -> rust_val -> (unit, 'env) t
    val zeros : full_ptr -> Typed.T.sint Typed.t -> (unit, 'env) t
    val alloc_ty : ?span:Meta.span_data -> Types.ty -> (full_ptr, 'env) t

    val alloc_tys :
      ?span:Meta.span_data -> Types.ty list -> (full_ptr list, 'env) t

    val alloc_untyped :
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

    val transmute :
      from:Types.ty -> to_:Types.ty -> rust_val -> (rust_val, 'env) t

    val uninit : full_ptr -> Types.ty -> (unit, 'env) t
    val free : full_ptr -> (unit, 'env) t
    val borrow : ?protect:bool -> full_ptr -> Types.ty -> (full_ptr, 'env) t
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

    val size_and_align_of_val :
      Types.ty ->
      Sptr.t Rust_val.meta ->
      (Typed.T.sint Typed.t * Typed.T.nonzero Typed.t, 'env) t
  end

  module Syntax : sig
    val ( let* ) : ('a, 'env) t -> ('a -> ('b, 'env) t) -> ('b, 'env) t
    val ( let+ ) : ('a, 'env) t -> ('a -> 'b) -> ('b, 'env) t
    val ( let*^ ) : 'a Rustsymex.t -> ('a -> ('b, 'env) t) -> ('b, 'env) t
    val ( let+^ ) : 'a Rustsymex.t -> ('a -> 'b) -> ('b, 'env) t

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

  module OptionM : sig
    type ('a, 'env) t = ('a option, 'env) monad

    val ok : 'a -> ('a, 'env) t
    val none : unit -> ('a, 'env) t
    val bind : ('a -> ('b, 'env) t) -> ('a, 'env) t -> ('b, 'env) t
    val map : ('a -> 'b) -> ('a, 'env) t -> ('b, 'env) t

    module Syntax : sig
      val ( let** ) : ('a, 'env) t -> ('a -> ('b, 'env) t) -> ('b, 'env) t
      val ( let++ ) : ('a, 'env) t -> ('a -> 'b) -> ('b, 'env) t
    end
  end
end

module Make (State : State_intf.S) :
  S
    with type st = State.t option
     and type syn = State.syn
     and type Sptr.t = State.Sptr.t = struct
  (* utilities *)

  type st = State.t option
  type syn = State.syn
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
            type fix = syn list
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
  let error err : ('a, 'env) t = ESM.lift @@ State.SM.lift @@ error err
  let vanish () _ = State.SM.vanish ()

  let lift_err (sym : ('a, Error.t, 'f) Rustsymex.Result.t) : ('a, 'env) t =
    ESM.lift
    @@ State.SM.lift
         (let open Rustsymex.Syntax in
          let* trace = Rustsymex.get_trace () in
          let+- err = sym in
          Error.log_at trace err;
          Error.decorate trace err)

  let assert_ cond err =
    lift_err (assert_or_error (cond :> Typed.T.sbool Typed.t) err)

  let assert_not cond err = assert_ (Typed.not cond) err
  let miss f : ('a, 'env) t = ESM.Result.miss f

  let not_impl str : ('a, 'env) t =
    ESM.lift @@ State.SM.lift @@ Rustsymex.not_impl str

  let get_state () : (st, 'env) t =
    ESM.Result.lift_state @@ State.SM.get_state ()

  let get_env () : ('env, 'env) t = ESM.Result.lift @@ ESM.get_state ()

  let bind (f : 'a -> ('b, 'env) t) (x : ('a, 'env) t) : ('b, 'env) t =
    ESM.Result.bind f x

  let map (f : 'a -> 'b) (x : ('a, 'env) t) : ('b, 'env) t =
    ESM.map (Compo_res.map f) x

  include Monad.Make_extension2 (struct
    type nonrec ('a, 'env) t = ('a, 'env) t

    let ok = ok
    let bind = bind
    let map = map
  end)

  let map_env f =
    let open ESM.Syntax in
    let+ () = ESM.map_state f in
    Compo_res.Ok ()

  let set_env e =
    let open ESM.Syntax in
    let+ () = ESM.set_state e in
    Compo_res.Ok ()

  let with_env ~(env : 'env1) (f : ('a, 'env1) t) : ('a, 'env) t =
   fun old_env ->
    let open State.SM.Syntax in
    let+ res, _ = f env in
    (res, old_env)

  let[@inline] with_pointers_sym (f : 'a Sptr.DecayMap.SM.t) : ('a, 'env) t =
    ESM.lift @@ State.SM.map Compo_res.ok @@ State.with_pointers_sym f

  let[@inline] lift_symex (s : 'a Rustsymex.t) : ('a, 'env) t =
    ESM.Result.lift_state @@ State.SM.lift s

  let of_opt_not_impl msg x = lift_symex (of_opt_not_impl msg x)
  let assume x = lift_symex (assume x)

  let with_loc ~loc (f : unit -> ('a, 'env) t) : ('a, 'env) t =
   fun env state -> with_loc ~loc (f () env state)

  let with_alloc_kind ~kind (f : unit -> ('a, 'env) t) : ('a, 'env) t =
   fun env state -> with_alloc_kind kind (f () env state)

  let get_trace () : (Trace.t, 'env) t = lift_symex @@ Rustsymex.get_trace ()

  let with_extra_call_trace ?name ~loc ~msg (x : ('a, 'env) t) : ('a, 'env) t =
   fun env state ->
    Rustsymex.with_extra_call_trace ?name ~loc ~msg (x env state)

  let with_frame name (f : unit -> ('a, 'env) t) : ('a, 'env) t =
   fun env state -> Rustsymex.with_frame name (fun () -> f () env state)

  let[@inline] unwind_with ~f ~fe : ('a, 'env) monad -> ('b, 'env) monad =
    ESM.Result.bind2 f (fun ((err_ty, _) as err) ->
        if Error.is_unwindable err_ty then fe err else error_raw err)

  (** Run the state monad, with the given initial state and environment; the
      environment is discarded at the end of the execution. *)
  let run ~env ~state (f : unit -> ('a, 'env) t) =
    let open Rustsymex.Syntax in
    let+ (res, _env), state = f () env state in
    match res with
    | Ok v -> Ok (v, state)
    | Error e -> Error (e, state)
    | Missing f -> Missing f

  module Syntax = struct
    let ( let* ) x f = bind f x
    let ( let+ ) x f = map f x
    let ( let*^ ) x f = bind f (lift_symex x)
    let ( let+^ ) x f = map f (lift_symex x)

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

  module Poly = struct
    let[@inline] push_generics ~params ~args
        (x : Types.generic_args -> ('a, 'env) t) : ('a, 'env) t =
     fun env state ->
      Poly.push_generics ~params ~args (fun args -> x args env state)

    let[@inline] fill_params params = lift_symex (Poly.fill_params params)
    let[@inline] subst_ty ty = lift_symex (Poly.subst_ty ty)
    let[@inline] subst_tys tys = lift_symex (Poly.subst_tys tys)
    let[@inline] subst_tref tref = lift_symex (Poly.subst_tref tref)

    let[@inline] subst_generic_args generic_args =
      lift_symex (Poly.subst_generic_args generic_args)

    let[@inline] subst_constant_expr c = lift_symex (Poly.subst_constant_expr c)
  end

  module Layout = struct
    include Layout

    let[@inline] layout_of ty = lift_err (layout_of ty)
    let[@inline] size_of ty = lift_err (size_of ty)
    let[@inline] align_of ty = lift_err (align_of ty)

    let[@inline] is_abi_compatible ty1 ty2 =
      lift_err (is_abi_compatible ty1 ty2)
  end

  module Sptr = struct
    include State.Sptr

    let[@inline] offset ?check ?ty ~signed off ptr =
      ESM.lift @@ offset ?check ?ty ~signed off ptr

    let[@inline] check_aligned ptr ty = ESM.lift (check_aligned ptr ty)

    let[@inline] check_non_dangling ptr ty =
      ESM.lift (check_non_dangling ptr ty)

    let[@inline] check_non_dangling_untyped ptr size =
      ESM.lift (check_non_dangling_untyped ptr size)

    let[@inline] distance ptr1 ptr2 = with_pointers_sym (distance ptr1 ptr2)
    let[@inline] decay ptr = with_pointers_sym (decay ptr)
    let[@inline] expose ptr = with_pointers_sym (expose ptr)

    (** If [ty] is a ZST, returns [Some ptr] where [ptr] is a valid pointer to
        such a ty; otherwise, return [None] *)
    let dangling_if_zst ty =
      let open Syntax in
      let open Typed.Syntax in
      let open Typed.Infix in
      let* layout = Layout.layout_of ty in
      if%sat layout.size ==@ Usize.(0s) then
        let ptr =
          (of_address (layout.align :> Typed.T.sint Typed.t), Rust_val.Thin)
        in
        ok (Some ptr)
      else ok None
  end

  module Encoder = struct
    include Value_codec.Encoder (State.Sptr)

    let[@inline] encode ~offset v ty = lift_err (encode ~offset v ty)
    let[@inline] nondet_valid ty = lift_err (nondet_valid ty)

    (* We painfully lift [Layout.ref_tys_in] to make it nicer to use without
       having to re-define. *)
    let ref_tys_in
        ~(f : 'acc -> Types.ty -> full_ptr -> (full_ptr * 'acc, 'env) monad)
        ~(init : 'acc) (ty : Types.ty) (v : rust_val) :
        (rust_val * 'acc, 'env) monad =
     fun env state ->
      let open Rustsymex.Syntax in
      (* The inner function operates in Rustsymex.Result.t, carrying (acc, env,
         state) as accumulator *)
      let f_inner (acc, env, state) ty ptr =
        let+ (res, new_env), new_state = f acc ty ptr env state in
        Compo_res.map (fun (ptr, acc) -> (ptr, (acc, new_env, new_state))) res
      in
      let+ res = ref_tys_in f_inner (init, env, state) ty v in
      match res with
      | Ok (v, (acc, env, state)) -> ((Ok (v, acc), env), state)
      | Error e -> ((Error e, env), state)
      | Missing fixes -> ((Missing fixes, env), state)
  end

  module State = struct
    open State

    let empty = State.empty

    let[@inline] load ?ignore_borrow ptr ty =
      ESM.lift (load ?ignore_borrow ptr ty)

    let[@inline] load_discriminant ptr ty = ESM.lift (load_discriminant ptr ty)
    let[@inline] store ptr ty v = ESM.lift (store ptr ty v)
    let[@inline] zeros ptr size = ESM.lift (zeros ptr size)
    let[@inline] alloc_ty ?span ty = ESM.lift (alloc_ty ?span ty)
    let[@inline] alloc_tys ?span tys = ESM.lift (alloc_tys ?span tys)

    let[@inline] alloc_untyped ?span ~zeroed ~size ~align () =
      ESM.lift (alloc_untyped ?span ~zeroed ~size ~align)

    let[@inline] copy_nonoverlapping ~src ~dst ~size =
      ESM.lift (copy_nonoverlapping ~src ~dst ~size)

    let[@inline] transmute ~from ~to_ v = ESM.lift (transmute ~from ~to_ v)
    let[@inline] uninit ptr ty = ESM.lift (uninit ptr ty)
    let[@inline] free ptr = ESM.lift (free ptr)
    let[@inline] borrow ?protect ptr ty = ESM.lift (borrow ?protect ptr ty)
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
    let[@inline] pop_error () : ('a, 'env) monad = ESM.lift (pop_error ())
    let[@inline] leak_check () = ESM.lift (leak_check ())

    let[@inline] register_thread_exit (f : unit -> (unit, unit) monad) =
      let unlifted () : (unit, Error.with_trace, syn list) SM.Result.t =
        SM.map fst (f () ())
      in
      ESM.lift (register_thread_exit unlifted)

    let[@inline] run_thread_exits () = ESM.lift (run_thread_exits ())
    let[@inline] fake_read ptr ty = ESM.lift (fake_read ptr ty)

    let[@inline] size_and_align_of_val ty meta =
      ESM.lift (size_and_align_of_val ty meta)
  end

  module OptionM = struct
    type nonrec ('a, 'env) t = ('a option, 'env) t

    let bind f x = bind (function Some v -> f v | None -> ok None) x
    let map f x = map (Option.map f) x
    let none () = ok None
    let ok x = ok (Some x)

    module Syntax = struct
      let ( let** ) x f = bind f x
      let ( let++ ) x f = map f x
    end
  end
end
