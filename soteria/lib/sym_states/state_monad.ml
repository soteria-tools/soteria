open Soteria_std
module Compo_res = Symex.Compo_res
module Approx = Symex.Approx

module type S = sig
  include Symex.Base
  module Symex : Symex.Base

  type st

  val lift : 'a Symex.t -> 'a t
  val get_state : unit -> st t
  val map_state : (st -> st) -> unit t
  val with_state : state:st -> 'a t -> 'a t
  val run_with_state : state:st -> 'a t -> ('a * st) Symex.t

  module Result : sig
    include module type of Result

    val run_with_state :
      state:st -> ('a, 'e, 'f) t -> ('a * st, 'e, 'f) Symex.Result.t
  end

  module Syntax : sig
    include module type of Syntax

    val ( let*^ ) : 'a Symex.t -> ('a -> 'b t) -> 'b t
    val ( let+^ ) : 'a Symex.t -> ('a -> 'b) -> 'b t

    val ( let**^ ) :
      ('a, 'e, 'f) Symex.Result.t ->
      ('a -> ('b, 'e, 'f) Result.t) ->
      ('b, 'e, 'f) Result.t

    val ( let++^ ) :
      ('a, 'e, 'f) Symex.Result.t -> ('a -> 'b) -> ('b, 'e, 'f) Result.t
  end
end

module Make
    (Sym : Symex.Base)
    (State : sig
      type t
    end) :
  S
    with module Value = Sym.Value
     and module Stats = Sym.Stats
     and module Symex = Sym
     and type st = State.t
     and type 'a t = State.t -> ('a * State.t) Sym.t = struct
  open Sym.Syntax
  module Symex = Sym
  module Value = Sym.Value
  module Stats = Sym.Stats

  let nondet_UNSAFE = Sym.nondet_UNSAFE

  module MONAD = struct
    type 'a t = State.t -> ('a * State.t) Sym.t

    let return x st = Sym.return (x, st)

    let bind m f st =
      let* x, st' = m st in
      f x st'

    let map m f st =
      let+ x, st' = m st in
      (f x, st')
  end

  include MONAD

  type lfail = Sym.lfail [@@deriving show { with_path = false }]
  type cons_fail = Sym.cons_fail [@@deriving show { with_path = false }]
  type st = State.t

  let lift x st =
    let+ x in
    (x, st)

  let get_state () st = Sym.return (st, st)
  let map_state f st = Sym.return ((), f st)

  let with_state ~state x =
   fun st ->
    let+ res, _ = x state in
    (res, st)

  let run_with_state ~state x = x state
  let assume b = lift (Sym.assume b)
  let vanish () = lift (Sym.vanish ())
  let assert_ b = lift (Sym.assert_ b)
  let consume_pure b = lift (Sym.consume_pure b)
  let consume_false () = lift (Sym.consume_false ())
  let nondet ty = lift (Sym.nondet ty)
  let simplify v = lift (Sym.simplify v)
  let fresh_var ty = lift (Sym.fresh_var ty)
  let assert_or_error b err = lift (Sym.assert_or_error b err)
  let branches b st = Sym.branches (List.map (fun f () -> f () st) b)
  let consume_fuel_steps n = lift (Sym.consume_fuel_steps n)
  let give_up ~loc msg = lift (Sym.give_up ~loc msg)
  let some_or_give_up ~loc msg x = lift (Sym.some_or_give_up ~loc msg x)
  let all fn xs = Monad.all fn xs ~return ~bind
  let foldM ~fold x ~init ~f = Monad.foldM ~bind ~return ~fold x ~init ~f
  let fold_list x ~init ~f = foldM ~fold:Foldable.List.fold x ~init ~f
  let fold_iter x ~init ~f = foldM ~fold:Foldable.Iter.fold x ~init ~f
  let fold_seq x ~init ~f = foldM ~fold:Foldable.Seq.fold x ~init ~f

  let branch_on ?left_branch_name ?right_branch_name guard ~then_ ~else_ =
   fun st ->
    Sym.branch_on ?left_branch_name ?right_branch_name guard
      ~then_:(fun () -> then_ () st)
      ~else_:(fun () -> else_ () st)

  let branch_on_take_one ?left_branch_name ?right_branch_name guard ~then_
      ~else_ =
   fun st ->
    Sym.branch_on_take_one ?left_branch_name ?right_branch_name guard
      ~then_:(fun () -> then_ () st)
      ~else_:(fun () -> else_ () st)

  let if_sure ?left_branch_name ?right_branch_name guard ~then_ ~else_ =
   fun st ->
    Sym.if_sure ?left_branch_name ?right_branch_name guard
      ~then_:(fun () -> then_ () st)
      ~else_:(fun () -> else_ () st)

  module Result = struct
    include Compo_res.T (struct
      type nonrec 'a t = 'a t

      let return = return
      let bind = bind
      let map = map
    end)

    let run_with_state ~state x =
      Symex.map (x state) @@ function
      | Compo_res.Ok res, state -> Compo_res.Ok (res, state)
      | Error e, _ -> Error e
      | Missing f, _ -> Missing f

    let miss_no_fix ~reason () = lift (Sym.Result.miss_no_fix ~reason ())
    let foldM ~fold x ~init ~f = Monad.foldM ~bind ~return:ok ~fold x ~init ~f
    let fold_list x ~init ~f = foldM ~fold:Foldable.List.fold x ~init ~f
    let fold_iter x ~init ~f = foldM ~fold:Foldable.Iter.fold x ~init ~f
    let fold_seq x ~init ~f = foldM ~fold:Foldable.Seq.fold x ~init ~f
  end

  module Syntax = struct
    let ( let* ) = bind
    let ( let+ ) = map
    let ( let** ) = Result.bind
    let ( let++ ) = Result.map
    let ( let+- ) = Result.map_error
    let ( let+? ) = Result.map_missing
    let ( let*^ ) x f = bind (lift x) f
    let ( let+^ ) x f = map (lift x) f
    let ( let**^ ) x f = Result.bind (lift x) f
    let ( let++^ ) x f = Result.map (lift x) f

    module Symex_syntax = struct
      let branch_on = branch_on
      let branch_on_take_one = branch_on_take_one
      let if_sure = if_sure
    end
  end

  module Producer = struct
    include
      Soteria_std.Monad.StateT
        (struct
          type t = Value.Syn.Subst.t option
        end)
        (MONAD)

    let vanish () = lift (vanish ())

    let apply_subst (sf : (Value.Syn.t -> 'a Value.t) -> 'syn -> 'sem)
        (e : 'syn) : 'sem t =
     fun s ->
      (* There's maybe a safer version with effects and no reference? *)
      match s with
      | None ->
          let vsf e =
            let v, _ =
              Value.Syn.subst
                ~missing_var:(fun v ty -> Value.mk_var v ty)
                Value.Syn.Subst.empty e
            in
            v
          in
          let res = sf vsf e in
          MONAD.return (res, None)
      | Some s ->
          let s = ref s in
          let vsf e =
            let v, s' =
              Value.Syn.subst ~missing_var:(fun _ ty -> nondet_UNSAFE ty) !s e
            in
            s := s';
            v
          in
          let res = sf vsf e in
          MONAD.return (res, Some !s)

    let produce_pure e : unit t =
      (* FIXME: This does no check that `e` is indeed a boolean. *)
      bind (apply_subst Fun.id e) @@ fun v -> lift (assume [ v ])

    let run_producer ~subst p =
      MONAD.map (p (Some subst)) (fun (x, s) -> (x, Option.get s))

    let run_identity_producer p = MONAD.map (p None) (fun (x, _s) -> x)
  end

  module Consumer = struct
    type 'a symex = 'a t
    type subst = Value.Syn.Subst.t
    type ('a, 'fix) t = subst -> ('a * subst, cons_fail, 'fix) Result.t

    let apply_subst (sf : (Value.Syn.t -> 'a Value.t) -> 'syn -> 'sem)
        (e : 'syn) : ('sem, 'fix) t =
      let exception Missing_subst of Soteria.Symex.Var.t in
      fun s ->
        let vsf e =
          let v, _ =
            Value.Syn.subst
              ~missing_var:(fun x _ -> raise (Missing_subst x))
              s e
          in
          v
        in
        try
          let res = sf vsf e in
          Result.ok (res, s)
        with Missing_subst x -> Result.error (`Missing_subst x)

    let learn_eq syn v : (unit, 'fix) t =
      let open Syntax in
      fun subst ->
        let subst =
          match Value.Syn.learn subst syn v with
          | Some s -> s
          | None ->
              failwith
                "Consumed something that was not yet consumable, this is a \
                 tool bug!"
        in
        let v', subst =
          Value.Syn.subst
            ~missing_var:(fun _ _ ->
              failwith
                "Tool Bug: learned substitution does not cover expression's \
                 free variables.")
            subst syn
        in
        let++ () = consume_pure (Value.sem_eq_untyped v v') in
        ((), subst)

    let expose_subst () : (subst, 'fix) t = fun s -> Result.ok (s, s)

    let lift_res (r : ('a, cons_fail, 'fix) Result.t) : ('a, 'fix) t =
     fun subst -> Result.map r (fun a -> (a, subst))

    let lift_symex (m : 'a symex) : ('a, 'fix) t =
     fun subst -> MONAD.map m (fun a -> Compo_res.ok (a, subst))

    let branches (l : (unit -> ('a, 'fix) t) list) : ('a, 'fix) t =
     fun s -> branches (List.map (fun f () -> f () s) l)

    let ok x = fun subst -> Result.ok (x, subst)
    let lfail v = lift_res (Result.error (`Lfail v))
    let miss fixes = lift_res (Result.miss fixes)
    let miss_no_fix ~reason () = lift_res (Result.miss_no_fix ~reason ())

    let map (m : ('a, 'fix) t) (f : 'a -> 'b) : ('b, 'fix) t =
     fun s -> Result.map (m s) (fun (a, s) -> (f a, s))

    let map_missing (m : ('a, 'fix) t) (f : 'fix -> 'g) : ('a, 'g) t =
     fun s -> Result.map_missing (m s) f

    let bind (m : ('a, 'fix) t) (f : 'a -> ('b, 'fix) t) : ('b, 'fix) t =
     fun s -> Result.bind (m s) (fun (a, s) -> f a s)

    let bind_res (m : ('a, 'fix) t)
        (f : ('a, cons_fail, 'fix) Compo_res.t -> ('b, 'fix2) t) : ('b, 'fix2) t
        =
     fun s ->
      MONAD.bind (m s) (fun r ->
          match r with
          | Compo_res.Ok (a, s) -> f (Compo_res.Ok a) s
          | Error e -> f (Compo_res.Error e) s
          | Missing fixes -> f (Compo_res.Missing fixes) s)

    let fold_list x ~init ~f =
      Monad.foldM ~return:ok ~bind ~fold:Foldable.List.fold x ~init ~f

    let run_consumer ~subst p = p subst

    module Syntax = struct
      let ( let* ) = bind
      let ( let+ ) = map
      let ( let+? ) = map_missing
      let ( let*! ) = bind_res
    end

    let consume_pure e : (unit, 'fix) t =
      let open Syntax in
      let* v = apply_subst Fun.id e in
      if Approx.As_ctx.is_ux () then lift_symex (assume [ v ])
      else
        bind (lift_symex (assert_ v)) @@ fun assert_passed ->
        if assert_passed then ok () else lfail v
  end
end
