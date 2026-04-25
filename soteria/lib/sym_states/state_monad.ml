open Soteria_std
module Compo_res = Symex.Compo_res
module Soteria_symex = Symex

module type Base_sig = Symex.Base

module type S = sig
  type st

  module Symex : Base_sig

  include module type of
      Monad.StateT_base
        (struct
          type t = st
        end)
        (Symex)

  include Base_sig with type 'a t := 'a t

  module Result : sig
    include
      module type of Result
        with type ('a, 'e, 'f) t = ('a, 'e, 'f) Compo_res.t t

    val get_state : unit -> (st, 'e, 'f) t
    val set_state : st -> (unit, 'e, 'f) t

    val run_with_state :
      state:st -> ('a, 'e, 'f) t -> ('a * st, 'e * st, 'f) Symex.Result.t
  end

  module Producer : sig
    include module type of Producer

    val run_with_state : state:st -> 'a t -> ('a * st) Symex.Producer.t
  end

  module Consumer : sig
    include module type of Consumer

    val run_with_state :
      state:st -> ('a, 'f) t -> ('a * st, 'f) Symex.Consumer.t
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
     and module Symex = Sym
     and type st = State.t
     and type 'a t = State.t -> ('a * State.t) Sym.t = struct
  module CORE = struct
    module Symex = Sym
    module Value = Sym.Value

    type lfail = Sym.lfail [@@deriving show { with_path = false }]
    type cons_fail = Sym.cons_fail [@@deriving show { with_path = false }]
    type st = State.t

    include Monad.StateT_base (State) (Sym)

    let[@inline] assume b = lift (Sym.assume b)
    let[@inline] vanish () = lift (Sym.vanish ())
    let[@inline] assert_ b = lift (Sym.assert_ b)
    let[@inline] nondet_UNSAFE ty = Sym.nondet_UNSAFE ty
    let[@inline] nondet ty = lift (Sym.nondet ty)
    let[@inline] simplify v = lift (Sym.simplify v)
    let[@inline] fresh_var ty = lift (Sym.fresh_var ty)
    let[@inline] branches b st = Sym.branches (List.map (fun f () -> f () st) b)
    let[@inline] consume_fuel_steps n = lift (Sym.consume_fuel_steps n)
    let[@inline] give_up msg = lift (Sym.give_up msg)

    let[@inline] branch_on ?left_branch_name ?right_branch_name guard ~then_
        ~else_ =
     fun st ->
      Sym.branch_on ?left_branch_name ?right_branch_name guard
        ~then_:(fun () -> then_ () st)
        ~else_:(fun () -> else_ () st)

    let[@inline] branch_on_take_one ?left_branch_name ?right_branch_name guard
        ~then_ ~else_ =
     fun st ->
      Sym.branch_on_take_one ?left_branch_name ?right_branch_name guard
        ~then_:(fun () -> then_ () st)
        ~else_:(fun () -> else_ () st)

    let[@inline] if_sure ?left_branch_name ?right_branch_name guard ~then_
        ~else_ =
     fun st ->
      Sym.if_sure ?left_branch_name ?right_branch_name guard
        ~then_:(fun () -> then_ () st)
        ~else_:(fun () -> else_ () st)

    let[@inline] with_frame (name : string) (f : unit -> 'a t) : 'a t =
     fun st -> Sym.with_frame name (fun () -> f () st)
  end

  include CORE
  include Soteria_symex.Base_extension (CORE)

  module Result = struct
    include Result

    let[@inline] get_state () = fun st -> Symex.return (Compo_res.Ok st, st)
    let[@inline] set_state st = fun _ -> Symex.return (Compo_res.Ok (), st)

    let[@inline] run_with_state ~state x =
      Symex.map (x state) @@ function
      | Compo_res.Ok res, state -> Compo_res.Ok (res, state)
      | Error e, state -> Error (e, state)
      | Missing f, _ -> Missing f
  end

  module Producer = struct
    open Syntax
    include Producer

    let leak m = function
      | None ->
          let+ res = run_identity m in
          (res, None)
      | Some subst ->
          let+ res, subst = run ~subst m in
          (res, Some subst)

    let[@inline] run_with_state ~state (x : 'a t) : ('a * st) Symex.Producer.t =
      Symex.Producer.from_raw_UNSAFE (fun subst ->
          let open Symex.Syntax in
          let+ (res, subst), state = leak x subst state in
          ((res, state), subst))
  end

  module Consumer = struct
    include Consumer

    let[@inline] run_with_state ~state (x : ('a, 'f) t) :
        ('a * st, 'f) Symex.Consumer.t =
      let open Symex.Syntax in
      Symex.Consumer.from_raw_UNSAFE (fun subst ->
          let+ res, state = run ~subst x state in
          match res with
          | Compo_res.Ok (res, subst) -> Compo_res.Ok ((res, state), subst)
          | Error e -> Error e
          | Missing f -> Missing f)
  end

  module Syntax = struct
    include Syntax

    let[@inline] ( let*^ ) x f = bind (lift x) f
    let[@inline] ( let+^ ) x f = map (lift x) f
    let[@inline] ( let**^ ) x f = Result.bind (lift x) f
    let[@inline] ( let++^ ) x f = Result.map (lift x) f
  end
end
