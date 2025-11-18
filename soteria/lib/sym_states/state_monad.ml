open Soteria_std
module Compo_res = Symex.Compo_res

module type S = sig
  include Symex.Base
  module Symex : Symex.Base

  type st

  val lift : 'a Symex.t -> 'a t
  val with_state : state:st -> 'a t -> ('a * st) Symex.t

  module Result : sig
    include module type of Result

    val with_state :
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

  type 'a t = State.t -> ('a * State.t) Sym.t
  type lfail = Sym.lfail
  type st = State.t

  let return x st = Sym.return (x, st)

  let bind m f st =
    let* x, st' = m st in
    f x st'

  let map m f st =
    let+ x, st' = m st in
    (f x, st')

  let lift x st =
    let+ x in
    (x, st)

  let with_state ~state x = x state
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

    let with_state ~state x =
      Symex.map (x state) @@ function
      | Compo_res.Ok res, _ -> Compo_res.Ok (res, state)
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
end
