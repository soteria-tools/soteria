open Soteria_logs
open Syntaxes.FunctionWrap
module L = Logs.L
module List = ListLabels

exception Gave_up of string

module Or_gave_up = struct
  type 'err t = E of 'err | Gave_up of string

  let pp pp_err fmt = function
    | E e -> pp_err fmt e
    | Gave_up reason -> Format.fprintf fmt "Gave up: %s" reason
end

module Meta = struct
  (** Management of meta-information about the execution. *)

  module type S = sig
    (** Management of meta-information about the execution. *)

    (** Used for various bookkeeping utilities. *)
    module Range : Soteria_stats.CodeRange
  end

  module Dummy : S with type Range.t = unit = struct
    (** Can be used by users as a default *)

    module Range = struct
      type t = unit

      let to_yojson () = `Null
      let of_yojson _ = Ok ()
    end
  end
end

module type S = sig
  module Value : Value.S
  module Stats : Soteria_stats.S

  type 'a t

  (** Type of error that corresponds to a logical failure (i.e. a logical
      mismatch during consumption).

      Use this instead of [`Lfail] directly in type signatures to avoid
      potential typos such as [`LFail] which will take precious time to debug...
      trust me. *)
  type lfail = [ `Lfail of Value.sbool Value.t ]

  type 'a v := 'a Value.t
  type 'a vt := 'a Value.ty
  type sbool := Value.sbool

  val assume : sbool v list -> unit t
  val vanish : unit -> 'a t

  (** Assert is a symbolic process that does not branch but tests for the
      feasibility of the input symbolic value.

      - In UX, [assert_] returns [false] if and only if [not value] is
        {b satisfiable}.
      - In OX, [assert_] returns [true] if and only if [not value] is
        {b unsatisfiable}. *)
  val assert_ : sbool v -> bool t

  (** [consume_pure v] is somewhat equivalent to
      [if%sat v then ok () else error (`Lfail v)]. The difference is that it
      does not branch:
      - In UX, analysis gives up in case of [`Lfail], and the error branch is
        discarded. Because of that, doing two sat checks is not required, and
        [consume_pure] is just an [assume].
      - In OX, the error case is enough to know the proof cannot be concluded,
        and the ok branch is discarded. Therefore, in OX, it is equivalent to
        [assert_]. *)
  val consume_pure : sbool v -> (unit, [> lfail ], 'a) Compo_res.t t

  (** [consume_false] is [consume_pure (Value.bool false)], but with a signature
      that is easier to manipulate. *)
  val consume_false : unit -> ('a, [> lfail ], 'b) Compo_res.t t

  (** [nondet ty f] creates a fresh variable of type [ty]. *)
  val nondet : 'a vt -> 'a v t

  val fresh_var : 'a vt -> Var.t t

  val branch_on :
    ?left_branch_name:string ->
    ?right_branch_name:string ->
    sbool v ->
    then_:(unit -> 'a t) ->
    else_:(unit -> 'a t) ->
    'a t

  (** Branches on value, and ({b in UX only}) takes at most one branch, starting
      with the [then] branch. This means that if the [then_] branch is SAT, it
      is taken and the [else_] branch is ignored, otherwise the [else_] branch
      is taken. In OX mode, this behaves exactly as [branch_on]. *)
  val branch_on_take_one :
    ?left_branch_name:string ->
    ?right_branch_name:string ->
    sbool v ->
    then_:(unit -> 'a t) ->
    else_:(unit -> 'a t) ->
    'a t

  val branches : (unit -> 'a t) list -> 'a t

  (** {2 Fuel} *)

  val consume_fuel_steps : int -> unit t

  (** [run ~fuel  p] actually performs symbolic execution of the symbolic
      process [p] and returns a list of obtained branches which capture the
      outcome together with a path condition that is a list of boolean symbolic
      values.

      The [mode] parameter is used to specify the whether execution should be
      done in an under-approximate ({!Approx.UX}) or an over-approximate
      ({!Approx.OX}) manner. Users may optionally pass a
      {{!Fuel_gauge.t}fuel gauge} to limit execution depth and breadth.

      @raise {!Gave_up}
        if the symbolic process calls [give_up] and the mode is {!Approx.UX}.
        Prefer using {!Result.run} when possible. *)
  val run :
    ?fuel:Fuel_gauge.t -> mode:Approx.t -> 'a t -> ('a * sbool v list) list

  (** Same as {!run}, but returns additional information about execution, see
      {!Soteria_stats}. *)
  val run_with_stats :
    ?fuel:Fuel_gauge.t ->
    mode:Approx.t ->
    'a t ->
    ('a * sbool v list) list Stats.with_stats

  (** Same as {!run} but has to be run within {!Stats.with_stats} or will throw
      an exception. This function is exposed should users wish to run several
      symbolic execution processes using a single [stats] record. *)
  val run_needs_stats :
    ?fuel:Fuel_gauge.t -> mode:Approx.t -> 'a t -> ('a * sbool v list) list

  include Monad.Base with type 'a t := 'a t

  (** Gives up on this path of execution for incompleteness reason. For
      instance, if a give feature is unsupported.

      This function also logs, and adds the reason for giving up to the
      execution statistics. *)
  val give_up : loc:Stats.Range.t -> string -> 'a t

  (** If the given option is None, gives up execution, otherwise continues,
      unwrapping the option. *)
  val some_or_give_up : loc:Stats.Range.t -> string -> 'a option -> 'a t

  val all : ('a -> 'b t) -> 'a list -> 'b list t
  val fold_list : 'a list -> init:'b -> f:('b -> 'a -> 'b t) -> 'b t
  val fold_iter : 'a Iter.t -> init:'b -> f:('b -> 'a -> 'b t) -> 'b t
  val fold_seq : 'a Seq.t -> init:'b -> f:('b -> 'a -> 'b t) -> 'b t

  module Result : sig
    type nonrec ('ok, 'err, 'fix) t = ('ok, 'err, 'fix) Compo_res.t t

    (** Same as {{!Soteria_symex.Symex.S.run}run}, but receives a symbolic
        process that returns a {!Compo_res.t} and maps the result to an
        {!Or_gave_up.t}, potentially adding any path that gave up to the list.
    *)
    val run :
      ?fuel:Fuel_gauge.t ->
      mode:Approx.t ->
      ('ok, 'err, 'fix) t ->
      (('ok, 'err Or_gave_up.t, 'fix) Compo_res.t * Value.sbool Value.t list)
      list

    val run_with_stats :
      ?fuel:Fuel_gauge.t ->
      mode:Approx.t ->
      ('ok, 'err, 'fix) t ->
      (('ok, 'err Or_gave_up.t, 'fix) Compo_res.t * Value.sbool Value.t list)
      list
      Stats.with_stats

    val run_needs_stats :
      ?fuel:Fuel_gauge.t ->
      mode:Approx.t ->
      ('ok, 'err, 'fix) t ->
      (('ok, 'err Or_gave_up.t, 'fix) Compo_res.t * Value.sbool Value.t list)
      list

    val ok : 'ok -> ('ok, 'err, 'fix) t
    val error : 'err -> ('ok, 'err, 'fix) t
    val miss : 'fix list -> ('ok, 'err, 'fix) t

    (** Missing without any fix. Will add to the statistics and log that
        information. *)
    val miss_no_fix : reason:string -> unit -> ('ok, 'err, 'fix) t

    val bind :
      ('ok, 'err, 'fix) t -> ('ok -> ('a, 'err, 'fix) t) -> ('a, 'err, 'fix) t

    val map : ('ok, 'err, 'fix) t -> ('ok -> 'a) -> ('a, 'err, 'fix) t

    val bind_2 :
      ('ok, 'err, 'fix) t ->
      f:('ok -> ('a, 'b, 'fix) t) ->
      fe:('err -> ('a, 'b, 'fix) t) ->
      ('a, 'b, 'fix) t

    val map_error : ('ok, 'err, 'fix) t -> ('err -> 'a) -> ('ok, 'a, 'fix) t
    val map_missing : ('ok, 'err, 'fix) t -> ('fix -> 'a) -> ('ok, 'err, 'a) t

    val fold_list :
      'a list -> init:'b -> f:('b -> 'a -> ('b, 'c, 'd) t) -> ('b, 'c, 'd) t

    val fold_iter :
      'a Iter.t -> init:'b -> f:('b -> 'a -> ('b, 'c, 'd) t) -> ('b, 'c, 'd) t

    val fold_seq :
      'a Seq.t -> init:'b -> f:('b -> 'a -> ('b, 'c, 'd) t) -> ('b, 'c, 'd) t
  end

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val ( let** ) :
      ('a, 'c, 'd) Result.t ->
      ('a -> ('b, 'c, 'd) Result.t) ->
      ('b, 'c, 'd) Result.t

    val ( let++ ) : ('a, 'c, 'd) Result.t -> ('a -> 'b) -> ('b, 'c, 'd) Result.t
    val ( let+- ) : ('a, 'b, 'd) Result.t -> ('b -> 'c) -> ('a, 'c, 'd) Result.t
    val ( let+? ) : ('a, 'b, 'c) Result.t -> ('c -> 'd) -> ('a, 'b, 'd) Result.t

    module Symex_syntax : sig
      type sbool_v := Value.sbool Value.t

      val branch_on :
        ?left_branch_name:string ->
        ?right_branch_name:string ->
        sbool_v ->
        then_:(unit -> 'a t) ->
        else_:(unit -> 'a t) ->
        'a t

      val branch_on_take_one :
        ?left_branch_name:string ->
        ?right_branch_name:string ->
        sbool_v ->
        then_:(unit -> 'a t) ->
        else_:(unit -> 'a t) ->
        'a t
    end
  end
end

module Make (Meta : Meta.S) (Sol : Solver.Mutable_incremental) :
  S with module Value = Sol.Value and module Stats.Range = Meta.Range = struct
  module Stats = Soteria_stats.Make (Meta.Range)

  module Solver = struct
    include Solver.Mutable_to_in_place (Sol)

    let sat () =
      let res = sat () in
      if res = Unknown then Stats.As_ctx.add_sat_unknowns 1;
      res
  end

  module Fuel = struct
    include Reversible.Effectful (Fuel_gauge)

    let consume_branching n = wrap (Fuel_gauge.consume_branching n) ()
    let consume_fuel_steps n = wrap (Fuel_gauge.consume_fuel_steps n) ()
    let take_branches list = wrap (Fuel_gauge.take_branches list) ()
  end

  module Value = Solver.Value
  module MONAD = Monad.IterM
  include MONAD

  module Give_up = struct
    type _ Effect.t += Gave_up_eff : string -> unit Effect.t

    let perform reason = Effect.perform (Gave_up_eff reason)

    let with_give_up_raising f =
      try f ()
      with effect Gave_up_eff reason, k ->
        let backtrace = Printexc.get_raw_backtrace () in
        Effect.Deep.discontinue_with_backtrace k (Gave_up reason) backtrace
  end

  type 'a t = 'a Iter.t
  type lfail = [ `Lfail of Value.sbool Value.t ]

  module Symex_state : Reversible.In_place = struct
    let backtrack_n n =
      Solver.backtrack_n n;
      Fuel.backtrack_n n

    let save () =
      Solver.save ();
      Fuel.save ()

    let reset () = Solver.reset ()
  end

  let consume_fuel_steps n f =
    match Fuel.consume_fuel_steps n with
    | Exhausted ->
        Stats.As_ctx.add_unexplored_branches 1;
        L.debug (fun m -> m "Exhausted step fuel")
    | Not_exhausted ->
        Stats.As_ctx.add_steps n;
        f ()

  let assume learned f =
    let rec aux acc learned =
      match learned with
      | [] ->
          Solver.add_constraints acc;
          f ()
      | l :: ls -> (
          let l = Solver.simplify l in
          match Value.as_bool l with
          | Some true -> aux acc ls
          | Some false ->
              L.trace (fun m -> m "Assuming false, stopping this branch")
          | None -> aux (l :: acc) ls)
    in
    aux [] learned

  (** Same as assert_, but not captured within the monad. Not to be exposed to
      the user, because without proper care, this could have unwanted
      side-effects at the wrong time. *)
  let assert_raw value : bool =
    let value = Solver.simplify value in
    match Value.as_bool value with
    | Some true -> true
    | Some false -> false
    | None ->
        let@ () =
          Logs.with_section
            (Fmt.str "Checking entailment for %a" Value.ppa value)
        in
        Symex_state.save ();
        Solver.add_constraints [ Value.(not value) ];
        let sat_result = Solver.sat () in
        Symex_state.backtrack_n 1;
        if Approx.As_ctx.is_ux () then not (Solver_result.is_sat sat_result)
        else Solver_result.is_unsat sat_result

  (** Assert is [if%sat (not value) then error else ok]. In UX, assert only
      returns [false] if (not value) is {b satisfiable}. In OX, assert only
      returns [true] if (not value) is {b unsatisfiable}. *)
  let assert_ value f = f (assert_raw value)

  let consume_pure value f =
    if Approx.As_ctx.is_ux () then
      assume [ value ] (fun () -> f (Compo_res.Ok ()))
    else
      let assert_passed = assert_raw value in
      if assert_passed then f (Ok ()) else f (Error (`Lfail value))

  let consume_false () f =
    if Approx.As_ctx.is_ux () then ()
    else f (Compo_res.Error (`Lfail (Value.bool false)))

  let nondet ty f =
    let v = Solver.fresh_var ty in
    let v = Value.mk_var v ty in
    f v

  let fresh_var ty f = f (Solver.fresh_var ty)

  let branch_on ?(left_branch_name = "Left branch")
      ?(right_branch_name = "Right branch") guard ~(then_ : unit -> 'a Iter.t)
      ~(else_ : unit -> 'a Iter.t) : 'a Iter.t =
   fun f ->
    let guard = Solver.simplify guard in
    match Value.as_bool guard with
    (* [then_] and [else_] could be ['a t] instead of [unit -> 'a t],
       if we remove the Some true and Some false optimisation. *)
    | Some true -> then_ () f
    | Some false -> else_ () f
    | None ->
        let left_unsat = ref false in

        Symex_state.save ();
        Logs.with_section ~is_branch:true left_branch_name (fun () ->
            Solver.add_constraints ~simplified:true [ guard ];
            let sat_res = Solver.sat () in
            left_unsat := Solver_result.is_unsat sat_res;
            if Solver_result.is_sat sat_res then then_ () f
            else L.trace (fun m -> m "Branch is not feasible"));
        Symex_state.backtrack_n 1;
        Logs.with_section ~is_branch:true right_branch_name (fun () ->
            Solver.add_constraints [ Value.(not guard) ];
            if !left_unsat then
              (* Right must be sat since left was not! We didn't branch so we don't consume the counter *)
              else_ () f
            else
              match Fuel.consume_branching 1 with
              | Exhausted ->
                  Stats.As_ctx.add_unexplored_branches 1;
                  L.debug (fun m ->
                      m "Exhausted branching fuel, not continuing")
              | Not_exhausted ->
                  Stats.As_ctx.add_branches 1;
                  if Solver_result.is_sat (Solver.sat ()) then else_ () f
                  else L.trace (fun m -> m "Branch is not feasible"))

  let branch_on_take_one_ux ?left_branch_name:_ ?right_branch_name:_ guard
      ~then_ ~else_ : 'a Iter.t =
   fun f ->
    let guard = Solver.simplify guard in
    match Value.as_bool guard with
    | Some true -> then_ () f
    | Some false -> else_ () f
    | None ->
        Symex_state.save ();
        Solver.add_constraints ~simplified:true [ guard ];
        let left_sat = ref true in
        if Solver_result.is_sat (Solver.sat ()) then then_ () f
        else left_sat := false;
        Symex_state.backtrack_n 1;
        if not !left_sat then (
          Solver.add_constraints [ Value.(not guard) ];
          else_ () f)

  let branch_on_take_one ?left_branch_name ?right_branch_name guard ~then_
      ~else_ f =
    if Approx.As_ctx.is_ux () then
      branch_on_take_one_ux ?left_branch_name ?right_branch_name guard ~then_
        ~else_ f
    else branch_on ?left_branch_name ?right_branch_name guard ~then_ ~else_ f

  let branches (brs : (unit -> 'a Iter.t) list) : 'a Iter.t =
   fun f ->
    let brs = Fuel.take_branches brs in
    (* If there are 0 or 1 branches, we don't do anything,
       else we add how many *new* branches we take. *)
    Stats.As_ctx.add_branches (max (List.length brs - 1) 0);
    match brs with
    | [] -> ()
    | [ a ] -> a () f
    | a :: r ->
        (* First branch should not backtrack and last branch should not save *)
        let with_section =
          let branch_counter = ref 0 in
          fun k ->
            Logs.start_section ~is_branch:true
              ("Branch number " ^ string_of_int !branch_counter);
            k ();
            Logs.end_section ();
            incr branch_counter
        in
        let rec loop brs =
          match brs with
          | [ x ] ->
              Symex_state.backtrack_n 1;
              with_section @@ fun () -> x () f
          | x :: r ->
              Symex_state.backtrack_n 1;
              Symex_state.save ();
              (with_section @@ fun () -> x () f);
              loop r
          | [] -> failwith "unreachable"
        in
        Symex_state.save ();
        (with_section @@ fun () -> a () f);
        loop r

  let run_needs_stats ?(fuel = Fuel_gauge.infinite) ~mode iter =
    let@ () = Stats.As_ctx.add_time_of in
    Symex_state.reset ();
    let@ () = Fuel.run ~init:fuel in
    let@ () = Approx.As_ctx.with_mode mode in
    let@ () = Give_up.with_give_up_raising in
    let l = ref [] in
    let () = iter @@ fun x -> l := (x, Solver.as_values ()) :: !l in
    List.rev !l

  let run ?fuel ~mode iter =
    let@ () = Stats.As_ctx.with_stats_ignored () in
    run_needs_stats ?fuel ~mode iter

  let run_with_stats ?fuel ~mode iter =
    let@ () = Stats.As_ctx.with_stats () in
    run_needs_stats ?fuel ~mode iter

  let vanish () _f = ()

  let all fn xs =
    let rec aux acc rs =
      match rs with
      | [] -> return (List.rev acc)
      | r :: rs -> bind (fn r) @@ fun x -> aux (x :: acc) rs
    in
    aux [] xs

  let give_up ~loc reason _f =
    (* The bind ensures that the side effect will not be enacted before the whole process is ran. *)
    L.info (fun m -> m "%s" reason);
    Stats.As_ctx.push_give_up_reason ~loc reason;
    if Approx.As_ctx.is_ox () then Give_up.perform reason

  let some_or_give_up ~loc reason = function
    | Some x -> return x
    | None -> give_up ~loc reason

  let foldM ~fold x ~init ~f = Monad.foldM ~bind ~return ~fold x ~init ~f
  let fold_list x ~init ~f = foldM ~fold:Foldable.List.fold x ~init ~f
  let fold_iter x ~init ~f = foldM ~fold:Foldable.Iter.fold x ~init ~f
  let fold_seq x ~init ~f = foldM ~fold:Foldable.Seq.fold x ~init ~f

  module Result = struct
    include Compo_res.T (MONAD)

    let run_needs_stats ?(fuel = Fuel_gauge.infinite) ~mode iter =
      let@ () = Stats.As_ctx.add_time_of in
      Symex_state.reset ();
      let@ () = Fuel.run ~init:fuel in
      let@ () = Approx.As_ctx.with_mode mode in
      let l = ref [] in
      let () =
        try
          iter @@ fun x ->
          let x = Compo_res.map_error x (fun e -> Or_gave_up.E e) in
          l := (x, Solver.as_values ()) :: !l
        with effect Give_up.Gave_up_eff reason, k ->
          l := (Compo_res.Error (Gave_up reason), Solver.as_values ()) :: !l;
          Effect.Deep.continue k ()
      in
      List.rev !l

    let run ?fuel ~mode iter =
      let@ () = Stats.As_ctx.with_stats_ignored () in
      run_needs_stats ?fuel ~mode iter

    let run_with_stats ?fuel ~mode iter =
      let@ () = Stats.As_ctx.with_stats () in
      run_needs_stats ?fuel ~mode iter

    let miss_no_fix ~reason () =
      bind (ok ()) @@ fun () ->
      Stats.As_ctx.push_missing_without_fix reason;
      L.debug (fun m -> m "Missing without fix: %s" reason);
      miss []

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

    module Symex_syntax = struct
      let branch_on = branch_on
      let branch_on_take_one = branch_on_take_one
    end
  end
end
