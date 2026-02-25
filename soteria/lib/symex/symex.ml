(** The core of Soteria symbolic execution. *)

open Soteria_std
open Logs.Import
open Syntaxes.FunctionWrap

(* Re-export a few definitions, leaving this module as root. *)
module Approx = Approx
module Compo_res = Compo_res
module Fuel_gauge = Fuel_gauge
module Solver = Solver
module Solver_result = Solver_result
module Value = Value
module Var = Var

exception Gave_up of string

module Or_gave_up = struct
  type 'err t = E of 'err | Gave_up of string

  let pp pp_err fmt = function
    | E e -> pp_err fmt e
    | Gave_up reason -> Format.fprintf fmt "Gave up: %s" reason

  let unwrap_exn = function
    | E e -> e
    | Gave_up reason -> raise (Gave_up reason)
end

module type Core = sig
  module Value : Value.S

  (** Represents a yet-to-be-executed symbolic process which terminates with a
      value of type ['a]. *)
  type 'a t

  include Monad.Base with type 'a t := 'a t

  (** Type of error that corresponds to a logical failure (i.e. a logical
      mismatch during consumption).

      Use this instead of [`Lfail] directly in type signatures to avoid
      potential typos such as [`LFail] which will take precious time to debug...
      trust me. *)
  type lfail = [ `Lfail of Value.(sbool t) ]

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

  (** [nondet ty] creates a fresh variable of type [ty]. *)
  val nondet : 'a vt -> 'a v t

  (** [simplify v] simplifies the value [v] according to the current path
      condition. *)
  val simplify : 'a v -> 'a v t

  val fresh_var : 'a vt -> Var.t t

  val branch_on :
    ?left_branch_name:string ->
    ?right_branch_name:string ->
    sbool v ->
    then_:(unit -> 'a t) ->
    else_:(unit -> 'a t) ->
    'a t

  (** [if_sure cond ~then_ ~else_] evaluates the [~then_] branch if [cond] is
      guaranteed to hold in the current context, and otherwise evaluates
      [~else_].

      This is to be used with caution: the [~then_] branch should {e always}
      describe a behaviour that is semantically equivalent to that of the
      [~else_] branch when [cond] holds. *)
  val if_sure :
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

  (** Gives up on this path of execution for incompleteness reason. For
      instance, if a give feature is unsupported. *)
  val give_up : string -> 'a t

  val branches : (unit -> 'a t) list -> 'a t

  (** {2 Fuel} *)

  val consume_fuel_steps : int -> unit t
end

module type Base = sig
  include Core

  (** [assert_or_error guard err] asserts [guard] is true, and otherwise returns
      [Compo_res.Error err]. Biased towards the assertion being [false] to
      reduce SAT-checks.

      This is provided as a utility, and is equivalent to
      {@ocaml[
        branch_on (not guard)
          ~then_:(fun () -> return (Compo_res.error err))
          ~else_:(fun () -> return (Compo_res.ok ()))
      ]} *)
  val assert_or_error :
    Value.(sbool t) -> 'err -> (unit, 'err, 'f) Compo_res.t t

  (** If the given option is None, gives up execution, otherwise continues,
      unwrapping the option. *)
  val some_or_give_up : string -> 'a option -> 'a t

  val all : ('a -> 'b t) -> 'a list -> 'b list t
  val fold_list : 'a list -> init:'b -> f:('b -> 'a -> 'b t) -> 'b t
  val fold_iter : 'a Iter.t -> init:'b -> f:('b -> 'a -> 'b t) -> 'b t
  val fold_seq : 'a Seq.t -> init:'b -> f:('b -> 'a -> 'b t) -> 'b t
  val iter_list : 'a list -> f:('a -> unit t) -> unit t
  val iter_iter : 'a Iter.t -> f:('a -> unit t) -> unit t
  val map_list : 'a list -> f:('a -> 'b t) -> 'b list t

  module Result : sig
    type nonrec ('ok, 'err, 'fix) t = ('ok, 'err, 'fix) Compo_res.t t

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

    val bind_error :
      ('ok, 'err, 'fix) t -> ('err -> ('ok, 'a, 'fix) t) -> ('ok, 'a, 'fix) t

    val map_error : ('ok, 'err, 'fix) t -> ('err -> 'a) -> ('ok, 'a, 'fix) t
    val map_missing : ('ok, 'err, 'fix) t -> ('fix -> 'a) -> ('ok, 'err, 'a) t

    val fold_list :
      'a list -> init:'b -> f:('b -> 'a -> ('b, 'c, 'd) t) -> ('b, 'c, 'd) t

    val fold_iter :
      'a Iter.t -> init:'b -> f:('b -> 'a -> ('b, 'c, 'd) t) -> ('b, 'c, 'd) t

    val fold_seq :
      'a Seq.t -> init:'b -> f:('b -> 'a -> ('b, 'c, 'd) t) -> ('b, 'c, 'd) t

    val iter_list : 'a list -> f:('a -> (unit, 'b, 'c) t) -> (unit, 'b, 'c) t
    val iter_iter : 'a Iter.t -> f:('a -> (unit, 'b, 'c) t) -> (unit, 'b, 'c) t
    val map_list : 'a list -> f:('a -> ('b, 'c, 'd) t) -> ('b list, 'c, 'd) t
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

    val ( let*- ) :
      ('a, 'b, 'c) Result.t ->
      ('b -> ('a, 'd, 'c) Result.t) ->
      ('a, 'd, 'c) Result.t

    val ( let+? ) : ('a, 'b, 'c) Result.t -> ('c -> 'd) -> ('a, 'b, 'd) Result.t

    module Symex_syntax : sig
      type sbool_v := Value.(sbool t)

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

      val if_sure :
        ?left_branch_name:string ->
        ?right_branch_name:string ->
        sbool_v ->
        then_:(unit -> 'a t) ->
        else_:(unit -> 'a t) ->
        'a t
    end
  end
end

module type S = sig
  include Base

  (** A Symex runs in a pooled solver environment. This module exposes some
      information about the solver pool. *)
  module Solver_pool : sig
    val total_created : unit -> int
    val total_available : unit -> int
  end

  type 'a v := 'a Value.t
  type sbool := Value.sbool

  (** [run ~fuel  p] actually performs symbolic execution of the symbolic
      process [p] and returns a list of obtained branches which capture the
      outcome together with a path condition that is a list of boolean symbolic
      values.

      The [mode] parameter is used to specify whether execution should be done
      in an under-approximate ({!Symex.Approx.UX}) or an over-approximate
      ({!Symex.Approx.OX}) manner. Users may optionally pass a
      {{!Fuel_gauge.t}fuel gauge} to limit execution depth and breadth.

      @raise Symex.Gave_up
        if the symbolic process calls [give_up] and the mode is
        {!Symex.Approx.OX}. Prefer using {!Result.run} when possible. *)
  val run :
    ?fuel:Fuel_gauge.t -> mode:Approx.t -> 'a t -> ('a * sbool v list) list

  (** Same as {!run}, but returns additional information about execution, see
      {!Soteria.Stats}. *)
  val run_with_stats :
    ?fuel:Fuel_gauge.t ->
    mode:Approx.t ->
    'a t ->
    ('a * sbool v list) list Stats.with_stats

  (** Same as {!run} but has to be run within {!Stats.As_ctx.with_stats} or will
      throw an exception. This function is exposed should users wish to run
      several symbolic execution processes using a single [stats] record. *)
  val run_needs_stats :
    ?fuel:Fuel_gauge.t -> mode:Approx.t -> 'a t -> ('a * sbool v list) list

  module Result : sig
    include module type of Result

    (** Same as {{!Symex.S.run}run}, but receives a symbolic process that
        returns a {!Symex.Compo_res.t} and maps the result to an
        {!Symex.Or_gave_up.t}, potentially adding any path that gave up to the
        list. *)
    val run :
      ?fuel:Fuel_gauge.t ->
      ?fail_fast:bool ->
      mode:Approx.t ->
      ('ok, 'err, 'fix) t ->
      (('ok, 'err Or_gave_up.t, 'fix) Compo_res.t * Value.(sbool t) list) list

    val run_with_stats :
      ?fuel:Fuel_gauge.t ->
      ?fail_fast:bool ->
      mode:Approx.t ->
      ('ok, 'err, 'fix) t ->
      (('ok, 'err Or_gave_up.t, 'fix) Compo_res.t * Value.(sbool t) list) list
      Stats.with_stats

    val run_needs_stats :
      ?fuel:Fuel_gauge.t ->
      ?fail_fast:bool ->
      mode:Approx.t ->
      ('ok, 'err, 'fix) t ->
      (('ok, 'err Or_gave_up.t, 'fix) Compo_res.t * Value.(sbool t) list) list
  end
end

module StatKeys = struct
  (** Keys for statistics used in Soteria's symex engine. These are exposed so
      clients can query statistics if needed; the type with which they are
      logged is also documented.

      It is recommended for clients to not use these keys for custom statistics
      tracking. To avoid clashes, all of these are prefixed with [soteria.] *)

  (** Total execution time. Logged as a {!Stats.Float}. *)
  let exec_time = "soteria.exec-time"

  (** SAT solving time. Logged as a {!Stats.Float}. *)
  let sat_time = "soteria.sat-time"

  (** Number of calls to the solver's [sat] function. Logged as a {!Stats.Int}.
  *)
  let sat_checks = "soteria.sat-checks"

  (** Number of calls to the solver's [sat] function that returned [Unknown].
      Logged as a {!Stats.Int}. *)
  let sat_unknowns = "soteria.sat-unknowns"

  (** Number of unexplored branches due to fuel exhaustion. Logged as a
      {!Stats.Int}. *)
  let unexplored_branches = "soteria.unexplored-branches"

  (** Number of branches explored. Logged as a {!Stats.Int}. *)
  let branches = "soteria.branches"

  (** Total number of steps taken across all branches. Logged as a {!Stats.Int}.
  *)
  let steps = "soteria.steps"

  (** Number of give-ups due to incompleteness. Logged as a
      {!Stats.stat_entry.StrSeq}. *)
  let give_up_reasons = "soteria.give-up-reasons"

  (** Number of misses without any fix. Logged as a {!Stats.stat_entry.StrSeq}.
  *)
  let miss_without_fix = "soteria.miss-without-fix"

  (** Number of times [branch_on] was called *)
  let branch_on_calls = "soteria.branch-on-calls"

  (** Number of times [branch_on] actually branched *)
  let branch_on_branched = "soteria.branch-on-branched"

  let () =
    let open Stats in
    let open Logs.Printers in
    register_float_printer exec_time ~name:"Execution time" (fun _ -> pp_time);
    register_float_printer sat_time ~name:"SAT solving time" (fun stats ft t ->
        let exec_time = get_float stats exec_time in
        Fmt.pf ft "%a (%a)" pp_time t
          (pp_unstable ~name:"%" pp_percent)
          (exec_time, t));
    disable_printer sat_unknowns;
    register_int_printer sat_checks ~name:"SAT checks" (fun stats ft sats ->
        let unknowns = get_int stats sat_unknowns in
        Fmt.pf ft "%d (%d unknowns)" sats unknowns);
    disable_printer unexplored_branches;
    register_int_printer branches ~name:"Branches" (fun stats ft b ->
        let unexplored = get_int stats unexplored_branches in
        Fmt.pf ft "%d (%d unexplored)" b unexplored);
    register_int_printer steps ~name:"Steps" (fun _ -> Fmt.int);
    register_printer give_up_reasons ~name:"Give up reasons" (fun _ ->
        default_printer);
    register_printer miss_without_fix ~name:"Misses without fix" (fun _ ->
        default_printer);
    disable_printer branch_on_branched;
    register_int_printer branch_on_calls ~name:"branch_on"
      (fun stats ft calls ->
        let branched = get_int stats branch_on_branched in
        Fmt.pf ft "branches %a of calls (%d of %d)" pp_percenti
          (branched, calls) branched calls)
end

module Make_core (Sol : Solver.Mutable_incremental) = struct
  module Solver = struct
    include Solver.Mutable_to_pooled (Sol)

    let sat () =
      let res = Stats.As_ctx.add_time_of_to StatKeys.sat_time sat in
      Stats.As_ctx.incr StatKeys.sat_checks;
      if res = Unknown then Stats.As_ctx.incr StatKeys.sat_unknowns;
      res
  end

  module Solver_pool = struct
    let total_created () = Solver.total_resources ()
    let total_available () = Solver.available_resources ()
  end

  module Fuel = struct
    include Reversible.Make_effectful (Fuel_gauge)

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
  type lfail = [ `Lfail of Value.(sbool t) ]

  module Symex_state = struct
    let backtrack_n n =
      Solver.backtrack_n n;
      Fuel.backtrack_n n

    let save () =
      Solver.save ();
      Fuel.save ()

    let run ~init_fuel f =
      Solver.run @@ fun () ->
      Fuel.run ~init:init_fuel @@ fun () -> f ()
  end

  let consume_fuel_steps n f =
    match Fuel.consume_fuel_steps n with
    | Exhausted ->
        Stats.As_ctx.incr StatKeys.unexplored_branches;
        L.debug (fun m -> m "Exhausted step fuel")
    | Not_exhausted ->
        Stats.As_ctx.add_int StatKeys.steps n;
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

  (** Same as {!assert_}, but not captured within the monad. Not to be exposed
      to the user, because without proper care, this could have unwanted
      side-effects at the wrong time. *)
  let assert_raw value : bool =
    let value = Solver.simplify value in
    match Value.as_bool value with
    | Some true -> true
    | Some false -> false
    | None ->
        let@ () =
          L.with_section (Fmt.str "Checking entailment for %a" Value.ppa value)
        in
        Symex_state.save ();
        Solver.add_constraints [ Value.(not value) ];
        let sat_result = Solver.sat () in
        let () =
          L.debug (fun m ->
              m "Entailment SAT check returned %a" Solver_result.pp sat_result)
        in
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

  let simplify v f = f (Solver.simplify v)
  let fresh_var ty f = f (Solver.fresh_var ty)

  let branch_on ?(left_branch_name = "Left branch")
      ?(right_branch_name = "Right branch") guard ~(then_ : unit -> 'a Iter.t)
      ~(else_ : unit -> 'a Iter.t) : 'a Iter.t =
   fun f ->
    Stats.As_ctx.incr StatKeys.branch_on_calls;
    let guard = Solver.simplify guard in
    match Value.as_bool guard with
    (* [then_] and [else_] could be ['a t] instead of [unit -> 'a t], if we
       remove the Some true and Some false optimisation. *)
    | Some true -> then_ () f
    | Some false -> else_ () f
    | None ->
        let left_unsat = ref false in
        Symex_state.save ();
        L.with_section ~is_branch:true left_branch_name (fun () ->
            Solver.add_constraints ~simplified:true [ guard ];
            let sat_res = Solver.sat () in
            left_unsat := Solver_result.is_unsat sat_res;
            if Solver_result.is_sat sat_res then then_ () f
            else L.trace (fun m -> m "Branch is not feasible"));
        Symex_state.backtrack_n 1;
        L.with_section ~is_branch:true right_branch_name (fun () ->
            Solver.add_constraints [ Value.(not guard) ];
            if !left_unsat then
              (* Right must be sat since left was not! We didn't branch so we
                 don't consume the counter *)
              else_ () f
            else (
              Stats.As_ctx.incr StatKeys.branch_on_branched;
              match Fuel.consume_branching 1 with
              | Exhausted ->
                  Stats.As_ctx.incr StatKeys.unexplored_branches;
                  L.debug (fun m ->
                      m "Exhausted branching fuel, not continuing")
              | Not_exhausted ->
                  Stats.As_ctx.incr StatKeys.branches;
                  if Solver_result.is_sat (Solver.sat ()) then else_ () f
                  else L.trace (fun m -> m "Branch is not feasible")))

  let if_sure ?left_branch_name:_ ?right_branch_name:_ guard
      ~(then_ : unit -> 'a Iter.t) ~(else_ : unit -> 'a Iter.t) : 'a Iter.t =
   fun f ->
    let guard = Solver.simplify guard in
    match Value.as_bool guard with
    (* [then_] and [else_] could be ['a t] instead of [unit -> 'a t], if we
       remove the Some true and Some false optimisation. *)
    | Some true -> then_ () f
    | Some false -> else_ () f
    | None ->
        Symex_state.save ();
        Solver.add_constraints ~simplified:true [ Value.(not guard) ];
        let neg_unsat = Solver_result.is_unsat (Solver.sat ()) in
        if neg_unsat then then_ () f;
        Symex_state.backtrack_n 1;
        if not neg_unsat then (
          (* Adding this constraint is technically redundant, but it's still
             worth having it in the PC for simplifications. *)
          Solver.add_constraints [ guard ];
          else_ () f)

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
        let left_sat = Solver_result.is_sat (Solver.sat ()) in
        if left_sat then then_ () f;
        Symex_state.backtrack_n 1;
        if not left_sat then (
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
    (* If there are 0 or 1 branches, we don't do anything, else we add how many
       {new} branches we take. *)
    Stats.As_ctx.add_int StatKeys.branches (max (List.length brs - 1) 0);
    match brs with
    | [] -> ()
    | [ a ] -> a () f
    | a :: r ->
        (* First branch should not backtrack and last branch should not save *)
        let with_section =
          let branch_counter = ref 0 in
          fun f ->
            L.with_section ~is_branch:true
              ("Branch number " ^ string_of_int !branch_counter)
              (fun () ->
                f ();
                incr branch_counter)
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

  let vanish () _f = ()

  let give_up reason _f =
    (* The bind ensures that the side effect will not be enacted before the
       whole process is ran. *)
    L.warn (fun m -> m "Gave up: %s" reason);
    Stats.As_ctx.push_str StatKeys.give_up_reasons reason;
    if
      Approx.As_ctx.is_ox ()
      && Solver_result.admissible ~mode:OX (Solver.sat ())
    then Give_up.perform reason
end

module Base_extension (Core : Core) = struct
  open Core

  let assert_or_error guard err =
    branch_on
      Value.(not guard)
      ~then_:(fun () -> return (Compo_res.Error err))
      ~else_:(fun () -> return (Compo_res.Ok ()))

  let all fn xs = Monad.all ~bind ~return fn xs

  let some_or_give_up reason = function
    | Some x -> return x
    | None -> give_up reason

  let foldM ~fold x ~init ~f = Monad.foldM ~bind ~return ~fold x ~init ~f
  let fold_list x ~init ~f = foldM ~fold:Foldable.List.fold x ~init ~f
  let fold_iter x ~init ~f = foldM ~fold:Foldable.Iter.fold x ~init ~f
  let fold_seq x ~init ~f = foldM ~fold:Foldable.Seq.fold x ~init ~f
  let iterM ~fold x ~f = foldM ~fold x ~init:() ~f:(fun () -> f)
  let iter_list x ~f = iterM ~fold:Foldable.List.fold x ~f
  let iter_iter x ~f = iterM ~fold:Foldable.Iter.fold x ~f

  let mapM ~fold ~rev ~cons ~init x ~f =
    foldM ~fold x ~init ~f:(fun acc a -> map (f a) (fun b -> cons b acc))
    |> Fun.flip map rev

  let map_list x ~f =
    mapM ~init:[] ~fold:Foldable.List.fold ~rev:List.rev ~cons:List.cons x ~f

  module Result = struct
    include Compo_res.T (Core)

    let miss_no_fix ~reason () =
      bind (ok ()) @@ fun () ->
      Stats.As_ctx.push_str StatKeys.miss_without_fix reason;
      L.debug (fun m -> m "Missing without fix: %s" reason);
      miss []

    let foldM ~fold x ~init ~f = Monad.foldM ~bind ~return:ok ~fold x ~init ~f
    let fold_list x ~init ~f = foldM ~fold:Foldable.List.fold x ~init ~f
    let fold_iter x ~init ~f = foldM ~fold:Foldable.Iter.fold x ~init ~f
    let fold_seq x ~init ~f = foldM ~fold:Foldable.Seq.fold x ~init ~f
    let iterM ~fold x ~f = foldM ~fold x ~init:() ~f:(fun () -> f)
    let iter_list x ~f = iterM ~fold:Foldable.List.fold x ~f
    let iter_iter x ~f = iterM ~fold:Foldable.Iter.fold x ~f

    let mapM ~fold ~rev ~cons ~init x ~f =
      foldM ~fold x ~init ~f:(fun acc a -> map (f a) (fun b -> cons b acc))
      |> Fun.flip map rev

    let map_list x ~f =
      mapM ~init:[] ~fold:Foldable.List.fold ~rev:List.rev ~cons:List.cons x ~f
  end

  module Syntax = struct
    let ( let* ) = bind
    let ( let+ ) = map
    let ( let** ) = Result.bind
    let ( let++ ) = Result.map
    let ( let+- ) = Result.map_error
    let ( let*- ) = Result.bind_error
    let ( let+? ) = Result.map_missing

    module Symex_syntax = struct
      let branch_on = branch_on
      let branch_on_take_one = branch_on_take_one
      let if_sure = if_sure
    end
  end
end

module Make (Sol : Solver.Mutable_incremental) :
  S with module Value = Sol.Value = struct
  (* TODO: CORE this can go away when `include functors` land
     (https://github.com/ocaml/ocaml/pull/14177) *)
  module CORE = Make_core (Sol)
  include CORE
  include Base_extension (CORE)

  let run_needs_stats_iter ?(fuel = Fuel_gauge.infinite) ~mode iter :
      ('a * Value.(sbool t) list) Iter.t =
   fun continue ->
    let@ () = Stats.As_ctx.add_time_of_to StatKeys.exec_time in
    let@ () = Symex_state.run ~init_fuel:fuel in
    let@ () = Approx.As_ctx.with_mode mode in
    let@ () = Give_up.with_give_up_raising in
    let admissible () = Solver_result.admissible ~mode (Solver.sat ()) in
    iter @@ fun x -> if admissible () then continue (x, Solver.as_values ())

  let run_needs_stats ?(fuel = Fuel_gauge.infinite) ~mode iter =
    Iter.to_list (run_needs_stats_iter ~fuel ~mode iter)

  let run ?fuel ~mode iter =
    let@ () = Stats.As_ctx.with_stats_ignored () in
    run_needs_stats ?fuel ~mode iter

  let run_with_stats ?fuel ~mode iter =
    let@ () = Stats.As_ctx.with_stats () in
    run_needs_stats ?fuel ~mode iter

  module Result = struct
    include Result

    let run_needs_stats ?(fuel = Fuel_gauge.infinite) ?(fail_fast = false) ~mode
        iter =
      let@ () = Stats.As_ctx.add_time_of_to StatKeys.exec_time in
      let@ () = Symex_state.run ~init_fuel:fuel in
      let@ () = Approx.As_ctx.with_mode mode in
      let l = ref [] in
      let () =
        let exception Fail_fast in
        try
          iter @@ fun x ->
          if Solver_result.admissible ~mode (Solver.sat ()) then (
            (* Make sure to drop branche that have leftover assumes with
               unsatisfiable PCs. *)
            let x = Compo_res.map_error x (fun e -> Or_gave_up.E e) in
            l := (x, Solver.as_values ()) :: !l;
            if fail_fast && Compo_res.is_error x then raise Fail_fast)
        with
        | effect Give_up.Gave_up_eff reason, k ->
            l := (Compo_res.Error (Gave_up reason), Solver.as_values ()) :: !l;
            if fail_fast then Effect.Deep.discontinue k Fail_fast
            else Effect.Deep.continue k ()
        | Fail_fast -> ()
      in
      List.rev !l

    let run ?fuel ?fail_fast ~mode iter =
      let@ () = Stats.As_ctx.with_stats_ignored () in
      run_needs_stats ?fuel ?fail_fast ~mode iter

    let run_with_stats ?fuel ?fail_fast ~mode iter =
      let@ () = Stats.As_ctx.with_stats () in
      run_needs_stats ?fuel ?fail_fast ~mode iter
  end
end

module Substs = struct
  module type From_iter = sig
    type t
    type 'a ty
    type 'a symex

    val from_iter : 'a ty Var.iter_vars -> t symex
  end

  module Subst = struct
    include Map.Make (Var)

    let pp = Fmt.Dump.iter_bindings iter Fmt.nop Var.pp Var.pp

    let substitute_extensible ~f ~subst x =
      let next =
        ref
          (match max_binding_opt subst with
          | None -> 0
          | Some (x, _) -> Var.to_int x)
      in
      let subst = ref subst in
      let subst_var =
        match find_opt x !subst with
        | Some x' -> x'
        | None ->
            let x' = Var.of_int !next in
            incr next;
            subst := add x x' !subst;
            x'
      in
      let res = f subst_var x in
      (res, !subst)

    let to_fn subst x = find x subst

    module From_iter (Symex : Base) :
      From_iter
        with type t := Var.t t
         and type 'a ty := 'a Symex.Value.ty
         and type 'a symex := 'a Symex.t = struct
      let from_iter iter_vars =
        let open Symex.Syntax in
        Symex.fold_iter iter_vars ~init:empty ~f:(fun subst (var, ty) ->
            if mem var subst then Symex.return subst
            else
              let+ var' = Symex.fresh_var ty in
              add var var' subst)
    end
  end

  module Subst_mut = struct
    include Hashtbl.Make (Var)

    let add = replace

    let substitute_extensible ~f ~subst x =
      let next =
        ref (to_seq_keys subst |> Seq.map Var.to_int |> Seq.fold_left max 0)
      in
      let subst_var =
        match find_opt subst x with
        | Some x' -> x'
        | None ->
            let x' = Var.of_int !next in
            incr next;
            add subst x x';
            x'
      in
      let res = f subst_var x in
      (res, subst)

    module From_iter (Symex : S) :
      From_iter
        with type t := Var.t t
         and type 'a ty := 'a Symex.Value.ty
         and type 'a symex := 'a Symex.t = struct
      let from_iter iter_vars =
        let open Symex.Syntax in
        let subst = create 0 in
        let+ () =
          Symex.fold_iter iter_vars ~init:() ~f:(fun () (var, ty) ->
              if mem subst var then Symex.return ()
              else
                let+ var' = Symex.fresh_var ty in
                add subst var var')
        in
        subst
    end
  end

  module Bi_subst = struct
    type t = {
      forward : Var.t Subst.t;
      backward : Var.t Subst_mut.t;
      mutable next_backward : int;
    }

    let empty () =
      {
        forward = Subst.empty;
        backward = Subst_mut.create 0;
        next_backward = 0;
      }

    module From_iter (Symex : S) :
      From_iter
        with type t := t
         and type 'a ty := 'a Symex.Value.ty
         and type 'a symex := 'a Symex.t = struct
      open Symex.Syntax

      let from_iter iter_vars =
        Symex.fold_iter iter_vars ~init:(empty ()) ~f:(fun bi_subst (var, ty) ->
            if Subst.mem var bi_subst.forward then Symex.return bi_subst
            else
              let+ var' = Symex.fresh_var ty in
              let forward = Subst.add var var' bi_subst.forward in
              Subst_mut.replace bi_subst.backward var' var;
              let next_backward =
                max bi_subst.next_backward (Var.to_int var + 1)
              in
              { forward; backward = bi_subst.backward; next_backward })
    end

    let is_empty bi_subst = Subst.is_empty bi_subst.forward
    let forward bi_subst v_id = Subst.find v_id bi_subst.forward

    let backward bi_subst v_id =
      match Subst_mut.find_opt bi_subst.backward v_id with
      | Some v -> v
      | None ->
          let v = bi_subst.next_backward in
          bi_subst.next_backward <- v + 1;
          let v = Var.of_int v in
          Subst_mut.add bi_subst.backward v_id v;
          v
  end
end
