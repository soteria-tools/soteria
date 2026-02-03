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

exception Tool_bug of string
exception Gave_up of string

let tool_bug reason = raise (Tool_bug reason)

let () =
  Printexc.register_printer (function
    | Gave_up reason ->
        Some (Fmt.str "Analysis gave up (and was not caught): %s" reason)
    | Tool_bug reason ->
        Some
          (Fmt.str
             "@[<v>Tool Bug : %s@.This is due to an invalid use of the Soteria \
              API, please report this with the tool developer.@]"
             reason)
    | _ -> None)

module Or_gave_up = struct
  type 'err t = E of 'err | Gave_up of string

  let pp pp_err fmt = function
    | E e -> pp_err fmt e
    | Gave_up reason -> Format.fprintf fmt "Gave up: %s" reason

  let unwrap_exn = function
    | E e -> e
    | Gave_up reason -> raise (Gave_up reason)
end

module type Symex_syntax_S = sig
  type sbool_v
  type ('a, 'b) t

  val branch_on :
    ?left_branch_name:string ->
    ?right_branch_name:string ->
    sbool_v ->
    then_:(unit -> ('a, 'b) t) ->
    else_:(unit -> ('a, 'b) t) ->
    ('a, 'b) t

  val branch_on_take_one :
    ?left_branch_name:string ->
    ?right_branch_name:string ->
    sbool_v ->
    then_:(unit -> ('a, 'b) t) ->
    else_:(unit -> ('a, 'b) t) ->
    ('a, 'b) t

  val if_sure :
    ?left_branch_name:string ->
    ?right_branch_name:string ->
    sbool_v ->
    then_:(unit -> ('a, 'b) t) ->
    else_:(unit -> ('a, 'b) t) ->
    ('a, 'b) t
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
  [@@deriving show { with_path = false }]

  type cons_fail = [ lfail | `Missing_subst of Var.t ]
  [@@deriving show { with_path = false }]

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

  (** Do not use [nondet_UNSAFE]. *)
  val nondet_UNSAFE : 'a vt -> 'a v
  (* [nondet_UNSAFE] creates a nondet value but does not wrap it inside a symex
     monad. This could be used unsafely, because it's not lazy. It is exposed
     because we use it in Producer. TODO: this may be removable when we have
     modular explicit, and we can thread a monad through all our definitions *)

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

  (** Runs the process within a section of execution with given name.
      Corresponds to frames in the flamegraph. *)
  val with_frame : string -> (unit -> 'a t) -> 'a t

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

    module Symex_syntax :
      Symex_syntax_S
        with type ('a, 'b) t := 'a t
         and type sbool_v := Value.(sbool t)
  end

  module Producer : sig
    type 'a symex := 'a t
    type subst := Value.Expr.Subst.t

    include Monad.S

    val lift : 'a symex -> 'a t
    val vanish : unit -> 'a t

    val apply_subst :
      ((Value.Expr.t -> 'a Value.t) -> 'syn -> 'sem) -> 'syn -> 'sem t

    val produce_pure : Value.Expr.t -> unit t
    val run : subst:subst -> 'a t -> ('a * subst) symex
    val run_identity : 'a t -> 'a symex

    (** This is unsafe and shouldn't be used in clients, it is only available to
        enable the implementation of the state monad transformer. *)
    val from_raw_UNSAFE : (subst option -> ('a * subst option) symex) -> 'a t

    module Syntax : sig
      include module type of Syntax

      val ( let*^ ) : 'a symex -> ('a -> 'b t) -> 'b t
      val ( let+^ ) : 'a symex -> ('a -> 'b) -> 'b t

      module Symex_syntax :
        Symex_syntax_S
          with type ('a, 'b) t := 'a t
           and type sbool_v := Value.(sbool t)
    end
  end

  module Consumer : sig
    type subst := Value.Expr.Subst.t
    type 'a symex := 'a t
    type ('a, 'fix) t

    val apply_subst :
      ((Value.Expr.t -> 'a Value.t) -> 'syn -> 'sem) -> 'syn -> ('sem, 'fix) t

    val assert_pure : Value.(sbool t) -> (unit, 'fix) t
    val consume_pure : Value.Expr.t -> (unit, 'fix) t
    val learn_eq : Value.Expr.t -> 'a Value.t -> (unit, 'fix) t
    val expose_subst : unit -> (subst, 'fix) t
    val lift_res : ('a, cons_fail, 'fix) Result.t -> ('a, 'fix) t
    val lift : 'a symex -> ('a, 'fix) t
    val branches : (unit -> ('a, 'fix) t) list -> ('a, 'fix) t
    val ok : 'a -> ('a, 'fix) t
    val lfail : Value.sbool Value.t -> ('a, 'fix) t
    val miss : 'fix list -> ('a, 'fix) t
    val miss_no_fix : reason:string -> unit -> ('a, 'fix) t
    val map : ('a, 'fix) t -> ('a -> 'b) -> ('b, 'fix) t
    val map_missing : ('a, 'fix) t -> ('fix -> 'g) -> ('a, 'g) t
    val bind : ('a, 'fix) t -> ('a -> ('b, 'fix) t) -> ('b, 'fix) t

    val fold_list :
      'a list -> init:'b -> f:('b -> 'a -> ('b, 'fix) t) -> ('b, 'fix) t

    val iter_list : 'a list -> f:('a -> (unit, 'fix) t) -> (unit, 'fix) t

    val bind_res :
      ('a, 'fix) t ->
      (('a, cons_fail, 'fix) Compo_res.t -> ('b, 'fix2) t) ->
      ('b, 'fix2) t

    val run :
      subst:subst -> ('a, 'fix) t -> ('a * subst, cons_fail, 'fix) Result.t

    (** This is unsafe and shouldn't be used in clients, it is only available to
        enable the implementation of the state monad transformer. *)
    val from_raw_UNSAFE :
      (subst -> ('a * subst, cons_fail, 'fix) Result.t) -> ('a, 'fix) t

    module Syntax : sig
      val ( let* ) : ('a, 'fix) t -> ('a -> ('b, 'fix) t) -> ('b, 'fix) t
      val ( let+ ) : ('a, 'fix) t -> ('a -> 'b) -> ('b, 'fix) t
      val ( let+? ) : ('a, 'fix) t -> ('fix -> 'g) -> ('a, 'g) t

      val ( let*! ) :
        ('a, 'fix) t ->
        (('a, cons_fail, 'fix) Compo_res.t -> ('b, 'fix2) t) ->
        ('b, 'fix2) t

      val ( let*^ ) : 'a symex -> ('a -> ('b, 'fix) t) -> ('b, 'fix) t
      val ( let+^ ) : 'a symex -> ('a -> 'b) -> ('b, 'fix) t

      module Symex_syntax :
        Symex_syntax_S
          with type ('a, 'b) t := ('a, 'b) t
           and type sbool_v := Value.(sbool t)
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

  module Flamegraph = Profiling.Flamegraph.Make ()
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

  type lfail = [ `Lfail of (Value.(sbool t)[@printer Value.ppa]) ]
  [@@deriving show { with_path = false }]

  type cons_fail = [ lfail | `Missing_subst of Var.t ]
  [@@deriving show { with_path = false }]

  module Symex_state = struct
    let backtrack_n n =
      Solver.backtrack_n n;
      Fuel.backtrack_n n;
      Flamegraph.backtrack_n n

    let save () =
      Solver.save ();
      Fuel.save ();
      Flamegraph.save ()

    let run ~init_fuel f =
      Flamegraph.run @@ fun () ->
      Solver.run @@ fun () ->
      Fuel.run ~init:init_fuel @@ fun () -> f ()
  end

  let consume_fuel_steps n f =
    match Fuel.consume_fuel_steps n with
    | Exhausted ->
        Stats.As_ctx.incr StatKeys.unexplored_branches;
        [%l.debug "Exhausted step fuel"]
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
          match Value.to_bool l with
          | Some true -> aux acc ls
          | Some false -> [%l.trace "Assuming false, stopping this branch"]
          | None -> aux (l :: acc) ls)
    in
    aux [] learned

  (** Same as {!assert_}, but not captured within the monad. Not to be exposed
      to the user, because without proper care, this could have unwanted
      side-effects at the wrong time. *)
  let assert_raw value : bool =
    let value = Solver.simplify value in
    match Value.to_bool value with
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
          [%l.debug
            "Entailment SAT check returned %a" Solver_result.pp sat_result]
        in
        Symex_state.backtrack_n 1;
        if Approx.As_ctx.is_ux () then not (Solver_result.is_sat sat_result)
        else Solver_result.is_unsat sat_result

  (** Assert is [if%sat (not value) then error else ok]. In UX, assert only
      returns [false] if (not value) is {b satisfiable}. In OX, assert only
      returns [true] if (not value) is {b unsatisfiable}. *)
  let assert_ value f = f (assert_raw value)

  let nondet_UNSAFE ty =
    let v = Solver.fresh_var ty in
    Value.mk_var v ty

  let nondet ty f = f (nondet_UNSAFE ty)
  let simplify v f = f (Solver.simplify v)
  let fresh_var ty f = f (Solver.fresh_var ty)

  let branch_on ?(left_branch_name = "Left branch")
      ?(right_branch_name = "Right branch") guard ~(then_ : unit -> 'a t)
      ~(else_ : unit -> 'a t) : 'a t =
   fun f ->
    Stats.As_ctx.incr StatKeys.branch_on_calls;
    let guard = Solver.simplify guard in
    match Value.to_bool guard with
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
            else [%l.trace "Branch is not feasible"]);
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
                  [%l.debug "Exhausted branching fuel, not continuing"]
              | Not_exhausted ->
                  Stats.As_ctx.incr StatKeys.branches;
                  if Solver_result.is_sat (Solver.sat ()) then else_ () f
                  else [%l.trace "Branch is not feasible"]))

  let if_sure ?left_branch_name:_ ?right_branch_name:_ guard
      ~(then_ : unit -> 'a t) ~(else_ : unit -> 'a t) : 'a t =
   fun f ->
    let guard = Solver.simplify guard in
    match Value.to_bool guard with
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
      ~then_ ~else_ : 'a t =
   fun f ->
    let guard = Solver.simplify guard in
    match Value.to_bool guard with
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

  let branches (brs : (unit -> 'a t) list) : 'a t =
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

  let vanish () _f =
    Flamegraph.checkpoint ();
    ()

  let give_up reason _f =
    (* The bind ensures that the side effect will not be enacted before the
       whole process is ran. *)
    [%l.warn "Gave up: %s" reason];
    Stats.As_ctx.push_str StatKeys.give_up_reasons reason;
    if
      Approx.As_ctx.is_ox ()
      && Solver_result.admissible ~mode:OX (Solver.sat ())
    then (
      Flamegraph.checkpoint ();
      Give_up.perform reason)
    else Flamegraph.checkpoint ()

  module Flamegraph_with_frame = Flamegraph.With_frame (MONAD)

  let with_frame name f = Flamegraph_with_frame.with_frame name f
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
      [%l.debug "Missing without fix: %s" reason];
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

  module Producer = struct
    module P =
      Monad.StateT_base
        (struct
          type t = Value.Expr.Subst.t option
        end)
        (Core)

    include P
    include Monad.Extend (P)

    module Syntax = struct
      include Syntax

      let ( let*^ ) x f = bind (lift x) f
      let ( let+^ ) x f = map (lift x) f

      module Symex_syntax = struct
        let[@inline] branch_on ?left_branch_name ?right_branch_name guard ~then_
            ~else_ =
         fun st ->
          branch_on ?left_branch_name ?right_branch_name guard
            ~then_:(fun () -> then_ () st)
            ~else_:(fun () -> else_ () st)

        let[@inline] branch_on_take_one ?left_branch_name ?right_branch_name
            guard ~then_ ~else_ =
         fun st ->
          branch_on_take_one ?left_branch_name ?right_branch_name guard
            ~then_:(fun () -> then_ () st)
            ~else_:(fun () -> else_ () st)

        let[@inline] if_sure ?left_branch_name ?right_branch_name guard ~then_
            ~else_ =
         fun st ->
          if_sure ?left_branch_name ?right_branch_name guard
            ~then_:(fun () -> then_ () st)
            ~else_:(fun () -> else_ () st)
      end
    end

    let vanish () = lift (vanish ())

    let apply_subst (sf : (Value.Expr.t -> 'a Value.t) -> 'syn -> 'sem)
        (e : 'syn) : 'sem t =
     fun s ->
      (* There's maybe a safer version with effects and no reference? *)
      match s with
      | None ->
          let vsf e =
            let v, _ =
              let open Value.Expr.Subst in
              apply ~missing_var:(fun v ty -> Value.mk_var v ty) empty e
            in
            v
          in
          let res = sf vsf e in
          Core.return (res, None)
      | Some s ->
          let s = ref s in
          let vsf e =
            let v, s' =
              Value.Expr.Subst.apply
                ~missing_var:(fun _ ty -> nondet_UNSAFE ty)
                !s e
            in
            s := s';
            v
          in
          let res = sf vsf e in
          Core.return (res, Some !s)

    let produce_pure e : unit t =
      let open Syntax in
      let is_bool = Value.is_bool_ty @@ Value.Expr.ty e in
      if not is_bool then (
        [%l.error
          "Producing non-boolean pure value!! This is quite probably a tool \
           bug, please report it. Expr: %a"
          Value.Expr.pp e];
        vanish ())
      else
        let* v = apply_subst Fun.id e in
        lift (assume [ v ])

    let run ~subst p =
      let ( let+ ) = Core.map in
      let+ x, s = p (Some subst) in
      (x, Option.get s)

    let run_identity p =
      let ( let+ ) = Core.map in
      let+ x, _s = p None in
      x

    let from_raw_UNSAFE x = x
  end

  module Consumer = struct
    type 'a symex = 'a t
    type subst = Value.Expr.Subst.t
    type ('a, 'fix) t = subst -> ('a * subst, cons_fail, 'fix) Result.t

    let lift_res (r : ('a, cons_fail, 'fix) Result.t) : ('a, 'fix) t =
     fun subst -> Result.map r (fun a -> (a, subst))

    let lift (m : 'a symex) : ('a, 'fix) t =
     fun subst -> Core.map m (fun a -> Compo_res.ok (a, subst))

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
      Core.bind (m s) (fun r ->
          match r with
          | Compo_res.Ok (a, s) -> f (Compo_res.Ok a) s
          | Error e -> f (Compo_res.Error e) s
          | Missing fixes -> f (Compo_res.Missing fixes) s)

    let fold_list x ~init ~f =
      Monad.foldM ~return:ok ~bind ~fold:Foldable.List.fold x ~init ~f

    let iter_list x ~f = fold_list x ~init:() ~f:(fun () a -> f a)
    let run ~subst p = p subst

    module Syntax = struct
      let ( let* ) = bind
      let ( let+ ) = map
      let ( let+? ) = map_missing
      let ( let*! ) = bind_res
      let ( let*^ ) m k = bind (lift m) k
      let ( let+^ ) m k = map (lift m) k

      module Symex_syntax = struct
        let[@inline] branch_on ?left_branch_name ?right_branch_name guard ~then_
            ~else_ =
         fun st ->
          branch_on ?left_branch_name ?right_branch_name guard
            ~then_:(fun () -> then_ () st)
            ~else_:(fun () -> else_ () st)

        let[@inline] branch_on_take_one ?left_branch_name ?right_branch_name
            guard ~then_ ~else_ =
         fun st ->
          branch_on_take_one ?left_branch_name ?right_branch_name guard
            ~then_:(fun () -> then_ () st)
            ~else_:(fun () -> else_ () st)

        let[@inline] if_sure ?left_branch_name ?right_branch_name guard ~then_
            ~else_ =
         fun st ->
          if_sure ?left_branch_name ?right_branch_name guard
            ~then_:(fun () -> then_ () st)
            ~else_:(fun () -> else_ () st)
      end
    end

    let apply_subst (sf : (Value.Expr.t -> 'a Value.t) -> 'syn -> 'sem)
        (e : 'syn) : ('sem, 'fix) t =
      let exception Missing_subst of Var.t in
      fun s ->
        let vsf e =
          let v, _ =
            Value.Expr.Subst.apply
              ~missing_var:(fun v _ -> raise (Missing_subst v))
              s e
          in
          v
        in
        try
          let res = sf vsf e in
          Result.ok (res, s)
        with Missing_subst v -> Result.error (`Missing_subst v)

    let assert_pure v : (unit, 'fix) t =
      if Approx.As_ctx.is_ux () then lift (assume [ v ])
      else
        bind (lift (assert_ v)) @@ fun assert_passed ->
        if assert_passed then ok () else lfail v

    let consume_pure e : (unit, 'fix) t =
      let open Syntax in
      let* v = apply_subst Fun.id e in
      assert_pure v

    let learn_eq expr v : (unit, 'fix) t =
     fun subst ->
      match Value.Expr.Subst.learn subst expr v with
      | None ->
          [%l.debug "Couldn't learn %a := %a" Value.Expr.pp expr Value.ppa v];
          lfail (Value.of_bool false) subst
      | Some subst ->
          let v', subst =
            Value.Expr.Subst.apply
              ~missing_var:(fun _ _ ->
                tool_bug
                  "Tool Bug: learned substitution does not cover expression's \
                   free variables.")
              subst expr
          in
          assert_pure (Value.sem_eq_untyped v v') subst

    let expose_subst () : (subst, 'fix) t = fun subst -> Result.ok (subst, subst)
    let from_raw_UNSAFE x = x
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
      ('a * Value.(sbool t) list) t =
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
