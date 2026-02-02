(** {1 Symex - Core Symbolic Execution Engine}

    This module provides the core symbolic execution monad and related utilities
    for performing symbolic analysis. The engine supports both under-approximate
    (UX) and over-approximate (OX) execution modes, path branching, fuel-based
    execution limits, and compositional reasoning via bi-abduction.

    {2 Key Concepts}

    {b Symbolic Execution Monad}: The type ['a t] represents a symbolic process
    that, when executed, may explore multiple paths and produce multiple results
    of type ['a], each paired with a path condition.

    {b Approximation Modes}:
    - {b UX (Under-Approximate)}: Guarantees no false positives. Unknown SAT
      results lead to branch pruning. Useful for bug finding.
    - {b OX (Over-Approximate)}: Guarantees no false negatives. Unknown SAT
      results are treated as potentially satisfiable. Useful for verification.

    {b Compositional Results}: Operations return {!Compo_res.t} which can be:
    - [Ok]: Successful execution
    - [Error]: A definite error was found
    - [Missing]: Frame inference needed (bi-abduction)

    {2 Usage Example}

    {[
      module Symex = Symex.Make(My_solver)
      open Symex.Syntax

      let example =
        let* x = Symex.nondet (Value.t_int 32) in
        let* () = Symex.assume [Value.BitVec.geq ~signed:false x (Value.BitVec.zero 32)] in
        if%sat Value.(x ==@ BitVec.zero 32) then
          Symex.Result.ok "is zero"
        else
          Symex.Result.ok "is positive"

      let results = Symex.Result.run ~mode:UX example
    ]}
*)

(** {2 Re-exported Modules} *)

module Approx = Approx
module Compo_res = Compo_res
module Fuel_gauge = Fuel_gauge
module Solver = Solver
module Solver_result = Solver_result
module Value = Value
module Var = Var

(** {2 Exceptions} *)

(** Raised when symbolic execution gives up in OX mode due to incompleteness
    (e.g., unsupported features). In UX mode, giving up simply prunes the path. *)
exception Gave_up of string

(** {2 Give-up Wrapper} *)

(** Wraps errors to distinguish between actual errors and give-ups. *)
module Or_gave_up : sig
  type 'err t =
    | E of 'err  (** An actual error *)
    | Gave_up of string  (** Execution gave up with a reason *)

  val pp : (Format.formatter -> 'err -> unit) -> Format.formatter -> 'err t -> unit
  val unwrap_exn : 'err t -> 'err
end

(** {2 Module Types} *)

(** Core symbolic execution interface. Provides the fundamental operations for
    symbolic execution including branching, assumptions, and assertions. *)
module type Core = sig
  module Value : Value.S

  (** A symbolic process that produces values of type ['a]. When executed, it
      may explore multiple paths, each producing a result paired with a path
      condition. *)
  type 'a t

  include Soteria_std.Monad.Base with type 'a t := 'a t

  (** Type alias for logical failure during consumption. Use this instead of
      [`Lfail] directly to avoid typos. *)
  type lfail = [ `Lfail of Value.sbool Value.t ]

  type 'a v := 'a Value.t
  type 'a vt := 'a Value.ty
  type sbool := Value.sbool

  (** [assume constraints] adds [constraints] to the current path condition.
      Paths where the constraints are unsatisfiable are pruned. *)
  val assume : sbool v list -> unit t

  (** [vanish ()] terminates the current path without producing a result.
      Useful for paths that should not contribute to the output. *)
  val vanish : unit -> 'a t

  (** [assert_ guard] checks if [guard] is guaranteed to hold.
      - In UX mode: returns [false] only if [not guard] is {b satisfiable}
      - In OX mode: returns [true] only if [not guard] is {b unsatisfiable}

      This is a non-branching operation that queries the solver. *)
  val assert_ : sbool v -> bool t

  (** [consume_pure guard] is similar to [if%sat guard then ok () else error (`Lfail guard)]
      but without branching:
      - In UX mode: equivalent to [assume [guard]] (errors are ignored)
      - In OX mode: equivalent to [assert_ guard] (returns error if assertion fails)

      Used for consuming pure preconditions in specifications. *)
  val consume_pure : sbool v -> (unit, [> lfail ], 'a) Compo_res.t t

  (** [consume_false ()] is [consume_pure (Value.bool false)] with a convenient signature.
      Always fails in OX mode, does nothing in UX mode. *)
  val consume_false : unit -> ('a, [> lfail ], 'b) Compo_res.t t

  (** [nondet ty] creates a fresh symbolic variable of type [ty]. *)
  val nondet : 'a vt -> 'a v t

  (** [simplify value] simplifies [value] using the current path condition.
      May use solver-based simplification or interval analysis. *)
  val simplify : 'a v -> 'a v t

  (** [fresh_var ty] creates a fresh variable identifier of type [ty] without
      creating a symbolic value. Lower-level than {!nondet}. *)
  val fresh_var : 'a vt -> Var.t t

  (** [branch_on guard ~then_ ~else_] explores both branches if feasible:
      - Executes [then_ ()] with [guard] added to the path condition
      - Executes [else_ ()] with [not guard] added to the path condition

      Infeasible branches are pruned via SAT checks. *)
  val branch_on :
    ?left_branch_name:string ->
    ?right_branch_name:string ->
    sbool v ->
    then_:(unit -> 'a t) ->
    else_:(unit -> 'a t) ->
    'a t

  (** [if_sure guard ~then_ ~else_] executes [then_ ()] only if [guard] is
      {b guaranteed} to hold (i.e., [not guard] is unsatisfiable). Otherwise
      executes [else_ ()].

      {b Warning}: Use with care. The [then_] branch must be semantically
      equivalent to [else_] when [guard] holds. This is an optimization to
      avoid unnecessary branching. *)
  val if_sure :
    ?left_branch_name:string ->
    ?right_branch_name:string ->
    sbool v ->
    then_:(unit -> 'a t) ->
    else_:(unit -> 'a t) ->
    'a t

  (** [branch_on_take_one guard ~then_ ~else_] is like {!branch_on} but in UX
      mode only takes one branch (preferring [then_] if feasible). In OX mode,
      behaves identically to {!branch_on}.

      Useful for reducing path explosion when only one witness is needed. *)
  val branch_on_take_one :
    ?left_branch_name:string ->
    ?right_branch_name:string ->
    sbool v ->
    then_:(unit -> 'a t) ->
    else_:(unit -> 'a t) ->
    'a t

  (** [give_up reason] abandons the current path due to incompleteness.
      - In UX mode: silently prunes the path
      - In OX mode: raises {!Gave_up} if the path is feasible

      Use when encountering unsupported features. *)
  val give_up : string -> 'a t

  (** [branches thunks] explores all branches in [thunks], subject to fuel limits.
      Each thunk is executed in its own path context. *)
  val branches : (unit -> 'a t) list -> 'a t

  (** {3 Fuel Management} *)

  (** [consume_fuel_steps n] consumes [n] steps of fuel. If fuel is exhausted,
      the current path is pruned and recorded as unexplored. *)
  val consume_fuel_steps : int -> unit t
end

(** Extended symbolic execution interface with utility functions. *)
module type Base = sig
  include Core

  (** [assert_or_error guard err] asserts [guard], returning [Error err] if it
      fails. Equivalent to:
      {[
        branch_on (not guard)
          ~then_:(fun () -> return (Compo_res.error err))
          ~else_:(fun () -> return (Compo_res.ok ()))
      ]} *)
  val assert_or_error :
    Value.sbool Value.t -> 'err -> (unit, 'err, 'f) Compo_res.t t

  (** [some_or_give_up reason opt] unwraps [opt] or gives up with [reason]. *)
  val some_or_give_up : string -> 'a option -> 'a t

  (** [all f xs] maps [f] over [xs] collecting results. *)
  val all : ('a -> 'b t) -> 'a list -> 'b list t

  (** Monadic fold over lists. *)
  val fold_list : 'a list -> init:'b -> f:('b -> 'a -> 'b t) -> 'b t

  (** Monadic fold over iterators. *)
  val fold_iter : 'a Iter.t -> init:'b -> f:('b -> 'a -> 'b t) -> 'b t

  (** Monadic fold over sequences. *)
  val fold_seq : 'a Seq.t -> init:'b -> f:('b -> 'a -> 'b t) -> 'b t

  (** Monadic iteration over lists. *)
  val iter_list : 'a list -> f:('a -> unit t) -> unit t

  (** Monadic iteration over iterators. *)
  val iter_iter : 'a Iter.t -> f:('a -> unit t) -> unit t

  (** Monadic map over lists. *)
  val map_list : 'a list -> f:('a -> 'b t) -> 'b list t

  (** {3 Compositional Results} *)

  (** Operations that return compositional results (Ok/Error/Missing). *)
  module Result : sig
    type nonrec ('ok, 'err, 'fix) t = ('ok, 'err, 'fix) Compo_res.t t

    val ok : 'ok -> ('ok, 'err, 'fix) t
    val error : 'err -> ('ok, 'err, 'fix) t
    val miss : 'fix list -> ('ok, 'err, 'fix) t

    (** [miss_no_fix ~reason ()] records a missing case without a fix suggestion.
        Logs the reason and adds to statistics. *)
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

  (** {3 Syntax Extensions} *)

  module Syntax : sig
    (** Bind for the symbolic monad *)
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

    (** Map for the symbolic monad *)
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    (** Bind for compositional results *)
    val ( let** ) :
      ('a, 'c, 'd) Result.t ->
      ('a -> ('b, 'c, 'd) Result.t) ->
      ('b, 'c, 'd) Result.t

    (** Map for compositional results *)
    val ( let++ ) : ('a, 'c, 'd) Result.t -> ('a -> 'b) -> ('b, 'c, 'd) Result.t

    (** Map error for compositional results *)
    val ( let+- ) : ('a, 'b, 'd) Result.t -> ('b -> 'c) -> ('a, 'c, 'd) Result.t

    (** Bind error for compositional results *)
    val ( let*- ) :
      ('a, 'b, 'c) Result.t ->
      ('b -> ('a, 'd, 'c) Result.t) ->
      ('a, 'd, 'c) Result.t

    (** Map missing for compositional results *)
    val ( let+? ) : ('a, 'b, 'c) Result.t -> ('c -> 'd) -> ('a, 'b, 'd) Result.t

    (** Branching syntax for use with PPX *)
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

(** Full symbolic execution interface with execution functions. *)
module type S = sig
  include Base

  (** {3 Solver Pool Information} *)

  module Solver_pool : sig
    (** Total number of solver instances created. *)
    val total_created : unit -> int

    (** Number of solver instances currently available in the pool. *)
    val total_available : unit -> int
  end

  type 'a v := 'a Value.t
  type sbool := Value.sbool

  (** [run ~mode process] executes [process] and returns all terminal states,
      each paired with its path condition.

      @param fuel Optional fuel gauge to limit execution depth/breadth
      @param mode Approximation mode (UX or OX)
      @raise Gave_up In OX mode if execution gives up on a feasible path *)
  val run :
    ?fuel:Fuel_gauge.t -> mode:Approx.t -> 'a t -> ('a * sbool v list) list

  (** Same as {!run} but also returns execution statistics. *)
  val run_with_stats :
    ?fuel:Fuel_gauge.t ->
    mode:Approx.t ->
    'a t ->
    ('a * sbool v list) list Stats.with_stats

  (** Same as {!run} but requires being called within {!Stats.As_ctx.with_stats}.
      Use when running multiple executions with shared statistics. *)
  val run_needs_stats :
    ?fuel:Fuel_gauge.t -> mode:Approx.t -> 'a t -> ('a * sbool v list) list

  (** {3 Result Execution} *)

  module Result : sig
    include module type of Result

    (** [run ~mode process] executes a process returning compositional results.
        Give-ups are wrapped in {!Or_gave_up.t} rather than raising exceptions.

        @param fuel Optional fuel gauge
        @param fail_fast Stop after first error if true
        @param mode Approximation mode *)
    val run :
      ?fuel:Fuel_gauge.t ->
      ?fail_fast:bool ->
      mode:Approx.t ->
      ('ok, 'err, 'fix) t ->
      (('ok, 'err Or_gave_up.t, 'fix) Compo_res.t * Value.sbool Value.t list) list

    val run_with_stats :
      ?fuel:Fuel_gauge.t ->
      ?fail_fast:bool ->
      mode:Approx.t ->
      ('ok, 'err, 'fix) t ->
      (('ok, 'err Or_gave_up.t, 'fix) Compo_res.t * Value.sbool Value.t list) list
      Stats.with_stats

    val run_needs_stats :
      ?fuel:Fuel_gauge.t ->
      ?fail_fast:bool ->
      mode:Approx.t ->
      ('ok, 'err, 'fix) t ->
      (('ok, 'err Or_gave_up.t, 'fix) Compo_res.t * Value.sbool Value.t list) list
  end
end

(** {2 Statistics Keys}

    Keys for querying execution statistics. All keys are prefixed with "soteria."
    to avoid collisions with user-defined statistics. *)
module StatKeys : sig
  (** Total execution time in seconds. Type: {!Stats.Float} *)
  val exec_time : string

  (** Time spent in SAT solving. Type: {!Stats.Float} *)
  val sat_time : string

  (** Number of SAT solver calls. Type: {!Stats.Int} *)
  val sat_checks : string

  (** Number of SAT calls returning Unknown. Type: {!Stats.Int} *)
  val sat_unknowns : string

  (** Number of branches not explored due to fuel exhaustion. Type: {!Stats.Int} *)
  val unexplored_branches : string

  (** Number of branches explored. Type: {!Stats.Int} *)
  val branches : string

  (** Total steps taken across all branches. Type: {!Stats.Int} *)
  val steps : string

  (** Reasons for giving up (incompleteness). Type: {!Stats.StrSeq} *)
  val give_up_reasons : string

  (** Reasons for missing without fix. Type: {!Stats.StrSeq} *)
  val miss_without_fix : string
end

(** {2 Functor} *)

(** [Make(Solver)] creates a symbolic execution engine using [Solver] for
    constraint solving. The solver must implement {!Solver.Mutable_incremental}. *)
module Make (Sol : Solver.Mutable_incremental) :
  S with module Value = Sol.Value

(** {2 Substitution Utilities}

    Utilities for variable substitution during compositional reasoning. *)
module Substs : sig
  module type From_iter = sig
    type t
    type 'a ty
    type 'a symex

    (** Creates a substitution from an iterator over variables and their types.
        Each variable is mapped to a fresh variable of the same type. *)
    val from_iter : 'a ty Var.iter_vars -> t symex
  end

  (** Immutable substitution map from variables to variables. *)
  module Subst : sig
    include Map.S with type key = Var.t

    val pp : Format.formatter -> Var.t t -> unit

    (** [substitute_extensible ~f ~subst x] substitutes variable [x] using [subst],
        extending [subst] with a fresh mapping if [x] is not present. Returns
        the result of applying [f] and the extended substitution. *)
    val substitute_extensible :
      f:(Var.t -> Var.t -> 'a) -> subst:Var.t t -> Var.t -> 'a * Var.t t

    (** Converts a substitution map to a function. *)
    val to_fn : Var.t t -> Var.t -> Var.t

    module From_iter (Symex : S) :
      From_iter
        with type t := Var.t t
         and type 'a ty := 'a Symex.Value.ty
         and type 'a symex := 'a Symex.t
  end

  (** Mutable substitution using hash tables. More efficient for large substitutions. *)
  module Subst_mut : sig
    include Hashtbl.S with type key = Var.t

    val add : 'a t -> key -> 'a -> unit

    val substitute_extensible :
      f:(Var.t -> Var.t -> 'a) -> subst:Var.t t -> Var.t -> 'a * Var.t t

    module From_iter (Symex : S) :
      From_iter
        with type t := Var.t t
         and type 'a ty := 'a Symex.Value.ty
         and type 'a symex := 'a Symex.t
  end

  (** Bidirectional substitution for compositional reasoning. Maintains both
      forward (original -> fresh) and backward (fresh -> original) mappings. *)
  module Bi_subst : sig
    type t

    (** Creates an empty bidirectional substitution. *)
    val empty : unit -> t

    module From_iter (Symex : S) :
      From_iter
        with type t := t
         and type 'a ty := 'a Symex.Value.ty
         and type 'a symex := 'a Symex.t

    (** Returns true if the substitution is empty. *)
    val is_empty : t -> bool

    (** [forward subst var] returns the fresh variable for [var]. *)
    val forward : t -> Var.t -> Var.t

    (** [backward subst var] returns the original variable for [var],
        creating a new mapping if needed. *)
    val backward : t -> Var.t -> Var.t
  end
end
