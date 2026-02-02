(** {1 Abstract Analyses for Symbolic Execution}

    This module provides abstract domain analyses that can be combined with
    symbolic execution to improve constraint solving efficiency. These analyses
    track additional information about symbolic variables (such as value ranges
    or equality relationships) and use this information to simplify constraints
    before they reach the SMT solver.

    The analyses implement an incremental, reversible interface following
    {!Soteria_std.Reversible.Mutable}, enabling backtracking during symbolic
    exploration.

    {2 Overview}

    The main analysis signature [S] defines operations for:
    - {b Simplifying} constraints using tracked knowledge
    - {b Adding constraints} and extracting derived information
    - {b Filtering} possible values for variables
    - {b Encoding} analysis state back to SMT constraints

    Multiple analyses can be composed using {!Merge}, and a no-op analysis
    {!None} is provided as a base case.

    {2 Available Analyses}

    - {!Interval}: Tracks value ranges for bitvector variables using
      an interval domain with negative ranges, based on
      {{:https://ceur-ws.org/Vol-1617/paper8.pdf} this paper}.

    - {!Equality}: Tracks equality relationships using a union-find data
      structure, enabling term simplification via equivalence classes.

    {2 Usage Example}

    Analyses are typically composed and used within a solver:
    {[
      module Combined = Analyses.Merge(Analyses.Interval)(Analyses.Equality)

      let analysis = Combined.init ()
      let constraint' = Combined.simplify analysis some_constraint
      let updated, dirty_vars = Combined.add_constraint analysis constraint'
    ]}
*)

(** {2 Analysis Signature} *)

(** The signature for abstract analyses.

    An analysis tracks information about symbolic variables that can be used
    to simplify constraints and reduce solver queries. Analyses are mutable
    and support backtracking via the {!Soteria_std.Reversible.Mutable}
    interface. *)
module type S = sig
  include Soteria_std.Reversible.Mutable

  (** [simplify t v] simplifies the constraint [v] using the current knowledge
      base in [t], without modifying the analysis state.

      Returns a potentially simplified version of [v]. If the constraint is
      known to be true given current knowledge, returns [Svalue.Bool.v_true].
      If known to be false, returns [Svalue.Bool.v_false]. *)
  val simplify : t -> Svalue.t -> Svalue.t

  (** [add_constraint t v] incorporates the constraint [v] into the analysis
      state [t].

      Returns [(v', dirty)] where:
      - [v'] is a potentially simplified constraint (may be [true] if fully
        absorbed, or [false] if the constraint is unsatisfiable)
      - [dirty] is the set of variables whose tracked data changed, which
        may require re-checking satisfiability

      The analysis state [t] is mutated to include information from [v]. *)
  val add_constraint : t -> Svalue.t -> Svalue.t * Var.Set.t

  (** [filter t var ty values] filters an iterator of possible values for
      variable [var] of type [ty], keeping only those consistent with the
      analysis knowledge.

      Used during model generation to restrict candidate values to those
      within known ranges or equality classes. *)
  val filter : t -> Var.t -> Svalue.ty -> Svalue.t Iter.t -> Svalue.t Iter.t

  (** [encode ?vars t] returns an iterator of SMT constraints encoding the
      current analysis state.

      @param vars If provided, only encodes information relevant to these
                  variables. If [None], encodes all tracked information.

      These constraints can be added to the solver to communicate knowledge
      derived from the analysis. *)
  val encode : ?vars:Var.Hashset.t -> t -> Typed.sbool Typed.t Iter.t
end

(** {2 Analysis Combinators} *)

(** [Merge(A1)(A2)] creates a combined analysis that runs both [A1] and [A2]
    in sequence.

    The combined analysis:
    - Simplifies through both analyses in order
    - Adds constraints to both and unions the dirty variable sets
    - Filters through both analyses
    - Encodes by appending the constraints from both *)
module Merge (A1 : S) (A2 : S) : S

(** A no-op analysis that tracks nothing and passes all constraints through
    unchanged. Useful as a base case for composition. *)
module None : S

(** {2 Concrete Analyses} *)

(** {3 Interval Analysis}

    An interval analysis for bitvectors based on
    {{:https://ceur-ws.org/Vol-1617/paper8.pdf} Wrapped Intervals for
    Bitvector Arithmetic}.

    This analysis tracks:
    - A positive range [\[low, high\]] for each variable
    - A list of negative ranges (excluded subranges)

    The analysis can derive:
    - That constraints are redundant (already implied by ranges)
    - That constraints are unsatisfiable (range becomes empty)
    - Exact values when a range narrows to a singleton

    {4 Range Semantics}

    Intervals handle both signed and unsigned comparisons on bitvectors.
    For a variable [x] of size [n]:
    - Unsigned range [\[a, b\]] means [a ≤ᵤ x ≤ᵤ b]
    - Signed comparisons use the midpoint [2^{n-1}] as a boundary

    Negative ranges [\[a, b\]] exclude values: [¬(a ≤ x ≤ b)] *)
module Interval : S

(** {3 Equality Analysis}

    An equality analysis using union-find to track equivalence classes
    of symbolic values.

    When an equality [v1 = v2] is added, the analysis:
    1. Unifies [v1] and [v2] into the same equivalence class
    2. Chooses a representative based on a cost heuristic (preferring
       simpler terms like constants over complex expressions)

    During simplification, terms are replaced with their cheapest equivalent
    representative, potentially reducing expression complexity significantly.

    {4 Cost Heuristic}

    The cost function assigns higher costs to:
    - Variables (cost 3)
    - Floating-point operations (cost 3-5)
    - Constants and pointers (cost 1-2)

    When merging equivalence classes, the lower-cost term becomes the
    representative. *)
module Equality : S
