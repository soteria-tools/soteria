(** {1 Fuel Gauge - Execution Limits}

    This module provides fuel-based limiting for symbolic execution. Fuel
    controls both the depth (steps per branch) and breadth (number of branches)
    of exploration, preventing non-termination and managing resource usage.

    {2 Key Concepts}

    {b Step Fuel}: Limits how many symbolic execution steps can be taken along
    a single path. Each step roughly corresponds to one statement or operation.

    {b Branching Fuel}: Limits how many times execution can branch. Since the
    number of paths can grow exponentially (2^n for n branches), this provides
    crucial control over exploration breadth.

    {2 Usage}

    {[
      (* Create a fuel gauge with 1000 steps and 20 branches *)
      let fuel = { steps = Finite 1000; branching = Finite 20 }

      (* Use infinite fuel for exhaustive exploration *)
      let fuel = Fuel_gauge.infinite

      (* Run with fuel limits *)
      Symex.run ~fuel ~mode:UX my_process
    ]}
*)

(** Result of attempting to consume fuel. *)
type exhaust =
  | Exhausted      (** No fuel remaining *)
  | Not_exhausted  (** Fuel successfully consumed *)

(** A fuel value that can be finite or infinite. *)
module Fuel_value : sig
  type t =
    | Infinite  (** Unlimited fuel *)
    | Finite of int  (** Finite amount of fuel *)

  val pp : Format.formatter -> t -> unit

  (** [decrease fv n] subtracts [n] from [fv], clamping at 0. *)
  val decrease : t -> int -> t

  (** [geq fv n] returns true if [fv >= n]. Infinite is always >= any value. *)
  val geq : t -> int -> bool
end

(** Fuel gauge containing limits for both steps and branching. *)
type t = {
  steps : Fuel_value.t;
      (** Maximum steps per branch. Each step consumes one unit. *)
  branching : Fuel_value.t;
      (** Maximum number of branches. Each new branch consumes one unit.
          Note: Total paths can be exponential in this value! *)
}
[@@deriving show]

(** Infinite fuel gauge. Use for exhaustive exploration (may not terminate). *)
val infinite : t

(** [is_infinite fuel] returns true if both steps and branching are infinite. *)
val is_infinite : t -> bool

(** [consume_fuel_steps n gauge] consumes [n] steps of fuel.
    Returns [(Exhausted, _)] if insufficient fuel, otherwise
    [(Not_exhausted, updated_gauge)]. *)
val consume_fuel_steps : int -> t -> exhaust * t

(** [consume_branching n gauge] consumes [n] units of branching fuel.
    Returns [(Exhausted, _)] if insufficient fuel. *)
val consume_branching : int -> t -> exhaust * t

(** [take_branches list gauge] returns a prefix of [list] that can be explored
    within the branching fuel, along with the updated gauge.

    For example, if [branching = Finite 3] and [list] has 5 elements, returns
    the first 4 elements (taking 3 branches consumes branching fuel for each
    additional path beyond the first). *)
val take_branches : 'a list -> t -> 'a list * t

(** {2 Command-Line Interface}

    Utilities for parsing fuel options from command-line arguments using
    Cmdliner. *)
module Cli : sig
  (** [validate_or_exit result] unwraps [result] or exits with an error. *)
  val validate_or_exit : (t, string) result -> t

  (** Cmdliner converter for fuel values. Accepts "inf", "infinite", or integers. *)
  val fuel_value_conv : Fuel_value.t Cmdliner.Arg.conv

  (** [process_args ~default steps branching infinite_fuel] combines CLI options
      into a fuel gauge, validating for conflicts. *)
  val process_args :
    default:t ->
    Fuel_value.t option ->
    Fuel_value.t option ->
    bool ->
    (t, string) result

  (** [term ~default ()] creates a Cmdliner term for parsing fuel options.

      Provides the following CLI flags:
      - [--step-fuel N]: Steps per branch (env: SOTERIA_STEP_FUEL)
      - [--branching-fuel N]: Maximum branches (env: SOTERIA_BRANCHING_FUEL)
      - [--infinite-fuel]: Use infinite fuel (env: SOTERIA_INFINITE_FUEL) *)
  val term : default:t -> unit -> (t, string) result Cmdliner.Term.t
end
