(** {1 Bi-Abduction Support}

    This module implements bi-abduction for symbolic execution, enabling
    automatic frame inference. Bi-abduction answers the question:

    {i Given a precondition and a postcondition, what additional resources
    (the "frame") must be added to make the precondition imply the
    postcondition?}

    {2 Overview}

    In separation logic terms, bi-abduction solves:

    {[ H * ?anti-frame ⊢ H' * ?frame ]}

    where [H] is the current heap, [H'] is the required heap, and we need
    to find both the anti-frame (what to add to [H]) and frame (what's
    left over).

    This module wraps a base state type [B] to track:
    - The current state (of type [B.t option])
    - A list of "fixes" (predicates that were added via bi-abduction)

    {2 Usage}

    Bi-abduction is used during symbolic execution when a memory access
    requires ownership that isn't currently held. Instead of failing,
    the system:

    1. Proposes "fixes" (additional predicates to assume)
    2. Produces those fixes into the state
    3. Retries the operation
    4. Records the fixes for later use (e.g., generating preconditions)

    {b Warning}: Bi-abduction is only sound in under-approximate (UX) mode.
    Do not use in over-approximate (OX) mode.

    {2 Example}

    {[
      module MyBiAbd = Bi_abd.Make(Symex)(MyBaseState)

      (* Wrap a potentially-failing operation *)
      let result = MyBiAbd.wrap ~fuel:2 some_operation state in
      (* If operation returns Missing, fixes are applied and retried *)
    ]}
*)

(** Pretty-print a bi-abduction state (state + fixes). *)
val pp_bi_state :
  (Format.formatter -> 'st -> unit) ->
  (Format.formatter -> 'fixes -> unit) ->
  Format.formatter ->
  'st option * 'fixes list ->
  unit

(** [Make(Symex)(B)] creates a bi-abduction wrapper around base state [B]. *)
module Make (Symex : Symex.Base) (B : Base.M(Symex).S) : sig
  (** {2 Types} *)

  (** The bi-abduction state: a base state plus accumulated fixes.

      - [fst]: The underlying state (or [None] if empty)
      - [snd]: List of predicates that were added via bi-abduction *)
  type t = B.t option * B.serialized list

  (** Serialized predicates (same as underlying state). *)
  type serialized = B.serialized

  (** State monad over bi-abduction state. *)
  module SM :
    State_monad.S with type Symex.t = Symex.t and type state = t option

  (** {2 Conversions} *)

  (** Convert [None] to empty state with no fixes. *)
  val of_opt : t option -> t

  (** Convert empty state with no fixes to [None]. *)
  val to_opt : t -> t option

  (** Extract the state and fixes as a tuple. *)
  val expose : t -> B.t option * B.serialized list

  (** {2 Pretty Printing} *)

  (** Pretty-print with custom formatters for inner state and serialized. *)
  val pp' :
    ?inner:(Format.formatter -> B.t -> unit) ->
    ?serialized:(Format.formatter -> B.serialized -> unit) ->
    Format.formatter ->
    t ->
    unit

  val pp : Format.formatter -> t -> unit
  val show : t -> string

  (** {2 Core Operations} *)

  (** [wrap ?fuel f] wraps a fallible operation with bi-abduction.

      When [f] returns [Missing fix_choices], this function:
      1. Branches over the possible fixes
      2. Produces each fix into the state
      3. Records the fix in the accumulated fixes list
      4. Retries [f] with remaining fuel

      @param fuel Maximum retries after applying fixes (default: 1).
                  Must be positive.

      If [f] succeeds ([Ok]) or fails ([Error]), the result is returned
      directly. If fuel is exhausted, the execution vanishes.

      {b Note}: This is the key bi-abduction mechanism. It automatically
      extends the state with missing predicates and tracks what was added. *)
  val wrap :
    ?fuel:int ->
    ('v, 'err, B.serialized list) B.SM.Result.t ->
    ('v, 'err, serialized list) SM.Result.t

  (** [wrap_no_fail f] wraps an infallible operation.

      Simply runs [f] on the underlying state and preserves the fixes. *)
  val wrap_no_fail : 'a B.SM.t -> 'a SM.t

  (** [produce ser] produces a serialized predicate into the state.

      This directly adds the predicate without going through bi-abduction
      (no fix tracking). *)
  val produce : serialized -> unit SM.t
end
