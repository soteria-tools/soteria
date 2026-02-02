(** {1 State_monad - State Monad Transformer for Symbolic Execution}

    This module provides a state monad transformer that layers a stateful
    computation on top of the symbolic execution monad. This enables symbolic
    analysis with explicit state management (e.g., for heap, memory, or
    program state).

    {2 Key Concepts}

    {b State Transformer}: The resulting monad carries both a state value of
    type [st] and operates within the underlying symbolic execution monad.
    Branching in symbolic execution forks the state, giving each path its own
    copy.

    {b Lifting}: Operations from the base [Symex] monad can be lifted into the
    stateful monad. The syntax module provides convenient operators for this.

    {2 Usage}

    {[
      module State = struct
        type t = { heap : Heap.t; counter : int }
      end

      module MySymex = Symex.Make(MySolver)
      module Stateful = State_monad.Make(MySymex)(State)

      open Stateful.Syntax

      let increment_counter =
        let* state = get_state () in
        set_state { state with counter = state.counter + 1 }

      let allocate size =
        let*^ ptr = nondet (Value.t_ptr 64) in  (* lift from base Symex *)
        let* state = get_state () in
        let heap' = Heap.alloc state.heap ptr size in
        let* () = set_state { state with heap = heap' } in
        return ptr
    ]}
*)

open Soteria_std
module Compo_res = Symex.Compo_res

(** Base signature for symbolic execution (re-exported from Symex). *)
module type Base_sig = Symex.Base

(** {2 State Monad Signature} *)

(** Signature for the state monad transformer. *)
module type S = sig
  (** The type of state carried through computations. *)
  type st

  (** The underlying symbolic execution monad. *)
  module Symex : Base_sig

  (** Inherited from StateT transformer: includes [return], [bind], [map],
      [get_state], [set_state], [lift], etc. *)
  include module type of
      Monad.StateT_base
        (struct
          type t = st
        end)
        (Symex)

  (** Re-exports symbolic execution interface with the stateful monad type. *)
  include Base_sig with type 'a t := 'a t

  (** {3 Stateful Compositional Results} *)

  module Result : sig
    include
      module type of Result
        with type ('a, 'e, 'f) t = ('a, 'e, 'f) Compo_res.t t

    (** [get_state ()] retrieves the current state within a Result context. *)
    val get_state : unit -> (st, 'e, 'f) t

    (** [set_state st] replaces the current state within a Result context. *)
    val set_state : st -> (unit, 'e, 'f) t

    (** [run_with_state ~state computation] runs [computation] with initial
        [state], threading state through Ok and Error results. Missing results
        do not carry state (they represent missing preconditions). *)
    val run_with_state :
      state:st -> ('a, 'e, 'f) t -> ('a * st, 'e * st, 'f) Symex.Result.t
  end

  (** {3 Syntax Extensions} *)

  module Syntax : sig
    include module type of Syntax

    (** Bind from base Symex, lifting into stateful monad. *)
    val ( let*^ ) : 'a Symex.t -> ('a -> 'b t) -> 'b t

    (** Map from base Symex, lifting into stateful monad. *)
    val ( let+^ ) : 'a Symex.t -> ('a -> 'b) -> 'b t

    (** Bind from base Symex Result, lifting into stateful Result. *)
    val ( let**^ ) :
      ('a, 'e, 'f) Symex.Result.t ->
      ('a -> ('b, 'e, 'f) Result.t) ->
      ('b, 'e, 'f) Result.t

    (** Map from base Symex Result, lifting into stateful Result. *)
    val ( let++^ ) :
      ('a, 'e, 'f) Symex.Result.t -> ('a -> 'b) -> ('b, 'e, 'f) Result.t
  end
end

(** {2 Functor} *)

(** [Make(Symex)(State)] creates a state monad transformer over [Symex] with
    state type [State.t].

    The resulting monad has type [State.t -> ('a * State.t) Symex.t], meaning
    each computation takes an initial state and produces a result paired with
    a new state, all within the symbolic execution monad. *)
module Make
    (Sym : Symex.Base)
    (State : sig
      type t
    end) :
  S
    with module Value = Sym.Value
     and module Symex = Sym
     and type st = State.t
     and type 'a t = State.t -> ('a * State.t) Sym.t
