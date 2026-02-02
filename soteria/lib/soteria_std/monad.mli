(** Monad interfaces and implementations.

    This module provides a comprehensive collection of monad abstractions:
    - Basic monad signatures ({!Base}, {!Base2})
    - Extended monad interfaces with derived operations ({!S}, {!S2})
    - Syntax extensions for convenient monadic programming ({!Syntax}, {!Syntax2})
    - Common monad implementations (Id, Option, Result, List, Seq, State)
    - Monad transformers (ResultT, StateT)
    - Generic monadic fold and map functions

    {1 Monad Hierarchy}

    {[
      Base ─────> S (via Extend)
        │           │
        │           └─> Syntax
        v
      Base2 ────> S2 (via Extend2)
        │           │
        │           └─> Syntax2
        v
      Ty3 (for three-parameter types)
    ]}

    {1 Usage Example}

    {[
      open Monad.OptionM.Syntax

      let safe_divide x y =
        let* y = if y = 0 then None else Some y in
        Some (x / y)

      let result =
        let* a = safe_divide 10 2 in
        let* b = safe_divide a 5 in
        Some (a + b)
      (* result = Some 6 *)
    ]} *)

(** {1 Basic Monad Interface} *)

(** Basic interface for a monad with one type parameter.

    This is the minimal monad interface, providing [return], [bind], and [map]. *)
module type Base = sig
  (** The monadic type. *)
  type 'a t

  (** [return x] injects value [x] into the monad. *)
  val return : 'a -> 'a t

  (** [bind m f] sequences monadic computation [m] with [f].

      Also known as [>>=] or "flatMap". *)
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  (** [map m f] applies [f] to the result of [m].

      Also known as [<$>] or "fmap". *)
  val map : 'a t -> ('a -> 'b) -> 'b t
end

(** {1 Monadic Fold} *)

(** Functor to create a monadic fold type for a foldable container.

    @param M The monad module
    @param F The foldable container module *)
module FoldM (M : Base) (F : Foldable.S) : sig
  (** Type of a monadic fold function.

      [folder] folds over elements of type ['a] in container [F.t],
      accumulating a result of type ['b] within monad [M.t]. *)
  type ('a, 'b) folder = 'a F.t -> init:'b -> f:('b -> 'a -> 'b M.t) -> 'b M.t
end

(** [foldM ~return ~bind ~fold xs ~init ~f] is a generic monadic fold.

    Folds over [xs] using [fold], threading the accumulator through
    monadic function [f].

    @param return The monad's return function
    @param bind The monad's bind function
    @param fold The container's fold function
    @param xs The container to fold over
    @param init Initial accumulator value
    @param f Monadic folding function *)
val foldM :
  return:('a -> 'b) ->
  bind:('b -> ('a -> 'b) -> 'b) ->
  fold:('c -> init:'a -> f:('a -> 'd -> 'a) -> 'a) ->
  'c ->
  init:'e ->
  f:('e -> 'd -> 'b) ->
  'b

(** [all ~return ~bind fn xs] maps [fn] over [xs] and collects results.

    Generic version that works with any monad given [return] and [bind].

    @return A monadic list of results in the same order as [xs] *)
val all :
  return:('a list -> 'b) ->
  bind:('c -> ('d -> 'b) -> 'b) ->
  ('d -> 'c) ->
  'd list ->
  'b

(** {1 Monad Syntax} *)

(** Syntax extension for monads.

    Provides [let*] for bind and [let+] for map. *)
module type Syntax = sig
  type 'a t

  (** [let* x = m in f x] is equivalent to [bind m f]. *)
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  (** [let+ x = m in f x] is equivalent to [map m f]. *)
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
end

(** {1 Complete Monad Interface} *)

(** Complete monad interface with derived operations and syntax.

    Extends {!Base} with:
    - [all]: Map and collect results
    - [fold_list]: Monadic fold over lists
    - [Syntax]: Convenient binding operators *)
module type S = sig
  include Base

  (** [all fn xs] maps [fn] over list [xs] and collects all results.

      Equivalent to [sequence (List.map fn xs)] in Haskell. *)
  val all : ('a -> 'b t) -> 'a list -> 'b list t

  (** [fold_list ~init ~f xs] folds [f] over [xs] monadically.

      @param init Initial accumulator
      @param f Monadic folding function
      @param xs List to fold over *)
  val fold_list : init:'a -> f:('a -> 'elem -> 'a t) -> 'elem list -> 'a t

  (** Syntax module for convenient monadic programming. *)
  module Syntax : Syntax with type 'a t := 'a t
end

(** {1 Extend Functor} *)

(** Functor to extend a basic monad with derived operations.

    Given a {!Base} implementation, produces a full {!S} implementation
    with [all], [fold_list], and [Syntax]. *)
module Extend (Base : Base) : S with type 'a t = 'a Base.t

(** {1 Two-Parameter Monad Interface} *)

(** Basic interface for a monad with two type parameters.

    Used for monads like [Result] that have both a success and error type.
    Provides operations for both the success path ([bind], [map]) and
    the error path ([bind_error], [map_error]). *)
module type Base2 = sig
  (** The monadic type with success type ['a] and error type ['b]. *)
  type ('a, 'b) t

  (** [ok x] injects success value [x] into the monad. *)
  val ok : 'a -> ('a, 'b) t

  (** [bind m f] sequences on the success path. *)
  val bind : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t

  (** [map m f] applies [f] to successful results. *)
  val map : ('a, 'b) t -> ('a -> 'c) -> ('c, 'b) t

  (** [error e] injects error value [e] into the monad. *)
  val error : 'b -> ('a, 'b) t

  (** [bind_error m f] sequences on the error path. *)
  val bind_error : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t

  (** [map_error m f] applies [f] to error values. *)
  val map_error : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t
end

(** Functor to create monadic fold for two-parameter monads.

    @param M The two-parameter monad
    @param F The foldable container *)
module FoldM2 (M : Base2) (F : Foldable.S) : sig
  type ('elem, 'a, 'b) folder =
    'elem F.t -> init:'a -> f:('a -> 'elem -> ('a, 'b) M.t) -> ('a, 'b) M.t
end

(** {1 Three-Parameter Type} *)

(** Interface for a type with three type parameters.

    Used for types like [Compo_res.t] that have success, error, and missing. *)
module type Ty3 = sig
  type ('a, 'b, 'c) t
end

(** Functor to create monadic fold for three-parameter types.

    @param M The three-parameter type
    @param F The foldable container *)
module FoldM3 (M : Ty3) (F : Foldable.S) : sig
  type ('elem, 'a, 'b, 'c) folder =
    'elem F.t ->
    init:'a ->
    f:('a -> 'elem -> ('a, 'b, 'c) M.t) ->
    ('a, 'b, 'c) M.t
end

(** {1 Two-Parameter Monad Syntax} *)

(** Syntax extension for two-parameter monads.

    Provides operators for both success and error paths:
    - [let*], [let+]: Success path (like {!Syntax})
    - [let/], [let-]: Error path *)
module type Syntax2 = sig
  type ('a, 'b) t

  (** [let* x = m in f x] binds on the success path. *)
  val ( let* ) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t

  (** [let+ x = m in f x] maps on the success path. *)
  val ( let+ ) : ('a, 'b) t -> ('a -> 'c) -> ('c, 'b) t

  (** [let/ e = m in f e] binds on the error path. *)
  val ( let/ ) : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t

  (** [let- e = m in f e] maps on the error path. *)
  val ( let- ) : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t
end

(** {1 Complete Two-Parameter Monad Interface} *)

(** Complete interface for two-parameter monads.

    Extends {!Base2} with derived operations and syntax for both
    success and error paths. *)
module type S2 = sig
  include Base2

  (** [fold_list ~init ~f xs] folds [f] over [xs] monadically.

      Stops on first error and returns it. *)
  val fold_list :
    init:'a -> f:('a -> 'elem -> ('a, 'b) t) -> 'elem list -> ('a, 'b) t

  (** [all fn xs] maps [fn] over [xs] and collects successful results.

      Stops on first error and returns it. *)
  val all : ('a -> ('b, 'c) t) -> 'a list -> ('b list, 'c) t

  (** Syntax module for two-parameter monadic programming. *)
  module Syntax : Syntax2 with type ('a, 'b) t := ('a, 'b) t
end

(** Functor to extend a basic two-parameter monad.

    Given a {!Base2} implementation, produces a full {!S2} implementation. *)
module Extend2 (Base : Base2) : S2 with type ('a, 'b) t = ('a, 'b) Base.t

(** {1 Standard Monad Implementations} *)

(** Identity monad.

    The trivial monad where ['a t = 'a]. Useful as a base case for
    monad transformers.

    {[
      Id.return 42 = 42
      Id.bind 42 (fun x -> x + 1) = 43
    ]} *)
module Id : S with type 'a t = 'a

(** Result transformer.

    Adds error handling to any monad [M]. The resulting type is
    [('a, 'b) Result.t M.t].

    @param M The base monad to transform *)
module ResultT (M : Base) : Base2 with type ('a, 'b) t = ('a, 'b) Result.t M.t

(** List monad.

    Models non-deterministic computation where each step can produce
    multiple results.

    {[
      let* x = [1; 2] in
      let* y = [10; 20] in
      [x + y]
      (* = [11; 21; 12; 22] *)
    ]} *)
module ListM : S with type 'a t = 'a list

(** Result monad.

    Standard error-handling monad. Computation short-circuits on
    first error.

    {[
      let* x = Ok 1 in
      let* y = Error "oops" in
      Ok (x + y)
      (* = Error "oops" *)
    ]} *)
module ResultM : S2 with type ('a, 'b) t = ('a, 'b) result

(** Option monad.

    Models computations that may fail without an error value.
    Short-circuits on [None].

    {[
      let* x = Some 1 in
      let* y = None in
      Some (x + y)
      (* = None *)
    ]} *)
module OptionM : S with type 'a t = 'a option

(** Sequence monad.

    Lazy version of the list monad using [Seq.t]. *)
module SeqM : S with type 'a t = 'a Seq.t

(** Iterator monad.

    Non-deterministic computation using the [Iter.t] iterator type. *)
module IterM : S with type 'a t = 'a Iter.t

(** {1 State Monad} *)

(** State monad functor.

    Creates a state monad with a fixed state type.

    @param State Module with the state type *)
module StateM (State : sig
  type t
end) : S with type 'a t = State.t -> 'a * State.t

(** {1 State Transformer} *)

(** Base state transformer (without derived operations).

    @param State Module with the state type
    @param M The base monad to transform *)
module StateT_base
    (State : sig
      type t
    end)
    (M : Base) : sig
  (** Stateful computation returning ['a] *)
  type 'a t = State.t -> ('a * State.t) M.t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : 'a t -> ('a -> 'b) -> 'b t

  (** [get_state ()] returns the current state. *)
  val get_state : unit -> State.t t

  (** [set_state s] replaces the state with [s]. *)
  val set_state : State.t -> unit t

  (** [map_state f] applies [f] to transform the state. *)
  val map_state : (State.t -> State.t) -> unit t

  (** [run_with_state ~state x] runs computation [x] with initial [state].

      @return The result and final state, wrapped in monad [M] *)
  val run_with_state : state:State.t -> 'a t -> ('a * State.t) M.t

  (** [with_state ~state x] runs [x] with [state], then restores original state.

      Useful for temporarily using a different state. *)
  val with_state : state:State.t -> 'a t -> 'a t

  (** [lift m] lifts a computation from the base monad [M]. *)
  val lift : 'a M.t -> 'a t
end

(** State transformer functor with derived operations.

    @param State Module with the state type
    @param M The base monad to transform *)
module StateT
    (State : sig
      type t
    end)
    (M : Base) : sig
  include S with type 'a t = State.t -> ('a * State.t) M.t

  val get_state : unit -> State.t t
  val set_state : State.t -> unit t
  val map_state : (State.t -> State.t) -> unit t
  val run_with_state : state:State.t -> 'a t -> ('a * State.t) M.t
  val with_state : state:State.t -> 'a t -> 'a t
  val lift : 'a M.t -> 'a t
end

(** {1 Polymorphic State Transformer} *)

(** Polymorphic state transformer.

    Like {!StateT} but the state type is a parameter of the computation
    type rather than fixed at functor application.

    @param M The base monad to transform *)
module StateT_p (M : Base) : sig
  (** Computation returning ['a] with state type ['state]. *)
  type ('a, 'state) t = 'state -> ('a * 'state) M.t

  val return : 'a -> ('a, 'state) t
  val bind : ('a, 'state) t -> ('a -> ('b, 'state) t) -> ('b, 'state) t
  val map : ('a, 'state) t -> ('a -> 'b) -> ('b, 'state) t

  (** [get_state ()] returns the current state. *)
  val get_state : unit -> ('state, 'state) t

  (** [set_state s] replaces the state with [s]. *)
  val set_state : 'state -> (unit, 'state) t

  (** [map_state f] applies [f] to transform the state. *)
  val map_state : ('state -> 'state) -> (unit, 'state) t

  (** [lift m] lifts a computation from the base monad [M]. *)
  val lift : 'a M.t -> ('a, 'state) t

  (** Syntax module with additional [let*^] and [let+^] for lifting. *)
  module Syntax : sig
    val ( let* ) : ('a, 'st) t -> ('a -> ('b, 'st) t) -> ('b, 'st) t
    val ( let+ ) : ('a, 'st) t -> ('a -> 'b) -> ('b, 'st) t

    (** [let*^ x = m in f x] lifts [m] then binds. *)
    val ( let*^ ) : 'a M.t -> ('a -> ('b, 'st) t) -> ('b, 'st) t

    (** [let+^ x = m in f x] lifts [m] then maps. *)
    val ( let+^ ) : 'a M.t -> ('a -> 'b) -> ('b, 'st) t
  end
end
