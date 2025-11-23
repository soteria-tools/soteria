(** Reversible computation abstractions.

    This module provides interfaces and implementations for reversible
    state management, allowing computations to save checkpoints and
    backtrack to previous states. Supports mutable, in-place, immutable,
    and effect-based reversible computations. *)

(** Interface for mutable reversible state. *)
module type Mutable = sig
  type t

  val init : unit -> t
  val backtrack_n : t -> int -> unit
  val save : t -> unit
  val reset : t -> unit
end

(** Functor to create a mutable reversible state from a default value.
    The state is stored as a dynamic array, allowing backtracking by
    truncating to previous checkpoints. *)
module Make_mutable (M : sig
  type t

  val default : t
end) : sig
  include Mutable with type t = M.t Dynarray.t

  (** [set_default v] sets the default value used when initializing new states. *)
  val set_default : M.t -> unit
  (** [wrap f t] applies function [f] to the current state, updating it with
      the returned value. *)
  val wrap : (M.t -> 'a * M.t) -> t -> 'a
  (** [wrap_read f t] applies function [f] to the current state without
      modifying it. *)
  val wrap_read : (M.t -> 'a) -> t -> 'a
end = struct
  type t = M.t Dynarray.t

  let default = ref M.default

  (** [init ()] creates a new reversible state initialized with the default value. *)
  let init () =
    let d = Dynarray.create () in
    Dynarray.add_last d !default;
    d

  let set_default v = default := v

  (** [backtrack_n d n] removes the last [n] checkpoints from state [d]. *)
  let backtrack_n d n =
    let len = Dynarray.length d in
    Dynarray.truncate d (len - n)

  (** [save d] saves the current state as a new checkpoint. *)
  let save d = Dynarray.add_last d (Dynarray.get_last d)

  (** [reset d] clears all checkpoints and resets to the default value. *)
  let reset d =
    Dynarray.clear d;
    Dynarray.add_last d !default

  let wrap (f : M.t -> 'a * M.t) d =
    let e = Dynarray.pop_last d in
    let a, e' = f e in
    Dynarray.add_last d e';
    a

  let wrap_read (f : M.t -> 'a) d =
    let e = Dynarray.get_last d in
    f e
end

(** Interface for in-place reversible operations that operate on a global state. *)
module type In_place = sig
  (** [backtrack_n n] removes the last [n] checkpoints. *)
  val backtrack_n : int -> unit
  (** [save ()] saves the current state as a new checkpoint. *)
  val save : unit -> unit
  (** [reset ()] clears all checkpoints and resets to the default value. *)
  val reset : unit -> unit
end

(** Converts a mutable reversible state to an in-place interface using
    a lazy global state. *)
module Mutable_to_in_place (M : Mutable) = struct
  let state = lazy (M.init ())
  let save () = M.save (Lazy.force state)
  let backtrack_n n = M.backtrack_n (Lazy.force state) n
  let reset () = M.reset (Lazy.force state)
  let wrap (f : M.t -> 'a) () : 'a = f (Lazy.force state)
end

(** Functor to create an in-place reversible state from a default value.
    Provides a global state that can be saved and backtracked. *)
module Make_in_place (M : sig
  type t

  val default : t
end) =
struct
  module Mutable = Make_mutable (M)
  include Mutable_to_in_place (Mutable)

  (** [set_default v] sets the default value used when resetting. *)
  let set_default = Mutable.set_default
  (** [wrap_read f ()] applies [f] to the current state without modifying it. *)
  let wrap_read f () = wrap (Mutable.wrap_read f) ()
  (** [wrap f ()] applies [f] to the current state, updating it with the result. *)
  let wrap f () = wrap (Mutable.wrap f) ()
end

(** Interface for immutable reversible state. *)
module type Immutable = sig
  type t

  (** [init] is the initial state value. *)
  val init : t
  (** [backtrack_n t n] returns a state with the last [n] checkpoints removed. *)
  val backtrack_n : t -> int -> t
  (** [save t] saves the current state as a new checkpoint. *)
  val save : t -> t
  (** [reset t] returns the initial state. *)
  val reset : t -> t
end

module Effectful (M : sig
  type t
end) =
struct
  (** Effectful reversible computation. Note that this module is a functor, so
      that we can create any amount, we are not limited to a unique reversible
      effect.

      I.e. one can do
      {@ocaml[
        module Rev1 = Effectful (struct
          type t = int
        end)

        module Rev2 = Effectful (struct
          type t = string
        end)

        let computation () =
          Rev1.save ();
          Rev2.save ();
          Rev1.backtrack ()

        let () = Rev1.run ~init:0 @@ fun () -> Rev2.run ~init:"x" @@ computation
      ]} *)

  type _ Effect.t +=
    | Backtrack_n : int -> unit Effect.t
    | Save : unit Effect.t
    | Update : 'a. (M.t -> 'a * M.t) -> 'a Effect.t

  (** [backtrack_n n] removes the last [n] checkpoints. *)
  let backtrack_n n = Effect.perform (Backtrack_n n)
  (** [save ()] saves the current state as a new checkpoint. *)
  let save () = Effect.perform Save
  (** [wrap f ()] applies [f] to the current state, updating it with the result. *)
  let wrap f () = Effect.perform (Update f)

  (** [wrap_read f ()] applies [f] to the current state without modifying it. *)
  let wrap_read f () =
    let update s = (f s, s) in
    wrap update ()

  (** [run ~init f] executes the effectful computation [f] with initial state [init],
      handling backtrack, save, and update effects. *)
  let run ~(init : M.t) f =
    let state = Dynarray.create () in
    Dynarray.add_last state init;
    try f () with
    | effect Backtrack_n n, k ->
        let len = Dynarray.length state in
        Dynarray.truncate state (len - n);
        Effect.Deep.continue k ()
    | effect Save, k ->
        Dynarray.add_last state (Dynarray.get_last state);
        Effect.Deep.continue k ()
    | effect Update g, k ->
        let last = Dynarray.pop_last state in
        let result, new_st = g last in
        Dynarray.add_last state new_st;
        Effect.Deep.continue k result
end
