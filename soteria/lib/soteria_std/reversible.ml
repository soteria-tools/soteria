(** Reversible computation abstractions. *)

(** Interface for mutable reversible state. *)
module type Mutable = sig
  type t

  (** Create a new reversible state initialized with the default value. *)
  val init : unit -> t

  (** Remove the last [n] checkpoints from state. *)
  val backtrack_n : t -> int -> unit

  (** Save the current state as a new checkpoint. *)
  val save : t -> unit

  (** Clear all checkpoints and reset to the default value. *)
  val reset : t -> unit
end

(** Functor to create a mutable reversible state from a default value. *)
module Make_mutable (M : sig
  type t

  val default : t
end) : sig
  include Mutable with type t = M.t Dynarray.t

  (** Set the default value used when initializing new states. *)
  val set_default : M.t -> unit

  (** Apply function [f] to the current state, updating it with the returned
      value. *)
  val wrap : (M.t -> 'a * M.t) -> t -> 'a

  (** Apply function [f] to the current state without modifying it. *)
  val wrap_read : (M.t -> 'a) -> t -> 'a
end = struct
  type t = M.t Dynarray.t

  let default = ref M.default

  let init () =
    let d = Dynarray.create () in
    Dynarray.add_last d !default;
    d

  let set_default v = default := v

  let backtrack_n d n =
    let len = Dynarray.length d in
    Dynarray.truncate d (len - n)

  let save d = Dynarray.add_last d (Dynarray.get_last d)

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

(** Interface for effectful reversible operations that operate on a state
    captured by an algebraic effect.

    There is no [init] or [reset] here, initialisation and resetting is handled
    by [run] at the start and end of the computation. *)
module type Effectful = sig
  (** Type of the reversible state. May be mutable! *)
  type t

  (** Remove the last [n] checkpoints. *)
  val backtrack_n : int -> unit

  (** Save the current state as a new checkpoint. *)
  val save : unit -> unit

  (** Apply function [f] to the current state, may modify the state in place if
      it is mutable. *)
  val wrap : (t -> 'a) -> unit -> 'a

  (** Runs an effectful reversible computation [f] with a fresh mutable state.
  *)
  val run : (unit -> 'a) -> 'a
end

(** Provides the same interface as the input, but backed by a resource pool.

    When initialising a new state, it acquires one from the pool, taking
    existing resource if any. This produces an effectful reversible state: using
    [run] ensures to release the resource back to the pool at the end of the
    computation.

    This functor owns its own resource pool, so multiple applications of this
    functor will create independent pools. *)
module Mutable_to_pooled (M : Mutable) : sig
  include Effectful with type t = M.t

  val total_resources : unit -> int
  val available_resources : unit -> int
end = struct
  module Pool = Resource_pool.Make (M)

  let pool = Pool.create_pool ()

  type t = M.t

  type _ Effect.t +=
    | Backtrack_n : int -> unit Effect.t
    | Save : unit Effect.t
    | Wrap : 'a. (M.t -> 'a) -> 'a Effect.t

  let backtrack_n n = Effect.perform (Backtrack_n n)
  let save () = Effect.perform Save
  let wrap f () = Effect.perform (Wrap f)

  let run f =
    let state, auth = Pool.acquire pool in
    let apply state f = Pool.apply ~auth state f in
    apply state M.reset;
    (* Being conservative and resetting state on acquisition *)
    let r =
      try f () with
      | effect Backtrack_n n, k ->
          apply state (fun state -> M.backtrack_n state n);
          Effect.Deep.continue k ()
      | effect Save, k ->
          apply state M.save;
          Effect.Deep.continue k ()
      | effect Wrap g, k ->
          let result = apply state g in
          Effect.Deep.continue k result
    in
    apply state M.reset;
    Pool.release pool auth state;
    r

  let total_resources () = Pool.total_resources pool
  let available_resources () = Pool.available_resources pool
end

(** Converts a mutable reversible state to an effectful interface *)
module Mutable_to_effectful (M : Mutable) : Effectful with type t = M.t = struct
  type t = M.t

  type _ Effect.t +=
    | Backtrack_n : int -> unit Effect.t
    | Save : unit Effect.t
    | Wrap : 'a. (M.t -> 'a) -> 'a Effect.t

  let backtrack_n n = Effect.perform (Backtrack_n n)
  let save () = Effect.perform Save
  let wrap f () = Effect.perform (Wrap f)

  let run f =
    let state = M.init () in
    let r =
      try f () with
      | effect Backtrack_n n, k ->
          M.backtrack_n state n;
          Effect.Deep.continue k ()
      | effect Save, k ->
          M.save state;
          Effect.Deep.continue k ()
      | effect Wrap g, k ->
          let result = g state in
          Effect.Deep.continue k result
    in
    M.reset state;
    r
end

module Make_effectful (M : sig
  type t
end) =
struct
  (** Effectful reversible computation for a value of type [t]. Note that this
      module is a functor, so that we can create any amount, we are not limited
      to a unique reversible effect.

      I.e. one can do
      {@ocaml[
        module Rev1 = Make_effectful (struct
          type t = int
        end)

        module Rev2 = Make_effectful (struct
          type t = string
        end)

        let computation () =
          Rev1.save ();
          Rev2.save ();
          Rev1.backtrack ()

        let () = Rev1.run ~init:0 @@ fun () -> Rev2.run ~init:"x" @@ computation
      ]} *)

  type t = M.t

  type _ Effect.t +=
    | Backtrack_n : int -> unit Effect.t
    | Save : unit Effect.t
    | Update : 'a. (M.t -> 'a * M.t) -> 'a Effect.t

  (** Remove the last [n] checkpoints. *)
  let backtrack_n n = Effect.perform (Backtrack_n n)

  (** Save the current state as a new checkpoint. *)
  let save () = Effect.perform Save

  (** Apply [f] to the current state, updating it with the result. *)
  let wrap f () = Effect.perform (Update f)

  (** Apply [f] to the current state without modifying it. *)
  let wrap_read f () =
    let update s = (f s, s) in
    wrap update ()

  (** Execute the effectful computation [f] with initial state [init], handling
      backtrack, save, and update effects. *)
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
