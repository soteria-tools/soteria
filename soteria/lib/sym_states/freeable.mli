(** {1 Freeable State Model}

    This module wraps a state model to add explicit deallocation tracking.
    It distinguishes between "alive" (usable) and "freed" (deallocated) states.

    {2 Overview}

    The freeable wrapper transforms any state [I] into a state that can be:
    - [Alive inner]: Contains an inner state that can be used normally
    - [Freed]: Has been deallocated; any access is a use-after-free error

    This enables detection of:
    - Use-after-free: Accessing a [Freed] state
    - Double-free: Freeing an already [Freed] state

    {2 Usage Example}

    {[
      module FreeMem = Freeable.Make(Symex)(MemoryState)

      (* Wrap operations to check liveness *)
      let* result = FreeMem.wrap (MemoryState.load addr) in

      (* Free the resource *)
      let* () = FreeMem.free () in
      (* Now state is Freed; further access returns UseAfterFree error *)
    ]}
*)

open Symex

(** A value that can be either freed or alive with content ['a]. *)
type 'a freeable =
  | Freed           (** The resource has been deallocated *)
  | Alive of 'a     (** The resource is alive with value *)

(** [pp_freeable pp_a] creates a pretty-printer for freeable values. *)
val pp_freeable : 'a Fmt.t -> 'a freeable Fmt.t

(** [Make(Symex)(I)] wraps state [I] to track deallocation. *)
module Make
    (Symex : Symex.Base)
    (I : sig
      include Base.M(Symex).S

      (** [assert_exclusively_owned st] required for [free] operation. *)
      val assert_exclusively_owned :
        t option -> (unit, 'err, serialized list) Symex.Result.t
    end) : sig
  (** The wrapped state type. *)
  type t = I.t freeable [@@deriving show]

  (** [pp' ?inner ?info] creates a custom pretty-printer. *)
  val pp' : ?inner:(Format.formatter -> I.t -> unit) -> t Fmt.t

  val pp : Format.formatter -> t -> unit

  (** Serialized form wraps inner serialized. *)
  type serialized = I.serialized freeable [@@deriving show]

  (** State monad for freeable state. *)
  module SM :
    State_monad.S with type Symex.t = Symex.t and type state = t option

  (** [serialize t] serializes the freeable state. *)
  val serialize : t -> serialized list

  (** [subst_serialized f s] substitutes variables. *)
  val subst_serialized : (Var.t -> Var.t) -> serialized -> serialized

  (** [iter_vars_serialized s f] iterates over variables. *)
  val iter_vars_serialized : serialized -> (Var.t * 'a Symex.Value.ty -> unit) -> unit

  (** [wrap f] lifts an inner state operation to freeable.

      Returns [`UseAfterFree] error if state is [Freed]. *)
  val wrap :
    ('a, 'err, I.serialized list) I.SM.Result.t ->
    ('a, 'err, serialized list) SM.Result.t

  (** [free ()] deallocates the resource.

      Requires exclusive ownership of the inner state.
      After freeing, the state becomes [Freed]. *)
  val free : unit -> (unit, 'err, serialized list) SM.Result.t

  (** [produce s] produces predicate [s] into the state.

      - [Freed]: Sets state to freed (vanishes if already alive)
      - [Alive inner]: Produces into inner (vanishes if freed) *)
  val produce : serialized -> unit SM.t
end
