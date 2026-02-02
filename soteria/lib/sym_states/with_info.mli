(** {1 State with Metadata}

    This module wraps a state model to attach metadata/info to each value.
    The info is preserved across operations and propagated through fixes.

    {2 Overview}

    This wrapper is useful for attaching debugging information, source
    locations, or other metadata to state components without modifying
    the underlying state logic.

    The info is:
    - Preserved when wrapping/unwrapping state
    - Attached to serialized predicates
    - Merged when producing (first info wins)

    {2 Usage Example}

    {[
      module InfoMem = With_info.Make(Symex)(SourceLoc)(MemState)

      (* Info is automatically propagated through operations *)
      let* result = InfoMem.wrap some_mem_operation in
    ]}
*)

open Symex

(** [Make(Symex)(Info)(B)] wraps state [B] with metadata of type [Info.t]. *)
module Make
    (Symex : Symex.Base)
    (Info : sig
      type t
      val pp : t Fmt.t
    end)
    (B : Base.M(Symex).S) : sig
  (** A value with optional attached info. *)
  type ('a, 'info) with_info = {
    node : 'a;          (** The wrapped value *)
    info : 'info option (** Optional metadata *)
  }
  [@@deriving show]

  (** The wrapped state type. *)
  type t = (B.t, Info.t) with_info [@@deriving show]

  (** [pp' ?inner ?info] creates a custom pretty-printer. *)
  val pp' :
    ?inner:(Format.formatter -> B.t -> unit) ->
    ?info:(Format.formatter -> Info.t -> unit) ->
    t Fmt.t

  val pp : Format.formatter -> t -> unit

  (** State monad for the wrapped state. *)
  module SM :
    State_monad.S with type Symex.t = Symex.t and type state = t option

  (** Serialized form also carries info. *)
  type serialized = (B.serialized, Info.t) with_info [@@deriving show]

  (** [serialize t] attaches the current info to all serialized predicates. *)
  val serialize : t -> serialized list

  (** [subst_serialized f s] substitutes variables in the inner predicate. *)
  val subst_serialized : (Var.t -> Var.t) -> serialized -> serialized

  (** [iter_vars_serialized s f] iterates over variables in inner. *)
  val iter_vars_serialized : serialized -> (Var.t * 'a Symex.Value.ty -> unit) -> unit

  (** [wrap f] lifts an inner state operation, preserving info.

      Missing fixes are wrapped with the current info. *)
  val wrap :
    ('a, 'err, B.serialized list) B.SM.Result.t ->
    ('a, 'err, serialized list) SM.Result.t

  (** [produce s] produces a serialized predicate.

      Info is merged: existing info is kept, new info used if none exists. *)
  val produce : serialized -> unit SM.t
end
