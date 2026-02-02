(** {1 Predicate List (Array-like) State Model}

    This module implements an indexed collection of state elements,
    similar to an array. Elements are accessed by symbolic integer indices
    and the collection can have an optional bound.

    {2 Overview}

    A predicate list maps symbolic integer indices to elements of type [Elem].
    It supports:
    - Symbolic index lookup (with case splitting on equality)
    - Optional bounds checking
    - Serialization to index-value pair predicates

    {2 Usage Example}

    {[
      module MyPlist = Plist.Make(Symex)(SInt)(ElemState)

      (* Create a fixed-size list *)
      let plist = MyPlist.create 10 ~new_codom:initial_elem in

      (* Access element at symbolic index *)
      let* result = MyPlist.wrap index some_elem_operation in
    ]}
*)

open Symex
open Compo_res

(** Signature for symbolic integers used as indices. *)
module SInt_sig (Symex : Symex.Base) : sig
  module type S = sig
    (** The symbolic integer type. *)
    type t

    include Stdlib.Map.OrderedType with type t := t

    (** Shorthand for symbolic boolean values. *)
    type sbool_v := Symex.Value.sbool Symex.Value.t

    val pp : Format.formatter -> t -> unit

    (** [sem_eq a b] returns symbolic boolean for [a = b]. *)
    val sem_eq : t -> t -> sbool_v

    (** [of_int n] creates a concrete index from integer [n]. *)
    val of_int : int -> t

    (** [in_range x (lo, hi)] checks if [lo <= x < hi]. *)
    val in_range : t -> t * t -> sbool_v

    (** [greater_or_equal x y] checks if [x >= y]. *)
    val greater_or_equal : t -> t -> sbool_v

    (** [subst f t] substitutes variables in index [t]. *)
    val subst : (Var.t -> Var.t) -> t -> t

    (** [iter_vars t f] iterates over variables in [t]. *)
    val iter_vars : t -> (Var.t * 'a Symex.Value.ty -> unit) -> unit
  end
end

(** [Make(Symex)(SInt)(Elem)] creates a predicate list with indices [SInt]
    and elements [Elem]. *)
module Make
    (Symex : Symex.Base)
    (SInt : SInt_sig(Symex).S)
    (Elem : Base.M(Symex).S) : sig
  (** The predicate list type: a map from indices to elements plus optional bound. *)
  type t = Elem.t Stdlib.Map.Make(SInt).t * SInt.t option

  (** State monad for the predicate list. *)
  module SM :
    State_monad.S with type Symex.t = Symex.t and type state = t option

  (** Serialized form: either a binding or a bound assertion. *)
  type serialized =
    | Ser_binding of (SInt.t * Elem.serialized)  (** Index-value pair *)
    | Ser_bound of SInt.t                         (** Size bound *)

  (** [pp' ?elem] creates a pretty-printer with custom element printer. *)
  val pp' : ?elem:(Format.formatter -> Elem.t -> unit) -> Format.formatter -> t -> unit

  val pp : Format.formatter -> t -> unit
  val pp_serialized : Format.formatter -> serialized -> unit

  (** [serialize t] converts to a list of binding and bound predicates. *)
  val serialize : t -> serialized list

  (** The empty index map. *)
  val empty : Elem.t Stdlib.Map.Make(SInt).t

  (** [create size ~new_codom] creates a list of [size] elements,
      each initialized to [new_codom].

      @raise Invalid_argument if [size <= 0] *)
  val create : int -> new_codom:Elem.t -> t

  (** [assert_exclusively_owned ()] checks that all indices up to the
      bound are owned. *)
  val assert_exclusively_owned : unit -> (unit, 'err, serialized list) SM.Result.t

  (** [wrap index f] applies element operation [f] at [index].

      Performs bounds checking and symbolic index resolution. *)
  val wrap :
    SInt.t ->
    ('b, 'err, Elem.serialized list) Elem.SM.Result.t ->
    ('b, 'err, serialized list) SM.Result.t

  (** [produce s] produces a serialized predicate into the list.

      - [Ser_bound]: Sets the bound (vanishes if already set)
      - [Ser_binding]: Adds the index-value pair *)
  val produce : serialized -> unit SM.t

  (** [subst_serialized f s] substitutes variables. *)
  val subst_serialized : (Var.t -> Var.t) -> SInt.t * Elem.serialized list * SInt.t option -> SInt.t * Elem.serialized list * SInt.t option

  (** [iter_vars_serialized s f] iterates over variables. *)
  val iter_vars_serialized : SInt.t * Elem.serialized list * SInt.t option -> (Var.t * 'a Symex.Value.ty -> unit) -> unit
end
