(** By default, [Symex] has no exposed notion of syntax for it's values. This
    module describes the interface for syntactic representations of values in a
    [Symex]. *)

module type Base = sig
  include Symex.Base

  (** An [Expr] is a syntactic representation of a symbolic value, on which
      syntactic manipulation can be performed (such as substitution). *)
  module type Expr = sig
    type 'a symex := 'a t

    (** An expression is a syntactic representation of Symex.Value. *)
    type t

    include Map.OrderedType with type t := t

    (** Retrieve the syntactic (untyped) representation of a value. Note that
        this assumes some identity substitution. *)
    val of_value : 'a Value.t -> t

    (** Retrieves the value representation of a syntactic object. This is unsafe
        because:
        - it jumps from the syntactic world to the symbolic world without
          applying a substitution. There is no guarantee that the resulting
          value makes sense in the current symbolic context.
        - the inferred type ['a] may not correspond to the actual type of the
          syntactic object. *)
    val to_value_unsafe : t -> 'a Value.t

    val subst : (t -> t symex) -> t -> t
  end
end
