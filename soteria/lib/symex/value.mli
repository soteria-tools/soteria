(** {1 Symbolic Value Interface}

    This module defines the minimal interface that symbolic values must
    implement to be used with the symbolic execution engine.

    {2 Overview}

    The [S] signature specifies the core operations needed for symbolic
    execution:
    - Creating and manipulating boolean values
    - Variable creation and substitution
    - Pretty-printing for debugging

    Different backends can implement this interface with different
    representations (e.g., bitvector-based, abstract interpretation-based).

    {2 Type Parameters}

    The interface uses two type parameters:
    - ['a t]: A value of type ['a] (e.g., [sbool t] for boolean values)
    - ['a ty]: The type descriptor for type ['a]
*)

(** The signature for symbolic value implementations. *)
module type S = sig
  (** The type of symbolic values, parameterized by their semantic type.
      For example, [sbool t] represents a symbolic boolean. *)
  type +'a t

  (** Type descriptors for values. *)
  type +'a ty

  (** The semantic type for booleans. *)
  type sbool

  (** [not b] returns the logical negation of boolean [b]. *)
  val not : sbool t -> sbool t

  (** [ppa fmt v] pretty-prints value [v] to formatter [fmt].
      The 'a' stands for "any" - it prints any value type. *)
  val ppa : Format.formatter -> 'a t -> unit

  (** [iter_vars v f] iterates over all variables in value [v],
      calling [f (var, ty)] for each variable with its type. *)
  val iter_vars : 'a t -> 'b ty Var.iter_vars

  (** [subst f v] substitutes variables in [v] using function [f].
      Each variable [x] is replaced with [f x]. *)
  val subst : (Var.t -> Var.t) -> 'a t -> 'a t

  (** [mk_var var ty] creates a symbolic variable with name [var] and type [ty]. *)
  val mk_var : Var.t -> 'a ty -> 'a t

  (** [as_bool v] returns [Some b] if [v] is a concrete boolean [b],
      or [None] if [v] is symbolic. *)
  val as_bool : 'a t -> bool option

  (** [bool b] creates a concrete boolean value from [b]. *)
  val bool : bool -> sbool t
end
