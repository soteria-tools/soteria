(** This module defines the minimal interface that symbolic values must
    implement to be used in Soteria's symbolic execution engine. *)

module type S = sig
  (** The type of symbolic values, parameterized by their type. For example,
      [sbool t] represents a symbolic boolean. *)
  type +'a t

  (** Type of values. *)
  type +'a ty

  (** The type for booleans. *)
  type sbool

  (** [not b] returns the logical negation of boolean [b]. *)
  val not : sbool t -> sbool t

  val ppa : Format.formatter -> 'a t -> unit

  (** [iter_vars v f] iterates over all variables in value [v], calling
      [f (var, ty)] for each variable with its type. *)
  val iter_vars : 'a t -> 'b ty Var.iter_vars

  (** [subst f v] substitutes variables in [v] using function [f]. Each variable
      [x] is replaced with [f x]. *)
  val subst : (Var.t -> Var.t) -> 'a t -> 'a t

  (** [mk_var var ty] creates a symbolic variable with name [var] and type [ty].
  *)
  val mk_var : Var.t -> 'a ty -> 'a t

  (** [as_bool v] returns [Some b] if [v] is a concrete boolean [b], or [None]
      if [v] is symbolic. *)
  val as_bool : 'a t -> bool option

  (** [bool b] creates a concrete boolean value from [b]. *)
  val bool : bool -> sbool t
end
