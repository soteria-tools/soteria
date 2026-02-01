(** This module defines the minimal interface that symbolic values must
    implement to be used in Soteria's symbolic execution engine. *)

module type Expr = sig
  type 'a ty
  type 'a v
  type t [@@deriving show]

  (** Obtain a syntactic representation from a semantic value. This implicitly
      uses an identity substitution. *)
  val of_value : 'a v -> t

  (** Gets the type associated to a syntactic values.

      Note: this might be too restrictive for some applications with dynamic
      types? *)
  val ty : t -> 'a ty

  (** A susbtitution projects the syntactic to the semantic world. *)
  module Subst : sig
    (** Type of substitutions *)
    type t

    val empty : t
  end

  (** Applies a substitution to a syntactic representation to obtain a symbolic
      value. Should values be missing, fresh values are generated using the
      provided function. *)
  val subst :
    missing_var:(Var.t -> 'a ty -> 'a v) -> Subst.t -> t -> 'a v * Subst.t

  (** [learn θ e v] takes the substitution [θ], the expression [e] and the value
      [v] and completes [θ] into the unique substitution [θ'] such that
      [θ'(e) = v]. If there is zero or more than one such substitution, returns
      [None].

      FIXME: I'm not sure that's true. I believe that for the inputs:
      - [θ = [x -> 0]]
      - [v = 1]
      - [e = x] the function should return [Some θ] (i.e. not modify the
        substitution), and the consumption is in charge of checking the equality
        after. This function needs a better documentation. *)
  val learn : Subst.t -> t -> 'a v -> Subst.t option
end

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

  (* TODO: BEFORE END OF PR: remove iter_vars when soteria-c is updated to use
     Expr. *)

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

  module Expr : Expr with type 'a ty := 'a ty and type 'a v := 'a t
end
