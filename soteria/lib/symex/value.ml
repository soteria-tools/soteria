(** This module defines the minimal interface that symbolic values must
    implement to be used in Soteria's symbolic execution engine. *)

module type Expr = sig
  type 'a ty
  type 'a v
  type t [@@deriving show]

  (** Obtain a syntactic representation from a semantic value. This implicitly
      uses an identity substitution. *)
  val of_value : 'a v -> t

  (* NOTE: Being able to get a type from an expresion might be too restrictive
     for some applications with dynamic types? *)

  (** Gets the type associated to a syntactic values. *)
  val ty : t -> 'a ty

  (** Convenience function *)
  val subst : (t -> 'a v) -> t -> 'b v

  (** A susbtitution projects the syntactic to the semantic world. *)
  module Subst : sig
    type expr := t

    (** Type of substitutions *)
    type t

    val pp : Format.formatter -> t -> unit
    val empty : t

    (** Applies a substitution to a syntactic representation to obtain a
        symbolic value. Should a variable binding be missing, fresh values are
        generated using the provided function. *)
    val apply : missing_var:(Var.t -> 'a ty -> 'a v) -> t -> expr -> 'a v * t

    (** [learn θ e v] takes the substitution [θ], the expression [e] and the
        value [v] and tries to complete [θ] into a substitution [θ'] such that
        [θ'(e) = v].

        The only requirement from this function is that, if this function
        returns [Some θ'], then [θ'] covers all variables in [e]. However, all
        implementations of this function should be sound:
        - If this function returns a substitution that does not cover all
          variables in [e], the client may crash (but not result to an
          unsoundness).
        - If this function returns a wrong substitution (where [θ'(e) != v]),
          this will not result in an unsoundness, but may prevent successful
          consumption.
        - If there are more than one substitution that satisfies the
          requirement, this function can return any of them. This will lead to
          under-approximation when in UX mode, and to a probable consumption
          failure in UX mode.
        - If this function returns [None] when there exists a correct
          substitution, this will only lead to consumption failure. The function
          can always be extended to return a correct substitution. *)
    val learn : t -> expr -> 'a v -> t option
  end
end

module type S = sig
  (** The type of symbolic values, parameterized by their type. For example,
      [sbool t] represents a symbolic boolean. *)
  type +'a t

  (** Type of values. *)
  type +'a ty

  (** The type for booleans. *)
  type sbool

  (** Whether the given type is [sbool ty] *)
  val is_bool_ty : 'a ty -> bool

  (** [not b] returns the logical negation of boolean [b]. *)
  val not : sbool t -> sbool t

  val ppa : Format.formatter -> 'a t -> unit

  (* TODO: BEFORE END OF PR: remove iter_vars when soteria-c is updated to use
     Expr. *)

  (** [iter_vars v f] iterates over all variables in value [v], calling
      [f (var, ty)] for each variable with its type. *)
  val iter_vars : 'a t -> 'b ty Var.iter_vars

  val sem_eq_untyped : 'a t -> 'b t -> sbool t

  (** [subst f v] substitutes variables in [v] using function [f]. Each variable
      [x] is replaced with [f x]. *)
  val subst : (Var.t -> Var.t) -> 'a t -> 'a t

  (** [mk_var var ty] creates a symbolic variable with name [var] and type [ty].
  *)
  val mk_var : Var.t -> 'a ty -> 'a t

  (** [to_bool v] returns [Some b] if [v] is a concrete boolean [b], or [None]
      if [v] is symbolic. *)
  val to_bool : 'a t -> bool option

  (** [of_bool b] creates a concrete boolean value from [b]. *)
  val of_bool : bool -> sbool t

  module Expr : Expr with type 'a ty := 'a ty and type 'a v := 'a t
end
