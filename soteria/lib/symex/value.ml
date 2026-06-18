(** This module defines the minimal interface that symbolic values must
    implement to be used in Soteria's symbolic execution engine. *)

module type Expr = sig
  (** [Expr.t] describes a {e syntactic} representation of a value of type
      ['a v]. Syntactic objects, unlike semantic values, can contain "free"
      variables and operations such as substitution have meaning.

      [Expr.t] objects are used in, for instance, assertions
      ({{!Soteria.Logic}[Logic]}). They can be substituted through a
      {{!Soteria.Symex.S.Producer}[Producer]} monad or learned through a
      {{!Soteria.Symex.S.Consumer}[Consumer]} monad. *)

  type 'a ty
  type 'a v
  type t [@@deriving show]

  (** Existentially-wrapped value and type. Because the syntactic representation
      [t] is monomorphic (it has erased its index), substitution can only
      recover a value up to this existential; callers re-type with {!cast_value}
      (using a type-equality witness). *)
  type packed_v = Packed : 'a v -> packed_v

  type packed_ty = PackedTy : 'a ty -> packed_ty

  (** Obtain a syntactic representation from a semantic value. This implicitly
      uses an identity substitution. *)
  val of_value : 'a v -> t

  (** Gets the type associated to a syntactic value. *)
  val ty : t -> packed_ty

  (** Convenience function *)
  val subst : (t -> packed_v) -> t -> packed_v

  (** A susbtitution projects the syntactic to the semantic world. *)
  module Subst : sig
    type expr := t

    (** Type of substitutions *)
    type t

    val pp : Format.formatter -> t -> unit
    val empty : t

    (** Generates a fresh value for a missing variable; polymorphic in the
        variable's kind (hence wrapped in a record). *)
    type missing = { missing : 'a. Var.t -> 'a ty -> 'a v }

    (** Applies a substitution to a syntactic representation to obtain a
        symbolic value. Should a variable binding be missing, fresh values are
        generated using the provided function. *)
    val apply : missing_var:missing -> t -> expr -> packed_v * t

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
        - If there are multiple substitutions that satisfy the requirement, this
          function can return any of them. This will lead to under-approximation
          when in UX mode, and to a probable consumption failure in UX mode.
        - If this function returns [None] when there exists a correct
          substitution, this will only lead to consumption failure. The function
          can always be extended to return a correct substitution. *)
    val learn : t -> expr -> 'a v -> t option
  end
end

module type S = sig
  (** The type of symbolic values, parameterized by their type. For example,
      [sbool t] represents a symbolic boolean.

      Values also expose an {!module:Expr} module for their syntactic
      representation. *)
  type 'a t

  (** Type of values. *)
  type 'a ty

  (** The type for booleans. *)
  type sbool

  (** Whether the given type is [sbool ty] *)
  val is_bool_ty : 'a ty -> bool

  (** [not b] returns the logical negation of boolean [b]. *)
  val not : sbool t -> sbool t

  val ppa : Format.formatter -> 'a t -> unit
  val sem_eq_untyped : 'a t -> 'b t -> sbool t

  (** [mk_var var ty] creates a symbolic variable with name [var] and type [ty].
  *)
  val mk_var : Var.t -> 'a ty -> 'a t

  (** [to_bool v] returns [Some b] if [v] is a concrete boolean [b], or [None]
      if [v] is symbolic. *)
  val to_bool : 'a t -> bool option

  (** [of_bool b] creates a concrete boolean value from [b]. *)
  val of_bool : bool -> sbool t

  (* TODO: in the future, I'd like to separate [S_with_syn] from [S]. I think
     everything is heavier because of the syntactic part, it'd be nice to make
     it optional. *)

  module Expr : Expr with type 'a ty := 'a ty and type 'a v := 'a t

  (** [cast_value ty pv] recovers the value wrapped in the existential [pv] at
      type [ty], using a type-equality witness, or [None] if [pv]'s type is not
      [ty]. This is how a substituted value (necessarily existential) is brought
      back to a statically-known type. *)
  val cast_value : 'a ty -> Expr.packed_v -> 'a t option

  (** [as_bool pv] recovers a boolean value from the existential [pv]; raises if
      [pv] is not a boolean (only sound on values known to be pure). *)
  val as_bool : Expr.packed_v -> sbool t
end
