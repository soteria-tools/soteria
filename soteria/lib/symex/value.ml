module type Syn = sig
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
  type +'a t
  type +'a ty
  type sbool

  val not : sbool t -> sbool t
  val sem_eq_untyped : 'a t -> 'b t -> sbool t
  val ppa : Format.formatter -> 'a t -> unit
  val iter_vars : 'a t -> 'b ty Var.iter_vars
  val subst : (Var.t -> Var.t) -> 'a t -> 'a t
  val mk_var : Var.t -> 'a ty -> 'a t
  val as_bool : 'a t -> bool option
  val bool : bool -> sbool t

  module Syn : Syn with type 'a v := 'a t and type 'a ty := 'a ty
end
