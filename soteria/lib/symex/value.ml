module type Syn = sig
  type 'a ty
  type 'a v
  type t [@@deriving show]

  (** Obtain a syntactic representation from a semantic value. This implicitly
      uses an identity substitution. *)
  val of_value : 'a v -> t

  (** A susbtitution projects the syntactic to the semantic world. *)
  module Subst : sig
    type s := t

    (** Type of substitutions *)
    type t

    val empty : t

    (** Adds a binding to the substitution *)
    val add : s -> 'a v -> t -> t

    (** Find a binding in the substitution *)
    val find_opt : s -> t -> 'a v option
  end

  (** Applies a substitution to a syntactic representation to obtain a symbolic
      value. Should values be missing, fresh values are generated using the
      provided function. *)
  val subst :
    missing_var:(Var.t -> 'a ty -> 'a v) -> Subst.t -> t -> 'a v * Subst.t
end

module type S = sig
  type +'a t
  type +'a ty
  type sbool

  val not : sbool t -> sbool t
  val ppa : Format.formatter -> 'a t -> unit
  val iter_vars : 'a t -> 'b ty Var.iter_vars
  val subst : (Var.t -> Var.t) -> 'a t -> 'a t
  val mk_var : Var.t -> 'a ty -> 'a t
  val as_bool : 'a t -> bool option
  val bool : bool -> sbool t

  module Syn : Syn with type 'a v := 'a t and type 'a ty := 'a ty
end
