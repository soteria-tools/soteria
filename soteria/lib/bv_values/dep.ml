open Soteria_std
module Var = Symex.Var

(** What a value {e depends on}, for the solver's relevance computation: a
    constraint must be re-checked together with every other constraint it may
    interact with, and two constraints may only interact if they share a
    dependency (see {!Hashset.relevant}). Substitution and quantification only
    care about {!Var}s, and should keep using [iter_vars]. *)
type t =
  | Var of Var.t
  | Sym of String.Interned.t
      (** An uninterpreted function applied to symbolic arguments. *)
  | App of String.Interned.t * int
      (** An uninterpreted function applied to concrete arguments, identified by
          the hash-consing tag of the application. *)
[@@deriving eq, ord, hash]

let pp ft = function
  | Var v -> Fmt.pf ft "V%a" Var.pp v
  | Sym f -> String.Interned.pp ft f
  | App (f, tag) -> Fmt.pf ft "%a#%d" String.Interned.pp f tag

module Set = Set.Make (struct
  type nonrec t = t

  let compare = compare
end)

(** A mutable set of dependencies, with the relevance check of the solver.
    Relevance is calculated with the following rules:

    - Two variables are related if they are equal.
    - Two ground applications of the same function are related if they have the
      same arguments.
    - A symbolic function application is related to all (symbolic {i and}
      ground) applications of the same function.
    - Anything else is unrelateed.

    A {e ground} application is one whose arguments are all literals (values
    built only of constants), so that syntactically distinct arguments are
    semantically distinct. *)
module Hashset : sig
  type dep := t
  type t

  val with_capacity : int -> t
  val add : t -> dep -> unit

  (** [add_check t d] adds [d] to [t], returning [true] if it wasn't there. *)
  val add_check : t -> dep -> bool

  val add_iter : t -> dep Iter.t -> unit

  (** [relevant t d] is [true] if [d] may interact with a dependency in [t]. *)
  val relevant : t -> dep -> bool
end = struct
  module Deps = Hashset.Make (struct
    type nonrec t = t

    let equal = equal
    let hash = hash
    let pp = pp
  end)

  module Syms = Hashset.Make (String.Interned)

  type t = {
    deps : Deps.t;
    ground : Syms.t;  (** functions with a ground application in [deps] *)
  }

  let with_capacity n =
    { deps = Deps.with_capacity n; ground = Syms.with_capacity n }

  let add_check t d =
    (match d with App (f, _) -> Syms.add t.ground f | Var _ | Sym _ -> ());
    Deps.add_check t.deps d

  let add t d = ignore (add_check t d)
  let add_iter t iter = iter (add t)

  let relevant t = function
    | Var _ as d -> Deps.mem t.deps d
    | App (f, _) as d -> Deps.mem t.deps d || Deps.mem t.deps (Sym f)
    | Sym f as d -> Deps.mem t.deps d || Syms.mem t.ground f
end
