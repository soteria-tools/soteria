(** Symbolic maps from a key type to a symbolic-state codomain.

    A {e pmap} ("partial map") is the workhorse symbolic state model for
    mapping symbolic keys (e.g. memory locations) to
    {{!Soteria.Sym_states.Base.M.S} state components} (e.g. memory blocks). It
    is the {{!Soteria.Data.S_map} symbolic map} abstraction lifted to the
    symbolic-state world: each {{!Make} variant} differs only in how it
    searches for a key, with a uniform interface described by the {!S}
    module type. *)

(** @canonical Pmap_intf.Key *)
module Key = Pmap_intf.Key

(** @canonical Pmap_intf.M *)
module S = Pmap_intf.M

(** Builds a symbolic map. On [find_opt], looks for a syntactic match first and
    otherwise branches on equality with each existing key. *)
module Make
    (Symex : Symex.Base)
    (Key : Key(Symex).S)
    (Codom : Base.M(Symex).S) :
  S(Symex)(Key).S with type codom := Codom.t and type codom_syn := Codom.syn

(** Same as {!Make}, but backed by a
    {{:https://ocaml.org/p/patricia-tree/latest/doc/index.html}Patricia tree},
    which may offer performance benefits when the key set is large. *)
module Make_patricia_tree
    (Symex : Symex.Base)
    (Key : Key(Symex).S_patricia_tree)
    (Codom : Base.M(Symex).S) :
  S(Symex)(Key).S with type codom := Codom.t and type codom_syn := Codom.syn

(** Builds a symbolic map that avoids branching when possible. On [find_opt],
    looks for a syntactic match first; if none is found, checks whether the
    requested key is {{!Key.S.distinct}distinct} from every existing key (in
    which case the search returns [None] without branching) before falling back
    to a full equality search. *)
module Direct_access
    (Symex : Symex.Base)
    (Key : Key(Symex).S)
    (Codom : Base.M(Symex).S) :
  S(Symex)(Key).S with type codom := Codom.t and type codom_syn := Codom.syn

(** Same as {!Direct_access}, but backed by a
    {{:https://ocaml.org/p/patricia-tree/latest/doc/index.html}Patricia tree}.
*)
module Direct_access_patricia_tree
    (Symex : Symex.Base)
    (Key : Key(Symex).S_patricia_tree)
    (Codom : Base.M(Symex).S) :
  S(Symex)(Key).S with type codom := Codom.t and type codom_syn := Codom.syn

(** Builds a symbolic map keyed by a {e concrete} ordered type. Lookups use
    plain structural equality; no symbolic branching ever happens on the keys.
    Only sound when the keys are invariant under interpretation of the symbolic
    variables (typically, when the keys are concrete program identifiers). *)
module Concrete
    (Symex : Symex.Base)
    (Key : Soteria_std.Ordered_type.S)
    (Codom : Base.M(Symex).S) :
  S(Symex)(Pmap_intf.Ckey(Key)).S
    with type codom := Codom.t
     and type codom_syn := Codom.syn
