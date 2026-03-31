(** A symbolic map abstraction. The main function of the
    {{!S.S}symbolic map module type} is {{!S.S.find_opt}find_opt}, which allows
    symbolically looking up a key in the map (possibly branching), and returns
    the value at that key if any, along with the key to be used for future
    {i concrete} operations on the map. *)

module Key = S_map_intf.Key
module S = S_map_intf.M

(** Lifts a concrete key type into the symbolic realm. *)
module Mk_concrete_key (Symex : Symex.Base) (K : Soteria_std.Ordered_type.S) :
  Key(Symex).S with type t = K.t

(** Makes a symbolic map. The algorithm for {{!S.S.find_opt}find_opt} will first
    look for a syntactic match, and otherwise attempt checking for equality on
    all keys. *)
module Make (Symex : Symex.Base) (Key : Key(Symex).S) : S(Symex)(Key).S

(** Same as {!Make}, but backed by a
    {{:https://ocaml.org/p/patricia-tree/latest/doc/index.html}Patricia Tree},
    which may offer performance benefits. *)
module Make_patricia_tree
    (Symex : Symex.Base)
    (Key : Key(Symex).S_patricia_tree) : S(Symex)(Key).S

(** Makes a symbolic map. The algorithm for {{!S.S.find_opt}find_opt} will first
    look for a syntactic match. If none is found, it will check if the given key
    is {{!Key.S.distinct}distinct} from all other existing keys (in which case
    [None] is returned) before iterating over all entries. *)
module Direct_access (Symex : Symex.Base) (Key : Key(Symex).S) : S(Symex)(Key).S

(** Same as {!Direct_access}, but backed by a
    {{:https://ocaml.org/p/patricia-tree/latest/doc/index.html}Patricia Tree},
    which may offer performance benefits. *)
module Direct_access_patricia_tree
    (Symex : Symex.Base)
    (Key : Key(Symex).S_patricia_tree) : S(Symex)(Key).S

(** Creates a "symbolic" map that relies purely on syntactic equality. Only
    sound to use if the keys of the map are invariant under interpretations of
    the symbolic variables. See {!Mk_concrete_key} for a way to lift concrete
    keys into the symbolic realm. *)
module Concrete (Symex : Symex.Base) (Key : Key(Symex).S) : S(Symex)(Key).S
