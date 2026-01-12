(** @canonical *)
module Key = Pmap_intf.Key

(** @canonical *)
module S = Pmap_intf.M

module Make (Symex : Symex.Base) (Key : Key(Symex).S) :
  S(Symex).S with type key := Key.t and type key_syn := Key.syn

module Make_patricia_tree
    (Symex : Symex.Base)
    (Key : Key(Symex).S_patricia_tree) :
  S(Symex).S with type key := Key.t and type key_syn := Key.syn

module Direct_access (Symex : Symex.Base) (Key : Key(Symex).S) :
  S(Symex).S with type key := Key.t and type key_syn := Key.syn

module Direct_access_patricia_tree
    (Symex : Symex.Base)
    (Key : Key(Symex).S_patricia_tree) :
  S(Symex).S with type key := Key.t and type key_syn := Key.syn

module Concrete (Symex : Symex.Base) (Key : Soteria_std.Ordered_type.S) :
  S(Symex).S with type key := Key.t and type key_syn := Key.t
