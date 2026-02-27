(** @canonical *)
module Key = Pmap_intf.Key

(** @canonical *)
module S = Pmap_intf.M

module Make
    (Symex : Symex.Base)
    (Key : Key(Symex).S)
    (Codom : Base.M(Symex).S) :
  S(Symex)(Key).S with type codom := Codom.t and type codom_syn := Codom.syn

module Make_patricia_tree
    (Symex : Symex.Base)
    (Key : Key(Symex).S_patricia_tree)
    (Codom : Base.M(Symex).S) :
  S(Symex)(Key).S with type codom := Codom.t and type codom_syn := Codom.syn

module Direct_access
    (Symex : Symex.Base)
    (Key : Key(Symex).S)
    (Codom : Base.M(Symex).S) :
  S(Symex)(Key).S with type codom := Codom.t and type codom_syn := Codom.syn

module Direct_access_patricia_tree
    (Symex : Symex.Base)
    (Key : Key(Symex).S_patricia_tree)
    (Codom : Base.M(Symex).S) :
  S(Symex)(Key).S with type codom := Codom.t and type codom_syn := Codom.syn

module Concrete
    (Symex : Symex.Base)
    (Key : Soteria_std.Ordered_type.S)
    (Codom : Base.M(Symex).S) :
  S(Symex)(Pmap_intf.Ckey(Key)).S
    with type codom := Codom.t
     and type codom_syn := Codom.syn
