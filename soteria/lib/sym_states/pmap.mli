(** @canonical *)
module Key = Pmap_intf.Key

(** @canonical *)
module S = Pmap_intf.M

module Make
    (Symex : Symex.Base)
    (Key : Key(Symex).S)
    (Codom : Base.M(Symex).S) :
  S(Symex)(Key).S
    with type codom := Codom.t
     and type codom_serialized := Codom.serialized

module Make_patricia_tree
    (Symex : Symex.Base)
    (Key : Key(Symex).S_patricia_tree)
    (Codom : Base.M(Symex).S) :
  S(Symex)(Key).S
    with type codom := Codom.t
     and type codom_serialized := Codom.serialized

module Direct_access
    (Symex : Symex.Base)
    (Key : Key(Symex).S)
    (Codom : Base.M(Symex).S) :
  S(Symex)(Key).S
    with type codom := Codom.t
     and type codom_serialized := Codom.serialized

module Direct_access_patricia_tree
    (Symex : Symex.Base)
    (Key : Key(Symex).S_patricia_tree)
    (Codom : Base.M(Symex).S) :
  S(Symex)(Key).S
    with type codom := Codom.t
     and type codom_serialized := Codom.serialized

module Concrete
    (Symex : Symex.Base)
    (Key : Soteria_std.Ordered_type.S)
    (Codom : Base.M(Symex).S) :
  S(Symex)(Key).S
    with type codom := Codom.t
     and type codom_serialized := Codom.serialized
