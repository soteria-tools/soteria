open Symex

module KeyS (Symex : Symex.Base) : sig
  module type S = sig
    type t

    include Stdlib.Map.OrderedType with type t := t

    type sbool_v := Symex.Value.(sbool t)

    val pp : Format.formatter -> t -> unit
    val sem_eq : t -> t -> sbool_v
    val fresh : unit -> t Symex.t
    val simplify : t -> t Symex.t
    val distinct : t list -> sbool_v
    val subst : (Var.t -> Var.t) -> t -> t
    val iter_vars : t -> 'a Symex.Value.ty Var.iter_vars
  end

  module type S_patricia_tree = sig
    include S

    val to_int : t -> int
  end
end

module S
    (Symex : Symex.Base)
    (Key : sig
      type t
    end) : sig
  module type S = sig
    type codom
    type t
    type codom_serialized
    type serialized = Key.t * codom_serialized

    type ('a, 'err) res :=
      t option -> (('a, 'err, serialized list) Compo_res.t * t option) Symex.t

    type ('a, 'err) codom_res :=
      codom option ->
      (('a, 'err, codom_serialized list) Compo_res.t * codom option) Symex.t

    val empty : t
    val syntactic_bindings : t -> (Key.t * codom) Seq.t
    val syntactic_mem : Key.t -> t -> bool
    val pp : ?ignore:(Key.t * codom -> bool) -> Format.formatter -> t -> unit
    val show : t -> string
    val pp_serialized : Format.formatter -> serialized -> unit
    val show_serialized : serialized -> string
    val serialize : t -> serialized list
    val subst_serialized : (Var.t -> Var.t) -> serialized -> serialized

    val iter_vars_serialized :
      serialized -> (Var.t * 'b Symex.Value.ty -> unit) -> unit

    val of_opt : t option -> t
    val to_opt : t -> t option
    val alloc : new_codom:codom -> (Key.t, 'err) res

    val allocs :
      fn:('a -> Key.t -> ('k * codom) Symex.t) ->
      els:'a list ->
      ('k list, 'err) res

    val wrap : Key.t -> ('a, 'err) codom_res -> ('a, 'err) res

    val fold :
      ('acc -> Key.t * codom -> ('acc, 'err, serialized list) Symex.Result.t) ->
      'acc ->
      t option ->
      ('acc, 'err, serialized list) Symex.Result.t

    val produce : serialized -> t option -> (unit * t option) Symex.t

    (* val consume :
      ('inner_serialized ->
      'inner_st option ->
      ( 'inner_st option,
        ([> Symex.lfail ] as 'a),
        'inner_serialized )
      Symex.Result.t) ->
      'inner_serialized serialized ->
      'inner_st t option ->
      ('inner_st t option, 'a, 'inner_serialized serialized) Symex.Result.t *)
  end
end

module Make
    (Symex : Symex.Base)
    (Key : KeyS(Symex).S)
    (Codom : Base.M(Symex).S) :
  S(Symex)(Key).S
    with type codom := Codom.t
     and type codom_serialized := Codom.serialized

module Make_patricia_tree
    (Symex : Symex.Base)
    (Key : KeyS(Symex).S_patricia_tree)
    (Codom : Base.M(Symex).S) :
  S(Symex)(Key).S
    with type codom := Codom.t
     and type codom_serialized := Codom.serialized

module Direct_access
    (Symex : Symex.Base)
    (Key : KeyS(Symex).S)
    (Codom : Base.M(Symex).S) :
  S(Symex)(Key).S
    with type codom := Codom.t
     and type codom_serialized := Codom.serialized

module Direct_access_patricia_tree
    (Symex : Symex.Base)
    (Key : KeyS(Symex).S_patricia_tree)
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
