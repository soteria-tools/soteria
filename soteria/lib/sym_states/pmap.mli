open Symex

module KeyS (Symex : Symex.Base) : sig
  module type S = sig
    type t

    include Stdlib.Map.OrderedType with type t := t

    type sbool_v := Symex.Value.sbool Symex.Value.t

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
    type 'a t
    type 'a serialized = (Key.t * 'a) list

    val empty : 'a t
    val syntactic_bindings : 'a t -> (Key.t * 'a) Seq.t
    val syntactic_mem : Key.t -> 'a t -> bool

    val pp :
      ?ignore:(Key.t * 'a -> bool) ->
      (Format.formatter -> 'a -> unit) ->
      Format.formatter ->
      'a t ->
      unit

    val pp_serialized :
      (Format.formatter -> 'a -> unit) ->
      Format.formatter ->
      'a serialized ->
      unit

    val serialize : ('a -> 'b) -> 'a t -> (Key.t * 'b) list

    val subst_serialized :
      ((Var.t -> Var.t) -> 'a -> 'b) ->
      (Var.t -> Var.t) ->
      'a serialized ->
      'b serialized

    val iter_vars_serialized :
      ('a -> (Var.t * 'b Symex.Value.ty -> unit) -> unit) ->
      'a serialized ->
      (Var.t * 'b Symex.Value.ty -> unit) ->
      unit

    val of_opt : 'a t option -> 'a t
    val to_opt : 'a t -> 'a t option

    val alloc :
      new_codom:'a ->
      'a t option ->
      (Key.t * 'a t option, 'err, 'fix list) Symex.Result.t

    val allocs :
      fn:('b -> Key.t -> ('a * 'k) Symex.t) ->
      els:'b list ->
      'a t option ->
      ('k list * 'a t option, 'err, 'fix list) Symex.Result.t

    val wrap :
      ('a option -> ('b * 'a option, 'err, 'fix) Symex.Result.t) ->
      Key.t ->
      'a t option ->
      ('b * 'a t option, 'err, 'fix serialized) Symex.Result.t

    val fold :
      ('acc -> Key.t * 'a -> ('acc, 'err, 'fix serialized) Symex.Result.t) ->
      'acc ->
      'a t option ->
      ('acc, 'err, 'fix serialized) Symex.Result.t

    val produce :
      ('inner_serialized -> 'inner_st option -> 'inner_st option Symex.t) ->
      'inner_serialized serialized ->
      'inner_st t option ->
      'inner_st t option Symex.t

    val consume :
      ('inner_serialized ->
      'inner_st option ->
      ( 'inner_st option,
        ([> Symex.lfail ] as 'a),
        'inner_serialized )
      Symex.Result.t) ->
      'inner_serialized serialized ->
      'inner_st t option ->
      ('inner_st t option, 'a, 'inner_serialized serialized) Symex.Result.t
  end
end

module Make (Symex : Symex.Base) (Key : KeyS(Symex).S) : S(Symex)(Key).S

module Make_patricia_tree
    (Symex : Symex.Base)
    (Key : KeyS(Symex).S_patricia_tree) : S(Symex)(Key).S

module Direct_access (Symex : Symex.Base) (Key : KeyS(Symex).S) :
  S(Symex)(Key).S

module Direct_access_patricia_tree
    (Symex : Symex.Base)
    (Key : KeyS(Symex).S_patricia_tree) : S(Symex)(Key).S

module Concrete (Symex : Symex.Base) (Key : Soteria_std.Ordered_type.S) :
  S(Symex)(Key).S
