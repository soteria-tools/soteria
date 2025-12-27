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

module type MapS = sig
  type key
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  val mem : key -> 'a t -> bool
  val find_opt : key -> 'a t -> 'a option
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val to_seq : 'a t -> (key * 'a) Seq.t
  val bindings : 'a t -> (key * 'a) list
end

module S (Symex : Symex.Base) : sig
  module type S = sig
    module M : MapS

    type 'a t = 'a M.t
    type 'a serialized = (M.key * 'a) list

    val pp :
      ?ignore:(M.key * 'a -> bool) ->
      (Format.formatter -> 'a -> unit) ->
      Format.formatter ->
      'a t ->
      unit

    val pp_serialized :
      (Format.formatter -> 'a -> unit) ->
      Format.formatter ->
      'a serialized ->
      unit

    val serialize : ('a -> 'b) -> 'a t -> (M.key * 'b) list

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
      (M.key * 'a t option, 'err, 'fix list) Symex.Result.t

    val allocs :
      fn:('b -> M.key -> ('a * 'k) Symex.t) ->
      els:'b list ->
      'a t option ->
      ('k list * 'a t option, 'err, 'fix list) Symex.Result.t

    val wrap :
      ('a option -> ('b * 'a option, 'err, 'fix) Symex.Result.t) ->
      M.key ->
      'a t option ->
      ('b * 'a t option, 'err, 'fix serialized) Symex.Result.t

    val fold :
      ('acc -> M.key * 'a -> ('acc, 'err, 'fix serialized) Symex.Result.t) ->
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

module Make (Symex : Symex.Base) (Key : KeyS(Symex).S) :
  S(Symex).S with type M.key = Key.t

module Make_patricia_tree
    (Symex : Symex.Base)
    (Key : KeyS(Symex).S_patricia_tree) : S(Symex).S with type M.key = Key.t

module Direct_access (Symex : Symex.Base) (Key : KeyS(Symex).S) :
  S(Symex).S with type M.key = Key.t

module Direct_access_patricia_tree
    (Symex : Symex.Base)
    (Key : KeyS(Symex).S_patricia_tree) : S(Symex).S with type M.key = Key.t

module Concrete (Symex : Symex.Base) (Key : Soteria_std.Ordered_type.S) :
  S(Symex).S with type M.key = Key.t
