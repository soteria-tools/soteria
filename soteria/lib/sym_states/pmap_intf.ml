open Symex

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
end

module Key (Symex : Symex.Base) = struct
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

module M
    (Symex : Symex.Base)
    (Key : sig
      type t
    end) =
struct
  module type S = sig
    type codom
    type t
    type codom_serialized
    type serialized = Key.t * codom_serialized

    module SM :
      State_monad.S
        with module Symex = Symex
         and module Value = Symex.Value
         and type st = t option

    type ('a, 'err) res := ('a, 'err, serialized list) SM.Result.t

    type ('a, 'err) codom_res :=
      codom option ->
      (('a, 'err, codom_serialized list) Compo_res.t * codom option) Symex.t

    val empty : t
    val syntactic_bindings : t -> (Key.t * codom) Seq.t
    val syntactic_mem : Key.t -> t -> bool

    val pp' :
      ?key:(Format.formatter -> Key.t -> unit) ->
      ?codom:(Format.formatter -> codom -> unit) ->
      ?ignore:(Key.t * codom -> bool) ->
      Format.formatter ->
      t ->
      unit

    val pp : Format.formatter -> t -> unit
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
     *  ('inner_serialized ->
     *  'inner_st option ->
     *  ( 'inner_st option,
     *    ([> Symex.lfail ] as 'a),
     *    'inner_serialized )
     *  Symex.Result.t) ->
     *  'inner_serialized serialized ->
     *  'inner_st t option ->
     *  ('inner_st t option, 'a, 'inner_serialized serialized) Symex.Result.t *)
  end
end
