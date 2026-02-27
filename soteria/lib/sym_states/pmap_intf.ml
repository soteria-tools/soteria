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

module Ckey (K : sig
  type t
end) =
struct
  type t = K.t
  type syn = K.t
end

module Key (Symex : Symex.Base) = struct
  open Symex
  module Data = Data.M (Symex)

  module type S = sig
    include Soteria_std.Ordered_type.S
    include Data.Abstr_with_syn with type t := t
    include Data.Sem_eq with type t := t
    include Data.Simplifiable with type t := t

    val distinct : t list -> Value.(sbool t)
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
      type syn
    end) =
struct
  module type S = sig
    type codom
    type t
    type codom_serialized
    type syn = Key.syn * codom_serialized

    module SM :
      State_monad.S
        with module Symex = Symex
         and module Value = Symex.Value
         and type st = t option

    type ('a, 'err) res := ('a, 'err, syn list) SM.Result.t

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
    val pp_syn : Format.formatter -> syn -> unit
    val show_serialized : syn -> string
    val show_syn : syn -> string
    val to_syn : t -> syn list
    val of_opt : t option -> t
    val to_opt : t -> t option
    val ins_outs : syn -> Symex.Value.Expr.t list * Symex.Value.Expr.t list
    val alloc : new_codom:codom -> (Key.t, 'err) res

    val allocs :
      fn:('a -> Key.t -> ('k * codom) Symex.t) ->
      els:'a list ->
      ('k list, 'err) res

    val wrap : Key.t -> ('a, 'err) codom_res -> ('a, 'err) res

    val fold :
      ('acc -> Key.t * codom -> 'acc Symex.t) ->
      'acc ->
      t option ->
      'acc Symex.t

    val produce : syn -> t option -> t option Symex.Producer.t

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
