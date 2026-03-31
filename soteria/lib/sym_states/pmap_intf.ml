open Symex
open Data

module Ckey (K : sig
  type t
end) =
struct
  type t = K.t
  type syn = K.t
end

module Key (Symex : Symex.Base) = struct
  module type S = sig
    include S_map.Key(Symex).S
    include Abstr.M(Symex).S_with_syn with type t := t
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
    type codom_syn

    include Base.M(Symex).S with type syn = Key.syn * codom_syn

    module SM :
      State_monad.S
        with module Symex = Symex
         and module Value = Symex.Value
         and type st = t option

    type ('a, 'err) res := ('a, 'err, syn list) SM.Result.t

    type ('a, 'err) codom_res :=
      codom option ->
      (('a, 'err, codom_syn list) Compo_res.t * codom option) Symex.t

    val empty : t
    val syntactic_bindings : t -> (Key.t * codom) Seq.t
    val syntactic_mem : Key.t -> t -> bool

    val pp' :
      ?codom:(Format.formatter -> codom -> unit) ->
      ?key:(Format.formatter -> Key.t -> unit) ->
      ?ignore:(Key.t * codom -> bool) ->
      Format.formatter ->
      t ->
      unit

    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val pp_syn : Format.formatter -> syn -> unit
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
