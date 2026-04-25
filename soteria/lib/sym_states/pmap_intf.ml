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
  module Key_S = S_map.Key (Symex)
  module Abstr = Abstr.M (Symex)

  module type S = sig
    type t [@@mixins Key_S.S + Abstr.S_with_syn]
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

    val of_opt : t option -> t
    val to_opt : t -> t option
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
  end
end
